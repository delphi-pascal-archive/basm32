{-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-}
{-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-* Win-Assembler *+*-*+*-*+*-*+*-*+*-*+*-*+*-}
{-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-}

Unit Symbols;
{$i Switches.inc}

INTERFACE

USES	SysUtils, ObjList, Assemble;

TYPE { Symbols definition }

  TSymbolName = String[64]; // Max Allocation size of an identifier
  TSymbolHash = Cardinal;

TYPE
 { Here All Symbols definitions that we can find in a source file }
  TSymbolKind =(
      SYM_Undef,      // ???
      SYM_PROGRAM,    // Program
      SYM_EQUAL,      // for EQU definitions
      SYM_VALUE,      // for '=' definitions
	    SYM_LABEL,      // Label definitions
      SYM_TYPEDEF     // Type Definition
  );
CONST
  TSymbolKindName: Array[TSymbolKind] of String[9] =(
      'Undefined',
      'PROGRAM',
	    'EQUAL',
	    'VALUE',
	    'LABEL',
      'TYPEDEF'
  );


TYPE
  TDefinitionKind = (
	    DEF_LABEL,  // UNKNOWN definition
	    DEF_NEAR,
	    DEF_FAR,
	    DEF_BYTE,
	    DEF_WORD,
	    DEF_DWORD,
      DEF_STRUCT
  );

CONST
  TDefinitionKindName : Array[TDefinitionKind] of String[6] =(
	    'LABEL',  // UNKNOWN definition
	    'NEAR',
	    'FAR',
	    'BYTE',
	    'WORD',
	    'DWORD',
      'STRUCT'
  );

TYPE
  TSymbolFlag = Set of (
 	    SYM_FLAG_EXTERN,  // External Symbol
	    SYM_FLAG_PUBLIC,    // Symbol is public
      SYM_FLAG_IMPORT,
	    SYM_FLAG_DEFINED,   // Symbol is defined in current module
      SYM_FLAG_USED       // Symbol was called or used in an instruction
  );

  pSymbol = ^TSymbol;
  tSymbol = Packed Object { Symbol Structure Header }
      Next    : pSymbol;      // next symbol
      Prev    : pSymbol;
	    Kind	  : TSymbolKind;
	    Name	  : TSymbolName;
      Hash    : TSymbolHash; { Hash calculated for fastest searching }
	    Flag	  : TSymbolFlag;
      Data    : Pointer;
      Constructor Init(sKind:TSymbolKind; Const sName:String; sFlag:TSymbolFlag; aData:Pointer);
      Destructor SelfDestroy; Virtual;  // Free Symbol from Memory, for pointer use
  end;

  // Symbols Data
  pDataLabel = ^tDataLabel;
  tDataLabel = Packed Object
      Def     : TDefinitionKind;
      DefSym  : pSymbol;
      OwnSeg  : Pointer; // Own Segment
      OwnOfs  : Integer; // Address within Own Segment
      Constructor Init(ADef:TDefinitionKind; aDefSym:pSymbol; sSeg:Pointer; sOfs:Integer);
      Destructor SelfDestroy; Virtual;
  End;

  pDataValue = ^tDataValue;
  tDataValue = packed object
      Value : Integer;
      Constructor Init(Val:Integer);
      Destructor SelfDestroy; Virtual;
  end;

  pDataEqual = ^tDataEqual;
  tDataEqual = packed object
      Expr    : tAsmOperand;
      Constructor Init(Const Op:tAsmOperand);
      Destructor SelfDestroy; Virtual;
  end;

  pDataTypeDef = ^tDataTypeDef;
  tDataTypeDef = packed object
      Def     : TDefinitionKind;
      Size    : Integer;
      Constructor Init(sDef:TDefinitionKind);
      Destructor SelfDestroy; Virtual;
  end;

  pSymbolStack = ^tSymbolStack;
  tSymbolStack = Object
      Head    : pSymbol;
      Parent  : pSymbolStack;
      Constructor Init;
      Destructor Done;
      Destructor SelfDestroy;
      procedure PushSymbol( NewSym:pSymbol );
      procedure PopSymbol; // and destroy
      function FindSymbol(Const S:tSymbolName): pointer;
      procedure DelSymbol(Sym:pSymbol);
      procedure ReplaceSym(OldSym, NewSym:pSymbol);
  end;

  // Done only for Fast Search
  pSymbolLength = ^tSymbolLength;
  tSymbolLength = packed Object
      Parent  : pSymbolLength;
      SymbolStack: packed Array[1..128] of tSymbolStack;
      Constructor Init;
      Destructor Done;
      procedure PushSymbol( NewSym:pSymbol );
      function FindSymbol(Const S:tSymbolName): pointer;
  end;

  pSymbolGroupItem  =  ^tSymbolGroupItem;
  tSymbolGroupItem  =  tSymbolLength;

  pSymbolGroup = ^tSymbolGroup;
  tSymbolGroup = Object
      Head    : pSymbolGroupItem;
      Constructor Init;
      Destructor Done;
      procedure PushItem( NewItem:pSymbolGroupItem );
      procedure PopItem;
      function FindSymbol(Const S:tSymbolName):pointer;
  end;

////////////////////////////////////////////////////////////////////////////////

IMPLEMENTATION

function CalcHash (Const S: ShortString): TSymbolHash; Assembler;
Asm // in assembler to get more speed
        PUSH    ESI
        LEA     ESI,[S]
        LODSB
        XOR     EDX,EDX
        XOR     ECX,ECX
        MOV     CL,AL
        JECXZ   @Exit
        XOR     EAX,EAX
@Ret:   LODSB
        CMP     AL,'a'
        JB      @Up
        CMP     AL,'z'
        JA      @Up
        SUB     AL,'a'-'A'  // there is no difference between lowercase and uppercase
 @Up:   ROL     EDX,1
        ADD     EDX,ECX
        ADD     EDX,EAX
        LOOP    @Ret
@Exit:  MOV     EAX,EDX
        POP     ESI
End;


////////////////////////////////////////////////////////////////////////////////
// tSymbol
////////////////////////////////////////////////////////////////////////////////

Constructor tSymbol.Init(sKind:TSymbolKind; Const sName:String; sFlag:TSymbolFlag; aData:Pointer);
begin
  Kind := sKind;
  Hash := CalcHash(sName);
  Name := sName;
  Flag:=sFlag;
  Data:=aData;
end;


Destructor tSymbol.SelfDestroy;
begin
  FreeMem( @Self, Sizeof(tSymbol) );
end;

////////////////////////////////////////////////////////////////////////////////
// tSymLabel
////////////////////////////////////////////////////////////////////////////////

Constructor tDataLabel.Init(ADef:TDefinitionKind; aDefSym:pSymbol; sSeg:Pointer; sOfs:Integer);
begin
  Def  :=ADef;
  DefSym:=aDefSym;
  OwnSeg := sSeg;
  OwnOfs := sOfs;
end;

Destructor tDataLabel.SelfDestroy;
begin
  FreeMem( @Self, Sizeof(tDataLabel) );
end;

////////////////////////////////////////////////////////////////////////////////
// tSymValue
////////////////////////////////////////////////////////////////////////////////

Constructor tDataValue.Init(Val:Integer);
begin
  Value:= Val;
end;

Destructor tDataValue.SelfDestroy;
begin
  FreeMem( @Self, Sizeof(tDataValue) );
end;

////////////////////////////////////////////////////////////////////////////////
// tSymEqual
////////////////////////////////////////////////////////////////////////////////

Constructor tDataEqual.Init(Const Op:tAsmOperand);
begin
  Move(Op, Expr, Sizeof(tAsmOperand) );
end;

Destructor tDataEqual.SelfDestroy;
begin
  FreeMem( @Self, Sizeof(tDataEqual) );
end;


////////////////////////////////////////////////////////////////////////////////
// tSymTypeDef
////////////////////////////////////////////////////////////////////////////////

Constructor tDataTypeDef.Init(sDef:TDefinitionKind);
begin
 Def:=sDef;
end;

Destructor tDataTypeDef.SelfDestroy;
begin
  FreeMem( @Self, Sizeof(tDataTypeDef) );
end;

////////////////////////////////////////////////////////////////////////////////
// tSymbolStack
////////////////////////////////////////////////////////////////////////////////

constructor tSymbolStack.Init;
begin
  Head := nil;
end;

destructor tSymbolStack.Done;
begin
  while (Head<>NIL) do PopSymbol;
end;

Destructor tSymbolStack.SelfDestroy;
begin
  Done;
  FreeMem(@Self, Sizeof(tSymbolStack) );
end;

procedure tSymbolStack.PushSymbol( NewSym:pSymbol );
begin
  if Head<>NIL then Head^.Prev:=NewSym;
  NewSym^.Next:=Head;
  NewSym^.Prev:=NIL;
  Head := NewSym;
end;

procedure tSymbolStack.PopSymbol;
 Var    Item : pSymbol;
begin
  if Head=NIL then Exit;
  Item:=Head.Next;
  Head^.SelfDestroy;
  Head:=Item;
  if (Head<>NIL) then Head.Prev:=NIL;
end;

function tSymbolStack.FindSymbol(Const S:tSymbolName):pointer;
 Var  Item:pSymbol;
      Hash  : tSymbolHash;
begin
  Item:=Head;
  Hash:=CalcHash(S);
  while Item<>Nil do begin
    if Item.Hash=Hash then
      if Item.Name=S then begin
        FindSymbol:=Item;
        Exit;
      end;
    Item:=Item^.Next;
  end;
  FindSymbol:=NIL;
end;

procedure tSymbolStack.DelSymbol(Sym:pSymbol);
 Var  Prev,Next : pSymbol;
begin
  if Sym=NIL then Exit;
  Prev:=Sym.Prev;
  Next:=Sym.Next;
  if (Prev<>NIL) then Prev.Next:=Next;
  if (Next<>NIL) then Next.Prev:=Prev;
  if (Head=Sym) then Head:=Next;
  Sym.SelfDestroy;
end;

procedure tSymbolStack.ReplaceSym(OldSym, NewSym:pSymbol);
 Var  Prev,Next : pSymbol;
begin
  if (OldSym=NIL)OR(NewSym=NIL) then Exit;
  Prev:=OldSym.Prev;
  Next:=OldSym.Next;
  NewSym.Prev:= Prev;
  NewSym.Next:= Next;
  if (Prev<>NIL) then Prev.Next:=NewSym;
  if (Next<>NIL) then Next.Prev:=NewSym;
  if Head=OldSym then Head:=NewSym;
  OldSym.SelfDestroy;
end;



////////////////////////////////////////////////////////////////////////////////
// tSymbolGroup
////////////////////////////////////////////////////////////////////////////////

Constructor tSymbolLength.Init;
 Var  Len: Integer;
begin
 for Len:= 1 to 128 do  SymbolStack[Len].Init;
end;

Destructor tSymbolLength.Done;
 Var  Len: Integer;
begin
 for Len:= 1 to 128 do  SymbolStack[Len].Done;
end;

procedure tSymbolLength.PushSymbol( NewSym:pSymbol );
 Var  Len: Integer;
begin
  Len:=Length(NewSym.Name);
  SymbolStack[Len].PushSymbol(NewSym);
end;

function tSymbolLength.FindSymbol(Const S:tSymbolName): pointer;
 Var  Len: Integer;
begin
  Len:=Length(S);
  FindSymbol:=SymbolStack[Len].FindSymbol(S);
end;





////////////////////////////////////////////////////////////////////////////////
// tSymbolGroup
////////////////////////////////////////////////////////////////////////////////

Constructor tSymbolGroup.Init;
begin
  Head := nil;
end;

Destructor tSymbolGroup.Done;
begin
  while (Head<>NIL) do PopItem;
end;

procedure tSymbolGroup.PushItem( NewItem:pSymbolGroupItem );
begin
  NewItem^.Parent:=Head;
  Head := NewItem;
end;

procedure tSymbolGroup.PopItem;
 Var    Item : pSymbolGroupItem;
begin
  if (Head=NIL) then Exit;
  Item:=Head.Parent;
  Head:=Item;
end;

function tSymbolGroup.FindSymbol(Const S:tSymbolName):pointer;
 Var  Item  : pSymbolGroupItem;
      Sym   : pSymbol;
begin
  Item:=Head;
  while Item<>NIL do begin
    Sym:=Item.FindSymbol(S);
    if Sym<>NIL then begin
      FindSymbol:=Sym;
      Exit;
    end;
    Item:=Item.Parent;
  end;
  FindSymbol:=NIL;
end;

end.
