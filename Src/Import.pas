{-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-}
{-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-* Win-Assembler *+*-*+*-*+*-*+*-*+*-*+*-*+*-}
{-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-}

Unit Import;

{$i Switches.inc}

interface
uses Objlist, Symbols, Symbols2;

TYPE

  pImportItem = ^tImportItem;
  tImportItem = Object(tSymbol)
        Index   : Integer;
        Jmp_Sym : pSymbol;
        Constructor Init(Const AName:String; AIndex:Integer; ASym:pSymbol);
        function IsCalled:Boolean;
  End;

  pLibraryItem = ^TLibraryItem;
  tLibraryItem = Object
        Head    : pImportItem;
        Next    : pLibraryItem;
        Name	  : TSymbolName;
        Constructor Init(Const AName:String);
        procedure PushImport(Const AName:String; AIndex:Integer; ASym:pSymbol);
        procedure PopImport;
        procedure PopAll;
        function FindImport( Const S: String ):pImportItem;
        function CountCalledImports:Integer;
        function CountImports:Integer;
        function IsCalled:Boolean;
  end;

  TLibraryStack = Object
        Head    : pLibraryItem;
        procedure Init;
        procedure PushLib( Const AName:String );
        procedure PopLib;
        procedure PopAll;
        function FindLib( Const S: String ):pLibraryItem;
        function CountCalledLibs:Integer;
        function CountLibs:Integer;
        function CountCalledImports:Integer;
        function CountImports:Integer;
  End;

implementation

///////////////////////////////////////////////////////////////////////////////
//
///////////////////////////////////////////////////////////////////////////////

Constructor tImportItem.Init(Const AName:String; AIndex:Integer; ASym:pSymbol);
begin
 Inherited Init(SYM_LABEL, AName, [SYM_FLAG_IMPORT],NIL);
 Next:=NIL;
 Index:=AIndex;
 Jmp_Sym:=ASym;
end;

function tImportItem.IsCalled:Boolean;
begin
  if SYM_FLAG_USED in Jmp_Sym.Flag then Result:=True else Result:=False;
end;

///////////////////////////////////////////////////////////////////////////////
// TLibraryItem
///////////////////////////////////////////////////////////////////////////////

Constructor TLibraryItem.Init(Const AName:String);
begin
  Head:=NIL;
  Next:=NIL;
  Name:=AName;
end;


procedure TLibraryItem.PushImport(Const AName:String; AIndex:Integer; ASym:pSymbol);
 Var  NewItem  : pImportItem;
begin
  New( NewItem, Init(AName, AIndex, ASym) );
  NewItem^.Next:=Head;
  Head := NewItem;
end;

procedure TLibraryItem.PopImport;
 Var    Item : pImportItem;
begin
  if Head=NIL then Exit;
  Item:=pImportItem(Head.Next);
  Dispose( Head );
  Head:=Item;
end;

procedure TLibraryItem.PopAll;
begin
  while (Head<>NIL) do PopImport;
end;

function TLibraryItem.FindImport( Const S: String ):pImportItem;
 Var    Item : pImportItem;
begin
  Item:=Head;
  while (Item<>NIL) do begin
    if (Item^.Name=S) then begin
        Result:=Item;
        Exit;
    end;
    Item:=pImportItem(Item^.Next);
  end;
  Result:=NIL;
end;

function TLibraryItem.CountCalledImports:Integer;
 Var    Item  : pImportItem;
        Num   : Integer;
begin
  Item:=Head;
  Num:=0;
  while (Item<>NIL) do begin
    if Item^.IsCalled then Inc(Num);
    Item:=pImportItem(Item^.Next);
  end;
  Result:=Num;
end;

function TLibraryItem.CountImports:Integer;
 Var    Item  : pImportItem;
        Num   : Integer;
begin
  Item:=Head;
  Num:=0;
  while (Item<>NIL) do begin
    Inc(Num);
    Item:=pImportItem(Item^.Next);
  end;
  Result:=Num;
end;

function TLibraryItem.IsCalled:Boolean;
 Var    Item : pImportItem;
begin
  Item:=Head;
  while (Item<>NIL) do begin
    if (Item^.IsCalled) then begin
        Result:=True;
        Exit;
    end;
    Item:=pImportItem(Item^.Next);
  end;
  Result:=FALSE;
end;


///////////////////////////////////////////////////////////////////////////////
// TLibraryStack
///////////////////////////////////////////////////////////////////////////////

procedure TLibraryStack.Init;
begin
  Head:=NIL;
end;

procedure TLibraryStack.PushLib( Const AName:String );
 Var  NewItem  : pLibraryItem;
begin
  New( NewItem, Init(AName) );
  NewItem^.Next:=Head;
  Head := NewItem;
end;

procedure TLibraryStack.PopLib;
 Var    Item : pLibraryItem;
begin
  if (Head<>NIL) then begin
    Item:=Head.Next;
    Head.PopAll;
    Dispose( Head );
    Head:=Item;
  end;
end;

procedure TLibraryStack.PopAll;
begin
  while (Head<>NIL) do PopLib;
end;

function TLibraryStack.FindLib( Const S: String ):pLibraryItem;
 Var    Item : pLibraryItem;
begin
  Item:=Head;
  while (Item<>NIL) do begin
    if (Item^.Name=S) then begin
        Result:=Item;
        Exit;
    end;
    Item:=Item^.Next;
  end;
  Result:=NIL;
end;

function TLibraryStack.CountCalledLibs:Integer;
 Var    Item  : pLibraryItem;
        Num   : Integer;
begin
  Item:=Head;
  Num:=0;
  while (Item<>NIL) do begin
    if Item.IsCalled then Inc(Num);
    Item:=Item^.Next;
  end;
  Result:=Num;
end;

function TLibraryStack.CountLibs:Integer;
 Var    Item  : pLibraryItem;
        Num   : Integer;
begin
  Item:=Head;
  Num:=0;
  while (Item<>NIL) do begin
    Inc(Num);
    Item:=Item^.Next;
  end;
  Result:=Num;
end;

function TLibraryStack.CountCalledImports:Integer;
 Var    Item  : pLibraryItem;
        Num   : Integer;
begin
  Item:=Head;
  Num:=0;
  while (Item<>NIL) do begin
    Inc(Num, Item^.CountCalledImports);
    Item:=Item^.Next;
  end;
  Result:=Num;
end;

function TLibraryStack.CountImports:Integer;
 Var    Item  : pLibraryItem;
        Num   : Integer;
begin
  Item:=Head;
  Num:=0;
  while (Item<>NIL) do begin
    Inc(Num, Item^.CountImports);
    Item:=Item^.Next;
  end;
  Result:=Num;
end;

end.
