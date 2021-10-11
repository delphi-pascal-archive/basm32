{-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-}
{-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-* Win-Assembler *+*-*+*-*+*-*+*-*+*-*+*-*+*-}
{-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-}

Unit Parser;

Interface
USES	SysUtils, Convert, StrTools,
      ComLine, Symbols, Symbols2, Global, Segments, RelocationStack, Assemble, Scanner, Import, Asmtostr;

Var NumArg : Integer;
    Inst    : tAsmInstruction;

PROCEDURE ParseLINE;
PROCEDURE Compile;

Implementation


///////////////////////////////////////////////////////////////////////////////
// Exprssion
///////////////////////////////////////////////////////////////////////////////

TYPE
{ Main Expression type to calculate Value and get registers and references used }
  tOperator = (
    coEXPO,{Highest precedence}
	  coMOD,coDIV,coMUL,
    coAND,coSHL,coSHR,
    coPLUS,coMINUS,coXOR,coOR,
    coEQUAL, {Lowest}
    coNONE
  );

CONST
 Precedence :Array[tOperator] of BYTE =(
    1,
    2,2,2,
    3,3,3,
    4,4,4,4,
    5,
    6
 );


PROCEDURE ParseExpression(Var Term:tAsmOperand);Forward;

PROCEDURE EvalOperation( Term1, Term2:tAsmOperand; Op:tOperator;Var Rt:tAsmOperand);
Begin
 Rt.Loc:=LOC_NONE;
 Rt.RefKind:=REF_NONE;
 Rt.Value:=0;

 Case (OP) OF
  coPLUS: // Addition of values, registers and references
    Case Term1.Loc of
      LOC_IMMEDIATE:
        Case Term2.Loc of
          LOC_IMMEDIATE: begin
            if Term1.RefKind<>REF_NONE then
              if Term2.RefKind<>REF_NONE then MakeError('EvalOperation','More than one symbol in an operand')
              else begin
                Rt.RefKind:=Term1.RefKind;
                Rt.RefSym:=Term1.RefSym;
              end
            else begin
              Rt.RefKind:=Term2.RefKind;
              Rt.RefSym:=Term2.RefSym;
            end;
            Rt.Loc:=LOC_IMMEDIATE;
            Rt.Value:=Term1.Value+Term2.Value;
          end;
          LOC_REGISTER: begin
            Rt.RefKind:=Term1.RefKind;
            Rt.RefSym:=Term1.RefSym;
            SetMemoryOperand(Rt, SIZE_NONE, Term2.Reg, REG_NONE, 0, Term1.Value);
          end;
          LOC_MEMORY: begin
            if Term1.RefKind<>REF_NONE then
              if Term2.RefKind<>REF_NONE then MakeError('EvalOperation','More than one symbol in an operand')
              else begin
                Rt.RefKind:=Term1.RefKind;
                Rt.RefSym:=Term1.RefSym;
              end
            else begin
              Rt.RefKind:=Term2.RefKind;
              Rt.RefSym:=Term2.RefSym;
            end;
            SetMemoryOperand(Rt, SIZE_NONE, Term2.Base, Term2.Index, Term2.Scale, Term1.Value+Term2.Offset);
          end;
        end;

      LOC_REGISTER:
        Case Term2.Loc of
          LOC_IMMEDIATE: begin
            Rt.RefKind:=Term2.RefKind;
            Rt.RefSym:=Term2.RefSym;
            SetMemoryOperand(Rt, SIZE_NONE, Term1.Reg, REG_NONE, 0, Term2.Value);
          end;
          LOC_REGISTER: begin
            if RegisterSize(Term1.Reg)<>RegisterSize(Term2.Reg) then MakeError('EvalOperation','Different Register Sizes');
            Rt.RefKind:=REF_NONE;
            Rt.RefSym:=NIL;
            SetMemoryOperand(Rt, SIZE_NONE, Term1.Reg, Term2.Reg, 0, 0);
          end;
          LOC_MEMORY: begin
            if Term2.Index<>REG_NONE then MakeError('EvalOperation','Too much registers');
            Rt.RefKind:=Term2.RefKind;
            Rt.RefSym:=Term2.RefSym;
            SetMemoryOperand(Rt, SIZE_NONE, Term1.Reg, Term2.Reg, Term2.Scale, Term2.Offset);
          end;
        end;

      LOC_MEMORY:
        Case Term2.Loc of
          LOC_IMMEDIATE: begin
            if Term1.RefKind<>REF_NONE then
              if Term2.RefKind<>REF_NONE then MakeError('EvalOperation','Cant evaluate 2 references')
              else begin
                Rt.RefKind:=Term1.RefKind;
                Rt.RefSym:=Term1.RefSym;
              end
            else begin
              Rt.RefKind:=Term2.RefKind;
              Rt.RefSym:=Term2.RefSym;
            end;
            SetMemoryOperand(Rt, SIZE_NONE, Term1.Base, Term1.Index, Term1.Scale, Term1.Value+Term2.Offset);
          end;
          LOC_REGISTER: begin
            if Term1.Index<>REG_NONE then MakeError('EvalOperation','Too much registers');
            Rt.RefKind:=Term1.RefKind;
            Rt.RefSym:=Term1.RefSym;
            SetMemoryOperand(Rt, SIZE_NONE, Term2.Reg, Term1.Base, Term1.Scale, Term1.Offset);
          end;
          LOC_MEMORY: begin
            if (Term1.Index<>REG_NONE)AND(Term2.Index<>REG_NONE) then MakeError('EvalOperation','Too much registers');
            if (Term1.Scale<>0)AND(Term2.Scale<>0) then MakeError('EvalOperation','Only scaled index');
            if Term1.RefKind<>REF_NONE then
              if Term2.RefKind<>REF_NONE then MakeError('EvalOperation','More than one symbol in an operand')
              else begin
                Rt.RefKind:=Term1.RefKind;
                Rt.RefSym:=Term1.RefSym;
              end
            else begin
              Rt.RefKind:=Term2.RefKind;
              Rt.RefSym:=Term2.RefSym;
            end;
            if Term1.Scale<>0
              then SetMemoryOperand(Rt, SIZE_NONE, Term2.Base, Term1.Base, Term1.Scale, Term2.Offset)
              else SetMemoryOperand(Rt, SIZE_NONE, Term1.Base, Term2.Base, Term2.Scale, Term2.Offset)
          end;
        end;
    end;

  coMINUS: Begin{ Substraction of values only }
    if Term2.Loc<>LOC_IMMEDIATE then MakeError('EvalOperation','Invalid Register operation');
    if Term2.RefKind<>REF_NONE then MakeError('EvalOperation','Invalid Register operation');
    Case Term1.Loc of
      LOC_IMMEDIATE: begin
        Rt.RefKind:=Term1.RefKind;
        Rt.RefSym:=Term1.RefSym;
        SetImmediateOperand(Rt, Term1.Value-Term2.Value);
      end;
      LOC_REGISTER: begin
        Rt.RefKind:=Term1.RefKind;
        Rt.RefSym:=Term1.RefSym;
        SetMemoryOperand(Rt,SIZE_NONE, Term1.Reg, REG_NONE,0,-Term2.Value);
      end;
      LOC_MEMORY: begin
        Rt.RefKind:=Term1.RefKind;
        Rt.RefSym:=Term1.RefSym;
        SetMemoryOperand(Rt,SIZE_NONE, Term1.Base, Term1.Index, Term1.Scale, Term1.Offset-Term2.Value);
      end;
    end;
  end;

  coNONE: Move(Term2,Rt,Sizeof(Term2));
  Else Begin
    if (Term1.Loc<>LOC_IMMEDIATE)OR(Term2.Loc<>LOC_IMMEDIATE) then MakeError('EvalOperation','Invalid Register operation');
    if (Term1.RefKind<>REF_NONE)OR(Term2.RefKind<>REF_NONE) then MakeError('EvalOperation','Invalid Register operation');
    Case Op of
    	//coEXPO:Result.Value:=Expo( Term1.Value, Term2.Value);
    	coEQUAL: Rt.Value:=Integer(Term1.Value = Term2.Value);
    	coMOD: Rt.Value:=Term1.Value MOD Term2.Value;
    	coDIV: Rt.Value:=Term1.Value DIV Term2.Value;
    	coMUL: Rt.Value:=Term1.Value*Term2.Value;
    	coAND: Rt.Value:=Term1.Value AND Term2.Value;
    	coSHL: Rt.Value:=Term1.Value SHL Term2.Value;
    	coSHR: Rt.Value:=Term1.Value SHR Term2.Value;
    	coXOR: Rt.Value:=Term1.Value XOR Term2.Value;
    	coOR : Rt.Value:=Term1.Value OR  Term2.Value;
    End;
  End;
 End;{CASE}
End;{EvalOperation}

PROCEDURE ReadFactor(Var Term:tAsmOperand);
 Var
      Sym   :pSymbol;
      Sym2  :pSymbol;
      Val   : Integer;
Begin
  Term.Loc:=LOC_NONE;
  Term.RefKind:=REF_NONE;
  Case Token of
    TOK_INTCONST: SetImmediateOperand(Term, TokenInt);
    TOK_LFRAME: begin  // Memory
      ReadToken;
      ParseExpression(Term);
      Case Term.Loc Of // change any operand location to memory location.
        LOC_IMMEDIATE: SetMemoryOperand(Term, SIZE_NONE, REG_NONE, REG_NONE, 0, Term.Value);
        LOC_REGISTER: begin
          SetMemoryOperand(Term, SIZE_NONE, Term.Reg, REG_NONE, 0, 0);
          Term.RefKind:=REF_NONE;
        end;
        LOC_MEMORY: ; // nothing to do
      end;
      if Token<>TOK_RFRAME Then MakeError('ReadFactor',ExpectRFRAME);
      // now check for structure type.
      if Readtoken<>TOK_POINT then Exit; // no used structure.
      Readtoken;
      if not IsTokenIdent then MakeError('ReadFactor','Identifier Expected but found '+TokenStr);
      Sym:=SymGroup.FindSymbol(TokenUpStr);
      if Sym=NIL then MakeError('ReadFactor','Undeclared Structure: '+TokenStr);
      if (Sym.Kind=SYM_TYPEDEF)and(pDataTypeDef(Sym.Data).Def=DEF_STRUCT) then begin// is a structure
        if Readtoken<>TOK_POINT then MakeError('ReadFactor','"." expected');
        ReadToken;
        Sym2:=pDataStruct(Sym.Data).Symbols.FindSymbol(TokenUpStr);
        if Sym2=NIL then MakeError('ReadFactor','Field Expected of the Structure: '+Sym.Name);
        Term.Offset:=Term.Offset+pDataLabel(Sym2.Data).OwnOfs;
      end else MakeError('ReadFactor','Structure Type Expected');
    end;
    TOK_MINUS	:
      Begin{ Function NEG }
        ReadToken;
        if (Token<>TOK_INTCONST) Then MakeError('ReadFactor',ExpectINTCONST);
        SetImmediateOperand(Term, -TokenInt);
      End;
    TOK_EXCLAMATION	:
      Begin{ Function NOT }
        ReadToken;
        if (Token<>TOK_INTCONST) Then MakeError('ReadFactor',ExpectINTCONST);
        SetImmediateOperand(Term, NOT(TokenInt));
      End;
    TOK_LPAREN	:
      Begin
        ReadToken;
        ParseExpression(Term);
        if (Token<>TOK_RPAREN) Then MakeError('ReadFactor',ExpectRPAREN);
      End;
    TOK_IDENTIFIER:
      Case TokenFlag([TOK_FLAG_REGISTER, TOK_FLAG_ROUTINE, TOK_FLAG_SPECIFIER]) of
        TOK_REGISTER: begin // check for segment register???
          SetRegisterOperand(Term, tAsmRegister(TokenInt));
          if ReadToken<>TOK_COLON then Exit;
          Case tAsmRegister(TokenInt) of
            REG_ES: Inst.PrefixSegment:=PRE_SEGES;
            REG_CS: Inst.PrefixSegment:=PRE_SEGCS;
            REG_SS: Inst.PrefixSegment:=PRE_SEGSS;
            REG_DS: Inst.PrefixSegment:=PRE_SEGDS;
            else MakeError('ReadFactor','Register segment Expected');
          end;
          ReadToken;  
          ParseExpression(Term);
          Exit;
        end;

        TOK_SPECIFIER: begin
          Val:=TokenInt;
          ReadToken;
          if not IsTokenIdent Then MakeError('ReadFactor','Expect Symbol after NEAR');
          Sym:=SymGroup.FindSymbol(TokenUpStr);
          if (Sym=NIL) then begin
            New( Sym, Init(SYM_UNDEF,TokenUpStr,[SYM_FLAG_USED], NIL) );
            SymGroup.Head.PushSymbol(Sym);
          end else Sym.Flag:= Sym.Flag + [SYM_FLAG_USED];
          Term.RefSym:=Sym;
          SetImmediateOperand(Term,0);
          Case tSizeSpecifier(Val) of
            SZS_NEAR:  Term.RefKind:=REF_NEAR;
            SZS_SHORT: Term.RefKind:=REF_SHORT;
          end;
        end;
        TOK_ROUTINE:
          Case tRoutine(TokenInt) of
            ROUT_ADDR, ROUT_OFFSET: Begin // Offset <Ref> -> exOFS
              ReadToken;
              if not IsTokenIdent Then MakeError('ReadFactor','Expect Symbol');
              Sym:=SymGroup.FindSymbol(TokenUpStr);
              if (Sym=NIL) then begin
                New( Sym, Init(SYM_UNDEF,TokenUpStr,[SYM_FLAG_USED], NIL) );
                SymGroup.Head.PushSymbol(Sym);
              end else Sym.Flag:= Sym.Flag + [SYM_FLAG_USED];
              Term.RefSym:=Sym;
              Term.RefKind:=REF_OFS;
              SetImmediateOperand(Term,0);
            end;
            ROUT_SIZEOF: Begin // Offset <Ref> -> exOFS
              Term.RefKind:=REF_NONE;
              ReadToken;
              if not IsTokenIdent Then MakeError('ReadFactor','Expect Symbol');
              Sym:=SymGroup.FindSymbol(TokenUpStr);
              if (Sym=NIL) then MakeError('ReadFactor','Undeclared symbol');
              Case Sym.Kind of
                SYM_LABEL:
                  SetImmediateOperand(Term, 4);
                SYM_TYPEDEF:
                  SetImmediateOperand(Term, pDataTypeDef(Sym.Data).Size);
              end;
            end;
          end;

        TOK_IDENTIFIER: Begin{ <Ref> -> exFIXUP (default type) }
          if not IsTokenIdent then MakeError('ReadFactor','Identifier Expected');
          Term.RefKind:=REF_FIXUP;
          Sym:=SymGroup.FindSymbol(TokenUpStr);
          if (Sym=NIL) then begin
            New( Sym, Init(SYM_UNDEF,TokenUpStr,[SYM_FLAG_USED], NIL) );
            SymGroup.Head.PushSymbol(Sym);
            SetImmediateOperand(Term,0);
            Term.RefSym:=Sym;
          end
          else begin
            Sym.Flag:= Sym.Flag + [SYM_FLAG_USED];
            Case Sym.Kind of
              SYM_LABEL: begin
                Sym.Flag:= Sym.Flag + [SYM_FLAG_USED];
                SetImmediateOperand(Term,0);
                Term.RefSym:=Sym;
                if pDataLabel(Sym.Data).Def=DEF_STRUCT then
                  if ReadToken=TOK_POINT then begin
                    ReadToken;
                    Sym2:=pDataLabel(Sym.Data).DefSym;
                    Sym:=pDataStruct(Sym2.Data).Symbols.FindSymbol(TokenUpStr);
                    if Sym=NIL then MakeError('ReadFactor','Field Struct Expected');
                    Term.Offset:=pDataLabel(Sym.Data).OwnOfs;
                  end else Exit;
              end;
              SYM_VALUE: begin
                SetImmediateOperand(Term, pDataValue(Sym.Data).Value );
                Term.RefKind:=REF_NONE;
              end;
              SYM_EQUAL: Move( pDataEqual(Sym.Data).Expr, Term, Sizeof(term) );
              else begin
                SetImmediateOperand(Term,0);
                Term.RefSym:=Sym;
              end;
            end;
          end;
        end;
      end;
    else MakeError('ReadFactor','Expression Error');
  End;
  ReadToken;
End;

FUNCTION ReadOperator:tOperator;
Begin
 Case Token of
  TOK_EQUAL : ReadOperator:=coEQUAL;
	TOK_PLUS	: ReadOperator:=coPLUS;
	TOK_MINUS	: ReadOperator:=coMINUS;
	TOK_SLASH	: ReadOperator:=coDIV;
	TOK_STAR	: ReadOperator:=coMUL;
	TOK_CARET	: ReadOperator:=coEXPO;
	TOK_IDENTIFIER:
    if TokenFlag([TOK_FLAG_OPERATOR])=TOK_OPERATOR then
      Case tTokOperator(TokenInt) of
        OPER_SHR: ReadOperator:=coSHR;
        OPER_SHL: ReadOperator:=coSHL;
        OPER_MOD: ReadOperator:=coDIV;
        OPER_DIV: ReadOperator:=coDIV;
        OPER_AND: ReadOperator:=coAND;
        OPER_OR : ReadOperator:=coOR;
        OPER_XOR: ReadOperator:=coXOR;
      end
    else Begin ReadOperator:=coNONE;Exit; End;
  else Begin ReadOperator:=coNONE;Exit; End;
 End;
 ReadToken;
End;

PROCEDURE ReadExpression(Var LastOper:tOperator; Var LastTerm:tAsmOperand);
 Var	CurTerm:tAsmOperand;
	CurOper:tOperator;
Begin
  ReadFactor(CurTerm);
  CurOper:=ReadOperator;
  While Precedence[CurOper]<Precedence[LastOper] DO  ReadExpression(CurOper,CurTerm);
  EvalOperation(LastTerm,CurTerm, LastOper , LastTerm);
  LastOper:=CurOper;
End;

PROCEDURE ParseExpression(Var Term:tAsmOperand);
 Var	Oper:tOperator;
Begin
 Oper:=coNONE;
 ReadExpression(Oper,Term);
 Term.Size:=SIZE_NONE;
End;

///////////////////////////////////////////////////////////////////////////////
// DIRECTIVES
///////////////////////////////////////////////////////////////////////////////

PROCEDURE ParseDB;
 Var	Value	:Longint;
    	DupValue:Longint;
Begin
 Repeat
  ReadToken;
  if (Token=TOK_IntConst) Then begin
     Value:=TokenInt; DupValue:=1;
     ReadToken;
     if (Token=TOK_Identifier)AND(TokenUpStr='DUP') Then
      Begin
        if (ReadToken<>TOK_LPAREN) Then MakeError('ParseDB',ExpectLPAREN);
        if (ReadToken<>TOK_IntConst) Then MakeError('ParseDB',ExpectINTCONST);
        DupValue:=Value;
        Value:=TokenInt;
        if (ReadToken<>TOK_RPAREN) Then MakeError('ParseDB',ExpectRPAREN);
      End;
     if Value>255 Then MakeError('ParseDB','Value out of range');
     CurSeg^.AddByte(Value, DupValue);

   End
   else if (Token=TOK_StrConst) Then
		Begin CurSeg^.AddString(TokenStr);ReadToken; End
   else MakeError('ParseDB','Expect Byte Data');
 Until (Token<>TOK_COMMA);
End;

PROCEDURE ParseDW;
 Var	Value		  : Longint;
    	DupValue	: Longint;
Begin
  Repeat
    ReadToken;
    if (Token=TOK_IntConst) Then begin
       Value:=TokenInt; DupValue:=1;
       ReadToken;
       if (Token=TOK_Identifier)AND(TokenUpStr='DUP') Then begin
          if (ReadToken<>TOK_LPAREN) Then MakeError('ParseDW',ExpectLPAREN);
          if (ReadToken<>TOK_IntConst) Then MakeError('ParseDW',ExpectINTCONST);
          DupValue:=Value;
          Value:=TokenInt;
          if (ReadToken<>TOK_RPAREN) Then MakeError('ParseDW',ExpectRPAREN);
       end;
       if Cardinal(Value)>$FFFF Then MakeError('ParseDW','Out of range');
       CurSeg^.AddWord(Value,DupValue);
    end
    else MakeError('ParseDW','DW error');
  Until (ReadToken<>TOK_COMMA);
End;

PROCEDURE ParseDD;
 Var	Value		  : Longint;
    	DupValue	: Longint;
      Sym       : pSymbol;
Begin
  Repeat
    Case ReadToken of
      TOK_IntConst: begin
        Value:=TokenInt; DupValue:=1;
        ReadToken;
        if (Token=TOK_Identifier)AND(TokenUpStr='DUP') Then begin
          if (ReadToken<>TOK_LPAREN) Then MakeError('ParseDD',ExpectLPAREN);
          if (ReadToken<>TOK_IntConst) Then MakeError('ParseDD',ExpectINTCONST);
          DupValue:=Value;
          Value:=TokenInt;
          if (ReadToken<>TOK_RPAREN) Then MakeError('ParseDD',ExpectRPAREN);
        end;
        CurSeg^.AddDword(Value,DupValue);
      end;

      TOK_IDENTIFIER: begin
    	  if TokenUpStr='OFFSET' Then begin
    			ReadToken;
          Sym:= SymGroup.FindSymbol(TokenUpStr);
          if (Sym=NIL) then begin
            New( Sym, Init(SYM_UNDEF, TokenUpStr, [SYM_FLAG_USED], NIL) );
            SymGroup.Head.PushSymbol(Sym);
          end
          else if (Sym.Kind=SYM_LABEL) then  else Sym.Flag:= Sym.Flag + [SYM_FLAG_USED];
          CurSeg.Relocs.PushReloc(REL_OFFSET,Sym,CurSeg.Size,0);
          Value:=0;
    			CurSeg^.AddDword(Value,1);
    			ReadToken;
  	    end
        else MakeError('ParseDD','Expect Byte Data');
      end;
      // else ?.
      else MakeError('ParseDD','DD error');
    end;
  Until (ReadToken<>TOK_COMMA);
End;

PROCEDURE ParseORG;
Begin
  ReadToken;
  if (Token=TOK_IntConst) Then begin
     if CurSeg^.Size>TokenInt then MakeError('ParseORG','Address Already Used');
     CurSeg^.Size:=TokenInt;
  End
  Else MakeError('ParseORG',ExpectINTCONST);
End;

PROCEDURE ParseINCLUDE;
 Var  Dir,Name,Ext:String;
      fScan  : pFileScanner;
Begin
  if ReadToken=TOK_STRCONST then
    SplitPath(TokenStr,Dir,Name,Ext);
    if Ext='' Then Ext:='.INC';
    if FileExists(DirSOURCE+Dir+Name+Ext)
      Then Dir:=DirSOURCE+Dir
      Else Dir:=DirINCLUDE+Dir;
    New( fScan, Init);
    if fScan^.OpenFile(Dir+Name+Ext) then begin
      PushScannerItem(fScan);
      ReadToken;
    end;
End;

PROCEDURE ParseEXTERN;
 Var Labl  : String;
     Sym   : pSymbol;
Begin
  ReadToken;
  if not IsTokenIdent Then MakeError('ParseEXTERN','Identifier expected');
  Labl:=TokenUpStr;{ Get The Name }
  Sym:=SymGroup.FindSymbol(Labl);
  if (Sym=NIL) then begin
    New( Sym, Init(SYM_UNDEF, Labl, [SYM_FLAG_EXTERN], NIL) );
    SymGroup.Head.PushSymbol(Sym);
  end
  else if (Sym.Kind=SYM_LABEL) then
    if SYM_FLAG_DEFINED in Sym.Flag then MakeError('ParseEXTERN','External Label Defined in current module')
    else Sym.Flag:= Sym.Flag + [SYM_FLAG_EXTERN]
  else MakeError('ParseEXTERN','External Label Defined in current module');
End;

PROCEDURE ParsePUBLIC;
 Var Labl  : String;
     Sym   : pSymbol;
Begin
  ReadToken;
  if not IsTokenIdent Then MakeError('ParsePUBLIC','Identifier expected');
  Labl:=TokenUpStr;{ Get The Name }
  Sym:=SymGroup.FindSymbol(Labl);
  if (Sym=NIL) then begin
    New( Sym, Init(SYM_UNDEF, Labl, [SYM_FLAG_PUBLIC],NIL) );
    SymGroup.Head.PushSymbol(Sym);
  end
  else if (Sym.Kind=SYM_LABEL)
    then Sym.Flag:=Sym.Flag+[SYM_FLAG_PUBLIC]
    else MakeError('ParsePUBLIC','Error in symbol declaration');
End;

procedure ParseIMPORT;
  Var
      Labl      : String;
      LibName   : String;
      ImportName: String;
      LibItem   : pLibraryItem;
      ImportItem:pImportItem;
      Sym       : pSymbol;
begin
  ReadToken;
  if not IsTokenIdent Then MakeError('ParseIMPORT','Identifier expected');
  Labl:=TokenUpStr;
  ImportName:=TokenStr;
  if (ReadToken<>TOK_COMMA) then MakeError('ParseIMPORT','"," expected');
  if (ReadToken<>TOK_Identifier) Then MakeError('ParseIMPORT','Identifier expected');
  if TokenUpStr<>'LIB' then MakeError('ParseIMPORT','');
  if (ReadToken<>TOK_COLON) then MakeError('ParseIMPORT','":" expected');
  if (ReadToken<>TOK_STRCONST) then MakeError('ParseIMPORT','');
  LibName:=TokenStr;

  if (ReadToken=TOK_COMMA) then begin
    if (ReadToken<>TOK_Identifier) Then MakeError('ParseIMPORT','Identifier expected');
    if TokenUpStr<>'NAME' then MakeError('ParseIMPORT','');
    if (ReadToken<>TOK_COLON) then MakeError('ParseIMPORT','":" expected');
    if (ReadToken<>TOK_STRCONST) then MakeError('ParseIMPORT','');
    ImportName:=TokenStr;
  end;

  LibItem:=App.LibStack.FindLib(LibName);
  if LibItem=NIL then begin
    App.LibStack.PushLib(LibName);
    LibItem:=App.LibStack.Head;
  end;

  ImportItem:=LibItem.FindImport(ImportName);
  if ImportItem=NIL then begin
    New( Sym, Init(SYM_UNDEF, Labl, [SYM_FLAG_IMPORT],NIL) );
    SymGroup.Head.PushSymbol(Sym);
    LibItem.PushImport(ImportName,-1,Sym);
    ImportItem:=LibItem.Head;
  end;
  SkipLine;
 //IMPORT ExitProcess:DWORD, lib:'kernel32.dll', Name:'exitprocess', Index:1
end;

procedure ParseALIGN;
 Var  Expr  : tAsmOperand;
begin
 ReadToken;
 ParseExpression(Expr);
 if (Expr.Loc<>LOC_IMMEDIATE) then MakeError('ParseALIGN','Integer constant expected');
 if (Expr.RefKind<>REF_NONE) then MakeError('ParseALIGN','Integer constant expected');
 // Check value
 if (Expr.Value>0)and(Expr.Value<1024) then CurSeg^.Align(Expr.Value);
 SkipLine;
end;

PROCEDURE ParseDIRECTIVE;
Begin
 Case tDirective(TokenInt) of
   DIR_DB: ParseDB;
   DIR_DW: ParseDW;
   DIR_DD: ParseDD;
   DIR_ORG: ParseORG;
   DIR_END: Finish:=True;
   DIR_INCLUDE: ParseINCLUDE;
   DIR_EXTERN: ParseEXTERN;
   DIR_PUBLIC: ParsePUBLIC;
   DIR_IMPORT: ParseIMPORT;
   DIR_ALIGN: ParseALIGN;
 End;
 SkipLine;
End;

PROCEDURE ParseIDENTIFIER;
 Var  Labl    : String;
      Expr    : tAsmOperand;
      Sym     : pSymbol;
      SymDef  : pSymbol;
      SymData : pDataTypeDef;

      SaveCurSeg  : pSegment;

Begin
  Labl:=TokenUpStr;{ Get The Name }
  Sym:=SymGroup.Head.FindSymbol(Labl);
  if (Sym<>NIL) then
   if (SYM_FLAG_DEFINED in Sym.Flag)
     then MakeError('ParseIDENTIFIER','Redeclared Identifier '+Labl);
  ReadToken;
  Case Token of
    TOK_IDENTIFIER: begin
      if (TokenUpStr='EQU') Then Begin { Symbol EQU Expression }
        ReadToken;
        ParseExpression( Expr );
        if (Expr.Loc=LOC_IMMEDIATE)AND(Expr.RefKind=REF_NONE) then begin
          if (Sym=NIL) then begin
            New( Sym, Init(SYM_UNDEF, Labl, [SYM_FLAG_DEFINED], NIL) );
            SymGroup.Head.PushSymbol(Sym);
          end;
          New( pDataValue(SymData), Init(Expr.Value) );
          Sym.Kind:=SYM_VALUE;
          Sym.Flag:=Sym.Flag+ [SYM_FLAG_DEFINED];
          Sym.Data:=SymData;
        end
        else begin
          if (Sym=NIL) then begin
            New( Sym, Init(SYM_UNDEF, Labl, [SYM_FLAG_DEFINED], NIL) );
            SymGroup.Head.PushSymbol(Sym);
          end;
          New( pDataEqual(SymData), Init(Expr) );
          Sym.Kind:=SYM_EQUAL;
          Sym.Flag:=Sym.Flag+ [SYM_FLAG_DEFINED];
          Sym.Data:=SymData;
        end;
        SkipLine;
      end
      else if (TokenUpStr='DB') Then begin { Symbol DB ? }
        if (Sym=NIL) then begin
          New( Sym, Init(SYM_UNDEF, Labl, [SYM_FLAG_DEFINED], NIL) );
          SymGroup.Head.PushSymbol(Sym);
        end;
        New( pDataLabel(SymData), Init(DEF_BYTE,NIL,CurSeg,CurSeg^.Size) );
        Sym.Kind:=SYM_LABEL;
        Sym.Flag:=Sym.Flag+ [SYM_FLAG_DEFINED];
        Sym.Data:=SymData;
	      ParseDB;
      end
      else if (TokenUpStr='DW') Then begin { Symbol DW ? }
        if (Sym=NIL) then begin
          New( Sym, Init(SYM_UNDEF, Labl, [SYM_FLAG_DEFINED], NIL) );
          SymGroup.Head.PushSymbol(Sym);
        end;
        New( pDataLabel(SymData), Init(DEF_WORD,NIL,CurSeg,CurSeg^.Size) );
        Sym.Kind:=SYM_LABEL;
        Sym.Flag:=Sym.Flag+ [SYM_FLAG_DEFINED];
        Sym.Data:=SymData;
	      ParseDW;
      end
      else if (TokenUpStr='DD') Then begin { Symbol DW ? }
        if (Sym=NIL) then begin
          New( Sym, Init(SYM_UNDEF, Labl, [SYM_FLAG_DEFINED], NIL) );
          SymGroup.Head.PushSymbol(Sym);
        end;
        New( pDataLabel(SymData), Init(DEF_DWORD,NIL,CurSeg,CurSeg^.Size) );
        Sym.Kind:=SYM_LABEL;
        Sym.Flag:=Sym.Flag+ [SYM_FLAG_DEFINED];
        Sym.Data:=SymData;
	      ParseDD;
      end
      
      else if (TokenUpStr='STRUCT') Then begin
        if (Sym<>NIL) then MakeError('ParseIDENTIFIER','Redeclared Structure '+Sym.Name);
        // New Struct : Segment, SymList
        New( pDataStruct(SymData), Init(Labl) );
        New( Sym, Init(SYM_TYPEDEF, Labl, [SYM_FLAG_DEFINED], SymData) );
        SymGroup.Head.PushSymbol(Sym);
        // Save Current Segment
        SaveCurSeg:=CurSeg;
        CurSeg:=@pDataStruct(SymData).Segment;
        SymGroup.PushItem(@pDataStruct(SymData).Symbols);
        SkipLine;
        while (Token<>TOK_EOF) do begin
          if (Token=TOK_IDENTIFIER) then
            if (Labl=TokenUpStr)or(TokenUpStr='ENDS') then begin // it's the end of the struct
              pDataStruct(SymData).Size:= CurSeg.Size;
              CurSeg:=SaveCurSeg;
              SymGroup.PopItem;
              SkipLine;
              Break;
            end;
          ParseLine;
        end;
      end

      else begin
        SymDef:=SymGroup.FindSymbol(TokenUpStr);
        if (SymDef<>NIL)and(SymDef.Kind=SYM_TYPEDEF) then
          Case pDataTypeDef(SymDef.Data).Def of
            DEF_STRUCT: begin
              // ADD SYMBOL
              if (Sym=NIL) then begin
                New ( Sym, Init(SYM_LABEL, Labl, [SYM_FLAG_DEFINED], NIL) );
                SymGroup.Head.PushSymbol(Sym);
              end;
              New( pDataLabel(SymData), Init(DEF_STRUCT,SymDef,CurSeg,CurSeg^.Size) );
              Sym.Kind:=SYM_LABEL;
              Sym.Flag:=Sym.Flag+ [SYM_FLAG_DEFINED];
              Sym.Data:=SymData;
              // WRITE DATA TO SEGMENT
              CurSeg^.AddBuffer( pDataStruct(SymDef.Data).Segment.Data[0] ,pDataStruct(SymDef.Data).Segment.Size, 1);
              SkipLine;
            end;
            else MakeError('ParseIDENTIFIER',ExpectCOLON)
          end
        else MakeError('ParseIDENTIFIER',ExpectCOLON);
      end;
    end;

    TOK_EQUAL: Begin { Symbol = NumberValue }
      ReadToken;
      ParseExpression(Expr);
      if (Expr.Loc<>LOC_IMMEDIATE)OR(Expr.RefKind<>REF_NONE) then MakeError('ParseIDENTIFIER',ExpectCONSTANT);
      if (Sym=NIL) then begin
        New ( Sym, Init(SYM_VALUE, Labl, [SYM_FLAG_DEFINED], NIL) );
        SymGroup.Head.PushSymbol(Sym);
      end;
      New( pDataValue(SymData), Init(Expr.Value) );
      Sym.Kind:=SYM_VALUE;
      Sym.Flag:=Sym.Flag+ [SYM_FLAG_DEFINED];
      Sym.Data:=SymData;
      SkipLine;
    end;

    TOK_COLON: Begin { Label : }
      if (Sym=NIL) then begin
        New ( Sym, Init(SYM_LABEL, Labl, [SYM_FLAG_DEFINED], NIL) );
        SymGroup.Head.PushSymbol(Sym);
      end;
      New( pDataLabel(SymData), Init(DEF_LABEL,NIL,CurSeg,CurSeg^.Size) );
      Sym.Kind:=SYM_LABEL;
      Sym.Flag:=Sym.Flag+ [SYM_FLAG_DEFINED];
      Sym.Data:=SymData;
     	ReadToken;
    end;
    else MakeError('ParseIDENTIFIER',ExpectCOLON);{LABEL}
  end; // CASE
End;


///////////////////////////////////////////////////////////////////////////////
// 
///////////////////////////////////////////////////////////////////////////////

PROCEDURE ParseAPPTYPE;
Begin
 ReadToken;
 if (Token<>TOK_Identifier) Then MakeError('ParseAPPTYPE','Identifier expected');
 if TokenUpStr='CONSOLE' then App.AppType:=APP_CONSOLE
 else if TokenUpStr='GUI' then App.AppType:=APP_GUI
 else MakeError('ParseAPPTYPE','Unknown App Type');
End;

PROCEDURE ParseMODULE;
Begin
  ReadToken;
  if TokenFlag([TOK_FLAG_MODULE])<>TOK_MODULE then MakeError('ParseMODULE','Module Expected');
  Case tModule(TokenInt) of
    MOD_MODEL: begin
      ReadToken;
      { get Model Type }
      ReadToken;
    end;
    MOD_CODE: CurSeg:=@App.SecCODE;
    MOD_DATA: CurSeg:=@App.SecDATA;
    MOD_APPTYPE: ParseAPPTYPE;
    else MakeError('ParseMODULE','');
  end;
  SkipLine;
End;

///////////////////////////////////////////////////////////////////////////////
//
///////////////////////////////////////////////////////////////////////////////

PROCEDURE ParseOPERAND(Var Arg:tAsmOperand);
 Const
    DataSize: Array[0..4] of Record N:String[5];S:tDataSize; end = (
      (N:'BYTE';S:SIZE_BYTE),(N:'WORD';S:SIZE_WORD), (N:'DWORD';S:SIZE_DWORD),
      (n:'SHORT';S:SIZE_SHORT),(n:'NEAR';S:SIZE_NEAR)
    );

 Var  Size: tDataSize;
      i:integer;
Begin
 Size:=SIZE_NONE;
 if Token=TOK_IDENTIFIER then
   for i:=0 to 4 do
    if TokenUpStr=DataSize[i].N then begin
      Size:=DataSize[i].S;
      if ReadToken=TOK_IDENTIFIER then
        if TokenUpStr='PTR' then ReadToken;
      Break;
    end;
 ParseExpression(Arg);
 if Size<>SIZE_NONE then Arg.Size:=Size;
End;

///////////////////////////////////////////////////////////////////////////////
PROCEDURE ParseOPERANDS;
Begin
 NumArg:=0;
 ReadToken;
 if (Token=TOK_EOLN) Then Exit;
 ParseOPERAND(Inst.Op1);
 if (Inst.Op1.Loc<>LOC_NONE) Then Begin
   Inc( NumArg );
   if (Token=TOK_COMMA) Then
    Begin
      ReadToken;
      ParseOPERAND(Inst.Op2);
      if (Inst.Op2.Loc=LOC_NONE) Then MakeError('ParseOPERANDS','Argument Syntax Error');
      Inc( NumArg );
      if (Token=TOK_COMMA) Then
       Begin
         ReadToken; ParseOPERAND(Inst.Op3);
         if (Inst.Op3.Loc=LOC_NONE) Then MakeError('ParseOPERANDS','Argument Syntax Error');
         Inc( NumArg );
       End;
   End;
  End;
End;

///////////////////////////////////////////////////////////////////////////////
procedure CheckRelocation(Const Op:tAsmOperand; top:tTableOperandLocation);
 Var Rel:TRelocKind;
begin
 Case Op.Loc of
  LOC_IMMEDIATE:
    if (Op.RefKind<>REF_NONE) then begin
      Case top of
        coJRS: Rel:=REL_SHORT; { SHORT Immediate relative byte offset (for jumps)}
        coJRN: Rel:=REL_NEAR;  { NEAR Immediate relative full offset (for jumps)}
        else Rel:=REL_OFFSET;
      end;
      CurSeg^.Relocs.PushReloc( Rel, Op.RefSym, CurSeg^.Size+DumpOfs.Immediate, Op.Value);
    end;
  LOC_MEMORY:
    if (Op.RefKind<>REF_NONE) then
      CurSeg^.Relocs.PushReloc( REL_OFFSET, Op.RefSym, CurSeg^.Size+DumpOfs.Disp, Op.Value);
 end;
end;

///////////////////////////////////////////////////////////////////////////////
PROCEDURE ParseLINE;{ Only One Instruction Per Line }
Begin
 Case Token of
    TOK_EOLN : ReadToken;
    TOK_EOF: Finish:=True;
    TOK_POINT : ParseMODULE;
    TOK_IDENTIFIER: begin
      Case TokenFlag([TOK_FLAG_OPCODE,TOK_FLAG_DIRECTIVE,TOK_FLAG_PREFIX]) of
        TOK_OPCODE: begin
          Inst.Opcode:=tAsmOpcode(TokenInt);
          ParseOPERANDS;
          Inst.SearchTable;
          if (Inst.TableRow<>-1) then begin
            Inst.DumpCode;
            CheckRelocation(Inst.Op1, InstTable[Inst.TableRow].O1);
            CheckRelocation(Inst.Op2, InstTable[Inst.TableRow].O2);
            CheckRelocation(Inst.Op3, InstTable[Inst.TableRow].O3);
            if Listing Then begin
              Write( fLst,CurSeg^.Name,':', RtJustify( Hex16(CurSeg^.Size),8));
              Writeln( fLst, RtJustify(InstructionToStr(Inst),40) ,' DUMP = ',BytesToHex( Inst.Code.Dump, Inst.Code.Size,' ') );
            end;
            if (Inst.Code.Size>0) Then CurSeg^.AddBuffer(Inst.Code.Dump ,Inst.Code.Size ,1);
            Inst.init;
            SkipLine;
          end
          else MakeError('ParseLINE','unknown instruction');
        end;
        TOK_DIRECTIVE: ParseDIRECTIVE;
        TOK_PREFIX: begin
          Case tAsmPrefix(TokenInt) of
            PRE_LOCK..PRE_REPZ: Inst.PrefixLockRepeat:=tAsmPrefix(TokenInt);
            PRE_SegES..PRE_SegGS: Inst.PrefixSegment:=tAsmPrefix(TokenInt);
          end;
          ReadToken;
        end;
        else ParseIDENTIFIER;
      end;
    end;
    else begin
      ReadToken;
      MakeError('ParseLINE','Special Character '+TokenStr);
    end;
  end;
End;


///////////////////////////////////////////////////////////////////////////////
PROCEDURE Compile;
Begin
  Finish:=False;
  Inst.Init;
  ReadToken;
  while not(finish) do ParseLINE;
End;

end.


