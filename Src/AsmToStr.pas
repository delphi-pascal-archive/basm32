{-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-}
{-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-* Win-Assembler *+*-*+*-*+*-*+*-*+*-*+*-*+*-}
{-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-}

UNIT AsmToStr;
//	lods	byte [esi]
//	stos	byte [edi]

INTERFACE

USES  Convert,ObjCodeDump, Symbols, Assemble;

Function OperandToStr(Size:tDataSize; Const Op:tAsmOperand):String;
function InstructionToStr(Const Inst:tAsmInstruction):String;

IMPLEMENTATION

///////////////////////////////////////////////////////////////////////////////
// OperandToStr(Size:tDataSize; Const Op:tAsmOperand):String;
///////////////////////////////////////////////////////////////////////////////


Function typecast( Size:tDataSize):String;
begin
  typecast:='';
  case Size of
    SIZE_BYTE:typecast:='Byte Ptr ';
    SIZE_WORD:typecast:='Word Ptr ';
    SIZE_DWORD:typecast:='Dword Ptr ';
  end;
end;

function jmptype(Size:tDataSize):string;
begin
 jmptype:='';
 case Size of
  SIZE_BYTE:jmptype:='Short Ptr ';
  SIZE_WORD:jmptype:='Near Ptr ';
  SIZE_DWORD:jmptype:='Far Ptr ';
 end;
end;

function RefToStr(Ref:tRefKind):string;
begin
  case Ref of
    REF_NONE,REF_FIXUP: RefToStr:='';
    REF_OFS: RefToStr:='Offset ';
    REF_NEAR: RefToStr:='Near ';
    REF_SHORT: RefToStr:='Short ';
    REF_FAR: RefToStr:='Far ';
  end;
end;

Function OperandToStr(Size:tDataSize; Const Op:tAsmOperand):String;
 Var  S     : String;
      First : Boolean;
Begin
  S:='';
  Case Op.Loc Of
    LOC_REGISTER: OperandToStr:=AsmRegisterName[ Op.Reg ];
    LOC_IMMEDIATE: begin
      if Op.RefKind<>REF_NONE then begin
        S:=RefToStr(Op.RefKind)+ pSymbol(Op.RefSym).Name;
        if Op.Value<>0 then S:=S+'+ 0'+Hex(Op.Value)+'h';
      end else S:='0'+Hex(Op.Value)+'h';
      OperandToStr:=S;
    end;
    LOC_MEMORY: begin   // [ BASE + INDEX + Disp ]
      S:=typecast(Size)+'[';
      First:=True;
      if (Op.Base<>REG_NONE) Then begin
        S:=S+AsmRegisterName[Op.Base];
        First:=False;
      end;
      if (Op.Index<>REG_NONE) Then begin
        if Not First then S:=S+'+'+AsmRegisterName[Op.Index] else S:=S+AsmRegisterName[Op.Index];
        if (Op.Scale<>0) Then S:=S+'*'+Hex8(Op.Scale);
        First:=False;
      end;
      if Op.RefKind<>REF_NONE then begin
        if Not First then S:=S+'+';
        S:=S+RefToStr(Op.RefKind)+ pSymbol(Op.RefSym).Name;
        First:=False;
      end;
      if (Op.Value<>0)or First Then begin
        if Not First then S:=S+'+';
        S:=S+'0'+Hex(Op.Value)+'h';
        First:=False;
      end;
      OperandToStr:=S+']';
    End;
  else OperandToStr:='';
 End;
End;



function InstructionToStr(Const Inst:tAsmInstruction):String;
Var S:String;
Begin
  Result:='';
  with Inst do begin
    if OpCode=OP_NONE Then Exit;
    S:='';//Justify(Dump, 12);
    if PrefixLockRepeat<>PRE_NONE Then S:=S+AsmPrefixName[PrefixLockRepeat]+' ';
    S:=S+AsmOpcodeName[opcode];
    if Op1.Loc<>LOC_NONE Then S:=S+' '+OperandToStr(DataSize, Op1);
    if Op2.Loc<>LOC_NONE Then S:=S+', '+OperandToStr(DataSize, Op2);
    if Op3.Loc<>LOC_NONE Then S:=S+', '+OperandToStr(DataSize, Op3);
  end;
  Result:=S;
End;

end.



