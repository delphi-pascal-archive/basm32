{-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-}
{-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-* Win-Assembler *+*-*+*-*+*-*+*-*+*-*+*-*+*-}
{-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-}

Unit ObjCodeDump;

INTERFACE

TYPE
  TCodeDump	= Packed Object
        Size    :Integer;
        Dump    :Array[0..31]of BYTE;
        PROCEDURE Init;
        Procedure AddByte(N:BYTE);
        PROCEDURE AddWord(N:WORD);
        procedure AddDword(N:Integer);
        procedure AddBuffer(Const Buf;Count:Integer);
        procedure AddCodeDump( Const cd:tCodeDump );
  End;

IMPLEMENTATION

PROCEDURE TCodeDump.Init;
Begin
 Size:=0;
End;

Procedure TCodeDump.AddByte( n:BYTE );
Begin
 Move( N, Dump[Size], 1);
 Inc(Size,1);
End;

PROCEDURE TCodeDump.AddWord( n:WORD );
begin
 Move(N,Dump[Size],2);
 Inc(Size,2);
End;

PROCEDURE TCodeDump.AddDword(N:Integer);
begin
 Move(N,Dump[Size],4);
 Inc(Size,4);
End;

PROCEDURE TCodeDump.AddBuffer(Const Buf;Count:Integer);
begin
 Move(Buf,Dump[Size],Count);
 Inc(Size,Count);
End;

procedure TCodeDump.AddCodeDump( Const cd:tCodeDump );
begin
 Move( cd.Dump[0], Dump[Size], cd.Size);
 Inc(Size, cd.Size);
end;

end.
