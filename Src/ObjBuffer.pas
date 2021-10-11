{-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-}
{-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-* Win-Assembler *+*-*+*-*+*-*+*-*+*-*+*-*+*-}
{-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-}

Unit ObjBuffer;

INTERFACE

TYPE
{############################################################################}
{############################################################################}
  TBuffer = Packed OBJECT
        Delta   : Integer;
        Data    : Array of Byte;
        Size    : Longint;
        procedure AllocData(NewSize:Integer);
        procedure Init(ADelta:Integer);
        procedure Done;{ Same as Reset }
        procedure AddBuffer(Const Buf; BufSize,Times:Integer);
        procedure AddString(Const S:String);
        procedure AddByte( b:byte; Times:Integer);
        procedure AddWord( w:Word; Times:Integer);
        procedure AddDword( d:Cardinal; Times:Integer);
        procedure SetBuffer(Loc:Integer;Const Buf; BufSize,Times:Integer);
        procedure SetString(Loc:Integer;Const S:String;Times:Integer);
        procedure SetByte(Loc:Integer; B:Byte; Times:Integer);
        procedure SetWord(Loc:Integer; W:Word; Times:Integer);
        procedure SetDword(Loc:Integer; D:Cardinal; Times:Integer);
  End;

IMPLEMENTATION

procedure TBuffer.Init(ADelta:Integer);
Begin
 Delta:=Adelta;
 SetLength(Data,0);
 Size:=0;
End;

procedure TBuffer.Done;
Begin
 if (Data<>NIL) Then SetLength(Data,0);
 Data:=NIL;
 Size:=0;
End;

procedure TBuffer.AllocData(NewSize:Integer);
 Var  NewData : Array of Byte;
begin
  NewSize:= ((NewSize div Delta)+1)*Delta;
  SetLength(NewData, NewSize); //Delta );
  Move( Data[0], NewData[0], Size);
  SetLength(Data,0);
  Data:=Pointer(NewData);
End;

procedure TBuffer.AddBuffer(Const Buf; BufSize,Times:Integer);
 Var
      NewSize : Integer;
      i       : Integer;
Begin { add a buffer at the end of the section }
  NewSize:=Size+(BufSize*Times);
  if NewSize>Length(Data) then AllocData(NewSize);
  for i:=1 To Times Do begin
    Move(Buf,Data[Size], BufSize);
    Inc(Size, BufSize);
  end;
End;

procedure TBuffer.AddString(Const S:String);
Begin { Add a string at the end of the section }
 AddBuffer(S[1],Length(S), 1);
End;

procedure TBuffer.AddByte( b:byte; Times:Integer);
Begin
 AddBuffer(B,1, Times);
End;

procedure TBuffer.AddWord( w:Word; Times:Integer);
Begin
 AddBuffer(W,2, Times);
End;

procedure TBuffer.AddDword( d:Cardinal; Times:Integer);
Begin
 AddBuffer(D,4, Times);
End;

procedure TBuffer.SetBuffer(Loc:Integer;Const Buf; BufSize,Times:Integer);
 Var
      i       : Integer;
      NewSize : Integer;
Begin
  NewSize:=Loc+(BufSize*Times);
  if NewSize>Length(Data) then AllocData(NewSize);
  for i:=1 To Times Do begin
    Move(Buf,Data[Loc], BufSize);
    Inc(Loc, BufSize);
  end;
  if NewSize>Size then Size:=NewSize;
End;

procedure TBuffer.SetString(Loc:Integer;Const S:String;Times:Integer);
Begin { set a string at the location within the section }
  SetBuffer(Loc, s[1], Length(S), Times);
End;

procedure TBuffer.SetByte(Loc:Integer; B:Byte; Times:Integer);
Begin { set a string at the location within the section }
  SetBuffer(Loc, B, 1, Times);
End;
procedure TBuffer.SetWord(Loc:Integer; W:Word; Times:Integer);
Begin { set a string at the location within the section }
  SetBuffer(Loc, W, 2, Times);
End;
procedure TBuffer.SetDword(Loc:Integer; D:Cardinal; Times:Integer);
Begin { set a string at the location within the section }
  SetBuffer(Loc, D, 4, Times);
End;

end.