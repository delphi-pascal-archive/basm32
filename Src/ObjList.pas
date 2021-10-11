{-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-}
{-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-* Win-Assembler *+*-*+*-*+*-*+*-*+*-*+*-*+*-}
{-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-}

UNIT ObjList; { The List Object }

INTERFACE

TYPE TList = Packed Object
        List    :pPointerArray;
        Count   :Longint;
        Capacity:Longint;
        Delta   :Longint;
        PROCEDURE CreateList( Size,del:Integer );
        PROCEDURE DestroyList;
        function Add( Item:Pointer ):Integer;
        function Get( Index:Integer ):Pointer;
        function IndexOf( Item: Pointer ):Integer;
 End;

IMPLEMENTATION
                                { TLIST }

PROCEDURE FillDWord1(VAR Dest;Count:Longint;Data:LongInt);Assembler;
Asm
        MOV     EAX,Data
        MOV     EDI,Dest
        MOV     ECX,Count
        CLD
        REP     STOSD
End;


PROCEDURE TList.CreateList( Size,Del:Integer );
Begin
 if (Size>0)
   then begin
     GetMem( List, Size*Sizeof(Pointer) );
     FillChar( List^, Size*Sizeof(Pointer), 0);
    end
   else List:=NIL;
 Capacity:=Size;
 Count:=0;
 Delta:=Del;
End;

PROCEDURE TList.DestroyList;
Begin
 if (List<>NIL) Then FreeMem( List, Capacity*Sizeof(Pointer) );
 List:=NIL;
 Capacity:=0;
 Count:=0
End;

Function TList.ADD( Item: Pointer ): Integer;
 Var    NewList :pPointerArray;
begin
  ADD := Count;
  if Count=Capacity Then
   begin
     Writeln(' Getmem ',(Capacity+Delta)*Sizeof(Pointer));

     GetMem( NewList, (Capacity+Delta)*Sizeof(Pointer) );
     if (List<>NIL) Then
      Begin
       Move(List^,NewList^,Capacity*Sizeof(Pointer));
       FreeMem( List, Capacity*Sizeof(Pointer) );
       Writeln(' freemem');
      End;
     Inc(Capacity, Delta);
     List:=NewList;
   End;
  List^[ Count ] := Item;
  Inc(Count);
End;

function TList.Get(Index: Integer): Pointer;
Begin
  if (Index < 0) or (Index >= Count)
        Then Get:=NIL
        Else Get:=List^[Index];
End;

function TList.IndexOf(Item: Pointer): Integer;
 Var Rt:integer;
Begin
  Rt := 0;
  while (Rt < Count) and (List^[Rt] <> Item) do Inc(Rt);
  if Rt = Count then Rt := -1;
  IndexOf:=Rt;
End;

END.
