{-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-}
{-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-* Win-Assembler *+*-*+*-*+*-*+*-*+*-*+*-*+*-}
{-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-}

UNIT ObjStack;

INTERFACE

TYPE


 { The Last Item entred is the first in the list }
 PStackItem = ^TStackItem;
 TStackItem = record
	Next: PStackItem;
	Data: Pointer;
 End;

 PStack = ^TStack;
 TStack = object
	  Head	: PStackItem;
	  PROCEDURE Init;
    PROCEDURE PUSH( NewData:Pointer );
	  FUNCTION POP : Pointer;
	  FUNCTION TOP : Pointer;
	  FUNCTION isEmpty: boolean;
 End;


IMPLEMENTATION

PROCEDURE TStack.init;
Begin
  head := nil;
End;

PROCEDURE TStack.PUSH( NewData:Pointer );
 Var NewItem: PStackItem;
Begin
  New( NewItem );
  NewItem^.Data := NewData;
  NewItem^.Next := Head;
  Head := NewItem;
End;

FUNCTION TStack.POP: pointer;
 Var Item: PStackItem;
begin
  if (Head<>NIL) Then
    Begin
	POP := Head^.Data;
	Item := Head^.next;
	Dispose(Head);
	Head := Item;
    End Else POP:=NIL;
end;

FUNCTION TStack.Top: pointer;
Begin
  if (Head<>NIL)
	Then Top := head^.data
	Else Top := NIL;
End;

FUNCTION TStack.isEmpty: boolean;
Begin
  isEmpty := Head=NIL;
End;


END.




