{-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-}
{-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-* Win-Assembler *+*-*+*-*+*-*+*-*+*-*+*-*+*-}
{-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-}

Unit RelocationStack;

INTERFACE

TYPE
  TRelocKind =( REL_NONE, REL_OFFSET, REL_SHORT, REL_NEAR, REL_FAR );

 {* Relocation entries *}
  pRelocationItem = ^tRelocationItem;
  tRelocationItem = RECORD
        Next        : pRelocationItem;
        Prev        : pRelocationItem;
     // Data
        Kind        : tRelocKind;
       // Source Symbol
        OwnSec      : Pointer;      // symbol own section
        Sym         : Pointer;      // Symbols that doesn't have yet an offset

       // Dest. Patch
         // PatchSeg    : Pointer;
        PatchOfs    : Integer;      // Offset within the target segment
        Shift       : LongInt;      // Effective Address Adjuster of the result value
  End;

  TRelocationStack = Object
        Head    : pRelocationItem;
        procedure Init;
        procedure Done;
        procedure PushReloc(AKind:tRelocKind; ASym:Pointer; APatchOfs,AShift: Integer);
        procedure PopReloc;
        procedure DelReloc(Var Rel: pRelocationItem );
  end;


IMPLEMENTATION

{##############################################################################}
{##############################################################################}
procedure TRelocationStack.Init;
Begin
  head := nil;
End;

procedure TRelocationStack.Done;
begin
  while (Head<>NIL) do PopReloc;
end;


procedure TRelocationStack.PushReloc(AKind:tRelocKind; ASym:Pointer; APatchOfs,AShift: Integer);
 Var    NewItem : pRelocationItem;
begin
  GetMem( NewItem, Sizeof(TRelocationItem) );
  with NewItem^ do begin
    Kind:=AKind;
    Sym:=ASym;
    PatchOfs:=APatchOfs;
    Shift:=AShift;
  end;
  if Head<>NIL then Head^.Prev:=NewItem;
  NewItem^.Next:=Head;
  NewItem^.Prev:=NIL;
  Head := NewItem;
end;

procedure TRelocationStack.PopReloc;
 Var    Item : pRelocationItem;
begin
  if (Head=NIL) then Exit;
  Item:=Head.Next;
  FreeMem( Head, Sizeof(TRelocationItem) );
  Head:=Item;
  if (Head<>NIL) then Head.Prev:=NIL;
end;

procedure TRelocationStack.DelReloc(Var Rel: pRelocationItem );
 Var  Prev,Next : pRelocationItem;
begin
  if Rel=NIL then Exit;
  Prev:=Rel.Prev;
  Next:=Rel.Next;
  if Prev<>NIL then Prev.Next:=Next;
  if Next<>NIL then Next.Prev:=Prev;
  if (Head=Rel) then Head:=Next;
  FreeMem( Rel, Sizeof(TRelocationItem) );
  Rel:=Next;
end;


end.
