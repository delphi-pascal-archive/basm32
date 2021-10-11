{-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-}
{-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-* Win-Assembler *+*-*+*-*+*-*+*-*+*-*+*-*+*-}
{-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-}

Unit Segments;

INTERFACE

Uses ObjList, ObjBuffer, Symbols, RelocationStack, PEHeader;

TYPE
  tSegmentKind = (
        SEG_NONE, SEG_CODE, SEG_IDATA, SEG_CONST, SEG_BSS, SEG_DATA, SEG_RES
  );

  PSegment = ^TSegment;
  TSegment = Packed Object(TBuffer)
        Next    : pSegment;
        Kind    : TSegmentKind;
        Name    : String[12]; { Segment name }
        Relocs  : TRelocationStack;  // Own Relocations
      // Address in the memory
        VirtualAddress : Integer;
        procedure Init(Del:Integer; AKind:TSegmentKind; AName:TSymbolName);
        procedure Done;
        procedure Align( a:Integer);

        procedure SaveToFile( Var F:File; From:WORD);
        procedure SetFixups;
        procedure SetLocalFixups;

  End;

  pSegmentStack = ^tSegmentStack;
  tSegmentStack = Object
        Head    : pSegment;
        Constructor Init;
        Destructor Done;
        procedure PushItem( NewItem:pSegment );
        procedure PopItem;
  end;

IMPLEMENTATION


////////////////////////////////////////////////////////////////////////////////
// TSegment
////////////////////////////////////////////////////////////////////////////////
procedure TSegment.Init(Del:Integer; AKind:TSegmentKind; AName:TSymbolName);
begin
 inherited Init(Del);
 Kind:=AKind;
 Name:=AName;
 Relocs.Init;
 VirtualAddress:=0;
end;

procedure TSegment.Done;
begin
 Kind:=SEG_NONE;
 Name:='';
 Relocs.Done;
 inherited Done;
end;

procedure TSegment.Align( a:Integer);
 Var i:Integer;
begin
  i := a - ( Size mod a );
  AddByte( 0,i );
end;

procedure TSegment.SaveToFile( Var F:File; From:WORD);
Begin
 if From > Size then Exit;
 BlockWrite(F, Data[From], Size-From );
End;


procedure TSegment.SetFixups;
Var
    Sym     : pSymbol;
    Sec     : pSegment;
    Item    : pRelocationItem;
    Address : Integer;
    Diff    : Integer;
    SymData : pDataLabel;
Begin
  Item:=Relocs.Head;
  while (Item<>NIL) do begin
    Sym:=Item^.Sym;
    if (Sym^.Kind=SYM_LABEL)AND(SYM_FLAG_DEFINED in Sym^.Flag) then begin
      SymData:=Sym.Data;
      Sec:=SymData^.OwnSeg;
      Case Item^.Kind of
        REL_OFFSET:	Begin
          Address:= Sec^.VirtualAddress + SymData.OwnOfs + Item^.Shift;
          SetDword(Item^.PatchOfs ,Address, 1);
        End;
        REL_SHORT: Begin
          Diff:= (Sec.VirtualAddress+SymData.OwnOfs+Item^.Shift) - (VirtualAddress+Item^.PatchOfs+1); // Destination - Source
          //if (n>127)or(n<-128) Then MakeError('Short Jump Size to '+Sym.Name+' at '+hex16(Item^.PatchOfs));
          SetByte(Item^.PatchOfs,Diff,1);
        End;
        REL_NEAR: Begin
          Diff:= (Sec.VirtualAddress+SymData.OwnOfs+Item^.Shift) - (VirtualAddress+Item^.PatchOfs+4); // Destination - Source
          // if (N<-32768)OR(N>32767) Then MakeError('Invalid Near Jump to Target Label '+Sym.Name);
          SetDword(Item^.PatchOfs,Diff,1);
        End;
      End;{Case}
      Relocs.DelReloc( Item );
    end else Item:=Item^.Next;
 End; {for}
END;

procedure TSegment.SetLocalFixups;
Var
    Sym     : pSymbol;
    SymData : pDataLabel;
    Item    : pRelocationItem;
    Diff    : Integer;
    SelfSeg : pSegment;
Begin
  SelfSeg:=@Self;
  Item:=Relocs.Head;
  while (Item<>NIL) do begin
    Sym:=Item^.Sym;
    SymData:=Sym.Data;
    if (Item^.Kind<>REL_OFFSET)AND
     (Sym^.Kind=SYM_LABEL)AND(SYM_FLAG_DEFINED in Sym^.Flag) AND
      (SymData.OwnSeg=SelfSeg) then begin
        Case Item^.Kind of
          REL_SHORT: Begin
            Diff:= (SymData.OwnOfs+Item^.Shift) - (Item^.PatchOfs+1); // Destination - Source
            //if (n>127)or(n<-128) Then MakeError('Short Jump Size to '+Sym.Name+' at '+hex16(Item^.PatchOfs));
            SetByte(Item^.PatchOfs,Diff,1);
          End;
          REL_NEAR: Begin
            Diff:= (SymData.OwnOfs+Item^.Shift) - (Item^.PatchOfs+4); // Destination - Source
            // if (N<-32768)OR(N>32767) Then MakeError('Invalid Near Jump to Target Label '+Sym.Name);
            SetDword(Item^.PatchOfs,Diff,1);
          End;
        End;{Case}
        // delete fixup
        Relocs.DelReloc( Item );
      end else Item:=Item^.Next;
  end; {while}
END;


////////////////////////////////////////////////////////////////////////////////
// tSegmentStack
////////////////////////////////////////////////////////////////////////////////

Constructor tSegmentStack.Init;
begin
  Head:=NIL;
end;

Destructor tSegmentStack.Done;
begin
end;

procedure tSegmentStack.PushItem( NewItem:pSegment );
begin
  NewItem.Next:=Head;
  Head:=NewItem;
end;

procedure tSegmentStack.PopItem;
 Var    Item : pSegment;
begin
  if Head=NIL then Exit;
  Item:=Head.Next;
  Head:=Item;
end;


end.

