{-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-}
{-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-* Win-Assembler *+*-*+*-*+*-*+*-*+*-*+*-*+*-}
{-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-}
// Written by Basssem

USES	SysUtils, Convert, StrTools,
      ObjList, ObjStack, ObjCodeDump, ObjBuffer,
      ComLine, Symbols, Symbols2, Segments, Asmtostr, Assemble, RelocationStack, Scanner, Parser, Global, PeLinker;

{$i Switches.inc}

{$APPTYPE CONSOLE}

PROCEDURE Done;
Begin
  Close(fLst);
  DoneScanner;
//
  App.Symbols.Done;
  App.SecCODE.Done;
  App.SecDATA.Done;
  App.SecBSS.Done;
  App.SecIDATA.Done;
  App.LibStack.PopAll;
  App.EntryPoint:=0;
  ///SymGroup.Done; not needed  
  Readln;
  Halt;
End;


PROCEDURE Init;
 Var fScan  : pFileScanner;
Begin
  Listing:=True;
  if Listing Then Begin
    Assign(fLst, fLstName);
	  Rewrite(fLst);
	  Writeln(fLst,ProgVersion);
	  Writeln(fLst,fAsmName);
	  Writeln(fLst);
  End;

  // Initialize Data Structures
  InitScanner;

  New( fScan, Init);
  fScan^.OpenFile(fAsmName);
  PushScannerItem(fScan);
  ReadChar;

  App.Symbols.Init;
  App.SecCODE.Init(1024,SEG_CODE,'CODE');
  App.SecDATA.Init(1024,SEG_DATA,'DATA');
  App.SecBSS.Init(1024,SEG_BSS,'BSS');
  App.SecIDATA.Init(1024,SEG_IDATA,'IDATA');
  App.LibStack.Init;
  App.AppType:=APP_CONSOLE;

  SymGroup.Init;
  SymGroup.PushItem(@App.Symbols);
  CurSeg:=@App.SecCODE;
  AppExitProc:=Done;
End;


PROCEDURE MakeError(Const n:String);
Begin
  WriteLN;
  WriteLN(' Error: ', n );
  if Listing Then Writeln(fLst,' Error: ',n);
  Done;
End;

PROCEDURE PrintSymbol(Sym:pSymbol);
 Var  SymData : pointer;
Begin
  SymData:=Sym.Data;
  Case Sym.Kind of
    SYM_LABEL:
        if (SYM_FLAG_USED in Sym.Flag) then begin
          if SYM_FLAG_IMPORT in Sym.Flag then Write(fLst,'IMPORT')
          else if SYM_FLAG_EXTERN in Sym.Flag then Write(fLst,'EXTERN')
          else if SYM_FLAG_PUBLIC in Sym.Flag then Write(fLst,'PUBLIC');
          Write(fLst,#9,RtJustify(Sym.Name,20));
          if (SYM_FLAG_DEFINED in Sym.Flag) then
            Writeln(fLst, TDefinitionKindName[pDataLabel(SymData).Def], #9, Hex32( pDataLabel(SymData).OwnOfs+ pSegment(pDataLabel(SymData).OwnSeg).VirtualAddress ) )
          else Writeln(fLst,' UNDEFINED AND USED');
        end;
    SYM_VALUE:
      ;//WriteLN(fLst,#9,RtJustify(Sym.Name,20),' ',TSymbolKindName[Sym.Kind],' = ', Hex32(pDataValue(SymData).Value),'H' );
    SYM_EQUAL:
      ;//WriteLN(fLst,#9,RtJustify(Sym.Name,20),' ',TSymbolKindName[Sym.Kind], #9, OperandToStr(SIZE_NONE,pDataEqual(SymData).Expr) );
    SYM_TYPEDEF:
      ;//WriteLN(fLst,#9,RtJustify(Sym.Name,20),' ',TSymbolKindName[Sym.Kind], #9 );
  end;
end;

PROCEDURE PrintSymbolStack(SymStack:pSymbolStack);
 Var
      Sym : pSymbol;
Begin
  Sym:= SymStack.Head;
  while (Sym<>NIL) do begin
    PrintSymbol( Sym );
    Sym:=Sym.Next;
  End;
end;

PROCEDURE PrintSymbolLength(SymLength:pSymbolLength);
 Var Len:integer;
begin
  for Len:=0 to 128 do PrintSymbolStack( @SymLength.SymbolStack[Len] );
end;

{##########################################################################}
				{ MAIN }
{##########################################################################}
Var Lnk : tPELinker;
BEGIN
 { Initialize }
  Init;

// Pass1
  Writeln;
  Writeln('Assembling file "',fAsmName,'"' );
  WriteLN('+Pass 1 ');
  Compile;
  App.SecCODE.SetLocalFixups;
  App.SecDATA.SetLocalFixups;
  if Not(finish) Then Writeln('Warning: Unexpected end of file!');
  Writeln(' -> ok');

// Pass2
  WriteLN('+Pass 2 ');

//  ComLinker;
  Lnk.Link(fExeName);
 { Terminate }
  if Listing=TRUE Then Begin
    Writeln(fLst,#13#10'Symbols:');
    PrintSymbolLength(@App.Symbols);
  end;
  Done;
END.






