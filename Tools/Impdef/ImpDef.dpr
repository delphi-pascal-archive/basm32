PROGRAM Impdef;

{$APPTYPE CONSOLE}

uses
     SysUtils, PeHeader, Convert, PathTools;


Var
    	EXEhead	:tEXEheader;
    	PEhead  :tPEheader;
    	OptHeader:tPEoptheader;
      fExe    : File;
      fDef  : Text;


Var
    ExportRva : Integer;
    ExportSize: Integer;
    SectionOfs: Integer;
    Section   : tPESection;

Var
    fName:String;
    DefName: String;


function ReadHeader:Boolean;
Begin
  Result:=False;
  {Verify file Size}
  if FileSize(fExe)<SizeOf(tEXEheader) then Exit;
  BlockRead(fExe, EXEHead, SizeOf(tEXEheader));
  if (EXEhead.Signature<>'MZ')AND(EXEhead.Signature<>'ZM') Then Exit;
 {PEheader}
  if EXEhead.OfsReloc<>$0040 then Exit;
  if (EXEhead.OfstPEheader<SizeOf(tEXEheader)) or (EXEhead.OfstPEheader>=FileSize(fExe)) then Exit;
  Seek (fExe, EXEhead.OfstPEheader);
  BlockRead (fExe, PEhead, SizeOf(tPEheader));
  if PEhead.Magic<>'PE'#0#0 Then Exit;
  BlockRead (fExe, OptHeader, SizeOf(tPEoptheader));
  Result:=True;
End;


procedure ProcessEdata(Delta:Integer);
 Var
    ExportDir   : tExportDirectory;
    j           : Integer;
    Ordinal     : Integer;

    OfsNameList : Integer;
    OfsName     : Integer;

    Str         : Array[0..100] of char;

begin
  Seek(fExe, Section.PtrtoRawData+Delta);
  Blockread(fExe,ExportDir,sizeof(tExportDirectory));
  Writeln('Number of Exported Functions  = ',ExportDir.NumFunctions);

  OfsNameList := Section.PtrtoRawData-Section.RelVirtualAddr+ExportDir.AddrNames;
  // Print Names
  for j:=1 to ExportDir.NumNames do begin
    Seek(fExe,OfsNameList+(j-1)*4);
    BlockRead( fExe, OfsName, 4);
    Seek( fExe, Section.PtrtoRawData-Section.RelVirtualAddr+OfsName );
    BlockRead( fExe, Str, Sizeof(Str) );
    // Get Ordinal
    Seek(fExe,Section.PtrtoRawData-Section.RelVirtualAddr+ExportDir.AddrOrdinals+(j-1)*2);
    Ordinal:=0;
    BlockRead(fExe, Ordinal, 2);
    Writeln(' + ',Str);
    Writeln(fDef, 'IMPORT ',Str,', lib:"', fName,'"');//, Name:"', Str,'", Index:"', Ordinal,'"' );
  end;

end;





procedure GetExports;
 Var  i:Integer;
begin
  if not ReadHeader then Exit;
  ExportRva := OptHeader.Directory[IMAGE_DIRECTORY_ENTRY_EXPORT].RVA;
  ExportSize:= OptHeader.Directory[IMAGE_DIRECTORY_ENTRY_EXPORT].Size;
  if (ExportRva=0) or (ExportSize=0) then Exit;
  SectionOfs:=FilePos(fExe);
  for i:=1 to PEHead.NumberOfSections do begin
    Seek(fExe, SectionOfs);
    Blockread(fExe, Section, sizeof(tPESection));
    Inc(SectionOfs, Sizeof(tPESection));
    if(Section.RelVirtualAddr<=ExportRva)and(ExportRva<Section.RelVirtualAddr+Section.SizeofRawData)then
        ProcessEdata(ExportRva-Section.RelVirtualAddr);
  end;
end;


Var
    Path, Dir, Nam, Ext, ExecDir: String;
begin
  if ParamCount<>1 then Exit;
  SplitPath(ParamStr(0), Dir, Nam, Ext);
  ExecDir:=Dir;
  Path:= ParamStr(1);
  Writeln('Source file : ', Path);
  Assign(fExe, Path );
  SplitPath(Path, Dir, Nam, Ext);
  fName:= Nam+'.'+Ext;
  DefName:=ExecDir+Nam+'.def';
  Assign(fDef, DefName);
  Rewrite(fDef);

  Reset(fExe,1);
  GetExports;
  Close(fExe);
  Close(fDef);
  Writeln('FINISH.');
  Readln;
end.
