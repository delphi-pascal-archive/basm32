{-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-}
{-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-* Win-Assembler *+*-*+*-*+*-*+*-*+*-*+*-*+*-}
{-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-}

UNIT PeLinker;
{$i Switches.inc}

INTERFACE
{
 RVA : Relative Virtual Address
 VA  : Virtual Address = RVA + ImageBase   : Offset in the memory when process is loaded
}
USES Global, Segments, Symbols, PEheader, Import, RelocationStack, Convert;

Const DOSStub: array[0..$7F] of Char = (
    'M', 'Z',
    #$6C,#$00,  #$01,#$00,  #$00,#$00,  #$04,#$00,  #$00,#$00,  #$FF,#$FF,
    #$03,#$00,  #$00,#$01,  #$00,#$00,  #$00,#$00,  #$00,#$00,  #$40,#$00,
    #$00,#$00,#$00,#$00,#$00,#$00,#$00,#$00,#$00,#$00,#$00,#$00,#$00,#$00,
    #$00,#$00,#$00,#$00,#$00,#$00,#$00,#$00,#$00,#$00,#$00,#$00,#$00,#$00,
    #$00,#$00,#$00,#$00,#$00,#$00,
    #$80,#$00,#$00,#$00,
    {dos executable code}
    #$0E,  #$1F,  #$BA,#$0E,#$00,  #$B4,#$09,  #$CD,#$21,  #$B8,#$00,#$4C,
    #$CD,#$21,
    'T','h','i','s',' ','p','r','o','g','r','a','m',' ','c','a','n','n','o','t',
    ' ','b','e',' ','r','u','n',' ','i','n',' ','d','o','s',' ','m','o','d','e',
    '.',#$0D,#$0A,'$','-','B','A','S','S','E','M','-');

TYPE
  tPELinker = Object
      protected
        _RelVirtualAddr : Integer; // Current RVA Address
        _PtrtoRawData   : Integer; // Current File (Raw Data) Pointer
      public
        ExeFile         : File;
        PEHeader        : tPEHeader;
        OptHeader       : tPEoptHeader;
        Header  : Record
            SecCODE : tPESection;
            SecDATA : tPESection;
            SecIDATA: tPESection; // Imported DATA
            SecBSS  : tPESection;
        end;
        PROCEDURE InitHeaders;

        PROCEDURE FixImportSection;
        PROCEDURE FixBSSSection;
        PROCEDURE FixDATASection;
        PROCEDURE FixCodeSection;

        PROCEDURE Align_File;
        PROCEDURE WritePE;
        PROCEDURE Link(Const S:String);
  End;


CONST{ Section flags }

        SEC_FLAG_CODE    = $60000020;
        SEC_FLAG_IDATA   = $C0000040;
        SEC_FLAG_RSRC    = $50000040;
        SEC_FLAG_RELOC   = $42000040;


{--------------------------------------------------------------------------}
{--------------------------------------------------------------------------}
IMPLEMENTATION


{###########################################################################}
PROCEDURE tPELinker.InitHeaders;
begin
  FillChar (PEHeader, SizeOf(PEHeader), 0);
  With PEheader do begin
    Magic := 'PE'#0#0;
    Machine   := $014C;
    NumberOfSections:= 0; 
    TimeDateStamp := 0;
    PointerToSymbolTable := 0;
    NumberOfSymbols := 0;
    SizeOfOptionalHeader := SizeOf(OptHeader);
    Characteristics := $818E {or IMAGE_FILE_RELOCS_STRIPPED};
  End;

  FillChar( Optheader, Sizeof(tPEoptHeader), 0);
  With Optheader do begin
    Magic           := $010B;
    MajorLinkerVersion := 0;
    MinorLinkerVersion := 0;
    SizeOfCode      := 0;{ Set Later }
    SizeOfInitializedData   := 0;
    SizeOfUninitializedData   := 0;
    AddressOfEntryPoint   := 0;
    BaseOfCode      := 0;
    BaseOfData      := 0;
    ImageBase       := $400000;
    SectionAlignment:= $1000;
    FileAlignment := $200;
    MajorOperatingSystemVersion := 1;
    MinorOperatingSystemVersion := 0;
    MajorImageVersion:= 0;
    MajorImageVersion:= 0;
    MajorSubsystemVersion := 4;
    Win32VersionValue := 0;
    SizeOfImage     := { $1000 + $3000;{ (address of first section) + }(1+PEHeader.NumberOfSections)*OptHeader.SectionAlignment;
    SizeOfHeaders   := $200; {//FilePos(F) + SizeOf(OptHeader)}
    CheckSum        := 0;
    if App.AppType=APP_GUI
      then Subsystem := SUBSYSTEM_WINDOWS_GUI
      else Subsystem := SUBSYSTEM_WINDOWS_CUI;
    DllCharacteristics := 0;
    SizeOfStackReserve := $100000;
    SizeOfStackCommit  := $4000;
    SizeOfHeapReserve  := $100000;
    SizeOfHeapCommit   := $1000;
    LoaderFlags     := 0;
    NumberOfRvaAndSizes := 16;
  End;
End;

///////////////////////////////////////////////////////////////////////////////
// IMPORT SECTION
///////////////////////////////////////////////////////////////////////////////
PROCEDURE TPELinker.FixImportSection;
{
 DIRECTORY TABLE
 NULL DIR ENTRY

 DLL 1 LOOKUP TABLE
 DLL 2 LOOKUP TABLE
 ...
 NULL(4bytes)

 HINT - NAME TABLE
 NULL(4bytes)
}
 Var
        SecRVA_IDATA      : Integer;
        ofsDLL1LookupTable: Integer;  // Address for the offset table within import section
        ofsHintNameTable: Integer;    // Address for the name table within import section
        LibraryCount  : Integer;        // Number of Impported Libraries
        ImportCount   : Integer;        // Number of All Impported functions
        ImportDir     : TImportDirectory;
        Name          : String[64];
        Rva_Name      : LONGINT;
        LibItem       : pLibraryItem;
        ImportItem    : pImportItem;
        SymData       : pDataLabel;
        Sym           : pSymbol;
Begin
  SecRVA_IDATA :=_RelVirtualAddr;
  Header.SecIDATA.RelVirtualAddr:=_RelVirtualAddr;
  App.SecIDATA.VirtualAddress:=OptHeader.ImageBase+_RelVirtualAddr;

// get The number of librairies and imported Data
  LibraryCount:=App.LibStack.CountCalledLibs;
  ImportCount:=App.LibStack.CountCalledImports;
  Writeln(' + Used Libraries Number = ',LibraryCount);
  Writeln(' + Called Imports Count = ',ImportCount);

// get the offset of the first lirary lookup table
  ofsDLL1LookupTable:= (Sizeof(tImportDirectory)*(LibraryCount+1)); // +1 for null import dir

// get the offset of the (HInt/Name) table
  ofsHintNameTable   := ofsDLL1LookupTable+( 4*(ImportCount+LibraryCount) );

// Set buffer to be filled with import dirs and address table
  App.SecIDATA.AddByte(0, ofsHintNameTable);

  LibraryCount := 0;
  ImportCount := 0;
  FillChar(ImportDir,SizeOf(ImportDir),0);

  LibItem:=App.LibStack.Head;
  while (LibItem<>NIL) do begin
    if (LibItem^.IsCalled) Then Begin
       // Set Import Directory
        ImportDir.Characteristics := SecRVA_IDATA + ofsDLL1LookupTable + (ImportCount*4);
        ImportDir.LookupTableRVA  := SecRVA_IDATA + ofsDLL1LookupTable + (ImportCount*4);
        ImportDir.NameRVA         := SecRVA_IDATA + App.SecIDATA.Size;
        App.SecIDATA.SetBuffer( Sizeof(tImportDirectory)*LibraryCount, ImportDir, Sizeof(tImportDirectory), 1);
       // Set Library Name
        Name:=LibItem^.Name+#0#0#0; // for alignemnt
        if odd( byte(Name[0]) ) Then Dec(Name[0]);
        App.SecIDATA.AddString(Name);  // Add lib name within name table
       // Set Address Table & functions names for current library
        ImportItem:=LibItem^.Head;
        while (ImportItem<>NIL) do begin
            if (ImportItem^.IsCalled) then begin
                RVA_Name := SecRVA_IDATA+App.SecIDATA.Size;
                { 00000000h - Import by name }
                { 80000000h - Import by ordinal }
                App.SecIDATA.SetDword( ofsDLL1LookupTable+Sizeof(Longint)*ImportCount, RVA_Name, 1);
                Name:=#0#0+ImportItem^.Name+#0#0;
                if odd( byte(Name[0]) ) Then Dec(Name[0]);
                App.SecIDATA.AddString(Name);
               // Set Import Function Jump Code address
                New( SymData, Init(DEF_LABEL,NIL, @App.SecIDATA, ofsDLL1LookupTable+(ImportCount*4)) );
                ImportItem^.Data:=SymData;
                ImportItem.Kind:=SYM_LABEL;
                ImportItem.Flag:=ImportItem.Flag+[SYM_FLAG_DEFINED];
               // Set Jmp_Sym Address : FF 25 <00 00 00 00>
                App.SecCODE.Align(8);
                New( SymData, Init(DEF_LABEL,NIL, @App.SecCODE,App.SecCODE.Size) );
                Sym:=ImportItem.Jmp_Sym;
                Sym^.Data:=Symdata;
                Sym.Kind:=SYM_LABEL;
                Sym.Flag:=Sym.Flag+[SYM_FLAG_DEFINED];
                App.SecCODE.AddString(#$FF#$25);
                // Set Relocation
                App.SecCODE.Relocs.PushReloc(REL_OFFSET,ImportItem,App.SecCODE.Size,0);
                App.SecCODE.AddByte(0,4);

                Inc(ImportCount);
            End;
            ImportItem:=Pointer(ImportItem.Next);
        End;
        Inc(ImportCount);
        Inc (LibraryCount);
    End;
    LibItem:=LibItem.Next;
 End;

// Update Headers
// we are using an import directory
 OptHeader.Directory[image_Directory_Entry_Import].RVA :=SecRVA_IDATA;
 OptHeader.Directory[image_Directory_Entry_Import].Size:=App.SecIDATA.Size;
 With Header.SecIDATA do begin
    Name:='.idata'#0#0;
    VirtualSize := ( ((App.SecIDATA.Size+OptHeader.SectionAlignment-1)div OptHeader.SectionAlignment) * OptHeader.SectionAlignment);
    RelVirtualAddr := SecRVA_IDATA;
    SizeofRawData := ( ((App.SecIDATA.Size+OptHeader.FileAlignment-1)div OptHeader.FileAlignment) * OptHeader.FileAlignment) ;  { must be multiple of file alignment }
    PtrtoRawData := _PtrtoRawData; // Last File Pos
    Characteristics := SEC_FLAG_IDATA;
 End;

 _RelVirtualAddr:= _RelVirtualAddr + Header.SecIDATA.VirtualSize;
 _PtrtoRawData:=_PtrtoRawData + Header.SecIDATA.SizeofRawData;
End;


///////////////////////////////////////////////////////////////////////////////
// BSS SECTION
///////////////////////////////////////////////////////////////////////////////
PROCEDURE tPELinker.FixBSSSection;
begin
 App.SecBSS.VirtualAddress:=OptHeader.ImageBase+_RelVirtualAddr;

 With Header.SecBSS do begin
    Name:='.bss'#0#0#0#0;
    VirtualSize :=OptHeader.SectionAlignment;//( ((SectionBSS.Size+OptHeader.SectionAlignment-1)div OptHeader.SectionAlignment) * OptHeader.SectionAlignment);
    RelVirtualAddr := _RelVirtualAddr;
    SizeOfRawData := 0;  { must be multiple of file alignment }
    PtrToRawData := _PtrtoRawData;
    Characteristics := Section_BSS + Section_READ + Section_WRITE ;
 End;
 OptHeader.SizeOfUninitializedData:=Header.SecBSS.VirtualSize;

 _RelVirtualAddr:= _RelVirtualAddr + Header.SecBSS.VirtualSize;
 _PtrtoRawData:=_PtrtoRawData+Header.SecBSS.SizeofRawData;
end;

///////////////////////////////////////////////////////////////////////////////
// DATA SECTION
///////////////////////////////////////////////////////////////////////////////
PROCEDURE tPELinker.FixDATASection;
begin
 App.SecDATA.VirtualAddress:=OptHeader.ImageBase+_RelVirtualAddr;

 With Header.SecDATA do begin
    Name:='.data'#0#0#0;
    VirtualSize :=OptHeader.SectionAlignment;//( ((SectionBSS.Size+OptHeader.SectionAlignment-1)div OptHeader.SectionAlignment) * OptHeader.SectionAlignment);
    RelVirtualAddr := _RelVirtualAddr;
    SizeofRawData := ( ((App.SecDATA.Size+OptHeader.FileAlignment-1)div OptHeader.FileAlignment) * OptHeader.FileAlignment) ;  { must be multiple of file alignment }
    PtrToRawData := _PtrtoRawData;
    Characteristics := Section_BSS + Section_READ + Section_WRITE ;
 End;

 _RelVirtualAddr:= _RelVirtualAddr + Header.SecDATA.VirtualSize;
 _PtrtoRawData:=_PtrtoRawData+Header.SecDATA.SizeofRawData;
end;

///////////////////////////////////////////////////////////////////////////////
// CODE SECTION
///////////////////////////////////////////////////////////////////////////////
PROCEDURE tPELinker.FixCodeSection;
Var   SizeOfCode  : Integer;
      SecRVA_CODE : Integer;
begin
  SecRVA_CODE:=_RelVirtualAddr;
  App.SecCODE.VirtualAddress:=OptHeader.ImageBase+_RelVirtualAddr;

// Get Total Section Code Size
  SizeofCode:= App.SecCODE.Size;
  Writeln(' + Total Code size = ',SizeofCode);

// ApplyFixups Code Sections fixup

// Update Headers
  with Header.SecCODE do begin
    Name:='.text'#0#0#0;
    VirtualSize := ( ((SizeofCode+OptHeader.SectionAlignment-1)div OptHeader.SectionAlignment) * OptHeader.SectionAlignment);
    RelVirtualAddr := _RelVirtualAddr;
    SizeOfRawData := ( ((SizeofCode+OptHeader.FileAlignment-1)div OptHeader.FileAlignment) * OptHeader.FileAlignment);  { must be multiple of file alignment }
    PtrToRawData := _PtrtoRawData;
    Characteristics := SEC_FLAG_CODE;
  End;

  OptHeader.SizeOfCode := SizeofCode;
  OptHeader.AddressOfEntryPoint := SecRVA_CODE+App.EntryPoint;
  OptHeader.BaseOfCode := SecRVA_CODE;

 _RelVirtualAddr:= _RelVirtualAddr + Header.SecCODE.VirtualSize;
 _PtrtoRawData:=_PtrtoRawData+ Header.SecCODE.SizeofRawData;
end;


//###########################################################################
// Writing Output
//###########################################################################

PROCEDURE tPELinker.Align_File;
 Var    I     : Integer;
        TempBuf : Pointer;
Begin
  I := OptHeader.FileAlignment - ( FilePos(ExeFile) MOD OptHeader.FileAlignment );
  if I > 0 then begin
    GetMem(TempBuf,i);
    FillChar (TempBuf^, I, #0);
    BlockWrite (ExeFile, TempBuf^, I);
    Freemem(TempBuf,i);
  End;
End;


PROCEDURE tPELinker.WritePE;
Begin
 // PE Header
  BlockWrite (ExeFile, DOSStub, SizeOf(DOSStub));
  BlockWrite (ExeFile, PEHeader, SizeOf(PEHeader));
  BlockWrite (ExeFile, OptHeader, SizeOf(OptHeader));
  if (App.SecBSS.Size>0) then BlockWrite (ExeFile, Header.SecBSS, SizeOf(tPEsection));
  BlockWrite (ExeFile, Header.SecIDATA, SizeOf(tPEsection));
  if (App.SecDATA.Size>0) then BlockWrite (ExeFile, Header.SecDATA, SizeOf(tPEsection));
  BlockWrite (ExeFile, Header.SecCODE, SizeOf(tPEsection));
  Align_File;
 // IMPORT
  BlockWrite(ExeFile, App.SecIDATA.Data[0], App.SecIDATA.Size);
  Align_File;
 // Section DATA
  if (App.SecDATA.Size>0) then begin
    BlockWrite(ExeFile, App.SecDATA.Data[0], App.SecDATA.Size);
    Align_File;
  end;
 // Code Section
  BlockWrite(ExeFile, App.SecCODE.Data[0], App.SecCODE.Size);
  Align_File;
End;


PROCEDURE tPELinker.Link;
 Var  SectionCount  : Integer;
      HeaderSize    : Integer;
Begin
  InitHeaders;

  // Calculate the Number of Sections to Write
  SectionCount:=0;
  if (App.SecBSS.Size>0) then Inc( SectionCount );
  {if (App.SecIDATA.Size>0) then} Inc( SectionCount );
  if (App.SecDATA.Size>0) then Inc( SectionCount );
  {if (App.SecCODE.Size>0) then} Inc( SectionCount );
  PEHeader.NumberOfSections:= SectionCount;

  // Calculate All Headers Size
  HeaderSize:= SizeOf(DOSStub) + SizeOf(PEHeader) + SizeOf(OptHeader) + SectionCount*Sizeof(tPESection);
  OptHeader.SizeOfHeaders:=HeaderSize;

  _RelVirtualAddr :=( ((HeaderSize+OptHeader.SectionAlignment-1)div OptHeader.SectionAlignment) * OptHeader.SectionAlignment);
  _PtrtoRawData:=( ((HeaderSize+OptHeader.FileAlignment-1)div OptHeader.FileAlignment) * OptHeader.FileAlignment);

 // Fix All Sections ( Addresses and sizes )
  if (App.SecBSS.Size>0) then FixBSSSection;
  FixImportSection;
  if (App.SecDATA.Size>0) then FixDATASection;
  FixCodeSection;

  App.SecCODE.SetFixups;
  App.SecDATA.SetFixups;

  if (App.SecCODE.Relocs.Head=NIL)AND(App.SecDATA.Relocs.Head=NIL) then begin
    OptHeader.SizeOfImage:= _RelVirtualAddr;
    Assign(ExeFile,S); Rewrite(ExeFile,1);
    WritePE; // Creating EXE file
    Close(ExeFile);
    Writeln(' + BSS RVA   = $', Hex32(Header.SecBSS.RelVirtualAddr));
    Writeln(' + IDATA RVA = $', Hex32(Header.SecIDATA.RelVirtualAddr));
    Writeln(' + DATA RVA  = $', Hex32(Header.SecDATA.RelVirtualAddr));
    Writeln(' + CODE RVA  = $', Hex32(Header.SecCODE.RelVirtualAddr));
    WriteLN(' -> ',fExeName);
  end
  else begin // Create Object File??????????
    if (App.SecCODE.Relocs.Head<>NIL) THEN Writeln('Undefined Symbol: ',pSymbol(App.SecCODE.Relocs.Head.Sym).Name);
    if (App.SecDATA.Relocs.Head<>NIL) THEN Writeln('Undefined Symbol: ',pSymbol(App.SecCODE.Relocs.Head.Sym).Name);
  end;
End;

END.


