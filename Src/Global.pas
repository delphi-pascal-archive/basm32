{-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-}
{-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-* Win-Assembler *+*-*+*-*+*-*+*-*+*-*+*-*+*-}
{-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-}

Unit Global;
{$i Switches.inc}

INTERFACE

Uses Symbols, Assemble, RelocationStack, Segments, Import, Scanner;

CONST

    ProgVersion = 'BASM v0.5 - Win32 Assembler';

  // Error Messages
  	ExpectCONSTANT	:String='Constant expected';
  	ExpectINTCONST	:String='Integer constant expected';
  	ExpectREGISTER	:String='Expect Register';
  	ExpectLPAREN	  :String='Expect "("';
  	ExpectRPAREN	  :String='Expect ")"';
  	ExpectLFRAME	  :String='Expect "["';
  	ExpectRFRAME	  :String='Expect "]"';
  	ExpectCOMMA	    :String='Expect ","';
  	ExpectCOLON	    :String='Expect ":"';
  	TypeMismatch	  :String='Type mismatch';
  	InvREGISTER	    :String='Invalid register combination';
  	ErrArgCount	    :String='Invalid argument number';

TYPE

  tApplication = Record
        EntryPoint  : Integer; // Offset within SecCODE
        Symbols     : tSymbolGroupItem;
        SecCODE     : TSegment;
        SecDATA     : TSegment; //initialized data segment
        SecBSS      : TSegment; //uninitialized data segment
        SecIDATA    : TSegment; // Imported Data Segment
        LibStack    : TLibraryStack;
        AppType : ( APP_CONSOLE, APP_GUI );
  end;

VAR


  	Finish	  : Boolean;

    SymGroup  : TSymbolGroup;

    CurSeg    : pSegment;
    App       : tApplication;

  // Listing file
    fLst	    : Text;
    Listing   : Boolean;

  // Input/OutPut File names
    fSRCname  : String;
    fCOMname  : String;
    fEXEname  : String;
    fASMname  : String;
    fLSTname  : String;

  // Directories
    DirEXEC   : String; { Executable directory }
    DirSOURCE : String;
    DirOUTPUT : String;
    DirINCLUDE: String;

    AppExitProc : PROCEDURE;



PROCEDURE MakeError(Const ProcName, n:String);

IMPLEMENTATION

PROCEDURE MakeError(Const ProcName, n:String);
Begin
  WriteLN('ERROR: ',N);
  Writeln(' + SOURCE = ',ScanItem.SrcName);
  Writeln(' + LINE = ',TokenRow);
  Writeln(' + COL = ',TokenCol);
  Writeln(' + MODULE = ',ProcName);
  if Listing Then Writeln(fLst,' Error: ',n);
  AppExitProc;
  Readln;
  Halt;
End;

end.
