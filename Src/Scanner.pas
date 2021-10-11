{-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-}
{-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-* Win-Assembler *+*-*+*-*+*-*+*-*+*-*+*-*+*-}
{-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-}

Unit Scanner;

INTERFACE

Uses SysUtils, Convert, Assemble;


///////////////////////////////////////////////////////////////////////////////
//  tRoutine : internal functions
///////////////////////////////////////////////////////////////////////////////

TYPE
  tRoutine = (
    ROUT_ADDR,     // Addr <Sym Label> ==> return the offset of the symbol
    ROUT_OFFSET,   // Offset <Sym Label> ==> return the offset of the symbol
    ROUT_SIZEOF    // Sizeof <Sym TypeDef> ==> return the size of the symbol
  );

CONST
  RoutineName: Array [tRoutine] of String[6]= (
    'ADDR',
    'OFFSET',
    'SIZEOF'
  );

///////////////////////////////////////////////////////////////////////////////
//  tDirective
///////////////////////////////////////////////////////////////////////////////

TYPE
  tDirective = (
    DIR_DB,
    DIR_DW,
    DIR_DD,
    DIR_ORG,
    DIR_END,
    DIR_INCLUDE,
    DIR_EXTERN,
    DIR_PUBLIC,
    DIR_IMPORT,
    DIR_ALIGN
  );

Const
  DirectiveName: Array [tDirective] of String[7]= (
    'DB', 'DW', 'DD',
    'ORG','END',
    'INCLUDE',
    'EXTERN','PUBLIC', 'IMPORT',
    'ALIGN'
  );

///////////////////////////////////////////////////////////////////////////////
//  tModule
///////////////////////////////////////////////////////////////////////////////

TYPE
  TModule = (
      MOD_MODEL,
      MOD_CODE,
      MOD_DATA,
      MOD_APPTYPE
  );

Const
  ModuleName: Array [tModule] of String[7]= (
    'MODEL', 'CODE', 'DATA' , 'APPTYPE'
  );


///////////////////////////////////////////////////////////////////////////////
//  tSizeSpecifier
///////////////////////////////////////////////////////////////////////////////
TYPE
  tSizeSpecifier = (
    SZS_BYTE,
    SZS_WORD,
    SZS_DWORD,
    SZS_NEAR,
    SZS_SHORT
  );

CONST
  SizeSpecifierName : Array [tSizeSpecifier] of String[7]= (
    'BYTE',
    'WORD',
    'DWORD',
    'NEAR',
    'SHORT'
  );

///////////////////////////////////////////////////////////////////////////////
// tTokOperator
///////////////////////////////////////////////////////////////////////////////

TYPE
  tTokOperator = (
      OPER_SHR,
      OPER_SHL,
      OPER_MOD,
      OPER_DIV,
      OPER_AND,
      OPER_OR,
      OPER_XOR
  );

CONST
  TokOperatorName : Array [tTokOperator] of String[3]= (
      'SHR',  'SHL',
      'MOD',  'DIV',
      'AND',  'OR',
      'XOR'
  );

///////////////////////////////////////////////////////////////////////////////
//  tTOKEN
///////////////////////////////////////////////////////////////////////////////
TYPE
 tAsmToken=(
	TOK_UNKNOWN,TOK_EOF,TOK_EOLN,
    { constants }
	TOK_RealConst,TOK_STRCONST,TOK_IntConst,
    { special characters }
	TOK_POINT,TOK_COMMA,TOK_COLON,TOK_DOLLAR,
	TOK_RPAREN,TOK_LPAREN,TOK_RFRAME,TOK_LFRAME,
    { operators }
  TOK_EQUAL, TOK_PLUS, TOK_MINUS, TOK_STAR, TOK_SLASH,
	TOK_CARET, TOK_EXCLAMATION,
    { symbols }
	TOK_IDENTIFIER, TOK_PREFIX, TOK_OPCODE,
	TOK_REGISTER, TOK_ROUTINE, TOK_DIRECTIVE, TOK_MODULE,
  TOK_SPECIFIER, TOK_OPERATOR
 );


  tTokenFlag = (
      TOK_FLAG_PREFIX,
      TOK_FLAG_OPCODE,
      TOK_FLAG_REGISTER,
      TOK_FLAG_ROUTINE,
      TOK_FLAG_DIRECTIVE,
      TOK_FLAG_MODULE,
      TOK_FLAG_SPECIFIER,
      TOK_FLAG_OPERATOR
  );

  tTokenFlagSet = Set of tTokenFlag;

///////////////////////////////////////////////////////////////////////////////
// tScannerItem
///////////////////////////////////////////////////////////////////////////////
TYPE
  pScannerItem = ^tScannerItem;
  tScannerItem = Object
        SrcName : String;
        SelfSize: Integer;
        Next    : pScannerItem;
        LastRow : Integer;
        LastCol : Integer;
        LastChar: Char;
        constructor Init;
        destructor Done; Virtual; Abstract;
        function ReadChar:Char; Virtual; Abstract;
        function NextChar:Char; Virtual; Abstract;
  end;

///////////////////////////////////////////////////////////////////////////////
// tFileScanner
///////////////////////////////////////////////////////////////////////////////
Const   FileScanner_BufSize = 4096;

TYPE

  TBufferInfo = Record
        buf     : pchar;
        Start   : Longint;    // File Postion for the Start of the Buffer
        Size    : Longint;    // Data Size within the Buffer
        Index   : Longint;    // Current Cursor Position
  end;

  TFileInfo = Record
        f       : file;
        Name    : String;
        Size    : Integer;
        Closed  : Boolean;
        ClosePos: Integer;
  end;

  pFileScanner = ^TFileScanner;
  tFileScanner = Object(tScannerItem)
        fi      : TFileInfo;
        bi      : TBufferInfo;
        constructor Init;
        destructor Done; Virtual;
        function OpenFile(const fn:string):boolean;
        procedure CloseFile;
        procedure ReloadBuf;
        function ReadChar:Char; Virtual;
        function NextChar:Char; Virtual;
        function TempOpen:boolean; // inspired from Free Pascal Compiler  :)
        procedure TempClose;       // to open only one file per scan
  end;

CONST
    _EOLN   = #13;
    _EOF    = #26;



///////////////////////////////////////////////////////////////////////////////
//  Asm Scanner
///////////////////////////////////////////////////////////////////////////////

procedure InitScanner;
procedure DoneScanner;
procedure PushScannerItem(Item:pScannerItem);
procedure PopScannerItem;
function ReadChar:Char;
procedure SkipComment;
PROCEDURE SkipSpace;
PROCEDURE SkipLine;
PROCEDURE Readstring;
PROCEDURE ReadNumber;
function ReadFileName:String;
function ReadToken: tAsmToken;
function TokenFlag( Flags:tTokenFlagSet ) : tAsmToken;
function IsTokenIdent:Boolean;



Var
    LastChar  : Char;
    ScanItem  : pScannerItem;

    Token		  : tAsmToken;
    TokenStr	: ShortString; // Current Token String
    TokenUpStr: ShortString; // UpperCase of the Token String
    TokenInt	: Longint;
    TokenRow  : Integer;
    TokenCol  : Integer;

////////////////////////////////////////////////////////////////////////////////

IMPLEMENTATION

uses Global;

function IsPrefix(Const S:String): Integer;
Var
    i   : tAsmPrefix;
begin
  IsPrefix:=-1;
  For i:=Low(tAsmPrefix) to High(tAsmPrefix) do begin
    if (S=AsmPrefixName[i]) then begin
      IsPrefix:=Ord(i);
      exit;
    end;
  end;
end;

function IsOpcode(Const S:String): Integer;
Var
    i   : tAsmOpcode;
begin
  IsOpCode:=-1;
  For i:=Low(tAsmOpcode) to High(tAsmOpcode) do begin
    if (S=AsmOpcodeName[i]) then begin
      IsOpCode:=Ord(i);
      exit;
    end;
  end;
end;

function IsRegister(Const S:String): Integer;
Var
    i   : tAsmRegister;
begin
  IsRegister:=-1;
  For i:=Low(tAsmRegister) to High(tAsmRegister) do begin
    if (S=AsmRegisterName[i]) then begin
      IsRegister:=Ord(i);
      exit;
    end;
  end;
end;

function IsRoutine(Const S:String): Integer;
Var i   : tRoutine;
begin
  IsRoutine:=-1;
  For i:=Low(tRoutine) to High(tRoutine) do begin
    if (S=RoutineName[i]) then begin
      IsRoutine:=Ord(i);
      exit;
    end;
  end;
end;

function IsDirective(Const S:String): Integer;
Var i   : tDirective;
begin
  IsDirective:=-1;
  For i:=Low(tDirective) to High(tDirective) do begin
    if (S=DirectiveName[i]) then begin
      IsDirective:=Ord(i);
      exit;
    end;
  end;
end;


function IsModule(Const S:String): Integer;
Var i   : tModule;
begin
  IsModule:=-1;
  For i:=Low(tModule) to High(tModule) do begin
    if (S=ModuleName[i]) then begin
      IsModule:=Ord(i);
      exit;
    end;
  end;
end;

function IsSpecifier(Const S:String): Integer;
Var i   : tSizeSpecifier;
begin
  IsSpecifier:=-1;
  For i:=Low(tSizeSpecifier) to High(tSizeSpecifier) do begin
    if (S=SizeSpecifierName[i]) then begin
      IsSpecifier:=Ord(i);
      exit;
    end;
  end;
end;

function IsOperator(Const S:String): Integer;
Var i   : tTokOperator;
begin
  IsOperator:=-1;
  For i:=Low(tTokOperator) to High(tTokOperator) do begin
    if (S=TokOperatorName[i]) then begin
      IsOperator:=Ord(i);
      exit;
    end;
  end;
end;

///////////////////////////////////////////////////////////////////////////////
// tScannerItem
///////////////////////////////////////////////////////////////////////////////

constructor tScannerItem.Init;
begin
  SelfSize:=Sizeof(tScannerItem);
end;

///////////////////////////////////////////////////////////////////////////////
// TFileScanner
///////////////////////////////////////////////////////////////////////////////

Constructor TFileScanner.Init;
begin
  SelfSize:=Sizeof(TFileScanner);
  fi.Closed:=True;
  GetMem( bi.buf,FileScanner_BufSize );
end;


Destructor TFileScanner.Done;
begin
  CloseFile;
  if assigned(bi.buf) then FreeMem(bi.buf,FileScanner_BufSize);
end;


function TFileScanner.OpenFile(const fn:string):boolean;
begin
  Result:=false;
  Assign(fi.f,fn);
  filemode:=$0;
  {$I-} reset(fi.f,1); {$I+}
  if ioresult<>0 then exit;

  fi.Name:=fn;
  fi.Closed:=false;
  LastChar:=#0;
  LastRow:=1;
  LastCol:=0;
  fi.Size:=filesize(fi.f);

 //reset buffer
  bi.Start:=0;
  bi.Size:=0;
  bi.Index:=0;

  // ReadChar; // IT WORK ON THE LAST READ CHAR
  SrcName:=fn;
  Result:=true;
end;


procedure TFileScanner.CloseFile;
begin
 if not(fi.Closed) then begin
    Close(fi.f);
    fi.Closed:=true;
 end;
end;


procedure TFileScanner.ReloadBuf;
begin // Load data to buffer
  inc( bi.Start, bi.Size );
  blockread( fi.f, bi.buf^, FileScanner_BufSize, bi.Size );
  bi.Index:=0;
end;


function TFileScanner.ReadChar:Char;
begin
 if (bi.Index<bi.Size) then begin
    LastChar:=bi.buf[bi.Index];
    Inc( bi.Index );
 end
 else begin
    ReloadBuf;
    if (bi.Size>0) then begin
        LastChar:=bi.Buf[0];
        bi.Index:=1;
    end
    else LastChar:=_EOF;
 end;

 if (LastChar=#10) then begin
    Inc(LastRow);
    LastCol:=0;
 end
 else if (LastChar<>#13) Then Inc(LastCol);
 Result:=LastChar;
end;

function TFileScanner.NextChar:Char;
Begin
 if bi.Index<bi.Size
    then NextChar:= bi.buf[bi.Index]
    else begin
        ReloadBuf;
        if (bi.Size>0) then begin
            NextChar:=bi.Buf[0];
            bi.Index:=0;
        end
        else NextChar:=_EOF;
   end;
End;


function TFileScanner.TempOpen:boolean;
begin
  Result:=false;
  Assign(fi.f,fi.Name);
  filemode:=$0;
  {$I-} reset(fi.f,1); {$I+}
  Seek(fi.f,fi.ClosePos);
  if ioresult<>0 then exit;
  fi.Closed:=false;
  Result:=true;
end;

procedure TFileScanner.TempClose;
begin
 if not(fi.Closed) then begin
    fi.ClosePos:=FilePos(fi.f);
    close(fi.f);
    fi.Closed:=true;
 end;
end;


///////////////////////////////////////////////////////////////////////////////
// tScannerItem
///////////////////////////////////////////////////////////////////////////////

procedure InitScanner;
Begin
  ScanItem := nil;
End;

procedure DoneScanner;
begin
  while (ScanItem<>NIL) do PopScannerItem;
end;


procedure PushScannerItem(Item:pScannerItem);
begin
  Item^.Next:=ScanItem;
  ScanItem := Item;
end;

procedure PopScannerItem;
 Var    Item : pScannerItem;
begin
  if ScanItem=NIL then Exit;
  Item:=ScanItem.Next;
  FreeMem( ScanItem, ScanItem^.SelfSize );
  ScanItem:=Item;
end;


function ReadChar:Char;
 Var  C:Char;
begin
  if ScanItem<>NIL then begin
    C:=ScanItem^.ReadChar;
    if C=_EOF then begin
      PopScannerItem;
      if ScanItem<>NIL then C:=ReadChar;
    end;
  end else C:=_EOF;
  LastChar:=C;
  ReadChar:=C;
end;

function NextChar:Char;
 Var  C:Char;
begin
  if (ScanItem<>NIL)
    then C:=ScanItem^.NextChar
    else C:=_EOF;
  NextChar:=C;
end;

///////////////////////////////////////////////////////////////////////////////
PROCEDURE SkipSpace;
begin
 while (LastChar in [' ', #9, #10]) do ReadChar;
End;

///////////////////////////////////////////////////////////////////////////////
PROCEDURE SkipLine;
begin
 if  Token=TOK_EOLN then Exit;
 while not(LastChar in [_EOF,_EOLN]) do ReadChar;
 ReadChar;
 Token:=TOK_EOLN;
End;

///////////////////////////////////////////////////////////////////////////////
PROCEDURE Readstring;
 Var    St  : String;
Begin
  St:=LastChar;
  while ReadChar in ['$','0'..'9','@'..'Z','_','a'..'z'] Do begin
     St:=St+LastChar;
  End;
  TokenStr:=St;
  if St='$' Then Token:=TOK_DOLLAR
  else begin
    TokenUpStr:=UpperCase(TokenStr);
    Token:=TOK_IDENTIFIER;
  end;
End;


///////////////////////////////////////////////////////////////////////////////
PROCEDURE ReadNumber;
 Var ValType : ( Val_Bin, Val_Oct, Val_Dec, Val_Hex );
Begin
 TokenStr := '';
 // Check for binary number
 ValType:=Val_Bin;
 Repeat
    Case LastChar of
        '0'..'1': ;
        '2'..'7': if ValType<Val_Oct then ValType:=Val_Oct;
        '8'..'9': if ValType<Val_Dec then ValType:=Val_Dec;
        'a'..'f','A'..'F':
            begin
                if not (NextChar in ['0'..'9','a'..'f','A'..'F']) then
                 if (Lastchar in['b','B']) and (ValType=Val_Bin) then begin
                    TokenStr := TokenStr + LastChar;
                    ReadChar;
                    Token:=TOK_INTCONST;
                    TokenInt:=Bin2Long(TokenStr);
                    Exit;
                 end;
                ValType:=Val_Dec;
            end;
        else Break;
    end;
    TokenStr := TokenStr + LastChar;
    ReadChar;
 Until False;

 Case LastChar of
    'b','B': // Binary Value?
        if (ValType=Val_Bin) then begin
          Token:=TOK_INTCONST;
          TokenInt:=Bin2Long(TokenStr);
          TokenStr := TokenStr + LastChar;
          ReadChar;
          Exit;
        end else MakeError('ReadNumber', 'Error parsing Binary value');
    'o','O': // Octal Value?
        if (ValType<=Val_Oct) then begin
          Token:=TOK_INTCONST;
          TokenInt:=Oct2Long(TokenStr);
          TokenStr := TokenStr + LastChar;
          ReadChar;
          Exit;
        end else MakeError('ReadNumber', 'Error parsing Octal value');
    'h','H': // Hexadecimal value
        begin
            Token:=TOK_INTCONST;
            TokenInt:=Hex2Long(TokenStr);
            TokenStr := TokenStr + LastChar;
            ReadChar;
            Exit;
        end;
    '.': ; // Real Value
    else begin
        if (ValType<=Val_Dec) then begin // Decimal value
            Token:=TOK_INTCONST;
            TokenInt:=Dec2Long(TokenStr);
            Exit;
        end else MakeError('ReadNumber', 'Error parsing Decimal value');
    end;
 end;
end;


function ReadFileName:String;
Begin
 SkipSpace;
 Result:='';
 while NextChar in ['.','$','0'..'z','_'] do Result:=Result+ UPCASE( ReadChar );
End;

procedure SkipComment;
begin
 while not(ReadChar in[_EOLN,_EOF]) do;
 Case LastChar of
   _EOLN:
      begin
        ReadChar;
        Token:=TOK_EOLN;
      end;
   _EOF: Token:=TOK_EOF;
 end;
end;

FUNCTION ReadToken: tAsmToken;
 Var    C   : Char;
BEGIN{ Readtoken }
 SkipSpace;
 TokenStr := LastChar;

  if ScanItem<>NIL then begin
    TokenRow:=ScanItem^.LastRow;
    TokenCol:=ScanItem^.LastCol;
  end;

 Case LastChar of

  '$','@'..'Z','_','a'..'z': ReadString;

  '0'..'9': ReadNumber;

  '''','"':{ String constant }
    begin
        C:=LastChar;
        TokenStr[0]:=#0;
        while (ReadChar<>C) do begin
            if LastChar in[_EOF,_EOLN] then break;// ERROR
            Inc( TokenStr[0] );
            TokenStr[ Byte(TokenStr[0]) ]:=LastChar;
        end;
        Case TokenStr[0] of
            #1: begin Token:=TOK_INTCONST; TokenInt:=Byte(TokenStr[1]); end;
            #2: begin Token:=TOK_INTCONST; TokenInt:=Byte(TokenStr[1])+256*Byte(TokenStr[2]); end;
            Else Token:=TOK_STRCONST;
        end;
        ReadChar;
    end;

  _EOLN :{ End of Line }
    begin
        ReadChar;
        Token:=TOK_EOLN;
    end;

  _EOF :{ End of Line }
    begin
        Token:=TOK_EOF;
    end;

  ';' : SkipComment;

  '.':
    begin
        TokenStr := LastChar;
        Token := TOK_POINT;
        ReadChar;
    end;

  ':':{ Label: , SEG : [OFS] }
	begin
        TokenStr:=LastChar;
        Token:=TOK_COLON;
        ReadChar;
    end;
  ',':{ Opcode Arg,Arg }
    begin
        TokenStr:=LastChar;
        Token:=TOK_COMMA;
        ReadChar;
    end;
{ Les operateurs }
  '+':
    begin
        TokenStr:=LastChar;
        Token:=TOK_PLUS;
        ReadChar;
    end;
  '-':
    begin
        TokenStr:=LastChar;
        Token:=TOK_MINUS;
        ReadChar;
    end;
  '*':
    begin
        TokenStr:=LastChar;
        Token:=TOK_STAR;
        ReadChar;
    end;
  '/':
    begin
        if NextChar=_EOLN then begin
          ReadChar; ReadChar; ReadToken;
        end
        else begin
          TokenStr:=LastChar;
          Token:=TOK_SLASH;
          ReadChar;
        end;
    end;

  '=':
    begin
	    TokenStr:=LastChar;
	    Token:=TOK_EQUAL;
      ReadChar;
    end;

  '(':
    begin
	    TokenStr:=LastChar;
	    Token:=TOK_LPAREN;
      ReadChar;
    end;
  ')':
    begin
        TokenStr:=LastChar;
        Token:=TOK_RPAREN;
        ReadChar;
    end;

  '[':
    begin
	    TokenStr:=LastChar;
	    Token:=TOK_LFRAME;
      ReadChar;
    end;
  ']':
    begin
        TokenStr:=LastChar;
        Token:=TOK_RFRAME;
        ReadChar;
    end;

  '^':
    begin
	    TokenStr:=LastChar;
	    Token:=TOK_CARET;
      ReadChar;
    end;

  '!':
    begin
	    TokenStr:=LastChar;
	    Token:=TOK_EXCLAMATION;{Not Operand}
      ReadChar;
    end;

  ELSE
    Begin
      ReadChar;
	    Token:=TOK_UNKNOWN;
    End;
 END;{CASE}
 ReadToken:=Token;
END;{ ReadToken }

function TokenFlag( Flags:tTokenFlagSet ) : tAsmToken;
begin
  TokenFlag:=Token;
  if Token=TOK_IDENTIFIER then begin
    if TOK_FLAG_PREFIX in Flags then begin
      TokenInt:=IsPREFIX(TokenUpStr);
      if TokenInt<>-1 then begin
        Token:=TOK_PREFIX;
        TokenFlag:=Token;
        exit;
      end;
    end;
    if TOK_FLAG_OPCODE in Flags then begin
      TokenInt:=IsOPCODE(TokenUpStr);
      if TokenInt<>-1 then begin
        Token:=TOK_OPCODE;
        TokenFlag:=Token;
        exit;
      end;
    end;
    if TOK_FLAG_REGISTER in Flags then begin
      TokenInt:=IsRegister(TokenUpStr);
      if TokenInt<>-1 then begin
        Token:=TOK_REGISTER;
        TokenFlag:=Token;
        exit;
      end;
    end;
    if TOK_FLAG_ROUTINE in Flags then begin
      TokenInt:=IsROUTINE(TokenUpStr);
      if TokenInt<>-1 then begin
        Token:=TOK_ROUTINE;
        TokenFlag:=Token;
        exit;
      end;
    end;
    if TOK_FLAG_DIRECTIVE in Flags then begin
      TokenInt:=IsDIRECTIVE(TokenUpStr);
      if TokenInt<>-1 then begin
        Token:=TOK_DIRECTIVE;
        TokenFlag:=Token;
        exit;
      end;
    end;
    if TOK_FLAG_MODULE in Flags then begin
      TokenInt:=IsMODULE(TokenUpStr);
      if TokenInt<>-1 then begin
        Token:=TOK_MODULE;
        TokenFlag:=Token;
        exit;
      end;
    end;
    if TOK_FLAG_SPECIFIER in Flags then begin
      TokenInt:=IsSPECIFIER(TokenUpStr);
      if TokenInt<>-1 then begin
        Token:=TOK_SPECIFIER;
        TokenFlag:=Token;
        exit;
      end;
    end;
    if TOK_FLAG_OPERATOR in Flags then begin
      TokenInt:=IsOPERATOR(TokenUpStr);
      if TokenInt<>-1 then begin
        Token:=TOK_OPERATOR;
        TokenFlag:=Token;
        exit;
      end;
    end;
  end;
end;

function IsTokenIdent:Boolean;
begin
  Result:= TokenFlag([Low(tTokenFlag)..High(tTokenFlag)] ) = TOK_IDENTIFIER;
end;

end.


