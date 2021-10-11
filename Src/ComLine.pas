{-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-}
{-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-* Win-Assembler *+*-*+*-*+*-*+*-*+*-*+*-*+*-}
{-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-}

Unit ComLine;

INTERFACE
uses SysUtils, Global;


PROCEDURE SplitPath(Const Path:String;Var Dir,Name,Ext:String);

IMPLEMENTATION


PROCEDURE SplitPath(Const Path:String;Var Dir,Name,Ext:String);
 Var i      :Byte;
     DirEndPos,PointPos:Byte;
Begin
 DirEndPos:=0;
 PointPos:=0;
 For i:=1 To Length(Path) Do
  Case Path[I] of
   '\':Begin DirEndPos:=i;PointPos:=0;End;
   '.':PointPos:=i;
  End;

 if DirEndPos>1 Then Dir:=Copy(Path,1,DirEndPos) Else Dir:='';
 if PointPos>1 Then
   Begin
     Name:=Copy(Path, DirEndPos+1,PointPos-DirEndPos-1);
     Ext:=Copy(Path, PointPos,Length(Path)-PointPos+1);
   End
   Else Begin
     Name:=Copy(Path, DirEndPos+1,Length(Path)-DirEndPos);
     Ext:='';
   End;
End;


Var   Path:ShortString;
    Dir,Name,Ext:String;
{ Parse Command Line to extract options directories and files names }
BEGIN
 Writeln(ProgVersion);
 if ParamCount<1 Then Begin
   Writeln(' Syntax: Basm [AsmFile]');
   Readln;
   Halt(0);
 End;

 Path:=ParamStr(0);
 SplitPath(Path,Dir,Name,Ext);
 DirEXEC:=Dir;

 Path:=ParamStr(1);
 SplitPath(Path,Dir,Name,Ext);
 if Ext='' Then Ext:='.asm';
 fASMname:=Dir+Name+Ext;
 if not FileExists(fAsmname) Then Begin
   Writeln('File not found ',fASMname);
   Halt(1);
 End;
 if FileExists(DirExec+'OUT')
   Then DirOUTPUT:=DirExec+'OUT\'
   Else DirOUTPUT:=Dir;
 if FileExists(DirExec+'INC')
   Then DirINCLUDE:=DirExec+'INC\'
   Else DirINCLUDE:=Dir;

 DirSOURCE:=Dir;
 fCOMname:=DirOUTPUT+Name+'.com';
 fEXEname:=DirOUTPUT+Name+'.exe';
 fLSTname:=DirOUTPUT+Name+'.lst';
END.
