unit PathTools;

INTERFACE

PROCEDURE SplitPath(Const Path:String;Var Dir,Name,Ext:String);

IMPLEMENTATION


PROCEDURE SplitPath(Const Path:String;Var Dir,Name,Ext:String);
 Var    i       : Integer;
        DirEndPos,PointPos:Integer;
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
     Ext:=Copy(Path, PointPos+1,Length(Path)-PointPos);
   End
   Else Begin
     Name:=Copy(Path, DirEndPos+1,Length(Path)-DirEndPos);
     Ext:='';
   End;
End;

end.
