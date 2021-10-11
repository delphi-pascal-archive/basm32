{-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-}
{-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-* Win-Assembler *+*-*+*-*+*-*+*-*+*-*+*-*+*-}
{-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-}

Unit Symbols2;
{$i Switches.inc}

INTERFACE

USES	SysUtils, ObjList, Assemble, Symbols, Segments;

TYPE { Symbols definition }

  pDataStruct = ^tDataStruct;
  tDataStruct = packed object(tDataTypeDef)
      Symbols : tSymbolGroupItem;
      Segment : tSegment;
      Constructor Init(Const sName:String);
      Destructor Done;
      Destructor SelfDestroy; Virtual;
  end;



////////////////////////////////////////////////////////////////////////////////

IMPLEMENTATION

Constructor tDataStruct.Init(Const sName:String);
begin
  Inherited Init(DEF_STRUCT);
  Symbols.Init;
  Segment.Init($FF,SEG_DATA,sName);
end;

Destructor tDataStruct.Done;
begin
  //Symbols.Done;
  Segment.Done;
end;

Destructor tDataStruct.SelfDestroy;
begin
  Done;
  FreeMem( @Self, Sizeof(tDataStruct) );
end;

end.
