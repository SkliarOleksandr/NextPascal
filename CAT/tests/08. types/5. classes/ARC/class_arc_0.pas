unit class_arc_0;

interface

uses System;

implementation

type
  TC = class
    destructor Destroy; override;
  end;

var
  Obj: TObject;
  G: Int32 = 0;

destructor TC.Destroy;
begin
  G := 1234;
end;

procedure Test;
begin
  Obj := TC.Create();
end;

initialization
  Test();

finalization
  Obj := nil;
  Assert(G = 1234);
end.