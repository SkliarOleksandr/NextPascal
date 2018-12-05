unit class_arc_1;

interface

uses System;

implementation

type
  TC = class
    destructor Destroy; override;
  end;

var
  C1, C2: TObject;
  G: Int32;


destructor TC.Destroy;
begin
  G := 1;
end;

procedure Test;
begin
  G := 0;
  C1 := TC.Create();
  C2 := C1;
end;

initialization
  Test();

finalization
  C1 := nil;
  C2 := nil;
  Assert(G = 1);

end.