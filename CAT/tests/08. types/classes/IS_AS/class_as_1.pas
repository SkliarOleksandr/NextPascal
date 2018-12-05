unit class_as_1;

interface

implementation

uses System;

var
  C1, C2: TObject;   

procedure Test;
begin
  C1 := TObject.Create();
  C2 := C1 as TObject; 
end;

initialization
  Test();

finalization
  Assert(C1 = C2);
end.