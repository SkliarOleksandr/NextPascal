unit class_is_1;

interface

implementation

uses System;

var
  G: Boolean;
  C: TObject;   

procedure Test;
begin
  C := TObject.Create();
  G := C is TObject; 
end;

initialization
  Test();

finalization
  Assert(G = True);
end.