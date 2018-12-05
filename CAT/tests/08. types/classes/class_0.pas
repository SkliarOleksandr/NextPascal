unit class_0;

interface

implementation

uses System;

var 
  Obj: TObject;

initialization
  Obj := TObject.Create();

finalization
  Assert(Obj <> nil); 
 
end.