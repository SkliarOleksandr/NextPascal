unit class_result_1;

interface

implementation

uses System;

function CreateObj: TObject;
begin
  Result := TObject.Create();
end;

var Obj: TObject;

procedure Test;
begin
  Obj := CreateObj();
  Assert(Assigned(Obj));
  Obj := nil;
end;

initialization
  Test();

finalization

end.