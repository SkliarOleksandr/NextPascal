unit class_arc_result_2;

interface

implementation

uses System;

var C: TObject;

function GetObj: TObject;
begin
  Result := TObject.Create();
  Result := TObject.Create();   
end;

procedure Test;
begin
  C := GetObj();
end;

initialization
  Test();

finalization

end.