unit array_open_2;
interface
implementation

var
  GL, GS: Int32;  

function Sum(Values: openarray of Int32): Int32;
var
  i: Int32;
begin
  Result := 0;
  GL := Length(Values); 
  for i := 0 to GL - 1  do
    Result := Result + Values[i];
end;

procedure Test;
begin
  GS := Sum([]);
  Assert(GL = 0); 
  Assert(GS = 0);     
  
  GS := Sum([1, 2, 3, 4, 5]);
  Assert(GL = 5); 
  Assert(GS = 15);     
end;

initialization
  Test();

finalization

end.