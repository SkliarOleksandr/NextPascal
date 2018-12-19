unit simple_func_call_1;

interface

implementation

var 
  G: Int32;
  
function GetFive: Int32;
begin
  Result := 5;
end;

initialization
  G := GetFive;
   
finalization
  Assert(G = 5);
end.