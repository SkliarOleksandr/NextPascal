unit str_result_1;

interface

implementation

var S1, S2: string;

function GetStr: string;
begin
  Result := S1;
end;

procedure Test;
begin
  S1 := Copy('string');
  S2 := S1 + S1;
  if GetStr() = S2 then
    S2 := ''
  else   
    S2 := GetStr(); 
end;

initialization
  Test();

finalization
  Assert(S1 = S2);
end.