unit recursion_1;

interface

var G: Int32;

implementation

function FindNumber(StartValue, TargetValue: Int32): Int32;
begin
  if StartValue = TargetValue then
    Result := -StartValue
  else begin
    StartValue := StartValue + 1;
    Result := FindNumber(StartValue, TargetValue);
  end;     
end;

procedure Test;
begin
  G := FindNumber(0, 100);
end;

initialization
  Test();

finalization
  Assert(G = -100);
end.