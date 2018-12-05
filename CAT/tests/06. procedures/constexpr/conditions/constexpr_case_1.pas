unit constexpr_case_1;

interface

implementation

function GetValue(a: int32): string; pure;
begin
  case a of
    0: Result := 'zero';
    1: Result := 'one';
    2: Result := 'two';
  else
    Result := 'any';     
  end;
end;

var 
  s0, s1, s2, sa: string;

procedure Test;
begin
  s0 := GetValue(0);
  s1 := GetValue(1);
  s2 := GetValue(2);
  sa := GetValue(3);      
end;

initialization
  Test();

finalization
  Assert(s0 = 'zero');
  Assert(s1 = 'one');
  Assert(s2 = 'two');
  Assert(sa = 'any');      
end.