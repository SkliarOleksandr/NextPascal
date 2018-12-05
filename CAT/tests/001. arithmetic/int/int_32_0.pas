unit int_32_0;

interface

implementation

var 
  i, j, r: Int32;

procedure Test;
begin
  i := 9;
  j := 9;
  r := j - i - 1;  
end;

initialization
  Test();

finalization
  Assert(r = -1);
end.