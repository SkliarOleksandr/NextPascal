unit repeat_4;

interface

implementation

var
  a, b: Int32;

procedure Test;
begin
  a := 0;
  b := 0;
  repeat
    a := a + 1;
    b := b + 2;
  until (a >= 10) or (b >= 15);
end;

initialization
  Test();

finalization
  Assert(a = 8);
  Assert(b = 16);  

end.