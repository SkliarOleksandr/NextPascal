unit cpp_test2;

interface

procedure Test(Cnt: Int32); export;

implementation


var g: int32;

function Sum(x, y: Int32): Int32;
begin
  Result := x + y;
end;

procedure Test(Cnt: Int32);
var
  x, y, z: Int32;
  i: int32; 
begin
  x := 1;
  y := 2;
  z := 3;

  for i := 1 to Cnt do 
  begin   
    if x > 0 and (x < y and x < z) then
    begin
      x := Sum(x, 1);
      y := Sum(y, 1);
      z := Sum(z, 1);                     
    end;
  end;  
  g := x + y + z;
end;


initialization
  Test(100);

finalization
  Assert(g = 306);
end.