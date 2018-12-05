unit for_loop_speed_test_1;

interface

implementation

var 
  G : Int32;  

procedure Test;
const
  c = 40;
begin
  G := 0;
  for var x := 0 to c - 1 do begin
    for var y := 0 to c - 1 do begin
      for var z := 0 to c - 1 do begin
        G := x + y + z;
      end;
    end;
  end;
end;

initialization
  Test();

finalization
  Assert(G = 117);
end.