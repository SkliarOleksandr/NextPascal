unit Params_3;

interface

implementation

procedure P1(var A, B, C: Int32);
begin
  A := A + 1;
  B := B + 2;
  C := A + B;
end;

var
  GA, GB, GC: Int32;

procedure Test;
begin
  GA, GB := 0;  
  P1(GA, GB, GC);
end;

initialization
  Test();

finalization
  Assert(GA = 1);
  Assert(GB = 2);
  Assert(GC = 3);    

end.