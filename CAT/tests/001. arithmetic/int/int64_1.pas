unit int64_1;

interface

uses System;

var
  G1, G2: Int64; 

implementation

procedure Test;
begin
  G1 := MinInt64;
  G2 := MaxInt64;  
end;

initialization
  Test();

finalization
  Assert(G1 = MinInt64);
  Assert(G2 = MaxInt64);

end.