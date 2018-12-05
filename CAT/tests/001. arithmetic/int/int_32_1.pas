unit int_32_1;

interface

uses System;

var
  G1, G2: Int32; 

implementation

procedure Test;
begin
  G1 := MinInt32;
  G2 := MaxInt32;  
end;

initialization
  Test();

finalization
  Assert(G1 = MinInt32);
  Assert(G2 = MaxInt32);
end.