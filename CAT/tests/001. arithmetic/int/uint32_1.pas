unit uint32_1;

interface

implementation

uses System;

var
  G1, G2: UInt32; 

implementation

procedure Test;
begin
  G1 := MinUInt32;
  G2 := MaxUInt32;  
end;

initialization
  Test();

finalization
  Assert(G1 = MinUInt32);
  Assert(G2 = MaxUInt32);

end.