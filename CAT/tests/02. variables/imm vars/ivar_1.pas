unit ivar_1;

interface

implementation

var G1, G2: Int32;
   
procedure Test;
begin

  var i, c := 5;
  
  G1 := i; 
  G2 := c;
end;

initialization
  Test();

finalization
  Assert(G1 = 5);
  Assert(G2 = 5);  
end.