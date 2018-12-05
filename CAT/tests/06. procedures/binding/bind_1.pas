unit bind_1;

interface

implementation

function Sum(a, b, c: Int32): Int32;
begin
  Result := (a + b)*c;
end; 

var G: int32; 
              
procedure Test;
begin 
  var F := bind Sum(a, b, 10); 
  G := F(1, 2);
end;

initialization
  Test();

finalization
  Assert(G = 30);
  
end.