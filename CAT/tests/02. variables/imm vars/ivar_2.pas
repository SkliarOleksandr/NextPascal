unit ivar_2;

interface

implementation

var
  G: Int32;

procedure Test(V: Int32);
begin
  if V > 0 then
  begin
    var i := V + 1;
    G := i;
  end else begin
    var i := V - 1;
    G := i;  
  end; 
end;

initialization
  Test(2);
  Assert(G = 3);
  Test(-2);
  Assert(G = -3);  

finalization

end.