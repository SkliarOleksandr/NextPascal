unit ivar_0;

interface

implementation

var G: Int32;
   
procedure Test;
begin
  var i := 3;
  G := i;
end;

initialization
  Test();

finalization
  Assert(G = 3);

end.