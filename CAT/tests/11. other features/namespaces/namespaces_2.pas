unit namespaces_2;

interface

namespace NS1 begin
var
  G: Int32;
  procedure P1;
end;

implementation

procedure NS1.P1;
begin
  G := 5;  
end;

procedure Test;
begin
  NS1.P1();
end;

initialization
  Test();

finalization
  Assert(NS1.G = 5);
end.