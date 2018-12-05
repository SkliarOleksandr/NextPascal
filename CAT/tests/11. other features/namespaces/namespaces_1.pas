unit namespaces_1;

interface
  
namespace NS1 begin
var
  G: Int32;
const
  CC = 12;    
  namespace NS2 begin
  var
    G: Int32;
  end;    
end;

implementation

procedure Test;
begin
  NS1.G := NS1.CC;
  NS1.NS2.G := 44;
end;

initialization
  Test();

finalization
  Assert(NS1.G = 12);
  Assert(NS1.NS2.G = 44);
end.