unit Unit1;

interface


implementation

var 
  G: Int32;

procedure Test;
var
  a: int32;
begin
  a := 1;
  case a of
    0: G := 1;
    1: G := 2;
  else
    G := 3;
  end;
end;

initialization
  Test();

finalization  
  assert(G = 2);

end.