unit params_out_1;

interface

implementation

procedure GetVal(out Value: Int32);
begin
  Value := 4;
end;

var
  G: Int32;

procedure Test;
begin
  GetVal(G);  
end;

initialization
  Test();

finalization
  Assert(G = 4);
end.