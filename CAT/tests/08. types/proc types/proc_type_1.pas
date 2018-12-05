unit proc_type_1;

interface
implementation

var
 G: int32;
 P: procedure(V: Int32);

procedure P1(V: Int32);
begin
  G := V;
end;

procedure Test;
begin
  P := P1;
  P1(222);
end;

initialization
  Test();

finalization
  Assert(G = 222);  

end.