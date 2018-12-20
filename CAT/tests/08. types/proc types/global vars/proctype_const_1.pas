unit proctype_const_1;

interface

implementation

var G: Int32;

procedure Test;
begin
  G := 22;
end;

type TProc = procedure;

const P: TProc = Test;

initialization
  P();

finalization
  Assert(G = 22);

end.