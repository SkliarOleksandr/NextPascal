unit set_bitarray_3;

interface

implementation

var
  S: set of 0..63;
  G: Boolean;  

procedure Test;
begin
  S[48] := True;
  G := S[48];
end;

initialization
  Test();

finalization
  Assert(G);
end.