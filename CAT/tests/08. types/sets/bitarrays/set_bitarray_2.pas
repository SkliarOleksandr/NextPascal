unit set_bitarray_2;

interface

implementation

var
  S: set of 0..15;
  B: Boolean;  

procedure Test;
begin
  S[9] := True;
  B := S[9];
end;

initialization
  Test();

finalization
  Assert(B);
end.