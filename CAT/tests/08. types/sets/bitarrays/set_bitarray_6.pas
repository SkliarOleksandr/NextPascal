unit set_bitarray_6;

interface

implementation

type
  TSet = set of 0..31;
 
var
  S: TSet;
  B: Boolean;   

procedure Test;
var
  V: Boolean;
begin
  V := True;
  S[7] := V;
  B := S[7];
end;

initialization
  Test();

finalization
  Assert(B);
end.