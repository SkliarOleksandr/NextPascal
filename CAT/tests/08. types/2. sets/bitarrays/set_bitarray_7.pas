unit set_bitarray_7;

interface

implementation

type
  TSet = set of 0..63;
 
var
  S: TSet;
  B: Boolean; 

procedure Test;
var
  V: Boolean;
begin
  V := True;
  S[63] := V;
  B := S[63];
end;

initialization
  Test();

finalization
  Assert(B);
end.