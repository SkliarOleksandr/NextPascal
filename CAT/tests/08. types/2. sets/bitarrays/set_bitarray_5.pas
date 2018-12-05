unit set_bitarray_5;

interface

implementation

type
  TSet = set of 0..63;
 
var
  S: TSet;
  B: Boolean; 

procedure Test;
var
  i: int32;
begin
  i := 63;
  S[i] := True;
  B := S[i];
end;

initialization
  Test();

finalization
  Assert(B);
end.