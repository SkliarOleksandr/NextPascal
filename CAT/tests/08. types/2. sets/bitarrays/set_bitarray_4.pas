unit set_bitarray_4;

interface

implementation

type
  TSet = set of 0..15;
 
var
  S: TSet;
  B: Boolean; 

procedure Test;
var
  i: int32;
begin
  i := 5;
  S[i] := True;
  B := S[i];
end;

initialization
  Test();

finalization
  Assert(B);
end.