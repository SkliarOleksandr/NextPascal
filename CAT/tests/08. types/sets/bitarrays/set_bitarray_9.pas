unit set_bitarray_9;

interface

implementation

type
  TSet = set of 0..63;
 
var
  S: TSet;
  B: Boolean;   

procedure Test;
var
  i: Int32;
  V: Boolean;
begin
  i := 63;
  V := True;
  S[i] := V;
  B := S[i];
end;

initialization
  Test();

finalization
  Assert(B);
end.