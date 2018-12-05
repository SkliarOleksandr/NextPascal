unit set_bitarray_8;

interface

implementation

type
  TSet = set of 0..31;
 
var
  S: TSet;
  B: Boolean;   

procedure Test;
var
  i: Int32;
  V: Boolean;
begin
  i := 2;
  V := True;
  S[i] := V;
  B := S[i];
end;

initialization
  Test();

finalization
  Assert(B);
end.