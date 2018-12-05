unit getbit_3;

interface

implementation

var
  V: Int64; 
  B: Boolean;    

procedure Test;
begin
  V := Low(Int64);
  B := getbit(V, 63);
end;    

initialization
  Test();

finalization
  Assert(B);
end.