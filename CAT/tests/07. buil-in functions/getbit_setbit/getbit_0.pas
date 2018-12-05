unit getbit_0;

interface

implementation

var
  V: Int32; 
  B: Boolean;    

procedure Test;
begin
  V := %10;
  B := getbit(V, 1);  
end;

initialization
  Test();

finalization
  Assert(B);

end.