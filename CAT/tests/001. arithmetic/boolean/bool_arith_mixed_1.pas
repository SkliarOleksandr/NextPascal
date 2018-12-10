unit bool_arith_mixed_1;

interface

implementation

function GetBool(Cond: Boolean): Boolean;
begin
  Result := Cond;
end; 

var
  A: Boolean = True;
  R1, R2, B: Boolean = False;

procedure Test; 
begin
  R1 := A and (not B);
  R2 := (not B) and A;  
end;

initialization
  Test();

finalization
  Assert(R1);
  Assert(R2);  
 
end.