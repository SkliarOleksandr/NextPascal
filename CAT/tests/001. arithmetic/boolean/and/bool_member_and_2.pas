unit bool_member_and_2;

interface

implementation

var 
  A: Boolean = False;
  B, C, D: record
    State: Boolean;
  end;
  
procedure Test;
begin
  A := B.State and C.State and D.State;
end;

initialization
  B.State := True;
  C.State := True;
  D.State := True;  
  Test();

finalization
  Assert(A);

end.