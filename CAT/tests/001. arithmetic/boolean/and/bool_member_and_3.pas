unit bool_member_and_3;

interface

implementation

var 
  A1, A2: Boolean = False;
  B, C, D: record
    State: Boolean;
  end;
  
procedure Test;
begin
  A1 := B.State and C.State and D.State;
  A2 := not (B.State and C.State and D.State); 
end;

initialization
  B.State := False;
  C.State := True;
  D.State := True;  
  Test();

finalization
//  Assert(A1);

end.