unit bool_member_and_1;

interface

implementation

var 
  A1, A2: Boolean = False;
  B, C, D: record
    State: Boolean;
  end;
  
procedure Test1;
begin
  A1 := B.State and C.State;
end;

procedure Test2;
begin
  A2 := B.State and D.State;
end;

initialization
  B.State := True;
  C.State := True;
  D.State := False;  
  Test1();

finalization
  Assert(A1);
  Assert(not A2);

end.