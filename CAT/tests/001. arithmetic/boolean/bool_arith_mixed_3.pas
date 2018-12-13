unit bool_arith_mixed_3;

interface

implementation

var G1, G2, G3, G4: boolean;    
    
function GetBool(Cond: Boolean): Boolean;
begin
  Result := Cond;
end;

procedure Test1;
begin
  G1 := GetBool(true) or GetBool(false) or not GetBool(true);  
end;

procedure Test2;
begin
  G2 := GetBool(false) or GetBool(true) or not GetBool(true);  
end;

procedure Test3;
begin
  G3 := GetBool(False) or GetBool(False) or not GetBool(False);  
end;

procedure Test4;
begin
  G4 := GetBool(False) or GetBool(False) or not GetBool(True);  
end;


initialization
  Test1();
  Test2();
  Test3();
  Test4();      

finalization
  Assert(G1);
  Assert(G2);
  Assert(G3);
  Assert(not G4);  
      
end.