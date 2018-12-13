unit bool_arith_mixed_2;

interface

implementation
    
var G1, G2: boolean;    
    
function GetBool(Cond: Boolean): Boolean;
begin
  Result := Cond;
end;

procedure Test1;
begin
  G1 := GetBool(True) or GetBool(False) and GetBool(True);
end;

procedure Test2;
begin
  G2 := GetBool(False) or GetBool(True) and GetBool(True);
end;


initialization
  Test1();
  Test2();  

finalization
  Assert(G1);
  Assert(G2);  
end.