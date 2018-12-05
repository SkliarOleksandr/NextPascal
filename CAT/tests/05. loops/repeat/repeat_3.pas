unit repeat_3;

interface

implementation

var
  G1, G2: Int32;

procedure Test1;
begin
  G1 := 0; 
  repeat
    G1 := G1 + 1;
  until True;  
end;

procedure Test2;
begin
  G2 := 0; 
  repeat
    G2 := G2 + 1;
    if G2 = 2 then
      break;      
  until False;  
end;

initialization
  Test1();
  Test2();  

finalization
  Assert(G1 = 1);
  Assert(G2 = 2);  

end.