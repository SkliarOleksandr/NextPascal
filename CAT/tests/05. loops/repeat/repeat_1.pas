unit repeat_1;

interface

implementation

var
  G1, G2: Int32;

procedure Test1;
begin
  G1 := 0;
  repeat
    G1 := G1 + 1;
  until G1 = 10; 
end;

procedure Test2;
var
  B: Boolean;
begin
  G2 := 0;
  repeat
    G2 := G2 + 1;    
    B := G2 = 20;
  until B;
end;

initialization
  Test1();
  Test2();  

finalization
  Assert(G1 = 10);
  Assert(G2 = 20);  

end.