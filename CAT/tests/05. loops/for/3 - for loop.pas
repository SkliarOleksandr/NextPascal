unit Unit1;

interface

var 
  G1: Int32;
  G2: Int32;
  G3: Int32;

implementation

procedure Test1;
var
  i: Int32;
begin
  G1 := 0;
  for i := 0 to 9 do 
    G1 := G1 + 1;
end;

procedure Test2;
const
  c = 100;
begin
  G2 := 0;
  for var i := 0 to c - 1 do 
    G2 := G2 + 1;
end;

procedure Test3;
const
  c = 10;
begin
  G3 := 0;
  for var i := 0 to c - 1 step 2 do 
  begin
    G3 := G3 + 1;
  end;  
end;   

initialization      
  Test1();
  Test2();  
  Test3();       
  
finalization  
  Assert(G1 = 10); 
  Assert(G2 = 100);  
  Assert(G3 = 5);    
  
end.