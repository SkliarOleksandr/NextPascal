unit ifdef_1;

interface

implementation

var
  G1, G2: int32;

procedure Test1;
begin
  #ifdef DEF1 
    G1 := 1; 
  #else
    G1 := 2;
  #end;     
end;

#define DEF1; 

procedure Test2;
begin
  #ifdef DEF1 
    G2 := 1; 
  #else
    G2 := 2;
  #end;     
end;

initialization
  Test1();
  Test2();  

finalization
  Assert(G1 = 2);
  Assert(G2 = 1);  
end.