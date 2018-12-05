unit array_open_1;
interface
implementation

var
  GL, G0, G1, G2, G3: Int32;  

procedure P1(Values: openarray of Int32);
begin
  GL := Length(Values);
  G0 := Values[0];
  G1 := Values[1];
  G2 := Values[2];
  G3 := Values[3];      
end;

procedure Test;
begin
  P1([11, 22, 33, 44]);  
end;

initialization
  Test();

finalization
  Assert(GL = 4); 
  Assert(G0 = 11);     
  Assert(G1 = 22);
  Assert(G2 = 33);
  Assert(G3 = 44);          
end.