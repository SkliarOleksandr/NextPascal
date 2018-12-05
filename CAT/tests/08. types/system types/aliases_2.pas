unit aliases_2;

interface

implementation

type 
  T1 = Int32;
  T2 = Int32;

var G1: T1;
    G2: T2;
    G3: Int32;
    S1: string;
procedure Test;
begin
  G3 := 1;
  G1 := G3;
  G2 := G3;   
  S1 := TypeName(G1);
end; 

initialization
  Test();

finalization

end.