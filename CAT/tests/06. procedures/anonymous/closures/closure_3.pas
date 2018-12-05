unit closure_3;

interface

implementation

uses system;

type
  TProc = procedure of object;

var 
  P: TProc;
  G1, G2: Int32;

procedure Test;
var
  L: Int32;
begin
  L := 2;
  P := procedure
       begin
         L := L + 45; 
         G1 := L;  
       end;
  P();    
  G2 := L;   
end;

initialization
  Test();

finalization
  Assert(G1 = 47);
  Assert(G2 = 2);  
end.