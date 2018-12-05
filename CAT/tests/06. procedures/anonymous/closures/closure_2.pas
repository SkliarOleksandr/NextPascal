unit closure_2;

interface

implementation

uses system;

type
  TProc = procedure of object;

var 
  P: TProc;
  G: Int32;

procedure Test;
var
  L1, L2: Int32;
begin
  L1 := 51;
  L2 := 1;
  P := procedure 
       begin
         G := L1 + L2; 
       end;
  P();      
end;

initialization
  Test();

finalization
  Assert(G = 52);
end.