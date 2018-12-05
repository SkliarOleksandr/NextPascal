unit closure_1;

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
  L: Int32;
begin
  L := 51;
  P := procedure 
       begin
         G := L; 
       end;
  P();      
end;

initialization
  Test();

finalization
  Assert(G = 51);
end.