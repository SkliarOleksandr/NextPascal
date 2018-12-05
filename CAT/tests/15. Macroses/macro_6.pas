unit macro_6;

interface
uses system;
implementation

type 
  TC = class
    F: Int32;
    procedure SetF;
  end;
 
procedure TC.SetF;
begin
  F := 11;
end;    

#macro RUN(OBJ, METHOD);
begin
  #emit OBJ + '.' + METHOD + '()';
end;

var G: Int32;

procedure Test;
var 
  O: TC;
begin
  O := TC.Create();
  RUN(O, SetF);
  G := O.F;
end;

initialization
  Test();
  
finalization
  Assert(G = 11);
end.