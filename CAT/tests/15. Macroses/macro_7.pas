unit macro_7;

interface
uses system;
implementation

type 
  TC = class
    function GetF: Int32;
  end;
 
function TC.GetF: Int32;
begin
  Result := 12;
end;    

#macro RUN(OBJ, METHOD);
begin
  #emit 'Result := ' + OBJ + '.' + METHOD + '();';
end;

var G: Int32;

procedure Test;
var 
  O: TC;
  Result: Int32;
begin
  O := TC.Create();
  RUN(O, GetF);
  G := Result; 
end;

initialization
  Test();
  
finalization
  Assert(G = 12);
end.