unit anonymous_func_0;

interface

implementation

type
  TFunc = function: Int32;

var P: TFunc;
    G: Int32; 
    
procedure Test;
begin
  P := function: Int32 begin
         Result := 441; 
       end;
  G := P();
end;

initialization
  Test();

finalization
  Assert(G = 441);
end.