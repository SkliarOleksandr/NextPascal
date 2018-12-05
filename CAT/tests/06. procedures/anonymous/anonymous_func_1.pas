unit anonymous_func_1;

interface

implementation

type
  TFunc = function(a, b: Int32): Int32;

var P: TFunc;
    G: Int32; 
    
procedure Test;
begin
  P := function(x, y: Int32): Int32 begin
         Result := x + y; 
       end;
  G := P(1, 2);
end;

initialization
  Test();

finalization
  Assert(G = 3);
end.