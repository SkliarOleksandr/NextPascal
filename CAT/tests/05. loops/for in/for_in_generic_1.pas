unit for_in_generic_1;

interface

implementation

function Iterate<T>(const Arr: array of T; const Proc: procedure (const Item: T; out BreakIterate: Boolean)): Integer; 
begin
  var Br := False;
  for var Item in Arr do 
  begin
    Proc(Item, Br);
    if Br then
      Exit;
  end;  
end;

var A: array of Int32;
    G: Integer = 0;

procedure Test;
begin
  A := [1, 2, 3, 4, 5];
  Iterate<Int32>(A, procedure (const Item: Int32; out BreakIterate: Boolean)
                 begin
                   G := G + Item;
                 end);  
end;

initialization
  Test();

finalization
  Assert(G = 15);
end.