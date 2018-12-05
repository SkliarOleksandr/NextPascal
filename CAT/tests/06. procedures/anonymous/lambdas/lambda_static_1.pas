unit lambda_1;

interface

implementation

var p: procedure;
    G: Int32;

procedure Test;
begin
  p := ~ G := 555;   
  p();
end;

initialization
  Test();

finalization
  Assert(G = 555);
end.