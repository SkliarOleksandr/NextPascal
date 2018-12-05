unit lambda_2;

interface

implementation

var p: procedure;
    G: Int32;

procedure Run(p: procedure);
begin
  p();
end;

procedure Test;
begin
  Run(~ G := 33);  
end;

initialization
  Test();

finalization
  Assert(G = 33);
end.