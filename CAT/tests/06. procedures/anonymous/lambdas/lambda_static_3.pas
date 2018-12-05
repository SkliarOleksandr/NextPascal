unit lambda_3;

interface

implementation

var p: procedure;
    G: Int32;

procedure Test;
begin
  p := ~[]() G := 443;  
  p();
end;

initialization
  Test();

finalization
  Assert(G = 443);
end.