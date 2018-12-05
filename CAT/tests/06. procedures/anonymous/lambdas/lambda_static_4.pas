unit lambda_4;

interface

implementation

var p: procedure;
    G: Int32;

procedure Test;
begin
  p := ~[]() begin G := 443; Inc(G); end;  
  p();
end;

initialization
  Test();

finalization
  Assert(G = 444);
end.