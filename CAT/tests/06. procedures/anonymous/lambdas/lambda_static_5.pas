unit lambda_5;

interface

implementation

var p: function: Int32;
    G: Int32;

procedure Test;
begin
  p := ~[](): Int32 Result := 111332;  
  G := p();
end;

initialization
  Test();

finalization
  Assert(G = 111332);
end.