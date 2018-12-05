unit set_3;

interface

implementation
  
var
  a: set of (v1, v2, v3, v4, v5);

procedure Test;
begin
  a := [v1, v3, v5];
end;

initialization
  Test();

finalization
  Assert(a = [v1, v3, v5]);

end.