unit refcount_2;

interface

implementation

var
  S: string;
  G: Int32; 

procedure Test;
begin
  S := copy('str');
  G := refcount(S);
end;

initialization
  Test();

finalization
  Assert(G = 1);

end.