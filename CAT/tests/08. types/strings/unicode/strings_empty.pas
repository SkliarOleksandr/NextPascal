unit strings_empty;

interface

implementation

var S: string;
    G: Int32;
procedure Test;
begin
  S := '';
  if S = '' then
    G := 1; 
end;

initialization
  Test();

finalization
  Assert(G = 1);

end.