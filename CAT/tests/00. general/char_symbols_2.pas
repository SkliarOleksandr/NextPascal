unit char_symbols_2;

interface

implementation

var S: string;
  
procedure Test;
begin
  S := #65#66#67;
end;

initialization
  Test();

finalization
  Assert(S = 'ABC');
end.