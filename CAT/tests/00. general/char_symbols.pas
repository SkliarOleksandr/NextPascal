unit char_symbols;

interface

implementation

var
  c: char;
  b: UInt8;

procedure Test;
begin
  c := #65;
  b := UInt8(c);
end;

initialization
  Test();

finalization
  Assert(b = 65);

end.