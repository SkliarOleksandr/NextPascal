unit setlength_1;

interface

implementation

var Str: string;

procedure Test;
begin
  SetLength(Str, 1);
  Str[0] := 'A';
  SetLength(Str, 2);
  Str[1] := 'B';
  SetLength(Str, 3);
  Str[2] := 'C';         
end;

initialization
  Test();

finalization
  Assert(Str = 'ABC');
end.