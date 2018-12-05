unit varinat_str_1;

interface

implementation

var V: Variant; 

procedure Test;
begin
  V := copy('string_variant');
end;

initialization
  Test();

finalization
  Assert(V = 'string_variant');
end.