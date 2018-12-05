unit int_not_i8;
                
interface

implementation

var I: UInt8;

procedure Test;
begin
  I := 128;
  I := not I;
end;

initialization
  Test();

finalization
  Assert(I = 127);
end.