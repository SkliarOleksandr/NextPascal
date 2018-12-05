unit int_not_i8;
                
interface

implementation

var I: Int8;

procedure Test;
begin
  I := 64;
  I := not I;
end;

initialization
  Test();

finalization
  Assert(I = -65);
end.