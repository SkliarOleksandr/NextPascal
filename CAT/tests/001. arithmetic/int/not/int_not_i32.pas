unit int_not_i32;
                
interface

implementation

var I: Int32;

procedure Test;
begin
  I := 1;
  I := not I;
end;

initialization
  Test();

finalization
  Assert(I = -2);
end.