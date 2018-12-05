#skip 'да бля ще не сделал!!!'
unit func_ccreg_set80;

interface

implementation

type
  TSet80 = packed set of 0..79;

function F(a: TSet80): TSet80; external 'CAT' name 'func_ccreg_set80'; 

procedure Test;
var
  a, r: TSet80;
begin
  a := [33]; 
  R := F(a);
  Assert(a = r);  
end;

initialization
  Test();

finalization

end.