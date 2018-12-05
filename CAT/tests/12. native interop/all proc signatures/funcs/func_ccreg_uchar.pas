unit func_ccreg_uchar;

interface

implementation

function Func(a, b: Char): Char; external 'CAT' name 'func_ccreg_uchar'; 

var
  R: Char;
  
procedure Test;
begin
  R := Func('D', 'A');
  Assert(R = 'D');  
end;  

initialization
  Test();

finalization

end.