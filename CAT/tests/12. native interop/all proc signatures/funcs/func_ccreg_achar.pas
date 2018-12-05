unit func_ccreg_achar;

interface

implementation

function Func(a, b: AnsiChar): AnsiChar; external 'CAT' name 'func_ccreg_achar'; 

var
  R: AnsiChar;
  
procedure Test;
begin
  R := Func('A', 'B');
  Assert(R = AnsiChar('B'));  
end;  

initialization
  Test();

finalization

end.