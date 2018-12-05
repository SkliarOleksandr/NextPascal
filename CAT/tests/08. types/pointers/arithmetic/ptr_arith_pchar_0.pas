unit ptr_arith_pchar_0;

interface

implementation

var S: string = 'XXZ';
   
procedure Test;
var
  P: ^Char;
begin
  P := @S[1]; 
  P^ := 'Y';
end;

initialization
  Test();

finalization
  Assert(S = 'XYZ');
end.