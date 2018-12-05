unit ptr_arith_pchar_1;

interface

implementation

var S: string = 'ASDF';
    P: ^Char;
 
procedure Test;
var
  C: Char;
begin
  P := @S[0];
  
  C := P^;
  Assert(C = 'A');

  Inc(P); 
  C := P^;
  Assert(C = 'S');
  
  P := @S[High(S)];
  C := P^;
  Assert(C = 'F');   
end;

initialization
  Test();

finalization

end.