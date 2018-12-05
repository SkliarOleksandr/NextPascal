unit array_init_2;

interface

implementation

type TArr = array of string;

procedure Test;
var 
  A: TArr;
begin
  SetLength(A, 3);
  A[0] := copy('str0');
  A[1] := copy('str1');
  A[2] := copy('str2');  
  Assert(A[0] = 'str0');
  Assert(A[1] = 'str1');
  Assert(A[2] = 'str2');        
end;

initialization
  Test();

finalization

end.