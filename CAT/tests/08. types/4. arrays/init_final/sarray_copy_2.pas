unit sarray_copy_2;

interface

implementation

type TArr = array [2] of string;

procedure Test;
var 
  A1, A2: TArr;
  S: string;
begin
  S := copy('str0');
  A1[0] := S;
  A1[1] := S;
  
  A2 := A1;
  
  Assert(A1[1] = S);
  Assert(A2[1] = S);            
end;

initialization
  Test();

finalization

end.