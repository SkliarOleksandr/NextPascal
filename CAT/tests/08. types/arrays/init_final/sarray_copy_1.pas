unit sarray_copy_1;

interface

implementation

type TArr = array [2] of string;

var A1, A2: TArr;
    S: string;

procedure Test;
begin
  A1[0] := S;
  A1[1] := S;
  
  A2 := A1;
end;

initialization
  S := copy('string');
  Test();

finalization
  Assert(A1[0] = 'string');
  Assert(A2[0] = 'string');  

end.