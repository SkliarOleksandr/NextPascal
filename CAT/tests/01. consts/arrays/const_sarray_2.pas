unit const_sarray_2;

interface

implementation

type
  TArr = array [5] of Int8;

const CA: TArr = (1, 2, 3, 4, 5);

var VA: TArr;       

procedure Test;
begin
  for var i := 0 to 4 do
    VA[i] := CA[i];
end;

initialization
  Test();

finalization
//  Assert(VA[0] = CA[0]);
//  Assert(VA[4] = 5);  

end.