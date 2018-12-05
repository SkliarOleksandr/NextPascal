unit result_record;

interface

implementation

type 
  TRec = record
    x, y: Int32;
  end;
 
function Get: TRec;
begin
  Result.X := 11;
  Result.Y := 12;
end;

procedure Test;
var
  R: TRec;
begin
  R := Get();
  Assert(R.X = 11);
  Assert(R.Y = 12);   
end;

initialization
  Test();

finalization

end.