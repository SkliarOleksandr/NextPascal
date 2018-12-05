unit record_overload_operators;
interface
implementation

type
  TRec = record
    F: int32;
    operator Add(const Left, Right: TRec): TRec;
  end;

var
  R: TRec;
 
operator TRec.Add(const Left, Right: TRec): TRec;
begin
  Result.F := Left.F + Right.F;
end;

procedure Test;
var
  X, Y: TRec;
begin
  X.F := 1;
  Y.F := 2;
  R := X + Y;
end;

initialization
  Test();
  
finalization
  Assert(R.F = 3);

end.    