unit record_overload_op_1;

interface

implementation

type
  TRec = record
    V: Int32;
    operator Add(const Left: TRec; const Right: Int32): TRec;  
  end;

operator TRec.Add(const Left: TRec; const Right: Int32): TRec;
begin
  Result.V := Left.V + Right; 
end;

var
  R: TRec;

procedure Test;
begin
  R.V := 1;
  R := R + 5;
end;

initialization
  Test();

finalization
  Assert(R.V = 6);
end.