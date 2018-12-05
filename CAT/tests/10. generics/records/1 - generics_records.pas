unit Unit1;

interface

implementation

type
  TRec<T> = record
    A: T;
  end;

var
  R: TRec<Int32>;
  
initialization
  R.A := 111;

finalization
  Assert(R.A = 111);

end.