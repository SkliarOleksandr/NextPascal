unit grecord_0;

interface

implementation

type
  TRec<T> = record
    A: T;
    procedure SetA(const V: T);
  end;

procedure TRec<T>.SetA(const V: T);
begin
  A := V;
end;

var R: TRec<string>;

procedure Test;
begin
  R.SetA('xxx');
end;

initialization
  Test();

finalization
  Assert(R.A = 'xxx');
end.