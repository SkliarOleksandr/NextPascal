unit generic_record_methods_1;

interface

implementation

type
  TRec<T> = record
    F: T;
    procedure SetF(V: T);
  end;
  
var
  R32: TRec<Int32>;

procedure TRec<T>.SetF(V: T);
begin
  F := V;
end;

procedure Test;
begin
  R32.SetF(11);
end;

initialization
  Test();

finalization
  Assert(R32.F = 11);
  
end.