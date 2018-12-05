unit generics_record_alias_type;

interface

implementation

type
  TRec<T> = record
    A: T;
  end;

  TT = TRec<Int32>;
  
var
  R: TT;   

procedure Test;
begin
  R.A  := 5;
end;

initialization
  Test();

finalization

end.