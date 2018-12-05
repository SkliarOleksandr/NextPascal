unit record_helper_1;

interface

implementation

type
  TRec = record
    A: Int32;
  end;

  TRecHelper = helper for TRec
  end;


var
  R: TRec;

procedure Test;
begin
  R.A := 43;
end;

initialization
  Test();

finalization

end.