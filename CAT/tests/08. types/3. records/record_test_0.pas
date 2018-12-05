unit record_test_0;

interface

type
  TRec = record
    FLD: Int32;
    procedure Run;  
  end;

implementation

procedure TRec.Run;
begin
  FLD := 5;
end;

var R: TRec;

procedure Test;
begin
  R.Run();
end;

initialization
  Test();

finalization

end.