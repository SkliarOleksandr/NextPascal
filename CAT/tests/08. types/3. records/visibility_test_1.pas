unit visibility_test_1;

interface

type
  TRec1 = record
  strict private
    FF: Int32;
  public
    property F: Int32 read FF write FF;
  end;  

implementation

var 
  R: TRec1;

procedure Test;
begin
  R.F := 5;
end;

initialization
  Test();

finalization
  Assert(R.F = 5);
end.