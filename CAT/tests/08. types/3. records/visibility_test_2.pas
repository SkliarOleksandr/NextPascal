unit visibility_test_2;

interface

type
  TRec1 = record
  private
    FF: Int32;
    procedure SetF(Value: Int32);
    function GetF: Int32;
  public
    property F: Int32 read GetF write SetF;
  end;  

implementation

var 
  R: TRec1;

procedure TRec1.SetF(Value: Int32);
begin
  FF := Value;
end;

function TRec1.GetF: Int32;
begin
  Result := FF;
end;

procedure Test;
begin
  R.F := 5;
end;

initialization
  Test();

finalization
  Assert(R.F = 5);
end.