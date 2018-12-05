unit record_nested_types_2;

interface

type
  TRec = record
  type
    TInt = Int32;
  var
    FValue: TInt;
    procedure S(V: TInt);     
  end;

implementation

procedure TRec.S(V: TInt);
begin
  FValue := V;
end;   

var
  R: TRec;

procedure Test;
begin
  R.S(5);
end;

initialization
  Test();

finalization
  Assert(R.FValue = 5);
end.