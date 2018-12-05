unit record_fields_init_test;
interface
implementation
type

  TRec = record
    a: int32 = 11;
    b: int32 = 22;
  end;

var
  G1, G2: Int32;
  
procedure Test;
var
  R: TRec;
begin
  G1 := R.a;
  G2 := R.b;
end;

initialization
  Test();
  
finalization
  Assert(G1 = 11);
  Assert(G2 = 22);

end.    