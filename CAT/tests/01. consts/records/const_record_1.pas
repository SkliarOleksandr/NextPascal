unit const_record_1;

interface

implementation

type
  TRec = record
    a, b: Int32;
  end;

const R: TRec = [12, 13]; 

var G1, G2: Int32;

procedure Test;
begin
  G1 := R.a;
  G2 := R.b;  
end;

initialization
  Test();

finalization
  Assert(G1 = 12);
  Assert(G2 = 13);  
end.