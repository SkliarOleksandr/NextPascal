unit const_record_2;

interface

implementation

type
  TRec = record
    a, b: string;
  end;

const R: TRec = ['aaa', 'bbb']; 

var G1, G2: string;

procedure Test;
begin
  G1 := R.a;
  G2 := R.b;  
end;

initialization
  Test();

finalization
  Assert(G1 = 'aaa');
  Assert(G2 = 'bbb');  

end.