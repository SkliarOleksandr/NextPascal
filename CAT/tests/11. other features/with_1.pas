unit with_1;
interface
implementation

type
  TRec = record
    a, b: int32;
  end;

var  
  G1, G2: Int32;
  R: TRec;  
  
procedure Test;    
begin
  with R do begin 
    a := 22;
    b := 33;
    G1 := a;
    G2 := b;
  end;
end;

initialization
  Test();

finalization  
  Assert(R.a = 22);
  Assert(R.b = 33);
  Assert(G1 = 22);
  Assert(G2 = 33);  

end.