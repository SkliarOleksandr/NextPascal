unit with_2;
interface
implementation

type
  TRecA = record
    a, b: int32;
  end;
  
  TRecB = record
    x, y: int32;
  end;  

var  
  G1, G2: Int32;
  RA: TRecA;
  RB: TRecB;  
  
procedure Test;    
begin
  with RA do begin 
    a := 22;
    b := 33;
    with RB do begin 
      x := a;
      y := b;
      G1 := x;
      G2 := y;
    end;
  end;	
end;

initialization
  Test();

finalization  
  Assert(RA.a = 22);
  Assert(RA.b = 33);
  Assert(RB.x = 22);
  Assert(RB.y = 33);  
  Assert(G1 = 22);
  Assert(G2 = 33);  

end.