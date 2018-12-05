unit class_4;
interface
uses System;
implementation
type
  TC1 = class
    a, b: int32;
  end;
  
  TC2 = class
    p: TC1;
    procedure Run;
  end;

var c1: TC1;
    c2: TC2;
    g1, g2: Int32;
    GP: TC1;

procedure TC2.Run;
begin
  if Assigned(p) then
  begin
    if p.a = 12 then
      p.b := 13;
  end;
end;

procedure Test;
begin
  c2.run();  
  G1 := c1.a;
  G2 := c1.b;  
  GP := c2.p;
end;

initialization
  c1 := TC1.Create();
  c1.a := 12;
  c2 := TC2.Create();
  c2.p := c1;  
  Test();

finalization
  Assert(GP <> nil);
  Assert(G1 = 12);
  Assert(G2 = 13);  

end.