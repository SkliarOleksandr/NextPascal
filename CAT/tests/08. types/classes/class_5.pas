unit class_5;
interface
uses System;
implementation
type
  TC1 = class
    a, b, c: int32;
  end;
  
  TC2 = class
    XX: Int32;
    C1: TC1;
  end;
  
  TC3 = class
    XX: Int32;
    C2: TC2;
    procedure Run;
  end;

var c1: TC1;
    c2: TC2;
    c3: TC3;
    G1, G2, G3: Int32;

procedure TC3.Run;
begin
  if Assigned(C2) then
  begin
    if Assigned(C2.C1) then
    begin
      if C2.C1.a = 12 then
      begin
        C2.C1.c := 14;
        C2.C1.b := 13;
      end;  
    end;  
  end;
end;

procedure Test;
begin
  c1 := TC1.Create();
  c1.a := 12;
  c2 := TC2.Create();
  c2.c1 := c1;
  c3 := TC3.Create();
  c3.c2 := c2; 
  c3.run();  
  G1 := c1.a;
  G2 := c1.b;  
  G3 := c1.c;  
end;

initialization 
  Test();

finalization
  Assert(G1 = 12);
  Assert(G2 = 13);
  Assert(G3 = 14);     

end.