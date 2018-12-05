unit deref_2;

interface

implementation

var
  a1, a2: Int8;
  b1, b2: Int16;
  c1, c2: Int32;
  d1, d2: Int64;
  
  pa: ^Int8;
  pb: ^Int16;
  pc: ^Int32;
  pd: ^Int64;      

procedure Test;
begin
  a1 := -10;
  pa := @a1;
  a2 := pa^;
  
  b1 := -2000;
  pb := @b1;
  b2 := pb^;
  
  c1 := -300000;
  pc := @c1;
  c2 := pc^;

  d1 := -40000000000;
  pd := @d1;
  d2 := pd^;        
end;

initialization
  Test();

finalization
  Assert(a2 = -10);
  Assert(b2 = -2000);
  Assert(c2 = -300000);
  Assert(d2 = -40000000000);

  
end.