unit enum_anonymous_1;

interface

implementation

function F(Value: (a, b, c, d)): Int32;
begin
  Result := ord(value); 
end; 

var
  G1, G2: Int32;

procedure Test;
begin
  G1 := F(b);
//  G2 := F(c);  
end;

initialization
  Test();

finalization
  Assert(G1 = 1);
//  Assert(G2 = 2);  
  
end.