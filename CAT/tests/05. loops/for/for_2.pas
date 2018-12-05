unit for_2;

interface

implementation

var G: Int32;

procedure Test;
begin
  G := 0;
  for var i := 1 to 10 do 
  begin
    for var i := 1 to 10 do 
    begin
      G := G + 1;
    end;         
  end;  
end;

initialization
  Test();

finalization
  Assert(G = 100);
end.