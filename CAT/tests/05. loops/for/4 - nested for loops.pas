unit Unit1;

interface

var 
  G: Int32;

implementation

procedure Test;
begin
  G := 0;
  for var x := 1 to 10 do begin
    for var y := 1 to 10 do begin
      for var z := 1 to 100 do begin
        G := G + 1;
      end;
    end;
  end;
end;

initialization
  Test();
    
finalization  
  Assert(G = 10000);    
  
end.