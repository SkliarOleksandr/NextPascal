unit try_finally_1;

interface
implementation

var
  G: Int32;

procedure Test;
begin
  try
    G := 1;
    Exit; 
  finally
    G := G + 1;  
  end;  
end;

initialization
  Test();

finalization
  Assert(G = 2);  
    
end.