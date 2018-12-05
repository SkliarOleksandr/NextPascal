unit conditionl_compilation_1;
interface
implementation

var
  G: Int32;

const
  C = 1;

procedure Test;
begin
  #if c = 1
    G := 2;
  #else
    G := 1;
  #end;
end;

initialization
  Test();
  
finalization
  Assert(G = 2);
  
end.