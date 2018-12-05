unit nested_procs;

interface

var
  G1: Int32;
  G2: Int32;

procedure Test; export;

implementation

procedure Test;
  function GetValue: Int32;
  begin
    Result := 11;
  end;
  procedure SetValue(V: Int32);
  begin
    G2 := V;
  end;    
begin
  G1 := GetValue();
  SetValue(12);
end;

initialization
  Test();

finalization  
  Assert(G1 = 11);  
  Assert(G2 = 12);  

end.