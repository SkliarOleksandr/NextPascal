unit low_high_0;

interface

implementation

var
  L, H: Int64; 

procedure Test;
begin
  L := Low(Int8);
  H := High(Int8); 
end;

initialization
  Test();

finalization
  Assert(L = -128);
  Assert(H = 127);   

end.