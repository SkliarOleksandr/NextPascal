unit low_high_1;

interface

implementation

var
  L, H: Int32; 

procedure Test;
begin
  L := Low(Int16);
  H := High(Int16);  
end;

initialization
  Test();

finalization
  Assert(L = -32768);
  Assert(H = 32767);
    
end.