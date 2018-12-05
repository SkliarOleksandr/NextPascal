unit Unit1;
interface
implementation

var
  F32_1, F32_2: float32;
  F64_1, F64_2: float64;

procedure Test;
begin
  F32_1 := 7.7;
  F32_2 := 7.7;
  Assert(F32_1 = F32_2);  
  
  F64_1 := F32_1;
  F64_2 := F32_2;
  Assert(F64_1 = F64_2);    
end;

initialization
  Test();
  
finalization  
  
end.