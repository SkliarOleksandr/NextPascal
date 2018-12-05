unit std_system;

interface

implementation

function LoByte(Value: UInt16): UInt8;
begin
  Result := Value and $FF;
end; 

function HiByte(Value: UInt16): UInt8;
begin
  Result := Value shr 8;
end;

function LoWord(Value: UInt32): UInt16;
begin
  Result := Value and $FFFF;
end; 

function HiWord(Value: UInt32): UInt16;
begin
  Result := Value shr 16;
end; 
 
function LoDWord(Value: UInt64): UInt32;
begin
  Result := Value and $FFFFFFFF;
end;      

function HiDWord(Value: UInt64): UInt32;
begin
  Result := Value shr 32;
end;  
 
var
  Src: UInt64 = $0123456789ABCDEF;
  lb, hb:  UInt8; 
  lw, hw:  UInt16;
  ld, hd:  UInt32;  


procedure Test;
begin
  lb := LoByte(Src);
  hb := HiByte(Src);
  lw := LoWord(Src);
  hw := HiWord(Src);
  ld := LoDWord(Src);
  hd := HiDWord(Src);      
end;

initialization
  Test();

finalization
  Assert(lb = $EF);
  Assert(hb = $CD);
  Assert(lw = $CDEF);
  Assert(hw = $89AB);
  Assert(ld = $89ABCDEF);
  Assert(hd = $01234567);           
    
end.