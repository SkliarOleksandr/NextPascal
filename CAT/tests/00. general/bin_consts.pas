unit bin_consts;

interface

implementation

var 
  C08: UInt8;
  C16: UInt16;
  C32: UInt32;    
  C64: UInt64;

initialization
  C08 := %01111111;
  C16 := %0111111111111111;
  C32 := %01111111111111111111111111111111;    
  C64 := %0111111111111111111111111111111111111111111111111111111111111111;

finalization

end.