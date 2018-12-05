unit hex_consts;

interface

implementation

var 
  C08: Int32;
  C16: Int32;
  C32: UInt32;    
  C64: UInt64;

initialization
  C08 := $FF;
  C16 := $FFFF;
  C32 := $FFFFFFFF;    
  C64 := $7FFFFFFFFFFFFFFF;

finalization

end.