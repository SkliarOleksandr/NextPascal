unit crc16_kermit;

interface

implementation

type PByte = ^UInt8;

function Crc16Kermit(Data: PByte; Length: UInt16): UInt16;
var
  i: Int32;
  Val : UInt16;
  CRC : UInt16;
  LB, HB:  UInt8;
Begin
  CRC := 0;
  for i := 0 to Length - 1 do
  begin
    Val := (((Data + i)^ XOR CRC) AND $0F) * $1081;
    CRC := CRC SHR 4;
    CRC := CRC XOR Val;
    LB := (CRC and $FF);
    Val := ((((Data + i)^ SHR 4) XOR LB) AND $0F);
    CRC := CRC SHR 4;
    CRC := CRC XOR (Val * $1081);
  end;
  LB := (CRC and $FF);   
  HB := (CRC shr 8); 
  Result := (LB SHL 8) OR HB;      
end;

var Data: Int64;
    CRC: UInt16;

procedure Test;
begin
  Data := $AABBCCDD99887766;
  CRC := Crc16Kermit(PByte(@Data), 8);
end;

initialization
  Test();

finalization
  Assert(CRC = 14171);
end.