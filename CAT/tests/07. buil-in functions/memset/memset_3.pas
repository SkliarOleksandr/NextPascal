unit memset_3;

interface

implementation

type
  TRec = record
    x, y, z: UInt8;
  end;

var
  R: TRec;

procedure Test;
var
  LR: TRec;
begin
  memset(LR, 3);
  R := LR;  
end;

initialization
  Test();

finalization
  Assert(R.x = 3);
  Assert(R.y = 3);
  Assert(R.z = 3);    
end.