unit record_inheritance_1;

interface

implementation

type
  TRec1 = record
    x: int32;    
  end;
  
  TRec2 = record(TRec1)
    y: int32;
  end;

var
  R1: TRec1;
  R2: TRec2;  

procedure Test;
begin
  R1.x := 5;
  
  R2.x := 6;
  R2.y := 7;  
end;

initialization
  Test();

finalization
  Assert(R1.x = 5);
  Assert(R2.x = 6);
  Assert(R2.y = 7);    
end.