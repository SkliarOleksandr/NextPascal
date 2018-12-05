unit new_1;
interface
implementation

type
  
  TRec = record
    x, y: Int32;
  end;  
  PRec = ^TRec;

var
  GR: PRec;
  GX: Int32;
  GY: Int32;

procedure Test;
var
  R: PRec;
begin
  New(R);  
  
  GR := R;
  
  R.x := 22;
  R.y := 33;  
    
  GX := GR.x;
  GY := GR.y;  
  
  Free(R);   
  
end;
initialization
  Test();

finalization  
  Assert(GX = 22);
  Assert(GY = 33);  

end.