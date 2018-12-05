unit record_test_1;
interface

type
  
  TRec = record
    a, b: int32;        
  end;    
  
implementation  
  
var
  R: TRec;  
  GA, GB: Int32;
  
procedure Test;
begin
  R.a := 5;
  R.b := 6;
  GA := R.a;
  GB := R.b;
end;

initialization
  Test();
  
finalization
  Assert(GA = 5);
  Assert(GB = 6);  
end.  