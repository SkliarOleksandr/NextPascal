unit tuple_test_1;
interface

type
  
  TRec = record
    a: int32;
    b: int32;        
  end;    
  
implementation  
  
var
  R: TRec;  
  GA, GB: Int32;
  
procedure Test;
begin
  R := [1, 2];
    
  GA := R.a;
  GB := R.b;
end;

initialization
  Test();
  
finalization
  Assert(GA = 1);
  Assert(GB = 2);  
end.  