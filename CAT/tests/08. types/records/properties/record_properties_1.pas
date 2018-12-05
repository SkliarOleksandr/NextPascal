unit record_properties_1;
interface

type
  
  TRec = record
    a, b: int32; 
    property AR: Int32 read a;  
    property ARW: Int32 read a write a;
    property BR: Int32 read b;  
    property BRW: Int32 read b write b;
  end;    
  
implementation  
  
var
  R: TRec;  
  GA, GB: Int32;
  
procedure Test;
begin
  R.ARW := 5;
  R.BRW := 6;
  GA := R.AR;
  GB := R.BR;
end;

initialization
  Test();
  
finalization
  Assert(GA = 5);
  Assert(GB = 6);  
end.  