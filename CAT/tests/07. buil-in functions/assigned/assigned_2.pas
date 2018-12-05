unit assigned_2;

interface

implementation

var R: record
      a, b: Int32;
    end;
    P: Pointer;

procedure Test;
begin
  P := @R;
  if Assigned(P) then
    R.a := 12;  
     
  P := nil;     

  if not Assigned(P) then
    R.b := 13;     
end;

initialization
  Test();

finalization
  Assert(R.a = 12);
  Assert(R.b = 13);  
end.