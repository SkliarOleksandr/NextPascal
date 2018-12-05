unit sarray_of_record_1;

interface

implementation

type
  TRec = record
    x, y: Int32;
  end; 
  TA = array [3] of TRec;

var A: TA;

procedure Test;
begin
  for var i := Low(A) to High(A) do
  begin
    A[i].X := i;
    A[i].Y := i;    
  end;  
end;

initialization
  Test();

finalization                   
  Assert(A[Low(A)].X = Low(A)); 
  Assert(A[High(A)].X = High(A)); 
end.