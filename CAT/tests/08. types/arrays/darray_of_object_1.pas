unit darray_of_object_1;

interface

implementation

type
  TC = class
    V: Int32;
  end;
  TA = array of TC;

var
  A: TA;
  G0, G1: Int32;

procedure Test;
begin
  A[0].V := 12;
  A[1].V := 13; 
  
  G0 := A[0].V;
  G1 := A[1].V;     
end;

initialization
  SetLength(A, 2);
  A[0] := TC.Create();
  A[1] := TC.Create();  
  Test();

finalization
  Assert(G0 = 12);
  Assert(G1 = 13);  
end.