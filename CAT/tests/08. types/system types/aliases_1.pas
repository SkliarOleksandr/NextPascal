unit aliases_1;

interface

type
  MyInt32 = Int32;
  MyFlt = Float64;
  MyStr = string;

implementation

var
  I: MyInt32;
  F: MyFlt;
  S: MyStr;
  
  NI: Int32;
  NF: Float64;
  NS: string;

procedure Test;
begin
  I := 12;
  F := 2.5;
  S := 'alias';
  
  NI := I;
  NF := F;
  NS := S;
end;

initialization
  Test();

finalization
  Assert(I = NI);
  Assert(F = NF);
  Assert(S = NS);    

end.