unit sarray_3;

interface

implementation

type 
  TRec =  array[Boolean] of Int32;

var Node1, Node2: TRec; 

procedure Test1;
begin
  Node1[False] := 1;
  Node1[True] := 2;
end;

procedure Test2(Comparison: Int32);
begin
  Node2[Comparison > 0] := 1;
  Node2[Comparison < 0] := 2;
end;

initialization
  Test1();
  Test2(5);  

finalization
  Assert(Node1[False] = 1);
  Assert(Node1[True] = 2);
  
  Assert(Node2[False] = 2);
  Assert(Node2[True] = 1);     
end.