unit sarray_4;

interface

implementation

type

   PAVLNode = ^TAVLNode;  
   TAVLNode = record
     Childs: array [boolean] of PAVLNode; 
   end;

var Root: TAVLNode; 
    P, P2: PAVLNode;
    
procedure Test;
var 
  C: Int32;
begin
  P := @Root;
  Root.Childs[False] := P;
  Root.Childs[True] := P;   
  C := -1;
  P2 := P.Childs[C < 0];     
end;

initialization
  Test();

finalization
  Assert(P2 = P);

end.