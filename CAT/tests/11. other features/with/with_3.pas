unit with_3;

interface

implementation

type 
 
  TAVLNode = record
    FChilds: array[Boolean] of ^TAVLNode;
    Data: Int32;
  end;
  
  PAVLNode = ^TAVLNode;
 
  TStackNode = record
    Node: PAVLNode;
    Comparison: Int32;
  end;

var Stack: array [2] of TStackNode;    
    N0, N1, N2: TAVLNode;  
    G1, G2: Int32;

procedure Init;
begin
  N0.Data := 10;  
  N0.FChilds[False] := @N0;
  N0.FChilds[True] := @N0;  
  Stack[0].Node := @N0;  
  Stack[0].Comparison := 11;
  
  N1.Data := 2;  
  N1.FChilds[False] := @N1;
  N1.FChilds[True] := @N1;  
  Stack[1].Node := @N1;  
  Stack[1].Comparison := 22; 
  
  N2.Data := 3;  
  N2.FChilds[False] := @N2;
  N2.FChilds[True] := @N2;   
end;  

procedure Test;
var
  Left: Boolean;
  StackP, TmpData: Int32;
  P: PAVLNode;
begin
  StackP := 0;
  Left := False;
  TmpData := 33; 
  P := @N2;
  with Stack[StackP] do begin
    Node.FChilds[False].Data := TmpData;
    Node.FChilds[True] := P.FChilds[Left];    
  end;
  G1 := Stack[0].Node.FChilds[False].Data;        
  G2 := Stack[0].Node.FChilds[True].Data;  
end;

initialization
  Init();
  Test();

finalization
  Assert(G1 = 33);
  Assert(G2 = 3);  
end.