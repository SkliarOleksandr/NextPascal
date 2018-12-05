unit class_arc_weak_2;

interface

implementation

uses System;

var
  S1, S2: TObject;
  B: Boolean;
  
weak 
  WC: TObject;     

procedure Test1;
begin
  S1 := TObject.Create(); 
  WC := S1;  
  B := GetRef(WC, S2);
  Assert(B = true); 
end;

procedure Test2;
begin
  S1 := nil; 
  WC := S1;  
  B := GetRef(WC, S2);
end;

initialization
  Test1();
  Test2();  

finalization
  Assert(B = False); 
  Assert(not Assigned(WC));
end.