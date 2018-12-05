unit generic_classes_2;

interface

implementation

uses System;

type
  TC<T1, T2, T3> = class  
    A: T1;   
    B: T2;
    C: T3;    
    procedure P(x, y, z: Int32);
    procedure Run;    
  end; 

procedure TC<T1, T2, T3>.P(x, y, z: Int32);
begin
  A := x;
  B := y;
  C := z;      
end;

procedure TC<T1, T2, T3>.Run;
begin
  P(1, 2, 3);
end;

var Obj: TC<Int32, Int32, Int32>;
  
procedure Test;
begin
  Obj := TC<Int32, Int32, Int32>.Create(); 
  Obj.Run(); 
end;

initialization
  Test();

finalization
  Assert(Obj.A = 1);
  Assert(Obj.B = 2);
  Assert(Obj.C = 3);    
end.