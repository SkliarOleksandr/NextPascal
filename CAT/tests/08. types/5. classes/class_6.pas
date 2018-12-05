unit class_6;

interface

implementation

uses System;

type 
  TC1 = class
    FData: Int32;
    procedure P(X, Y: Int32);  
    procedure Run;    
  end;
  
procedure TC1.P(X, Y: Int32);
begin
  FData := X + Y;
end;
  
procedure TC1.Run;
begin
  P(1, 2);
end;      
  
var Obj: TC1;  
  
procedure Test;
begin
  Obj := TC1.Create();
  Obj.Run(); 
end;

initialization
  Test();

finalization
  Assert(Obj.FData = 3);
end.