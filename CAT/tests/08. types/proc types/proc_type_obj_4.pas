unit proc_type_obj_4;

interface
implementation
uses System;

type
  TProc = procedure(V: Int32) of object;
  TC = class
  private
    FData: Int32;
    FProc: TProc;
    procedure Test;
    procedure SetData(Value: Int32);  
  end;
    
procedure TC.Test;
begin
  FProc := SetData;
  FProc(15);
end;      
    
procedure TC.SetData(Value: Int32);
begin
  FData := Value;
end;      

var 
  obj: TC;
  G: Int32;   
         
procedure Test;
begin
  obj := TC.Create();
  obj.Test();
  G := obj.FData;
end;

initialization
  Test();
finalization
  Assert(G = 15);  
end.