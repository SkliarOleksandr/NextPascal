unit proc_type_obj_2;

interface
implementation
uses System;

type
  TProc = procedure(V: Int32) of object;
  TC = class
  private
    FData: Int32;
    procedure SetData(Value: Int32);  
  end;
    
procedure TC.SetData(Value: Int32);
begin
  FData := Value;
end;      

var 
  obj: TC;
  p: TProc;
  G: Int32;   
    
procedure Assign(var P: TProc);
begin
  p := obj.SetData;
end;

procedure Call(var P: TProc);
begin
  p(13);
end;    
    
procedure Test;
begin
  obj := TC.Create();
  Assign(p);  
  Call(p);  
  G := obj.FData;
end;

initialization
  Test();
finalization
  Assert(G = 13);  
end.