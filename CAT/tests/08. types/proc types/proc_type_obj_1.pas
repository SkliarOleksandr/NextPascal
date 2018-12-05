unit proc_type_obj_1;

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

procedure Test;
begin
  obj := TC.Create();  
  p := obj.SetData;
  p(11);
  G := obj.FData;
end;

initialization
  Test();

finalization
  Assert(G = 11);
end.