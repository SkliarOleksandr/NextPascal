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
    
procedure Test;
var 
  obj: TC;
  p: TProc;   
  G: Int32;
begin
  obj := TC.Create();  
  p := obj.SetData;
  p(12);
  G := obj.FData;
  Assert(G = 12);  
end;

initialization
  Test();

finalization

end.