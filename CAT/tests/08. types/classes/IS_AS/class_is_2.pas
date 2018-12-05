unit class_is_2;

interface

implementation

uses System;

type 
  TC1 = class(TObject) end;
  TC2 = class(TC1) end;   
  TC3 = class(TC2) end;  

var
  G1, G2, G3: Boolean;
  C: TObject;   

procedure Test;
begin
  C := TC2.Create();
  G1 := C is TC1; 
  G2 := C is TC2;
  G3 := C is TC3;    
end;

initialization
  Test();

finalization
  Assert(G1 = True);
  Assert(G2 = True);
  Assert(G3 = False);    
end.