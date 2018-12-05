unit classes_virtual_methods_1;

interface

uses System;

type
  TC1 = class
    procedure VMethod; virtual;
  end;
  
  TC2 = class(TC1)
    procedure VMethod; override;
  end;  

implementation

var G: Int32;

procedure Test;
var
  C:  TC1;
begin 
  C := TC2.Create();
  C.VMethod();
end;

procedure TC1.VMethod;
begin
  G := 1; 
end;

procedure TC2.VMethod;
begin 
  inherited;
  G := 2;  
end;

initialization
  Test();

finalization
  Assert(G = 2); 

end.