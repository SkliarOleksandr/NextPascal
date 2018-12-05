unit class_2;

interface

implementation

uses System;

type
 
  TC2 = class;
 
  TC1 = class
    FData: TC2;
    procedure SetData;
  end;
  
  TC2 = class
    FData: TC1;
    procedure SetData;    
  end;  

var
  C1: TC1;
  C2: TC2;

procedure TC1.SetData;
begin 
  FData := C2;
  FData := nil;
end;

procedure TC2.SetData;
begin 
  FData := C1;
  FData := nil;  
end;

procedure Test;
begin
  C1 := TC1.Create();
  C2 := TC2.Create();
  C1.SetData();
  C2.SetData();    
end;

initialization
  Test();

finalization
  Assert(C1.FData = nil);
  Assert(C2.FData = nil);  

end.