unit class_dtor_1;

interface

implementation

uses System;

type
  
 TC1 = class
    FStr: string;    
    constructor Create; 
    destructor Destroy; override;
  end;    
  
implementation  
  
var
  GS: string; 
   
constructor TC1.Create; 
begin
  FStr := 'string';
  FStr := FStr + ' string';  
end; 

destructor TC1.Destroy; 
begin

end; 
  
procedure Test;
var
  Obj: TC1;  
begin
  Obj := TC1.Create();
  GS := Obj.FStr;
end;

initialization
  Test();

finalization

end.