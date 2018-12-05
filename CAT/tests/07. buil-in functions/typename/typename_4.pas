unit typename_4;

interface

implementation

uses System;

type
  TC1 = class
    function GetName: string;
  end;
  
  TC2 = class(TC1)
  end;

function TC1.GetName: string;
begin
  Result := typename(self);
end;
  
var S: string;  
    
procedure Test;
var
  Obj: TC1;
begin
  Obj := TC2.Create();
  S := Obj.GetName();
end;

initialization
  Test();

finalization
  Assert(S = 'TC1');
end.