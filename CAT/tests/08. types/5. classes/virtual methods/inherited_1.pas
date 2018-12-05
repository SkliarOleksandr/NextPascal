unit inherited_1;

interface

uses System;

type    
  TC1 = class
    function GetValue: Int32; virtual;
  end;

  TC2 = class(TC1)
    function GetValue: Int32; override;
  end;  

implementation


function TC1.GetValue: Int32; 
begin
  Result := 5;
end;

function TC2.GetValue: Int32; 
begin
  Result := inherited + 1;  
end;

var
  G: Int32;

procedure Test;
var
  C: TC1;
begin
  C := TC2.Create();
  G := C.GetValue();
end;

initialization
  Test();

finalization
  Assert(G = 6);
end.