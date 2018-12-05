unit class_members_3;

interface

implementation

uses system;

type
  TC1 = class
    FData: Int32;
  end;
  
  TC2 = class
    FC: TC1;
    function GetFC: TC1;
  end; 
  
  TC3 = class
    FC: TC2;
    constructor Create;     
  end;   

function TC2.GetFC: TC1;
begin
  Result := FC;
end;

constructor TC3.Create;
begin
  FC := TC2.Create(); 
  FC.FC := TC1.Create(); 
  FC.FC.FData := 53;
end;

var C: TC3;
    G: Int32;

procedure Test;
begin
  C := TC3.Create();
  G := C.FC.GetFC().FData;
end;

initialization
  Test();

finalization
  Assert(G = 53);
end.