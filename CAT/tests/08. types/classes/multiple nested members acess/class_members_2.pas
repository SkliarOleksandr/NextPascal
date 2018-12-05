unit class_members_2;

interface

implementation

uses system;

type
  TC1 = class
    FData: Int32;
    function GetData: Int32;
  end;
  
  TC2 = class
    FC: TC1;
  end; 
  
  TC3 = class
    FC: TC2;
    constructor Create;     
  end;   

function TC1.GetData: Int32;
begin
  Result := FData;
end;

constructor TC3.Create;
begin
  FC := TC2.Create(); 
  FC.FC := TC1.Create(); 
  FC.FC.FData := 52;
end;

var C: TC3;
    G: Int32;

procedure Test;
begin
  C := TC3.Create();
  G := C.FC.FC.GetData();
end;

initialization
  Test();

finalization
  Assert(G = 52);
end.