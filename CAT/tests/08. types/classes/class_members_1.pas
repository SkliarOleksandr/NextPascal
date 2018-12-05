unit class_8;

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
    constructor Create;     
  end; 

function TC1.GetData: Int32;
begin
  Result := FData;
end;

constructor TC2.Create;
begin
  FC := TC1.Create();
  FC.FData := 51;
end;

var C: TC2;
    G: Int32;

procedure Test;
begin
  G := C.FC.GetData();
end;

initialization
  C := TC2.Create();
  Test();

finalization
  Assert(G = 51);
end.