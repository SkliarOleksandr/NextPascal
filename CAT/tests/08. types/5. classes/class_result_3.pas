unit class_result_3;

interface

implementation

uses System;

type
  TC1 = class
    FData: Int32;
  end;
  
  TC2 = class
    FC: TC1;
    constructor Create; 
    function GetFC: TC1;
  end; 

constructor TC2.Create;
begin
  FC := TC1.Create();
  FC.FData := 55;
end;

function TC2.GetFC: TC1;
begin
  Result := FC;
end;

var Obj: TC2;
    Data: Int32;

procedure Test;
begin
  Data := Obj.GetFC().FData;
end;

initialization
  Obj := TC2.Create();
  Test();

finalization
  Assert(Data = 55);
end.