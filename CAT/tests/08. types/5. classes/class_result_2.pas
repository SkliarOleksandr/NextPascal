unit class_result_2;

interface

implementation

uses System;

type
  TC = class
    FC: TC;
    constructor Create;
    function GetFC: TC;
  end;

constructor TC.Create;
begin
  FC := Self;
end;

function TC.GetFC: TC;
begin
  Result := FC;
end;

var O1, O2: TC;

procedure Test;
begin
  O1 := TC.Create();
  O2 := O1.GetFC();  
end;

initialization
  Test();

finalization
  O2.FC := nil;  // remove cycle reference
end.