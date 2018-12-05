unit class_of_1;

interface

uses System;

implementation

type

  TC = class
    class function GetFive: Int32;
  end;
  
class function TC.GetFive: Int32;
begin
  Result := 5;
end;

type
  TClass = class of TC;
  
var 
  G: Int32;
  
procedure Test;
begin
  G := TClass.GetFive();
end;

initialization
  Test();

finalization
  Assert(G = 5);
end.