unit class_forward_decl;

interface

uses System;

type
  TC = class;
  TX = class;
   
  TC = class
    t: TX;
    a: int32;
  end;
 
implementation

var
  c: TC;

procedure Test;
begin
  c := TC.Create();
  c.t := nil;   
  c.a := 12; 
end;

initialization
  Test();

finalization
  Assert(c.a = 12);
end.