unit class_arc_2;

interface

uses System;

implementation

type
  TC = class
    destructor Destroy; override;
  end;

var
  C, C2: TC;
  G: Int32;

destructor TC.Destroy;
begin
  G := 11;
end;

procedure P(O: TC);
begin
  C2 := O;
end;

procedure Test;
begin
  C := TC.Create();
  P(C);
end;

initialization
  G := 0;
  Test();

finalization
   #warning 'ARC does not working hear!'; 
end.