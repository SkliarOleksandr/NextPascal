unit class_7;

interface

implementation
uses system;

var G: Int32;

type 
  TC1 = class
    G: Int32;
    procedure P1;
  end;
  
  TC2 = class
    F: TC1; 
    procedure P2;
    constructor Create;
  end;
  
procedure TC1.P1;
begin
  G := 22;
end;  

procedure TC2.P2;
begin
  F.P1();
end;  

procedure TC2.Create;
begin
  F := TC1.Create();
end;  

procedure Test;
var
  C2: TC2;
begin
  C2 := TC2.Create();
  C2.P2();
  G := C2.F.G;
end;

initialization
  Test();

finalization
  Assert(G = 22);
end.