unit inline_proc_2;

interface

implementation

uses System;

type
  TT = class
    D: Int32;
    procedure SetD; inline;
  end;
  
procedure TT.SetD;
begin
  D := 5;
end;  

procedure Test;
var
  G: Int32;  
  T: TT;
begin
  T := TT.Create();
  T.SetD();
  G := T.D;
  Assert(G = 5);
end;

initialization
  Test();

finalization

end.