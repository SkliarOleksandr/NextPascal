unit intf_1;

interface

implementation

uses System;

type
  II1 = interface
    procedure AAA;
  end;  

  TT1 = class(TObject, II1)
    procedure AAA;  
  end;
  
var G: Int32;

procedure TT1.AAA;
begin
  G := 12;
end;

var T: TT1;
    I: II1;

procedure Test;
begin
  T := TT1.Create();
  I := T;
  I.AAA();
end;

initialization
  Test();

finalization
  Assert(G = 12);

end.