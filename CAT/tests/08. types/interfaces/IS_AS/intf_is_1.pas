unit intf_is_1;

interface

implementation

uses System;

type
  II1 = interface
  end;  

  II2 = interface
  end;

  TT1 = class(TObject, II1)
  end;

var
  T: TT1;
  G1, G2:  Boolean;
  
procedure Test;
begin
  T := TT1.Create();
  G1 := T is II1;
  G2 := T is II2;  
end;

initialization
  Test();

finalization
  Assert(G1);
  Assert(not G2);  
end.