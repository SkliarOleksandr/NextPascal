unit intf_is_2;

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
  I: II1;
  G1, G2:  Boolean;
  
procedure Test;
begin
  I := TT1.Create();
  G1 := I is II1;
  G2 := I is II2;  
end;

initialization
  Test();

finalization
  Assert(G1);
  Assert(not G2); 

end.