unit intf_as_2;

interface

implementation

uses System;

type
  II1 = interface
  end;  

  II2 = interface(II1)
  end;

  II3 = interface(II1)
  end;

  TT1 = class(TObject, II1, II2)
  end;

var
  I1: II1;
  I2: II2; 
  I3: II3;
  
procedure Test;
begin
  I1 := TT1.Create();
  I2 := I1 as II2;
  I3 := I2 as II3;  
end;

initialization
  Test();

finalization

end.