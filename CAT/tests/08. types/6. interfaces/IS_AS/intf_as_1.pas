unit intf_as_1;

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
  I1: II1; 
  I2: II2;
  
procedure Test;
begin
  T := TT1.Create();
  I1 := T as II1;
  I2 := T as II2;  
end;

initialization
  Test();

finalization

end.