unit intf_0;

interface

uses System;

type
  II1 = interface
  end;
  
  II2 = interface
  end;  

  TT1 = class(II1)
  end;

  TT2 = class(TObject, II1)
  end;

  TT3 = class(II1, II2)
  end;
  
  TT4 = class(TObject, II1, II2)
  end;
  
implementation

procedure Test;
begin

end;

initialization
  Test();

finalization

end.