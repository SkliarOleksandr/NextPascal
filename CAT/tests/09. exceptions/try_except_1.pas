unit try_except_1;

interface

implementation

uses System;

var G: Int32;

procedure Test;
begin
  try
    //raise Exception.Create('test');  
  except
    G := 111;  
  end;  
end;

initialization
  Test();

finalization

end.