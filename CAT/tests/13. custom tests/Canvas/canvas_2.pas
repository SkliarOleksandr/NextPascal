unit canvas_2;

interface

uses VM_System;

type
  TCanvasColor = (
    ccNone,
    ccBlack,
    ccMaroon
  );

procedure DrawCircle(X, Y: Integer; CColor: TCanvasColor); external 'Canvas';
procedure ProcessMessages; external 'Canvas';

implementation

procedure Test;
begin
  for var i := 0 to 10 do begin
    DrawCircle(100, 100, ccNone);    
    ProcessMessages();    
    Sleep(5);
    DrawCircle(100, 100, ccMaroon);
    ProcessMessages();
    Sleep(5);              
  end;
end;                   

initialization
  Test();

finalization

end.