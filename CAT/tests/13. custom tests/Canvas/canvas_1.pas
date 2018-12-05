unit canvas_1;

interface

type
  TCanvasColor = (
    ccNone,
    ccMaroon
  );

procedure DrawCircle(X, Y: Integer; CColor: TCanvasColor); external 'Canvas';

implementation

procedure Test;
begin
  DrawCircle(100, 100, ccMaroon);
end;    

initialization
  Test();

finalization

end.