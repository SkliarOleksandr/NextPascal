unit VM_Canvas;

interface

uses SysUtils, VM.Invoke, Graphics, Forms, Classes, Types;

var _Canvas: TCanvas = nil;

implementation

function _GetCanvas: TCanvas;
begin
  Result := _Canvas;
end;

type
  TCanvasColor = (
    ccNone,
    ccBlack,
    ccMaroon
  );

function GetColor(CColor: TCanvasColor): TColor;
begin
  case CColor of
    ccNone: Result := clBtnFace;
    ccBlack: Result := clBlack;
    ccMaroon: Result := clMaroon;
  else
    Result := clNone;
  end;
end;

procedure _ClearCanvas;
begin
  _Canvas.Brush.Color := clWindow;
  _Canvas.FillRect(TRect.Create(0, 0, 1200, 1200));
end;

procedure _ProcessMessages;
begin
  Application.ProcessMessages;
end;

procedure _DrawCircle(X, Y: Integer; CColor: TCanvasColor);
var
  c: TColor;
begin
  c := GetColor(CColor);
  _Canvas.Pen.Color := c;
  _Canvas.Brush.Color := c;
  _Canvas.Ellipse(X, Y, X+15, Y+15);
end;

procedure RegisterVM_Canvas;
begin
  //RegisterType('Canvas', 'TCanvas', nil);
  RegisterProc('Canvas', 'GetCanvas', @_GetCanvas);
  RegisterProc('Canvas', 'DrawCircle', @_DrawCircle);
  RegisterProc('Canvas', 'ProcessMessages', @_ProcessMessages);
  RegisterProc('Canvas', 'ClearCanvas', @_ClearCanvas);
end;

initialization
  RegisterVM_Canvas;

end.
