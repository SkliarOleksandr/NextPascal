unit VM_SysUtils;

interface

{$IFDEF FPC}
  {$mode delphiunicode}
{$ENDIF}

uses SysUtils, VM.Invoke;

implementation

const
  SUtils = 'SysUtils';

// т.к. под Win это функция обьявлена как stdcall
procedure MySleep(milliseconds: Cardinal);
begin
  SysUtils.Sleep(milliseconds);
end;

function UnicodeFormat(const Fmt: string; const Params: array of const): string;
begin
  Result := SysUtils.Format(fmt, Params);
end;

procedure RegisterUnit;
begin
  with RegisterType(SUtils, 'Exception', pointer(SysUtils.Exception)) do
  begin
  end;
  {$IFDEF FPC}
  RegisterProc(SUtils, 'Format', @UnicodeFormat);
  {$ELSE}
  RegisterProc(SUtils, 'Format', @SysUtils.Format);
  {$ENDIF}
  //RegisterProc(SUtils, 'Format', @SysUtils.Format);
  RegisterProc(SUtils, 'FormatDateTime', @SysUtils.FormatDateTime);
  RegisterProc(SUtils, 'IntToStr', @SysUtils.IntToStr);
  RegisterProc(SUtils, 'StrToInt', @SysUtils.StrToInt);
  RegisterProc(SUtils, 'Sleep', @MySleep);
  RegisterProc(SUtils, 'Now', @SysUtils.Now);
  RegisterProc(SUtils, 'StrToDate', @SysUtils.StrToDate);
  RegisterProc(SUtils, 'TryStrToDate', @SysUtils.TryStrToDate);
  RegisterProc(SUtils, 'StrToTime', @SysUtils.StrToTime);
  RegisterProc(SUtils, 'TryStrToTime', @SysUtils.TryStrToTime);
  RegisterProc(SUtils, 'StrToDateTime', @SysUtils.StrToDateTime);
  RegisterProc(SUtils, 'TryStrToDateTime', @SysUtils.TryStrToDateTime);
end;

initialization
  RegisterUnit;


end.
