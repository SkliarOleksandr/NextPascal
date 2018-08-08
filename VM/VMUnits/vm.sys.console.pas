unit vm.sys.console;

interface

uses SysUtils, VM.Invoke, VM.Variant; // system

type
  TLogProc = reference to procedure (const Msg: string);

procedure SetLogProc(Proc: TLogProc);

implementation

const UN = 'sys.console';

var
  FLogProc: TLogProc = nil;

procedure SetLogProc(Proc: TLogProc);
begin
  FLogProc := Proc;
end;

procedure _Log(const Msg: PVMVariant);
var
  Str: string;
begin
  if Assigned(FLogProc) then
  begin
    Str := Msg.AsString;
    FLogProc(Str);
  end;
end;

procedure RegisterALL;
begin
  RegisterProc(UN, 'Log', @_Log);
end;

initialization
  RegisterALL();

end.
