program CompilerTest;

{$ifdef FPC}
  {$MODE DELPHI}
{$endif}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp,
  //NativeCallsAutoTests,
  NativeCallsAutoTestsNew
  { you can add units after this };

type

{ TMyApplication }

TMyApplication = class(TCustomApplication)
protected
  procedure DoRun; override;
public
  constructor Create(TheOwner: TComponent); override;
  destructor Destroy; override;
  procedure WriteHelp; virtual;
end;


{ TMyApplication }

procedure TMyApplication.DoRun;
var
  ErrorMsg: String;
begin
  writeln('Native calls auto test');
  // quick check parameters
  ErrorMsg:=CheckOptions('h', 'help');
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h', 'help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  { add your program here }
  try
    //NativeCallsAutoTests.RunTests();
    NativeCallsAutoTestsNew.RunALLTests();
    writeln('*** no errors ***');
  except
    on e: exception do
      writeln('ERROR: ' + e.Message);
  end;

  {$IFNDEF CPUARM}
  ReadLn;
  {$ENDIF}
  // stop program loop
  Terminate;
end;


constructor TMyApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TMyApplication.Destroy;
begin
  inherited Destroy;
end;

procedure TMyApplication.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ', ExeName, ' -h');
end;

var
  Application: TMyApplication;
begin
  Application:=TMyApplication.Create(nil);
  Application.Title:='My Application';
  Application.Run;
  Application.Free;
end.

