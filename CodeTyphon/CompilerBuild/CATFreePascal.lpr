program CATFreePascal;

{$mode delphi}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, Types, FileUtil
  ,Generics.Collections,
  CompilerUtils, iDStringParser, Operators, iDPascalParser,
  PascalCompiler, CompilerClasses, ILInstructions, CompilerMessages, CompilerErrors,
  ILMachineTypes, ILTypeInfo, ILMachineInvoke, ILMachineTranslator, ILMachine

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



procedure RunTests(const Path: string);
var
  DirName, FileName: string;
  Files: TStringDynArray;
  i: Integer;
begin
  DirName := StringSegment(Path, StringSegCount(Path, PathDelim) - 1, PathDelim);
  WriteLn(DirName);
//  FileUtil.get;
  Files := CompilerUtils.GetDirectoryFiles(Path, '', True);
  for i := 0 to Length(Files) - 1 do
  begin
    FileName := Files[i];
    WriteLn(FileName);
  end;
end;

procedure TMyApplication.DoRun;
var
  ErrorMsg: String;
  RootPath: string;
begin
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

  WriteLn('COMPILER AUTO TEST (CAT) console version');

  RootPath := ExtractFilePath(Params[0]) + 'Tests\';

  WriteLn('TESTS path: ' + RootPath);

  RunTests(RootPath);

  ReadLn;
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

