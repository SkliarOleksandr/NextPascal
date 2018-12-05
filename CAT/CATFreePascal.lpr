program CATFreePascal;

{$mode delphiunicode}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, Types, FileUtil,
  CompilerUtils, iDStringParser, Operators, iDPascalParser,
  PascalCompiler, CompilerClasses, ILInstructions, CompilerMessages, CompilerErrors,
  ILMachineTypes, ILTypeInfo, ILMachineInvoke, ILMachineTranslator, ILMachine, SystemUnit,
  VM_INTF, VM_CRI, CATNativeCallsTests, VM_System, VM_SysUtils, VM_DateUtils

  { you can add units after this };

type

  { TMyApplication }

  TMyApplication = class(TCustomApplication)
  private
    FSuccessCnt, FFailCnt, FSkipCnt: Integer;
    FRTTICharset: TRTTICharset;
    function CompileSource(const Src: string; ILStream: TMemoryStream; out ErrorString: string): TCompilerResult;
    function VMRun(ILStream: TMemoryStream; out Messages: string; ShowVMCode: Boolean): Boolean;
    procedure RunALLTests(const Path: string);
    procedure RunTest(const RootDir, FileName: string; ShowVMCode: Boolean);
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TMyApplication }

function TMyApplication.CompileSource(const Src: string; ILStream: TMemoryStream; out ErrorString: string): TCompilerResult;
var
  UN: TUnit;
  Package: IPackage;
  SearchPath: string;
begin
  Package := TPackage.Create('test');
  try
    SearchPath := ExtractFilePath(Params[0]) + 'units' + PathDelim;
    Package.AddUnitSearchPath(SearchPath);
    Package.IncludeDebugInfo := True;
    Package.RTTICharset := FRTTICharset;
    UN := TUnit.Create(Package, Src);
    Package.AddUnit(SystemUnit.SYSUnit, nil);
    Package.AddUnit(UN, nil);
    Result := UN.Compile;
    Package.SaveToStream(ILStream);
    ErrorString := Package.Messages.GetAsString;
  finally
    Package.Clear;
    FreeAndNil(SYSUnit);
  end;
end;

procedure ShowVMOut(VM: TILMachine);
var
  i, j: Integer;
  pUnit: PExportUnit;
  pVar: PExportVariable;
begin
  for i := 0 to VM.UnitsCount - 1 do
  begin
    pUnit := @VM.Units[i];
    if pUnit.VarsCount > -0 then
    begin
      WriteLn('------------------');
      WriteLn('UNIT: ' + VM.GetUnitName(pUnit));
      WriteLn('------------------');
      WriteLn('GLOBAL VARS:');
      WriteLn('');
      for j := 0 to pUnit.VarsCount - 1 do
      begin
        pVar := @pUnit.Variables[j];
        WriteLn(format('%d: %s = %s', [j, VM.GetVarName(pVar), VM.ReadVarValue(pVar)]));
      end;
    end;
  end;
end;

function TMyApplication.VMRun(ILStream: TMemoryStream; out Messages: string; ShowVMCode: Boolean): Boolean;
var
  VMStream: TMemoryStream;
  VMT: TILMachineTranslator;
  VM: TILMachine;
  //Str: TStringStream;
  //FStr: TFileStream;
  Level: Integer;
begin
  Level := 0;
  try
    Result := False;
    VMStream := TMemoryStream.Create;
    VMT := TILMachineTranslator.Create;
    VMT.RTTICharset := FRTTICharset;
    VMT.IncludeRTTI := True;
    VM := TILMachine.Create();
    try
      VMT.LoadFromStream(ILStream);
      Level := 1;
      {if ShowVMCode then
      begin
        Str := TStringStream.Create(VMT.DebugOut);
        FStr := TFileStream.Create(ExtractFilePath(Params[0]) + 'VMCode.txt', fmCreate);
        Str.Position := 0;
        FStr.CopyFrom(Str, Str.size);
        FStr.Free;
        Str.Free;
      end;}
      VMT.SavePreprared(VMStream);
      Level := 2;
      //VMStream.SaveToFile(ExtractFilePath(Params[0]) + 'VMCode.bin');
      VMStream.Position := 0;
      VM.LoadPreprared(VMStream);
      Level := 3;
      try
        VM.Run();
        Level := 4;
      except
        ShowVMOut(VM);
        raise;
      end;
      Result := True;
    finally
      if ShowVMCode then
        ShowVMOut(VM);
      FreeAndNil(VM);
      FreeAndNil(VMT);
      FreeAndNil(VMStream);
    end;
  except
    on e: exception do
      Messages := 'Level: ' + IntToStr(Level) + ' ' + e.Message;
  end;
end;

procedure TMyApplication.RunTest(const RootDir, FileName: string; ShowVMCode: Boolean);
var
  Str: TFileStream;
  Source: UnicodeString;
  Messages: String;
  ILStream: TMemoryStream;
  CR: TCompilerResult;
  TestName: string;
begin
  TestName := PathDelim + ExtractRelativepath(RootDir, FileName);
  Str := TFileStream.Create(FileName, fmOpenRead);
  try
    Str.Position := 0;
    Source := Str.AsUTF8String();
    ILStream := TMemoryStream.Create;
    try
      CR := CompileSource(Source, ILStream, Messages);

      case CR of
        CompileFail: begin
          Inc(FFailCnt);
          WriteLn(TestName + '...FAIL');
          WriteLn('Messages: ' + Messages);
          Exit;
        end;
        CompileSkip: begin
          Inc(FSkipCnt);
          WriteLn(TestName + '...SKIP');
          Exit;
        end;
      else
        WriteLn(TestName + '...COMPILE SUCCESS');
        ILStream.Position := 0;
        if VMRun(ILStream, Messages, ShowVMCode) then
        begin
          Inc(FSuccessCnt);
          WriteLn(TestName + '...SUCCESS');
        end else begin
          Inc(FFailCnt);
          WriteLn(TestName + '...FAIL');
          WriteLn('Messages: ' + Messages);
        end;
      end;
    finally
      ILStream.free;
    end;
  finally
    Str.Free;
  end;
end;


procedure TMyApplication.RunALLTests(const Path: string);
var
  FileName: string;
  Files: TStrArray;
  i: Integer;
begin
  Files := CompilerUtils.GetDirectoryFiles(Path, '*.pas', True);
  for i := 0 to Length(Files) - 1 do
  begin
    FileName := Files[i];
    //if Pos('1 - import FORMAT.pas', FileName) > 0 then

//   if (Pos('tariff_script.pas', FileName) > 0)
  //    (Pos('cpp_test1.pas', FileName) > 0) or
//      (Pos('cpp_test2.pas', FileName) > 0) or
//    if Pos('datetime_test_1.pas', FileName) > 0 then
      RunTest(Path, FileName, False);
//    if  i > 10 then
//      Exit;
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

  RootPath := ExtractFilePath(Params[0]) + 'tests' + PathDelim;

  FRTTICharset := RTTICharsetUTF16;

  WriteLn('TESTS path: ' + RootPath);
  WriteLn('-----------------------------------------------------------------------');
  RunALLTests(RootPath);
  WriteLn('-----------------------------------------------------------------------');
  WriteLn('TOTAL Sucess: ', FSuccessCnt, ' Fail: ', FFailCnt, ' Skip: ', FSkipCnt);
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

