program CATConsole;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils, Classes, Types, IOUtils, PascalCompiler, RTTI, CompilerClasses,
  CompilerUtils, ILMachineInvoke, ILMachine, ILMachineTranslator, SystemUnit,
  VM_INTF, CATNativeCallsTests, VM_System, VM_SysUtils, VM_DateUtils, VM_Variants;

var
  TotalCnt: Integer = 0;
  FailedCnt: Integer = 0;
  SkipedCnt: Integer = 0;
  SuccessCnt: Integer = 0;

procedure RUNVM(const FileName: string; const PKG: IPackage);
var
  i: Integer;
  M: TILMachine;
  MT: TILMachineTranslator;
  dt: TDateTime;
  BinPath: string;
  VMStream, ILStream: TMemoryStream;
begin
  VMStream := TILMemoryStream.Create;
  try
    ILStream := TMemoryStream.Create;
    try
      PKG.SaveToStream(ILStream);
      MT := TILMachineTranslator.Create;
      try
        MT.IncludeRTTI := True;
        MT.RTTICharset := RTTICharsetUTF16;
        try
          ILStream.Position := 0;
          MT.LoadILCode(ILStream);
          VMStream.Size := 0;
          MT.SaveVMCode(VMStream);
        except
          on e: exception do begin
            Inc(FailedCnt);
            Writeln('IL TRANSLATE [' + FileName + '] ERROR: ' + e.Message);
            Exit;
          end;
        end;
      finally
        MT.Free;
      end;
    finally
      ILStream.Free;
    end;

    M := TILMachine.Create();
    try
      VMStream.Position := 0;
      try
        M.LoadVMImage(VMStream);
        M.Run;
      except
        on e: exception do
        begin
          Inc(FailedCnt);
          Writeln('VM RUN [' + FileName + '] ERROR: ' + e.Message);
          Exit;
        end;
      end;
    finally
      M.Free;
    end;
  finally
    VMStream.Free;
  end;
end;

procedure RunTest(const FileName: string);
var
  UN: TUnit;
  i: Integer;
  SearchPath, Src: string;
  Package: IPackage;
  F: TStringStream;

begin
  Package := TPackage.Create('test');
  SearchPath := GetApplicationPath + 'Units\';
  Package.AddUnitSearchPath(SearchPath);
  Package.IncludeDebugInfo := True;
  Package.RTTICharset := RTTICharsetUTF16;

  Inc(TotalCnt);

  F := TStringStream.Create;
  try
    F.LoadFromFile(FileName);
    UN := TUnit.Create(Package, F.DataString);
    try
      Package.AddUnit(SystemUnit.SYSUnit, nil);
      Package.AddUnit(UN, nil);

      case UN.Compile() of
        CompileFail: begin
          Inc(FailedCnt);
          Writeln('Compile [' + FileName + ']: FAIL');
          Writeln(UN.Messages.Text);
        end;
        CompileSkip: begin
          Inc(SkipedCnt);
          Writeln('Compile [' + FileName + ']: SKIP');
        end;
      else
        RUNVM(FileName, Package);
        Writeln('Test [' + FileName + ']: SUCCESS');
        Inc(SuccessCnt);
      end;
    finally
      Package.Clear;
    end;
  finally
    F.Free;
  end;
  FreeAndNil(SYSUnit);
end;

procedure LoadTests(const RootPath: string);
var
  Files: TStringDynArray;
  i: Integer;
  s: string;
begin
  Files := TDirectory.GetFiles(RootPath, '*.pas', TSearchOption.soAllDirectories);
  for i := 0 to Length(Files) - 1 do
  begin
    s := Files[i];
    if not DirectoryExists(s) then
      RunTest(s)
  end;
end;

var
  TMP: string;
begin
  try
    Writeln('COMPILER AUTO TEST ' + {$IFDEF CPUX64}' X64'{$ELSE}'X32'{$ENDIF});
    LoadTests(GetApplicationPath + 'tests');
    Writeln('');
    Writeln(format('TOTAL: %d SKIPED: %d FAILED: %d SUCCESS: %d' , [TotalCnt, SkipedCnt, FailedCnt, SuccessCnt]));
    Writeln('');
    Writeln('press any key for exit...');
    Readln(TMP);
    { TODO -oUser -cConsole Main : Insert code here }
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
