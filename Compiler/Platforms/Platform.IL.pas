unit Platform.IL;

interface

uses SysUtils, NPCompiler.Classes, IL.Instructions, HWPlatforms;

type
  TILPInstruction = class(TPlatformInstruction)
  private
    FILInstruction: TILInstruction;
    FArgs: TIDExpressions;
  public
    procedure ParseArgumnets(const Context: THWPContext); override;
    property ILInstruction: TILInstruction read FILInstruction;
  end;

implementation

uses iDStringParser, NPCompiler.Parser, NPCompiler, SystemUnit, IL.Types, NPCompiler.DataTypes, NPCompiler.Errors;

type
  TILPlatform = class(TIDPlatform)
  public
    function ParseArguments(const Context: THWPContext; Instruction: TPlatformInstruction): TTokenID; override;
    function ParseArgument(const Context: THWPContext; out Arg: TIDExpression): TTokenID; reintroduce;
  end;

  TILPNope = class(TILPInstruction)
  protected
    class function GetName: string; override;
    class function GetArgsCount: Integer; override;
  public
    procedure CheckArguments(const Context: THWPContext); override;
  end;

  TILPDestInstruction = class(TILPInstruction)
  protected
    class function GetArgsCount: Integer; override;
  end;

  TILPInc = class(TILPDestInstruction)
  protected
    class function GetName: string; override;
  public
    procedure CheckArguments(const Context: THWPContext); override;
  end;

  TILPDec = class(TILPDestInstruction)
  protected
    class function GetName: string; override;
  end;

  TILPMacro = class(TILPInstruction)
  protected
    class function GetName: string; override;
    class function GetArgsCount: Integer; override;
  public
    procedure CheckArguments(const Context: THWPContext); override;
  end;

var
  Platform: TILPlatform = nil;

procedure RegisterILPInstructions;
begin
  Platform.DeclareInstruction(TILPNope);
  Platform.DeclareInstruction(TILPInc);
  Platform.DeclareInstruction(TILPMacro);
end;

{ TILPlatform }

function TILPlatform.ParseArgument(const Context: THWPContext; out Arg: TIDExpression): TTokenID;
var
  EContext: TEContext;
begin
  EContext.Initialize;
  EContext.EPosition := ExprNested;
  Result := Context.Parser.NextToken;
  Result := TNPUnit(Context.AUnit).ParseExpression(Context.Scope, EContext, Result);
  Arg := EContext.ResultExpression; // заменить на RPNPopExpression
  TNPUnit(Context.AUnit).CheckEmptyExpression(Arg);
end;

function TILPlatform.ParseArguments(const Context: THWPContext; Instruction: TPlatformInstruction): TTokenID;
var
  i, c, tc: Integer;
  Args: TIDExpressions;
begin
  c := Instruction.ArgsCount;
  if c = 0 then
    Exit(Context.Parser.NextToken);

  if C = -1 then C := 256; // пока так

  tc := 0;
  SetLength(Args, c);
  for i := 0 to c - 1 do begin
    Result := ParseArgument(Context, Args[i]);
    Inc(tc);
    if Result = token_semicolon then
      Break;
    if i < c - 1 then
      Context.Parser.MatchToken(Result, token_coma);
  end;

  if tc <> c then
    SetLength(Args, tc);

  TILPInstruction(Instruction).FArgs := Args;
end;

{ TILPInstruction }

procedure TILPInstruction.ParseArgumnets(const Context: THWPContext);
begin
  Context.Platform.ParseArguments(Context, Self);
end;

{ TILPNone }

class function TILPNope.GetArgsCount: Integer;
begin
  Result := 0;
end;

class function TILPNope.GetName: string;
begin
  Result := 'NOPE';
end;

{ TILPDestInstruction }

class function TILPDestInstruction.GetArgsCount: Integer;
begin
  Result := 1;
end;

procedure TILPNope.CheckArguments(const Context: THWPContext);
begin
  FILInstruction := TILNope.Create(Line);
end;

{ TILPInc }

procedure TILPInc.CheckArguments(const Context: THWPContext);
begin
  FILInstruction := TIL.IL_Add(FArgs[0], FArgs[0], SYSUnit._OneExpression);
end;

class function TILPInc.GetName: string;
begin
  Result := 'INC';
end;

{ TILPDec }

class function TILPDec.GetName: string;
begin
  Result := 'DEC';
end;

{ TILPMacro }

procedure TILPMacro.CheckArguments(const Context: THWPContext);
var
  MacroName: string;
  MacroID: TILMacroID;
begin
  if not (FArgs[0].Declaration is TIDStringConstant) then
    AbortWork('aaaa', FArgs[0].TextPosition);

  MacroName := FArgs[0].AsStrConst.Value;
  MacroID := GetILMacroID(MacroName);
  if MacroID = IL_MACROID_UNKNOWN then
    AbortWork('Unknown macro: %s', [MacroName], FArgs[0].TextPosition);

  FILInstruction := TIL.IL_Macro(FArgs[1], MacroID, nil);
end;

class function TILPMacro.GetArgsCount: Integer;
begin
  Result := -1;
end;

class function TILPMacro.GetName: string;
begin
  Result := 'MACRO';
end;

initialization
  Platform := TILPlatform.Create('IL');
  HWPlatforms.RegisterPlatform(Platform);
  RegisterILPInstructions;

finalization
  Platform.Free;

end.
