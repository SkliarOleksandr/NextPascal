unit Platform.X86;

interface


uses SysUtils, Types, Classes, CompilerClasses, CompilerUtils, CompilerMessages, HWPlatforms, Platform.X86Instructions,
     iDPascalParser, PascalCompiler, iDStringParser;  // system

function ParseArgument(const Context: THWPContext; var Arg: TX86Argument): TTokenID; forward;

type

  TX86Platform = class(TIDPlatform)
  public
    constructor Create(const Name: string); override;
  end;


  ///////////////////////////////////////////////////////
  ///  РЕГИСТРЫ X86
  ///////////////////////////////////////////////////////

  TX86_8BitRegister = class(TPlatformRegister)
  public
    class function GetSize: Integer; override;
  end;

  TX86_16BitRegister = class(TPlatformRegister)
  public
    class function GetSize: Integer; override;
  end;

  TX86_32BitRegister = class(TPlatformRegister)
  public
    class function GetSize: Integer; override;
  end;

  TX86_EAX = class(TX86_32BitRegister)
  public
    class function GetName: string; override;
    class function GetCode: Integer; override;
  end;

  TX86_EBX = class(TX86_32BitRegister)
  public
    class function GetName: string; override;
    class function GetCode: Integer; override;
  end;

  TX86_ECX = class(TX86_32BitRegister)
  public
    class function GetName: string; override;
    class function GetCode: Integer; override;
  end;

  TX86_EDX = class(TX86_32BitRegister)
  public
    class function GetName: string; override;
    class function GetCode: Integer; override;
  end;

  TX86_EBP = class(TX86_32BitRegister)
  public
    class function GetName: string; override;
    class function GetCode: Integer; override;
  end;

  TX86_ESP = class(TX86_32BitRegister)
  public
    class function GetName: string; override;
    class function GetCode: Integer; override;
  end;

  TX86_ESI = class(TX86_32BitRegister)
  public
    class function GetName: string; override;
    class function GetCode: Integer; override;
  end;

  TX86_EDI = class(TX86_32BitRegister)
  public
    class function GetName: string; override;
    class function GetCode: Integer; override;
  end;

  //================================================
  // управляющие регистры
  //================================================

  TX86_CR0 = class(TX86_32BitRegister)
  public
    class function GetName: string; override;
    class function GetCode: Integer; override;
  end;

  TX86_CR1 = class(TX86_32BitRegister)
  public
    class function GetName: string; override;
    class function GetCode: Integer; override;
  end;

  TX86_CR2 = class(TX86_32BitRegister)
  public
    class function GetName: string; override;
    class function GetCode: Integer; override;
  end;

  TX86_CR3 = class(TX86_32BitRegister)
  public
    class function GetName: string; override;
    class function GetCode: Integer; override;
  end;

  TX86_CR4 = class(TX86_32BitRegister)
  public
    class function GetName: string; override;
    class function GetCode: Integer; override;
  end;

  //================================================
  // регистры отладки
  //================================================



  //////////////////////////////////////////////////////////////////////
  ///  ИНСТРУКЦИИ X86
  //////////////////////////////////////////////////////////////////////

  TX86_0ARGInstruction = class(TPlatformInstruction)
  end;

  TX86_Prefix = class(TX86_0ARGInstruction)
  protected
    class function GetType: TInstructionType; override;
  end;

  TX86_1ARGInstruction = class(TPlatformInstruction)
  private
    FArgument: TX86Argument;
  protected
    class function GetArgsCount: Integer; override;
  public
    function Text: string; override;
    procedure Write(Stream: TStream); override;
    procedure ParseArgumnets(const Context: THWPContext); override;
  end;

  TX86_2ARGInstruction = class(TPlatformInstruction)
  private
    FLeft: TX86Argument;
    FRight: TX86Argument;
  protected
    class function GetArgsCount: Integer; override;
  public
    procedure Write(Stream: TStream); override;
    procedure ParseArgumnets(const Context: THWPContext); override;
    function Text: string; override;
  end;

  TX86_3ARGInstruction = class(TPlatformInstruction)
  private
    FDArg: TX86Argument;
    FLArg: TX86Argument;
    FRArg: TX86Argument;
  protected
    class function GetArgsCount: Integer; override;
  public
    procedure Write(Stream: TStream); override;
  end;

  TX86_LOCK = class(TX86_Prefix)
  protected
    class function GetName: string; override;
    class function GetCode: Integer; override;
  end;

  TX86_REPE = class(TX86_Prefix)
  protected
    class function GetName: string; override;
    class function GetCode: Integer; override;
  end;

  TX86_REPNE = class(TX86_Prefix)
  protected
    class function GetName: string; override;
    class function GetCode: Integer; override;
  end;

  TX86_NOP = class(TX86_0ARGInstruction)
  protected
    class function GetName: string; override;
    class function GetCode: Integer; override;
  public
    procedure Encode(Stream: TStream); override;
  end;

  TX86_CLI = class(TX86_0ARGInstruction)
  protected
    class function GetName: string; override;
    class function GetCode: Integer; override;
  end;

  TX86_STI = class(TX86_0ARGInstruction)
  protected
    class function GetName: string; override;
    class function GetCode: Integer; override;
  end;

  TX86_MOV = class(TX86_2ARGInstruction)
  protected
    class function GetName: string; override;
    class function GetCode: Integer; override;
    class function GetPrefixes: TPrefixes; override;
  public
    procedure Encode(Stream: TStream); override;
  end;

  TX86_LEA = class(TX86_2ARGInstruction)
  protected
    class function GetName: string; override;
    class function GetCode: Integer; override;
  public
    procedure Encode(Stream: TStream); override;
  end;

  TX86_PUSH = class(TX86_1ARGInstruction)
  protected
    class function GetName: string; override;
    class function GetCode: Integer; override;
  public
    procedure Encode(Stream: TStream); override;
  end;

  TX86_POP = class(TX86_1ARGInstruction)
  protected
    class function GetName: string; override;
    class function GetCode: Integer; override;
  public
    procedure Encode(Stream: TStream); override;
  end;

  TX86_ADD = class(TX86_2ARGInstruction)
  protected
    class function GetName: string; override;
    class function GetCode: Integer; override;
  public
    procedure Encode(Stream: TStream); override;
  end;

  TX86_ADC = class(TX86_2ARGInstruction)
  protected
    class function GetName: string; override;
    class function GetCode: Integer; override;
  public
    procedure Encode(Stream: TStream); override;
  end;

  TX86_CMP = class(TX86_2ARGInstruction)
  protected
    class function GetName: string; override;
    class function GetCode: Integer; override;
  public
    procedure Encode(Stream: TStream); override;
  end;

  TX86_SUB = class(TX86_2ARGInstruction)
  protected
    class function GetName: string; override;
    class function GetCode: Integer; override;
  public
    procedure Encode(Stream: TStream); override;
  end;

  TX86_SBB = class(TX86_2ARGInstruction)
  protected
    class function GetName: string; override;
    class function GetCode: Integer; override;
  public
    procedure Encode(Stream: TStream); override;
  end;

  TX86_JMP = class(TX86_1ARGInstruction)
  protected
    class function GetName: string; override;
    class function GetCode: Integer; override;
  public
    procedure Encode(Stream: TStream); override;
  end;

  TX86_CALL = class(TX86_1ARGInstruction)
  protected
    class function GetName: string; override;
    class function GetCode: Integer; override;
  public
    procedure Encode(Stream: TStream); override;
  end;

  TX86_RET = class(TX86_1ARGInstruction)
  protected
    class function GetName: string; override;
    class function GetCode: Integer; override;
  public
    procedure Encode(Stream: TStream); override;
  end;


implementation


uses CompilerErrors;

{ TX86Platform }

constructor TX86Platform.Create(const Name: string);
begin
  inherited;
  // регистры общего назначения
  DeclareRegister(TX86_EAX);
  DeclareRegister(TX86_EBX);
  DeclareRegister(TX86_ECX);
  DeclareRegister(TX86_EDX);
  DeclareRegister(TX86_EBP);
  DeclareRegister(TX86_ESP);
  DeclareRegister(TX86_ESI);
  DeclareRegister(TX86_EDI);

  DeclareRegister(TX86_CR0);
  DeclareRegister(TX86_CR1);
  DeclareRegister(TX86_CR2);
  DeclareRegister(TX86_CR3);
  DeclareRegister(TX86_CR4);

  // префиксы
  DeclareInstruction(TX86_LOCK);
  DeclareInstruction(TX86_REPE);
  DeclareInstruction(TX86_REPNE);

  // инструкции общего назначения
  DeclareInstruction(TX86_NOP);
  DeclareInstruction(TX86_MOV);
  DeclareInstruction(TX86_LEA);
  DeclareInstruction(TX86_PUSH);
  DeclareInstruction(TX86_POP);

  // арифметика
  DeclareInstruction(TX86_ADD);
  DeclareInstruction(TX86_ADC);
  DeclareInstruction(TX86_SUB);
  DeclareInstruction(TX86_SBB);

  // передача управления
  DeclareInstruction(TX86_CMP);
  DeclareInstruction(TX86_JMP);

  // системные
  DeclareInstruction(TX86_CLI);
  DeclareInstruction(TX86_STI);
end;

{ TX86_CLI }

class function TX86_CLI.GetCode: Integer;
begin
  Result := Integer(X86_CLI);
end;

class function TX86_CLI.GetName: string;
begin
  Result := 'CLI';
end;

{ TX86_STI }

class function TX86_STI.GetCode: Integer;
begin
  Result := Integer(X86_STI);
end;

class function TX86_STI.GetName: string;
begin
  Result := 'STI';
end;

{ TX86_NOP }

procedure TX86_NOP.Encode(Stream: TStream);
begin
  Stream.WriteUInt8($90);
end;

class function TX86_NOP.GetCode: Integer;
begin
  Result := Integer(TX86_NOP)
end;

class function TX86_NOP.GetName: string;
begin
  Result := 'NOP';
end;

{ TX86_2ARGInstruction }

class function TX86_2ARGInstruction.GetArgsCount: Integer;
begin
  Result := 2;
end;

procedure TX86_2ARGInstruction.ParseArgumnets(const Context: THWPContext);
var
  Token: TTokenID;
begin
  Token := ParseArgument(Context, FLeft);
  TUnit(Context.AUnit).parser_MatchToken(Token, token_coma);
  Token := ParseArgument(Context, FRight);
  TUnit(Context.AUnit).parser_MatchToken(Token, token_semicolon);
end;

function TX86_2ARGInstruction.Text: string;
begin
  Result := Format('%s %s, %s;', [Name, ArgumentToText(FLeft), ArgumentToText(FRight)]);
end;

procedure TX86_2ARGInstruction.Write(Stream: TStream);
begin
  inherited;
end;

{ TX86_MOV }

procedure TX86_MOV.Encode(Stream: TStream);
begin
  X86MOV(Stream, tmUSE32, FLeft, FRight);
end;

class function TX86_MOV.GetCode: Integer;
begin
  Result := Integer(X86_MOV)
end;

class function TX86_MOV.GetName: string;
begin
  Result := 'MOV';
end;

class function TX86_MOV.GetPrefixes: TPrefixes;
begin
  Result := TPrefixes.Create(Integer(X86_PREFIX_LOCK));
end;

{ TX86_1ARGInstruction }

class function TX86_1ARGInstruction.GetArgsCount: Integer;
begin
  Result := 1;
end;

procedure TX86_1ARGInstruction.ParseArgumnets(const Context: THWPContext);
var
  Token: TTokenID;
begin
  Token := ParseArgument(Context, FArgument);
  TUnit(Context.AUnit).parser_MatchToken(Token, token_semicolon);
end;

function TX86_1ARGInstruction.Text: string;
begin
  Result := Format('%s %s;', [Name, ArgumentToText(FArgument)])
end;

procedure TX86_1ARGInstruction.Write(Stream: TStream);
begin
  inherited;
end;

{ TX86_ECX }

class function TX86_ECX.GetCode: Integer;
begin
  Result := Integer(X86_ECX);
end;

class function TX86_ECX.GetName: string;
begin
  Result := 'ECX';
end;

{ TX86_EBX }

class function TX86_EBX.GetCode: Integer;
begin
  Result := Integer(X86_EBX);
end;

class function TX86_EBX.GetName: string;
begin
  Result := 'EBX';
end;

{ TX86_EAX }

class function TX86_EAX.GetCode: Integer;
begin
  Result := Integer(X86_EAX);
end;

class function TX86_EAX.GetName: string;
begin
  Result := 'EAX';
end;

{ TX86_EDX }

class function TX86_EDX.GetCode: Integer;
begin
  Result := Integer(X86_EDX);
end;

class function TX86_EDX.GetName: string;
begin
  Result := 'EDX';
end;

{ TX86_8BitRegister }

class function TX86_8BitRegister.GetSize: Integer;
begin
  Result := 8;
end;

{ TX86_16BitRegister }

class function TX86_16BitRegister.GetSize: Integer;
begin
  Result := 16;
end;

{ TX86_32BitRegister }

class function TX86_32BitRegister.GetSize: Integer;
begin
  Result := 32;
end;

{ TX86_LOCK }

class function TX86_LOCK.GetCode: Integer;
begin
  Result := Integer(X86_PREFIX_LOCK)
end;

class function TX86_LOCK.GetName: string;
begin
  Result := 'LOCK';
end;

{ TX86_REPNE }

class function TX86_REPNE.GetCode: Integer;
begin
  Result := Integer(X86_PREFIX_REPNE);
end;

class function TX86_REPNE.GetName: string;
begin
  Result := 'REPNE';
end;

{ TX86_REPE }

class function TX86_REPE.GetCode: Integer;
begin
  Result := Integer(X86_PREFIX_REPE);
end;

class function TX86_REPE.GetName: string;
begin
  Result := 'REPE';
end;

{ TX86_Prefix }

class function TX86_Prefix.GetType: TInstructionType;
begin
  Result := itInstructionPrefix;
end;

{ TX86_3ARGInstruction }

class function TX86_3ARGInstruction.GetArgsCount: Integer;
begin
  Result := 3;
end;

procedure TX86_3ARGInstruction.Write(Stream: TStream);
begin
  inherited;
end;

function ParseMemArgument(const Context: THWPContext; var Arg: TX86Argument): TTokenID;
var
  ID: TIdentifier;
  Parser: TPascalParser;
  Reg: TPlatformRegisterClass;
  Sign: Integer;
begin
  Sign := 1;
  Parser := Context.Parser;
  Arg.ArgumentType := ArgNone;
  while True do
  begin
    Result := Parser.NextToken;
    case Result of
      token_identifier: begin
        Parser.ReadCurrIdentifier(ID);
        // проверяем что это регистр
        Reg := Context.Platform.FindRegister(ID.Name);
        if Assigned(Reg) then
        begin
          case Arg.ArgumentType of
            ArgNone: begin
              Arg.ArgumentType := ArgMemRegister;
              Arg.Reg := Reg;
            end;
            ArgMemRegister: begin
              Arg.ArgumentType := ArgMemRegReg;
              Arg.RegIdx := Reg;
            end
          else
            AbortWork(msgInvalidArgument, Parser.Position);
          end;
          Continue;
        end;

        // временно!
        if Parser.IdentifireType = itInteger then
        begin
          case Arg.ArgumentType of
            ArgNone: Arg.ArgumentType := ArgMemImm;
            ArgMemRegister: Arg.ArgumentType := ArgMemRegDisp;
            ArgMemRegReg: Arg.ArgumentType := ArgMemRegRegDisp;
          else
            AbortWork(msgInvalidArgument, Parser.Position);
          end;
          Arg.IMMDisp := StrToInt(Parser.OriginalToken)*Sign;
          continue;
        end;

        AbortWork(msgInvalidArgument, ID.TextPosition);
      end;
      token_plus: begin
        Sign := 1;
      end;
      token_minus: begin
        Sign := -1;
      end;
      token_asterisk: begin
        if Arg.ArgumentType <> ArgMemRegReg then
          AbortWork(msgInvalidArgument, ID.TextPosition);
      end;
      token_closeblock: Break;
    end;
  end;
  //Parser.MatchToken(Result, token_closeblock);
  Result := Parser.NextToken;
end;

function ParseArgument(const Context: THWPContext; var Arg: TX86Argument): TTokenID;
var
  Reg: TPlatformRegisterClass;
  ID: TIdentifier;
  Decl: TIDDeclaration;
  Expr: TIDExpression;
  AUnit: TUnit;
  EContext: TEContext;
  SContext: TSContext;
  Parser: TPascalParser;
  Sign: Integer;
  LB: PHWLabel;
begin
  Sign := 1;
  AUnit := TUnit(Context.AUnit);
  Parser := Context.Parser;
  Result := Parser.NextToken;
  case Result of
    token_openblock: begin
      Result := ParseMemArgument(Context, Arg);
      Exit;
    end;
    token_minus, token_identifier: begin
      AUnit.parser_ReadCurrIdentifier(ID);
      // проверяем что это регистр
      Reg := Context.Platform.FindRegister(ID.Name);
      if Assigned(Reg) then
      begin
        Arg.ArgumentType := ArgRegister;
        Arg.Reg := Reg;
        Result := AUnit.parser_NextToken(nil);
        Exit;
      end;

      // проверяем что это метка
      Lb := Context.FindLabel(ID.Name, nil);
      if Assigned(lb) then
      begin
        Arg.ArgumentType := ArgLabel;
        Arg.LB := LB;
        Result := AUnit.parser_NextToken(nil);
        Exit;
      end;

      // иначе это выражение
      SContext.Initialize;
      EContext.Initialize;
      Result := AUnit.ParseExpression(Context.Scope, EContext, Result);
      Expr := EContext.ResultExpression;
      AUnit.CheckEmptyExpression(Expr);
      Decl := Expr.Declaration;
      case Decl.ItemType of
        itVar: Arg.ArgumentType := X86ARG_VAR;
        itConst: begin
          Arg.ArgumentType := ArgImmValue;
          Arg.IMMValue := Cardinal(TIDConstant(Decl).AsInt64);
        end;
      end;
      Arg.Decl := Decl;
      Exit;
    end;
    else
      AbortWork(msgInvalidArgument, Parser.Position);
  end;
end;

{ TX86_JMP }

procedure TX86_JMP.Encode(Stream: TStream);
begin
  X86JMP(Stream, tmUSE32, FArgument);
end;

class function TX86_JMP.GetCode: Integer;
begin
  Result := Integer(TX86_JMP);
end;

class function TX86_JMP.GetName: string;
begin
  Result := 'JMP';
end;


{ TX86_LEA }

procedure TX86_LEA.Encode(Stream: TStream);
begin
  X86LEA(Stream, tmUSE32, FLeft, FRight);
end;

class function TX86_LEA.GetCode: Integer;
begin
  Result := Integer(X86_LEA);
end;

class function TX86_LEA.GetName: string;
begin
  Result := 'LEA';
end;

var
  Platform: TX86Platform = nil;

{ TX86_ADC }

procedure TX86_ADC.Encode(Stream: TStream);
begin
  X86ADC(Stream, tmUSE32, FLeft, FRight);
end;

class function TX86_ADC.GetCode: Integer;
begin
  Result := Integer(X86_ADC);
end;

class function TX86_ADC.GetName: string;
begin
  Result := 'ADC';
end;

{ TX86_ADD }

procedure TX86_ADD.Encode(Stream: TStream);
begin
  X86ADD(Stream, tmUSE32, FLeft, FRight);
end;

class function TX86_ADD.GetCode: Integer;
begin
  Result := Integer(X86_LEA);
end;

class function TX86_ADD.GetName: string;
begin
  Result := 'ADD';
end;

{ TX86_CMP }

procedure TX86_CMP.Encode(Stream: TStream);
begin
  X86CMP(Stream, tmUSE32, FLeft, FRight);
end;

class function TX86_CMP.GetCode: Integer;
begin
  Result := Integer(X86_CMP);
end;

class function TX86_CMP.GetName: string;
begin
  Result := 'CMP';
end;

{ TX86_SUB }

procedure TX86_SUB.Encode(Stream: TStream);
begin
  X86SUB(Stream, tmUSE32, FLeft, FRight);
end;

class function TX86_SUB.GetCode: Integer;
begin
  Result := Integer(X86_SUB);
end;

class function TX86_SUB.GetName: string;
begin
  Result := 'SUB';
end;

{ TX86_SBB }

procedure TX86_SBB.Encode(Stream: TStream);
begin
  X86SBB(Stream, tmUSE32, FLeft, FRight);
end;

class function TX86_SBB.GetCode: Integer;
begin
  Result := Integer(X86_SBB);
end;

class function TX86_SBB.GetName: string;
begin
  Result := 'SBB';
end;

{ TX86_CR0 }

class function TX86_CR0.GetCode: Integer;
begin
  Result := Integer(X86_CR0);
end;

class function TX86_CR0.GetName: string;
begin
  Result := 'CR0';
end;

{ TX86_CR1 }

class function TX86_CR1.GetCode: Integer;
begin
  Result := Integer(X86_CR1);
end;

class function TX86_CR1.GetName: string;
begin
  Result := 'CR1';
end;

{ TX86_CR2 }

class function TX86_CR2.GetCode: Integer;
begin
 Result := Integer(X86_CR2);
end;

class function TX86_CR2.GetName: string;
begin
  Result := 'CR2';
end;

{ TX86_CR3 }

class function TX86_CR3.GetCode: Integer;
begin
  Result := Integer(X86_CR3);
end;

class function TX86_CR3.GetName: string;
begin
  Result := 'CR3';
end;

{ TX86_CR4 }

class function TX86_CR4.GetCode: Integer;
begin
 Result := Integer(X86_CR4);
end;

class function TX86_CR4.GetName: string;
begin
  Result := 'CR4';
end;

{ TX86_ESI }

class function TX86_ESI.GetCode: Integer;
begin
  Result := Integer(X86_ESI);
end;

class function TX86_ESI.GetName: string;
begin
  Result := 'ESI';
end;

{ TX86_EDI }

class function TX86_EDI.GetCode: Integer;
begin
  Result := Integer(X86_EDI);
end;

class function TX86_EDI.GetName: string;
begin
  Result := 'EDI';
end;

{ TX86_ESP }

class function TX86_ESP.GetCode: Integer;
begin
  Result := Integer(X86_ESP);
end;

class function TX86_ESP.GetName: string;
begin
  Result := 'ESP';
end;

{ TX86_EBP }

class function TX86_EBP.GetCode: Integer;
begin
  Result := Integer(X86_EBP);
end;

class function TX86_EBP.GetName: string;
begin
  Result := 'EBP';
end;

{ TX86_PUSH }

procedure TX86_PUSH.Encode(Stream: TStream);
begin
  X86PUSH(Stream, tmUSE32, FArgument);
end;

class function TX86_PUSH.GetCode: Integer;
begin
  Result := Integer(X86_PUSH);
end;

class function TX86_PUSH.GetName: string;
begin
  Result := 'PUSH';
end;

{ TX86_POP }

procedure TX86_POP.Encode(Stream: TStream);
begin
  X86POP(Stream, tmUSE32, FArgument);
end;

class function TX86_POP.GetCode: Integer;
begin
  Result := Integer(X86_POP);
end;

class function TX86_POP.GetName: string;
begin
  Result := 'POP';
end;

{ TX86_CALL }

procedure TX86_CALL.Encode(Stream: TStream);
begin
  X86CALL(Stream, tmUSE32, FArgument);
end;

class function TX86_CALL.GetCode: Integer;
begin
  Result := Integer(X86_CALL);
end;

class function TX86_CALL.GetName: string;
begin
  Result := 'CALL';
end;

{ TX86_RET }

procedure TX86_RET.Encode(Stream: TStream);
begin
  X86RETN(Stream, tmUSE32, FArgument);
end;

class function TX86_RET.GetCode: Integer;
begin
   Result := Integer(X86_RET);
end;

class function TX86_RET.GetName: string;
begin
  Result := 'RET';
end;

initialization
  Platform := TX86Platform.Create('X86');
  HWPlatforms.RegisterPlatform(Platform);

finalization
  Platform.Free;

end.
