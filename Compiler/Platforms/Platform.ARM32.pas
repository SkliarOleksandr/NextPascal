unit Platform.ARM32;

interface

uses SysUtils, Classes, CompilerUtils, CompilerClasses, HWPlatforms, Platform.ARM32Instructions, iDStringParser, iDPascalParser;

type
  TARM32Platform = class(TIDPlatform)
  public
    constructor Create(const Name: string); override;
  end;

  ///////////////////////////////////////////////////////
  /// РЕГИСТРЫ
  ///////////////////////////////////////////////////////

  TARM32_32BitRegister = class(TPlatformRegister)
  public
    class function GetSize: Integer; override;
  end;

  TARM32_R0 = class(TARM32_32BitRegister)
  public
    class function GetName: string; override;
    class function GetCode: Integer; override;
  end;

  TARM32_R1 = class(TARM32_32BitRegister)
  public
    class function GetName: string; override;
    class function GetCode: Integer; override;
  end;

  TARM32_R2 = class(TARM32_32BitRegister)
  public
    class function GetName: string; override;
    class function GetCode: Integer; override;
  end;

  TARM32_R3 = class(TARM32_32BitRegister)
  public
    class function GetName: string; override;
    class function GetCode: Integer; override;
  end;

  TARM32_R4 = class(TARM32_32BitRegister)
  public
    class function GetName: string; override;
    class function GetCode: Integer; override;
  end;

  TARM32_R5 = class(TARM32_32BitRegister)
  public
    class function GetName: string; override;
    class function GetCode: Integer; override;
  end;

  TARM32_R6 = class(TARM32_32BitRegister)
  public
    class function GetName: string; override;
    class function GetCode: Integer; override;
  end;

  TARM32_R7 = class(TARM32_32BitRegister)
  public
    class function GetName: string; override;
    class function GetCode: Integer; override;
  end;

  TARM32_R8 = class(TARM32_32BitRegister)
  public
    class function GetName: string; override;
    class function GetCode: Integer; override;
  end;

  TARM32_R9 = class(TARM32_32BitRegister)
  public
    class function GetName: string; override;
    class function GetCode: Integer; override;
  end;

  TARM32_R10 = class(TARM32_32BitRegister)
  public
    class function GetName: string; override;
    class function GetCode: Integer; override;
  end;

  TARM32_R11 = class(TARM32_32BitRegister)
  public
    class function GetName: string; override;
    class function GetCode: Integer; override;
  end;

  TARM32_R12 = class(TARM32_32BitRegister)
  public
    class function GetName: string; override;
    class function GetCode: Integer; override;
  end;

  TARM32_R13 = class(TARM32_32BitRegister)
  public
    class function GetName: string; override;
    class function GetCode: Integer; override;
  end;

  TARM32_R14 = class(TARM32_32BitRegister)
  public
    class function GetName: string; override;
    class function GetCode: Integer; override;
  end;

  ///////////////////////////////////////////////////////
  /// СУФИКСЫ
  ///////////////////////////////////////////////////////

  TARMSufix = class(TPlatformInstruction)
  public
    class function GetType: TInstructionType; override;
  end;

  TARM_EQ = class(TARMSufix)
  public
    class function GetName: string; override;
    class function GetCode: Integer; override;
  end;

  TARM_NQ = class(TARMSufix)
  public
    class function GetName: string; override;
    class function GetCode: Integer; override;
  end;

  TARM_CS = class(TARMSufix)
  public
    class function GetName: string; override;
    class function GetCode: Integer; override;
  end;

  TARM_CC = class(TARMSufix)
  public
    class function GetName: string; override;
    class function GetCode: Integer; override;
  end;

  TARM_MI = class(TARMSufix)
  public
    class function GetName: string; override;
    class function GetCode: Integer; override;
  end;

  TARM_PL = class(TARMSufix)
  public
    class function GetName: string; override;
    class function GetCode: Integer; override;
  end;

  TARM_VS = class(TARMSufix)
  public
    class function GetName: string; override;
    class function GetCode: Integer; override;
  end;

  TARM_VC = class(TARMSufix)
  public
    class function GetName: string; override;
    class function GetCode: Integer; override;
  end;

  TARM_HI = class(TARMSufix)
  public
    class function GetName: string; override;
    class function GetCode: Integer; override;
  end;

  TARM_LS = class(TARMSufix)
  public
    class function GetName: string; override;
    class function GetCode: Integer; override;
  end;

  TARM_GE = class(TARMSufix)
  public
    class function GetName: string; override;
    class function GetCode: Integer; override;
  end;

  TARM_LT = class(TARMSufix)
  public
    class function GetName: string; override;
    class function GetCode: Integer; override;
  end;

  TARM_GT = class(TARMSufix)
  public
    class function GetName: string; override;
    class function GetCode: Integer; override;
  end;

  TARM_LE = class(TARMSufix)
  public
    class function GetName: string; override;
    class function GetCode: Integer; override;
  end;

  TARM_AL = class(TARMSufix)
  public
    class function GetName: string; override;
    class function GetCode: Integer; override;
  end;

  ///////////////////////////////////////////////////////
  /// ИНСТРУКЦИИ
  ///////////////////////////////////////////////////////
  TARM32_Instruction = class(TPlatformInstruction)
  private
    FCondition: TARMCondition;
  end;

  TARM32_2ArgInstruction = class(TARM32_Instruction)
  private
    FLeft: TARMArgument;
    FRight: TARMArgument;
  public
    procedure ParseArgumnets(const Context: THWPContext); override;
    function Text: string; override;
  end;

  TARM32_3ArgInstruction = class(TARM32_Instruction)
  end;

  TARM32_LDR = class(TARM32_2ArgInstruction)
  protected
    class function GetCode: Integer; override;
    class function GetName: string; override;
  public
    procedure Encode(Stream: TStream); override;
    procedure CheckArguments(const Context: THWPContext); override;
  end;

  TARM32_STR = class(TARM32_2ArgInstruction)
  protected
    class function GetCode: Integer; override;
    class function GetName: string; override;
  public
    procedure Encode(Stream: TStream); override;
  end;

  TARM32_MOV = class(TARM32_2ArgInstruction)
  protected
    class function GetCode: Integer; override;
    class function GetName: string; override;
  public
    procedure Encode(Stream: TStream); override;
  end;

  TARM32_TEQ = class(TARM32_2ArgInstruction)
  protected
    class function GetCode: Integer; override;
    class function GetName: string; override;
  end;

  TARM32_ADD = class(TARM32_3ArgInstruction)
  protected
    class function GetCode: Integer; override;
    class function GetName: string; override;
  end;

  TARM32_SUB = class(TARM32_3ArgInstruction)
  protected
    class function GetCode: Integer; override;
    class function GetName: string; override;
  end;

  TARM32_RSB = class(TARM32_3ArgInstruction)
  protected
    class function GetCode: Integer; override;
    class function GetName: string; override;
  end;

  TARM32_MUL = class(TARM32_3ArgInstruction)
  protected
    class function GetCode: Integer; override;
    class function GetName: string; override;
  end;


implementation

uses PascalCompiler, CompilerMessages, CompilerErrors;

var
  Platform: TARM32Platform = nil;

function ParseArgument(const Context: THWPContext; var Arg: TARMArgument): TTokenID; forward;

{ TARM32Platform }

constructor TARM32Platform.Create(const Name: string);
begin
  inherited;
  // регистры
  DeclareRegister(TARM32_R0);
  DeclareRegister(TARM32_R1);
  DeclareRegister(TARM32_R2);
  DeclareRegister(TARM32_R3);
  DeclareRegister(TARM32_R4);
  DeclareRegister(TARM32_R5);
  DeclareRegister(TARM32_R6);
  DeclareRegister(TARM32_R7);
  DeclareRegister(TARM32_R8);
  DeclareRegister(TARM32_R9);
  DeclareRegister(TARM32_R10);
  DeclareRegister(TARM32_R11);
  DeclareRegister(TARM32_R12);
  DeclareRegister(TARM32_R13);
  DeclareRegister(TARM32_R14);

  // суфиксы условий
  DeclareInstruction(TARM_EQ);
  DeclareInstruction(TARM_NQ);
  DeclareInstruction(TARM_CS);
  DeclareInstruction(TARM_CC);
  DeclareInstruction(TARM_MI);
  DeclareInstruction(TARM_PL);
  DeclareInstruction(TARM_VS);
  DeclareInstruction(TARM_VC);
  DeclareInstruction(TARM_HI);
  DeclareInstruction(TARM_LS);
  DeclareInstruction(TARM_GE);
  DeclareInstruction(TARM_LT);
  DeclareInstruction(TARM_GT);
  DeclareInstruction(TARM_LE);
  DeclareInstruction(TARM_AL);

  // инструкции
  DeclareInstruction(TARM32_LDR);
  DeclareInstruction(TARM32_STR);
  DeclareInstruction(TARM32_MOV);
  DeclareInstruction(TARM32_TEQ);
  DeclareInstruction(TARM32_ADD);
  DeclareInstruction(TARM32_SUB);
  DeclareInstruction(TARM32_RSB);
  DeclareInstruction(TARM32_MUL);
end;


{ TARM32_2ArgInstruction }

procedure TARM32_2ArgInstruction.ParseArgumnets(const Context: THWPContext);
var
  Token: TTokenID;
begin
  Token := ParseArgument(Context, FLeft);
  TUnit(Context.AUnit).parser_MatchToken(Token, token_coma);
  Token := ParseArgument(Context, FRight);
  TUnit(Context.AUnit).parser_MatchToken(Token, token_semicolon);
end;

function TARM32_2ArgInstruction.Text: string;
begin

end;

{ TARM32_MOV }

procedure TARM32_MOV.Encode(Stream: TStream);
begin
  Stream.WriteUInt32(ARM32MOV(cAlways, FLeft, FRight));
end;

class function TARM32_MOV.GetCode: Integer;
begin
   Result := Integer(ARM32_MOV);
end;

class function TARM32_MOV.GetName: string;
begin
  Result := 'MOV';
end;

{ TARM32_ADD }

class function TARM32_ADD.GetCode: Integer;
begin
  Result := Integer(ARM32_ADD);
end;

class function TARM32_ADD.GetName: string;
begin
  Result := 'ADD';
end;

{ TARM32_SUB }

class function TARM32_SUB.GetCode: Integer;
begin
  Result := Integer(ARM32_SUB);
end;

class function TARM32_SUB.GetName: string;
begin
  Result := 'SUB';
end;

{ TARM32_RSB }

class function TARM32_RSB.GetCode: Integer;
begin
  Result := Integer(ARM32_RSB);
end;

class function TARM32_RSB.GetName: string;
begin
  Result := 'RSB';
end;

{ TARM32_MUL }

class function TARM32_MUL.GetCode: Integer;
begin
  Result := Integer(ARM32_MUL);
end;

class function TARM32_MUL.GetName: string;
begin
  Result := 'MUL';
end;

{ TARM32_STR }

procedure TARM32_STR.Encode(Stream: TStream);
begin
  Stream.WriteUInt32(ARM32STR(cAlways, FLeft, FRight));
end;

class function TARM32_STR.GetCode: Integer;
begin
  Result := Integer(ARM32_STR);
end;

class function TARM32_STR.GetName: string;
begin
  Result := 'STR';
end;

{ TARM32_LDR }

procedure TARM32_LDR.CheckArguments(const Context: THWPContext);
begin
  if Abs(FRight.IMMDisp) >= 4096 then
    AbortWork(sImmediateOffsetIsOutOfRangeFmt, [-4095, 4095], Context.Parser.PrevPosition);
end;

procedure TARM32_LDR.Encode(Stream: TStream);
begin
  Stream.WriteUInt32(ARM32LDR(cAlways, FLeft, FRight));
end;

class function TARM32_LDR.GetCode: Integer;
begin
  Result := Integer(ARM32_LDR);
end;

class function TARM32_LDR.GetName: string;
begin
  Result := 'LDR';
end;

{ TARM32_TEQ }

class function TARM32_TEQ.GetCode: Integer;
begin
  Result := Integer(ARM32_TEQ);
end;

class function TARM32_TEQ.GetName: string;
begin
  Result := 'TEQ';
end;

{ TARM32_32BitRegister }

class function TARM32_32BitRegister.GetSize: Integer;
begin
  Result := 4;
end;

{ TARM32_R0Register }

class function TARM32_R0.GetCode: Integer;
begin
  Result := Integer(ARM32_R0);
end;

class function TARM32_R0.GetName: string;
begin
  Result := 'R0';
end;

{ TARM32_R1Register }

class function TARM32_R1.GetCode: Integer;
begin
  Result := Integer(ARM32_R1);
end;

class function TARM32_R1.GetName: string;
begin
  Result := 'R1';
end;


{ TARM32_R2 }

class function TARM32_R2.GetCode: Integer;
begin
  Result := Integer(ARM32_R2);
end;

class function TARM32_R2.GetName: string;
begin
  Result := 'R2';
end;

{ TARM32_R3 }

class function TARM32_R3.GetCode: Integer;
begin
  Result := Integer(ARM32_R3);
end;

class function TARM32_R3.GetName: string;
begin
  Result := 'R3';
end;

{ TARM32_R4 }

class function TARM32_R4.GetCode: Integer;
begin
  Result := Integer(ARM32_R4);
end;

class function TARM32_R4.GetName: string;
begin
  Result := 'R4';
end;

{ TARM32_R5 }

class function TARM32_R5.GetCode: Integer;
begin
  Result := Integer(ARM32_R5);
end;

class function TARM32_R5.GetName: string;
begin
  Result := 'R5';
end;

{ TARM32_R6 }

class function TARM32_R6.GetCode: Integer;
begin
  Result := Integer(ARM32_R6);
end;

class function TARM32_R6.GetName: string;
begin
  Result := 'R6';
end;

{ TARM32_R7 }

class function TARM32_R7.GetCode: Integer;
begin
  Result := Integer(ARM32_R7);
end;

class function TARM32_R7.GetName: string;
begin
  Result := 'R7';
end;

{ TARM32_R8 }

class function TARM32_R8.GetCode: Integer;
begin
  Result := Integer(ARM32_R8);
end;

class function TARM32_R8.GetName: string;
begin
  Result := 'R8';
end;

{ TARM32_R9 }

class function TARM32_R9.GetCode: Integer;
begin
  Result := Integer(ARM32_R9);
end;

class function TARM32_R9.GetName: string;
begin
  Result := 'R9';
end;

{ TARM32_R10 }

class function TARM32_R10.GetCode: Integer;
begin
  Result := Integer(ARM32_R10);
end;

class function TARM32_R10.GetName: string;
begin
  Result := 'R10';
end;

{ TARM32_R11 }

class function TARM32_R11.GetCode: Integer;
begin
  Result := Integer(ARM32_R11);
end;

class function TARM32_R11.GetName: string;
begin
  Result := 'R11';
end;

{ TARM32_R12 }

class function TARM32_R12.GetCode: Integer;
begin
  Result := Integer(ARM32_R12);
end;

class function TARM32_R12.GetName: string;
begin
  Result := 'R12';
end;

{ TARM32_R13 }

class function TARM32_R13.GetCode: Integer;
begin
  Result := Integer(ARM32_R13);
end;

class function TARM32_R13.GetName: string;
begin
  Result := 'R13';
end;

{ TARM32_R14 }

class function TARM32_R14.GetCode: Integer;
begin
  Result := Integer(ARM32_R14);
end;

class function TARM32_R14.GetName: string;
begin
  Result := 'R14';
end;

{ TARMSufix }

class function TARMSufix.GetType: TInstructionType;
begin
  Result := itInstructionSufix;
end;

{ TARM_EQ }

class function TARM_EQ.GetCode: Integer;
begin
  Result := Integer(TARMCondition.cEqual);
end;

class function TARM_EQ.GetName: string;
begin
  Result := 'EQ';
end;

{ TARM_NQ }

class function TARM_NQ.GetCode: Integer;
begin
  Result := Integer(TARMCondition.cNotEqual);
end;

class function TARM_NQ.GetName: string;
begin
  Result := 'NE';
end;

{ TARM_CS }

class function TARM_CS.GetCode: Integer;
begin
  Result := Integer(TARMCondition.cCarrySet);
end;

class function TARM_CS.GetName: string;
begin
  Result := 'CS';
end;

{ TARM_CC }

class function TARM_CC.GetCode: Integer;
begin
  Result := Integer(TARMCondition.cCarryClear);
end;

class function TARM_CC.GetName: string;
begin
  Result := 'CС';
end;

{ TARM_MI }

class function TARM_MI.GetCode: Integer;
begin
  Result := Integer(TARMCondition.cMinus);
end;

class function TARM_MI.GetName: string;
begin
  Result := 'MI';
end;

{ TARM_PL }

class function TARM_PL.GetCode: Integer;
begin
  Result := Integer(TARMCondition.cPlus);
end;

class function TARM_PL.GetName: string;
begin
  Result := 'PL';
end;

{ TARM_VS }

class function TARM_VS.GetCode: Integer;
begin
  Result := Integer(TARMCondition.cOverflow);
end;

class function TARM_VS.GetName: string;
begin
  Result := 'VS';
end;

{ TARM_VC }

class function TARM_VC.GetCode: Integer;
begin
  Result := Integer(TARMCondition.cNoOverflow);
end;

class function TARM_VC.GetName: string;
begin
  Result := 'VC';
end;

{ TARM_HI }

class function TARM_HI.GetCode: Integer;
begin
  Result := Integer(TARMCondition.cUnsignedHigher);
end;

class function TARM_HI.GetName: string;
begin
  Result := 'HI';
end;

{ TARM_LS }

class function TARM_LS.GetCode: Integer;
begin
  Result := Integer(TARMCondition.cUnsignedLowerOrSame);
end;

class function TARM_LS.GetName: string;
begin
  Result := 'LS';
end;

{ TARM_GE }

class function TARM_GE.GetCode: Integer;
begin
  Result := Integer(TARMCondition.cSignedGreaterOrEqual);
end;

class function TARM_GE.GetName: string;
begin
  Result := 'GE';
end;

{ TARM_LT }

class function TARM_LT.GetCode: Integer;
begin
  Result := Integer(TARMCondition.cSignedLessThen);
end;

class function TARM_LT.GetName: string;
begin
  Result := 'LT';
end;

{ TARM_GT }

class function TARM_GT.GetCode: Integer;
begin
  Result := Integer(TARMCondition.cSignedGreaterThen);
end;

class function TARM_GT.GetName: string;
begin
  Result := 'GT';
end;

{ TARM_LE }

class function TARM_LE.GetCode: Integer;
begin
  Result := Integer(TARMCondition.cSignedLessOrEqual);
end;

class function TARM_LE.GetName: string;
begin
  Result := 'LE';
end;

{ TARM_AL }

class function TARM_AL.GetCode: Integer;
begin
  Result := Integer(TARMCondition.cAlways);
end;

class function TARM_AL.GetName: string;
begin
  Result := 'AL';
end;

function ParseMemArgument(const Context: THWPContext; var Arg: TARMArgument): TTokenID;
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
              Arg.RegDisp := Reg;
              //Include(Arg.Flags, memRegDisp);
            end
          else
            AbortWork(sInvalidArgument, Parser.Position);
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
            AbortWork(sInvalidArgument, Parser.Position);
          end;
          Arg.IMMDisp := StrToInt(Parser.OriginalToken)*Sign;
          Include(Arg.Flags, memIMMDisp);
          continue;
        end;

        AbortWork(sInvalidArgument, ID.TextPosition);
      end;
      token_plus: begin
        Sign := 1;
        Arg.DispSign := 1;
      end;
      token_minus: begin
        Sign := -1;
        Arg.DispSign := -1;
      end;
      token_asterisk: begin
        if Arg.ArgumentType <> ArgMemRegReg then
          AbortWork(sInvalidArgument, ID.TextPosition);
      end;
      token_closeblock: Break;
    end;
  end;
  //Parser.MatchToken(Result, token_closeblock);
  Result := Parser.NextToken;
end;

function ParseArgument(const Context: THWPContext; var Arg: TARMArgument): TTokenID;
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
  Lb: PHWLabel;
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
        itVar: Arg.ArgumentType := ArgVar;
        itConst: begin
          Arg.ArgumentType := ArgImmValue;
          Arg.IMMValue := Cardinal(TIDConstant(Decl).AsInt64);
        end;
      end;
      Arg.Decl := Decl;
      Exit;
    end;
    else
      AbortWork(sInvalidArgument, Parser.Position);
  end;
end;

initialization
  Platform := TARM32Platform.Create('ARM32');
  HWPlatforms.RegisterPlatform(Platform);

finalization
  Platform.Free;

end.
