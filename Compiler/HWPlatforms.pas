unit HWPlatforms;

interface

{$I compilers.inc}

uses SysUtils, StrUtils, Classes, Types, NPCompiler.Classes, iDStringParser,NPCompiler.Parser, NPCompiler.Messages, NPCompiler.Utils, AVL;

type

  TInstructionType = (itInstruction, itInstructionPrefix, itInstructionSufix);

  TArgumentType = (
    ArgNone   = -1,
    X86ARG_VAR    = 0,
    ArgProc   = 1,
    ArgType   = 2,
    ArgTConst = 3,
    ArgLabel,            // метка
    ArgFarPtr,           // дальний указатель                                - XXXX:XXXX
    ArgImmValue,         // Непосредственная (константа)	                   - mov eax, 1234567h
    ArgRegister,         // Регистровая	                                     - mov eax, ecx
    ArgMemImm,           // Прямая (абсолютная)	                             - mov eax, [3456789h]
    ArgMemRegister,      // Регистровая косвенная	                           - mov eax, [ecx]
    ArgMemRegDisp,       // Регистровая косвенная со смещением	             - mov eax, [ecx+1200h]
    ArgMemRegReg,        // Базово-индексная	                               - mov eax, [ecx+edx]
    ArgMemRegRegDisp,    // Базово-индексная со смещением	                   - mov eax, [ecx+edx+40h]
    ArgMemRegMulDisp,    // Индексная с масштабированием и смещением	       - mov eax, [esi*4+40h]
    ArgMemRegRegMul,     // Базово-индексная с масштабированием	             - mov eax, [edx+ecx*8]
    ArgMemRegRegMulDisp  // Базово-индексная с масштабированием и смещением  - mov eax, [ebx+edi*2+20h]
  );

  {=========================================================}
  {| BIT7 | BIT6 | BIT5 | BIT4 | BIT3 | BIT2 | BIT1 | BIT0 |}
  {=========================================================}
  {|      |      | TYPE | TYPE | TYPE | TYPE | SIDX | SIDX |}
  {=========================================================}

  TPlatformRegister = class;
  TPlatformRegisterClass = class of TPlatformRegister;
  TPlatformInstruction = class;


  THWInstructions = array of TPlatformInstruction;


  THWLabel = record
    Name: string;  // имя метки
    ADDR: Integer; // адрес с начала ASM секции
    Clients: THWInstructions; // список инструкций требующих эту метку
  end;
  PHWLabel = ^THWLabel;

  TX86Argument = record
    ArgumentType: TArgumentType;
    Decl: TIDDeclaration;
    Reg: TPlatformRegisterClass;
    RegIdx: TPlatformRegisterClass;
    IMMDisp: Integer;
    IMMValue: Cardinal;
    Mul: Integer;
    LB: PHWLabel;
  end;

  TIDPlatform = class;


  TLabels = TAVLTree<string, THWLabel>;

  THWPContext = record
  private
    FLabels: TLabels;
  public
    Scope: TScope;
    AUnit: TObject;
    Platform: TIDPlatform;
    Parser: TNPParser;
    Instructions: THWInstructions;
    Stream: TMemoryStream;
    function FindLabel(const Name: string; Instruction: TPlatformInstruction): PHWLabel;
    procedure AddLabel(const ID: TIdentifier; ADDR: Integer);
    procedure Init;
    procedure Destroy;
  end;


  TPrefixes = array of Integer;

  TPlatformInstruction = class
  private
    FLine: Integer;              // строка в исходном коде
    FNext: TPlatformInstruction;
  protected
    class function GetName: string; virtual; abstract;
    class function GetCode: Integer; virtual; abstract;
    class function GetArgsCount: Integer; virtual;
    class function GetType: TInstructionType; virtual;
    class function GetPrefixes: TPrefixes; virtual;
  public
    property Next: TPlatformInstruction read FNext write FNext;
    property Name: string read GetName;                      // имя инструкции
    property Code: Integer read GetCode;                     // код инструкции
    property ArgsCount: Integer read GetArgsCount;           // кол-во аргументов
    property InstructionType: TInstructionType read GetType; // тип (иструкция/префикс)
    property Prfixes: TPrefixes read GetPrefixes;            // возможные префиксы
    property Line: Integer read FLine;
    function Text: string; virtual;                          // мнемоника
    procedure Write(Stream: TStream); virtual;
    procedure ParseArgumnets(const Context: THWPContext); virtual;
    procedure Encode(Stream: TStream); virtual;
    procedure CheckArguments(const Context: THWPContext); virtual;
    procedure CorrectLabel(HWLabel: PHWLabel); virtual;
  end;

  TPlatformInstructionClass = class of TPlatformInstruction;

  TPlatformRegister = class
  public
    class function GetName: string; virtual; abstract;
    class function GetCode: Integer; virtual; abstract;
    class function GetSize: Integer; virtual; abstract;
  public
    property Name: string read GetName;
    property Code: Integer read GetCode;
  end;

  TIDPlatform = class
  type
    TInstructions = TAVLTree<string, TPlatformInstructionClass>;
    TRegisters = TAVLTree<string, TPlatformRegisterClass>;
  private
    FName: string;
    FInstructions: TInstructions;
    FRegisters: TRegisters;
    // FJoinPlatforms: array of TIDPlatform;
  public
    constructor Create(const Name: string); virtual;
    destructor Destroy; override;

    property Name: string read FName;

    function ParseArguments(const Context: THWPContext; Instruction: TPlatformInstruction): TTokenID; virtual;
    function ParseArgument(const Context: THWPContext; var Arg: TX86Argument): TTokenID; virtual; abstract;

    procedure DeclareInstruction(Instruction: TPlatformInstructionClass); overload;
    procedure DeclareInstructionAlias(const Name: string; Instruction: TPlatformInstructionClass); overload;
    procedure DeclareRegister(Register: TPlatformRegisterClass);
    procedure DeclareRegisterAlias(const Name: string; Register: TPlatformRegisterClass);
    //procedure Parse(Scope: TScope);

    function FindInstruction(const Name: string): TPlatformInstruction;
    function FindRegister(const Name: string): TPlatformRegisterClass;

    procedure JoinPlatfrom(Platform: TIDPlatform);
  end;

function FindPlatform(const Platform: string): TIDPlatform;

function ArgumentToText(const Arg: TX86Argument): string;
procedure RegisterPlatform(Platform: TIDPlatform);

implementation

uses {Platform.IL, Platform.X86, Platform.X86_64, Platform.ARM32,} NPCompiler, NPCompiler.Errors;

var
  Platforms: TStrings = nil;

procedure CheckCreatePlatforms;
begin
  if not Assigned(Platforms) then
    Platforms := TStringList.Create;
end;

function FindPlatform(const Platform: string): TIDPlatform;
var
  i: Integer;
begin
  CheckCreatePlatforms;
  i := Platforms.IndexOf(Platform);
  if i <> -1 then
    Result := TIDPlatform(Platforms.Objects[i])
  else
    Result := nil;
end;

procedure RegisterPlatform(Platform: TIDPlatform);
var
  i: Integer;
begin
  CheckCreatePlatforms;
  i := Platforms.IndexOf(Platform.Name);
  if i <> -1 then
    raise Exception.CreateFmt('Platfrom ''%s'' already registered', [UpperCase(Platform.Name)]);
  Platforms.AddObject(Platform.Name, Platform);
end;

{ TIDPlatform }

constructor TIDPlatform.Create(const Name: string);
begin
  FName := Name;
  FInstructions := TInstructions.Create(StrCICompare);
  FRegisters := TRegisters.Create(StrCICompare);
end;

function TIDPlatform.FindInstruction(const Name: string): TPlatformInstruction;
var
  Node: TInstructions.PAVLNode;
begin
  Node := FInstructions.Find(Name);
  if Assigned(Node) then
    Result := TPlatformInstructionClass(Node.Data).Create
  else
    Result := nil;
end;

function TIDPlatform.FindRegister(const Name: string): TPlatformRegisterClass;
var
  Node: TRegisters.PAVLNode;
begin
  Node := FRegisters.Find(Name);
  if Assigned(Node) then
    Result := TPlatformRegisterClass(Node.Data)
  else
    Result := nil;
end;

procedure TIDPlatform.DeclareInstructionAlias(const Name: string; Instruction: TPlatformInstructionClass);
begin
  if Assigned(FInstructions.InsertNode(Name, Instruction)) then
    raise Exception.CreateFmt(msgInstructionAlreadyDeclaredFmt, [Name]);
end;

procedure TIDPlatform.DeclareInstruction(Instruction: TPlatformInstructionClass);
begin
  if Assigned(FInstructions.InsertNode(Instruction.GetName, Instruction)) then
    raise Exception.CreateFmt(msgInstructionAlreadyDeclaredFmt, [Instruction.GetName]);
end;

procedure TIDPlatform.DeclareRegister(Register: TPlatformRegisterClass);
begin
  if Assigned(FRegisters.InsertNode(Register.GetName, Register)) then
    raise Exception.CreateFmt(msgRegisterAlreadyDeclaredFmt, [Register.GetName]);
end;

procedure TIDPlatform.DeclareRegisterAlias(const Name: string; Register: TPlatformRegisterClass);
begin
  if Assigned(FRegisters.InsertNode(Name, Register)) then
    raise Exception.CreateFmt(msgRegisterAlreadyDeclaredFmt, [Name]);
end;

destructor TIDPlatform.Destroy;
begin
  FRegisters.Free;
  FInstructions.Free;
  inherited;
end;

{ TPlatformInstruction }

procedure TPlatformInstruction.CheckArguments(const Context: THWPContext);
begin

end;

procedure TPlatformInstruction.CorrectLabel(HWLabel: PHWLabel);
begin

end;

procedure TPlatformInstruction.Encode(Stream: TStream);
begin

end;

class function TPlatformInstruction.GetArgsCount: Integer;
begin
  Result := 0;
end;

class function TPlatformInstruction.GetPrefixes: TPrefixes;
begin
  Result := nil;
end;

class function TPlatformInstruction.GetType: TInstructionType;
begin
  Result := itInstruction;
end;

procedure TPlatformInstruction.ParseArgumnets(const Context: THWPContext);
var
  Token: TTokenID;
begin
  Token := TNPUnit(Context.AUnit).parser_NextToken(nil);
  TNPUnit(Context.AUnit).parser_MatchToken(Token, token_semicolon);
end;

function TPlatformInstruction.Text: string;
begin
  Result := GetName;
end;

procedure TPlatformInstruction.Write(Stream: TStream);
begin

end;

procedure TIDPlatform.JoinPlatfrom(Platform: TIDPlatform);
begin

end;

function TIDPlatform.ParseArguments(const Context: THWPContext; Instruction: TPlatformInstruction): TTokenID;
var
  i, c: Integer;
  Args: array [0..3] of TX86Argument;
begin
  c := Instruction.ArgsCount;
  for i := 0 to c - 1 do begin
    Result := ParseArgument(Context, Args[i]);
    if i < c - 1 then
      Context.Parser.MatchToken(Result, token_coma);
  end;
  Result := token_unknown;
end;

function ArgumentToText(const Arg: TX86Argument): string;
begin
  case Arg.ArgumentType of
    X86ARG_VAR: Result := Arg.Decl.Name;
    ArgRegister: Result := Arg.Reg.GetName;
    ArgImmValue: Result := IntToStr(Arg.IMMValue);
    ArgMemRegister: Result := Format('[%s]', [Arg.Reg.GetName]);
    else Result := '<UNKNOWN FORMAT>';
  end;
end;

{ THWPContext }

procedure THWPContext.AddLabel(const ID: TIdentifier; ADDR: Integer);
var
  Node: TLabels.PAVLNode;
  LB: THWLabel;
  PL: PHWLabel;
  i: Integer;
begin
  LB.Name := ID.Name;
  LB.ADDR := ADDR;
  Node := FLabels.InsertNode(ID.Name, LB);
  if Assigned(Node) then
  begin
    PL := @Node.Data;
    if PL.ADDR <> -1 then
      AbortWork(sLabelRedeclaretedFmt, [ID.Name], ID.TextPosition);
    PL.ADDR := ADDR;
    for i := 0 to Length(PL.Clients) - 1 do
      PL.Clients[i].CorrectLabel(PL);
    PL.Clients := nil;
  end;
end;

function THWPContext.FindLabel(const Name: string; Instruction: TPlatformInstruction): PHWLabel;
var
  c: Integer;
  Node: TLabels.PAVLNode;
  LB: THWLabel;
begin
  Node := FLabels.Find(Name);
  if not Assigned(Node) then
  begin
    LB.Name := Name;
    LB.ADDR := -1;
    Node := FLabels.InsertNode(Name, LB);
  end;
  Result := @Node.Data;
  with Result^ do
  if ADDR = -1 then
  begin
    c := Length(Clients);
    SetLength(Clients, c + 1);
    Clients[c] := Instruction;
  end;
end;

procedure THWPContext.Init;
begin
  FLabels := TLabels.Create(StrCICompare);
  Stream := TMemoryStream.Create;
end;

procedure THWPContext.Destroy;
begin
  FLabels.Free;
  Stream.Free;
end;

initialization


finalization
  Platforms.Free;

end.
