unit NPCompiler.SysFunctions;

interface

uses NPCompiler, NPCompiler.Classes, NPCompiler.DataTypes, NPCompiler.Errors, SystemUnit, IL.Instructions,
  NPCompiler.Contexts, NPCompiler.ExpressionContext;

type
  {ôóíêöèÿ: typeid}
  TSF_typeid = class(TIDSysRuntimeFunction)
  protected
    function Process(var EContext: TEContext): TIDExpression; override;
  end;

  {ôóíêöèÿ: now}
  TSF_now = class(TIDSysRuntimeFunction)
  protected
    function Process(var EContext: TEContext): TIDExpression; override;
  end;

  {function: StaticAssert}
  TSF_StaticAssert = class(TIDSysCompileFunction)
  protected
    function Process(const Ctx: TSysFunctionContext): TIDExpression; override;
  end;



implementation

{ TSF_typeid }


function TSF_typeid.Process(var EContext: TEContext): TIDExpression;
var
  UN: TNPUnit;
  Expr: TIDExpression;
  Decl: TIDDeclaration;
  TypeID: TDataTypeID;
  RDecl: TIDIntConstant;
begin
  UN := GetUnit(EContext);
  // ÷èòàåì àðãóìåíò
  Expr := UN.RPNPopExpression(EContext);
  Decl := Expr.Declaration;

  case Decl.ItemType of
    itVar, itConst: TypeID := Decl.DataTypeID;
    itType: TypeID := TIDType(Decl).DataTypeID;
  else
    AbortWork(sVariableOrTypeRequired, Expr.TextPosition);
    Exit(nil);
  end;
  // ðåçóëüòàò
  RDecl := TIDIntConstant.CreateAnonymous(UN.ImplSection, SYSUnit._TypeID, Ord(TypeID));
  Result := TIDExpression.Create(RDecl, Expr.TextPosition);
end;

{ TSF_now }

function TSF_now.Process(var EContext: TEContext): TIDExpression;
var
  ILCode: TILInstruction;
begin
  Result := EContext.SContext.GetTMPVarExpr(SYSUnit._DateTime, GetUnit(EContext).parser_Position);
  ILCode := TIL.IL_Now(Result);
  EContext.SContext.ILWrite(ILCode);
end;

{ TSF_StaticAssert }

function TSF_StaticAssert.Process(const Ctx: TSysFunctionContext): TIDExpression;
var
  TextExpr, Expr: TIDExpression;
begin
  // читаем второй аргумент
  TextExpr := Ctx.UN.RPNPopExpression(Ctx.EContext^);
  Expr := Ctx.UN.RPNPopExpression(Ctx.EContext^);
  Ctx.UN.CheckConstExpression(Expr);
  if not Expr.AsBoolConst.Value then
  begin
    if TextExpr.AsStrConst.Value = '' then
      AbortWork('Static assertion. Expression: "' + Ctx.ParamsStr + '" is false.', Expr.TextPosition)
    else
      AbortWork('Static assertion. Message: "' + TextExpr.AsStrConst.Value + '"', Expr.TextPosition);
  end;
  Result := nil;
end;

end.
