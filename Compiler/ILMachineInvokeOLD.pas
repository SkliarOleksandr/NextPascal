unit ILMachineInvokeOLD;

interface

uses SysUtils, Rtti;

procedure InvokeExternal(PM: PNativeUInt; StackPtr, IMG: PByte); overload;
procedure InvokeExternalMethod(PM: PNativeUInt; Self, StackPtr, IMG: PByte); overload;
function FindExternalIntfMethod(const IntfName, MethodName: string): RTTI.TRttiMethod;

implementation

uses TypInfo, ILMachine, ILTypeInfo, DataTypes;

var
  RTTIContext: TRttiContext;

function GetTypeInfo(IMG: PByte; TypeInfoOffset: TOffset): PILTypeInfo;
begin
  if TypeInfoOffset > 0 then
    Result := PILTypeInfo(IMG + TypeInfoOffset)
  else
    Result := nil;
end;

function ILDataTypeToVCLTypeInfo(DataType: TDataTypeID): TypInfo.PTypeInfo;
begin
  case DataType of
    dtInt8: Result := TypeInfo(Int8);
    dtInt16: Result := TypeInfo(Int16);
    dtInt32: Result := TypeInfo(Int32);
    dtInt64: Result := TypeInfo(Int64);
    dtUInt8: Result := TypeInfo(UInt8);
    dtUInt16: Result := TypeInfo(UInt16);
    dtUInt32: Result := TypeInfo(UInt32);
    dtUInt64: Result := TypeInfo(UInt64);
    dtFloat32: Result := TypeInfo(Float32);
    dtFloat64: Result := TypeInfo(Float64);
    dtBoolean: Result := TypeInfo(Boolean);
    dtAnsiChar: Result := TypeInfo(AnsiChar);
    dtChar: Result := TypeInfo(Char);
    dtAnsiString: Result := TypeInfo(AnsiString);
    dtString: Result := TypeInfo(String);
  else
    raise Exception.CreateFmt('ILDataTypeToVCLTypeInfo: DataType "%s" is not supported', [GetDataTypeName(DataType)]);
  end;
end;


function PrepareInvokeArgs(ProcInfo: PRTTIProcedure; StackPtr, IMG: PByte; out ResultVCLTypeInfo: TypInfo.PTypeInfo): TArray<TValue>;
var
  i, c, StartPos: Integer;
  Params: PRTTIParams;
  Param: PRTTIParameter;
  ParamInfo: PILTypeInfo;
begin
  if ProcInfo.ResultType > 0 then begin
    ParamInfo := PILTypeInfo(IMG + ProcInfo.ResultType);
    ResultVCLTypeInfo := ILDataTypeToVCLTypeInfo(ParamInfo.DataType);
    Inc(StackPtr, cDataTypeSizes[ParamInfo.DataType]);
    StartPos := 1;
  end else begin
    ResultVCLTypeInfo := nil;
    StartPos := 0;
  end;

  c := ProcInfo.ParamsCount;
  SetLength(Result, c - StartPos);
  if c = 0 then
    Exit;

  Params := PRTTIParams(IMG + ProcInfo.Params2);
  for i := 0 to c - StartPos - 1 do
  begin
    {$R-}
    Param := @Params[i + StartPos];
    ParamInfo := PILTypeInfo(IMG + Param.DataType);
    case ParamInfo.DataType of
      dtInt8: begin
        Result[i] := PNativeInt(StackPtr)^;
        Inc(StackPtr, 4);
      end;
      dtInt16: begin
        Result[i] := PInt16(StackPtr)^;
        Inc(StackPtr, 4);
      end;
      dtInt32: begin
        Result[i] := PInt32(StackPtr)^;
        Inc(StackPtr, 4);
      end;
      dtInt64: begin
        Result[i] := PInt64(StackPtr)^;
        Inc(StackPtr, 8);
      end;
      dtUInt8, dtBoolean, dtAnsiChar: begin
        Result[i] := PUInt8(StackPtr)^;
        Inc(StackPtr, 4);
      end;
      dtUInt16, dtChar: begin
        Result[i] := PUInt16(StackPtr)^;
        Inc(StackPtr, 4);
      end;
      dtUInt32: begin
        Result[i] := PUInt32(StackPtr)^;
        Inc(StackPtr, 4);
      end;
      dtUInt64: begin
        Result[i] := PUInt64(StackPtr)^;
        Inc(StackPtr, 8);
      end;
      dtFloat32: begin
        Result[i] := PFlt32(StackPtr)^;
        Inc(StackPtr, 4);
      end;
      dtFloat64: begin
        Result[i] := PFlt64(StackPtr)^;
        Inc(StackPtr, 4);
      end;
      dtPointer: begin
        Result[i] := PPointer(StackPtr)^;
        Inc(StackPtr, SizeOf(Pointer));
      end;
      dtString: begin
        Result[i] := PUStr(StackPtr)^;
        Inc(StackPtr, SizeOf(Pointer));
      end;
      {$R+}
    else
      raise Exception.CreateFmt('ExternalInvoke: DataType "%s" is not supported', [GetDataTypeName(ParamInfo.DataType)]);
    end;
  end;
end;

procedure ProcessResultValue(StackPtr: PByte; ResultTypeInfo: PILTypeInfo; const Value: TValue); inline;
begin
  case ResultTypeInfo.DataType of
    dtInt8: PInt8(StackPtr)^ := Value.AsInteger;
    dtInt16: PInt16(StackPtr)^ := Value.AsInteger;
    dtInt32: PInt32(StackPtr)^ := Value.AsInteger;
    dtInt64: PInt64(StackPtr)^ := Value.AsInt64;
    dtUInt8: PUInt8(StackPtr)^ := Value.AsInteger;
    dtUInt16: PUInt16(StackPtr)^ := Value.AsInteger;
    dtUInt32: PUInt32(StackPtr)^ := UInt32(Value.AsInteger);
    dtUInt64: PUInt64(StackPtr)^ := UInt64(Value.AsInt64);
    dtFloat32: PFlt32(StackPtr)^ := Value.AsExtended;
    dtFloat64: PFlt64(StackPtr)^ := Value.AsExtended;
    dtBoolean: PBoolean(StackPtr)^ := Value.AsBoolean;
    dtAnsiChar: PAChar(StackPtr)^ := AnsiChar(Value.AsInteger);
    dtChar: PChar(StackPtr)^ := Char(Value.AsInteger);
    dtAnsiString: PAStr(StackPtr)^ := AnsiString(Value.AsString);
    dtString: PUstr(StackPtr)^ := Value.AsString;
  else
    raise Exception.CreateFmt('InvokeExternal: Result data type "%s" is not supported', [GetDataTypeName(ResultTypeInfo.DataType)]);
  end;
end;

procedure InvokeExternal(PM: PNativeUInt; StackPtr, IMG: PByte);
var
  ProcAddr: Pointer;
  ProcInfo: PRTTIProcedure;
  ProcArgs: TArray<TValue>;
  ResulVCLTypeInfo: TypInfo.PTypeInfo;
  Result: TValue;
begin
  ProcInfo := PRTTIProcedure(IMG + PNativeUInt(PM)^);
  ProcArgs := PrepareInvokeArgs(ProcInfo, StackPtr, IMG, ResulVCLTypeInfo);
  ProcAddr := PPointer(NativeUInt(PM) + SizeOf(Pointer))^;
  Result := Rtti.Invoke(ProcAddr, ProcArgs, ProcInfo.CallConvention, ResulVCLTypeInfo);
  // обработка результата
  if Assigned(ResulVCLTypeInfo) then
    ProcessResultValue(StackPtr, PILTypeInfo(IMG + ProcInfo.ResultType), Result);
end;

function FindExternalIntfMethod(const IntfName, MethodName: string): RTTI.TRttiMethod;
var
  Types: TArray<RTTI.TRttiType>;
  TypeInfo: RTTI.TRttiType;
  Methods: TArray<RTTI.TRttiMethod>;
  MethodInfo: TRttiMethod;
  i, j: Integer;
begin
  Types := RTTIContext.GetTypes;
  for i := 0 to Length(Types) - 1 do
  begin
    TypeInfo := Types[i];
    if (TypeInfo.ClassType <> TRttiInterfaceType) or (AnsiCompareText(TypeInfo.Name, IntfName) <> 0) then
      continue;
    Methods := TRttiInterfaceType(TypeInfo).GetMethods;
    for j := 0 to Length(Methods) - 1 do
    begin
      MethodInfo := Methods[j];
      if MethodInfo.Name = MethodName then
        Exit(MethodInfo);
    end;
  end;
  Result := nil;
end;

procedure InvokeExternalMethod(PM: PNativeUInt; Self, StackPtr, IMG: PByte);
var
  Method: RTTI.TRttiMethod;
  MethodInfo: PRTTIProcedure;
  MethodName: string;
  StructInfo: PILTypeInfo;
  StructName: string;
  InstanceV: TValue;
  ProcArgs: TArray<TValue>;
  ResulVCLTypeInfo: TypInfo.PTypeInfo;
  ResultValue: TValue;
begin
  MethodInfo := PRTTIProcedure(IMG + PM^);
  StructInfo := PILTypeInfo(IMG + MethodInfo.StructInfo);
  MethodName := string(IMG + MethodInfo.Name);
  StructName := string(IMG + StructInfo.TypeName);

  Method := FindExternalIntfMethod(StructName, MethodName);

  ProcArgs := PrepareInvokeArgs(MethodInfo, StackPtr, IMG, ResulVCLTypeInfo);

  case StructInfo.DataType of
    dtInterface: begin
      InstanceV := TValue.From(IInterface(Self));
      ResultValue := Method.Invoke(InstanceV, ProcArgs);
    end;
  else
    raise Exception.CreateFmt('InvokeExternalMethod: Data type [%s] is not supported', [GetDataTypeName(StructInfo.DataType)]);
  end;
  // обработка результата
  if Assigned(ResulVCLTypeInfo) then
    ProcessResultValue(StackPtr, PILTypeInfo(IMG + MethodInfo.ResultType), ResultValue);
end;

initialization
  RTTIContext := TRttiContext.Create;

finalization
  RTTIContext.Free;

end.
