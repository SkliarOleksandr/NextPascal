unit NativeCalls;

interface

{$i compilers.inc}

uses SysUtils, TypInfo;

type

  TArgTypeID = (
    atInt8,
    atInt16,
    atInt32,
    atInt64,
    atNativeInt,
    atDynArray,
    atString,
    atInterface,
    atFloat32,
    atFloat64,

    atSet1,
    atSet2,
    atSet3,
    atSet4,
    atSet5,
    atSet6,
    atSet7,
    atSet8,
    atSetN,

    atRecord1,
    atRecord2,
    atRecord3,
    atRecord4,
    atRecord5,
    atRecord6,
    atRecord7,
    atRecord8,
    atRecordN,

    atStaticArray1,
    atStaticArray2,
    atStaticArray3,
    atStaticArray4,
    atStaticArray5,
    atStaticArray6,
    atStaticArray7,
    atStaticArray8,
    atStaticArrayN,

    atVariant
  );

  TArgs = record
  type
    TArgList = array of NativeInt;
    TNativeIntArray = array of NativeInt;
  private
    FArgs: TArgList;
    FData: TNativeIntArray;
    FResultPtr: PNativeUInt;
    FResultTID: TArgTypeID;
    procedure TypeError(TypeKind: TTypeKind);
    function GetTID(TypeKind: TTypeKind; TypeSize: Integer): TArgTypeID;
    function GetHasResult: Boolean;
  public
    procedure Init;
    procedure InitWithResult<T>({$IFDEF FPC}constref {$ELSE} const [ref]{$ENDIF} Arg: T);
    procedure Add<T>({$IFDEF FPC}constref {$ELSE} const [ref]{$ENDIF} Arg: T; isVarParam: Boolean = False);
    procedure Free;
    property Args: TArgList read FArgs;
    property Data: TNativeIntArray read FData;
    property HasResult: Boolean read GetHasResult;
    property ResultPtr: PNativeUInt read FResultPtr;
    property ResultTID: TArgTypeID read FResultTID;
  end;

  TNCCallingConvention = (cdRegister, cdPascal, cdCdecl, cdStdCall, cdSafeCall, cdConstructor);

procedure NativeCall(const Address: Pointer; const ArgsInfo: TArgs.TArgList; const Data: PCardinal); overload;
procedure NativeCall(const Address: Pointer; const ArgsInfo: TArgs.TArgList; const Data, ResultData: PCardinal; const ResultTID: TArgTypeID); overload;

implementation

  {$IFDEF CPUX86}
   uses NativeCallX86;
   {$ENDIF}
   {$IFDEF CPUX64}
   {$IFNDEF NEXTGEN}
   uses NativeCallX64;
   {$ENDIF}
   {$ENDIF}
   {$IFDEF CPUARM}
   uses NativeCallARM32;
   {$ENDIF}

procedure NativeCall(const Address: Pointer; const ArgsInfo: TArgs.TArgList; const Data: PCardinal);
begin
  {$IFDEF CPUX86}
  NativeCallX86.X86Invoke(Address, ArgsInfo, Data);
  {$ENDIF}
  {$IFDEF CPUX64} {$IFNDEF NEXTGEN}
  NativeCallX64.X64Invoke(Address, ArgsInfo, Data);
  {$ENDIF} {$ENDIF}
  {$IFDEF CPUARM}
  NativeCallARM32.ARM32Invoke(Address, ArgsInfo, Data);
  {$ENDIF}
end;

procedure NativeCall(const Address: Pointer; const ArgsInfo: TArgs.TArgList; const Data, ResultData: PCardinal; const ResultTID: TArgTypeID); overload;
begin
  {$IFDEF CPUX86}
  NativeCallX86.X86Invoke(Address, ArgsInfo, Data, ResultData, ResultTID);
  {$ENDIF}
  {$IFDEF CPUX64} {$IFNDEF NEXTGEN}
  NativeCallX64.X64Invoke(Address, ArgsInfo, Data, ResultData, ResultTID);
  {$ENDIF} {$ENDIF}
  {$IFDEF CPUARM}
  NativeCallARM32.ARM32Invoke(Address, ArgsInfo, Data, ResultData, ResultTID);
  {$ENDIF}
end;

{ TArgs }

function TArgs.GetHasResult: Boolean;
begin
  Result := Assigned(FResultPtr);
end;

function TArgs.GetTID(TypeKind: TTypeKind; TypeSize: Integer): TArgTypeID;
begin
  case TypeKind of
    tkInteger, tkEnumeration: begin
      case TypeSize of
        1: Result := atInt8;
        2: Result := atInt16;
        4: Result := atInt32;
      else
        TypeError(TypeKind);
        Result := atInt8;
      end;
    end;
    tkChar: Result := atInt8;
    tkFloat: begin
      case TypeSize of
        4: Result := atFloat32;
        8: Result := atFloat64;
      else
        TypeError(TypeKind);
        Result := atInt8;
      end;
    end;
    tkSet: begin
      case TypeSize of
        1: Result := atSet1;
        2: Result := atSet2;
        3: Result := atSet3;
        4: Result := atSet4;
        5: Result := atSet5;
        6: Result := atSet6;
        7: Result := atSet7;
        8: Result := atSet8;
      else
        Result := atSetN;
      end;
    end;
    tkClass: Result := atNativeInt;
    tkWChar: Result := atInt16;
    tkString, tkLString,
    tkWString, tkUString {$IFDEF FPC}, tkAString{$ENDIF}: Result := atString;
    tkArray: begin
      case TypeSize of
        1: Result := atStaticArray1;
        2: Result := atStaticArray2;
        3: Result := atStaticArray3;
        4: Result := atStaticArray4;
        5: Result := atStaticArray5;
        6: Result := atStaticArray6;
        7: Result := atStaticArray7;
        8: Result := atStaticArray8;
      else
        Result := atStaticArrayN;
      end;
    end;
    tkRecord: begin
      case TypeSize of
        1: Result := atRecord1;
        2: Result := atRecord2;
        3: Result := atRecord3;
        4: Result := atRecord4;
        5: Result := atRecord5;
        6: Result := atRecord6;
        7: Result := atRecord7;
        8: Result := atRecord8;
      else
        Result := atRecordN;
      end;
    end;
    tkInterface: Result := atInterface;
    tkInt64 {$IFDEF FPC}, tkQWord{$ENDIF}: Result := atInt64;
    tkDynArray: Result := atDynArray;
    tkClassRef, tkPointer: Result := atNativeInt;
    tkVariant: Result := atVariant;
  else
    TypeError(TypeKind);
    Result := atInt8;
  end;
end;

procedure TArgs.Add<T>({$IFDEF FPC}constref {$ELSE} const [ref]{$ENDIF} Arg: T; isVarParam: Boolean);
const
  PTR_SIZE = sizeof(Pointer);
var
  c: Integer;
  ti: PTypeInfo;
  ts: Integer;
  TID: TArgTypeID;
  TypeKind: TTypeKind;
begin
  c := Length(FArgs);
  SetLength(FArgs, c + 1);
  ti := TypeInfo(T);
  ts := SizeOf(T);

  if Assigned(ti) then
    TypeKind := ti.Kind
  else
    TypeKind := tkSet; // workaround

  if not isVarParam then
  begin
    TID := GetTID(TypeKind, ts);
  end else begin
    TID := atNativeInt;
    ts := PTR_SIZE;
  end;
  FArgs[c] := ord(TID) + (ts shl 8);

  {$IFDEF PTR_SIZE4}
  if (ts > 4) and (ts <= 8) then
  begin
    c := Length(FData);
    SetLength(FData, c + 2);
    FData[c] := 0;
    FData[c + 1] := 0;
    Move(Arg, FData[c], ts);
  end else begin
    c := Length(FData);
    SetLength(FData, c + 1);
    FData[c] := 0;
    if isVarParam or (ts > PTR_SIZE) then
      FData[c] := NativeInt(@Arg)
    else
      Move(Arg, FData[c], ts);
  end;
  {$ELSE}
  c := Length(FData);
  SetLength(FData, c + 1);
  FData[c] := 0;
  if isVarParam or (ts > PTR_SIZE) then
    FData[c] := NativeInt(@Arg)
  else
    Move(Arg, FData[c], ts);
  {$ENDIF}
end;

procedure TArgs.Free;
begin
  FArgs := nil;
  FData := nil;
end;

procedure TArgs.InitWithResult<T>({$IFDEF FPC}constref {$ELSE} const [ref]{$ENDIF} Arg: T);
var
  ti: PTypeInfo;
  ts: Integer;
  TypeKind: TTypeKind;
begin
  ti := TypeInfo(T);
  ts := SizeOf(T);

  if Assigned(ti) then
    TypeKind := ti.Kind
  else
    TypeKind := tkSet; // workaround

  FResultTID := GetTID(TypeKind, ts);
  FResultPtr := PNativeUInt(@Arg);
end;

procedure TArgs.Init;
begin
  FResultPtr := nil;
  FResultTID := atInt8;
end;

procedure TArgs.TypeError(TypeKind: TTypeKind);
begin
  raise Exception.CreateFmt('type is not supported: %d', [Ord(TypeKind)]);
end;

end.
