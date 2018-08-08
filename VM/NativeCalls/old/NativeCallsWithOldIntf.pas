unit NativeCallsWithOldIntf;

interface

{$ifdef FPC}
  {$MODE DELPHI}	
{$endif}

uses SysUtils, DataTypes, ILTypeInfo, TypInfo;

type

  TNCDataType = (
    btU8              ,
    btS8              ,
    btU16             ,
    btS16             ,
    btU32             ,
    btS32             ,
    btSingle          ,
    btDouble          ,
    btExtended        ,
    btAnsiString      ,
    btRecord          ,
    btDynArray        ,
    btPointer         ,
    btVariant         ,
    btS64             ,
    btAnsiChar        ,
    btWideString      ,
    btWideChar        ,
    btProcPtr         ,
    btStaticArray     ,
    btSet             ,
    btCurrency        ,
    btClass           ,
    btInterface       ,
    btUnicodeString,
    btEnum,
    btVarParam
  );

  TNCArg = record
    Dta: Pointer;
    DataSize: Integer;
    BaseType: TNCDataType;
  end;
  PNCArg = ^TNCArg;

  TNCArgs = array of TNCArg;

  TNCCallingConvention = (cdRegister, cdPascal, cdCdecl, cdStdCall, cdSafeCall, cdConstructor);

function CreateNativeCallArgs(const DataTypes: array of TNCDataType; const Pointers: array of Pointer): TNCArgs;
function CreateNativeCallArg(DataType: TNCDataType): PNCArg;
function NativeCall(_Self, Address: Pointer; CallingConv: TNCCallingConvention; Params: TNCArgs; res: PNCArg): Boolean;

implementation

uses {$IFDEF CPUX86}
     NativeCallX86,
     NativeCallX86New
     {$ENDIF}
     {$IFDEF CPUX64}
     NativeCallX64,
     NativeCallX64New
     {$ENDIF}
     {$IFDEF CPUARM}
     NativeCallARM32,
     NativeCallARM32NEw
     {$ENDIF};

function NativeCall(_Self, Address: Pointer; CallingConv: TNCCallingConvention; Params: TNCArgs; res: PNCArg): Boolean;
begin
  {$IFDEF CPUX86}
  Result := NativeCallX86.X86Invoke(_Self, Address, CallingConv, Params, res);
  {$ENDIF}
  {$IFDEF CPUX64}
  Result := NativeCallX64.X64Invoke(_Self, Address, Params, res);
  {$ENDIF}
  {$IFDEF CPUARM}
  Result := NativeCallARM32.Invoke(_Self, Address, CallingConv, Params, res);
  {$ENDIF}
end;

function CreateNativeCallArgs(const DataTypes: array of TNCDataType; const Pointers: array of Pointer): TNCArgs;
var
  c, i: Integer;
begin
  c := Length(DataTypes);
  SetLength(Result, c);
  for i := 0 to c - 1 do begin
    Result[i].Dta := Pointers[i];
    Result[i].BaseType := DataTypes[i];
  end;
end;

function CreateNativeCallArg(DataType: TNCDataType): PNCArg;
begin
  New(Result);
  Result.BaseType := DataType;
  Result.Dta := GetMemory(8);
  PInt64(Result.Dta)^ := 0;
end;

end.
