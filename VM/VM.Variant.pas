unit VM.Variant;

interface

uses SysUtils, NPCompiler.DataTypes, AnsiStrings; // variants

type
  {тип - вариант, размер всегда 16 байт}
  TVMVariant = record
  private
  type
    TVarValue = record case Integer of
      0: (Data: array [0..1] of NativeUInt);
      1: (I64: Int64);
      2: (U64: UInt64);
      3: (I32: Int32);
      4: (U32: UInt32);
      5: (I16: Int16);
      6: (U16: UInt16);
      7: (I8: Int8);
      8: (U8: UInt8);
      9: (F64: Double);
     10: (PTR: Pointer);
     11: (NUInt: NativeUInt);
     12: (NInt: NativeInt);
     13: (Bool: Boolean);
     14: (UChar: Char);
     15: (AChar: AnsiChar);
    end;
  var
    VarType: TDataTypeID;
    VarData: TVarValue;
    procedure VariantConvertToError(ToTID: TDataTypeID);
    class procedure VariantCompareError(Left, Right: TDataTypeID); static;
  public
    procedure Clear;
    procedure Assign(const Value: TVMVariant);

    procedure ToInt8(var Dest: Int8);
    procedure ToInt16(var Dest: Int16);
    procedure ToInt32(var Dest: Int32);
    procedure ToInt64(var Dest: Int64);
    procedure ToUInt8(var Dest: UInt8);
    procedure ToUInt16(var Dest: UInt16);
    procedure ToUInt32(var Dest: UInt32);
    procedure ToUInt64(var Dest: UInt64);
    procedure ToNativeInt(var Dest: NativeInt);
    procedure ToNativeUInt(var Dest: NativeUInt);
    procedure ToBoolean(var Dest: Boolean);
    procedure ToChar(var Dest: Char);
    procedure ToAnsiChar(var Dest: AnsiChar);
    procedure ToString(var Dest: Pointer);
    procedure ToAnsiString(var Dest: Pointer);
    procedure ToFloat32(var Dest: Double);
    procedure ToFloat64(var Dest: Double);
    procedure ToPointer(var Dest: Pointer);
    procedure ToClass(var Dest: Pointer);
    procedure ToInteface(var Dest: Pointer);
    procedure ToClassRef(var Dest: Pointer);
    procedure ToDelphiVariant(var Dest: Variant);

    procedure FromInt8(const Src: Int8);
    procedure FromInt16(const Src: Int16);
    procedure FromInt32(const Src: Int32);
    procedure FromInt64(const Src: Int64);
    procedure FromUInt8(const Src: UInt8);
    procedure FromUInt16(const Src: UInt16);
    procedure FromUInt32(const Src: UInt32);
    procedure FromUInt64(const Src: UInt64);
    procedure FromNativeInt(const Src: NativeInt);
    procedure FromNativeUInt(const Src: NativeUInt);
    procedure FromBoolean(const Src: Boolean);
    procedure FromChar(const Src: Char);
    procedure FromAnsiChar(const Src: AnsiChar);
    procedure FromString(const Src: string);
    procedure FromAnsiString(const Src: AnsiString);
    procedure FromFloat32(const Src: Single);
    procedure FromFloat64(const Src: Double);
    procedure FromPointer(const Src: Pointer);
    procedure FromClass(const Src: Pointer);
    procedure FromInteface(const Src: Pointer);
    procedure FromClassRef(const Src: Pointer);
    procedure FromDelphiVariant(const Src: Variant);
    class function Compare(const Left, Right: TVMVariant): Int64; static;
    function AsString: string;
  end;
  PVMVariant = ^TVMVariant;

implementation

{ TVMVariant }

uses VM.Core.Managed;

procedure TVMVariant.Assign(const Value: TVMVariant);
begin
  Clear;
  VarType := Value.VarType;
  VarData := Value.VarData;
end;

function TVMVariant.AsString: string;
begin
  case VarType of
    dtInt8, dtInt16, dtInt32: Result := IntToStr(VarData.I32);
    dtUInt8, dtUInt16, dtUInt32: Result := UIntToStr(VarData.U32);
    dtInt64, dtNativeInt: Result := IntToStr(VarData.I64);
    dtUInt64, dtNativeUInt: Result := UIntToStr(VarData.U64);
    dtBoolean: Result := BoolToStr(VarData.Bool, True);
    dtChar: Result := VarData.UChar;
    dtAnsiChar: Result := string(VarData.AChar);
    dtFloat32, dtFloat64: Result := FloatToStr(VarData.F64);
    dtString: Result := string(VarData.PTR);
    dtAnsiString: Result := string(AnsiString(VarData.PTR));
  else
    VariantConvertToError(dtString);
  end;
end;

procedure TVMVariant.Clear;
begin
  case VarType of
    dtString, dtAnsiString: _VM_STR_DECREF(VarData.PTR);
  end;
  VarType := TDataTypeID(dtUnknown);
end;

class function TVMVariant.Compare(const Left, Right: TVMVariant): Int64;
begin
  case Left.VarType of
    dtInt8, dtInt16, dtInt32: begin
      case Right.VarType of
        dtInt8, dtInt16, dtInt32: Exit(Left.VarData.I32 - Right.VarData.I32);
        dtUInt8, dtUInt16, dtUInt32: Exit(Left.VarData.I32 - Right.VarData.U32);
      end;
    end;
    dtUInt8, dtUInt16, dtUInt32: begin
      case Right.VarType of
        dtUInt8, dtUInt16, dtUInt32: Exit(Left.VarData.U32 - Right.VarData.U32);
      end;
    end;
    dtInt64: begin
      case Right.VarType of
        dtInt8, dtInt16, dtInt32: Exit(Left.VarData.I64 - Right.VarData.I32);
        dtInt64: Exit(Left.VarData.I64 - Right.VarData.I64);
      end;
    end;
    dtUInt64: begin
      case Right.VarType of
        dtInt8, dtInt16, dtInt32: Exit(Left.VarData.U64 - Right.VarData.I32);
        dtUInt64: Exit(Left.VarData.U64 - Right.VarData.U64);
      end;
    end;
    dtNativeInt: begin
      case Right.VarType of
        dtInt8, dtInt16, dtInt32, dtNativeInt: Exit(Left.VarData.NInt - Right.VarData.NInt);
      end;
    end;
    dtNativeUInt: begin
      case Right.VarType of
        dtUInt8, dtUInt16, dtUInt32, dtNativeUInt: Exit(Left.VarData.NUInt - Right.VarData.NUInt);
      end;
    end;
    dtFloat32, dtFloat64: begin
      case Right.VarType of
        dtFloat32, dtFloat64: Exit(Trunc(Left.VarData.F64 - Right.VarData.F64));
      end;
    end;
    dtString: begin
      case Right.VarType of
        dtString: Exit(CompareText(string(Left.VarData.PTR), string(Right.VarData.PTR)));
      end;
    end;
    dtAnsiString: begin
      case Right.VarType of
        dtAnsiString: Exit(AnsiStrings.CompareText(AnsiString(Left.VarData.PTR), AnsiString(Right.VarData.PTR)));
      end;
    end;
    dtPointer: begin
    end;
    dtClass: begin
    end;
    dtClassOf: begin
    end;
    dtInterface: begin
    end;
  end;
  VariantCompareError(Left.VarType, Right.VarType);
  Result := 0;
end;

procedure TVMVariant.FromAnsiChar(const Src: AnsiChar);
begin
  VarType := dtAnsiChar;
  VarData.AChar := Src;
end;

procedure TVMVariant.FromAnsiString(const Src: AnsiString);
begin
  VarType := dtAnsiString;
  VarData.PTR := Pointer(Src);
  _VM_STR_INCREF(Pointer(Src));
end;

procedure TVMVariant.FromBoolean(const Src: Boolean);
begin
  VarType := dtBoolean;
  VarData.Bool := Src;
end;

procedure TVMVariant.FromChar(const Src: Char);
begin
  VarType := dtChar;
  VarData.UChar := Src;
end;

procedure TVMVariant.FromClass(const Src: Pointer);
begin
  VarType := dtClass;
  VarData.PTR := Src;
end;

procedure TVMVariant.FromClassRef(const Src: Pointer);
begin
  VarType := dtClassOf;
  VarData.PTR := Src;
end;

procedure TVMVariant.FromDelphiVariant(const Src: Variant);
begin
  // todo
end;

procedure TVMVariant.FromFloat32(const Src: Single);
begin
  VarType := dtFloat32;
  VarData.F64 := Src;
end;

procedure TVMVariant.FromFloat64(const Src: Double);
begin
  VarType := dtFloat64;
  VarData.F64 := Src;
end;

procedure TVMVariant.FromInt16(const Src: Int16);
begin
  VarType := dtInt16;
  VarData.I32 := Src;
end;

procedure TVMVariant.FromInt32(const Src: Int32);
begin
  VarType := dtInt32;
  VarData.I32 := Src;
end;

procedure TVMVariant.FromInt64(const Src: Int64);
begin
  VarType := dtInt64;
  VarData.I64 := Src;
end;

procedure TVMVariant.FromInt8(const Src: Int8);
begin
  VarType := dtInt8;
  VarData.I32 := Src;
end;

procedure TVMVariant.FromInteface(const Src: Pointer);
begin
  VarType := dtInterface;
  VarData.PTR := Src;
end;

procedure TVMVariant.FromNativeInt(const Src: NativeInt);
begin
  VarType := dtNativeInt;
  VarData.NInt := Src;
end;

procedure TVMVariant.FromNativeUInt(const Src: NativeUInt);
begin
  VarType := dtNativeUInt;
  VarData.NUInt := Src;
end;

procedure TVMVariant.FromPointer(const Src: Pointer);
begin
  VarType := dtPointer;
  VarData.PTR := Src;
end;

procedure TVMVariant.FromString(const Src: string);
begin
  VarType := dtString;
  VarData.PTR := Pointer(Src);
  _VM_STR_INCREF(Pointer(Src));
end;

procedure TVMVariant.FromUInt16(const Src: UInt16);
begin
  VarType := dtUInt16;
end;

procedure TVMVariant.FromUInt32(const Src: UInt32);
begin
  VarType := dtUInt32;
  VarData.U32 := Src;
end;

procedure TVMVariant.FromUInt64(const Src: UInt64);
begin
  VarType := dtUInt64;
  VarData.U64 := Src;
end;

procedure TVMVariant.FromUInt8(const Src: UInt8);
begin
  VarType := dtUInt8;
  VarData.U32 := Src;
end;

procedure TVMVariant.ToAnsiChar(var Dest: AnsiChar);
begin
  // todo
end;

procedure TVMVariant.ToAnsiString(var Dest: Pointer);
begin
  // todo
end;

procedure TVMVariant.ToBoolean(var Dest: Boolean);
begin
  case VarType of
    dtBoolean: Dest := VarData.Bool;
    dtInt8, dtInt16, dtInt32: Dest := Boolean(VarData.I32);
    dtUInt8, dtUInt16, dtUInt32: Dest := Boolean(VarData.U32);
  else
    VariantConvertToError(dtFloat32);
  end;
end;

procedure TVMVariant.ToChar(var Dest: Char);
begin
  // todo
end;

procedure TVMVariant.ToClass(var Dest: Pointer);
begin
  case VarType of
    dtClass: Dest := VarData.PTR;
  else
    VariantConvertToError(dtClass);
  end;
end;

procedure TVMVariant.ToClassRef(var Dest: Pointer);
begin
  case VarType of
    dtClassOf: Dest := VarData.PTR;
  else
    VariantConvertToError(dtClassOf);
  end;
end;

procedure TVMVariant.ToDelphiVariant(var Dest: Variant);
begin
  // todo
end;

procedure TVMVariant.ToFloat32(var Dest: Double);
begin
  case VarType of
    dtFloat32, dtFloat64: Dest := VarData.F64;
  else
    VariantConvertToError(dtFloat32);
  end;
end;

procedure TVMVariant.ToFloat64(var Dest: Double);
begin
  case VarType of
    dtFloat32, dtFloat64: Dest := VarData.F64;
  else
    VariantConvertToError(dtFloat64);
  end;
end;

procedure TVMVariant.ToInt16(var Dest: Int16);
begin
  case VarType of
    dtInt8, dtInt16: Dest := VarData.I16;
    dtInt32: Dest := VarData.I32;
    dtInt64: Dest := VarData.I64;
    dtUInt8, dtBoolean, dtAnsiChar: Dest := VarData.U16;
    dtUInt16: Dest := VarData.U16;
    dtUInt32: Dest := VarData.U32;
    dtUInt64: Dest := VarData.U64;
    dtChar: Dest := Ord(VarData.UChar);
  else
    VariantConvertToError(dtInt16);
  end;
end;

procedure TVMVariant.ToInt32(var Dest: Int32);
begin
  case VarType of
    dtInt8, dtInt16, dtInt32: Dest := VarData.I32;
    dtInt64: Dest := VarData.I64;
    dtUInt8, dtUInt16, dtBoolean, dtChar, dtAnsiChar: Dest := VarData.U16;
    dtUInt32: Dest := VarData.U32;
    dtUInt64: Dest := VarData.U64;
  else
    VariantConvertToError(dtInt32);
  end;
end;

procedure TVMVariant.ToInt64(var Dest: Int64);
begin
  case VarType of
    dtInt8, dtInt16, dtInt32, dtInt64: Dest := VarData.I64;
    dtUInt8, dtUInt16, dtUInt32,
    dtBoolean, dtChar, dtAnsiChar: Dest := VarData.U32;
    dtUInt64: Dest := VarData.U64;
    dtPointer: Dest := NativeInt(VarData.PTR);
  else
    VariantConvertToError(dtInt64);
  end;
end;

procedure TVMVariant.ToInt8(var Dest: Int8);
begin
  case VarType of
    dtInt8: Dest := VarData.I8;
    dtInt16: Dest := VarData.I16;
    dtInt32: Dest := VarData.I32;
    dtInt64: Dest := VarData.I64;
    dtUInt8: Dest := VarData.U8;
    dtUInt16: Dest := VarData.U16;
    dtUInt32: Dest := VarData.U32;
    dtUInt64: Dest := VarData.U64;
    dtBoolean: Dest := ord(VarData.Bool);
    dtChar: Dest := ord(VarData.UChar);
    dtAnsiChar: Dest := ord(VarData.AChar);
  else
    VariantConvertToError(dtInt8);
  end;
end;

procedure TVMVariant.ToInteface(var Dest: Pointer);
begin
  case VarType of
    dtInterface: Dest := VarData.PTR;
  else
    VariantConvertToError(dtInterface);
  end;
end;

procedure TVMVariant.ToNativeInt(var Dest: NativeInt);
begin
  case VarType of
    dtInt8, dtInt16, dtInt32: Dest := VarData.I32;
    dtInt64: Dest := VarData.I64;
    dtUInt8, dtUInt16, dtUInt32,
    dtBoolean, dtChar, dtAnsiChar: Dest := VarData.U32;
    dtUInt64: Dest := VarData.U64;
    //dtPointer: Dest := NativeInt(VarData.PTR);
  else
    VariantConvertToError(dtNativeInt);
  end;
end;

procedure TVMVariant.ToNativeUInt(var Dest: NativeUInt);
begin
  // todo
end;

procedure TVMVariant.ToPointer(var Dest: Pointer);
begin
  // todo
end;

procedure TVMVariant.ToString(var Dest: Pointer);
var
  Str: string;
begin
  Str := AsString;
  Dest := Pointer(Str);
  _VM_STR_INCREF(Dest);
end;

procedure TVMVariant.ToUInt16(var Dest: UInt16);
begin
  // todo
end;

procedure TVMVariant.ToUInt32(var Dest: UInt32);
begin
  // todo
end;

procedure TVMVariant.ToUInt64(var Dest: UInt64);
begin
  // todo
end;

procedure TVMVariant.ToUInt8(var Dest: UInt8);
begin
  // todo
end;

class procedure TVMVariant.VariantCompareError(Left, Right: TDataTypeID);
begin
  raise Exception.CreateFmt('Variant (left: %s and right %s) compare error',
                            [GetDataTypeName(Left), GetDataTypeName(Right)]);
end;

procedure TVMVariant.VariantConvertToError(ToTID: TDataTypeID);
begin
  raise Exception.CreateFmt('Variant (source: %s and dest: %s) convert error',
                           [GetDataTypeName(VarType), GetDataTypeName(ToTID)]);
end;

end.
