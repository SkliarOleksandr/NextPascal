unit system;

interface

const 
  MinInt8  = -128;
  MaxInt8  =  127;
	
  MinInt16 = -32768;
  MaxInt16 =  32767;
	
  MinInt32 = -2147483648;
  MaxInt32 =  2147483647;
		
  MinInt64 = -9223372036854775808; // F333333333333334 !!!!!!!!! 
  MaxInt64 =  9223372036854775807; // 7FFFFFFFFFFFFFFF

  MinUInt8  = 0;
  MaxUInt8  = 255;
	
  MinUInt16 = 0;
  MaxUInt16 = 65535;
  
  MinUInt32 = 0;
  MaxUInt32 = 4294967295;

  MinUInt64 = 0;
  MaxUInt64 = $FFFFFFFFFFFFFFFF;


type

  Byte = UInt8;
  Integer = Int32;
  
{ Single = Float32;   
  Double = Float64;
     
  PInt8 = ^Int8;
  PInt16 = ^Int16;
  PInt32 = ^Int32;
  PInt64 = ^Int64;  

  PUInt8 = ^UInt8;
  PUInt16 = ^UInt16;
  PUInt32 = ^UInt32;
  PUInt64 = ^UInt64;  
  
  PFloat32 = ^Float32;   
  PFloat64 = ^Float64;
  
  PBoolean = ^Boolean;
 
  PAnsiChar = ^AnsiChar;
  PChar = ^Char;
  PAnsiString = ^AnsiString;     
  PString = ^String;  
  
  PVariant = ^Variant;}

  TDataTypeID =
  (
    dtInt8,
    dtInt16,
    dtInt32,
    dtInt64,
    dtUInt8,
    dtUInt16,
    dtUInt32,
    dtUInt64,
    dtNativeInt,
    dtNativeUInt,
    dtFloat32,
    dtFloat64,
    dtBoolean,      // булевый тип
    dtAnsiChar,     // ansi-символ
    dtChar,         // utf16-символ
    dtAnsiString,   // ansi-строка
    dtString,       // utf16-строка
    dtVariant,      // тип вариант
    dtGuid,         // GUID
    dtPointer,      // указатель (любой)
    dtWeakRef,      // слабая ссылка
    dtGeneric,      // обобщенный тип
    dtRange,        // диаппазоный тип (тип с ограниченным диаппазоном)
    dtEnum,         // перечисление
    dtSet,
    dtStaticArray,  // статический массив
    dtDynArray,     // динамический массив
    dtOpenArray,
    dtProcType,     // процедурная ссылка
    dtRecord,       // структура
    dtClass,        // класс
    dtClassOf,      // метакласс
    dtInterface     // интерфейс
  );


	IInterface = interface
	  
	end;
	
  TObject = class
  private
    FTypeInfo: NativeInt;  // has offset - 16
    FRefCount: NativeInt;  // has offset - 12
    FWeekInfo: NativeInt;  // has offset - 8
    FSyncInfo: NativeInt;  // has offset - 4
  public
    constructor Create; inline;
    destructor Destroy; virtual;
    //function ClassName: string;
  end;
  
  TClass = class of TObject;


  Exception = class(TObject)
  private
    FMsg: string;
  public  
    property Message: string read FMsg;
    constructor Create(const Msg: string); overload;
    //constructor Create(const Msg: string; const Params: array of _Variant); overload;
  end;


  EAssert = class(Exception)
  {protected
    constructor InternalCreate(const Msg: string);}
  end;
{}

implementation

constructor TObject.Create;
begin
end;

destructor TObject.Destroy;
begin
end;

constructor Exception.Create(const Msg: string);
begin
  FMsg := Msg;	
end;

{constructor EAssert.InternalCreate(const Msg: string);
begin
  
end;

{function TObject.ClassName: string;
begin
  Result := TypeName(Self);
end;}

end.