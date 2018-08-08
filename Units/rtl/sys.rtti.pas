unit sys.rtti;

interface

uses System;

type

  //=====================================================
  // BASE RTTI CLASS
  //=====================================================

  TRTTIClassID = (
  	rttiUnit,
    rttiType,
    rttiProc,
    rttiVar
  );

  TRTTI = packed class
  private
    FClassID: TRTTIClassID;  
    FName: string;             // имя типа  
    FImportLib: Integer;       // название библиотеки импорта
    FImportName: Integer;      // название типа в библиотке импорта    
  public
    property Name: string read FName;  
    property ClassID: TRTTIClassID read FClassID;      
  end;

  TRTTIType = packed class(TRTTI)
  private
    FDataTypeID: TDataTypeID;  // ID типа
    FTypePacked: Boolean;      // тип упакованный
    FDataSize: Integer;        // размер в байтах элемента данного типа
    FUnitID: Integer;          // ID модуля
    FIndex: Integer;           // индекс
  public
    property TypeID: TDataTypeID read FDataTypeID;
    property TypeSize: Int32 read FDataSize;    
    property TypePacked: Boolean read FTypePacked; 
  end;  

  //=====================================================
  // PROCEDURE INFO
  //=====================================================

  TCallConvention = (ConvNative, ConvRegister, ConvStdCall, ConvCDecl, ConvFastCall);

  TRTTIProcedure = packed class(TRTTI)
  private
    FADDR: Pointer;
    FCallConvention: TCallConvention;
    FStructInfo: Pointer;          // self
    FExternalMethod: Pointer;
    FResultType: Pointer;
    FParamsCount: Integer;
    FParams: Pointer;
    FStackSize: Integer;
    FFlags: Integer;
    FVirtualIndex: Int32;
  public
    
  end;

  //=====================================================
  // POINTER TYPE
  //=====================================================

  TRTTIPointer = packed class(TRTTIType)
  private
    FRefTypeInfoOffset: UInt32;
  end;

  //=====================================================
  // ORDINAL TYPE
  //=====================================================

  TRTTIOrdinal = packed class(TRTTIType)
  private
    FLoBound: Int64;   // нижняя граница ordinal типа
    FHiBound: Int64;   // верхняя граница ordinal типа
    FSigned: Boolean;  // знаковый ли тип
  public
    property LoBound: Int64 read FLoBound;
    property HiBound: Int64 read FHiBound;   
    property Signed: Boolean read FSigned;
  end;

  //=====================================================
  // FLOAT TYPE
  //=====================================================

  TRTTIFloat = packed class(TRTTIType)
  end;

  //=====================================================
  // ARRAY TYPE
  //=====================================================

  //TRTTIDimensions = array [0..65535] of TOffset;
  //PRTTIDimensions = ^TRTTIDimensions;

  TRTTIArray = packed class(TRTTIType)
  private  
    FElementTypeInfo: TRTTIType;     // ссылка на тип элемента массива
    FDimsCount: UInt32;              // кол-во измерений массива
    FDims: UInt32;                   // информация по измерениям (ссылки на TypeInfo)
  public
    property DimsCount: UInt32 read FDimsCount;    
  end;  

  //=====================================================
  // DYNAMIC ARRAY TYPE
  //=====================================================
  TRTTIDynArray = packed class(TRTTIType)
  private
    FFlags: UInt32;
    FInitProc: Pointer;
    FFinalProc: Pointer;
  public
    property InitProc: Pointer read FInitProc;
    property FinalProc: Pointer read FFinalProc;
  end;
  
  //=====================================================
  // SET TYPE
  //=====================================================

  TRTTISet = packed class(TRTTIArray) 
  end;

  //=====================================================
  // PROCEDURAL TYPE
  //=====================================================  

  TRTTIProcType = packed class(TRTTIType) 
  private
    FProcStatic: Boolean; // является ли обявление типа указателем на статическую процедуру или на метод
    FResultType: TRTTIType;
    FCallConvention: TCallConvention;
    FParamsCount: UInt32;
    FParams: Pointer;
  public
    property ParamsCount: Integer read FParamsCount;
    property ResultType: TRTTIType read FResultType;
    property CallConvention: TCallConvention read FCallConvention;
  end;  

  //=====================================================
  // STRUCT FIELD INFO
  //=====================================================

  TCopyOnWriteState = (cowNotNeeded, cowNeeded, cowDone);
    
  TRTTIField = packed record
  private
    FName: string;     
    FOffset: Integer;      // смещение в стековом фрейме/области глобальных переменных/структуре
    FReference: Boolean;   // это ссылочный параметр
    FIsConstant: Boolean;  // это константа
    FDataType: TRTTIType;  // тип поля
    FCopyOnWrite: TCopyOnWriteState;
  public
    property Name: string read FName;  
    property DataType: TRTTIType read FDataType;
  end;

  TRTTIFields = array of TRTTIFields;

  //=====================================================
  // COMMON STRUCT TYPE
  //=====================================================

  TRTTIStruct = packed class(TRTTIType) 
  private
    FAncestor: UInt32;                    // предок
    FFieldsCount: UInt32;                 // кол-во полей
    FFields: UInt32;                      // поля
    FMethodsCount: UInt32;                // кол-во методов
    FMethods: UInt32;                     // методы
    FTotalMethodsCount: UInt32;           // кол-во методов включая всех предков
    FTotalFieldsCount: UInt32;            // кол-во полей включая всех предков
  public
    property FieldsCount: UInt32 read FFieldsCount;
    property MethodsCount: UInt32 read FMethodsCount;
  end;

  //=====================================================
  // RECORD TYPE
  //=====================================================  


  TRTTIRecord = packed class(TRTTIStruct)
  private
    FFlags: Byte;
  end;

  //=====================================================
  // INTERFACE TYPE
  //=====================================================  

  TRTTIInterface = packed class(TRTTIStruct) 
  private
    FInterfaceID: UInt32;
    FGUID: TGUID;
  end;

  //=====================================================
  // CLASS TYPE
  //=====================================================

  TRTTIClass = packed class(TRTTIStruct)
  private
    FVMTCount: UInt32;  // кол-во строк в таблице VMT
    FVMT: UInt32;       // указатель на таблицу VMT, таблица содержит все вируальные методы начиная с TObject
    FIMTS: UInt32;      // ссылка на таблицу таблиц IMT
  end;  

  TRTTITypes = array of TRTTIType;
  TRTTIProcs = array of TRTTIProcedure; 
 
  TRTTIUnit = packed class(TRTTI)
  private
    FTypes: TRTTITypes;          // список типов (RTTI)
    FVarsCount: Int32;           // кол-во глобальных переменных
    FVars: Pointer;              // список RTTI глобальных переменных
    FProcs: TRTTIProcs;          // список RTTI процедур
    FExportProcsCount: Int32;    // кол-во экспортируемых процедур модуля
    FExportProcs: Pointer;       // список экспортируемых процедур модуля (RTTI)
    FInitProc: Pointer;          // процедура инициализации (Address)
    FFinalProc: Pointer;         // процедура финализации (Address)
    function GetTypesCount: Integer; inline;
    function GetProcsCount: Integer; inline;    
  public 
    property Types: TRTTITypes read FTypes;
    property TypesCount: Integer read GetTypesCount;
    property Procs: TRTTIProcs read FProcs;
    property ProcsCount: Integer read GetProcsCount;
  end;
 
  TRTTIUnits = array of TRTTIUnit; 
 
  function GetCurrentUnit: TRTTIUnit; inline;
  function GetUnitsList: TRTTIUnits;

implementation

function GetCurrentUnit: TRTTIUnit;
begin
  asm
    macro 'getcurrentunit', Result;
  end;	    
end;

function GetUnitsList: TRTTIUnits;
begin
  asm
    macro 'getunitslist', Result;
  end;	  
end;

function TRTTIUnit.GetTypesCount: Integer;
begin
  Result := Length(FTypes);	  
end;

function TRTTIUnit.GetProcsCount: Integer;
begin
  Result := Length(FProcs);	  
end;

initialization 
   var _tmp := sizeof(TRTTIUnit) +
      sizeof(TRTTIType) +
      sizeof(TRTTIOrdinal) +
      sizeof(TRTTIFloat) +
      sizeof(TRTTIProcedure) +
      sizeof(TRTTIPointer) +
      sizeof(TRTTIArray) +
      sizeof(TRTTIDynArray) +
      sizeof(TRTTIField)  +
      sizeof(TRTTIStruct) +
      sizeof(TRTTIClass) +
      sizeof(TRTTIInterface) +
      sizeof(TRTTIRecord) +
      sizeof(TRTTIProcType) +
      sizeof(TRTTISet);
end.