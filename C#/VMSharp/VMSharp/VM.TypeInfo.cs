using System;
using System.Runtime.InteropServices;

namespace VM
{
    using Pointer = UInt32;
    using NativeInt = Int32;
    using NativeUInt = UInt32;    
    using TOffset = UInt32;

    public enum TDataTypeID : Byte
    {
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
        dtGuid,         // тип GUID
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
    };

    [StructLayout(LayoutKind.Sequential, Pack = 1)]
    public struct TVMObjHeader
    {
        public Pointer TypeInfo;
        public NativeInt RefCount;
        public Pointer WeekInfo;
        public Pointer SyncInfo;
    }
    // PVMObjHeader = ^TVMObjHeader;*/


    public enum TRTTIClassID : Byte
    {
        rttiUnit,
        rttiType,
        rttiProc,
        rttiVar
    };

    //=====================================================
    // COMMON RTTI CLASSES ANCESTOR
    //=====================================================

    [StructLayout(LayoutKind.Sequential, Pack = 1)]
    public struct TRTTI
    {
        public TVMObjHeader Base;
        public TRTTIClassID FClassID;
        public TOffset Name;            // имя
        public TOffset ImportLib;       // название библиотеки импорта
        public TOffset ImportName;      // название типа в библиотке импорта
    };

    //=====================================================
    // COMMON TYPE ANCESTOR
    //=====================================================

    [StructLayout(LayoutKind.Sequential, Pack = 1)]
    public struct TRTTIType
    {
        public TRTTI Base;
        public TDataTypeID DataTypeID;  // ID типа
        public Boolean TypePacked;      // тип упакованный
        public int DataSize;        // размер в байтах элемента данного типа
        public int UnitID;          // ID модуля
        public int Index;           // индекс
    };
    //=====================================================
    // ORDINAL TYPE
    //=====================================================
    [StructLayout(LayoutKind.Sequential, Pack = 1)]
    struct TRTTIOrdinal
    {
        TRTTIType Base;
        Int64 LoBound;  // нижняя граница ordinal типа
        Int64 HiBound;  // верхняя граница ordinal типа
        Boolean Signed;  // знаковый ли тип
    }
    //=====================================================
    // FLOAT TYPE
    //=====================================================
    [StructLayout(LayoutKind.Sequential, Pack = 1)]
    struct TRTTIFloat
    {
        TRTTIType Base;
    }
    //=====================================================
    // ARRAY TYPE
    //=====================================================
    [StructLayout(LayoutKind.Sequential, Pack = 1)]
    struct TRTTIArray
    {
        TRTTIType Base;
        TOffset ElementTypeInfoOffset; // ссылка на тип элемента массива
        UInt32 DimensionsCount;        // кол-во измерений массива
        TOffset Dimensions;            // информация по измерениям (ссылки на TypeInfo)
    }
    //=====================================================
    // DYNAMIC ARRAY TYPE
    //=====================================================
    [StructLayout(LayoutKind.Sequential, Pack = 1)]
    struct TRTTIDynArray
    {
        TRTTIType Base;
        Int32 Flags;
        TOffset InitProc;    // tmp
        Int32 FinalUIdx;   // tmp
        TOffset FinalProc;   // tmp
    }
    //=====================================================
    // POINTER TYPE
    //=====================================================
    [StructLayout(LayoutKind.Sequential, Pack = 1)]
    struct TRTTIPointer
    {
        TRTTIType Base;
        TOffset RefTypeInfoOffset;
    }
    //=====================================================
    // COMMON STRUCT TYPE
    //=====================================================
    [StructLayout(LayoutKind.Sequential, Pack = 1)]
    struct TRTTIStruct
    {
        public TRTTIType Base;
        public TOffset Ancestor;                    // предок
        public Int32 FieldsCount;                 // кол-во полей
        public TOffset Fields;                      // поля (array of TRTTIField)
        public Int32 MethodsCount;                // кол-во методов
        public TOffset Methods;                     // методы
        public Int32 TotalMethodsCount;           // кол-во методов включая всех предков
        public Int32 TotalFieldsCount;            // кол-во полей включая всех предков
    }
    //=====================================================
    // RECORD TYPE
    //=====================================================
    [StructLayout(LayoutKind.Sequential, Pack = 1)]
    struct TRTTIRecord
    {
        TRTTIStruct Base;
        Byte Flags;
    }
    //=====================================================
    // CLASS TYPE
    //=====================================================
    [StructLayout(LayoutKind.Sequential, Pack = 1)]
    struct TRTTIClass
    {
        TRTTIStruct Base;
        Int32 VMTCount;  // кол-во строк в таблице VMT
        TOffset VMT;       // указатель на таблицу VMT, таблица содержит все вируальные методы начиная с TObject
        TOffset IMTS;      // ссылка на таблицу таблиц IMT
    }
    //=====================================================
    // INTERFACE TYPE
    //=====================================================
    [StructLayout(LayoutKind.Sequential, Pack = 1)]
    struct TRTTIInterface
    {
        TRTTIStruct Base;
        Int32 InterfaceID;
        Guid GUID;
    }
    //=====================================================
    // PROCEDURAL TYPE
    //=====================================================
    public enum TCallConvention : Byte
    {
        ConvNative,
        ConvRegister,
        ConvStdCall,
        ConvCDecl,
        ConvFastCall
    };

    [StructLayout(LayoutKind.Sequential, Pack = 1)]
    unsafe struct TRTTIProcType
    {
        TRTTIType Base;
        Boolean ProcStatic; // является ли обявление типа указателем на статическую процедуру или на метод
        TOffset ResultType;
        TCallConvention CallConvention;
        int ParamsCount;
        TOffset Params;
    };

    //=====================================================
    // PROCEDURE INFO
    //=====================================================
    [StructLayout(LayoutKind.Sequential, Pack = 1)]
    struct TRTTIProcedure
    {
        public TRTTI Base;
        public TOffset ADDR;
        public TCallConvention CallConvention;
        public TOffset StructInfo;          // self
        public Pointer ExternalMethod;
        public TOffset ResultType;
        public Int32 ParamsCount;
        public TOffset Params;
        public Int32 StackSize;
        public Int32 Flags;
        public Int32 VirtualIndex;
        public TOffset LocalVars;    // TRTTILocalVars
    }

    [StructLayout(LayoutKind.Sequential, Pack = 1)]
    struct TSystemTypes
    {
        public const int Count = (int)(TDataTypeID.dtPointer) + 1;
        public TRTTIOrdinal _Int8;
        public TRTTIOrdinal _Int16;
        public TRTTIOrdinal _Int32;
        public TRTTIOrdinal _Int64;
        public TRTTIOrdinal _UInt8;
        public TRTTIOrdinal _UInt16;
        public TRTTIOrdinal _UInt32;
        public TRTTIOrdinal _UInt64;
        public TRTTIOrdinal _NativeInt;
        public TRTTIOrdinal _NativeUInt;
        public TRTTIOrdinal _Float32;
        public TRTTIOrdinal _Float64;
        public TRTTIOrdinal _Boolean;
        public TRTTIOrdinal _AnsiChar;
        public TRTTIOrdinal _Char;
        public TRTTIArray _AnsiString;
        public TRTTIArray _String;
        public TRTTIType _Variant;
        public TRTTIStruct _GUID;
        public TRTTIPointer _Pointer;
    };


}