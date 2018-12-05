using System;
using System.IO;
using System.Runtime.InteropServices;

namespace VM
{
    using Pointer = UInt32;
    using NativeInt = Int32;
    using NativeUInt = UInt32;
    using TOffset = UInt32;

    public class TStream: IDisposable
    {
        private Stream fStream;
        private BinaryReader reader;
        private BinaryWriter writer;

        public TStream Create(Stream stream)
        {
            fStream = stream;
            reader = new BinaryReader(stream);
            writer = new BinaryWriter(stream);
            return this;
        }
        public UInt32 ReadUInt32()
        {
            return reader.ReadUInt32();
        }
        public void WriteUInt32(UInt32 Value)
        {
            writer.Write(Value);
        }
        public long Position { get { return fStream.Position; } set { fStream.Position = value; } }
        public long Size { get { return fStream.Length; } }
        public Stream Data { get { return fStream; } }
        public void SetLength(long Value)
        {
            fStream.SetLength(Value);
        }
        public UInt32 GetAsUInt32(long Position)
        {
            fStream.Position = Position;
            return reader.ReadUInt32();
        }
        public void SetAsUInt32(long Position, UInt32 Value)
        {
            fStream.Position = Position;
            writer.Write(Value);
        }

        public void Dispose()
        {
            fStream.Dispose();
            reader.Dispose();
            writer.Dispose();
        }
    }

    public class TFileSteam : TStream
    {
        public TFileSteam Create(string FileName)
        {            
            base.Create(new FileStream(FileName, FileMode.Open));
            return this;            
        }
    }

    public class TMemoryStream : TStream
    {
        public TMemoryStream Create(string FileName)
        {
            base.Create(new MemoryStream());
            return this;
        }
    }

    enum TILCondition { cNone, cEqual, cNotEqual, cGreater, cGreaterOrEqual, cLess, cLessOrEqual, cZero, cNonZero };

    [StructLayout(LayoutKind.Sequential, Pack = 1)]
    unsafe struct TIMGHeader {
        private const int FLD_SIZE = 4;
        public UInt32 Signature;           // стартовая cигнатура
        public const int Offset_Signature = FLD_SIZE * 0;
        public UInt32 IMGFlags;            // флаги образа
        public const int Offset_IMGFlags = FLD_SIZE * 1;
        public UInt32 IMGSize;             // размер образа
        public const int Offset_IMGSize = FLD_SIZE * 2;
        public TOffset RTTIOffset;         // смщение секции RTTI
        public const int Offset_RTTIOffset = FLD_SIZE * 3;
        public TOffset ConstsOffset;       // смщение секции констант
        public const int Offset_ConstsOffset = FLD_SIZE * 4;
        public UInt32 ProcFramesCount;     // кол-во всего процедур в образе
        public const int Offset_ProcFramesCount = FLD_SIZE * 5;
        public TOffset ProcFramesSection;  // смещение секции списка (адрес, rtti) процедур
        public const int Offset_ProcFramesSection = FLD_SIZE * 6;
        public UInt32 ImportTableCount;    // кол-во элементов таблицы импорта
        public const int Offset_ImportTableCount = FLD_SIZE * 7;
        public TOffset ImportTable;        // смещение таблицы импорта
        public const int Offset_ImportTable = FLD_SIZE * 8;
        public TOffset UnitsOffset;        // смещение секции модулей
        public const int Offset_UnitsOffset = FLD_SIZE * 9;
        public TOffset FixTable;           // смещение фикс-таблицы
        public const int Offset_FixTable = FLD_SIZE * 10;
        public UInt32 HeaderCRC32;         // CRC32 заголовка
        public const int Offset_HeaderCRC32 = FLD_SIZE * 11;

    }

    [StructLayout(LayoutKind.Sequential, Pack = 1)]
    unsafe struct TVMUnit
    {
        public TRTTI Base;
        public TOffset FTypes;               // список типов (RTTI)
        public Int32 FVarsCount;             // кол-во глобальных переменных
        public TOffset FVars;           // список RTTI глобальных переменных
        public TOffset FProcs;               // список RTTI процедур
        public TOffset FExportProcs;  // список экспортируемых процедур модуля (RTTI)
        public TOffset InitProc;            // процедура инициализации (Address)
        public TOffset FinalProc;           // процедура финализации (Address)
        public int TypesCount { get { return *(NativeInt*)(FTypes - IntPtr.Size); } }
        public int VarsCount { get { return FVarsCount; } }
        public int ProcsCount { get { return *(NativeInt*)(FProcs - IntPtr.Size); } }
    }
  /*TVMUnit = packed object (TRTTI)
  private
    FTypes: TOffset;               // список типов (RTTI)
    FVarsCount: Int32;             // кол-во глобальных переменных
    FVars: PVMVariables;           // список RTTI глобальных переменных
    FProcs: Pointer;               // список RTTI процедур
    FExportProcs: PVMExportProcs;  // список экспортируемых процедур модуля (RTTI)
    FInitProc: TOffset;            // процедура инициализации (Address)
    FFinalProc: TOffset;           // процедура финализации (Address)
    function GetTypesCount: Integer; inline;
    function GetProcsCount: Int32;
    function GetExportProcsCount: Int32;
  public
    property TypesCount: Integer read GetTypesCount;
    property Types: TOffset read FTypes;
    property VarsCount: Int32 read FVarsCount;
    property Variables: PVMVariables read FVars;
    property ProcsCount: Int32 read GetProcsCount;
    property Procs: Pointer read FProcs;
    property ExportProcsCount: Int32 read GetExportProcsCount;
    property ExportProcs: PVMExportProcs read FExportProcs;
    property InitProc: TOffset read FInitProc;
    property FinalProc: TOffset read FFinalProc;
  end;*/

   // структура описывающая RTTI процедуры и размер ее стека
    public struct TVMIMGProcFrame
    {
        public Pointer ProcAddr;  // адрес размещения процедуры в образе
        public Pointer ProcInfo;  // RTTI процедуры
    }

    struct TVMVariable
    {
        public TRTTI Base;
        public Pointer DataType;
        public Pointer Addr;
        public Boolean Reference;
        public Boolean IsConstant;
    }

}