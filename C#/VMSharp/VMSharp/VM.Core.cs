using System;
using System.IO;
using System.Diagnostics;
using System.Runtime.InteropServices;

namespace VM
{

    using Int8 = SByte;
    using UInt8 = Byte;    
    using Pointer = UInt32;
    using NativeInt = IntPtr;
    using NativeUInt = UInt32;
    using Float32 = Single;
    using Float64 = Double;
    using TOffset = UInt32;    

    enum TVMCode {
        VM_NOPE,
        VM_STACK,        // [аргументы: const: Int32 StackSize]
        //-----------
        LD_C_I32,        // [аргументы: сonst: Int32]
        LD_C_U32,
        LD_C_I64,        // [аргументы: сonst: Int64]
        LD_C_F32,        // [аргументы: сonst: Float32]
        LD_C_F64,        // [аргументы: сonst: Float64]
        //-----------
        LD_L_I8,
        LD_L_U8,
        LD_L_I16,
        LD_L_U16,
        LD_L_I32,
        LD_L_I64,
        LD_L_F32,
        LD_L_PTR,
        //-----------
        LD_R_I8,
        LD_R_U8,
        LD_R_I16,
        LD_R_U16,
        LD_R_I32,
        LD_R_I64,
        LD_R_F32,
        //-----------
        LD_G_I8,
        LD_G_U8,
        LD_G_I16,
        LD_G_U16,
        LD_G_I32,
        LD_G_I64,
        LD_G_F32,
        LD_G_PTR,
        LD_G_PROC,       // загрузка в Dst адреса процедуры указанной в памяти
        //-----------
        LD_D_I8,         // Dst.I8  = (Src1.PTR + offset)^
        LD_D_U8,         // Dst.U8  = (Src1.PTR + offset)^
        LD_D_I16,        // Dst.I16 = (Src1.PTR + offset)^
        LD_D_U16,        // Dst.U16 = (Src1.PTR + offset)^
        LD_D_I32,        // Dst.I16 = (Src1.PTR + offset)^
        LD_D_I64,        // Dst.I64 = (Src1.PTR + offset)^
        LD_D_F32,        // Dst.F64 = (Src1.PTR + offset)^
        //-----------
        LD_C_ZERO,     // RO := 0; [аргументы: нет]
        LD_C_ONE,      // R0 := 1; [аргументы: нет]
        //-----------
        MOVE_L_I32,      // копоирования 4 байт на стеке [аргументы: Dst.offset, Src.offset]
        ST_C_I32,        // сохранение константы в локалькую память [аргументы: Dst.offset, Const]
        //-----------
        ST_L_I8,
        ST_L_I16,
        ST_L_I32,
        ST_L_I64,
        ST_L_F32,
        ST_L_VAR,
        //-----------
        ST_R_I8,
        ST_R_I16,
        ST_R_I32,
        ST_R_I64,
        ST_R_F32,
        //-----------
        ST_G_I8,
        ST_G_I16,
        ST_G_I32,
        ST_G_I64,
        ST_G_F32,
        ST_G_VAR,
        //-----------
        ST_D_I8,         // (Dst.Ptr + [const offset])^ := Src1.I8
        ST_D_I16,        // (Dst.Ptr + [const offset])^ := Src1.I16
        ST_D_I32,        // (Dst.Ptr + [const offset])^ := Src1.I32
        ST_D_I64,        // (Dst.Ptr + [const offset])^ := Src1.I64
        ST_D_F32,        // (Dst.Ptr + [const offset])^ := Src1.F32
        //-----------
        CLR_L_I8,
        CLR_L_I16,
        CLR_L_I32,
        CLR_L_I64,
        CLR_L_F32,
        CLR_L_F64,
        //-----------
        CLR_R_I8,
        CLR_R_I16,
        CLR_R_I32,
        CLR_R_I64,
        CLR_R_F32,
        CLR_R_F64,
        //-----------
        CLR_G_I8,
        CLR_G_I16,
        CLR_G_I32,
        CLR_G_I64,
        CLR_G_F32,
        CLR_G_F64,
        //-----------
        CLR_D_I8,        // обнуление в памяти (Dst.PTR + [const offset])^.I8 := 0;
        CLR_D_I16,       // обнуление в памяти (Dst.PTR + [const offset])^.I16 := 0;
        CLR_D_I32,       // обнуление в памяти (Dst.PTR + [const offset])^.I32 := 0;
        CLR_D_I64,       // обнуление в памяти (Dst.PTR + [const offset])^.I64 := 0;
        CLR_D_F32,       // обнуление в памяти (Dst.PTR + [const offset])^.F32 := 0;
        CLR_D_F64,       // обнуление в памяти (Dst.PTR + [const offset])^.F64 := 0;
        //-----------
        MOVE_REG,        // Dst := Src
        MOVE_REG_TO_MEM, // копирование заданного кол-ва байт (не больше 8) из регистра в память
        //-----------
        CMP_I32_C,       // R0.I32 - CONST
        CMP_L_C32,       // сравнение в локальной памяти с константой  [args: Left.offset, Const: Int32]
        CMP_I32,         // сравнение INT32 в регистрах R0 и R1
        CMP_L_I32,       // сравнение в локальной памяти [args: Left.offset, Right.offset]
        CMP_I64,         // сравнение INT64 в регистрах R0 и R1
        CMP_F64,         // сравнение FLT64 в регистрах R0 и R1
        CMP_ASTR,        // сравнение ansi-строк
        CMP_USTR,        // сравнение unicode-строк
        CMP_VAR,         // сравнение variant-значений
        CMP_TEST32,      // логический AND
        CMP_TEST64,      // логический AND
        CMP_MEM_VS_REG,  // сравнение любого блока побайтово, cо значением в  регистре (макс 8 байт) размер задан в команде
        CMP_MEM,         // сравнение любого блока побайтово, размер задан в команде
        //-----------
        INC_L_I32,       // LocalVar.I32 := LocalVar.I32 + 1
        ADD_I32_C,       // Dst.I32 := Src.I32 + CONST
        ADD_L_I32_C,     // сложение в локальной памяти [аргументы: Dst.offset, Const: Int32]
        ADD_NUINT_C,     // Dst.NUInt := Src.NUInt + CONST Int32
        ADD_U32,
        ADD_U64,
        ADD_I32,
        ADD_L_I32,
        ADD_I64,
        ADD_F64,
        ADD_F64_CI32,    // Dst.F64 := Src.F64 + CONST Int32
        ADD_ASTR,
        ADD_USTR,
        //-----------
        SUB_I32_C,       // Dst.I32 := Src.I32 - CONST Int32
        SUB_F64_CI32,    // Dst.F64 := Src.F64 - CONST Int32
        SUB_I32,         // Dst.I32 := Src1.I32 - Src2.I32
        SUB_I64,         // Dst.I64 := Src1.I64 - Src2.I64
        SUB_F64,         // Dst.F64 := Src1.F64 - Src2.F64
        //-----------
        MUL_C32,
        MUL_I32,
        MUL_I64,
        MUL_F64,
        //-----------
        FMA_U32,         // Dst.U32 := Src1.U32 + Src2.U32 * Data1
        FMA_U64,         // Dst.U64 := Src1.U64 + Src2.U64 * Data1
        //-----------
        IDIV_U32_C,
        IDIV_I32,
        IDIV_I64,
        //-----------
        IMOD_U32_C,
        IMOD_I32,
        IMOD_I64,
        //-----------
        DIV_I32,
        DIV_I64,
        DIV_F64,
        //-----------
        NEG_I32,
        NEG_I64,
        NEG_F64,
        //-----------
        BIN_SHL32,
        BIN_SHL64,
        BIN_SHR32,
        BIN_SHR64,
        //-----------
        BIN_AND32,
        BIN_AND64,
        BIN_AND32_C,
        BIN_AND64_C,
        //-----------
        BIN_OR32,
        BIN_OR64,
        BIN_OR32_C,
        BIN_OR64_C,
        //-----------
        BIN_XOR32,
        BIN_XOR64,
        //-----------
        BIN_NOT32,
        BIN_NOT64,

        STRU_CREATE,                 // выделяет память под unicode-строку
        STRA_CREATE,                 // выделяет память под ansi-строку
        UNIQUE_USTR,
        UNIQUE_ASTR,

        STR_DECREF,                  // декремент счетчика строки
//        {$IFDEF CPUX64}STR_INCREF,{$ENDIF}
//        {$IFDEF CPUX64}STR_LENGTH,{$ENDIF}
//        {$IFDEF CPUX64}STR_MOVE,{$ENDIF}

        ARRAY_LENGTH,                // вычислят длинну дин. массива/строки
        ARRAY_INCREF,                // инкремент счетчика дин. массива/строки
        ARRAY_DECREF,                // декремент счетчика дин. массива
        ARRAY_MEM_ALLOC,             // выделяет память под дин. массив (Dst - массив, Src1 - кол-во элементов, Data1 - размер элемента)
        ARRAY_INIT,                  // инициализирует внутреннюю структуру дин. массива

        VIRT_METHOD_PTR,             // инструкция вычисляет адрес виртуального метода  R0 - self, R1 - VMT offset
        VM_SET_METHOD_PTR,           // инструкция записывает адрес метода. [Dst - адрес TMethod, Src1 - Self, Src2 - Адерс процедуры]

        MEM_ALLOC,                   // выделение памяти заданного размера
        MEM_FREE,                    // освобождение памяти
        MEM_SET,                     // заполнение памяти

        OBJ_CREATE,                  // создание экземпляра класса (экземляр в R0)
        OBJ_INCREF,                  // инкремент счетчика (экземляр в R0)
        OBJ_DECREF,                  // декремент счетчика (экземляр в R0), если счетчик достигает 0 то вызывает деструктор

        OBJ_WEAKREF,                 // получение слабой ссылки на обьект
        OBJ_STRONGREF,               // получение сильной ссылки на обьект

        WEAK_INCREF,                 // инкремент счетчика слабой ссылки
        WEAK_DECREF,                 // декремент счетчика слабой ссылки

        DINF_DECREF,                 // Delphi-интрефейс
        DINF_INCREF,                 // Delphi-интрефейс

        ETHROW,                      // выбрас исключения
        SCHECKB,                     // проверка диаппазона статического массива (индекс в R0, диаппазон массива в коде инструкции)
        DCHECKB,                     // проверка диаппазона динамического массива (индекс в R0, размер массива в R1)
        ACHECKB,                     // проверка диаппазона динамического массива (массив в R0, индекса R1)

        MOVE_MEM,                    // копирование памяти (Dst, Src1, Src2 - кол-во байт)
        MOVE_MEM_C,                  // копирование памяти, константный объем (Dst, Src1, Data1 - кол-во байт)
        MOVE_ARRAY,                  // копирование массива Copy(Dst, Src1)
        //-----------
        CNV_F64_S32,
        CNV_F64_U32,
        CNV_F64_S64,
        CNV_F64_U64,
        CNV_UCHR_TO_USTR,            // конвертирует unicode-символ в unicode-строку
        CNV_ACHR_TO_ASTR,            // конвертирует ansi-символ в ansi-строку

        CNV_ASTR_TO_USTR,            // конвертирует ansi-строку в unicode-строку
        CNV_USTR_TO_ASTR,            // конвертирует unicode-строку в ansi-строку
        //-----------
        CNV_VAR_TO_VALUE,            // конвертирует variant в заданный тип (Dst = Src1, CONST Int32 - dst data type)
        CNV_VALUE_TO_VAR,            // конвертирует заданный тип в variant (Dst = Src1, CONST Int32 - src data type)
        VAR_RELEASE,                 // финализация варианта
        //-----------

        CALL_PROC,                   // вызов обычной процедуры/метода (параметры: размер стека текущей процедуры, адрес вызываемой процедуры)
        CALL_NEAR,
        CALL_VIRT,                   // вызов виртуального метода (параметры: размер стека текущей процедуры, адрес VMT, индекс метода)
        CALL_INTF,                   // вызов интерфейсного метода (Data1 - ID интервейса, Data2 - ID метода, Data3 - размер стека текущей процедуры)
        CALL_INDIRECT,               // косвенный вызов процедуры, (адрес которой загружен в R0)

        CALL_EXT_FAST,               // оптимизированная версия для наиболее распространенных вызовов
        CALL_EXT_FASTV,              // оптимизированная версия для наиболее распространенных вызовов (виртуальных)
        CALL_EXT_FAST_INTF_PROC,     // оптимизированная версия для наиболее распространенных вызовов (интерфейсных)
        CALL_EXT_FAST_INTF_FUNC,     // оптимизированная версия для наиболее распространенных вызовов (интерфейсных)
        CALL_EXT_COMMON_PROC,        // общая версия для всех возможны натаций и наборов параметров
        CALL_EXT_COMMON_FUNC,        // общая версия для всех возможны натаций и наборов параметров (с результатом)
        //-----------
        VM_SYSMACRO,                 // различные системные вызовы (макроинструкции)

        VM_JMP,
        PROC_RET
        //-----------
  };




    unsafe class VMachine
    {
        [StructLayout(LayoutKind.Sequential, Pack = 1)]
        unsafe struct VMString
        {
            private int RefCnt;
            private int StrLen;
            private fixed char str[128];
            public unsafe VMString(string Str)
            {
                RefCnt = -1;
                StrLen = Str.Length;
                fixed (char* buffer = str)
                {
                    int i = 0;
                    foreach (char chr in Str)
                    {
                        buffer[i] = chr;
                        i++;
                    }
                    buffer[i] = (char)0;
                }
            }
            public unsafe Pointer StrPtr()
            {
                fixed (char* buffer = str) { return (Pointer)buffer; }
            }

            //public unsafe Pointer StrPtr() { fixed (char* buffer = str) { return (Pointer)buffer; }
        }

        private const UInt32 VM_IMG_SIGNATURE_V1 = 0xABCDEF01;
        private const UInt32 IMG_HAS_RTTI = 16;
        private const UInt32 IMG_HAS_DEBUG_INFO = 32;
        private VMString[] cSysTypeNames = {
            new VMString("Int8"),
            new VMString("Int16"),
            new VMString("Int32"),
            new VMString("Int64"),
            new VMString("UInt8"),
            new VMString("UInt16"),
            new VMString("UInt32"),
            new VMString( "UInt64"),
            new VMString("NativeInt"),
            new VMString("NativeUInt"),
            new VMString("Float32"),
            new VMString("Float64"),
            new VMString("Boolean"),
            new VMString("AnsiChar"),
            new VMString("Char"),
            new VMString("AnsiString"),
            new VMString("String"),
            new VMString("Variant"),
            new VMString("Guid"),
            new VMString("Pointer"),
            new VMString("WeakRef"),
            new VMString("Generic"),
            new VMString("Range"),
            new VMString("Enum"),
            new VMString("Set"),
            new VMString("Array"),
            new VMString("DynArray"),
            new VMString("OpenArray"),
            new VMString("ProcType"),
            new VMString("Record"),
            new VMString("Class"),
            new VMString("ClassOf"),
            new VMString("Interface")
        };
        private VMString InitStr = new VMString("$initialization");
        private VMString FinalStr = new VMString("$finalization");

        private static readonly int PTR_SIZE = IntPtr.Size;
        private MemoryStream fMem;  // need pinned !!!
        private TIMGHeader* fHeader;
        private Byte* fImgPtr;
        private Pointer fUnits;
        private int fUnitsCount;
        private int fStackSize;
        private Pointer fStack;          // stack pointer
        private GCHandle fStackGCH;     // stack GC handle
        private GCHandle fImageGCH;     // image GC handle


        private enum TIMGFormat
        {
            IMG_32Bit,     // 32-разрядный образ
            IMG_64Bit      // 64-разрядный образ
        }
        [Conditional("DEBUG")]
        void DebugLog(string Message)
        {
            Console.WriteLine(Message);
        }
        public VMachine(int StackSize = 65535)
        {
            // Stack allocation
            fStackSize = StackSize;
            fStackGCH = GCHandle.Alloc(new Byte[StackSize], GCHandleType.Pinned);
            fStack = (Pointer)fStackGCH.AddrOfPinnedObject();
            DebugLog("Stack Size: " + StackSize);
            DebugLog("Stack Pointer: " + fStack);

            DebugLog("***** system structs sizes *****");
            DebugLog("TIMGHeader: " + sizeof(TIMGHeader));
            DebugLog("TVMUnit: " + sizeof(TVMUnit));
            DebugLog("TVMObjHeader: " + sizeof(TVMObjHeader));
            DebugLog("TRTTI: " + sizeof(TRTTI));
            DebugLog("TRTTIType: " + sizeof(TRTTIType));
            DebugLog("TRTTIOrdinal: " + sizeof(TRTTIOrdinal));
            DebugLog("TRTTIFloat: " + sizeof(TRTTIFloat));
            DebugLog("TRTTIArray: " + sizeof(TRTTIArray));
            DebugLog("TRTTIDynArray: " + sizeof(TRTTIDynArray));
            DebugLog("TRTTIStruct: " + sizeof(TRTTIStruct));
            DebugLog("TRTTIRecord: " + sizeof(TRTTIRecord));
            DebugLog("TRTTIClass: " + sizeof(TRTTIClass));
            DebugLog("TRTTIInterface: " + sizeof(TRTTIInterface));
            DebugLog("***** end *****");
        }

        Boolean Check(uint Value, uint Flag)
        {
            return (Value & Flag) == Flag;
        }
        Pointer GetIMGPtr(TOffset Offset)
        {
            return (Pointer)(fImgPtr + Offset);
        }
        Pointer GetRTTIPtr(TOffset TypeInfoOffset)
        {
            return (Pointer)(fImgPtr + fHeader->RTTIOffset + TypeInfoOffset);
        }
        public void LoadVMImage(string FileName)
        {
            using (TFileSteam Stream = new TFileSteam().Create(FileName))
            {
                DebugLog("File Size: " + Stream.Size);

                // read sign
                UInt32 IMGSign = Stream.ReadUInt32();
                if (IMGSign != VM_IMG_SIGNATURE_V1)
                    throw new Exception("Image signature is invalid");
                DebugLog("IMG Sign: " + IMGSign);
                // read flags
                UInt32 IMGFlags = Stream.ReadUInt32();
                TIMGFormat IMGFormat = (TIMGFormat)(IMGFlags & 3);
                if (IMGFormat != TIMGFormat.IMG_32Bit)
                    throw new Exception("Only 32 bit image supported");
                DebugLog("IMG Flags: " + IMGFlags);
                // read code size
                UInt32 IMGSize = Stream.ReadUInt32();
                DebugLog("IMG Code Size: " + IMGSize);
                // load full image
                fMem = new MemoryStream();
                fMem.SetLength(Stream.Size);
                // pin the image memory stream
                //fImageGCH = GCHandle.Alloc(fMem, GCHandleType.Pinned);
                //IntPtr ImagePtr = fImageGCH.AddrOfPinnedObject();
                //byte* M = (byte*)ImagePtr;

                Stream.Position = 0;
                Stream.Data.CopyTo(fMem);
                DebugLog("Mem Size: " + fMem.Length);
                      
                fixed (byte* M = fMem.GetBuffer())
                {
                    fImgPtr = M;
                    fHeader = (TIMGHeader*)M;

                    // correct offsets and prepare data
                    if (fHeader->ImportTableCount > 0)
                    {
                        fHeader->ImportTable = (NativeUInt)(M + fHeader->ImportTable);
                        MapImportProcedures();
                    }


                    // modules RTTI offsets correction                
                    fHeader->UnitsOffset = (NativeUInt)(M + fHeader->UnitsOffset);
                    fUnitsCount = *(int*)(fHeader->UnitsOffset - PTR_SIZE);

                    if (fUnitsCount <= 0)
                        throw new Exception("Invalid package (Units count = 0)");

                    NativeUInt UnitsPtr = fHeader->UnitsOffset;
                    for (int ui = 0; ui < fUnitsCount; ui++)
                    {
                        *(NativeUInt*)(UnitsPtr) = *(NativeUInt*)(UnitsPtr + PTR_SIZE * ui);
                    }
                    DebugLog("Modules count: " + fUnitsCount);

                    // pointer to the first unit struct in list
                    fUnits = (Pointer)(fHeader->UnitsOffset + fUnitsCount * PTR_SIZE);

                    // коррекция списка всех процедур образа
                    if (fHeader->ProcFramesCount > 0)
                    {
                        fHeader->ProcFramesSection = (NativeUInt)(M + fHeader->ProcFramesSection);
                        Byte* Procs = (Byte*)(fHeader->ProcFramesSection);
                        for (int pi = 0; pi < fHeader->ProcFramesCount; pi++)
                        {
                            TVMIMGProcFrame* ProcEntry = (TVMIMGProcFrame*)(fHeader->ProcFramesSection + pi * sizeof(TVMIMGProcFrame));
                            ProcEntry->ProcAddr = (Pointer)(M + ProcEntry->ProcAddr);
                            if (ProcEntry->ProcInfo > 0)
                                ProcEntry->ProcInfo = (Pointer)(M + fHeader->RTTIOffset + ProcEntry->ProcInfo + sizeof(TVMObjHeader));
                        };
                    };

                    // коррекция RTTI системных типов
                    if (Check(IMGFlags, IMG_HAS_RTTI))
                    {
                        TRTTIType* pType = (TRTTIType*)GetRTTIPtr(4);
                        for (int ti = 0; ti < TSystemTypes.Count; ti++)
                        {
                            pType->Base.Base.TypeInfo = GetRTTIPtr(pType->Base.Base.TypeInfo);
                            pType->Base.Name = cSysTypeNames[(int)pType->DataTypeID].StrPtr();
                            int RTTISize = GetRTTIStructSize(pType->DataTypeID);
                            pType = (TRTTIType*)((NativeUInt)pType + RTTISize);
                        };
                    };

                    // коррекция модулей                
                    for (int ui = 0; ui < fUnitsCount; ui++)
                    {
                        TVMUnit* UN = (TVMUnit*)(fUnits + ui * sizeof(TVMUnit));

                        if (UN->Base.Base.TypeInfo > 0)
                            UN->Base.Base.TypeInfo = GetRTTIPtr(UN->Base.Base.TypeInfo);

                        if (UN->Base.Name > 0)
                            UN->Base.Name = GetIMGPtr(UN->Base.Name);

                        if (UN->FVars > 0)
                            UN->FVars = GetIMGPtr(UN->FVars);

                        if (UN->FExportProcs > 0)
                            UN->FExportProcs = GetRTTIPtr(UN->FExportProcs);

                        // types corrections
                        if (UN->FTypes > 0)
                        {
                            UN->FTypes = GetRTTIPtr(UN->FTypes);

                            NativeUInt* TypesRTTIArray = (NativeUInt*)UN->FTypes;
                            for (int ti = 0; ti < UN->TypesCount; ti++)
                            {
                                TRTTIType* pType = (TRTTIType*)GetRTTIPtr(*(TypesRTTIArray++) - (uint)sizeof(TVMObjHeader));

                                if (Check(IMGFlags, IMG_HAS_RTTI))
                                    pType->Base.Base.TypeInfo = GetRTTIPtr(pType->Base.Base.TypeInfo);

                                if (pType->Base.Name > 0)
                                    pType->Base.Name = GetIMGPtr(pType->Base.Name);

                                if ((pType->DataTypeID == TDataTypeID.dtRecord) |
                                    (pType->DataTypeID == TDataTypeID.dtClass) |
                                    (pType->DataTypeID == TDataTypeID.dtInterface))

                                {
                                    TRTTIStruct* Struct = (TRTTIStruct*)pType;
                                    if (Struct->Ancestor > 0)
                                        Struct->Ancestor = GetRTTIPtr(Struct->Ancestor);
                                }
                            }

                        }

                        // коррекция глобальных переменных
                        for (int vi = 0; vi < UN->VarsCount; vi++)
                        {
                            TVMVariable* pVar = (TVMVariable*)(UN->FVars + vi * sizeof(TVMVariable));
                            if (pVar->Base.Name > 0)
                                pVar->Base.Name = GetIMGPtr(pVar->Base.Name);
                            pVar->Addr = GetIMGPtr(pVar->Addr);
                            pVar->DataType = GetRTTIPtr(pVar->DataType);
                            TRTTIType* VarType = (TRTTIType*)pVar->DataType;
                            // инициализация значений по умолчанию дин. массивов и строк
                            if (
                                    *(NativeUInt*)(pVar->Addr) > 0 &&
                                    (
                                        (VarType->DataTypeID == TDataTypeID.dtAnsiString) ||
                                        (VarType->DataTypeID == TDataTypeID.dtString)
                                    )
                               ) *(NativeUInt*)(pVar->Addr) = GetIMGPtr(*(NativeUInt*)(pVar->Addr));
                        }

                        // подготавливаем процедуры
                        PrepareProcs(UN);

                        // подготавливаем экспортные процедуры
                        PrepareExportProcs(UN);
                    }

                    if (fHeader->FixTable > 0)
                    {
                        fHeader->FixTable = GetIMGPtr(fHeader->FixTable);
                        PrepareFixTable(M);
                    }
                }
            }
        }

        private void PrepareFixTable(byte* BasePtr)
        {
            // get the count of the FIX table
            UInt32* Ptr = (UInt32*)(fHeader->FixTable);
            UInt32 Cnt = *Ptr;
            Ptr++;
            while (Cnt > 0)
            {
                // Get the next fixed memory pointer
                NativeUInt* FixedPtr = (NativeUInt*)GetIMGPtr(*Ptr);
                // Write the correct value to it
                *FixedPtr = GetIMGPtr(*FixedPtr);
                Cnt++;
                Ptr++;
            }
        }

        private void PrepareExportProcs(TVMUnit* UN)
        {
            //throw new NotImplementedException();
        }

        private void PrepareProcs(TVMUnit* UN)
        {
            // коррекция секции initialization
            if (UN->InitProc > 0)
                UN->InitProc = GetIMGPtr(UN->InitProc);

            // коррекция секции finalization
            if (UN->FinalProc > 0)
                UN->FinalProc = GetIMGPtr(UN->FinalProc);

            if (UN->FProcs == 0)
                return;

            UN->FProcs = GetRTTIPtr(UN->FProcs);
            NativeUInt* ProcPtr = (NativeUInt*)UN->FProcs;
            for (int pi = 0; pi < UN->ProcsCount; pi++)
            {
                TRTTIProcedure* Proc = (TRTTIProcedure*)GetRTTIPtr(*ProcPtr);
                // increment Ptr on Object header size
                *ProcPtr = (NativeUInt)Proc + (NativeUInt)sizeof(TVMObjHeader);

                Proc->ADDR = GetIMGPtr(Proc->ADDR);
                if (Proc->Base.Name > 0)
                    Proc->Base.Name = GetIMGPtr(Proc->Base.Name);
                else
                {
                    if (Proc->ADDR == UN->InitProc)
                        Proc->Base.Name = InitStr.StrPtr();
                    else
                    if (Proc->ADDR == UN->FinalProc)
                        Proc->Base.Name = FinalStr.StrPtr();
                }
                // коррекция локальных переменных
                PrepareProcLocalVars(Proc);

                ProcPtr++;
            }
        }

        public void Run()
        {
            RunInitSections(fStack);
            RunFinalSections(fStack);
        }

        private void RunFinalSections(Pointer Stack)
        {
            //throw new NotImplementedException();
        }

        private void RunInitSections(Pointer Stack)
        {
            *(NativeUInt*)(Stack) = 0; // адрес возврата
            Stack++;
            // местро в стеке под результат
            *(NativeUInt*)(Stack) = 0;


            for (int i = fUnitsCount - 1; i > 0; i--)
            {
                TVMUnit* UN = (TVMUnit*)(fUnits + i * sizeof(TVMUnit));
                NativeUInt* P = (NativeUInt*)UN->InitProc;
                if (P == null)
                    continue;
                CallILProc(P, Stack, (TOffset)fImgPtr);
            }
        }
        [StructLayout(LayoutKind.Explicit)]
        private struct TVMReg
        {
            [FieldOffset(0)]
            public Int64 I64;
            [FieldOffset(0)]
            public UInt64 U64;
            [FieldOffset(0)]
            public Int32 I32;
            [FieldOffset(0)]
            public UInt32 U32;
            [FieldOffset(0)]
            public Int16 I16;
            [FieldOffset(0)]
            public UInt16 U16;
            [FieldOffset(0)]
            public SByte I8;
            [FieldOffset(0)]
            public Byte U8;
            [FieldOffset(0)]
            public Double F64;
            [FieldOffset(0)]
            public Pointer PTR;
            [FieldOffset(0)]
            public NativeUInt NUInt;
            [FieldOffset(0)]
            public NativeInt NInt;
            [FieldOffset(0)]
            public Pointer PVrnt;
            [FieldOffset(0)]
            public Boolean Bool;
            [FieldOffset(0)]
            public Char UChar;
            [FieldOffset(0)]
            public Byte AChar;
        }

        private struct TVMRegisters
        {
            public fixed Int64 Data[16];
        }


        //=============================================================================================|
        //                                   ФОРМАТ КОМАНДЫ (32 bit)                                   |
        //------------------------|-----------------------|-----------------------|--------------------|
        //|31|30|29|28|27|26|25|24|23|22|21|20|19|18|17|16|15|14|13|12|11|10|09|08|07|06|05|04|03|02|00|
        //------------------------|-----------------------|-----------------------|--------------------|
        // RR PC PC PC PC PC PC PC DR DR DR DR S1 S1 S1 S1|S2 S2 S2 S2 CC CC CC CC|IC IC IC IC IC IC IC|
        //------------------------|-----------------------|-----------------------|--------------------|
        //
        // IC - код инсрукци (8 бит)
        // СC - код условия исполнения (4 бит)
        // PC - кол-во дополнительных слов инструкции (8 бит)
        // DR - Destination Register - регистр приемник (4 бит)
        // S1 - Source Register 1 - регистр источник (4 бит)
        // S2 - Source Register 2 - регистр источник (4 бит)
        // PC - кол-во слов данных инструкции (7 бит)
        // RR - зарезервировано транслятором (1 бит)
        //=============================================================================================|
        const uint cNoneBit = 1 << (int)TILCondition.cNone;
        const uint cEqualBit = 1 << (int)TILCondition.cEqual;
        const uint cNotEqualBit = 1 << (int)TILCondition.cNotEqual;
        const uint cGreaterBit = 1 << (int)TILCondition.cGreater;
        const uint cGreaterOrEqualBit = 1 << (int)TILCondition.cGreaterOrEqual;
        const uint cLessBit = 1 << (int)TILCondition.cLess;
        const uint cLessOrEqualBit = 1 << (int)TILCondition.cLessOrEqual;
        const uint cZeroBit = 1 << (int)TILCondition.cZero;
        const uint cNonZeroBit = 1 << (int)TILCondition.cNonZero;


        private void SetCondition(Int32 Result, ref uint Flags)
        {
            if (Result == 0)
                Flags = cEqualBit | cGreaterOrEqualBit | cLessOrEqualBit | cZeroBit;
            else
            {
                if (Result > 0)
                    Flags = cGreaterBit | cGreaterOrEqualBit | cNotEqualBit | cNonZeroBit;
                else
                    Flags = cLessBit | cLessOrEqualBit | cNotEqualBit | cNonZeroBit;
            }    
        }

        private void SetCondition(Int64 Result, ref uint Flags)
        {
            if (Result == 0)
                Flags = cEqualBit | cGreaterOrEqualBit | cLessOrEqualBit | cZeroBit;
            else
            {
                if (Result > 0)
                    Flags = cGreaterBit | cGreaterOrEqualBit | cNotEqualBit | cNonZeroBit;
                else
                    Flags = cLessBit | cLessOrEqualBit | cNotEqualBit | cNonZeroBit;
            }
        }

        private void SetCondition(Float64 Result, ref uint Flags)
        {
            if (Result == 0)
                Flags = cEqualBit | cGreaterOrEqualBit | cLessOrEqualBit | cZeroBit;
            else
            {
                if (Result > 0)
                    Flags = cGreaterBit | cGreaterOrEqualBit | cNotEqualBit | cNonZeroBit;
                else
                    Flags = cLessBit | cLessOrEqualBit | cNotEqualBit | cNonZeroBit;
            }
        }

        private unsafe void CallILProc(NativeUInt* MP, /*указатель на код процедры*/
                                TOffset SP, /*Указатель на стек процедуры*/
                                TOffset GP  /*указатель на память глобальных переменных*/)
        {
            TVMRegisters Registers;            
            uint Flags = 0;
            uint StackSize = 0;
            while (true)
            {
                NativeUInt RawInstr = *MP;               
                TILCondition Cond = (TILCondition)((Byte) (RawInstr >> 8) & 15);
                TVMCode Code = (TVMCode)(RawInstr & 255);

                uint Idx = (RawInstr >> 20) & 15;
                TVMReg* Dst =  (TVMReg*)&(Registers.Data[Idx]);

                Idx = (RawInstr >> 16) & 15;
                TVMReg* Src1 = (TVMReg*)&(Registers.Data[Idx]);

                Idx = (RawInstr >> 12) & 15;
                TVMReg* Src2 = (TVMReg*)&(Registers.Data[Idx]);

                MP++;
                NativeUInt* PR = MP;
                MP++;
                if ((Cond == TILCondition.cNone) || (((uint)Cond & Flags) != 0)) 
                switch (Code)
                    {
                        case TVMCode.VM_NOPE: continue;
                        case TVMCode.VM_STACK: StackSize = *PR; break;
                        //==================================================================================================
                        // LOAD INSTRUCTIONS
                        //==================================================================================================
                        case TVMCode.LD_C_I32: Dst->I64 = (Int32)(*PR); break;
                        case TVMCode.LD_C_U32: Dst->U64 = (UInt32)(*PR); break;
                        case TVMCode.LD_C_I64: Dst->I64 = *(Int64*)PR; break;
                        case TVMCode.LD_C_F32: Dst->F64 = *(Single*)PR; break;
                        case TVMCode.LD_C_F64: Dst->F64 = *(Double*)PR; break;
                        case TVMCode.LD_C_ZERO: Dst->I64 = 0; break;
                        case TVMCode.LD_C_ONE: Dst->I64 = 1; break;
                        //==================================================================================================
                        case TVMCode.LD_L_I8: Dst->I64 = *(Int8*)(SP + *PR); break;
                        case TVMCode.LD_L_U8: Dst->U64 = *(UInt8*)(SP + *PR); break;
                        case TVMCode.LD_L_I16: Dst->I64 = *(Int16*)(SP + *PR); break;
                        case TVMCode.LD_L_U16: Dst->U64 = *(UInt16*)(SP + *PR); break;
                        case TVMCode.LD_L_I32: Dst->I64 = *(Int32*)(SP + *PR); break;
                        case TVMCode.LD_L_I64: Dst->I64 = *(Int64*)(SP + *PR); break;
                        case TVMCode.LD_L_F32: Dst->F64 = *(Float32*)(SP + *PR); break;
                        case TVMCode.LD_L_PTR: Dst->PTR = (SP + *PR); break;
                        //==================================================================================================
                        case TVMCode.LD_R_I8: Dst->I64 = *(Int8*)(*(IntPtr*)(SP + *PR)); break;
                        case TVMCode.LD_R_U8: Dst->U64 = *(UInt8*)(*(IntPtr*)(SP + *PR)); break;
                        case TVMCode.LD_R_I16: Dst->I64 = *(Int16*)(*(IntPtr*)(SP + *PR)); break;
                        case TVMCode.LD_R_U16: Dst->U64 = *(UInt16*)(*(IntPtr*)(SP + *PR)); break;
                        case TVMCode.LD_R_I32: Dst->I64 = *(Int32*)(*(IntPtr*)(SP + *PR)); break;
                        case TVMCode.LD_R_I64: Dst->I64 = *(Int64*)(*(IntPtr*)(SP + *PR)); break;
                        case TVMCode.LD_R_F32: Dst->F64 = *(Float32*)(*(IntPtr*)(SP + *PR)); break;
                        //==================================================================================================
                        case TVMCode.LD_G_I8: Dst->I64 = *(Int8*)(GP + *PR); break;
                        case TVMCode.LD_G_U8: Dst->U64 = *(UInt8*)(GP + *PR); break;
                        case TVMCode.LD_G_I16: Dst->I64 = *(Int16*)(GP + *PR); break;
                        case TVMCode.LD_G_U16: Dst->U64 = *(UInt16*)(GP + *PR); break;
                        case TVMCode.LD_G_I32: Dst->I64 = *(Int32*)(GP + *PR); break;
                        case TVMCode.LD_G_I64: Dst->I64 = *(Int64*)(GP + *PR); break;
                        case TVMCode.LD_G_F32: Dst->F64 = *(Float32*)(GP + *PR); break;
                        case TVMCode.LD_G_PTR:
                        case TVMCode.LD_G_PROC: Dst->PTR = (GP + *PR); break;
                        //==================================================================================================
                        // load value by addr in Src1 + [imm offset]
                        case TVMCode.LD_D_I8: if (Idx > 0) Dst->I32 = *(Int8*)(Src1->PTR + *PR); else Dst->I32 = *(Int8*)(Src1->PTR); break;
                        case TVMCode.LD_D_U8: if (Idx > 0) Dst->U32 = *(UInt8*)(Src1->PTR + *PR); else Dst->U32 = *(UInt8*)(Src1->PTR); break;
                        case TVMCode.LD_D_I16: if (Idx > 0) Dst->I32 = *(Int16*)(Src1->PTR + *PR); else Dst->I32 = *(Int16*)(Src1->PTR); break;
                        case TVMCode.LD_D_U16: if (Idx > 0) Dst->U32 = *(UInt16*)(Src1->PTR + *PR); else Dst->U32 = *(UInt16*)(Src1->PTR); break;
                        case TVMCode.LD_D_I32: if (Idx > 0) Dst->I32 = *(Int32*)(Src1->PTR + *PR); else Dst->I32 = *(Int32*)(Src1->PTR); break;
                        case TVMCode.LD_D_I64: if (Idx > 0) Dst->I64 = *(Int64*)(Src1->PTR + *PR); else Dst->I64 = *(Int64*)(Src1->PTR); break;
                        case TVMCode.LD_D_F32: if (Idx > 0) Dst->F64 = *(Float32*)(Src1->PTR + *PR); else Dst->F64 = *(Float32*)(Src1->PTR); break;

                        //==================================================================================================
                        // STORE INSTRUCTIONS
                        //==================================================================================================
                        case TVMCode.MOVE_L_I32: *(Int32*)(SP + *PR) = *(Int32*)(SP + *(NativeUInt*)(PR + PTR_SIZE)); break;
                        case TVMCode.ST_C_I32: *(Int32*)(SP + *PR) = *(Int32*)(PR + PTR_SIZE); break;
                        case TVMCode.ST_L_I8: *(Int8*)(SP + *PR) = Src1->I8; break;
                        case TVMCode.ST_L_I16: *(Int16*)(SP + *PR) = Src1->I16; break;
                        case TVMCode.ST_L_I32: *(Int32*)(SP + *PR) = Src1->I32; break;
                        case TVMCode.ST_L_I64: *(Int64*)(SP + *PR) = Src1->I64; break;
                        case TVMCode.ST_L_F32: *(Float32*)(SP + *PR) = (Float32)(Src1->F64); break;
                        case TVMCode.ST_L_VAR: _VM_VARIANT_ASSIGN(SP + *PR, Src1->PVrnt); break;
                        //==================================================================================================
                        case TVMCode.ST_R_I8: *(Int8*)(*(IntPtr*)(SP + *PR)) = Src1->I8; break;
                        case TVMCode.ST_R_I16: *(Int16*)(*(IntPtr*)(SP + *PR)) = Src1->I16; break;
                        case TVMCode.ST_R_I32: *(Int32*)(*(IntPtr*)(SP + *PR)) = Src1->I32; break;
                        case TVMCode.ST_R_I64: *(Int64*)(*(IntPtr*)(SP + *PR)) = Src1->I64; break;
                        case TVMCode.ST_R_F32: *(Float32*)(*(IntPtr*)(SP + *PR)) = (Float32)Src1->F64; break;
                        //==================================================================================================
                        case TVMCode.ST_G_I8: *(Int8*)(GP + *PR) = Src1->I8; break;
                        case TVMCode.ST_G_I16: *(Int16*)(GP + *PR) = Src1->I16; break;
                        case TVMCode.ST_G_I32: *(Int32*)(GP + *PR) = Src1->I32; break;
                        case TVMCode.ST_G_I64: *(Int64*)(GP + *PR) = Src1->I64; break;
                        case TVMCode.ST_G_F32: *(Float32*)(GP + *PR) = (Float32)Src1->F64; break;
                        case TVMCode.ST_G_VAR: _VM_VARIANT_ASSIGN(GP + *PR, Src1->PVrnt); break;
                        //==================================================================================================
                        case TVMCode.ST_D_I8: if (Idx > 0) *(Int8*)(*(IntPtr*)(Dst->PTR + *PR)) = Src1->I8; else *(Int8*)(Dst->PTR) = Src1->I8; break;
                        case TVMCode.ST_D_I16: if (Idx > 0) *(Int16*)(*(IntPtr*)(Dst->PTR + *PR)) = Src1->I16; else *(Int16*)(Dst->PTR) = Src1->I16; break;
                        case TVMCode.ST_D_I32: if (Idx > 0) *(Int32*)(*(IntPtr*)(Dst->PTR + *PR)) = Src1->I32; else *(Int32*)(Dst->PTR) = Src1->I32; break;
                        case TVMCode.ST_D_I64: if (Idx > 0) *(Int64*)(*(IntPtr*)(Dst->PTR + *PR)) = Src1->I64; else *(Int64*)(Dst->PTR) = Src1->I64; break;
                        case TVMCode.ST_D_F32: if (Idx > 0) *(Float32*)(*(IntPtr*)(Dst->PTR + *PR)) = (Float32)Src1->F64; else *(Float32*)(Dst->PTR) = (Float32)Src1->F64; break;

                        case TVMCode.MOVE_REG: Dst->U64 = Src1->U64; break;
                        //case TVMCode.MOVE_REG_TO_MEM: _VM_MOVE(Src1->U64, Dst->PTR ^, PR ^); break; // todo
                        //==================================================================================================
                        // CLEAR
                        //==================================================================================================
                        case TVMCode.CLR_L_I8: *(Int8*)(SP + *PR) = 0; break;
                        case TVMCode.CLR_L_I16: *(Int16*)(SP + *PR) = 0; break;
                        case TVMCode.CLR_L_I32: *(Int32*)(SP + *PR) = 0; break;
                        case TVMCode.CLR_L_I64: *(Int64*)(SP + *PR) = 0; break;
                        case TVMCode.CLR_L_F32: *(Float32*)(SP + *PR) = 0; break;
                        case TVMCode.CLR_L_F64: *(Float64*)(SP + *PR) = 0; break;
                        //==================================================================================================
                        case TVMCode.CLR_R_I8: *(Int8*)(*(IntPtr*)(SP + *PR)) = 0; break;
                        case TVMCode.CLR_R_I16: *(Int16*)(*(IntPtr*)(SP + *PR)) = 0; break;
                        case TVMCode.CLR_R_I32: *(Int32*)(*(IntPtr*)(SP + *PR)) = 0; break;
                        case TVMCode.CLR_R_I64: *(Int64*)(*(IntPtr*)(SP + *PR)) = 0; break;
                        case TVMCode.CLR_R_F32: *(Float32*)(*(IntPtr*)(SP + *PR)) = 0; break;
                        case TVMCode.CLR_R_F64: *(Float64*)(*(IntPtr*)(SP + *PR)) = 0; break;
                        //==================================================================================================
                        case TVMCode.CLR_G_I8: *(Int8*)(GP + *PR)  = 0; break;
                        case TVMCode.CLR_G_I16: *(Int16*)(GP + *PR) = 0; break;
                        case TVMCode.CLR_G_I32: *(Int32*)(GP + *PR) = 0; break;
                        case TVMCode.CLR_G_I64: *(Int64*)(GP + *PR) = 0; break;
                        case TVMCode.CLR_G_F32: *(Float32*)(GP + *PR) = 0; break;
                        case TVMCode.CLR_G_F64: *(Float64*)(GP + *PR) = 0; break;
                        //==================================================================================================
                        case TVMCode.CLR_D_I8: if (Idx > 0) *(Int8*)(*(IntPtr*)(Dst->PTR + PR)) = 0; else *(Int8*)(Dst->PTR) = 0; break;
                        case TVMCode.CLR_D_I16: if (Idx > 0) *(Int16*)(*(IntPtr*)(Dst->PTR + PR)) = 0; else *(Int16*)(Dst->PTR) = 0; break;
                        case TVMCode.CLR_D_I32: if (Idx > 0) *(Int32*)(*(IntPtr*)(Dst->PTR + PR)) = 0; else *(Int32*)(Dst->PTR) = 0; break;
                        case TVMCode.CLR_D_I64: if (Idx > 0) *(Int64*)(*(IntPtr*)(Dst->PTR + PR)) = 0; else *(Int64*)(Dst->PTR) = 0; break;
                        case TVMCode.CLR_D_F32: if (Idx > 0) *(Float32*)(*(IntPtr*)(Dst->PTR + PR)) = 0; else *(Float32*)(Dst->PTR) = 0; break;
                        case TVMCode.CLR_D_F64: if (Idx > 0) *(Float64*)(*(IntPtr*)(Dst->PTR + PR)) = 0; else *(Float64*)(Dst->PTR) = 0; break;
                        //==================================================================================================
                        //==================================================================================================
                        // CMP
                        //==================================================================================================
                        case TVMCode.CMP_I32_C: SetCondition(Dst->I32 - (Int32)(*PR), ref Flags); break;
                        case TVMCode.CMP_L_C32: SetCondition(*(Int32*)(SP + *PR) -(Int32)(*(NativeUInt*)(PR + PTR_SIZE)), ref Flags); break;
                        case TVMCode.CMP_I32: SetCondition(Dst->I32 - Src1->I32, ref Flags); break;
                        case TVMCode.CMP_L_I32: SetCondition(*(Int32*)(SP + *PR) - *(Int32*)(SP + *(NativeUInt*)(PR + PTR_SIZE)), ref Flags); break;
                        case TVMCode.CMP_I64: SetCondition(Dst->I64 - Src1->I64, ref Flags); break;
                        case TVMCode.CMP_F64: SetCondition(Dst->F64 - Src1->F64, ref Flags); break;
                        //case TVMCode.CMP_ASTR: SetCondition(_VM_STRA_COMPARE(Dst, Src1), Flags); break;
                        //case TVMCode.CMP_USTR: SetCondition(_VM_STRU_COMPARE(Dst, Src1), Flags); break;
                        //case TVMCode.CMP_VAR: SetCondition(TVMVariant.Compare(Dst->PVrnt ^, Src1->PVrnt ^), Flags); break;
                        case TVMCode.CMP_TEST32: SetCondition(Src1->I32 & Src2->I32, ref Flags); break;
                        case TVMCode.CMP_TEST64: SetCondition(Src1->U64 & Src2->U64, ref Flags); break;
                        //case TVMCode.CMP_MEM_VS_REG: _VM_MEM_COMPARE(Dst->PTR, Src1, *PR, Flags); break;
                        //case TVMCode.CMP_MEM: _VM_MEM_COMPARE(Dst->PTR, Src1->PTR, *PR, Flags); break;
                        //==================================================================================================
                        case TVMCode.PROC_RET: return;
                        default: throw new Exception("Unknown instruction: " + Code);                        
                }
            }
        }


        private void _VM_VARIANT_ASSIGN(Pointer Dst, Pointer Src)
        {
            // todo 
        }

        private void PrepareProcLocalVars(TRTTIProcedure* Proc)
        {
            if (Proc->LocalVars == 0)
                return;
            Proc->LocalVars = GetRTTIPtr(Proc->LocalVars);
        }

        private void MapImportProcedures()
        {

        }
        int GetRTTIStructSize(TDataTypeID DataTypeID)
        {
            switch (DataTypeID)
            {
                case TDataTypeID.dtInt8:
                case TDataTypeID.dtInt16:
                case TDataTypeID.dtInt32:
                case TDataTypeID.dtInt64:
                case TDataTypeID.dtUInt8:
                case TDataTypeID.dtUInt16:
                case TDataTypeID.dtUInt32:
                case TDataTypeID.dtUInt64:
                case TDataTypeID.dtNativeInt:
                case TDataTypeID.dtNativeUInt:
                case TDataTypeID.dtBoolean: 
                case TDataTypeID.dtAnsiChar:
                case TDataTypeID.dtChar: return sizeof(TRTTIOrdinal);

                case TDataTypeID.dtEnum: return sizeof(TRTTIOrdinal);

                case TDataTypeID.dtRange: return sizeof(TRTTIOrdinal);
                case TDataTypeID.dtSet: return sizeof(TRTTIOrdinal);
                case TDataTypeID.dtStaticArray: return sizeof(TRTTIArray);
                case TDataTypeID.dtDynArray: 
                case TDataTypeID.dtOpenArray: return sizeof(TRTTIDynArray);

                case TDataTypeID.dtPointer: return sizeof(TRTTIPointer);
                case TDataTypeID.dtWeakRef: return sizeof(TRTTIPointer);
                case TDataTypeID.dtRecord: return sizeof(TRTTIRecord);
                case TDataTypeID.dtClass: return sizeof(TRTTIClass);
                case TDataTypeID.dtClassOf: return sizeof(TRTTIPointer);
                case TDataTypeID.dtProcType: return sizeof(TRTTIProcType);
                case TDataTypeID.dtInterface: return sizeof(TRTTIInterface);
                case TDataTypeID.dtFloat32:
                case TDataTypeID.dtFloat64: return sizeof(TRTTIFloat);
                case TDataTypeID.dtAnsiString:
                case TDataTypeID.dtString: return sizeof(TRTTIArray);
                case TDataTypeID.dtGuid: return sizeof(TRTTIStruct);
                case TDataTypeID.dtVariant: return sizeof(TRTTIType);
                default: return 0;
                    //raise Exception.CreateFmt('Unknown data type ID: %d', [Ord(DataTypeID)]);
        }
        }
    }
}