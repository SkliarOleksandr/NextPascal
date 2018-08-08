//---------------------------------------------------------------------------

#ifndef ILMachineH
#define ILMachineH

#include <stdint.h>
#include "CRUtils.h"
#include "CRStreams.h"
#include "ILTypeInfo.h"
#include "ILMachineTypes.h"
#include <locale>

// #define USE_EXCEPTIONS

namespace VM{

#ifdef __GNUG__
size_t strnlen_s(const char* str, size_t len);
#endif

CRBase::String IntToHex(Int32 i, size_t);
CRBase::String str_to_wstring(const char* str);
CRBase::String str_to_wstring(const std::string& in, std::locale loc = std::locale());

  enum TILMachineCode: unsigned char
  {
	VM_NOPE,
	VM_STACK,        // [аргументы: const: Int32 StackSize]
	//-----------
	LD_C_I32,        // [аргументы: сonst: Int32]
	LD_C_U32,        // [аргументы: сonst: Int32]
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
	LD_G_PROC,      // загрузка в Dst адреса процедуры указанной в памяти
	//-----------
	LD_D_I8,      // Dst.I8  = (Src1.PTR + offset)^
	LD_D_U8,      // Dst.U8  = (Src1.PTR + offset)^
	LD_D_I16,     // Dst.I16 = (Src1.PTR + offset)^
	LD_D_U16,     // Dst.U16 = (Src1.PTR + offset)^
	LD_D_I32,     // Dst.I16 = (Src1.PTR + offset)^
	LD_D_I64,     // Dst.I64 = (Src1.PTR + offset)^
	LD_D_F32,     // Dst.F64 = (Src1.PTR + offset)^
	//-----------
	ST_REG_ZERO,      // RO := 0; [аргументы: нет]
	ST_REG_ONE,       // R0 := 1; [аргументы: нет]
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
	FMA_U32,        // Dst.U32 := Src1.U32 + Src2.U32 * Data1
	FMA_U64,        // Dst.U64 := Src1.U64 + Src2.U64 * Data1
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

	#ifdef CPUX64
	STR_INCREF,
	STR_LENGTH,
	STR_MOVE,
	#endif

	ARRAY_LENGTH,                // вычислят длинну дин. массива/строки
	ARRAY_INCREF,                // инкремент счетчика дин. массива/строки
	ARRAY_DECREF,                // декремент счетчика дин. массива
	ARRAY_MEM_ALLOC,             // выделяет память под дин. массив (Dst - массив, Src1 - кол-во элементов, Data1 - размер элемента)
	ARRAY_INIT,                  // инициализирует внутреннюю структуру дин. массива

	VIRT_METHOD_PTR,             // инструкция вычисляет адрес виртуального метода  R0 - self, R1 - VMT offset
	VM_SET_METHOD_PTR,           // инструкция записывает адрес метода. [Dst - адрес TMethod, Src1 - Self, Src2 - Адерс процедуры]

	MEM_ALLOC,                   // выделение памяти заданного размера
	MEM__FREE,                   // освобождение памяти
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
	CNV_UCHR_TO_USTR,         // конвертирует unicode-символ в unicode-строку
	CNV_ACHR_TO_ASTR,         // конвертирует ansi-символ в ansi-строку

	CNV_ASTR_TO_USTR,            // конвертирует ansi-строку в unicode-строку
	CNV_USTR_TO_ASTR,            // конвертирует unicode-строку в ansi-строку
	//-----------
	CNV_VAR_TO_VALUE,         // конвертирует variant в заданный тип (Dst = Src1, CONST Int32 - dst data type)
	CNV_VALUE_TO_VAR,         // конвертирует заданный тип в variant (Dst = Src1, CONST Int32 - src data type)
	VAR_RELEASE,              // финализация варианта
	//-----------

	CALL_PROC,                // вызов обычной процедуры/метода (параметры: размер стека текущей процедуры, адрес вызываемой процедуры)
	CALL_NEAR,
	CALL_VIRT,                // вызов виртуального метода (параметры: размер стека текущей процедуры, адрес VMT, индекс метода)
	CALL_INTF,                // вызов интерфейсного метода (Data1 - ID интервейса, Data2 - ID метода, Data3 - размер стека текущей процедуры)
	CALL_INDIRECT,            // косвенный вызов процедуры, (адрес которой загружен в R0)

	CALL_EXT_FAST,            // оптимизированная версия для наиболее распространенных вызовов
	CALL_EXT_FASTV,           // оптимизированная версия для наиболее распространенных вызовов (виртуальных)
	CALL_EXT_FAST_INTF_PROC,  // оптимизированная версия для наиболее распространенных вызовов (интерфейсных)
	CALL_EXT_FAST_INTF_FUNC,  // оптимизированная версия для наиболее распространенных вызовов (интерфейсных)
	CALL_EXT_COMMON_PROC,     // общая версия для всех возможны натаций и наборов параметров
	CALL_EXT_COMMON_FUNC,     // общая версия для всех возможны натаций и наборов параметров (с результатом)
	//-----------
	VM_SYSMACRO,                 // различные системные вызовы (макроинструкции)

	VM_JMP,
	PROC_RET
  };

  struct TVMVariant {

  };
  typedef TVMVariant* PVMVariant;

  union TVMReg {
    Int64 I64;
    UInt64 U64;
	Int32 I32;
	UInt32 U32;
	Int16 I16;
	UInt16 U16;
	Int8 I8;
    UInt8 U8;
    Float64 F64;
	Pointer PTR;
	NativeUInt NUInt;
	NativeInt NInt;
	PVMVariant PVrnt;
  };
  typedef TVMReg* PVMReg;

  typedef TVMReg TVMRegisters[16];
  typedef TVMRegisters* PVMRegisters;

  enum TVMError {
	VM_OK,
	VM_INVALID_PARAM,
	VM_PREPARE_ERROR,
	VM_RUN_ASSERT,
	VM_RUN_DIV_BY_ZERO,
	VM_RUN_STACK_OVERFLOW,
	VM_RUN_RANGE_CHECK_ERROR,
	VM_RUN_INVALID_INSTRUCTION
  };

  class TILMachine {
  private:
	TILMemoryStream* FMem;
	PIMGHeader FHeader;
	Int32 FUnitsCount;
	PVMUnits FUnits;
	UInt32 FStackSize;      // размер стека машины
	PByte FStack;
	Boolean FFreeStackWhenDestroy;
    PByte IMGPtr() const;   // возвращает указатель на начало образа в памяти
  protected:
	TVMError CallILProc(PNativeUInt M, /* указатель на код процедры*/
						PByte SP, /* Указатель на стек процедуры */
				     	PByte PS /* указатель на память глобальных переменных*/);
	PILTypeInfo GetTypeInfo(TOffset TypeInfoOffset) const;
	PILTypeInfo GetRTTIPtr(TOffset TypeInfoOffset) const;
	PByte GetIMGPtr(TOffset Offset) const;
	CRBase::String ReadPointer(PByte& P, PRTTIPointer TypeInfo) const;
	CRBase::String ReadClass(PByte& P, PRTTIClass TypeInfo) const;
	CRBase::String ReadIntf(PByte& P, PRTTIInterface TypeInfo) const;
	CRBase::String ReadProcType(PByte& P, PRTTIProcType TypeInfo) const;
	CRBase::String ReadArray(PByte& P, UInt32 DimIndex, PRTTIArray TypeInfo) const;
	CRBase::String ReadDynArray(PByte& P, UInt32 DimIndex, PRTTIArray TypeInfo) const;
	CRBase::String ReadSimple(PByte& P, PILTypeInfo TypeInfo, int Align = 1) const;
	CRBase::String DoReadRecord(PByte& P, PRTTIRecord TypeInfo) const;
	CRBase::String ReadRecord(PByte& P, PRTTIRecord TypeInfo) const;
	CRBase::String ReadSet(PByte& P, PRTTISet TypeInfo) const;
	CRBase::String ReadVarinat(PByte& P, PRTTIVariant TypeInfo) const;
	CRBase::String ReadValue(PByte& P, PILTypeInfo TypeInfo, int Align = 1) const;
	void MapImportProcedures();
	Pointer _VM_OBJ_CREATE(PNativeUInt M);
	void _VM_OBJ_DECREF(Pointer PTR, int StackSize, PByte GP, PByte SP, PNativeUInt PR);
    void _VM_ARRAY_DECREF_FINAL(PVMReg Dst, int StackSize, PByte GP, PByte SP, PNativeUInt PR);
	void _VM_SYSMACRO(const TVMMacroID MacroID, PVMReg Dst, PVMReg Src1, PVMReg Src2);
  public:
	TVMError RunInitSections(PByte Stack = nullptr);
	TVMError RunFinalSections(PByte Stack = nullptr);
	TVMError Run();
	TVMError Run(PByte Stack, UInt32 StackSize);
	TVMError RunProc(PVMProc Proc);
	TVMError RunProc(PVMProc Proc, Pointer Param0);
	TVMError RunFunc(PVMProc Proc, Pointer Param0, NativeUInt &Result);
	TVMError LoadVMImage(TStream* Stream);
    TRTTICharset GetRTTICharset() const;
    CRBase::String GetUnitName(PVMUnit pUnit) const;
    CRBase::String GetProcName(PVMProc pProc) const;
	CRBase::String GetVarName(PVMVariable pVar) const;
    CRBase::String GetTypeName(PILTypeInfo pType) const;
    CRBase::String GetString(Pointer Ptr) const;

	inline void SetVarAsPointer(const PVMVariable Var, const Pointer Value);
	inline void SetVarAsFloat32(const PVMVariable Var, const Float32 Value);
	inline void SetVarAsFloat64(const PVMVariable Var, const Float64 Value);
	inline void SetVarAsInt64(const PVMVariable Var, const Int64 Value);
	inline void SetVarAsInt32(const PVMVariable Var, const Int32 Value);
	inline void SetVarAsInt16(const PVMVariable Var, const Int16 Value);
	inline void SetVarAsInt8(const PVMVariable Var, const Int8 Value);
    inline void SetVarAsBool(const PVMVariable Var, const bool Value);

	Int32 UnitsCount()
	{
		return FUnitsCount;
	}
	PVMUnits Units()
	{
		return FUnits;
	}
	CRBase::String ReadVarValue(const PVMVariable Variable) const;
	PILTypeInfo FindType(const CRBase::String& TypeName, const CRBase::String& UnitName = EMPTY_STR) const;
	PVMProc FindProc(const CRBase::String& ProcName, const CRBase::String& UnitName = EMPTY_STR) const;
	PVMProc FindMethod(const CRBase::String& TypeName, const CRBase::String& ProcName, const CRBase::String& UnitName = EMPTY_STR) const;
	PVMVariable FindVar(const CRBase::String& VarName, const CRBase::String& UnitName = EMPTY_STR) const;
	TILMachine(PByte Stack, UInt32 StackSize);
	TILMachine(UInt32 StackSize);
	~TILMachine()
	{
	  if (FFreeStackWhenDestroy)
	    delete FStack;
	  delete FMem;
	}
  };

Pointer VMAllocMem(size_t Size);
void VMFreeMem(Pointer P);

}

//---------------------------------------------------------------------------
#endif


