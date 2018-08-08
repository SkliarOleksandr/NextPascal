unit Platform.ARM32Instructions;

interface

uses SysUtils, Classes, CompilerUtils, HWPlatforms, CompilerClasses;

type
  TARM32_REGISTER = (
    ARM32_R0,
    ARM32_R1,
    ARM32_R2,
    ARM32_R3,
    ARM32_R4,
    ARM32_R5,
    ARM32_R6,
    ARM32_R7,
    ARM32_R8,
    ARM32_R9,
    ARM32_R10,
    ARM32_R11,
    ARM32_R12,
    ARM32_R13,
    ARM32_R14,
    ARM32_R15
  );

  TARM32_INSTRUCTION = (
    ARM32_MOV,
    ARM32_ADD,
    ARM32_SUB,
    ARM32_RSB,
    ARM32_ORR,
    ARM32_MUL,
    ARM32_TEQ,
    ARM32_LDR,
    ARM32_STR
  );

  TARMCondition = (
    cEqual                = $0, // Z set
    cNotEqual             = $1, // Z clear
    cCarrySet             = $2, // C set
    cCarryClear           = $3, // C clear
    cMinus                = $4, // N set
    cPlus                 = $5, // N clear
    cOverflow             = $6, // V set
    cNoOverflow           = $7, // V clear C set and Z clear
    cUnsignedHigher       = $8, // C clear or Z set
    cUnsignedLowerOrSame  = $9, // C clear or Z set
    cSignedGreaterOrEqual = $A, // N == V
    cSignedLessThen       = $B, // N != V
    cSignedGreaterThen    = $C, // Z == 0,N == V
    cSignedLessOrEqual    = $D, // Z == 1 or N != V
    cAlways               = $E, // условия нет
    cInvlaid              = $F  // неверное условие
  );


  TARMArgumentType = (
    ArgNone   = -1,
    ArgVar    = 0,
    ArgProc   = 1,
    ArgType   = 2,
    ArgTConst = 3,
    ArgLabel,            // Метка
    ArgFarPtr,           // дальний указатель                                - XXXX:XXXX
    ArgImmValue,         // Непосредственная (константа)	                   - mov eax, 1234567h
    ArgRegister,         // Регистровая	                                     - mov eax, ecx
    ArgMemImm,           // Прямая (абсолютная)	                             - mov eax, [3456789h]
    ArgMemRegister,      // Регистровая косвенная	                           - mov eax, [ecx]
    ArgMemRegDisp,       // Регистровая косвенная со смещением	             - mov eax, [ecx+1200h]
    ArgMemRegReg,        // Базово-индексная	                               - mov eax, [ecx+edx]
    ArgMemRegRegDisp,    // Базово-индексная со смещением	                   - mov eax, [ecx+edx+40h]
    ArgMemRegMulDisp,    // Индексная с масштабированием и смещением	       - mov eax, [esi*4+40h]
    ArgMemRegRegMul,     // Базово-индексная с масштабированием	             - mov eax, [edx+ecx*8]
    ArgMemRegRegMulDisp  // Базово-индексная с масштабированием и смещением  - mov eax, [ebx+edi*2+20h]
  );

  TARMMemArgumentType = (memIMMDisp, mmREGDisp);

  TARMArgument = record
  type
    TShiftType = (shiftNONE, shiftLSL, shiftLSR, shiftASL, shiftROR, shiftRRX);
  var
    ArgumentType: TARMArgumentType;
    Flags: set of TARMMemArgumentType;
    Reg: TPlatformRegisterClass;
    RegDisp: TPlatformRegisterClass;
    DispSign: Integer;  // 1 или -1
    IMMDisp: Integer;
    IMMValue: Integer;
    Shift: TShiftType;
    RegShift: TPlatformRegisterClass;
    Decl: TIDDeclaration;
    LB: PHWLabel;
  end;


{
0000 EQ Equal Z set
0001 NE Not equal Z clear
0010 CS/HS Carry set/unsigned higher or same C set
0011 CC/LO Carry clear/unsigned lower C clear
0100 MI Minus/negative N set
0101 PL Plus/positive or zero N clear
0110 VS Overflow V set
0111 VC No overflow V clear
1000 HI Unsigned higher C set and Z clear
1001 LS Unsigned lower or same C clear or Z set
1010 GE Signed greater than or equal N set and V set, or N clear and V clear (N == V)
1011 LT Signed less than N set and V clear, or N clear and V set (N != V)
1100 GT Signed greater than Z clear, and either N set and V set, or N clear and V clear (Z == 0,N == V)
1101 LE Signed less than or equal Z set, or N set and V clear, or N clear and V set (Z == 1 or N != V)
1110 AL Always
}

function ARM32LDR(Condition: TARMCondition; const Dst, Src: TARMArgument): UInt32; // LOAD REG
function ARM32STR(Condition: TARMCondition; const Src, Dst: TARMArgument): UInt32; // STORE REG
function ARM32MOV(Condition: TARMCondition; const Dst, Src: TARMArgument): UInt32; // MOVE ARG
function ARM32CMP(Condition: TARMCondition; const Lft, Rht: TARMArgument): UInt32; // MOVE ARG

implementation

{
  Флаги:

  I(25) - флаг смещения/3-й регистр, если = 0, тогда смещение задано иначе код третьего регистра (первые 4-е бита)
  P(24) - если 1 то включен режим модификации базового регистра
  B(22) - определяет чтение/запись 32-х битного слова или байта. 1 – значит операция с байтом. При чтении байта в регистр старшие биты регистра обнуляются.
  U(23) - Indicates whether the offset is added to the base (U == 1) or is subtracted from the base (U == 0)
  W(21) -
  L(20) - определяет запись или чтение. 1 – ldr, чтение, 0 – str, запись.
}


function REGCODE(Reg: TPlatformRegisterClass): UInt32; inline;
begin
  Result := Reg.GetCode;
end;

function ADDR_MODE1(Arg: TARMArgument): Integer;
{
1. #<immediate>
See Data-processing operands - Immediate on page A5-6.
2. <Rm>
See Data-processing operands - Register on page A5-8.
3. <Rm>, LSL #<shift_imm>
See Data-processing operands - Logical shift left by immediate on page A5-9.
4. <Rm>, LSL <Rs>
See Data-processing operands - Logical shift left by register on page A5-10.
5. <Rm>, LSR #<shift_imm>
See Data-processing operands - Logical shift right by immediate on page A5-11.
6. <Rm>, LSR <Rs>
See Data-processing operands - Logical shift right by register on page A5-12.
7. <Rm>, ASR #<shift_imm>
See Data-processing operands - Arithmetic shift right by immediate on page A5-13.
8. <Rm>, ASR <Rs>
See Data-processing operands - Arithmetic shift right by register on page A5-14.
9. <Rm>, ROR #<shift_imm>
See Data-processing operands - Rotate right by immediate on page A5-15.
10. <Rm>, ROR <Rs>
See Data-processing operands - Rotate right by register on page A5-16.
11. <Rm>, RRX
See Data-processing operands - Rotate right with extend on page A5-17.
}
begin
  Result := 0;
end;

function ADDR_MODE2(Arg: TARMArgument): Integer;
{
1. [<Rn>, #+/-<offset_12>]
See Load and Store Word or Unsigned Byte - Immediate offset on page A5-20.
2. [<Rn>, +/-<Rm>]
See Load and Store Word or Unsigned Byte - Register offset on page A5-21.
3. [<Rn>, +/-<Rm>, <shift> #<shift_imm>]
See Load and Store Word or Unsigned Byte - Scaled register offset on page A5-22.
4. [<Rn>, #+/-<offset_12>]!
See Load and Store Word or Unsigned Byte - Immediate pre-indexed on page A5-24.
5. [<Rn>, +/-<Rm>]!
See Load and Store Word or Unsigned Byte - Register pre-indexed on page A5-25.
6. [<Rn>, +/-<Rm>, <shift> #<shift_imm>]!
See Load and Store Word or Unsigned Byte - Scaled register pre-indexed on page A5-26.
7. [<Rn>], #+/-<offset_12>
See Load and Store Word or Unsigned Byte - Immediate post-indexed on page A5-28.
8. [<Rn>], +/-<Rm>
See Load and Store Word or Unsigned Byte - Register post-indexed on page A5-30.
9. [<Rn>], +/-<Rm>, <shift> #<shift_imm>
See Load and Store Word or Unsigned Byte - Scaled register post-indexed on page A5-31.
}
begin
  case Arg.ArgumentType of
    // смещение задано регистром
    ArgMemRegReg: begin
      Result := REGCODE(Arg.RegDisp);
      Result := Result + UInt32($01) shl 25; // I = 1
    end;
  else
    // смещение задано непосредственным значением (12 бит)
    Result := Abs(Arg.IMMDisp);
  end;
  // операция сложения или вычитания над смещением
  if Arg.DispSign >= 0 then
    Result := Result + UInt32($01) shl 23; // U = 1
end;

function ARM32LDR(Condition: TARMCondition; const Dst, Src: TARMArgument): UInt32;
begin
  Result := UInt32(Condition) shl 28;         // condition
  Result := Result + UInt32($01) shl 26;      // constant = 01
  Result := Result + UInt32($01) shl 24;      // P = 1
  Result := Result + UInt32($01) shl 20;      // L = 1
  Result := Result + REGCODE(Dst.Reg) shl 12; // dst regcode
  Result := Result + REGCODE(Src.Reg) shl 16; // src regcode
  Result := Result + ADDR_MODE2(Src);
end;

function ARM32STR(Condition: TARMCondition; const Src, Dst: TARMArgument): UInt32;
begin
  Result := UInt32(Condition) shl 28;         // condition
  Result := Result + UInt32($01) shl 26;      // constant = 01
  Result := Result + UInt32($01) shl 24;      // P = 1
  Result := Result + REGCODE(Src.Reg) shl 12; // src regcode
  Result := Result + REGCODE(Dst.Reg) shl 16; // dst regcode
  Result := Result + ADDR_MODE2(Src);
end;

function ARM32MOV(Condition: TARMCondition; const Dst, Src: TARMArgument): UInt32; // LOAD ADDRES
begin
  Result := UInt32(Condition) shl 28;         // condition
  Result := Result + UInt32($0D) shl 21;      // constant = 1101
  Result := Result + REGCODE(Dst.Reg) shl 12; // dst regcode
  Result := Result + REGCODE(Src.Reg) shl 0;  // src regcode
end;

function ARM32CMP(Condition: TARMCondition; const Lft, Rht: TARMArgument): UInt32; // MOVE ARG
begin

end;

end.
