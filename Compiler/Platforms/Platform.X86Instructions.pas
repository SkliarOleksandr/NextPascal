unit Platform.X86Instructions;

interface

uses SysUtils, Classes, Math, HWPlatforms, CompilerUtils;

type
  TTranslateMode = (
    tmUSE16 = 2,
    tmUSE32 = 4,
    tmUSE64 = 8
  );

  // INSTRUCTION CODE (Внутренние коды инструкций)
  TX86_PREFIX = (
    X86_PREFIX_66,
    X86_PREFIX_67,
    X86_PREFIX_LOCK,
    X86_PREFIX_REPE,
    X86_PREFIX_REPNE,
    X86_PREFIX_ALTER,
    X86_PREFIX_TAKEN,
    X86_PREFIX_NTAKEN,
    X86_PREFIX_CS,
    X86_PREFIX_DS,
    X86_PREFIX_ES,
    X86_PREFIX_GS,
    X86_PREFIX_FS,
    X86_PREFIX_SS
  );


  // X86 регистры
  TX86_REGISTER = (
    X86_AL,
    X86_CL,
    X86_DL,
    X86_BL,
    X86_AH,
    X86_CH,
    X86_DH,
    X86_BH,

    X86_AX,
    X86_CX,
    X86_DX,
    X86_BX,
    X86_SI,
    X86_DI,
    X86_SP,
    X86_BP,
    X86_IP,

    X86_EAX,  // 0
    X86_ECX,
    X86_EDX,
    X86_EBX,
    X86_ESI,
    X86_EDI,
    X86_ESP,
    X86_EBP,
    X86_EIP,

    X86_CS,
    X86_DS,
    X86_ES,
    X86_FS,
    X86_GS,
    X86_SS,

    X86_CR0,
    X86_CR1,
    X86_CR2,
    X86_CR3,
    X86_CR4,

    X86_DR0,
    X86_DR1,
    X86_DR2,
    X86_DR3,
    X86_DR4,
    X86_DR5,
    X86_DR6,
    X86_DR7
  );

  TX86_CODE = (
    X86_NOP,

    X86_MOV,
    X86_LEA,
    X86_PUSH,
    X86_POP,

    X86_ADD,
    X86_ADC,
    X86_SUB,
    X86_SBB,
    X86_CMP,

    X86_WAIT,
    X86_FWAIT,
    X86_CPUID,
    X86_CLI,
    X86_STI,
    X86_HLT,
    X86_JMP,
    X86_RET,
    X86_CALL
  );

procedure X86Prefix(Stream: TStream; Prefix: TX86_PREFIX); overload;
procedure X86Instruction(Stream: TStream; Code: TX86_CODE); overload;

procedure X86MOV(Stream: TStream; Mode: TTranslateMode; const Dst, Src: TX86Argument);
procedure X86LEA(Stream: TStream; Mode: TTranslateMode; const Dst, Src: TX86Argument);
procedure X86PUSH(Stream: TStream; Mode: TTranslateMode; const Src: TX86Argument);
procedure X86POP(Stream: TStream; Mode: TTranslateMode; const Dst: TX86Argument);
procedure X86ADD(Stream: TStream; Mode: TTranslateMode; const Dst, Src: TX86Argument);
procedure X86ADC(Stream: TStream; Mode: TTranslateMode; const Dst, Src: TX86Argument);
procedure X86SUB(Stream: TStream; Mode: TTranslateMode; const Dst, Src: TX86Argument);
procedure X86SBB(Stream: TStream; Mode: TTranslateMode; const Dst, Src: TX86Argument);
procedure X86NEG(Stream: TStream; Mode: TTranslateMode; const Dst: TX86Argument);
procedure X86CMP(Stream: TStream; Mode: TTranslateMode; const Dst, Src: TX86Argument);
procedure X86AND(Stream: TStream; Mode: TTranslateMode; const Dst, Src: TX86Argument);
procedure X86XOR(Stream: TStream; Mode: TTranslateMode; const Dst, Src: TX86Argument);
procedure X86NOT(Stream: TStream; Mode: TTranslateMode; const Dst: TX86Argument);
procedure X86OR(Stream: TStream; Mode: TTranslateMode; const Dst, Src: TX86Argument);
procedure X86JMP(Stream: TStream; Mode: TTranslateMode; const Dst: TX86Argument);
procedure X86CALL(Stream: TStream; Mode: TTranslateMode; const Dst: TX86Argument);
procedure X86RETN(Stream: TStream; Mode: TTranslateMode; const Arg: TX86Argument);
procedure X86RETF(Stream: TStream; Mode: TTranslateMode; const Arg: TX86Argument);

implementation

const
  X86_REGISTER_CODES: array [TX86_REGISTER] of Integer = (
    {X86_AL} 0,
    {X86_CL} 1,
    {X86_DL} 2,
    {X86_BL} 3,
    {X86_AH} 4,
    {X86_CH} 5,
    {X86_DH} 6,
    {X86_BH} 7,

    {X86_AX} 0,
    {X86_CX} 1,
    {X86_DX} 2,
    {X86_BX} 3,
    {X86_SP} 4,
    {X86_BP} 5,
    {X86_SI} 6,
    {X86_DI} 7,
    {X86_IP} -1,

    {X86_EAX} 0,
    {X86_ECX} 1,
    {X86_EDX} 2,
    {X86_EBX} 3,
    {X86_ESP} 4,
    {X86_EBP} 5,
    {X86_ESI} 6,
    {X86_EDI} 7,
    {X86_EIP} -1,

    {X86_CS} 1,
    {X86_DS} 3,
    {X86_ES} 0,
    {X86_FS} 4,
    {X86_GS} 5,
    {X86_SS} 2,

    {X86_CR0} 0,
    {X86_CR1} 1,
    {X86_CR2} 2,
    {X86_CR3} 3,
    {X86_CR4} 4,

    {X86_DR0} 0,
    {X86_DR1} 1,
    {X86_DR2} 2,
    {X86_DR3} 3,
    {X86_DR4} 4,
    {X86_DR5} 5,
    {X86_DR6} 6,
    {X86_DR7} 7
 );

 MODRM_REG_MREG = 0;

function GetIMMSize(Mode: TTranslateMode): Integer; inline;
begin
  case Mode of
    tmUSE16: Result := 2;
    tmUSE32: Result := 4;
    tmUSE64: Result := 4; // максимальный размер непосредственного значения - 32 бита
  else
    Result := -1;
  end;
end;


function REGCODE(REG: TPlatformRegisterClass): Integer;
var
  RCode: TX86_REGISTER;
begin
  RCode := TX86_REGISTER(REG.GetCode);
  Result := X86_REGISTER_CODES[RCode];
end;

procedure MOD_RM(Stream: TStream; Mode: TTranslateMode; const Reg: Integer; const RegMem: TX86Argument);
const
  MOD_DISP_8 = 1 shl 6;      // смещение задано знаковым байтом
  MOD_DISP_NATIVE = 2 shl 6; // смешение задано знаковым словом/двойным словом/четверным словом
  MOD_REG = 3 shl 6;         // поле R/M содержит код регистра
  RM_SIB = 4;
var
  RMType: TArgumentType;
  RCode: Integer;
  MODRMSIB: Integer;
  DISPSize: Integer;
begin
  RMType := RegMem.ArgumentType;
  MODRMSIB := Reg shl 3;
  case RMType of
    //----------------------------------------------------------------
    // регистровая адресация
    //----------------------------------------------------------------
    ArgRegister: begin
      MODRMSIB := MODRMSIB + REGCODE(RegMem.Reg);
      MODRMSIB := MODRMSIB + MOD_REG;
      Stream.WriteUInt8(MODRMSIB);
    end;
    //----------------------------------------------------------------
    // прямая адресация
    //----------------------------------------------------------------
    ArgMemImm: begin
      if Mode > tmUSE16 then
        MODRMSIB := MODRMSIB + 5
      else
        MODRMSIB := MODRMSIB + 6;
      Stream.WriteUInt8(MODRMSIB);
      //Stream.WriteData(RegMem.IMMDisp, Integer(Mode));
    end;
    //----------------------------------------------------------------
    // косвенная регистровая адресация
    //----------------------------------------------------------------
    ArgMemRegister: begin
      RCode := REGCODE(RegMem.Reg);
      MODRMSIB := MODRMSIB + RCode;
      if Mode > tmUSE16 then
      begin
        // так как регистр {SP, ESP, RSP} кодирует базово-индексную адресацию, и
        // так как регистр {BP, EBP, RBP} кодирует прямую адресацию, то
        // кодирование этих регистров идет через косвенную регистровую адресацию со смещением ноль
        if (RCode = 4) or (RCode = 5) then
        begin
          MODRMSIB := MODRMSIB + MOD_DISP_8;
          Stream.WriteUInt8(MODRMSIB);
          Stream.WriteUInt8(0);
        end else
          Stream.WriteUInt8(MODRMSIB);
      end else begin
        raise exception.Create('16 bit not supported yet');
      end;
    end;
    //----------------------------------------------------------------
    // косвенная регистровая адресация со смещением
    //----------------------------------------------------------------
    ArgMemRegDisp: begin
      MODRMSIB := MODRMSIB + REGCODE(RegMem.Reg);
      if (RegMem.IMMDisp <= MaxInt8) and (RegMem.IMMDisp >= MinInt8) then
      begin
        DISPSize := 1;
        MODRMSIB := MODRMSIB + MOD_DISP_8;
      end else begin
        DISPSize := Integer(Mode);
        MODRMSIB := MODRMSIB + MOD_DISP_NATIVE;
      end;
      Stream.WriteUInt8(MODRMSIB);
      //Stream.WriteData(RegMem.IMMDisp, DISPSize);
      if DISPSize <> 0 then;
    end;
    //----------------------------------------------------------------
    // косвенная базово-индексная адресация
    //----------------------------------------------------------------
  else
    MODRMSIB := MODRMSIB + RM_SIB;
    MODRMSIB := MODRMSIB + REGCODE(RegMem.Reg) shl 8;
    if Assigned(RegMem.RegIdx) then
    begin
      MODRMSIB := MODRMSIB + REGCODE(RegMem.RegIdx) shl 11;
    end;
    case RMType of
      ArgMemRegRegDisp: begin
        if (RegMem.IMMDisp <= MaxInt8) and (RegMem.IMMDisp >= MinInt8) then
        begin
          DISPSize := 1;
          MODRMSIB := MODRMSIB + MOD_DISP_8;
        end else begin
          DISPSize := Integer(Mode);
          MODRMSIB := MODRMSIB + MOD_DISP_NATIVE;
        end;
      end;
    else
      DISPSize := 0;
    end;
    Stream.WriteUInt16(MODRMSIB);
    //Stream.WriteData(RegMem.IMMDisp, DISPSize);
    if DISPSize <> 0 then;
  end;
end;

procedure X86Prefix(Stream: TStream; Prefix: TX86_PREFIX); overload;
var
  NCode: UInt8;
begin
  case Prefix of
    X86_PREFIX_66: NCode := $66;
    X86_PREFIX_67: NCode := $67;
    X86_PREFIX_LOCK: NCode := $F0;
    X86_PREFIX_REPE: NCode := $F3;
    X86_PREFIX_REPNE: NCode := $F2;
    X86_PREFIX_ALTER: NCode := $64;
    X86_PREFIX_TAKEN: NCode := $3E;
    X86_PREFIX_NTAKEN: NCode := $2E;
    X86_PREFIX_CS: NCode := $2E;
    X86_PREFIX_DS: NCode := $3E;
    X86_PREFIX_ES: NCode := $26;
    X86_PREFIX_FS: NCode := $64;
    X86_PREFIX_GS: NCode := $65;
    X86_PREFIX_SS: NCode := $36;
  else
    raise Exception.CreateFmt('Invalid prefix %d', [Integer(Prefix)]);
    NCode := 0;
  end;
  Stream.WriteUInt8(NCode);
end;

procedure X86Instruction(Stream: TStream; Code: TX86_CODE);
var
  NCode: Integer;
begin
  case Code of
    X86_NOP: NCode := $90;
    X86_HLT: NCode := $F4;
  else
    raise Exception.CreateFmt('Invalid code %d', [Integer(Code)]);
    NCode := 0;
  end;
  Stream.WriteUInt8(NCode);
end;

procedure X86MOV(Stream: TStream; Mode: TTranslateMode; const Dst, Src: TX86Argument);
var
  NCode: Integer;          // код инструкции
  DstType: TArgumentType;  // тип приемника
  SrcType: TArgumentType;  // тип источника
  IMMSize: Integer;        // размер непосредственного значения (в байтах)
begin
  DstType := Dst.ArgumentType;
  SrcType := Src.ArgumentType;
  //=====================================================================================
  // MOV ACC, MEM
  //=====================================================================================
  if (DstType = ArgRegister) and (REGCODE(Dst.Reg) = 0) and (SrcType = ArgMemImm) then
  begin
    if Dst.Reg.GetSize > 8 then
      NCode := $A1  // w = 1
    else
      NCode := $A0; // w = 0
    Stream.WriteUInt8(NCode);
    Stream.WriteBuffer(Src.IMMDisp, Integer(Mode));
  end else
  //=====================================================================================
  // MOV MEM, ACC
  //=====================================================================================
  if (DstType = ArgMemImm) and (SrcType = ArgRegister) and (REGCODE(Src.Reg) = 0) then
  begin
    if Src.Reg.GetSize > 8 then
      NCode := $A3  // w = 1
    else
      NCode := $A2; // w = 0
    Stream.WriteUInt8(NCode);
    Stream.WriteBuffer(Dst.IMMDisp, Integer(Mode));
  end else
  //=====================================================================================
  // MOV REG, IMM
  //=====================================================================================
  if (DstType = ArgRegister) and (SrcType = ArgImmValue) then
  begin
    if Dst.Reg.GetSize > 8 then
    begin
      NCode := $B8; // w = 1
      IMMSize := Integer(Mode);
    end else begin
      NCode := $B0; // w = 0
      IMMSize := 1;
    end;
    NCode := NCode + REGCODE(Dst.Reg);
    Stream.WriteUInt8(NCode);
    Stream.WriteBuffer(Src.IMMValue, IMMSize);
  end else
  //=====================================================================================
  // MOV MEM, IMM
  //=====================================================================================
  if (DstType >= ArgMemImm) and (SrcType = ArgImmValue) then
  begin
    if (DstType = ArgRegister) and (Src.Reg.GetSize = 8) then
    begin
      NCode := $C6; // w = 0
      IMMSize := 1;
    end else begin
      NCode := $C7; // w = 1
      IMMSize := Integer(Mode);
    end;
    Stream.WriteUInt8(NCode);
    MOD_RM(Stream, Mode, 0, Dst);
    Stream.WriteBuffer(Src.IMMValue, IMMSize);
  end else
  //=====================================================================================
  // MOV MEM, REG (D = 1 - DST:REG, SRC:R/M)
  //=====================================================================================
  if (DstType >= ArgRegister) and (SrcType = ArgRegister) then
  begin
    if TX86_REGISTER(Dst.Reg.GetCode) >= X86_DR0 then
      Stream.WriteUInt16($230F)  // MOV DRn, REG
    else
    if TX86_REGISTER(Dst.Reg.GetCode) >= X86_CR0 then
      Stream.WriteUInt16($220F)  // MOV CRn, REG
    else begin
      if TX86_REGISTER(Src.Reg.GetCode) < X86_CS then
      begin
        if Src.Reg.GetSize > 8 then
          NCode := $89  // d = 0, w = 1
        else
          NCode := $88; // d = 0, w = 0
      end else
        NCode := $8C;   // сегментный
      Stream.WriteUInt8(NCode);
    end;
    MOD_RM(Stream, Mode, REGCODE(Src.Reg), Dst);
  end else
  //=====================================================================================
  // MOV REG, MEM
  //=====================================================================================
  if (DstType = ArgRegister) and (SrcType > ArgRegister) then
  begin
    if TX86_REGISTER(Src.Reg.GetCode) >= X86_DR0 then
      Stream.WriteUInt16($210F)  // MOV REG, DRn
    else
    if TX86_REGISTER(Src.Reg.GetCode) >= X86_CR0 then
      Stream.WriteUInt16($200F)  // MOV REG, CRn
    else begin
      if TX86_REGISTER(Dst.Reg.GetCode) < X86_CS then
      begin
        if Dst.Reg.GetSize > 8 then
          NCode := $8B  // d = 1, w = 1
        else
          NCode := $8A; // d = 1, w = 0
      end else
        NCode := $8E;   // сегментный
      Stream.WriteUInt8(NCode);
    end;
    MOD_RM(Stream, Mode, REGCODE(Dst.Reg), Src);
  end else
    raise Exception.Create('Invalid arguments combination');
end;

procedure X86LEA(Stream: TStream; Mode: TTranslateMode; const Dst, Src: TX86Argument);
begin
  Stream.WriteUInt8($8D);
  MOD_RM(Stream, Mode, REGCODE(Dst.Reg), Src);
end;


function GetArithmeticIMMSize(Dst: TPlatformRegisterClass; Mode: TTranslateMode): Integer;
begin
  if Dst.GetSize > 8 then
  case Mode of
    tmUSE16: Result := 2;
    tmUSE32: Result := 4;
    tmUSE64: Result := 4; // максимальный размер непосредственного значения - 32 бита
  else
    raise Exception.Create('Invalid translate mode');
  end else
    Result := 1;
end;

procedure X86Arithmetic(Stream: TStream; Mode: TTranslateMode; const Dst, Src: TX86Argument; ACC_IMM_Code, RM_IMM_Code, RM_REG_Code, REG_RM_Code: UInt8);
var
  NCode: Integer;          // код инструкции
  DstType: TArgumentType;  // тип приемника
  SrcType: TArgumentType;  // тип источника
begin
  DstType := Dst.ArgumentType;
  SrcType := Src.ArgumentType;
  //=====================================================================================
  // ADD ACC, IMM
  //=====================================================================================
  if (DstType = ArgRegister) and (REGCODE(Dst.Reg) = 0) and (SrcType = ArgImmValue) then
  begin
    if Dst.Reg.GetSize > 8 then
      NCode := ACC_IMM_Code  + 1 // w = 1
    else
      NCode := ACC_IMM_Code;     // w = 0
    Stream.WriteUInt8(NCode);
    Stream.WriteBuffer(Src.IMMValue, GetArithmeticIMMSize(Dst.Reg, Mode));
  end else
  //=====================================================================================
  // ADD REG/MEM, IMM
  //=====================================================================================
  if (DstType >= ArgRegister) and (SrcType = ArgImmValue) then
  begin
    if (DstType = ArgRegister) and (Dst.Reg.GetSize = 8) then
      NCode := $80  // w = 0
    else
      NCode := $81; // w = 1
    Stream.WriteUInt8(NCode);
    MOD_RM(Stream, Mode, RM_IMM_Code, Dst);
    Stream.WriteBuffer(Src.IMMValue, GetArithmeticIMMSize(Dst.Reg, Mode));
  end else
  //=====================================================================================
  // ADD REG/MEM, REG  (D = 0)
  //=====================================================================================
  if (DstType >= ArgRegister) and (SrcType = ArgRegister) then
  begin
    if Src.Reg.GetSize > 8 then
      NCode := RM_REG_Code + 1 // w = 1
    else
      NCode := RM_REG_Code;    // w = 0
    Stream.WriteUInt8(NCode);
    MOD_RM(Stream, Mode, REGCODE(Src.Reg), Dst);
  end else
  //=====================================================================================
  // ADD REG, REG/MEM  (D = 1)
  //=====================================================================================
  if (DstType = ArgRegister) and (SrcType >= ArgRegister) then
  begin
    if Src.Reg.GetSize > 8 then
      NCode := REG_RM_Code + 1 // w = 1
    else
      NCode := REG_RM_Code;    // w = 0
    Stream.WriteUInt8(NCode);
    MOD_RM(Stream, Mode, REGCODE(Dst.Reg), Src);
  end else
    raise Exception.Create('Invalid arguments combination');
end;

procedure X86ADD(Stream: TStream; Mode: TTranslateMode; const Dst, Src: TX86Argument);
begin
  X86Arithmetic(Stream, Mode, Dst, Src, $04, $00, $00, $02);
end;

procedure X86ADC(Stream: TStream; Mode: TTranslateMode; const Dst, Src: TX86Argument);
begin
  X86Arithmetic(Stream, Mode, Dst, Src, $14, $02, $10, $12);
end;

procedure X86SUB(Stream: TStream; Mode: TTranslateMode; const Dst, Src: TX86Argument);
begin
  X86Arithmetic(Stream, Mode, Dst, Src, $2C, $05, $28, $2A);
end;

procedure X86SBB(Stream: TStream; Mode: TTranslateMode; const Dst, Src: TX86Argument);
begin
  X86Arithmetic(Stream, Mode, Dst, Src, $1D, $03, $18, $1A);
end;

procedure X86NEG(Stream: TStream; Mode: TTranslateMode; const Dst: TX86Argument);
begin
  // проверить аргумента на 8 бит!
  Stream.WriteUint8($F7);
  MOD_RM(Stream, Mode, $03, Dst);
end;

procedure X86CMP(Stream: TStream; Mode: TTranslateMode; const Dst, Src: TX86Argument);
begin
  X86Arithmetic(Stream, Mode, Dst, Src, $3C, $07, $38, $3A);
end;

procedure X86AND(Stream: TStream; Mode: TTranslateMode; const Dst, Src: TX86Argument);
begin
  X86Arithmetic(Stream, Mode, Dst, Src, $24, $04, $20, $22);
end;

procedure X86XOR(Stream: TStream; Mode: TTranslateMode; const Dst, Src: TX86Argument);
begin
  X86Arithmetic(Stream, Mode, Dst, Src, $34, $06, $30, $32);
end;

procedure X86NOT(Stream: TStream; Mode: TTranslateMode; const Dst: TX86Argument);
begin
  // проверить аргумента на 8 бит!
  Stream.WriteUint8($F7);
  MOD_RM(Stream, Mode, $02, Dst);
end;

procedure X86OR(Stream: TStream; Mode: TTranslateMode; const Dst, Src: TX86Argument);
begin
  X86Arithmetic(Stream, Mode, Dst, Src, $0C, $01, $08, $0A);
end;

procedure X86JMP(Stream: TStream; Mode: TTranslateMode; const Dst: TX86Argument);
var
  NCode: UInt8;
  IMMSize: Integer;
  LabelDisp: Integer;
begin
  case Dst.ArgumentType of
    ArgLabel: begin
      NCode := $E9;    // ближний переход
      IMMSize := GetIMMSize(Mode);
      if Dst.LB.ADDR < Stream.Position then
        LabelDisp := -(Stream.Position - Dst.LB.ADDR)
      else
        LabelDisp := Dst.LB.ADDR - Stream.Position;
      Stream.WriteUInt8(NCode);
      Stream.WriteBuffer(LabelDisp, IMMSize);
    end;
    ArgImmValue: begin
      if (Dst.IMMValue <= MaxInt8) and (Dst.IMMValue >= MinInt8) then
      begin
        NCode := $EB;  // короткий переход
        IMMSize := 1;
      end else begin
        NCode := $E9;  // ближний переход
        IMMSize := GetIMMSize(Mode);
      end;
      Stream.WriteUInt8(NCode);
      Stream.WriteBuffer(Dst.IMMValue, IMMSize);
    end;
  else
    Stream.WriteUInt8($FF); // косвенный переход
    MOD_RM(Stream, Mode, 4, Dst);
  end;
end;

procedure X86CALL(Stream: TStream; Mode: TTranslateMode; const Dst: TX86Argument);
begin
  if Dst.ArgumentType = ArgImmValue then
  begin
    Stream.WriteUInt8($E8);
    Stream.WriteBuffer(Dst.IMMValue, ifthen(Mode = tmUSE16, 2, 4));
  end else begin
    Stream.WriteUInt8($FF);
    MOD_RM(Stream, Mode, 2, Dst);
  end;
end;

procedure X86RETN(Stream: TStream; Mode: TTranslateMode; const Arg: TX86Argument);
begin

end;

procedure X86RETF(Stream: TStream; Mode: TTranslateMode; const Arg: TX86Argument);
begin

end;

procedure X86PUSH(Stream: TStream; Mode: TTranslateMode; const Src: TX86Argument);
var
  NCode: Cardinal;
  IMMSize: Integer;
  RCode: TX86_REGISTER;
begin
  case Src.ArgumentType of
    //-----------------------------------------
    // PUSH REG
    //-----------------------------------------
    ArgRegister: begin
      RCode := TX86_REGISTER(Src.Reg.GetCode);
      case RCode of
        X86_CS: NCode := $0E;
        X86_SS: NCode := $16;
        X86_DS: NCode := $06;
        X86_FS: NCode := $0FA0;
        X86_GS: NCode := $0FA8;
        else
          NCode := $50 + REGCODE(Src.Reg);
      end;
      if NCode <= 255 then
        Stream.WriteUInt8(NCode)
      else
        Stream.WriteUInt16(NCode);
    end;
    //-----------------------------------------
    // PUSH IMM
    //-----------------------------------------
    ArgImmValue: begin
      IMMSize := GetValueByteSize(Src.IMMValue);
      NCode :=ifthen(IMMSize > 8, $68, $6A);
      Stream.WriteUInt8(NCode);
      Stream.WriteBuffer(Src.IMMValue, IMMSize);
    end;
    //-----------------------------------------
    // PUSH MEM
    //-----------------------------------------
  else
    Stream.WriteUInt8($FF);
    MOD_RM(Stream, Mode, 6, Src);
  end;
end;

procedure X86POP(Stream: TStream; Mode: TTranslateMode; const Dst: TX86Argument);
var
  NCode: Cardinal;
  RCode: TX86_REGISTER;
begin
  case Dst.ArgumentType of
    //-----------------------------------------
    // POP REG
    //-----------------------------------------
    ArgRegister: begin
      RCode := TX86_REGISTER(Dst.Reg.GetCode);
      case RCode of
        X86_DS: NCode := $1F;
        X86_ES: NCode := $07;
        X86_SS: NCode := $17;
        X86_FS: NCode := $0FA1;
        X86_GS: NCode := $0FA9;
        else
          NCode := $58+ REGCODE(Dst.Reg);
      end;
      if NCode <= 255 then
        Stream.WriteUInt8(NCode)
      else
        Stream.WriteUInt16(NCode);
    end;
    //-----------------------------------------
    // PUSH MEM
    //-----------------------------------------
  else
    Stream.WriteUInt8($8F);
    MOD_RM(Stream, Mode, 0, Dst);
  end;
end;


end.
