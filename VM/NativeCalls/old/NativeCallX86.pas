{ implementation of x86         abi }
unit NativeCallX86;

interface

{$ifdef FPC}
  {$MODE DELPHIUNICODE}
{$endif}

uses DataTypes, ILTypeInfo, Classes, NativeCalls;

function X86Invoke(_Self, ProcAddr: Pointer; CallingConv: TNCCallingConvention; const Args: TNCArgs; res: PNCArg): Boolean;

implementation

const PTR_SIZE = SizeOf(Pointer);

{$ifdef FPC}
{$define PS_ARRAY_ON_STACK}
{$endif}
function RealFloatCall_Register(p: Pointer;
  _EAX, _EDX, _ECX: Cardinal;
  StackData: Pointer;
  StackDataLen: Longint // stack length are in 4 bytes. (so 1 = 4 bytes)
  ): Extended; Stdcall; // make sure all things are on stack
var
  E: Extended;
begin
  asm
    mov ecx, stackdatalen
    jecxz @@2
    mov eax, stackdata
    @@1:
    mov edx, [eax]
    push edx
    sub eax, 4
    dec ecx
    or ecx, ecx
    jnz @@1
    @@2:
    mov eax,_EAX
    mov edx,_EDX
    mov ecx,_ECX
    call p
    fstp tbyte ptr [e]
  end;
  Result := E;
end;
                 
function RealFloatCall_Other(p: Pointer;
  StackData: Pointer;
  StackDataLen: Longint // stack length are in 4 bytes. (so 1 = 4 bytes)
  ): Extended; Stdcall; // make sure all things are on stack
var
  E: Extended;
begin
  asm
    mov ecx, stackdatalen
    jecxz @@2
    mov eax, stackdata
    @@1:
    mov edx, [eax]
    push edx
    sub eax, 4
    dec ecx
    or ecx, ecx
    jnz @@1
    @@2:
    call p
    fstp tbyte ptr [e]
  end;
  Result := E;
end;

function RealFloatCall_CDecl(p: Pointer;
  StackData: Pointer;
  StackDataLen: Longint // stack length are in 4 bytes. (so 1 = 4 bytes)
  ): Extended; Stdcall; // make sure all things are on stack
var
  E: Extended;
begin
  asm
    mov ecx, stackdatalen
    jecxz @@2
    mov eax, stackdata
    @@1:
    mov edx, [eax]
    push edx
    sub eax, 4
    dec ecx
    or ecx, ecx
    jnz @@1
    @@2:
    call p
    fstp tbyte ptr [e]
    @@5:
    mov ecx, stackdatalen
    jecxz @@2
    @@6:
    pop edx
    dec ecx
    or ecx, ecx
    jnz @@6
  end;
  Result := E;
end;

function RealCall_Register(p: Pointer;
  _EAX, _EDX, _ECX: Cardinal;
  StackData: Pointer;
  StackDataLen: Longint; // stack length are in 4 bytes. (so 1 = 4 bytes)
  ResultLength: Longint; ResEDX: Pointer): Longint; Stdcall; // make sure all things are on stack
var
  r: Longint;
begin
  asm
    mov ecx, stackdatalen
    jecxz @@2
    mov eax, stackdata
    @@1:
    mov edx, [eax]
    push edx
    sub eax, 4
    dec ecx
    or ecx, ecx
    jnz @@1
    @@2:
    mov eax,_EAX
    mov edx,_EDX
    mov ecx,_ECX
    call p
    mov ecx, resultlength
    cmp ecx, 0
    je @@5
    cmp ecx, 1
    je @@3
    cmp ecx, 2
    je @@4
    mov r, eax
    jmp @@5
    @@3:
    xor ecx, ecx
    mov cl, al
    mov r, ecx
    jmp @@5
    @@4:
    xor ecx, ecx
    mov cx, ax
    mov r, ecx
    @@5:
    mov ecx, resedx
    jecxz @@6
    mov [ecx], edx
    @@6:
  end;
  Result := r;
end;

function RealCall_Other(p: Pointer;
  StackData: Pointer;
  StackDataLen: Longint; // stack length are in 4 bytes. (so 1 = 4 bytes)
  ResultLength: Longint; ResEDX: Pointer): Longint; Stdcall; // make sure all things are on stack
var
  r: Longint;
begin
  asm
    mov ecx, stackdatalen
    jecxz @@2
    mov eax, stackdata
    @@1:
    mov edx, [eax]
    push edx
    sub eax, 4
    dec ecx
    or ecx, ecx
    jnz @@1
    @@2:
    call p
    mov ecx, resultlength
    cmp ecx, 0
    je @@5
    cmp ecx, 1
    je @@3
    cmp ecx, 2
    je @@4
    mov r, eax
    jmp @@5
    @@3:
    xor ecx, ecx
    mov cl, al
    mov r, ecx
    jmp @@5
    @@4:
    xor ecx, ecx
    mov cx, ax
    mov r, ecx
    @@5:
    mov ecx, resedx
    jecxz @@6
    mov [ecx], edx
    @@6:
  end;
  Result := r;
end;

function RealCall_CDecl(p: Pointer;
  StackData: Pointer;
  StackDataLen: Longint; // stack length are in 4 bytes. (so 1 = 4 bytes)
  ResultLength: Longint; ResEDX: Pointer): Longint; Stdcall; // make sure all things are on stack
var
  r: Longint;
begin
  asm
    mov ecx, stackdatalen
    jecxz @@2
    mov eax, stackdata
    @@1:
    mov edx, [eax]
    push edx
    sub eax, 4
    dec ecx
    or ecx, ecx
    jnz @@1
    @@2:
    call p
    mov ecx, resultlength
    cmp ecx, 0
    je @@5
    cmp ecx, 1
    je @@3
    cmp ecx, 2
    je @@4
    mov r, eax
    jmp @@5
    @@3:
    xor ecx, ecx
    mov cl, al
    mov r, ecx
    jmp @@5
    @@4:
    xor ecx, ecx
    mov cx, ax
    mov r, ecx
    @@5:
    mov ecx, stackdatalen
    jecxz @@7
    @@6:
    pop eax
    dec ecx
    or ecx, ecx
    jnz @@6
    mov ecx, resedx
    jecxz @@7
    mov [ecx], edx
    @@7:
  end;
  Result := r;
end;

const
  EmptyPchar: array[0..0] of char = #0;

function X86Invoke(_Self, ProcAddr: Pointer; CallingConv: TNCCallingConvention; const Args: TNCArgs; res: PNCArg): Boolean;
var
  Stack: ansistring;
  I: Longint;
  RegUsage: Byte;
  pp: ^Byte;
{$IFDEF FPC}
  IsConstructor,IsVirtualCons: Boolean;
{$ENDIF}

  EAX, EDX, ECX: Longint;

  procedure AddGen(Data: NativeInt);
  begin
    case RegUsage of
      0: begin EAX := Data; Inc(RegUsage); end;
      1: begin EDX := Data; Inc(RegUsage); end;
      2: begin ECX := Data; Inc(RegUsage); end;
    else
      Stack := StringOfChar(AnsiChar(#0),4) + Stack;
      Int32((@Stack[1])^) := Data;
    end;
  end;

  function GetPtr(Arg: PNCArg): Boolean;
  var
    varPtr: Pointer;
    UseReg: Boolean;
    tempstr: AnsiString;
  begin
    Result := False;
    UseReg := True;
    case Arg.BaseType of
      btVarParam: begin
        AddGen(NativeInt(Arg.Dta));
        Exit(True);
      end;
      btSet: begin
        UseReg := True;
        tempstr := StringOfChar(AnsiChar(#0),4);
        if Arg.DataSize > SizeOf(Pointer) then
          pointer((@tempstr[1])^) := Arg.dta
        else
        case Arg.DataSize of
          1: NativeInt((@tempstr[1])^) := PByte(Arg.dta)^;
          2: NativeInt((@tempstr[1])^) := PWord(Arg.dta)^;
          3: NativeInt((@tempstr[1])^) := PCardinal(Arg.dta)^ and $FFFFFF;
          4: NativeInt((@tempstr[1])^) := PCardinal(Arg.dta)^;
        end;
      end;
      btRecord, btStaticArray: begin
        tempstr := StringOfChar(AnsiChar(#0),4);
        {$IFDEF FPC}
        if Arg.DataSize > SizeOf(Pointer) then
          pointer((@tempstr[1])^) := Arg.dta
        else begin
          cardinal((@tempstr[1])^) := cardinal(Arg.dta^);
          UseReg := False;
        end;
        {$ELSE}
        case Arg.DataSize of
          1: Byte((@tempstr[1])^) := byte(Arg.dta^);
          2: word((@tempstr[1])^) := word(Arg.dta^);
          3: begin
              cardinal((@tempstr[1])^) := cardinal(Arg.dta^) and $FFFFFF;
              UseReg := False;
          end;
          4: cardinal((@tempstr[1])^) := cardinal(Arg.dta^);
        else
          pointer((@tempstr[1])^) := Arg.dta;
        end;
        {$ENDIF}
      end;
      btDynArray: begin
        TempStr := StringOfChar(AnsiChar(#0),4);
        Pointer((@TempStr[1])^) := Pointer(Arg.Dta^);
      end;
      btVariant: begin
        TempStr := StringOfChar(AnsiChar(#0),4);
        Pointer((@TempStr[1])^) := Pointer(Arg.Dta);
      end;
      btDouble: {8 bytes} begin
        TempStr := StringOfChar(AnsiChar(#0),8);
        UseReg := False;
        double((@TempStr[1])^) := double(Arg.dta^);
      end;
      btCurrency: {8 bytes} begin
        TempStr := StringOfChar(AnsiChar(#0),8);
        UseReg := False;
        currency((@TempStr[1])^) := currency(Arg.dta^);
      end;
      btSingle: {4 bytes} begin
        TempStr := StringOfChar(AnsiChar(#0),4);;
        UseReg := False;
        Single((@TempStr[1])^) := single(Arg.dta^);
      end;
      btExtended: {10 bytes} begin
        UseReg := False;
        TempStr:= StringOfChar(AnsiChar(#0),12);
        Extended((@TempStr[1])^) := extended(Arg.dta^);
      end;
      btAnsiChar,
      btU8,
      btS8: begin
          TempStr := AnsiChar(Arg^.dta^) + Ansistring(StringOfChar(AnsiChar(#0),3));
        end;
      btWideChar,
      btu16, btS16: begin
        TempStr := StringOfChar(AnsiChar(#0),4);
        Word((@TempStr[1])^) := word(Arg^.dta^);
      end;
      btu32, bts32: begin
        TempStr := StringOfChar(AnsiChar(#0),4);
        Longint((@TempStr[1])^) := Longint(Arg^.dta^);
      end;
      btPointer: begin
        TempStr := StringOfChar(AnsiChar(#0),4);
        if pointer(Arg^.dta^) = nil then
          Pointer((@TempStr[1])^) := @EmptyPchar
        else
          Pointer((@TempStr[1])^) := pointer(Arg^.dta^);
      end;
      btclass, btinterface, btAnsiString: begin
        TempStr := StringOfChar(AnsiChar(#0),4);
        Pointer((@TempStr[1])^) := pointer(Arg^.dta^);
      end;
      btWideString: begin
        TempStr := StringOfChar(AnsiChar(#0),4);
        Pointer((@TempStr[1])^) := pointer(Arg^.dta^);
      end;
      btUnicodeString: begin
        TempStr := StringOfChar(AnsiChar(#0),4);
        Pointer((@TempStr[1])^) := pointer(Arg^.dta^);
      end;
      btProcPtr: begin
        tempstr := StringOfChar(AnsiChar(#0),8);
        //TMethod((@TempStr[1])^) := MKMethod(Self, Longint(Param.Dta^)); disabled  !!!!!
        UseReg := false;
      end;
      bts64: begin
        TempStr:= StringOfChar(AnsiChar(#0),8);
        Int64((@TempStr[1])^) := int64(Arg^.dta^);
        UseReg := False;
      end;
    end; {case}
    if UseReg then
    begin
      case RegUsage of
        0: begin EAX := Longint((@Tempstr[1])^); Inc(RegUsage); end;
        1: begin EDX := Longint((@Tempstr[1])^); Inc(RegUsage); end;
        2: begin ECX := Longint((@Tempstr[1])^); Inc(RegUsage); end;
        else begin
        {$IFDEF FPC_OLD_FIX}
          if CallingConv = cdRegister then
          Stack := Stack + TempStr
        else
        {$ENDIF}
          Stack := TempStr + Stack;
        end;
      end;
    end else begin
      {$IFDEF FPC_OLD_FIX}
          if CallingConv = cdRegister then
          Stack := Stack + TempStr
        else
      {$ENDIF}
        Stack := TempStr + Stack;
    end;
    Result := True;
  end;
var
  ParamsCount: Integer;
  StackPtr: Pointer;
begin
  ParamsCount := Length(Args);
  {$IFDEF FPC}
  if (Integer(CallingConv) and 128) <> 0 then begin
    IsVirtualCons := true;
    CAllingConv := TNCCallingConvention(Integer(CallingConv) and not 128);
  end else
    IsVirtualCons:= false;
  if (Integer(CallingConv) and 64) <> 0 then begin
    IsConstructor := true;
    CAllingConv := TNCCallingConvention(Integer(CallingConv) and not 64);
  end else
    IsConstructor := false;
  {$ENDIF}
  Result := False;
  if ProcAddr = nil then
    exit; // need address
  Stack := '';

  {if res <> nil then
    res.VarParam := true;}

  case CallingConv of
    //=====================================================================================
    cdRegister: begin
        EAX := 0;
        EDX := 0;
        ECX := 0;
        RegUsage := 0;
        {$IFDEF FPC} // FIX FOR FPC constructor calls
        if IsConstructor then begin
          if not GetPtr(@Args[0]) then exit; // this goes first
          RegUsage := 2;
          EDX := Longint(_Self);
          //Params.Delete(0);
        end else
        {$ENDIF}
        if Assigned(_Self) then begin
          RegUsage := 1;
          EAX := Longint(_Self);
        end;

        for I := 0 to ParamsCount - 1 do
          if not GetPtr(@Args[I]) then Exit;

        if Assigned(res) then begin
          case res^.BaseType of
            btWideString, btUnicodeString,
            btInterface, btDynArray, {$IFNDEF PS_FPCSTRINGWORKAROUND}btAnsiString, {$ENDIF}btVariant: AddGen(Int32(res.Dta));
            {$IFDEF FPC}
            btStaticArray: AddGen(Int32(res.Dta));
            {$ELSE}
            btStaticArray: if (res.DataSize > 4) or (res.DataSize = 3) then AddGen(Int32(res.Dta));
            {$ENDIF}
            btRecord: if (res.DataSize > 4) or (res.DataSize = 3) then AddGen(Int32(res.Dta));
            btSet: begin
              if res.DataSize > 4 then AddGen(Int32(res.Dta));
            end;
          end;
          {$IFDEF DARWIN}
          if (length(Stack) mod 16) <> 0 then begin
            Stack := Stack + StringOfChar(ansichar(#32), 16 - (Length(Stack) mod 16)) ;
          end;
          {$ENDIF}
          if Stack <> '' then
            StackPtr := @Stack[Length(Stack)-3]
          else
            StackPtr := nil;
          case res^.BaseType of
            btSet: begin
              case res.DataSize  of
                1: byte(res.Dta^) := RealCall_Register(ProcAddr, EAX, EDX, ECX, StackPtr, Length(Stack) div 4, 1, nil);
                2: word(res.Dta^) := RealCall_Register(ProcAddr, EAX, EDX, ECX, StackPtr, Length(Stack) div 4, 2, nil);
                3, 4: Longint(res.Dta^) := RealCall_Register(ProcAddr, EAX, EDX, ECX, StackPtr, Length(Stack) div 4, 4, nil);
              else
                RealCall_Register(ProcAddr, EAX, EDX, ECX, StackPtr, Length(Stack) div 4, 0, nil)
              end;
            end;
            btSingle: Single(res.Dta^) := RealFloatCall_Register(ProcAddr, EAX, EDX, ECX, StackPtr, Length(Stack) div 4);
            btDouble: Double(res.Dta^) := RealFloatCall_Register(ProcAddr, EAX, EDX, ECX, StackPtr, Length(Stack) div 4);
            btExtended: Extended(res.Dta^) := RealFloatCall_Register(ProcAddr, EAX, EDX, ECX, StackPtr, Length(Stack) div 4);
            btAnsiChar,btU8, btS8: Byte(res.dta^) := RealCall_Register(ProcAddr, EAX, EDX, ECX, StackPtr, Length(Stack) div 4, 1, nil);
            btWideChar, btu16, bts16: UInt16(res.dta^) := RealCall_Register(ProcAddr, EAX, EDX, ECX, StackPtr, Length(Stack) div 4, 2, nil);
            btClass, btPointer: begin
            {$IF DEFINED (fpc) and (fpc_version < 3)}
              if IsConstructor or IsVirtualCons then
                UInt32(res.dta^) := RealCall_Register(ProcAddr, EDX, EAX, ECX, StackPtr, Length(Stack) div 4, 4, nil)
              else
           {$ENDIF}
              UInt32(res.dta^) := RealCall_Register(ProcAddr, EAX, EDX, ECX, StackPtr, Length(Stack) div 4, 4, nil);
            end;
            btu32, bts32: UInt32(res.dta^) := RealCall_Register(ProcAddr, EAX, EDX, ECX, StackPtr, Length(Stack) div 4, 4, nil);
            bts64: begin
              EAX := RealCall_Register(ProcAddr, EAX, EDX, ECX,StackPtr, Length(Stack) div 4, 4, @EDX);
              Int64(res.dta^) := Int64(Cardinal(EDX)) shl 32 or Cardinal(EAX);
            end;
            btCurrency: Currency(res.Dta^) := RealFloatCall_Register(ProcAddr, EAX, EDX, ECX, StackPtr, Length(Stack) div 4) / 10000;

            btRecord, btStaticArray: begin

              case Res.DataSize of
                1, 2, 4: begin
                  eax := RealCall_Register(ProcAddr, EAX, EDX, ECX, StackPtr, Length(Stack) div 4, Res.DataSize, nil);
                  Longint(res.dta^) := eax;
                end;
                3: begin
                  RealCall_Register(ProcAddr, EAX, EDX, ECX, StackPtr, Length(Stack) div 4, 0, nil);
                  Longint(res.dta^) := PInteger(StackPtr)^;
                end;
              else
                RealCall_Register(ProcAddr, EAX, EDX, ECX, StackPtr, Length(Stack) div 4, 0, nil);
              end;
            end;

            btInterface,
            btVariant,
            btWidestring,btUnicodestring,
            btDynArray, {$IFNDEF PS_FPCSTRINGWORKAROUND}btAnsiString {$ENDIF}: RealCall_Register(ProcAddr, EAX, EDX, ECX, StackPtr, Length(Stack) div 4, 0, nil);
            {$IFDEF PS_FPCSTRINGWORKAROUND}
            btString: begin
               eax := RealCall_Register(Address, EAX, EDX, ECX, StackPtr, Length(Stack) div 4, 4, nil);
               Longint(res.dta^) := eax;
            end;
            {$ENDIF}
          else
            exit;
          end;
        end else begin
          {$IFDEF DARWIN}
          if (length(Stack) mod 16) <> 0 then
            Stack := Stack + StringOfChar(ansichar(#32), 16 - (Length(Stack) mod 16));
          {$ENDIF}
          if Stack <> '' then
            StackPtr := @Stack[Length(Stack)-3]
          else
            StackPtr := nil;
          RealCall_Register(ProcAddr, EAX, EDX, ECX, StackPtr, Length(Stack) div 4, 0, nil);
        end;
        Result := True;
      end;
    //=====================================================================================
    cdPascal: begin
      RegUsage := 3;
      for I :=  0 to ParamsCount - 1 do begin
        if not GetPtr(@Args[i]) then Exit;
      end;
      if assigned(res) then begin
        case res^.BaseType of
          btWideString, btUnicodeString, btInterface, btDynArray, btrecord, btAnsiString, btVariant: GetPtr(res);
        end;
      end;
      if assigned(_Self) then begin
        Stack := StringOfChar(AnsiChar(#0),4) +Stack;
        Pointer((@Stack[1])^) := _Self;
      end;
      {$IFDEF DARWIN}
      if (length(Stack) mod 16) <> 0 then begin
        Stack := Stack + StringOfChar(ansichar(#32), 16 - (Length(Stack) mod 16)) ;
      end;
      {$ENDIF}
      if assigned(res) then begin
        case res^.BaseType of
          btSingle:      Single(res^.Dta^) := RealFloatCall_Other(ProcAddr, @Stack[Length(Stack)-3], Length(Stack) div 4);
          btDouble:      Double(res^.Dta^) := RealFloatCall_Other(ProcAddr, @Stack[Length(Stack)-3], Length(Stack) div 4);
          btExtended:    Extended(res^.Dta^) := RealFloatCall_Other(ProcAddr, @Stack[Length(Stack)-3], Length(Stack) div 4);
          btAnsiChar, btU8, btS8:    Byte(res^.Dta^) := RealCall_Other(ProcAddr, @Stack[Length(Stack)-3], Length(Stack) div 4, 1, nil);
          btWideChar, btu16, bts16:  UInt16(res^.Dta^) := RealCall_Other(ProcAddr, @Stack[Length(Stack)-3], Length(Stack) div 4, 2, nil);
          btClass, btu32, bts32:  UInt32(res^.Dta^):= RealCall_Other(ProcAddr, @Stack[Length(Stack)-3], Length(Stack) div 4, 4, nil);
          //btPChar:       TBTSTRING(res^.dta^) := Pansichar(RealCall_Other(ProcAddr, @Stack[Length(Stack)-3], Length(Stack) div 4, 4, nil));
          bts64: begin
            EAX := RealCall_Other(ProcAddr, @Stack[Length(Stack)-3], Length(Stack) div 4, 4, @EDX);
            Int64(res^.dta^) := Int64(EAX) shl 32 or EDX;
          end;
          btVariant,
          btInterface, btrecord, btAnsiString: RealCall_Other(ProcAddr, @Stack[Length(Stack)-3], Length(Stack) div 4, 0, nil);
        else
          exit;
        end;
      end else
        RealCall_Other(ProcAddr, @Stack[Length(Stack)-3], Length(Stack) div 4, 0, nil);
      Result := True;
    end;
    //=====================================================================================
    cdSafeCall: begin
      RegUsage := 3;
      if assigned(res) then begin
        GetPtr(res);
      end;
      for I := ParamsCount - 1 downto 0 do begin
        if not GetPtr(@Args[i]) then Exit;
      end;
      if assigned(_Self) then begin
        Stack := StringOfChar(AnsiChar(#0),4) +Stack;
        Pointer((@Stack[1])^) := _Self;
      end;
      {$IFDEF DARWIN}
      if (length(Stack) mod 16) <> 0 then begin
        Stack := Stack + StringOfChar(ansichar(#32), 16 - (Length(Stack) mod 16)) ;
      end;
      {$ENDIF}
      // OleCheck(RealCall_Other(Address, @Stack[Length(Stack)-3], Length(Stack) div 4, 4, nil)); disabled !!!!!!!!!
      Result := True;
    end;
    //=====================================================================================
    cdCdecl: begin
      RegUsage := 3;
      if assigned(_Self) then begin
        Stack := StringOfChar(AnsiChar(#0),4);
        Pointer((@Stack[1])^) := _Self;
      end;
      for I := ParamsCount - 1 downto 0 do begin
        if not GetPtr(@Args[I]) then Exit;
      end;
      {$IFDEF DARWIN}
      if (length(Stack) mod 16) <> 0 then begin
        Stack := Stack + StringOfChar(ansichar(#32), 16 - (Length(Stack) mod 16)) ;
      end;
      {$ENDIF}
      if assigned(res) then begin
        case res^.BaseType of
          btSingle:      Single(res^.dta^) := RealFloatCall_Cdecl(ProcAddr, @Stack[Length(Stack)-3], Length(Stack) div 4);
          btDouble:      Double(res^.dta^) := RealFloatCall_Cdecl(ProcAddr, @Stack[Length(Stack)-3], Length(Stack) div 4);
          btExtended:    Extended(res^.dta^) := RealFloatCall_Cdecl(ProcAddr, @Stack[Length(Stack)-3], Length(Stack) div 4);
          btAnsiChar, btU8, btS8:    Byte(res^.dta^) := RealCall_Cdecl(ProcAddr, @Stack[Length(Stack)-3], Length(Stack) div 4, 1, nil);
          btWideChar, btu16, bts16:  UInt16(res^.dta^) := RealCall_Cdecl(ProcAddr, @Stack[Length(Stack)-3], Length(Stack) div 4, 2, nil);
          btClass, btu32, bts32:  UInt32(res^.dta^) := RealCall_Cdecl(ProcAddr, @Stack[Length(Stack)-3], Length(Stack) div 4, 4, nil);
          //btPChar:       TBTSTRING(res^.dta^) := Pansichar(RealCall_Cdecl(ProcAddr, @Stack[Length(Stack)-3], Length(Stack) div 4, 4, nil));
          bts64: begin
            EAX := RealCall_CDecl(ProcAddr, @Stack[Length(Stack)-3], Length(Stack) div 4, 4, @EDX);
            Int64(res^.Dta^) := Int64(EAX) shl 32 or EDX;
          end;
          btVariant, btUnicodeString, btWideString,
          btInterface,
          btDynArray, btrecord, btAnsiString: begin
            GetPtr(res);
            RealCall_Cdecl(ProcAddr, @Stack[Length(Stack)-3], Length(Stack) div 4, 0, nil);
          end;
        else
          exit;
        end;
      end else begin
        RealCall_CDecl(ProcAddr, @Stack[Length(Stack)-3], Length(Stack) div 4, 0, nil);
      end;
      Result := True;
    end;
    //=====================================================================================
    cdStdCall: begin
      RegUsage := 3;
      for I := ParamsCount - 1 downto 0 do begin
        if not GetPtr(@Args[I]) then exit;
      end;
      if assigned(_Self) then begin
        Stack := StringOfChar(AnsiChar(#0),4) + Stack;
        Pointer((@Stack[1])^) := _Self;
      end;
      if Stack <> '' then
        StackPtr := @Stack[Length(Stack)-3]
      else
        StackPtr := nil;
      if Assigned(res) then
      begin
        case res^.BaseType of
          btSingle: Single(res^.dta^) := RealFloatCall_Other(ProcAddr, StackPtr, Length(Stack) div 4);
          btDouble: Double(res^.dta^) := RealFloatCall_Other(ProcAddr, StackPtr, Length(Stack) div 4);
          btExtended: Extended(res^.dta^):= RealFloatCall_Other(ProcAddr, StackPtr, Length(Stack) div 4);
          btAnsiChar, btU8, btS8: Byte(res^.dta^) := RealCall_Other(ProcAddr, StackPtr, Length(Stack) div 4, 1, nil);
          btWideChar, btu16, bts16:  UInt16(res^.dta^) := RealCall_Other(ProcAddr, StackPtr, Length(Stack) div 4, 2, nil);
          btclass, btu32, bts32:  UInt32(res^.dta^) := RealCall_Other(ProcAddr, StackPtr, Length(Stack) div 4, 4, nil);
          bts64: begin
            EAX := RealCall_Other(ProcAddr, StackPtr, Length(Stack) div 4, 4, @EDX);
            Int64(res^.dta^) := Int64(EAX) shl 32 or EDX;
          end;
          btVariant, btUnicodeString, btWideString,
          btInterface, btDynArray, btrecord, btAnsiString: begin
            GetPtr(res);
            RealCall_Other(ProcAddr, StackPtr, Length(Stack) div 4, 0, nil);
          end;
          btSet: begin
            if Res.DataSize <= PTR_SIZE then
            begin
              EAX := RealCall_Other(ProcAddr, StackPtr, Length(Stack) div 4, Res.DataSize, nil);
              Int32(res^.dta^) := EAX;
            end else begin
              EAX := RealCall_Other(ProcAddr, StackPtr, Length(Stack) div 4, 0, nil);
              Int32(res^.dta^) := EAX;
            end;
          end;
        else
          exit;
        end;
      end else
        RealCall_Other(ProcAddr, @Stack[Length(Stack)-3], Length(Stack) div 4, 0, nil);
      Result := True;
    end;
  end;
end;

end.
