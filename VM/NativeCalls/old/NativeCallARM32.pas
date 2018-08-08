unit NativeCallARM32;

{$ifdef FPC}
  {$MODE DELPHI}
{$endif}

{ implementation of the arm procedure call standard for function calls in pascal script
  Copyright (c) 2008 by Henry Vermaak (henry.vermaak@gmail.com)

  Support for EABI baseline and EABI hardfloat (c) 2015
    modulo7 ( https://github.com/modulo7 )
    Peter Michael Green ( plugwash@p10link.net )

  todo: add wince support

  notes:

  most arm cpus don't allow unaligned access.  by default (?) the linux kernel
  is set up to try and correct unaligned access, which can lead to strange behaviour.
  to turn this off, try (as root):

  echo 4 > /proc/cpu/alignment

  if you have an alignment problem, you will now get a crash with a backtrace like this:
  (make sure you compile with -O- -gl)

  An unhandled exception occurred at $0006C014 :
  EBusError : Bus error or misaligned data access
    $0006C014  PROCESSREPEAT,  line 9670 of upscompiler.pas
    $00068AAC  TPSPASCALCOMPILER__PROCESSSUB,  line 10459 of upscompiler.pas
    $0007D0B4  TPSPASCALCOMPILER__COMPILE,  line 11704 of upscompiler.pas

  you can fix this by using the "unaligned" keyword around the pointer operation.
  search for occurances of "unaligned" to see how this is done,
  (use $ifdef FPC_REQUIRES_PROPER_ALIGNMENT).

  for more information, visit:

  http://www.aleph1.co.uk/oldsite/armlinux/book/afaq.html
}   

interface

uses SysUtils, NativeCallsWithOldIntf;

function Invoke(_Self, Address: Pointer; CallingConv: TNCCallingConvention; const Args: TNCArgs; res: PNCArg): Boolean;

implementation

const
  rtINT = 0;
  rtFLOAT = 2;

type
  Trint = array[1..4] of dword;
{$IFDEF FPC_abi_eabihf}
  Trfloat = record case byte of
          1:(s:array[0..15] of single);
          2:(d:array[0..7] of double);
          end;
{$ELSE}
  Trfloat = array[1..4] of double;
{$ENDIF}
{$goto on}
{ define labels }
label
  stack_loop,
  load_regs,
  asmcall_end{,
  int_result,
  int64_result,
  float_result};

{ call a function from a pointer }
{ resulttype: 0 = int, 1 = int64, 2 = float }
function armasmcall(constref rint: Trint;constref rfloat: Trfloat; proc, stack: pointer; stacksize, resulttype: integer): int64; assembler; nostackframe;
asm
	mov	r12, r13
	stmfd	r13!, {r4, r5, r6, r7, r8, r9, r10, r11, r12, r14, r15}
	sub	r11, r12, #4
	mov	r4, #80			(* space for preserved registers and parameters *)
	ldr	r5, [r11, #4]		(* stacksize we need for subroutine *)
	add	r4, r4, r5
	sub	r13, r13, r4		(* create stack space *)

{$ifdef FPC_abi_eabi}
	(* EABI requires 8 byte aligned stack pointer for procedure calls, ensure alignment. *)
	bic	r13, r13, #7
{$endif}

	(* store parameters on stack *)
	str	r0, [r11, #-44]		(* rint *)
	str	r1, [r11, #-48]		(* rfloat *)
	str	r2, [r11, #-52]		(* proc *)
	str	r3, [r11, #-56] 	(* stack *)
	ldr	r0, [r11, #4]
	str	r0, [r11, #-60]		(* stacksize *)
	ldr	r0, [r11, #8]		
	str	r0, [r11, #-64]		(* resulttype *)

	(* store params for sub-routine that don't fit into r0-r3 at start of stack *)
	ldr	r0, [r11, #-60]		(* stacksize *)
	cmp	r0, #0
	beq	load_regs		(* skip if no stack *)
	mov	r1, r13			(* this points to the bottom now *)
	ldr	r2, [r11, #-56]		(* stack pointer *)
stack_loop:
	ldmia	r2!, {r4}		(* get stack + update pos *)
	stmia	r1!, {r4}		(* store stack + update pos *)
	subs	r0, r0, #4
	bne	stack_loop

load_regs:
	(* load general regs *)
	ldr	r4, [r11, #-44]	(* rint *)
	ldr	r0, [r4]
	ldr	r1, [r4, #4]
	ldr	r2, [r4, #8]
	ldr	r3, [r4, #12]

{$ifdef FPUFPA}
	(* load float regs *)
	ldr	r4, [r11, #-48]	(* rfloat *)
	ldfd	f0, [r4]
	ldfd	f1, [r4, #8]
	ldfd	f2, [r4, #16]
	ldfd	f3, [r4, #24]
{$endif}
{$ifdef FPC_abi_eabihf}
	(* load float regs *)
	ldr	r4, [r11, #-48]	(* rfloat *)
	fldmiad r4, {d0,d1,d2,d3,d4,d5,d6,d7}
{$endif}

	(* branch to the proc pointer *)
	ldr	r4, [r11, #-52]

{$ifdef FPC_abi_eabi}
	blx	r4
{$else}
	mov	r14, r15
	mov	r15, r4
{$endif}

	ldr	r4, [r11, #-64]		(* get resulttype *)
	cmp	r4, #1

	ble     asmcall_end
{$ifdef FPUFPA}
	stfd	f0, [r11, #-72]
	ldr r0, [r11, #-72]
	ldr r1, [r11, #-68]
{$endif}
{$IFDEF FPC_abi_eabihf}
	fmrrd    r0, r1, d0
{$endif}


asmcall_end:
	ldmea	r11,{r4,r5,r6,r7,r8,r9,r10,r11,r13,r15}
end;

function Invoke(_Self, Address: Pointer; CallingConv: TNCCallingConvention; const Args: TNCArgs; res: PNCArg): Boolean;
var
  rint: Trint;			{ registers r0 to r3 }
  rfloat: Trfloat;		{ registers f0 to f3 }
  st: packed array of byte;	{ stack }
  i, j, rindex, findex, stindex, sindex: integer;
  Arg: PNCArg;
  IsConstructor: Boolean;

  { add a dword to stack }
  procedure addstackdword(value: dword);
  begin
    setlength(st, stindex+4);
    pdword(@st[stindex])^ := value;
    inc(stindex, 4);
  end;

  { add a qword to stack }
  procedure addstackqword(value: qword);
  begin
    if (stindex and 4)<>0 then inc(stindex, 4);
    setlength(st, stindex+8);
    pqword(@st[stindex])^ := value;
    inc(stindex, 8);
  end;
  { add a float to stack }
  procedure addstackfloat(value: pointer; size: integer);
  begin
    setlength(st, stindex + (size * 4));
    if size = 1
      then psingle(@st[stindex])^ := single(value^)
      else pdouble(@st[stindex])^ := double(value^);
    inc(stindex, size*4);
  end;

  { add to the general registers or overflow to stack }
  procedure addgen(value: dword);
  begin
    if rindex <= 4
      then begin
        rint[rindex] := value;
        inc(rindex);
      end
      else begin
        addstackdword(value);
      end;
  end;

  { add to the general registers or overflow to stack }
  procedure addgend(value: qword);
  begin
    if (rindex and 1)=0 then inc(rindex);
    if rindex <= 4
      then begin
        rint[rindex] := lo(value);
        inc(rindex);
        rint[rindex] := hi(value);
        inc(rindex);
      end
      else begin
        addstackqword(value);
      end;
  end;
  { add to the float registers or overflow to stack }
  { size = 1 for single, 2 for double }
  procedure addfloat(value: pointer; size: integer);
  begin
    {$IFDEF FPC_abi_eabihf}
    if (findex <= 7) or ((size = 1) and (sindex >= 0)) then
    begin
      if size = 1 then
        if sindex>=0 then begin
           rfloat.s[sindex] := single(value^);
           sindex:=-1;
        end else begin
           rfloat.s[findex*2] := single(value^);
           sindex:=findex*2+1;
           inc(findex);
        end else begin
            rfloat.d[findex] := double(value^);
            inc(findex);
        end;
    end else begin
      sindex := -1;
      if size = 1 then
        addstackdword(pdword(value)^)
      else
        addstackqword(pqword(value)^);
    end;
    {$ELSE}
    if findex <= 4 then begin
      if size = 1 then
        rfloat[findex] := single(value^)
      else
        rfloat[findex] := double(value^);
      inc(findex);
    end else begin
      addstackfloat(value, size);
    end;
{$ENDIF}
  end;
var
  tempdw : dword;
  tempqw : qword;
begin
  if (Integer(CallingConv) and 64) <> 0 then begin
    IsConstructor := true;
    CAllingConv := TNCCallingConvention(Integer(CallingConv) and not 64);
  end else IsConstructor := false;

  rindex := 1;
{$IFDEF FPC_abi_eabihf}
  findex := 0;
  sindex := -1;
{$ELSE}
  findex := 1;
{$ENDIF}
  stindex := 0;
  setlength(st, stindex);
  Result := False;

 if Assigned(_Self) then
    addgen(DWORD(_Self));

  { the pointer of the result needs to be passed first in the case of some result types }
   if assigned(res) then begin
    case res.basetype of
      btStaticArray, btRecord, btInterface, btAnsiString, btUnicodeString, btDynArray, btVariant, btSet: addgen(dword(res.dta));
    end;
  end;

  { process all parameters }
  for i := 0 to Length(Args)-1 do
  begin
    Arg := @Args[i];
    { cook dynamic arrays - fpc stores size-1 at @array-4 }
    if (Arg.BaseType = btDynArray) then
      dec(pdword(pointer(Arg.dta^)-4)^);

      case Arg.BaseType of
        { add normal Args here }
        btVarParam: addgen(dword(Arg.dta));
        btAnsiString, btUnicodeString: addgen(dword(pstring(Arg.dta)^));
        btS8, btU8:                          addgen(dword(pbyte(Arg.dta)^));
        btU16, BtS16, btWideChar:            addgen(dword(pword(Arg.dta)^));
        btU32, btS32:                         addgen(dword(pdword(Arg.dta)^));
        btClass, btInterface, {btPChar, }btPointer: addgen(dword(pdword(Arg.dta)^));
        btSingle:                            {$if defined(FPUFPA) or defined(FPC_abi_eabihf)}
	                                       addfloat(fvar.dta, 1);
	                                     {$else}
					       addgen(dword(psingle(Arg.dta)^));
					     {$endif}
        btDouble{, btExtended}:              {$if defined(FPUFPA) or defined(FPC_abi_eabihf)}
	                                       addfloat(fvar.dta, 2);
	                                     {$else}
					       begin
					         {$IFDEF FPC_abi_eabi}
					         addgend(qword(pint64(Arg.dta)^));
					         {$ELSE}
        				         addgen(lo(qword(pdouble(Param.dta)^)));
				                 addgen(hi(qword(pdouble(Param.dta)^)));
					         {$ENDIF}
					       end;
					     {$endif}
        //:                  addgen(dword(ppchar(Arg.dta)^));
        btAnsiChar:                          addgen(dword(pchar(Arg.dta)^));
        bts64: begin
          {$IFDEF FPC_abi_eabi}
          addgend(qword(pint64(Arg.dta)^));
          {$ELSE}
          addgen(dword(pint64(Param.dta)^ and $ffffffff));
          addgen(dword(pint64(Param.dta)^ shr 32));
          {$ENDIF}
        end;
        btStaticArray: addgen(dword(Arg.dta));
        btSet: begin
          case Arg.DataSize of
            1: addgen(byte(Arg.dta^));
            2: addgen(word(Arg.dta^));
            3: addgen(dword(Arg.dta^) and $FFFFFF);
            4: addgen(dword(Arg.dta^));
          else
            addgen(dword(Arg.dta));
          end;
        end;
        btRecord: begin
          case Arg.DataSize of
            1: addgen(byte(Arg.dta^));
            2: addgen(word(Arg.dta^));
            3: addgen(dword(Arg.dta^) and $FFFFFF);
            4: addgen(dword(Arg.dta^));
          else
            for j := 0 to (Arg.DataSize div 4)-1 do
              addgen(pdword(Arg.dta + j*4)^);
            Arg.dta := Arg.dta + (Arg.DataSize div 4)*4;
            case (Arg.DataSize mod 4) of
              1: addgen(byte(Arg.dta^));
              2: addgen(word(Arg.dta^));
              3: addgen(dword(Arg.dta^) and $FFFFFF);
            end;
          end;
        end;
        btDynArray: addgen(dword(Arg.dta^));  { this is a bit weird }
        btVariant: addgen(dword(Arg.dta));
      else
        WriteLn(stderr, 'Parameter type not implemented! ' + inttostr(Byte(Arg.BaseType)));
        Exit;
      end;  { case }
  end;  { for }

  if not assigned(res) then
  begin
    armasmcall(rint, rfloat, address, st, stindex, rtINT);  { ignore return }
  end else
  begin
    case res.basetype of
      { add result types here }
      btU8, btS8, btAnsiChar:  pbyte(res.dta)^ := lo(lo(lo(armasmcall(rint, rfloat, address, st, stindex, rtINT))));
      btU16, btS16, btWideChar: pword(res.dta)^ := lo(lo(armasmcall(rint, rfloat, address, st, stindex, rtINT)));
      btU32, btS32{, btPChar}:    pdword(res.dta)^ := lo(armasmcall(rint, rfloat, address, st, stindex, rtINT));
      btPointer, btClass:      pdword(res.dta)^ := lo(armasmcall(rint, rfloat, address, st, stindex, rtINT));
      btS64:                   pqword(res.dta)^ := armasmcall(rint, rfloat, address, st, stindex,rtINT);
      {$IFDEF FPC_abi_eabi}
      btSingle:                pdword(res.dta)^ := lo(armasmcall(rint, rfloat, address, st, stindex, rtFLOAT));
      {$ELSE}
      btSingle:                begin
                                 tempqw := armasmcall(rint, rfloat, address, st, stindex, rtFLOAT);
                                 psingle(res.dta)^ := pdouble(@tempqw)^;
                               end;
      {$ENDIF}
      btDouble: pqword(res.dta)^ := armasmcall(rint, rfloat, address, st, stindex, rtFLOAT);
      btStaticArray, btRecord, btDynArray, btInterface, btAnsiString, btUnicodeString, btVariant, btSet: armasmcall(rint, rfloat, address, st, stindex, rtINT);
    else
      writeln(stderr, 'Result type not implemented! ' + inttostr(Byte(res.basetype)));
      exit;
    end;  { case }
  end;

  { cook dynamic arrays - fpc stores size-1 at @array-4 }
  for i := 0 to Length(Args)-1 do begin
    Arg := @Args[i];
    if (Arg.BaseType = btDynArray)
      then inc(pdword(pointer(Arg.dta^)-4)^);
   end;

  Result := True;
end;

end.
