unit VM.Core.Managed;

interface

uses VM.Types;

const STR_REC_PADDING = {$IFDEF CPUX64} 4 {$ELSE} 0 {$ENDIF};
      STR_REC_SIZE = STR_REC_PADDING + 4 + 4 + 4;
      STR_LEN_OFFSET = 4;
      STR_REFCNT_OFFSET = 8;
      ARR_REFCNT_OFFSET = 4 + PTR_SIZE;

procedure _VM_STR_INCREF(PTR: Pointer);
procedure _VM_STR_DECREF(PTR: Pointer);

function GetStrRefCount(const Str: string): Integer; inline;
function GetArrRefCount(const Arr: Pointer): Integer; inline;
function GetObjRefCount(const Obj: Pointer): Integer; inline;

implementation

uses IL.TypeInfo;

function GetStrRefCount(const Str: string): Integer;
begin
  Result := PInt32(NativeUInt(Str) - STR_REFCNT_OFFSET)^;
end;

function GetArrRefCount(const Arr: Pointer): Integer;
begin
  Result := PInt32(NativeUInt(Arr) - ARR_REFCNT_OFFSET)^;
end;

function GetObjRefCount(const Obj: Pointer): Integer; inline;
begin
  Result := PVMObjHeader(PByte(Obj) - SizeOf(TVMObjHeader)).RefCount;
end;

procedure _VM_STR_INCREF(PTR: Pointer);
var
  pRefCnt: PInt32;
begin
  if Assigned(PTR) then begin
    pRefCnt := PInt32(NativeUInt(PTR) - STR_REFCNT_OFFSET);
    if pRefCnt^ > -1 then
      AtomicIncrement(pRefCnt^);
  end;
end;

procedure _VM_STR_DECREF(PTR: Pointer);
var
  pRefCnt: PInt32;
begin
  if Assigned(PTR) then
  begin
    pRefCnt := PInt32(PByte(PTR) - STR_REFCNT_OFFSET);
    if pRefCnt^ > -1 then begin
      AtomicDecrement(pRefCnt^);
      if pRefCnt^ = 0 then
        FreeMemory(PByte(PTR) - STR_REC_SIZE);
    end;
  end;
end;


end.
