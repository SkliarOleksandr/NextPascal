unit VM_System;

interface

uses VM.Invoke, SysUtils; // system

implementation

const
  SYS = 'SYSTEM';

procedure F32_TO_Extended(Result: PExtended; const Value: Int32);
begin
  Result^ := PSingle(@Value)^;
end;

procedure F64_TO_Extended(Result: PExtended; const Value: Int64);
begin
  Result^ := PDouble(@Value)^;
end;

procedure F32_TO_ExtendedF(Result: PExtended; const Value: Single);
begin
  Result^ := Value;
end;

procedure F64_TO_ExtendedF(Result: PExtended; const Value: Double);
begin
  Result^ := Value;
end;

procedure _Sleep(MSecs: Cardinal);
begin
  Sleep(MSecs);
end;

procedure RegisterVM_System;
begin
  with RegisterType(SYS, 'TObject', pointer(System.TObject)) do
  begin
    RegisterMethod('Create', @TObject.Create);
    RegisterMethod('Free', @TObject.Free);
  end;

  with RegisterType(SYS, 'IInterface', nil) do
  begin
    RegisterIntfMethod('QueryInterface');
    RegisterIntfMethod('_AddRef');
    RegisterIntfMethod('_Release');
  end;

  with RegisterType(SYS, 'Extended', nil) do
  begin
    RegisterMethod('ImplicitFromF32', @F32_TO_ExtendedF);
    RegisterMethod('ImplicitFromF64', @F64_TO_ExtendedF);
  end;

  RegisterProc(SYS, 'Sleep', @_Sleep);
  RegisterProc(SYS, 'Random', @Random);

end;

initialization
  RegisterVM_System;

end.
