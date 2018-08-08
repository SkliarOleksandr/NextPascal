unit VM_Variants;

interface

{$IFDEF FPC}
  {$mode delphiunicode}
{$ENDIF}

implementation

uses System.Variants, VM.Invoke;

const
  _Variants = 'Variants';

procedure _VarFromInt(var Dst: Variant; const Src: Int32);
begin
  Dst := Src;
end;

procedure _VarFromUStr(var Dst: Variant; const Src: UnicodeString);
begin
  Dst := Src;
end;

procedure _VarToInt(const Src: Variant; var Dst: Int32);
begin
  Dst := Src;
end;

procedure _VarToUStr(const Src: Variant; var Dst: String);
begin
  Dst := Src;
end;

procedure _VarClear(var Src: Variant);
begin
  VarClear(Src);
end;

procedure RegisterVariants;
begin

  RegisterProc(_Variants, 'VarIsNull', @VarIsNull);
  RegisterProc(_Variants, 'VarIsEmpty', @VarIsEmpty);


  RegisterProc(_Variants, '_VarFromInt', @_VarFromInt);
  RegisterProc(_Variants, '_VarFromUStr', @_VarFromUStr);
  RegisterProc(_Variants, '_VarToInt', @_VarToInt);
  RegisterProc(_Variants, '_VarToUStr', @_VarToUStr);
  RegisterProc(_Variants, '_VarClear', @_VarClear);

  with RegisterType(_Variants, 'Variant', nil) do
  begin
    //RegisterMethod('ImplicitFromF32', @F32_TO_ExtendedF);
    //RegisterMethod('ImplicitFromF64', @F64_TO_ExtendedF);
  end;

end;

initialization
  RegisterVariants;

end.
