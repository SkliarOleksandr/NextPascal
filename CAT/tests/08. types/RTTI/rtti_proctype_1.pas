unit rtti_proctype_1;

interface

implementation

uses sys.rtti;

type
  TProc = procedure (a, b: int32);

var
  ti: TRTTIProcType;
  S: string;
  
procedure Test;
begin
  ti := typeinfo(TProc) as TRTTIProcType;
  Assert(ti.Name = 'TProc');
  Assert(ti.ParamsCount = 2);
  Assert(ti.ResultType = nil);
  Assert(ti.CallConvention = ConvNative);      
end;

initialization
  Test();

finalization

end.