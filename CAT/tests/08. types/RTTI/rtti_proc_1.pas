unit rtti_proc_1;

interface

implementation

uses sys.rtti;

var 
  ti: TObject;
  pi: TRTTIProcedure;
  S: string;

procedure Test;
begin
  ti := typeinfo(Test);
  pi := ti as TRTTIProcedure;
  S := pi.name;
end;

initialization
  Test();

finalization
  Assert(S = 'Test');
end.