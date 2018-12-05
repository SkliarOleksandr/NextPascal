unit rtti_unitprocs_1;

interface

implementation

uses sys.rtti;

var
  U: TRTTIUnit; 
  Procs: TRTTIProcs;
  S1, S2, S3: string;

procedure Test;
begin
  U := GetCurrentUnit();
  Procs := U.Procs;
  S1 := Procs[0].Name;
  S2 := Procs[1].Name;
  S3 := Procs[2].Name;    
end;

initialization
  Test();

finalization
  Assert(S1 = 'Test');
  Assert(S2 = '$initialization');
  Assert(S3 = '$finalization');

end.