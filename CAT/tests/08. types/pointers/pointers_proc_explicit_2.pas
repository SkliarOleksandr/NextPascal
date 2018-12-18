unit pointers_proc_explicit_2;

interface

implementation

type
  TProc = procedure(a, b: Int32; const s: string);

var 
  P: Pointer;
  GA, GB: Int32;
  GS: string;

procedure SetG(a, b: Int32; const s: string);
begin
  GA := a;
  GB := b;
  GS := s;   
end;

procedure Test;
begin
  P := @SetG;
  TProc(P)(44, 33, 'str');
end;

initialization
  Test();

finalization
  Assert(GA = 44);
  Assert(GB = 33);  
  Assert(GS = 'str');
  
end.