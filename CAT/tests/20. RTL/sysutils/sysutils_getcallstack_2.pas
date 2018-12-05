unit sysutils_getcallstack_2;

interface

implementation

uses sys.rtti, sys.utils;

var CS: TCallStack;
    S0, S1, S2: string;

procedure TestInner;
begin
  CS := GetCallStack();
end;
                
procedure Test;
begin
  TestInner();
  S0 := CS[0].ProcInfo.Name;
  S1 := CS[1].ProcInfo.Name;      
  S2 := CS[2].ProcInfo.Name;  
end;

initialization    
  Test();

finalization
  Assert(Length(CS) = 3);
  Assert(S0 = 'TestInner');
  Assert(S1 = 'Test');
  Assert(S2 = '$initialization');    

end.