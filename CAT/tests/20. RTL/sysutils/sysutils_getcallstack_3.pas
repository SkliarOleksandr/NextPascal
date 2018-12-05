unit sysutils_getcallstack_3;

interface

implementation

uses sys.rtti, sys.utils;

type 
  TStrArray = array of string;   

var CS: TCallStack;
    Names: TStrArray;
    
function GetProcNames(CS: TCallStack): TStrArray;
begin
  SetLength(Result, Length(CS)); 
  for var i := 0 to Length(CS) - 1 do
    Result[i] := CS[i].ProcInfo.Name;    
end;

procedure TestInner(c: integer);
begin
  if c < 3 then
  begin
    inc(c);
    TestInner(c);
  end else if c = 3 then
  begin   
    CS := GetCallStack();
    Names := GetProcNames(CS);
  end;  
end;
                
procedure Test;
begin
  TestInner(0);   
end;

initialization    
  Test();

finalization
  Assert(Length(CS) = 6);
  Assert(Names[0] = 'TestInner');
  Assert(Names[1] = 'TestInner');
  Assert(Names[2] = 'TestInner');
  Assert(Names[3] = 'TestInner');      
  Assert(Names[4] = 'Test');    
  Assert(Names[5] = '$initialization');  

end.