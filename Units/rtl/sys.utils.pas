unit sys.utils;

interface

uses system, sys.rtti;

type
  TCallStackFrame = packed record
    CallAddr: Pointer;
    ProcInfo: TRTTI;
  end;	 
  TCallStack = array of TCallStackFrame;

function GetCallStack: TCallStack; inline;

implementation

function GetCallStack: TCallStack;
begin
  asm
    macro 'getcallstack', Result;
  end; 
end;	

end.