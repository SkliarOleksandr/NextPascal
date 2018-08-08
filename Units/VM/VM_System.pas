unit VM_System;

interface

const SYSLIB = 'SYSTEM';

type

  TDateTime = Float64; 
  TDate = Float64;
  TTime = Float64;

  Extended = packed record
    FBytes: array [10] of UInt8;
    operator Implicit(const Value: Float32): Extended; overload; external SYSLIB name 'ImplicitFromF32';    
    operator Implicit(const Value: Float64): Extended; overload; external SYSLIB name 'ImplicitFromF64';        
  end external SYSLIB;  

	// delphi TVarRec
  TVarRec = packed record    	  
    FAsInteger: NativeInt;    		
    FAsPointer: pointer absolute FAsInteger;  						
    FVType: NativeInt;			
    operator Implicit(const Value: Int32): TVarRec; overload; // inline;
    operator Implicit(const Value: UInt32): TVarRec; overload; // inline;    
    operator Implicit(constref Value: Int64): TVarRec; overload; //inline;    
    operator Implicit(const Value: Char): TVarRec; overload; //inline;  
    operator Implicit(const Value: Boolean): TVarRec; overload; //inline;           
    operator Implicit(const Value: string): TVarRec; overload; //inline;                     
    operator Implicit(const Value: AnsiString): TVarRec; overload; //inline;                     
    operator Implicit(constref Value: Extended): TVarRec; overload; //inline;
  end;
	
  IUnknown = interface
    procedure QueryInterface(const IID: TGUID; out Intf); stdcall; 
    function _AddRef: Int32; stdcall;
    function _Release: Int32; stdcall;     
  end external SYSLIB;	
  
  {
  function format(const Fmt: string; const Params: openarray of TVarRec): string; external SystemLib;
  procedure Test; export;}

  procedure Sleep(SleepMSecs: UInt32); external SYSLIB;   
  function Random(Range: Int32): Int32; external SYSLIB;     

implementation

const
  vtInteger       = 0;
  vtBoolean       = 1;
  vtChar          = 2;
  vtExtended      = 3;
  vtString        = 4;
  vtPointer       = 5;
  vtPChar         = 6;
  vtObject        = 7;
  vtClass         = 8;
  vtWideChar      = 9;
  vtPWideChar     = 10;
  vtAnsiString    = 11;
  vtCurrency      = 12;
  vtVariant       = 13;
  vtInterface     = 14;
  vtWideString    = 15;
  vtInt64         = 16;
  vtUnicodeString = 17;


operator TVarRec.Implicit(const Value: Int32): TVarRec;
begin
  Result.FAsInteger := Value;
  Result.FVType := vtInteger; 
end;

operator TVarRec.Implicit(const Value: UInt32): TVarRec;
begin
  Result.FAsInteger := Value;
  Result.FVType := vtInteger; 
end;

operator TVarRec.Implicit(constref Value: Int64): TVarRec;
begin
  Result.FAsPointer := @Value;
  Result.FVType := vtInt64;  
end;

operator TVarRec.Implicit(const Value: Char): TVarRec;
begin
  Result.FAsInteger := NativeInt(Value);
  Result.FVType := vtChar;  
end;

operator TVarRec.Implicit(const Value: Boolean): TVarRec;
begin
  Result.FAsInteger := NativeInt(Value);
  Result.FVType := vtBoolean;  
end;  

operator TVarRec.Implicit(const Value: string): TVarRec;
begin
  Result.FAsPointer := pointer(Value);
  Result.FVType := vtUnicodeString;  
end;  

operator TVarRec.Implicit(const Value: AnsiString): TVarRec;
begin
  Result.FAsPointer := pointer(Value);
  Result.FVType := vtAnsiString;  
end;  

operator TVarRec.Implicit(constref Value: Extended): TVarRec;
begin
  Result.FAsPointer := @Value;
  Result.FVType := vtExtended;  
end;  

{var
  S: string;

procedure Test;
begin
  S := format('fmt: %d %d %d', [1, 2, 3]);
end;}

end.