unit SysUtils;

interface

uses System, VM_System;
 
const 
  SU_LIB = 'SysUtils';

procedure Sleep(milliseconds: UInt32); external SU_LIB;

function Now: TDateTime; external SU_LIB;
function FormatDateTime(const Format: string; DateTime: TDateTime): string; external SU_LIB;   
function Format(const Fmt: string; const Params: openarray of TVarRec): string; external SU_LIB;    
function IntToStr(Value: Integer): string; external SU_LIB;
function StrToInt(const S: string): Integer; external SU_LIB;   

function StrToDate(const S: string): TDateTime; overload; external SU_LIB;
function StrToTime(const S: string): TDateTime; overload; external SU_LIB;
function StrToDateTime(const S: string): TDateTime; overload; external SU_LIB;
function TryStrToDate(const S: string; out Value: TDateTime): Boolean; overload; external SU_LIB;
function TryStrToTime(const S: string; out Value: TDateTime): Boolean; overload; external SU_LIB;
function TryStrToDateTime(const S: string; out Value: TDateTime): Boolean; overload; external SU_LIB;


end.