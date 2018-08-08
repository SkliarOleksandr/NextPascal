unit DateUtils;

interface

uses System, VM_System;
 
const 
  DU = 'DateUtils';

function YearsBetween(const ANow, AThen: TDateTime): Int32; external DU;
function MonthsBetween(const ANow, AThen: TDateTime): Int32; external DU;
function WeeksBetween(const ANow, AThen: TDateTime): Int32; external DU;
function DaysBetween(const ANow, AThen: TDateTime): Int32; external DU;
function HoursBetween(const ANow, AThen: TDateTime): Int64; external DU;
function MinutesBetween(const ANow, AThen: TDateTime): Int64; external DU;
function SecondsBetween(const ANow, AThen: TDateTime): Int64; external DU;
function MilliSecondsBetween(const ANow, AThen: TDateTime): Int64; external DU;

function IncYear(const AValue: TDateTime; const ANumberOfYears: Integer = 1): TDateTime; external DU;
function IncWeek(const AValue: TDateTime; const ANumberOfWeeks: Integer = 1): TDateTime; external DU;
function IncDay(const AValue: TDateTime; const ANumberOfDays: Integer = 1): TDateTime; external DU;
function IncHour(const AValue: TDateTime; const ANumberOfHours: Int64 = 1): TDateTime; external DU;
function IncMinute(const AValue: TDateTime; const ANumberOfMinutes: Int64 = 1): TDateTime; external DU;
function IncSecond(const AValue: TDateTime; const ANumberOfSeconds: Int64 = 1): TDateTime; external DU;
function IncMilliSecond(const AValue: TDateTime; const ANumberOfMilliSeconds: Int64 = 1): TDateTime; external DU;

end.