unit VM_DateUtils;

interface

uses SysUtils, VM.Invoke, DateUtils;

implementation

const
  DU = 'DateUtils';

  // оригинальная функция в FPC работает не корректно
  function HoursBetweenMy(const ANow, AThen: TDateTime): Int64;
  begin
    Result := Abs(Round((ANow - AThen)*MSecsPerDay)) div (MSecsPerSec * SecsPerMin * MinsPerHour);
    //Result := Abs(Round(ANow*MSecsPerDay) - Round(AThen*MSecsPerDay)) div (MSecsPerSec * SecsPerMin * MinsPerHour);
  end;

procedure RegisterUnit;
begin
  RegisterProc(DU, 'YearsBetween', @DateUtils.YearsBetween);
  RegisterProc(DU, 'MonthsBetween', @DateUtils.MonthsBetween);
  RegisterProc(DU, 'WeeksBetween', @DateUtils.WeeksBetween);
  RegisterProc(DU, 'DaysBetween', @DateUtils.DaysBetween);
  RegisterProc(DU, 'HoursBetween', @HoursBetweenMy);
  RegisterProc(DU, 'MinutesBetween', @DateUtils.MinutesBetween);
  RegisterProc(DU, 'SecondsBetween', @DateUtils.SecondsBetween);
  RegisterProc(DU, 'MilliSecondsBetween', @DateUtils.MilliSecondsBetween);

  RegisterProc(DU, 'IncYear', @DateUtils.IncYear);
  RegisterProc(DU, 'IncWeek', @DateUtils.IncWeek);
  RegisterProc(DU, 'IncDay', @DateUtils.IncDay);
  RegisterProc(DU, 'IncHour', @DateUtils.IncHour);
  RegisterProc(DU, 'IncMinute', @DateUtils.IncMinute);
  RegisterProc(DU, 'IncSecond', @DateUtils.IncSecond);
  RegisterProc(DU, 'IncMilliSecond', @DateUtils.IncMilliSecond);
end;

initialization
  RegisterUnit;

end.
