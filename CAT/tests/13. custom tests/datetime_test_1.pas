unit datetime_test_1;

interface

implementation

uses VM_System, VM_SysUtils, VM_DateUtils;

type
  TCalcResult = (CalcSuccess, CalcExpireDate, CalcNotEnoughMoney);


var
  R: TCalcResult;
  DT, NDT: TDateTime;

procedure Test;
begin
  DT := Now();
  NDT := Now(); 
  if DT > NDT then  
    R := CalcSuccess
  else   
    R := CalcExpireDate;
end;

initialization 
  Test();
  
finalization
  Assert(R = CalcExpireDate);  
  
end.