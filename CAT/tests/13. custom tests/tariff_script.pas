unit tariff_test;
interface
uses System, VM_System, VM_SysUtils, VM_DateUtils;
implementation
type
  TCZonePeriod = packed record
    BeginDate: TDate;
    EndDate: TDate;
    ZoneInDate: TDateTime;
    LastUseDate: TDateTime;
    MAC: UInt32;
  end;
  TCWallet = packed record
    BeginDate: TDateTime;
    EndDate: TDateTime;
    RFU: array[0..7] of UInt8;
    MainWallet: Int32;
    BonusWallet: Int32;
    LastUseDate: TDateTime;
    MAC: UInt32;
  end;
  TCalcResult = (CalcSuccess, CalcExpireDate, CalcNotEnoughMoney);

var
  Hours: Int32;
  SDT: TDateTime;
  TotalSum: Int32;

function Calc(const CZonePeriod: TCZonePeriod; var CWallet: TCWallet; TRDate: TDateTime; BasePrice: Int32): TCalcResult;
begin
  SDT := CZonePeriod.ZoneInDate;
  Hours := HoursBetween(SDT, TRDate);
  if Hours <= 8 then
    TotalSum := Hours*BasePrice
  else
    TotalSum := (8*BasePrice) + (Hours - 8)*(BasePrice div 2);
  
  if CWallet.MainWallet >= TotalSum then
  begin
    CWallet.MainWallet := CWallet.MainWallet - TotalSum;
    CWallet.LastUseDate := TRDate;
    Result := CalcSuccess;
  end else
    Result := CalcNotEnoughMoney; 
end;

var
  Z: TCZonePeriod;
  W: TCWallet;
  R: TCalcResult;

var
  DT, NDT: TDateTime;
 
procedure Test;
begin
  DT := Now();
  NDT := IncHour(DT, 13);
  Z.ZoneInDate := NDT;
  W.MainWallet := 100;
  R := Calc(Z, W, DT, 10);
end;

initialization
  Test();
    
finalization
  Assert(R = CalcNotEnoughMoney);   
end.