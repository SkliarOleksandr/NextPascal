unit plc_1;

interface
uses system;
type
  TPLCObjectResult = (plcResultOK, plcResultWait, plcResultError);
  TModBusDeviceStatus = (mdbNormal = 0,mdbError = 1);
  //=====================================
  PM2827 = class;
  TA2_c = class;
  TL_Ms = class;
  //=====================================
  PM2827 = class
    J1_1: Boolean;
    J1_2: Boolean;
    J1_3: Boolean;
    J1_4: Boolean;
    J1_5: Boolean;
    J1_6: Boolean;
    J1_7: Boolean;
    J1_8: Boolean;
    J1_9: Boolean;
    J1_10: Boolean;
    J1_11: Boolean;
    J1_12: Boolean;
    J2_1: Boolean;
    J2_2: Boolean;
    J2_3: Boolean;
    J2_4: Boolean;
    J2_5: Boolean;
    J2_6: Boolean;
    J2_7: Boolean;
    J2_8: Boolean;
    J2_9: Boolean;
    J2_10: Boolean;
    J2_11: Boolean;
    J2_12: Boolean;
    Status: TModbusDeviceStatus;
    Data: array[4] of byte;
  end;
  TA2_c = class(PM2827)
    property I_1PI_F: Boolean read J1_1;
    property I_2PI_F: Boolean read J1_2;
    property I_ChDP_F: Boolean read J1_3;
    property I_612SP_F: Boolean read J1_5;
    property I_1416SP_F: Boolean read J1_7;
    property I_1P_F: Boolean read J1_9;
    property I_IIP_F: Boolean read J1_11;
    property I_3P_F: Boolean read J2_1;
    property I_5P_F: Boolean read J2_3;
    property I_7P_F: Boolean read J2_5;
    property I_Ch1IP1_F: Boolean read J2_7;
    property I_NG1_F: Boolean read J2_9;
    property I_1VV_F: Boolean read J2_10;
    property I_2VV_F: Boolean read J2_11;
  end;
  TL_Ms = class
    A2_c: TA2_c;
    State: Boolean;
    Meandr: Boolean;
    MCnt: Int32;
    function Execute: TPLCObjectResult;
  end;
implementation

function TL_Ms.Execute: TPLCObjectResult;
begin
  State := not (A2_c.I_1PI_F AND A2_c.I_2PI_F AND A2_c.I_2VV_F);
  if State then
  begin
    if MCnt > 0 then 
      dec(MCnt)
    else begin
      Meandr := not Meandr;
      MCnt := 8;
    end;        
  end else
    Meandr := False;
end;

var
  A2_c: TA2_c;
  L_Ms: TL_Ms;
  
procedure Test;
begin
  A2_c := TA2_c.Create();
  L_Ms := TL_Ms.Create();  
  L_Ms.A2_c := A2_c;
  L_Ms.Execute();
end;

initialization
  Test();
end.
