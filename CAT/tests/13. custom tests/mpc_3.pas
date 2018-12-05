unit mpc_3;
interface
uses system;
type
  TMPCObjectResult = (mpcResultOK, mpcResultWait, mpcResultError);
  TLightState = (lsRed = 0,lsRedYellow = 1,lsYellow = 2,lsYellowGreen = 3,lsGreen = 4,lsWhite = 5,lsBlue = 6,lsUnknown = 7);
  TSwitchState = (swPlus = 0,swMinus = 1,swInProgress = 2);
  TRTState = (rtFree = 0,rtInRoute = 1,rtBuzy = 2);
  TDirection = (dForward = 0,dBackward = 1);
  TSWRTState = (swrtPlusBuzy = 0,swrtPlusFree = 1,swrtPlusInRoute = 2,swrtMinusBuzy = 3,swrtMinusFree = 4,swrtMinusInRoute = 5);
  TRTTriggerState = (rttsWait = 0,rttsBuzy = 1);
  //=====================================
  TSignal = class;
  TRailTrack = class;
  TTRain = class;
  TSwitch = class;
  TSWRailTrackLR = class;
  TSWRailTrackRL = class;
  TSWRT33 = class;
  TBaseObj = class;
  Ttrigger1 = class;
  Ttrigger2 = class;
  TCMD1 = class;
  //=====================================
  TSignal = class
    Tick: Int32;
    State: TLightState;
    RT1: TRailTrack;
    RT2: TRailTrack;
    RT3: TRailTrack;
    RT4: TRailTrack;
    function SET_RED: TMPCObjectResult;
  end;
  TRailTrack = class
    Tick: Int32;
    State: TRTState;
    Next: TRailTrack;
    Prev: TRailTrack;
    ASignal: TSignal;
    Train: TTRain;
  end;
  TTRain = class
    Ticks: Int32;
    FirstRT: TRailTrack;
    Direction: TDirection;
    LastRT: TRailTrack;
    NextRT: TRailTrack;
  end;
  TSwitch = class
    Tick: Int32;
    PlusRT: TRailTrack;
    MinusRT: TRailTrack;
    State: TSwitchState;
  end;
  TSWRailTrackLR = class(TRailTrack)
    Tick: Int32;
    State: TRTState;
    Next: TRailTrack;
    Prev: TRailTrack;
    ASignal: TSignal;
    Train: TTRain;
    SW: TSwitch;
    SWRTState: TSWRTState;
  end;
  TSWRailTrackRL = class(TRailTrack)
    Tick: Int32;
    State: TRTState;
    Next: TRailTrack;
    Prev: TRailTrack;
    ASignal: TSignal;
    Train: TTRain;
    SW: TSwitch;
    SWRTState: TSWRTState;
  end;
  TSWRT33 = class(TRailTrack)
    Tick: Int32;
    State: TRTState;
    Next: TRailTrack;
    Prev: TRailTrack;
    ASignal: TSignal;
    Train: TTRain;
    SW: TSwitch;
    SWRTState: TSWRTState;
    function Execute: TMPCObjectResult;
  end;
  TBaseObj = class
    CurDT: Int32;
    MaxDT: Int32;
  end;
  Ttrigger1 = class
    RT: TRailTrack;
    Train: TTRain;
    SW1: TSwitch;
    SW2: TSwitch;
    State: Int32;
  end;
  Ttrigger2 = class
    RT: TRailTrack;
    Train: TTRain;
    SW1: TSwitch;
    SW2: TSwitch;
    State: Int32;
  end;
implementation

function TSignal.SET_RED: TMPCObjectResult;
begin
// SET_RED();
end;

function TSWRT33.Execute: TMPCObjectResult;
begin     
  if (Train.FirstRT = TObject(self)) or
     (Train.LastRT = TObject(self)) then
    State := rtBuzy
  else     
    State := rtFree;{}
    
  if Assigned(SW) then
  begin
    case SW.State of
      swMinus: Next := SW.MinusRT;
      swPlus: Next := SW.PlusRT;      
    end;
             
    case State of    
      rtBuzy: case SW.State of
                swMinus: SWRTState := swrtMinusBuzy; 
                swPlus: SWRTState := swrtPlusBuzy;     
              end;    
      rtFree: case SW.State of
                swMinus: SWRTState := swrtMinusFree; 
                swPlus: SWRTState := swrtPlusFree;     
              end;              
    end;              
  end;                         
  Tick := Tick + 1; {}
  ASignal.SET_RED();     
end;


var
  TR: TTRain;
  SW: TSwitch;
  SR: TSWRT33;
  RTS: TRTState; 
  SWRTState: TSWRTState;

initialization
  SW := TSwitch.Create();
  TR := TTRain.Create();
  SR := TSWRT33.Create();
  TR.FirstRT := SR;
  SR.SW := SW;
  SR.Train := TR; 
  SR.Execute();
  RTS := SR.State;
  SWRTState := SR.SWRTState; 
 
finalization
  Assert(RTS = rtBuzy);
  Assert(SWRTState = swrtPlusBuzy);
  SR.Train := nil;

end.