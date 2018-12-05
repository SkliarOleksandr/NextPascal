unit UN_TTRain;
interface
uses System;
type
  TLightState = (lsRed = 0,lsRedYellow = 1,lsYellow = 2,lsYellowGreen = 3,lsGreen = 4,lsWhite = 5,lsBlue = 6,lsUnknown = 7);
  TSwitchState = (swPlus = 0,swMinus = 1,swInProgress = 2);
  TRTState = (rtFree = 0,rtInRoute = 1,rtBuzy = 2);
  TSignal = class;
  TRailTrack = class;
  TTRain = class;
  TSwitch = class;
  TSWRailTrack = class;
  TSignal = class
    Tick: Int32;
    State: TLightState;
    RT1: TRailTrack;
    RT2: TRailTrack;
    RT3: TRailTrack;
    RT4: TRailTrack;
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
    Direction: Int32;
    LastRT: TRailTrack;
    NextRT: TRailTrack;
  end;
  TSwitch = class
    Tick: Int32;
    PlusRT: TRailTrack;
    MinusRT: TRailTrack;
    State: TSwitchState;
  end;
  TSWRailTrack = class
    Tick: Int32;
    State: TRTState;
    Next: TRailTrack;
    Prev: TRailTrack;
    ASignal: TSignal;
    Train: TTRain;
    SW: TSwitch;
  end;
  TRec = class
  private
    Ticks: Int32;
    FirstRT: TRailTrack;
    Direction: Int32;
    LastRT: TRailTrack;
    NextRT: TRailTrack;
  public
    procedure Execute;
  end;

procedure Run(const Obj: TRec); export;

implementation


procedure TRec.Execute;
var
  RT: TRailTrack;  
//  S: ^TSignal;
begin
  RT := NextRT;
  if Assigned(RT) then
  begin
    RT.State := rtBuzy;  // занимаем 
  
    //if Assigned(LastRT) then
    //  LastRT.State := rtFree;        // освобождаем последнюю занятую цепь     
   
    //LastRT := FirstRT;   
    //FirstRT := NextRT;
    //RT := RT.Next;     
    //NextRT := RT;
  end;
  //Ticks := Ticks + 1;       
end;
procedure Run(const Obj: TRec); 
begin
  Obj.Execute();
end;
end.