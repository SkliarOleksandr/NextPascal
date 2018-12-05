unit tetris_1;
interface

type
  TCanvasColor = (
    ccNone,
    ccBlack,
    ccMaroon
  );

  TTetrisFig = (
   tfI,
   tfJ,
   tfL,
   tfO,
   tfS,
   tfZ,
   tfT
  );

  TPt = record
    X, Y: Integer;                      
  end; 

implementation

uses VM_System;

const 
  CRad = 15;
  WndPosX = CRad; 
  WndPosY = CRad;  
  
  VK_SPACE = 32;  
  VK_LEFT = 37;
  VK_RIGHT = 39;
  VK_UP = 38;  
  VK_DOWN = 40; 
  VK_HOME = 36;
     
const FData: array [TTetrisFig, 4, 4] of TPt = 
  ( 
    ( // tfI
      ([0, 0], [0, 1], [0, 2], [0, 3]),   // Ang = 0
      ([-1, 0], [0, 0], [1, 0], [2, 0]),  // Ang = 90
      ([0, 0], [0, 1], [0, 2], [0, 3]),   // Ang = 180   
      ([-1, 0], [0, 0], [1, 0], [2, 0])   // Ang = 270
    ),               
    ( // tfJ
      ([0, 0], [0, 1], [0, 2], [-1, 2]), 
      ([1, 2], [0, 2], [-1, 2], [-1, 1]),
      ([0, 2], [0, 1], [0, 0], [1, 0]),
      ([-1, 1], [0, 1], [1, 1], [1, 2]) 
    ),       
    ( // tfL
      ([0, 0], [0, 1], [0, 2], [1, 2]), 
      ([1, 1], [0, 1], [-1, 1], [-1, 2]),
      ([0, 2], [0, 1], [0, 0], [-1, 0]),
      ([-1, 2], [0, 2], [1, 2], [1, 1]) 
    ),  
    ( // tfO
      ([0, 0], [1, 0], [1, 1], [0, 1]), 
      ([0, 0], [1, 0], [1, 1], [0, 1]),
      ([0, 0], [1, 0], [1, 1], [0, 1]),
      ([0, 0], [1, 0], [1, 1], [0, 1]) 
    ),      
    ( // tfS
      ([0, 0], [0, 1], [1, 1], [1, 2]), 
      ([1, 1], [0, 1], [0, 2], [-1, 2]),
      ([0, 0], [0, 1], [1, 1], [1, 2]),
      ([1, 1], [0, 1], [0, 2], [-1, 2]) 
    ),      
    ( // tfZ
      ([0, 0], [0, 1], [-1, 1], [-1, 2]), 
      ([-1, 1], [0, 1], [0, 2], [1, 2]),
      ([0, 0], [0, 1], [-1, 1], [-1, 2]),
      ([-1, 1], [0, 1], [0, 2], [1, 2]) 
    ),      
    ( // tfT
      ([0, 1], [-1, 2], [0, 2], [1, 2]), 
      ([0, 0], [0, 1], [1, 1], [0, 2]),
      ([-1, 1], [0, 1], [1, 1], [0, 2]),
      ([0, 0], [0, 1], [0, 2], [-1, 1]) 
    )                   
  );   

procedure DrawCircle(X, Y: Integer; CColor: TCanvasColor); external 'Canvas';
procedure ProcessMessages; external 'Canvas';
procedure ClearCanvas; external 'Canvas';
function GetKey: Integer; external 'Sys';

var 
  Buf: array [10, 20] of Boolean;  

procedure PrintBorder;
begin
  for var i := 0 to 20 do
  begin 
    DrawCircle(WndPosX, WndPosY + i*CRad, ccBlack);
    DrawCircle(WndPosX + 11*CRad, WndPosY + i*CRad, ccBlack);
  end;  
  for var i := 1 to 10 do
    DrawCircle(WndPosX + i*CRad, WndPosY + CRad*20, ccBlack);    
end;

function NewFig: TTetrisFig;
begin
  var RV := Random(Ord(High(TTetrisFig)) + 1); 
  Result := TTetrisFig(RV);
end;

procedure RepaintAll;
begin
  PrintBorder();  
  // рисуем все фигуры
  for var X := 0 to 9 do
    for var Y := 0 to 19 do
      if Buf[X, Y] then
        DrawCircle(WndPosX + CRad + X*CRad, WndPosY + Y*CRad, ccMaroon)
      else
        DrawCircle(WndPosX + CRad + X*CRad, WndPosY + Y*CRad, ccNone);  
end;

procedure PrintFigure(Fig: TTetrisFig; Ang: Int32; Pos: TPt);
begin 
  // рисуем новую
  for var i := 0 to 3 do
  begin
    var Pt := FData[Fig, Ang, i]; 
    Buf[Pos.X + Pt.X, Pos.Y + Pt.Y] := True;  
  end;
  RepaintAll();
end;

procedure DelLine(LineY: Int32);
begin
  for var Y := LineY downto 1 do
  begin
    for var X := 0 to 9 do
      Buf[X, Y] := Buf[X, Y - 1];   
    RepaintAll();
  end;
end;

procedure CheckCompleteLine;
begin
  var DY := 0;
  for var Y := 19 downto 0 do
  begin
    var Full := False;
    for var X := 0 to 9 do
    begin
      Full := Buf[X, Y + DY];
      if not Full then
        break; 
    end;
    if Full then
    begin
      DelLine(Y + DY);
      Inc(DY);
    end;    
  end;        
end;

function CheckAndPrintFig(Fig: TTetrisFig; const Pos, NPos: TPt; const Ang, NAng: Integer): Boolean;
begin
  // временно стираем теку/щую фигуру
  for var i := 0 to 3 do
  begin
    var Pt := FData[Fig, Ang, i];  
    Buf[Pos.X + Pt.X, Pos.Y + Pt.Y] := False;  
  end;
  
  var Busy := False;
  for var i := 0 to 3 do
  begin
    var Pt := FData[Fig, NAng, i];    
    var X := NPos.X + Pt.X;
    var Y := NPos.Y + Pt.Y;
    
    Busy := X < 0 or X > 9 or Y < 0 or Y > 19;
    if Busy then
      Break;         
      
    Busy := Buf[X, Y];  
    if Busy then
      Break;   
  end;
  
  if not Busy then
  begin
    // рисуем новую фигуру   
    PrintFigure(Fig, NAng, NPos);    
  end else
    // восстанавливаем текущую фигуру  
    PrintFigure(Fig, Ang, Pos);  
     
  Result := not Busy;   
end;

procedure MainLoop;
var
  PPos, NPos: TPt;
  Key: Integer;        // клавиша
  Fig: TTetrisFig;     // фигура
  PAng, NAng: Int32;    // угол
  Time: Integer;
begin
  exit;
  Time := 0;
  PAng, NAng := 0;  
  Fig := NewFig();
  PPos := [0, 0];
  NPos := [4, 0];  
  while True do       
  begin                        
    Key := GetKey();
    case Key of           
      VK_LEFT: dec(NPos.X); 
      VK_RIGHT: inc(NPos.X);
      VK_UP: begin
        inc(NAng);
        if NAng > 3 then
          NAng := 0;
      end;    
      VK_DOWN: inc(NPos.Y);
      VK_HOME: Exit;       
    end;      
         
    // влево-вправо/поворот                            
    if (NPos.X <> PPos.X) or (NAng <> PAng) then
    begin
      if CheckAndPrintFig(Fig, PPos, NPos, PAng, NAng) then
      begin
        PPos := NPos;
        PAng := NAng;      
      end else begin
        NPos := PPos;
        NAng := PAng;        
      end;       
    end;
                    
    // движение вниз                          
    if NPos.Y > PPos.Y then
    begin
      NPos.Y := PPos.Y + 1; // вниз только на одну клетку  
      if CheckAndPrintFig(Fig, PPos, NPos, PAng, NAng) then      
      begin
        PPos := NPos;
      end else begin
        // если мы уперлись вниз
        CheckCompleteLine();
        Fig := NewFig();
        NPos := [4, 0];
        PPos := [0, 0];          
      end;               
    end;             
               
    // авто-сдвиг вниз
    inc(Time);
    if Time > 5 then
    begin
      Inc(NPos.Y);
      Time := 0;           
    end;                
                           
    ProcessMessages(); 
    Sleep(100);
  end;
end;

procedure Start;
begin
  PrintBorder();
  MainLoop();
end;

initialization
  Start();

end.