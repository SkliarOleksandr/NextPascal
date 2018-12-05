unit UN_train1;
interface
type
  TSignal = record
    State: Int32;
  end;
  
  TRt = record
    State: Int32;
    Next: ^TRt;
    Prev: ^TRt;
    ASignal: ^TSignal;
  end;
  
  TRec = record
  private
    FirstRT: ^TRt;
    LastRT: ^TRt;
    NextRT: ^TRt;        
    Direction: Int32;
    Ticks: Int32;
  public
    procedure Execute;
  end;

implementation

var
  T: TRec;
  R1, R2, R3, R4, R5: TRt; 

procedure Init;
begin
  T.FirstRT := nil;  
  T.LastRT := nil;    
  T.NextRT := @R1;
  T.Direction := 0;
  T.Ticks := 0;  
  
  R1.Next := @R2;
  R1.Prev := nil;  

  R2.Next := @R3;  
  R2.Prev := @R1;  
  
  R3.Next := @R4;  
  R3.Prev := @R2;
  
  R4.Next := @R5;  
  R4.Prev := @R3;
  
  R5.Next := nil;  
  R5.Prev := @R4;       
end;

procedure TRec.Execute;
var
  RT, LRT: ^TRt;  
begin
  RT := NextRT;
  if Assigned(RT) then
  begin
    RT.State := 1 + Direction;  
  end;   
        
        
  if Assigned(LastRT) then
  begin
    LastRT.State := 0;           
  end;       
  
  LastRT := FirstRT;   
  FirstRT := NextRT;
  RT := RT.Next;     
  NextRT := RT;
  Ticks := Ticks + 1;       
end;

initialization
  Init();
  T.Execute();
  T.Execute();
  T.Execute();      
///  T.Execute();    

end.