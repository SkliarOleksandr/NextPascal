unit record_refs_3;

interface
type
  TRt = record
    State: Int32;
    Next: ^TRt;    
    Prev: ^TRt;
  end;

  TRec = record
    Curr: ^TRt;
    Direction: Int32;
    Ticks: Int32;
  end;

procedure Execute(const Self: ^TRec); 

implementation

var
  T: TRec;
  R1, R2, R3, R4, R5: TRt; 

procedure Init;
begin
  T.Curr := @R1;
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

procedure Execute(const Self: ^TRec);
var
  RT: ^TRt;  
begin
  RT := Self.Curr;
  if Assigned(RT) then
    RT.State := 1 + Self.Direction;

  if Self.Direction = 0 then
  begin
    if Assigned(RT.Prev) then
      RT.Prev.State := 0;  
  
    RT := RT.Next;
    if not Assigned(RT) then
    begin
      Self.Direction := 1;
      RT := Self.Curr; 
    end;
  end else begin
    if Assigned(RT.Next) then
      RT.Next.State := 0;  
   
    RT := RT.Prev;
    if not Assigned(RT) then
    begin
      Self.Direction := 0;
      RT := Self.Curr;
    end;  
  end;         
  Self.Curr := RT;
  Self.Ticks := Self.Ticks + 1;  
end;

initialization  
  Init();
  Execute(@T);
  Execute(@T);
  Execute(@T);
  Execute(@T);
  Execute(@T);
  
  Execute(@T);                   
  Execute(@T);
  Execute(@T);
  Execute(@T);
  Execute(@T);   
    
finalization  
  Assert(R1.State = 2);
  Assert(R5.State = 0);  
end.