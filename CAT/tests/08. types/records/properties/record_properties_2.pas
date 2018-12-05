unit record_properties_2;
interface

type
  
  TRec = record
    a, b: int32; 
    function GetA: Int32;
    function GetB: Int32;
    procedure SetA(Value: Int32);
    procedure SetB(Value: Int32);
    property AR: Int32 read GetA;  
    property ARW: Int32 read GetA write SetA;
    property BR: Int32 read GetB;  
    property BRW: Int32 read GetB write SetB;    
  end;    

var
  R: TRec;  
  GA, GB: Int32;  
  
implementation  
  
function TRec.GetA: Int32;
begin
  Result := a;
end;

function TRec.GetB: Int32;
begin
  Result := b;
end;

procedure TRec.SetA(Value: Int32);
begin
  a := Value;
end;

procedure TRec.SetB(Value: Int32);
begin
  b := Value;
end; 

procedure Test;
begin
  R.A := 5;
  R.B := 6;
  GA := R.AR;
  GB := R.BR;  
end;

initialization
  Test();
  
finalization
  Assert(GA = 5);
  Assert(GB = 6);  
end.  