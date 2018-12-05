unit visibility_test_3;
interface
type
  TRec1 = record
  private
    FF: Int32;
    procedure SetF(Value: Int32);
    function GetF: Int32;
  public
    property F: Int32 read GetF write SetF;
  end;  

  TRec2 = record
  private
    procedure SetF(Value: Int32);
    function GetF: Int32;
  public
    property F: Int32 read GetF write SetF;
  end;    

implementation
var 
  R1: TRec1;
  R2: TRec2;

procedure TRec1.SetF(Value: Int32); 
begin 
  FF := Value; 
end;
function TRec1.GetF: Int32; 
begin 
  Result := FF; 
end;
procedure TRec2.SetF(Value: Int32); 
begin 
  R1.F := Value; 
end;
function TRec2.GetF: Int32; 
begin 
  Result := R1.FF; 
end;

procedure Test;
begin
  R1.F := 12;  
end;

initialization
  Test();

finalization
  Assert(R1.F = 12);
  Assert(R2.F = 12);  
end.