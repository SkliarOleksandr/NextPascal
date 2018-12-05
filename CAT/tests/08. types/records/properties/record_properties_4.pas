unit record_properties_4;

interface

implementation

type 
  TRec1 = record
    a, b: Int32;
  end; 
  TRec2 = record
    FX: Int32;
    FR: TRec1;
    function GetR: TRec1;
    procedure SetR(const Value: TRec1);
    property R: TRec1 read FR write FR;
  end;

var
  V: TRec2;

function TRec2.GetR: TRec1;
begin
  Result := FR;
end;

procedure TRec2.SetR(const Value: TRec1);
begin
  FR := Value;
end;

procedure Test;
begin
  V.FX := 5;    
  V.R.a := 1;
  V.R.b := 2;  
end;

initialization
  Test();

finalization
  Assert(V.FX = 5);
  Assert(V.R.a = 1);
  Assert(V.R.b = 2);  

end.