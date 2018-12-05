unit record_props_indexed_2;

interface

type
  TRec = record
    function GetI(const Index: string): Int32;
    procedure SetI(const Index: string; Value: Int32);
    property Items[const Index: string]: Int32 read GetI write SetI;
  end;

implementation

var
  GR, GW: Int32;
  GS: string;

function TRec.GetI(const Index: string): Int32;
begin
  Result := Length(Index);
end;

procedure TRec.SetI(const Index: string; Value: Int32);
begin
  GS := Index;
  GW := Value;
end;

procedure Test;
var
  R: TRec;
begin
  GR := R.Items['asdf'];
  R.Items['hello'] := 8; 
end;

initialization
  Test();

finalization
  Assert(GR = 4);
  Assert(GW = 8);  
  Assert(GS = 'hello');  

end.