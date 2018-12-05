unit record_props_indexed_1;
interface
implementation

var
  G, GI, GV: Int32;

type
  TRec = record
    function GetP(Index: Int32): Int32;
    procedure SetP(Index: Int32; Value: Int32);
    property P[Index: Int32]: Int32 read GetP write SetP;
  end;

implementation

function TRec.GetP(Index: Int32): Int32;
begin
  Result := Index;
end;

procedure TRec.SetP(Index: Int32; Value: Int32);
begin
  GI := Index;
  GV := Value;
end;

procedure Test;
var
  R: TRec;
begin
  G := R.GetP(5);
  R.P[12] := 55;  
end;

initialization
  Test();
  
finalization
  Assert(G = 5);
  Assert(GI = 12);  
  Assert(GV = 55); 
  
end.  