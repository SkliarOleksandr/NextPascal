unit record_props_1;

interface

implementation

type
  TRec = record
    procedure SetCnt(value: Int32);
    property CNT: Int32 write SetCnt;  
  end;


var G: Int32; 
    R: TRec;

procedure TRec.SetCnt(value: Int32);
begin
  G := Value;
end;


procedure Test;
begin
  R.SetCnt(5);
  R.CNT := 5;
end;

initialization
  Test();

finalization
  Assert(G = 5);
end.