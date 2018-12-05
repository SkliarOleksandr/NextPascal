unit record_props_inline_1;

interface

implementation

type
  TRec = record
    function GetCnt: Int32; inline;
    property CNT: Int32 read GetCnt;  
  end;

function TRec.GetCnt: Int32;
begin
  Result := 3;
end;

var G: Int32; 
    R: TRec;

procedure Test;
begin
  G := R.CNT;
end;

initialization
  Test();

finalization
  Assert(G = 3);
end.