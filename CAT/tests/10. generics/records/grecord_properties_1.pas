unit grecord_properties_1;

interface
implementation

var
  G: Int32;

type
  TRec<T> = record
    FData: T;
    function GetP(Index: Int32): T;
    procedure SetP(Index: Int32; Value: T);
    property P[Index: Int32]: T read GetP write SetP;
  end;

implementation

function TRec<T>.GetP(Index: Int32): T;
begin
  Result := FData div Index;
end;

procedure TRec<T>.SetP(Index: Int32; Value: T);
begin
  FData := Value * Index;
end;

var
  R: TRec<Int32>;

procedure Test;
begin
  R.P[2] := 5; 
  G := R.P[2]; 
end;

initialization
  Test();
  
finalization
  Assert(G = 5);
  
end.  