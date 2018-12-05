unit record_properties_3;

interface

implementation

type
  TRec = record
  private
    FData: Int32;
  public  
    property Data: Int32 read FData;
    function Get: Int32;    
  end;

var R: TRec;
    G: Int32;

function TRec.Get: Int32;
begin
  Result := Data;
end;

procedure Test;
begin
  R.FData := 12;
  G := R.Get();
end;

initialization
  Test();

finalization
  Assert(G = 12);
end.