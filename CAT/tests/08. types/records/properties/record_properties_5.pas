unit record_properties_5;

interface

implementation

type
  TRec = record
  private
    FCnt: Int32;
    function GetCount: Int32;
  public
    property Count: Int32 read GetCount;
    procedure Run;    
  end;

var G: Int32;

function TRec.GetCount: Int32;
begin
  Result := FCnt;
end;

procedure TRec.Run;
begin
  G := Count;
end;

procedure Test;
var
  R: TRec;
begin
  R.FCnt := 12;
  R.Run();
end;

initialization
  Test();

finalization
  Assert(G = 12);

end.