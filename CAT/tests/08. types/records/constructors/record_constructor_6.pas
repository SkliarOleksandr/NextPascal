unit record_constructor_6;

interface

implementation

type
  TRec = record
    FData: Int32;
    constructor Init; 
    destructor Final;
  end;

var G: Int32;

constructor TRec.Init;
begin
  FData := 12;
end;

destructor TRec.Final;
begin
  G := 13;
end;

function GetRec: TRec;
begin
  Assert(Result.FData = 12);
end; 

procedure Test;
var 
  R: TRec;
begin
  R := GetRec();
end;

initialization
  Test();

finalization
  Assert(G = 13); 

end.