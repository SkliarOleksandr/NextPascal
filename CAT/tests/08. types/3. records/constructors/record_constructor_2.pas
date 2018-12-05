unit record_constructor_2;

interface

implementation

type
  TRec = record
    x: int32;
    constructor Init;
    destructor Final;      
  end;

constructor TRec.Init;
begin
  X := 111;
end;

constructor TRec.Final;
begin
  X := 222;
end;

var
  R: TRec;
  
initialization
  Assert(R.X = 111);

finalization
  Assert(R.X = 111);
end.