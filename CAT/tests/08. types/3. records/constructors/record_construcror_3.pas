unit record_constructor_3;

interface

implementation

type
  TRec = record
    x: int32;
    constructor Init; inline;
    destructor Final; inline;     
  end;

var G: Int32;

constructor TRec.Init;
begin
  G := 111;
end;

constructor TRec.Final;
begin
  G := 222;
end;

var
  R: TRec;
  
initialization
  Assert(G = 111);

finalization
  Assert(G = 111);
end.