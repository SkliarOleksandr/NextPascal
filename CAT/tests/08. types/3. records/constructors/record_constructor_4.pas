unit record_constructor_4;

interface

implementation

type
  TRec = record
    constructor Init;
    destructor Final;
  end;

var G: Int32;

constructor TRec.Init;
begin
  G := 5;
end;

destructor TRec.Final;
begin
  G := 6;
end;

var P: ^TRec;

procedure Test;
begin
  New(P);
  Assert(G = 5);
  Free(P);
  Assert(G = 6);  
end;

initialization
  Test();

finalization

end.