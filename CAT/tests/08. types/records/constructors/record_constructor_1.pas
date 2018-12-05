unit record_constructor_1;

interface

implementation

type
  TRec = record
    x: int32;
    constructor Init;          
    destructor Final;     
  end;

var
  G: Int32;

constructor TRec.Init;
begin
  G := 1;
end;
  
constructor TRec.Final;
begin
  G := 2; 
end;
  
procedure Test;
var
  R: TRec;
begin       
  Assert(G = 1); 
  R.x := 33;
end;

initialization
  Test();

finalization
  Assert(G = 2);

end.