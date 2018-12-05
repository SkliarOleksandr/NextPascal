unit notnull_4;

interface

implementation

type
  TRec = record  
    p1: Pointer!;
    p2: Pointer?;   
  end;

var
  R: TRec;
  
procedure Test;
begin
  R.P1 := nil;
end;

initialization
  Test();

finalization

end.