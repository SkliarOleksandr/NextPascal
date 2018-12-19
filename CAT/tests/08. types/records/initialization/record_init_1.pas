unit record_init_1;

interface

implementation

type
  TRec = record
    a, b: Int32;
  end;
 
var
  R: TRec {= (a: 1, b: 2)};  

procedure Test;
begin

end;

initialization
  Test();

finalization

end.