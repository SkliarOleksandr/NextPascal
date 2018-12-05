unit record_nested_types_1;

interface

type
  
  TRec = record
  type
    TTT = int32;
    TA = array [2] of TTT;
  var  
    A: ttt;
    B: TA;  
  end;

var
  R: TRec;
  AR: TRec.TA;
  
implementation

procedure Test;
begin
  R.A := 5;
  AR[0] := 11;
  AR[1] := 12;  
end;

initialization
  Test();

finalization

end.