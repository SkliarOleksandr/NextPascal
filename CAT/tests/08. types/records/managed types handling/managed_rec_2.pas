unit managed_rec_2;

interface

implementation

type 
  TRec = record
    S: string;
  end;

var
  GS: string;  

procedure Test;
var
  R: TRec;
begin
  R.S := GS;  
  Assert(R.S = 'new_str');  
end;

initialization
  GS := Copy('new_str');
  Test();

finalization

end.