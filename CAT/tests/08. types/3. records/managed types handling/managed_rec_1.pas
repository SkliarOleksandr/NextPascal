unit managed_rec_1;

interface

implementation

type 
  TRec = record
    S: string;
  end;

var
  R: TRec;
  GS: string;  

procedure Test;
begin
  R.S := GS;  
end;

initialization
  GS := Copy('new_str');
  Test();

finalization
  Assert(R.S = 'new_str');
end.