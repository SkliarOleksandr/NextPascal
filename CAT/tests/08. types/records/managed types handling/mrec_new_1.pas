unit mrec_new_1;

interface

implementation

type
  TRec = record
    S: string;
  end;

var
  R: ^TRec;
  G: string;
    
procedure Test;
begin
  New(R);
  R.S := copy('new_str');
  G := R.S;
  Free(R);
end;

initialization
  Test();

finalization
  Assert(G = 'new_str');
end.