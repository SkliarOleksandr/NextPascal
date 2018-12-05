unit inline_method_2;

interface

implementation

type
  TRec = record
    FD1: Int32;
    FD2: Int32;
    procedure SetData(D1, D2: Int32); inline;    
  end;

procedure TRec.SetData(D1, D2: Int32); 
begin
  FD1 := D1;
  FD2 := D2;
end;

var R: TRec;

procedure Test;
begin
  R.SetData(12, 13);
end;

initialization
  Test();

finalization
  Assert(R.FD1 = 12);
  Assert(R.FD2 = 13);  
end.