unit record_refs_4;

interface

implementation

type
  TRec = record
    A: Int32;
    B: Int32;
  public
    procedure Execute;  
  end;

var
  R: TRec;

procedure TRec.Execute;
begin
  A := 1;
  B := 2;
end;


procedure Test(const OBJ: TRec);
begin
  OBJ.Execute();
end;

initialization
  Test(R);

finalization
  Assert(R.A = 1);
  Assert(R.B = 2);  
end.      