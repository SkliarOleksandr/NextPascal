unit record_forward_decls;

interface

implementation

type

  PRec = ^TRec;

  TRec = record
    a: Int32;  
    p: PRec;
  end;
  
var
  R: TRec;  
  P: PRec;

procedure Test;
begin
  R.p := @R; 
  R.p.a := 5;
  P := R.p;
end;

initialization
  Test();

finalization
  Assert(R.a = 5);
end.