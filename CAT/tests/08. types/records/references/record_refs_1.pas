unit record_refs_1;

interface

implementation

type
  TRec = record
    a, b: Int32;
  end;

var
  R: TRec;  
  P: ^TRec;
procedure Test;
begin
  P := @R;
  P.a := 1;
  P.b := 2;  
end;

initialization
  Test();

finalization
  Assert(P.a = 1);
  Assert(P.b = 2);  
  
end.