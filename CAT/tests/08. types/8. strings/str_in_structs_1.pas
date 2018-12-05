unit str_in_structs_1;

interface

implementation

type
  TRec = record
    s: string;
  end;

var R: TRec;
    C: Char;

procedure Test;
begin
  C := R.S[1];
end;

initialization
  R.s := 'ABCD';
  Test();  

finalization
  Assert(C = 'B');
end.