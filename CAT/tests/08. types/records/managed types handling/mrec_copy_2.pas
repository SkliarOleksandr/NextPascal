unit mrec_copy_2;

interface

implementation

type
  TRec = record
    s: string;
  end;

var
  R1, R2: TRec;
  S: string;

procedure Test;
begin
  R1.S := S;  
  R2 := R1;
  R2 := R1;
  R1 := R2;
end;

initialization
  S := copy('string');
  Test();

finalization

end.