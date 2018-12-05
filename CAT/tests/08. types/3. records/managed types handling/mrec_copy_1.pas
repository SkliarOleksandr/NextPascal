unit mrec_copy_1;

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
  S := copy('string');
  R1.S := S;  
  R2 := R1;
end;

initialization

  Test();

finalization

end.