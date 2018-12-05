unit mrec_new_2;

interface

implementation

type
  TRec = record
    S: string;
    constructor CC;
    destructor DD;
  end;

var
  R: ^TRec;
  G: string;
    
constructor TRec.CC;
begin
  S := copy('new_str');
end;

destructor TRec.DD;
begin
  G := S;
end;    
    
procedure Test;
begin
  New(R);
  Free(R);
end;

initialization
  Test();

finalization
  Assert(G = 'new_str');

end.