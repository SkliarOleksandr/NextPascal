unit typename_3;

interface

implementation

type
  TRec = record
    function GetName: string;
  end;

function TRec.GetName: string;
begin
  Result := typename(self);
end;

var S: string;

procedure Test;
var
  R: TRec;
begin
  S := R.GetName();
end;

initialization
  Test();

finalization
  Assert(S = 'TRec');
end.