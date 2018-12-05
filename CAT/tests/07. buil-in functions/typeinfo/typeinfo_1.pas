unit typeinfo_1;

interface

implementation

implementation

uses sys.rtti;

var
  P: TObject;
  R: TRTTI;
  S: string;

procedure Test;
begin
  P := TypeInfo(S);
  R := P as TRTTI;
  S := R.Name;
end;

initialization
  Test();

finalization
 Assert(S = 'String');
end.