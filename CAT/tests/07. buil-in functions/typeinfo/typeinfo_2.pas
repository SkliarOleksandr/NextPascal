unit typeinfo_2;

interface

implementation

uses sys.rtti;

var
  P: TObject;
  R: TRTTIOrdinal;
  S: string;

procedure Test;
begin
  P := typeinfo(int32);
  R := P as TRTTIOrdinal;
  S := R.Name;  
end;

initialization
  Test();

finalization
  Assert(S = 'Int32');

end.