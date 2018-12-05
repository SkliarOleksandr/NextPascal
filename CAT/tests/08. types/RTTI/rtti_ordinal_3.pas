unit rtti_ordinal_3;

interface

implementation

uses
  System, sys.rtti;

var
  ti: TRTTIOrdinal;

procedure Test_bool;
begin
  ti := TypeInfo(Boolean) as TRTTIOrdinal;
  Assert(ti.Name = 'Boolean');
  Assert(ti.TypeSize = 1);  
  Assert(ti.Signed = False);
  Assert(ti.LoBound = 0);
  Assert(ti.HiBound = 1);
end;

procedure Test_AnsiChar;
begin
  ti := TypeInfo(AnsiChar) as TRTTIOrdinal;
  Assert(ti.Name = 'AnsiChar');
  Assert(ti.TypeSize = 1);  
  Assert(ti.Signed = False);
  Assert(ti.LoBound = 0);
  Assert(ti.HiBound = 255);
end;

procedure Test_Char;
begin
  ti := TypeInfo(Char) as TRTTIOrdinal;
  Assert(ti.Name = 'Char');
  Assert(ti.TypeSize = 2);  
  Assert(ti.Signed = False);
  Assert(ti.LoBound = 0);
  Assert(ti.HiBound = 65535);
end;

initialization
  Test_bool();
  Test_AnsiChar();
  Test_Char();

finalization

end.