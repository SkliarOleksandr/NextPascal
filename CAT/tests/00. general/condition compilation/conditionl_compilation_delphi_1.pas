unit conditionl_compilation_delphi_1;

interface

implementation

var
  G: Int32;

{$DEFINE AAA}

procedure Test;
begin
  {$IFDEF AAA}
  G := 111;
  {$ELSE}
  G := 222;
  {$ENDIF}
end;

initialization
  Test();

finalization
  {$IFDEF AAA}
  Assert(G = 111);
  {$ELSE}
  Assert(G = 222);
  {$ENDIF}
end.