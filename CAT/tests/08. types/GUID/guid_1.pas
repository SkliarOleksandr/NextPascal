unit guid_1;

interface

implementation

var
  G1, G2: TGUID;

procedure CreateGUID(out GUID: TGUID); external 'system';

procedure Test;
begin
  CreateGUID(G1);
  G2 := G1;
end;

initialization
  Test();

finalization
  Assert(G1 = G2);

end.