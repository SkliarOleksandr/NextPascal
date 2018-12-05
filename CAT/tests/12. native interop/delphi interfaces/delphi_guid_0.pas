unit delphi_guid;

interface

implementation

procedure WriteGUID(const GUID: TGUID); external 'system';
procedure ReadGUID(out GUID: TGUID); external 'system';

var G1, G2: TGUID; 

procedure Test;
begin
  G1 := '{00000000-0000-0000-C000-000000000046}';     
  WriteGUID(G1);
  ReadGUID(G2);  
end;

initialization
  Test();

finalization
  Assert(G1 = G2);

end.