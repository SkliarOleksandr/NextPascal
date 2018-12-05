unit sizeof_3;

interface

implementation

var
  A1: record a, b, c, d: Int8; end;
  A2: record a, b, c, d: Int16; end;
  A3: record a, b, c, d: Int32; end;
  A4: record a, b, c, d: Int64; end;
  A5: record a, b, c, d: Boolean; end;
  A6: record a, b, c, d: Pointer; end;          

procedure Test;
begin
  Assert(sizeof(A1) = 4);
  Assert(sizeof(A2) = 8);
  Assert(sizeof(A3) = 16);
  Assert(sizeof(A4) = 32);
  Assert(sizeof(A5) = 4);
  Assert(sizeof(A6) = sizeof(Pointer)*4);       
end;

initialization
  Test();

finalization

end.