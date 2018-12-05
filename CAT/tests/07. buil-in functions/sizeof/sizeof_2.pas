unit sizeof_2;

interface

implementation

var 
  A1: array [5] of Int8;
  A2: array [5] of Int16;
  A3: array [5] of Int32;
  A4: array [5] of Int64;
  A5: array [5] of Boolean;
  A6: array [5] of Pointer;    

procedure Test;
begin
  Assert(sizeof(A1) = 5);
  Assert(sizeof(A2) = 10);
  Assert(sizeof(A3) = 20);
  Assert(sizeof(A4) = 40);
  Assert(sizeof(A5) = 5);
  Assert(sizeof(A6) = sizeof(Pointer)*5);          
end;

initialization
  Test();

finalization

end.