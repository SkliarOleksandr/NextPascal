unit using_0;

interface

implementation

var 
  P1: Pointer!;
  P2: Pointer!;  
 
procedure Test;
begin
  P1 := @P1;
  // P1 is not null - no any check needed
  using P1 do begin
    P2 := P1;
  end;
end;

initialization
  Test();

finalization
  Assert(P1 = P2);
end.