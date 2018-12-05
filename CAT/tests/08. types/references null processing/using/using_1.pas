unit using_1;

interface

implementation

var 
  P1: Pointer?;
  P2: Pointer!;  
 

procedure Test;
begin
  P1 := @P1;
  using P1 do begin
    P2 := P1;
  end;
end;

initialization
  Test();

finalization

end.