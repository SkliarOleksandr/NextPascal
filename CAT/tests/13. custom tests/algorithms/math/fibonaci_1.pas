unit fibonaci_1;

interface

implementation

function fibo(n: Int32): Int32;
begin
  if (n = 1) or (n = 2) then
    Result := 1
  else 
    Result := fibo(n - 1) + fibo(n - 2);
end;

var F1, F2, F3: Int32;

procedure Test;
begin
  F1 := Fibo(3);
  F2 := Fibo(7);
  F3 := Fibo(11);    
end;

initialization
  Test();

finalization
  Assert(F1 = 2);
  Assert(F2 = 13);
  Assert(F3 = 89);    
end.