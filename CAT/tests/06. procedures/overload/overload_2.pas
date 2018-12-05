unit overload_2;

interface

function GetV(A, B: Int32): Int32; overload;
function GetV(A, B, C: Int32): Int32; overload;

implementation

function GetV(A, B: Int32): Int32; overload;
begin
  Result := A + B;
end;

function GetV(A, B, C: Int32): Int32; overload;
begin
  Result := A + B + C;
end;

var
  G1, G2: Int32;

procedure Test;
begin
  G1 := GetV(1, 2);
  G2 := GetV(1, 2, 3);  
end;

initialization
  Test();

finalization
  Assert(G1 = 3);
  Assert(G2 = 6);  
end.