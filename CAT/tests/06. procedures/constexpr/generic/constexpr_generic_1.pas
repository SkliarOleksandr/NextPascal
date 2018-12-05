unit constexpr_generic_1;

interface

implementation

function Sum<T>(a, b: T): T; pure;
begin
  Result := a + b;
end;

var
  VI: Int32;
  VF: Float32;
  VS: string;

procedure Test;
begin
  VI := Sum<Int32>(1, 4);
  VF := Sum<Float32>(1.1, 4.2);
  VS := Sum<string>('AA', 'BBB');           
end;

initialization
  Test();

finalization
  Assert(VI = 5);
  Assert(VF = 5.3);  
  Assert(VS = 'AABBB');
end.