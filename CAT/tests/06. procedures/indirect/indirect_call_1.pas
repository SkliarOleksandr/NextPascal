unit indirect_call_1;

interface

implementation

uses system;

type 
  TC = class
    FProc: function(V1, V2: Int32): Int32;
    procedure Run;
  end;

var C: TC;
    G: Int32;

function CMP(V1, V2: Int32): Int32;
begin
  Result := V1 - V2;
end;

procedure TC.Run;
begin
  G := FProc(333, 111);
end;

procedure Test;
begin
  C := TC.Create();
  C.FProc := CMP;
  C.Run();  
end;

initialization
  Test();

finalization
  Assert(G = 222);

end.