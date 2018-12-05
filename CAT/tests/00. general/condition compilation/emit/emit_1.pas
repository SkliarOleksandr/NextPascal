unit emit_1;

interface

implementation

var G: Int32;

const S = 'G' + ':=' + '111';

procedure Test;
begin
  #emit S;
  #emit 'G' + ':=' + 'G + 1';          
end;

initialization
  Test();

finalization
  Assert(G = 112);
end.