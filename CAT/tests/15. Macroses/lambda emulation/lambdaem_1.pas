#skip
unit lambdaem_1;

interface

implementation

#macro ANP(BODY);
begin
  #emit 'procedure begin ' + BODY + ' end';
end;

var G: Int32;

procedure Test;
var
  p: procedure;
begin
  p := ANP(G := 5);
end;

initialization
  Test();

finalization

end.