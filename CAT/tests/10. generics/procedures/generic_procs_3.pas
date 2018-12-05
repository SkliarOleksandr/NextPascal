unit generic_procs_3;

interface

implementation

procedure GetVal<T>(out Value: T);
begin
  Value := '5';
end;

var G: char;

procedure Test;
begin
  GetVal<char>(var X);   
  G := X;
end;

initialization
  Test();

finalization
  Assert(G = '5');
end.