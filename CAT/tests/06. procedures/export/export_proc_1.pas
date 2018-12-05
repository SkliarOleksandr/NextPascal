unit export_proc_1;

interface

procedure Test; export;

implementation

var G: Int32;

procedure Test;
begin
  G := 134;
end;

initialization
  Test();

finalization

end.