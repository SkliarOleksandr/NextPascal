unit import_FORMAT_proc;

interface

implementation
  
uses VM_System, VM_SysUtils;  
  
var 
  s: string;
  
procedure Test;
begin
  s := format('formatted string: %d', [5]); 
end;

initialization
  Test();
  
finalization
  Assert(S = 'formatted string: 5');

end.