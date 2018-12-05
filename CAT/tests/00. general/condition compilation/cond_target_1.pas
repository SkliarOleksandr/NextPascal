unit cond_target_1;

interface

#target any;

implementation

var
  s: string;

procedure Test;
begin
  #ifdef target_any 
    s := 'any';
  #end;  
end;

initialization
  Test();

finalization
  Assert(s = 'any');
end.