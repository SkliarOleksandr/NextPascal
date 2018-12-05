unit deref_5;

interface

implementation

var 
  AC1, AC2: AnsiChar;
  UC1, UC2: Char;  
  
  pa: ^AnsiChar;
  pu: ^Char;  
    
procedure Test;
begin
  AC1 := 'A';
  pa := @AC1;
  AC2 := pa^;
  
  UC1 := 'É';
  pu := @UC1;
  UC2 := pu^;  
end;

initialization
  Test();

finalization
  Assert(AC1 = AC2);
  Assert(UC1 = UC2);  

end.