unit ifthen_case_1;

interface

var
  A, R: Int32;
    
implementation

procedure Test;
begin
  A := 5;
  if A = 1 then
  begin
    A := A + 1;
    case A of
      0: R := 11;
      1: R := 12;
      2: R := 13;             
    end;       
  end else 
  begin
    case A of
      0: R := 21;
      1: R := 22;
    else  
      R := 333;             
    end;        
  end;       
end;

initialization
  Test();

finalization
  Assert(A = 5);
  Assert(R = 333);
  
end.