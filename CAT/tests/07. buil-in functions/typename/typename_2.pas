unit typename_2;

interface

implementation

var
  A: array [5] of Int32;
  B: record
       x, y: Int16;
     end;   
  C: (a1, a2, a3);
  D: array of string;   

var
  S: string;

procedure Test;
begin
  S := typename(A);
  S := typename(B);
  S := typename(C);
  S := typename(D);         
end;

initialization
  Test();

finalization

end.