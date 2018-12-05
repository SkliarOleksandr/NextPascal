unit tuple_test_3;
interface

type
  
  TRec = record
    a: string;
    b: string;        
  end;    
  
implementation  
  
var
  R, R2: TRec;
    
  G1, G2: string;
  
procedure Test;
begin
  R := ['asdf', 'str2'];
  G1 := R.a;
  G2 := R.b;  
end;

function GetR: TRec;
begin
  Result := ['asdf', 'str2'];
end;

initialization
  Test();
  R2 := GetR(); 
  
finalization
  Assert(G1 = 'asdf');
  Assert(G2 = 'str2');  
end.
