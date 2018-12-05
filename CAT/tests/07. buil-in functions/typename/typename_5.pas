unit typename_5;

interface

implementation

type
  TRec<T> = record
    a: T;
    function GetName1: string; 
    function GetName2: string;    
  end;

function TRec<T>.GetName1: string;
begin
  Result := typename(T);
end;
 
function TRec<T>.GetName2: string;
begin
  Result := typename(self);
end;    

var 
  R: TRec<Int32>;
  S1: string;
  S2: string;
    
procedure Test;
begin
  S1 := R.GetName1();
  S2 := R.GetName2();  
end;

initialization
  Test();

finalization
  Assert(S1 = 'Int32');
  Assert(S2 = 'TRec<Int32>');  
end.