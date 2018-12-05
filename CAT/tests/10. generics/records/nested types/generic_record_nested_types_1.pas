unit generic_record_nested_types_1;

interface

type
  TRec<T> = record
  type
    TInt = T;
  var  
    F1, F2: TInt;
    procedure S0(V: T);  
    procedure S1(V: TInt);
    procedure S2(V: TInt);            
  end;

implementation

var
  G1, G2: Int32;

procedure TRec<T>.S0(V: T);
begin
end;

procedure TRec<T>.S1(V: TInt);
begin
  F1 := V;
  G1 := F1;
end;

procedure TRec<T>.S2(V: TInt);
begin
  F2 := V;
  G2 := F2;
end;   
   
var
  R: TRec<Int32>;

procedure Test;
begin
  R.S1(5);
  R.S2(7);  
end; 

initialization
  Test();

finalization
  Assert(G1 = 5);
  Assert(G2 = 7);  
end.