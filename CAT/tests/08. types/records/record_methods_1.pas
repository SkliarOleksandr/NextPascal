unit record_methods_1;
interface
type

  TRec = record
    function Get5: Int32;
    function Get6: Int32;
  end;

var
  G1, G2: Int32;

implementation

function TRec.Get5: Int32;
begin
  Result := 5;
end;

function TRec.Get6: Int32;
begin
  Result := 6;
end;

procedure Test;
var
  R: TRec;
begin
  #bpt;       
  G1 := R.Get5();
  G2 := R.Get6();
end;  

initialization
  Test();
  
finalization
  Assert(G1 = 5);  
  Assert(G2 = 6);  
  
end. 