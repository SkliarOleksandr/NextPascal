unit record_refs_2;

interface

implementation

type
  TRec1 = record
    a, b: Int32;  
  end;
 
  TRec2 = record
    Ptr: ^TRec1;
  end;

var
  R1: TRec1;
  R2: TRec2; 
   
  P1: ^TRec1;
  P2: ^TRec2;  
  
procedure Test;
begin
  P1 := @R1;
  P2 := @R2;

  P2.Ptr := P1;   
  
  P2.Ptr.a := 11;
  P2.Ptr.b := 12;  
end;

initialization
  Test();

finalization

end.