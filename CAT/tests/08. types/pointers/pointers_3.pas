unit pointers_3;

interface

implementation

type
  TRec = record
  type     
    TData = record
      a, b: Int32;
    end;
    PData = ^TData;
  var
    Data: TData;
  end;

var R: TRec;
    P: TRec.PData;
   
procedure Test;
begin
  R.Data.a := 1;
  R.Data.b := 2;
  P := @R.Data;
end;

initialization
  Test();

finalization
  Assert(P.a = 1);  
  Assert(P.b = 2);  
end.