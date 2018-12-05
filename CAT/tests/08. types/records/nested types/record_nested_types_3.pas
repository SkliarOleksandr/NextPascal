unit record_nested_types_3;

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
   
procedure Test;
var
  D: TRec.TData;
begin
  D.a := 1;
  D.b := 2;
  R.Data := D;
end;

initialization
  Test();

finalization
  Assert(R.Data.a = 1);
  Assert(R.Data.b = 2);  
end.