unit dtc_record_1;

interface

implementation

var
  r1: record
        a: Int32;
      end;
  r2: record
        b: Int32;
      end;   
      

procedure Test;
begin
  r1.a := 44;
  r2 := r1;
end;

initialization
  Test();

finalization
  Assert(r2.b = r1.a);
end.