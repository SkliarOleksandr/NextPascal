unit const_array_of_record_1;

interface

implementation

type
  TRec = record
    a: Int32;
    b: Int32; 
  end;

const Fields: array [3] of TRec = ([11, 1], [22, 2], [33, 3]);

var
  f0a, f0b: Int32;
  f1a, f1b: Int32;  
  f2a, f2b: Int32;  

procedure Test;
begin
  f0a := Fields[0].a;
  f0b := Fields[0].b;
  f1a := Fields[1].a;
  f1b := Fields[1].b;
  f2a := Fields[2].a;
  f2b := Fields[2].b;      
end;

initialization
  Test();

finalization
  Assert(f0a = 11);
  Assert(f0b = 1);
  Assert(f1a = 22);
  Assert(f1b = 2);
  Assert(f2a = 33);
  Assert(f2b = 3);      
end.