unit tuple_test_2;

interface

type
  
  TRec = record
    a: int8;
    b: int16;
    c: int32;
    d: int64;
    x: Boolean;  
    y: Char;   
    z: float64;
    str: string;   
  end;    

var R: TRec;

implementation

procedure Test;
begin
  R := [1, 2, 3, 4, True, 'A', 3.14, 'asdf'];
end;

initialization
  Test();

finalization

end.