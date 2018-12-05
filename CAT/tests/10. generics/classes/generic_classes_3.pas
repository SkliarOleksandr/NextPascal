unit generic_classes_3;

interface

implementation

uses System;

type
  TC<T> = class  
    A: T;   
  end; 

  TInt32 = TC<Int32>;

var
  C: TInt32;
  G: Int32;

procedure Test;
begin
  C := TInt32.Create(); 
  C.A := 55;
  G := C.A;
end;

initialization
  Test();

finalization
  Assert(G = 55);

end.