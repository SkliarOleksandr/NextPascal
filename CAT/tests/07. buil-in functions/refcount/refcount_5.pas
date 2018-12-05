unit refcount_5;

interface

implementation

uses System;

var
  Obj1, Obj2, Obj3: TObject;
  C: Int32;

procedure Test;
begin
  Obj1 := TObject.Create();
  Obj2, Obj3 := Obj1;
  C := refcount(Obj1); 
end;

initialization
  Test();

finalization
  Assert(C = 3);

end.