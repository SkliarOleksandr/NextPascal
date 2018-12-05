unit class_smembers_1;

interface

implementation

uses System;

type
  TC = class
    A, B, C: Boolean;
    procedure Test;
  end;

procedure TC.Test;
begin
  A := B and C;
end;

initialization
  var Obj := TC.Create();
  Obj.B := True;
  Obj.C := True;  
  Obj.Test();  
  Assert(Obj.A);  

finalization

end.