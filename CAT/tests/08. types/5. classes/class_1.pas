unit class_1;

interface

implementation

uses System;

type 
  TC1 = class(TObject)
    FData: Int32;
    procedure SetData1;  
  end;
  
  TC2 = class(TC1)
    procedure SetData2;  
  end;  

procedure TC1.SetData1;
begin
  FData := 1;
end;

procedure TC2.SetData2;
begin
  FData := 2;
end;

procedure Test;
var 
  O: TC2;
begin
  O := TC2.Create();
  O.SetData1();
  Assert(O.FData = 1);
  O.SetData2();  
  Assert(O.FData = 2);
end;   
        
initialization
  Test();

finalization

end.