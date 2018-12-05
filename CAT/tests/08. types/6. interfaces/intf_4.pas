unit intf_4;

interface

implementation

uses System;

type
  II1 = interface
    procedure AAA;
    procedure BBB;
    procedure CCC;      
  end;
  
  TT1 = class(TObject, II1)
    Data:  Integer;
    procedure AAA;
    procedure BBB;
    procedure CCC;     
  end;



procedure TT1.AAA;
begin
  Inc(Data);
end;

procedure TT1.BBB;
begin
  Inc(Data, 10);
end;

procedure TT1.CCC;
begin
  Inc(Data, 100);
end;

var T: TT1;
    I: II1;
    G: Int32;    

procedure Test;
begin
  T := TT1.Create();
  I := T;
  I.AAA();
  I.BBB();
  I.CCC();    
  G := T.Data;
end;

initialization
  Test();

finalization
  Assert(G = 111);
end.