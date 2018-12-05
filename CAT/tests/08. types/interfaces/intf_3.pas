unit intf_3;

interface

implementation

uses System;

type
 
  II1 = interface
    procedure AAA;
  end;
  
  II2 = interface
    procedure BBB;  
  end;  
 
  TT1 = class(TObject, II1, II2)
    procedure AAA;
    procedure BBB;  
  end; 

var 
  G1, G2: Int32;
  T: TT1;
  I1: II1;
  I2: II2; 

procedure TT1.AAA;
begin
  G1 := 11;
end;

procedure TT1.BBB;
begin
  G2 := 22;
end;
  
procedure Test;
begin                
  T := TT1.Create(); 
  I1 := T;
  I2 := T;  
  I1.AAA();
  I2.BBB();    
end;

initialization
  Test();

finalization
  Assert(G1 = 11);
  Assert(G2 = 22);  
end.