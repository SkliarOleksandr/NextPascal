unit intf_2;

interface

implementation

uses System;

type
 
  II1 = interface
    function AAA(V: Int32): Int32;
    function BBB(V: Int32): Int32;    
  end;
   
  TT1 = class(TObject, II1)
    function AAA(V: Int32): Int32;
    function BBB(V: Int32): Int32;    
  end; 

var 
  G: Int32;

function TT1.AAA(V: Int32): Int32;
begin
  Result := V + 1;
end;

function TT1.BBB(V: Int32): Int32;
begin
  Result := V + 2;
end;
  
procedure Test;
var
  T: TT1;
  I: II1; 
begin         
  T := TT1.Create();       
  I := T;
  
  G := I.AAA(1);
  G := I.BBB(G);  
end;

initialization
  Test();

finalization
  Assert(G = 4);
end.