#skip
unit _ifassigned;

interface
uses System;

type 
  TC = class
  end;

implementation

procedure Test;
var
   cn: TC; // nullable reference
ref
   c: TC; // not null reference
weak
   cw: TC; // nullable weak reference      
begin
  cn := nil;         // OK
  cn := TC.Create(); // OK

  c := TC.Create();  // OK
  c := nil;          // ERROR
  c := cn;           // ERROR

  using cn do 
    cn.CallMethod(); // OK
  
  cw := TC.Create(); // ERROR???  
  cw := cn;          // OK
  cw := c;           // OK     
  cw := nil;         // OK 

  using cn do 
    cw.CallMethod(); // OK

  using cn, cw do begin
    cn.CallMethod(); // OK
    cw.CallMethod(); // OK
  end else begin
    
  end;  
  
  using cn, cw do begin
    cn.CallMethod(); // OK
    cw.CallMethod(); // OK
  end;
     

  using c as TMyC, cn as TMyC do begin
    c.CallMethod(); // OK
    c.CallMethod(); // OK
  end;   

    
end;

initialization
  Test();

finalization

end.