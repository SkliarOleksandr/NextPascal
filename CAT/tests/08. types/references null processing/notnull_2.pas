unit notnull_2;

interface

uses System;

implementation

var 
  C1, C2: TObject!;
  C3: TObject?;
    

procedure Test;
begin
  C1 := TObject.Create();

// C1 := nil; // compiling error!  
// C2 := nil; // compiling error!
// C1 := C3; // compiling error!
    
  C3 := nil;

  C2 := C1;
  C3 := C2; 
end;

initialization
  Test();

finalization
  Assert(C1 = C3);
end.