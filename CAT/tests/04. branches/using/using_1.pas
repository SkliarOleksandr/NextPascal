unit using_1;

interface

implementation

uses System;

{procedure TT(ref A: TObject);
begin
end;}
ref  
  Dst: TObject; 

procedure Test;
var
  Src: TObject;
begin
  Src := nil;
  using Src, Dst do begin
    Dst := Src;
  end;  
  //Dst := Src;
end;

initialization
  Test();

finalization

end.