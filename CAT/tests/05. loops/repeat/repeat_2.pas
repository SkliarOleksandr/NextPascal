unit repeat_2;

interface

implementation

var
  X, Y: Int32;

procedure Test;

begin
  X := 0;
  repeat
    X := X + 1;
    Y := 0; 
    repeat
      Y := Y + 1;
    until Y = 20;  
  until X = 10; 
end;

initialization
  Test();

finalization
  Assert(X = 10);
  Assert(Y = 20);  

end.