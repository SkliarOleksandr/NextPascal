unit ivar_3;

interface

implementation

var S: string;

procedure Test;
begin
  var s1 := 'asdf';
  var s2 := '1234';
  S := s1 + s2;  
end;

initialization
  Test();

finalization
  Assert(S = 'asdf1234');

end.