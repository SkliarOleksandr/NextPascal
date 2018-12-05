unit IntToStr_1;

interface

implementation

function _IntToStr(Value: UInt32): string;
var
  P: ^Char;
  I, R: UInt32;
  B: string;
begin
  I := 0;
  SetLength(B, 10);
  P := @B[9];
  repeat
    R := Value mod 10; 
    P^ := Char(R + ord('0'));
    Dec(P);
    Value := Value div 10;
    Inc(I);
  until Value = 0;
  Result := Copy(B, 10 + Low(string) - i, i);
end;

var S: string;

procedure Test;
begin
  S := _IntToStr(1);
  Assert(S = '1');
  
  S := _IntToStr(123);
  Assert(S = '123');
  
  S := _IntToStr(123235245);
  Assert(S = '123235245');     
end;

initialization
  Test();

finalization

end.