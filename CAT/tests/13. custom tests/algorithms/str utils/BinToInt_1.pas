unit BinToInt_1;

interface

implementation

function TryBinToInt(const BinString: string; out Value: Int64): Boolean;
var
  i, c: Int32;
  Digit: Char;
begin
  Value := 0;
  c := Length(BinString);
  if c = 0 then
    Exit(False);
    
  for i := Low(string) to c - 1 do
  begin
    Digit := BinString[i];
    case Digit of
      '0':;
      '1': Value := Value + 1;
    else
      Exit(False);
    end;
    if i < c - 1 then
      Value := Value shl 1;
  end;
end;

var 
  i: Int64;

procedure Test;
begin
  TryBinToInt('0', i);
  Assert(i = %0);
  
  TryBinToInt('01', i);
  Assert(i = %01);
  
  TryBinToInt('11', i);
  Assert(i = %11);
  
  TryBinToInt('1011', i);
  Assert(i = %1011);
  
  TryBinToInt('10111101010101001', i);
  Assert(i = %10111101010101001);
  
  TryBinToInt('1010101111001101111011110001001000110100010101100111100010010000', i);
  Assert(i = $ABCDEF1234567890);                  
end;

initialization
  Test();

finalization

end.