unit HexToInt_1;

interface

implementation

function TryHexToInt(const Hex: string; out Value: Int32): Boolean;
  function Multiplier(Position: UInt32): Int32; inline;
  var
    I: Int32;
  begin
    Result := 1;
    for I := 1 to Position do
      Result := 16 * Result;
  end;
var
  A, B, I: Int32;
  M: Int32;
begin
  B := 0;
  Value := 0;  
  for I := High(Hex) downto Low(Hex) do begin
    A := Ord(Hex[I]);
    M := Multiplier(B);
    case A of
      48: ;   // '0' -- Do nothing.
      49..57: Value := Value + (A - 48) * M; // '1'..'9'
      65..70: Value := Value + (A - 55) * M; // 'A'..'F'
      97..102: Value := Value + (A - 87) * M; // 'a'..'f'
    else
      Exit(False);
    end;
    Inc(B);
  end;
  Result := True;
end; 

var 
  G0, G1, G2, G3, G4, G5, G6, G7: Int32;
   
procedure Test;
begin
  TryHexToInt('a', G0);
  TryHexToInt('a0', G1);
  TryHexToInt('a0b', G2);  
  TryHexToInt('a0b0', G3);  
  TryHexToInt('a0b0c', G4);
  TryHexToInt('a0b0c0', G5);
  TryHexToInt('a0b0c0d', G6);  
  TryHexToInt('a0b0c0d0', G7); 
end;

initialization
  Test();

finalization
  Assert(G0 = $a);
  Assert(G1 = $a0);
  Assert(G2 = $a0b);
  Assert(G3 = $a0b0);
  Assert(G4 = $a0b0c);      
  Assert(G5 = $a0b0c0);      
  Assert(G6 = $a0b0c0d);      
  Assert(G7 = $a0b0c0d0);            
end.