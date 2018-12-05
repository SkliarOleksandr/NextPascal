unit HexToInt_1;

interface

implementation

function TryHexToInt64(const Hex: string; out Value: Int64): Boolean;
  function Multiplier(Position: Int32): Int64; inline;
  var
    I: Int32;
  begin
    Result := 1;
    for I := 1 to Position do
      Result := 16 * Result;
  end;
var
  A, B, I: Int32;
  M: Int64;
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
  G0, G1, G2, G3, G4, G5, G6, G7, G8, 
  G9, G10, G11, G12, G13, G14, G15: Int64;
   
procedure Test;
begin
  TryHexToInt64('A', G0);
  TryHexToInt64('AB', G1);
  TryHexToInt64('ABC', G2);  
  TryHexToInt64('ABCD', G3);
  TryHexToInt64('ABCDE', G4);
  TryHexToInt64('ABCDEF', G5);
  TryHexToInt64('ABCDEF1', G6);
  TryHexToInt64('ABCDEF12', G7); 
  TryHexToInt64('ABCDEF123', G8);        
  TryHexToInt64('ABCDEF1234', G9);
  TryHexToInt64('ABCDEF12345', G10);     
  TryHexToInt64('ABCDEF123456', G11);
  TryHexToInt64('ABCDEF1234567', G12);
  TryHexToInt64('ABCDEF12345678', G13);
  TryHexToInt64('ABCDEF123456789', G14);
  TryHexToInt64('ABCDEF1234567890', G15);
end;

initialization
  Test();

finalization
  Assert(G0 = $A);
  Assert(G1 = $AB);
  Assert(G2 = $ABC);
  Assert(G3 = $ABCD);
  Assert(G4 = $ABCDE);      
  Assert(G5 = $ABCDEF);      
  Assert(G6 = $ABCDEF1);      
  Assert(G7 = $ABCDEF12);      
  Assert(G8 = $ABCDEF123);
  Assert(G9 = $ABCDEF1234);   
  Assert(G10 = $ABCDEF12345);
  Assert(G11 = $ABCDEF123456);                               
  Assert(G12 = $ABCDEF1234567);
  Assert(G13 = $ABCDEF12345678);
  Assert(G14 = $ABCDEF123456789);
  Assert(G15 = $ABCDEF1234567890);  
  {}      
end.