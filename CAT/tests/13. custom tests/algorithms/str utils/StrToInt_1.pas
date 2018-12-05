unit StrToInt_1;

interface

implementation

uses System;

function StrToInt(const Str: string): Int32;
var
  i, SPos, Sign: Int32; 
  Ch: Char;
begin
  Result := 0;
  if Str[0] = '-' then
  begin
    sign := -1;
    SPos := 1;
  end else begin
    Sign := 1;  
    SPos := 0;    
  end;  
            
  for i := SPos to Length(Str) - 1 do
  begin
    Ch := Str[i];
    if (Ch > '9') or (Ch < '0') then
      Assert(False, 'Invalid chars');
 
    Result := Result * 10;
    Result := Result + ord(Ch) - ord('0');
  end;       
  Result := Result * sign;
end;

var
  I: Int32;

procedure Test;
begin
  I := StrToInt('-1');
  Assert(I = -1);
  
  I := StrToInt('00000');
  Assert(I = 0);
  
  I := StrToInt('45');
  Assert(I = 45);

  I := StrToInt('4512314123');
  Assert(I = 4512314123);          
  
  I := StrToInt('-2147483648');
  Assert(I = MinInt32);
  
  I := StrToInt('2147483647');
  Assert(I = MaxInt32);                
end;

initialization
  Test();

finalization

end.