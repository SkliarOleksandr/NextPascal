unit caseof_2;

interface

implementation

function GetID(const Str: string): Int32;
begin
  case Str of
    'str1': Result := 1;
    'str2': Result := 2;
    'str3': Result := 3;
  else
    Result := -1;            
  end;
end;

var G0, G1, G2: Int32;

procedure Test;
begin
  G0 := GetID('blabla');
  G1 := GetID('str2');
  G2 := GetID('str3');    
end;

initialization
  Test();

finalization
  Assert(G0 = -1);
  Assert(G1 = 2);
  Assert(G2 = 3);    
end.