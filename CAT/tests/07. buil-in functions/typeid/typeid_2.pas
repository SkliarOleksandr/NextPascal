unit typeid_2;

interface

implementation

function GetType<T>(const Value: T): string; 
begin
  case typeid(Value) of        
    dtInt8: Result := 'i8';  
    dtInt16: Result := 'i16';      
    dtInt32: Result := 'i32'; 
    dtInt64: Result := 'i64';
    dtUInt8: Result := 'u8';  
    dtUInt16: Result := 'u16';      
    dtUInt32: Result := 'u32'; 
    dtUInt64: Result := 'u64';
    dtFloat32: Result := 'f32';  
    dtFloat64: Result := 'f64';     
    dtstring: Result := 'str';
    dtAnsiString: Result := 'astr';                   
  else
    Result := 'another';  
  end;
end;

var
  S: string;

procedure Test;
begin
  S := GetType(1);
  Assert(S = 'u8');

  S := GetType(-1);
  Assert(S = 'i8');

  S := GetType(14.56);
  Assert(S = 'f32');

  S := GetType('aaa');
  Assert(S = 'str');
end;

initialization
  Test();

finalization

end.