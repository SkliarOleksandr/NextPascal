unit to_string_1;

interface

implementation

function to_string<T>(const Value: T): string;
begin
  case typeid(Value) of
    dtInt8:;
    dtInt16:;
    dtInt32:;
    dtInt64:;
    dtUInt8:;
    dtUInt16:;
    dtUInt32:;
    dtUInt64:;
    dtNativeInt:;
    dtNativeUInt:;
    dtFloat32:;
    dtFloat64:;
    dtBoolean:;
    dtAnsiChar:;
    dtChar:;
    dtAnsiString:;
    dtString:;
    dtVariant:;
    dtGuid:;
    dtPointer:;
    dtWeakRef:;
    dtGeneric:;
    dtRange:;
    dtEnum:;
    dtSet:;
    dtStaticArray:;
    dtDynArray:;
    dtOpenArray:;
    dtProcType:;
    dtRecord:;
    dtClass:;    
    dtClassOf:;  
    dtInterface:; 
  end; 
end;

var S: string;

procedure Test;
begin
  S := to_string(5);
end;

initialization
  Test();

finalization

end.