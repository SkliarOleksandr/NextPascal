unit array_dynamic_5;

interface

implementation

uses system;

type
  TC = class
    FA: array of string;
    procedure Add(const S: string);
  end;

procedure TC.Add(const S: string);
var
  Len: Int32;
begin
  Len := Length(FA);
  SetLength(FA, Len + 1);
  FA[Len] := S;
end;

var C: TC;
    GC: Int32;
    SS: string;

procedure Test;
begin
  C := TC.Create();
  C.Add('xxx');
  C.Add('yyy');
  C.Add('zzz');  
  GC := Length(C.FA);
  SS := C.FA[0];  
end;

initialization
  Test();

finalization
  Assert(GC = 3);
  Assert(SS = 'xxx');   

end.