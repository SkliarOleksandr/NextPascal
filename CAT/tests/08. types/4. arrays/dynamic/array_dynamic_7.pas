unit array_dynamic_7;

interface

implementation
uses system;

type
  TRec = record
    s: string;
  end;

  TC = class
    FA: array of TRec;
    procedure Add(const S: string);
    function GetCNT: Int32;
  end;

procedure TC.Add(const S: string);
var
  Len: Int32;
begin
  Len := Length(FA);
  SetLength(FA, Len + 1);
  FA[Len].S := S;
end;

function TC.GetCNT: Int32;
begin
  Result := Length(FA);
end;

var C: TC;
    GC: Int32;
    S1, S2: string;

procedure Test;
begin
  C := TC.Create();
  C.Add('aaa');
  C.Add('bbb');
  S1 := C.FA[0].S;
  S2 := C.FA[1].S;   
  GC := C.GetCNT();  
end;

initialization
  Test();

finalization
  Assert(GC = 2);
  Assert(S1 = 'aaa');
  Assert(S2 = 'bbb');    

end.