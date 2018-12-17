unit record_explicit_overload_1;

interface

implementation

type
  TRec = record
    VL: string; 
    VH: Int32;    
    operator Explicit(const a: TRec): Int32; 
    operator Explicit(const a: Int32): TRec;      
  end;


var
  R: TRec;
  V1, V2: Int32;  

operator TRec.Explicit(const a: TRec): Int32;
begin
  Result := A.VH;
end;

operator TRec.Explicit(const a: Int32): TRec; 
begin
  Result.VH := A;
end;

procedure Test;
begin
  V1 := 42;
  R := TRec(V1);
  V2 := Int32(R); 
end;

initialization
  Test();

finalization
  Assert(V1 = 42);
  Assert(V2 = 42);
  Assert(R.VH = 42);    
end.