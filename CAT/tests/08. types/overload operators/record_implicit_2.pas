unit record_implicit_2;

interface

implementation

type
  TRec = record
    FData: Int32;
    operator Implicit(const Src: Int32): TRec; overload;
    operator Implicit(const Src: TRec): Int32; overload;   
  end;
  
operator TRec.Implicit(const Src: Int32): TRec;
begin
  Result.FData := Src; 
end;

operator TRec.Implicit(const Src: TRec): Int32;
begin
  Result := Src.FData; 
end;

var
  G: Int32;
  R: TRec;
  

procedure Test;
begin
//  R := 5;
//  G := R;
end;

initialization
  Test();

finalization
//  Assert(G = 5);
end.