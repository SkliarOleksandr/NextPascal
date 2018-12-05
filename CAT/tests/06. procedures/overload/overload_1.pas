unit overload_1;

interface

var
  GI: Int32; 
  GS: String; 
  GF: Float32;

implementation

procedure P(V: Int32); overload;
begin
  GI := V;  
end;

procedure P(V: Float32); overload;
begin
  GF := V;
end;

procedure P(V: String); overload;
begin
  GS := V;
end;

procedure Test;
begin
  P('asdf');
  P(5);
  P(1.2);    
end;

initialization
  Test();

finalization
  Assert(GI = 5);
  Assert(GS = 'asdf');
  Assert(GF = 1.2);    

end.