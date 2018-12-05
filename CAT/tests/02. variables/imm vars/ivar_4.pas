unit ivar_4;

interface

implementation

var G: Int32;

procedure Test;
begin
  var i := 1;
  begin 
    var i := 2;    
    begin
      var i := 3;
      G := G + i;  
    end;
    G := G + i;    
  end;
  G := G + i;  
end;

initialization
  G := 0;
  Test();

finalization
  Assert(G = 6);
end.