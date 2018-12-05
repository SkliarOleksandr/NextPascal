unit func_ccreg_TRect16;

interface

implementation

type
  TRect = record
    Left, Top, Right, Bottom: Int32;
  end;

function ExtFunc: TRect; external 'CAT' name 'func_ccreg_TRect16'; 

var R: TRect;

procedure Test;
begin
  R := ExtFunc();
end;

initialization
  Test();

finalization
  Assert(R.Left = 20);
  
end.