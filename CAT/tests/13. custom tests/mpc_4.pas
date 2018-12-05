unit MAIN;
interface
uses system;
type
  TMPCObjectResult = (mpcResultOK, mpcResultWait, mpcResultError);
  //=====================================
  type1 = class(TObject)
    f1: Int32;
    f2: Int32;
    function Execute: TMPCObjectResult; export;
  end;
  
implementation

var G: Int32;

function type1.Execute: TMPCObjectResult;
begin
  f1 := f1 + 1;
  f2 := f2 + 10;
  G := f1 + f2;  
end;

end.