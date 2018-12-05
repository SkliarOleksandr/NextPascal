#skip
unit class_import_1;

interface

type
  TObject = class
    constructor Create;
    procedure Free;
  end external 'SYSTEM' name 'TObject'; 

implementation

var
  o: TObject;

procedure Test;
begin
  o := TObject.Create();
  o.Free();   
end;  
  
initialization
  Test();
  
finalization

end.