#skip
unit class_import_2;

interface
type
  TForm = class
    constructor Create(AOwner: Int32 = 0);
    procedure Show;
    procedure Close;
    procedure Free;
    function ShowModal: Integer; virtual;
  end external 'forms' name 'TForm';

implementation

var
  F: TForm;
  G: Int32;

procedure Test;
begin
  F := TForm.Create();
  F.Show();
  F.Close(); 
  F.Free();
end;
  
initialization
  Test();
  
finalization

end.