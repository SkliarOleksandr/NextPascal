unit VM_Forms;

interface

implementation

uses VM.Invoke, Forms;

procedure DoRegister;
begin
  with RegisterType('Forms', 'TForm', TForm) do
  begin
    RegisterVirtMethod('ShowModal', @TForm.ShowModal);
    RegisterMethod('Show', @TForm.Show);
    RegisterMethod('Create', @TForm.Create);
    RegisterMethod('Close', @TForm.Close);
    RegisterMethod('Free', @TForm.Free);
  end;
end;

initialization

  DoRegister;

end.
