unit delphi_intf_0;

interface

implementation

type
  IUnknown = interface
    procedure QueryInterface(const IID: TGUID; out Intf); stdcall; 
    function _AddRef: Int32; stdcall;
    function _Release: Int32; stdcall;     
  end external 'SYS';   

function GetIUnknown: IUnknown; external 'SYS';

var I: IUnknown;

procedure Test;
begin
  I := GetIUnknown();
  I._AddRef();
  I._Release();  
end;

initialization
  Test();

finalization

end.