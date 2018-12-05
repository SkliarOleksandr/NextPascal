unit delphi_intf_3;

interface

implementation

type
  IUnknown = interface
    ['{00000000-0000-0000-C000-000000000046}']
    function QueryInterface(const IID: TGUID; out Intf: IUnknown): Int32; stdcall; 
    function _AddRef: Int32; stdcall;
    function _Release: Int32; stdcall;     
  end external 'SYS';   

function GetIUnknown: IUnknown; external 'SYS';

var I, J: IUnknown;
    R: Int32;

procedure Test;
begin
  I := GetIUnknown();
  R := I.QueryInterface(IUnknown, J);
end;

initialization
  Test();

finalization
  Assert(I = J);
end.