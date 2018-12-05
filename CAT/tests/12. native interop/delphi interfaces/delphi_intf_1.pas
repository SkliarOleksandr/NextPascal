unit delphi_intf_1;

interface

type
  IUnknown = interface
    procedure QueryInterface(const IID: TGUID; out Intf); stdcall; 
    function _AddRef: Int32; stdcall;
    function _Release: Int32; stdcall;     
  end external 'SYS';   

  IMyIntf = interface(IUnknown)    
    function GetInt: Int32;
    procedure SetInt(V: Int32); 
  end external 'SYS';

function GetIntf: IMyIntf; external 'SYS'; 

implementation

var
  G: Int32;
        
procedure Test;
var
  I: IMyIntf; 
begin
  I := GetIntf();
  I.SetInt(12);  
  G := I.GetInt(); 
end;  

initialization
  Test();

finalization
  Assert(G = 12);
  
end.