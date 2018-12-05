unit VM_INTF;

interface

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

uses SysUtils, VM.Invoke, xmldom, XMLDoc, XMlIntf;

implementation

type
  IMyIntf = interface
    ['{965AC8F3-6952-41C7-BFB9-41D48BF3D860}']
    function GetInt: Integer;
    procedure SetInt(V: Integer);
  end;

  TMyIntf = class(TInterfacedObject, IMyIntf)
    FInt: Integer;
    function GetInt: Integer;
    procedure SetInt(V: Integer);
  public
    destructor Destroy; override;
  end;

function GetIntf: IMyIntf;
begin
  Result := TMyIntf.Create;
end;

function GetIUnknown: IInterface;
begin
  Result := TInterfacedObject.Create;
end;

procedure XCreateGUID(out GUID: TGUID);
begin
  CreateGUID(GUID);
end;


var
  _G: TGUID;

procedure WriteGUID(const GUID: TGUID);
begin
  _G := GUID;
end;

procedure ReadGUID(out GUID: TGUID);
begin
  GUID := _G;
end;


procedure RegisterVM_System;
begin
  RegisterProc('SYS', 'GetIUnknown', @GetIUnknown);
  RegisterProc('SYS', 'GetIntf', @GetIntf);
  RegisterProc('SYS', 'NewXMLDocument', @NewXMLDocument);
  RegisterProc('system', 'CreateGUID', @XCreateGUID);
  RegisterProc('system', 'WriteGUID', @WriteGUID);
  RegisterProc('system', 'ReadGUID', @ReadGUID);
end;

{ TMyIntf }

destructor TMyIntf.Destroy;
begin

  inherited;
end;

function TMyIntf.GetInt: Integer;
begin
  Result := FInt;
end;

procedure TMyIntf.SetInt(V: Integer);
begin
  FInt := V;
end;

var
  Doc: IXMLDocument;
  Node: IXMLNode;


initialization
  Doc := NewXMLDocument;
  Node := Doc.AddChild('root');
  RegisterVM_System;

end.
