unit intf_props_1;

interface

implementation

uses System;

type
  INested = interface
    function GetCNT: Int32;
    property CNT: Int32 read GetCNT;
  end;
  

  TNested = class(TObject, INested)
    FCNT: Int32;
    function GetCNT: Int32;   
    constructor Create;     
  end;
  
  IRoot = interface
    function GetNested: INested;
    property Nested: INested read GetNested;  
  end;
  
  TRoot = class(TObject, IRoot)
    FNested: INested;
    function GetNested: INested; 
    constructor Create; 
  end;  
  
function TNested.GetCNT: Int32;
begin
  Result := FCNT;
end;

constructor TNested.Create;
begin
  FCNT := 44;
end; 

function TRoot.GetNested: INested;
begin
  Result := FNested;
end;

constructor TRoot.Create;
begin
  FNested := TNested.Create();
end;   

var Obj: IRoot;
    G: Int32;

procedure Test;
begin
  Obj := TRoot.Create();
  G := Obj.Nested.CNT; 
end;

initialization
  Test();

finalization
  Assert(G = 44);
end.