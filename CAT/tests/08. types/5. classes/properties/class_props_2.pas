unit class_props_2;

interface

implementation

uses System;

type 
  TNested = class
  private
    FCnt: Int32;
    function GetCnt: Int32; inline; 
  public
    property Cnt: Int32 read GetCnt;         
  end;
  
  TRoot = class
  private
    FNested: TNested;
    function GetNested: TNested; inline;
  public
    constructor Create;  
    property Nested: TNested read GetNested;    
  end;
  
function TNested.GetCnt: Int32;
begin
  Result := FCnt;
end; 

function TRoot.GetNested: TNested;
begin
  Result := FNested;
end;

constructor TRoot.Create;
begin
  FNested := TNested.Create();
  FNested.FCNT := 8;
end;  

var G: Int32;
  
procedure Test;
var
  Obj: TRoot;
begin
  Obj := TRoot.Create();
  G := Obj.Nested.Cnt;
end;

initialization
  Test();

finalization
  Assert(G = 8);
end.