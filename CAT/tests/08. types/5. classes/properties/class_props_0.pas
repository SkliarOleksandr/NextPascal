unit class_props_0;

interface

implementation

uses System;

type 
  TNested = class
  private
    FCnt: Int32;
    function GetCnt: Int32;
  public
    property Cnt: Int32 read GetCnt;
  end;
  
  TRoot = class
  private
    FNested: TNested;
  public
    property Nested: TNested read FNested;
    constructor Create;        
  end;
  
function TNested.GetCnt: Int32;
begin
  Result := FCNT;
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