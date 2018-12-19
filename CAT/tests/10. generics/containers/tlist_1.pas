unit tlist_1;

interface

uses System;

type
  TList<T> = class
  private 
  type
    TItems = array of T;
  var  
    FItems: TItems;    
    FCount: Int32;
    procedure Grow; 
    procedure SetCapacity(Value: Int32);
    procedure MoveUpItems(FromIdx, ToIdx: Int32);
    function GetCapacity: Int32;   
  public
    property Count: Int32 read FCount;
    property Capacity: Int32 read GetCapacity write SetCapacity;    
    procedure Add(const Value: T);
    procedure Delete(Index: Int32);
        
  end;


implementation

procedure TList<T>.Grow;
var
  Len: Int32; 
begin
  Len := Length(FItems);
  SetLength(FItems, Len*2);
end; 

procedure TList<T>.SetCapacity(Value: Int32);
begin
  if Value < FCount then
    FCount := Value;
  SetLength(FItems, Value);
end;

procedure TList<T>.MoveUpItems(FromIdx, ToIdx: Int32);
begin
//  Move(FItems
end;

function TList<T>.GetCapacity: Int32;
begin
  Result := Length(FItems);
end;

procedure TList<T>.Add(const Value: T);
begin
  if FCount < Capacity then
  begin
    FItems[FCount] := T;
    Inc(FCount);
  end else
    Grow();       
end;

procedure TList<T>.Delete(Index: Int32);
begin
  
end;

procedure Test;
begin

end;

initialization
  Test();

finalization

end.