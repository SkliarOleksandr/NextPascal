unit TStringsUnit;

interface

implementation

uses System;

type
  TStrings<TData> = class
  private  
  type
    TItem = record
      Key: string;
      Value: TData; 
    end;
    TItemList = array of TItem; 
  var
    FList: TItemList; 
    FSorted: Boolean;
    function GetCount: Int32;
    function GetName(Index: Int32): string;
    function GetValue(Index: Int32): TData;
    procedure QuickSort(Values: TItemList; L, R: Int32);    
  public
    procedure Add(const Str: string);
    procedure Sort;
    property Count: Int32 read GetCount;
    property Sorted: Boolean read FSorted;
    property Names[Index: Int32]: string read GetName;
    property Values[Index: Int32]: TData read GetValue;       
    function Find(const S: string; var Index: Int32): Boolean;
    function IndexOf(const S: string): Int32;   
  end;

function Compare(const L, R: string): Int32;
begin
  if L < R then
    Result := -1
  else if L > R then
    Result := 1
  else
    Result := 0;
end;

function TStrings<TData>.Find(const S: string; var Index: Integer): Boolean;
var
  L, H, I, C: Integer;
begin
  Result := False;
  L := 0;
  H := Count - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := Compare(FList[I].Key, S);
    if C < 0 then L := I + 1 else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Result := True;
        Index := I;        
        Exit;
        //if Duplicates <> dupAccept then L := I;
      end;
    end;
  end;  
  Index := L;
end; 

function TStrings<TData>.IndexOf(const S: string): Int32;
begin
  if not Find(S, Result) then
    Result := -1;  
end;

function TStrings<TData>.GetCount: Int32;
begin
  Result := Length(FList);
end;

function TStrings<TData>.GetName(Index: Int32): string;
begin
  Result := FList[Index].Key;
end;

function TStrings<TData>.GetValue(Index: Int32): TData;
begin
  Result := FList[Index].Value;
end;

procedure TStrings<TData>.Add(const Str: string);
var
  Len: Int32;
begin
  Len := Length(FList);
  SetLength(FList, Len + 1);
  FList[Len].Key := Str; 
end;

procedure TStrings<TData>.QuickSort(Values: TItemList; L, R: Int32);
var
  I, J: Int32;
  pivot: string; 
  temp: TItem;
begin
  if (Length(Values) = 0) or ((R - L) <= 0) then
    Exit;
  repeat
    I := L;
    J := R;
    pivot := Values[L + (R - L) shr 1].Key;
    repeat
      while Compare(Values[I].Key, pivot) < 0 do
        Inc(I);
      while Compare(Values[J].Key, pivot) > 0 do
        Dec(J);
      if I <= J then
      begin
        if I <> J then
        begin
          temp := Values[I];
          Values[I] := Values[J];
          Values[J] := temp;
        end;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSort(Values, L, J);
    L := I;
  until I >= R;
end;

procedure TStrings<TData>.Sort;
begin
  QuickSort(FList, Low(FList), High(FList));
end;

type
  TStr = TStrings<Int32>;

var Str: TStr;
    Idx: Int32;
    GC: Int32;
 
procedure Test;
begin
  Str := TStr.Create();
  Str.Add('aaa');
  Str.Add('ccc');
  Str.Add('bbb');   
  Str.Sort(); 
  GC := Str.Count;   
  Idx := Str.IndexOf('bbb'); 
end;

initialization  
  Test();

finalization
  Assert(GC = 3);
  Assert(Idx = 1);  
  Assert(Str.Names[0] = 'aaa');
  Assert(Str.Names[1] = 'bbb');
  Assert(Str.Names[2] = 'ccc');              

end.