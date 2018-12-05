unit dynarray_of_string_2;

interface

implementation

type
  TItem = record
    Key: string;
    Value: Int32; 
  end;
  
var
  FList: array of TItem; 

procedure Add(const Str: string);
var
  Len: Int32;
begin
  Len := Length(FList);
  SetLength(FList, Len + 1);
  FList[Len].Key := Str;
  FList[Len].Value := Len; 
end;

procedure Test;
begin
  Add('aaa');
  Add('bbb');
  Add('ccc');    
end;

initialization
  Test();

finalization
  Assert(Length(FList) = 3);
  Assert(FList[0].Key = 'aaa');
  Assert(FList[1].Key = 'bbb');
  Assert(FList[2].Key = 'ccc');

  Assert(FList[0].Value = 0);   
  Assert(FList[1].Value = 1);         
  Assert(FList[2].Value = 2); 
end.