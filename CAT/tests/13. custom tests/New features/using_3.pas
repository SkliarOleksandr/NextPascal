#skip
unit using_3;

interface

implementation

type
  TLinkedList<T> = record
  type 
    TNode = record
      Prev: ^TNode;  // nullable 
      Next: ^TNode;  // nullable     
      Value: T;      // not null
      constructor Create(const V: T);  
    end;
  private
    FFirst: ^TNode;  // nullable
    FLast: ^TNode    // nullable
  public
    procedure Add(const Value: T);
    property First: ^TNode;
    property Last: ^TNode;     
  end;  

function GetRef: TObject;
begin
 
end;

procedure TLinkedList<T>.Add(const Value: T);
begin
end;

procedure Test;
begin

end;

initialization
  Test();

finalization

end.