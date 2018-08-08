unit Generics.Collections;

interface

{$MODE DELPHI}

uses fgl;

type

  TArray<T> = array of T;
  
  TList<T> = class(TFPGList<T>)
  end;

  { TDictionary }

  TDictionary<TKey, TData> = class(TFPGMap<TKey, TData>)
  public
    function TryGetValue(const Key: TKey; out Value: TData): Boolean;
  end;

  { TObjectList }

  TObjectList<T: class> = class(TFPGList<T>)
  private
    FOwnsObjects: Boolean;
  public
    constructor Create(AOwnsObjects: Boolean = True); overload;
    destructor Destroy; override;
    procedure Clear;
  end;

implementation

{ TObjectList<T> }

constructor TObjectList<T>.Create(AOwnsObjects: Boolean);
begin
  inherited Create;
  FOwnsObjects :=  AOwnsObjects;
end;

destructor TObjectList<T>.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TObjectList<T>.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Items[i].Free;
  inherited Clear;
end;

{ TDictionary }

function TDictionary<TKey, TData>.TryGetValue(const Key: TKey; out Value: TData): Boolean;
begin

end;

end.
