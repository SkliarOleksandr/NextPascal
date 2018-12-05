unit record_refs_5;

interface

implementation

type
  TAVLNode = record
  private
    FChilds: array[Boolean] of ^TAVLNode; 
    FBalance: Int32;         
  end;
  PAVLNode = ^TAVLNode;

function Get: PAVLNode;
begin
  if Assigned(Result.FChilds[False]) then;
end; 

procedure Test;
begin

end;

initialization
  Test();

finalization

end.