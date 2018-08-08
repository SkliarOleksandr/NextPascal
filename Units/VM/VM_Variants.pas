unit VM_Variants;

interface

type
  Variant = packed record
  private
    VType: UInt16;
    VData1: Int32;
    VData2: Int32;  
    VData3: Int32;
    VType4: Int16;        
    constructor Init;
    destructor Final; 
    operator Implicit(const Value: Int32): Variant; overload;    
    operator Implicit(const Value: String): Variant; overload;
    operator Implicit(const Value: Variant): Int32; overload;
    operator Implicit(const Value: Variant): String; overload;                                        
  end;
  
const sVariants = 'Variants';
	
function VarIsNull(const V: Variant): Boolean; external sVariants;
function VarIsEmpty(const V: Variant): Boolean; external sVariants;

implementation
 
procedure _VarFromInt(var Dst: Variant; const Src: Int32); external sVariants;
procedure _VarFromUStr(var Dst: Variant; const Src: String); external sVariants;
procedure _VarToInt(const Src: Variant; var Dst: Int32); external sVariants;
procedure _VarToUStr(const Src: Variant; var Dst: String); external sVariants;
procedure _VarClear(const Src: Variant); external sVariants;  
 

constructor Variant.Init;
begin
  VType := 0;
end;

constructor Variant.Final;
begin
  _VarClear(Self);
end;  

operator Variant.Implicit(const Value: Int32): Variant; overload;
begin
  _VarFromInt(Result, Value);
end;      

operator Variant.Implicit(const Value: String): Variant; overload;
begin
  _VarFromUStr(Result, Value);
end;      
   
operator Variant.Implicit(const Value: Variant): Int32; overload;
begin
  _VarToInt(Value, Result);
end;

operator Variant.Implicit(const Value: Variant): String; overload;
begin
  _VarToUStr(Value, Result);
end;

end.