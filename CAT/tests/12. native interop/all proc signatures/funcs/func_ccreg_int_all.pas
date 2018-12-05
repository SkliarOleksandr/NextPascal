unit func_ccreg_int_all;

interface

implementation

function F(a1: int8;
           a2: uint8;
           a3: int16;
           a4: uint16;
           a5: int32;
           a6: uint32;
           a7: int64;
           a8: uint64;
           a9: AnsiChar;
           a10: Char): Int64; external 'CAT' name 'func_ccreg_int_all';

var G: Int64;
  
procedure Test;
var
  c: AnsiChar = 'A';
begin
   G := F(1, 2, 3, 4, 5, 6, 7, 8, C, 'B');
end;

var S: Int64;  

initialization
  Test();
  
finalization
  s := 1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + ord('A') + ord('B'); 
  Assert(G = S);
end.