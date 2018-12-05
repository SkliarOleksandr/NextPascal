unit str_literal_multi_1;

interface

implementation

const C = "sql"  select id, name 
    from table1
   where name = "name1"
"sql";

var S: string; 

uses sys.console;

initialization
  S := C;
  log(S);
finalization

end.