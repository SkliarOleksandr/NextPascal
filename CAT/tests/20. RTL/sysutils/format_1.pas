unit format_1;

interface

implementation

var
  ACnt: Integer;
  Idx: Integer;
  ResLen: Integer;
  ResPos: Integer; 
  ArgStrings: array of string;
  S: string;  

function format(const Fmt: string; const Args: array of Variant): string;


begin
  ResLen := Length(Fmt);
  ACnt := Length(Args);  
  SetLength(ArgStrings, ACnt);
  // преобразуем аргументы в строки  
  for var i := 0 to ACnt - 1 do
  begin
    var S: string := Args[i];
    Inc(ResLen, Length(S));      
    ArgStrings[i] := S;
  end;
  // выдел€ем пам€ть под результат
  //SetLength(Result, ResLen);
    
    
  // поиск мест вставки аргументов
  ResPos := 0;
  var StartIdx: Int32 := 0;
  var ArgIdx: Int32 := 0; 
  for var i := 0 to Length(Fmt) - 2 do
  begin   
    var C := Fmt[i];
    if C = '%' then 
    begin
      var C2 := Fmt[i + 1];
      case C2 of
        's': begin
          {Move(Result, ResPos, Fmt, StartIdx, StartIdx - i - 2);  
          Inc(ResPos, StartIdx - i - 2); 
          
          Move(Result, ResPos, S, 0, Length(S));          
          StartIdx := i;}
          //var S := ArgStrings[ArgIdx]; 
          Result := Result + ArgStrings[ArgIdx];
          Inc(ArgIdx);          
        end;
      else
        continue; 
      end; 
    end;
  end;
  
  
  Idx := 0;   
   
end;

var
  S2: string;

procedure Test;
begin
  S2 := format('%s', [11]);
end;

initialization
  Test();

finalization

end.