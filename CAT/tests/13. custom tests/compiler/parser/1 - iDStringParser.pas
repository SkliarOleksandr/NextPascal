#skip
unit iDStringParser;

interface

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}


uses System; // Classes, SysUtils;

const
  MaxTokenChar = #127;

type


  TTokenType = (ttNone, ttToken, ttOmited, ttDigit, ttNewLine, ttHexPrefix, ttUnicodeChars, ttBinPrefix, ttQuote, ttOneLineRem, ttStartRem, ttEndRem);

  TIdentifierType = (
    itNone,
    itIdentifier, // named identifier like: 'a', 'value', 'width', etc.
    itSymbol,     // value char symbol like: #10, , #32, etc.#13
    itChar,       // char symbol like: 'a', 'b', '1', etc.
    itString,     // string like: 'abc', '111222333', etc.
    itInteger,    // integer number like: 12345, -4534534534, etc.
    itHextNumber, // hexadecimal number like: $FF, -$AABB, $12, etc.
    itBinNumber,  // binary number like: %1010101, -%1010101, etc.
    itFloat       // floating point number like: 1.5, -4234.53, etc
  );


  TParserErrorState = (psNoError, psCommentError, psStringError, psCharError, psSyntaxError);

  TTokenFlag = (tfBinDigit, tfHexDigit, tfDigit, tfFloat, tfSeparator, tfEndBlockRem);
  TTokenFlags = set of TTokenFlag;

  TCharToken = record
  type
    TTokens = array of TCharToken;
  var
    TokenChar: Char;
    TokenID: Integer;
    TokenType: TTokenType;
    Flags: TTokenFlags;
    ChildTokens: TTokens;
  end;
  PCharToken = ^TCharToken;

  TParseTokens = array [#0..#127] of TCharToken;
  PParseTokens = ^TParseTokens;

  TTextPosition = packed record
    Row: Integer;
    Col: Integer;
  end;

  TIdentifier = record
    Name: string;
    TextPosition: TTextPosition;
  end;

  TParserPosition = record
    SourcePosition: Integer;
    Row: Integer;
    LastEnterPos: Integer;
    TokenID: Integer;
    OriginalToekn: string;
  end;

  TStringParser = class
  strict private
    FTokens: TParseTokens;
    FSource: string;                   // ?????
    FLength: integer;                  // ??????????????? ?????? ??????
    FSrcPos: integer;                  // ??????? ??????? ??????? ? ??????
    FCurrentToken: string;
    FCurrentTokenID: Integer;
    FRow: Integer;                     // ????? ?????? ??????? ??????? (?????????? ? ???????)
    FLastEnterPos: Integer;            // ??????? ?????????? ???????? ?????? (????????? ??? ??????????? ??????? ??????? (row,col))
    FPrevPostion: TTextPosition;       // ??????? ??????????? ?????? ??????? (row,col)
    FUpCase: array [#0..#127] of AnsiChar; // ??????? ? UpCase
    FIdentifireType: TIdentifierType;  // ??? ?????????????? (???????/??????/?????/??????)
    FIdentifireID: Integer;            // TokenID ???? ?????????????
    FEofID: Integer;                   // TokenID ???? ????? ?????
    FTokenCaptions: TStrings;          // ???????? ????????
    FSeparators: string;
    FErrorState: TParserErrorState;
    FOmitted: string;
    FTokenIDGenerator: Integer;
  private
    function GetPosition: TTextPosition; inline;
    function GetLinePosition: TTextPosition; inline;
    procedure SetSource(const Value: string);
    procedure SetSeparators(const Value: string);
    procedure SetOmitted(const Value: string);
    function GetTokenName: string;
  protected
    procedure RegisterToken(const Token: string; aTokenID: Integer; aTokenType: TTokenType; const TokenCaption: string = '');
    procedure RegisterRemToken(const BeginToken, EndToken: string);
    property TokenCaptions: TStrings read FTokenCaptions;
    property EofID: Integer read FEofID write FEofID;
    property IdentifireID: Integer read FIdentifireID write FIdentifireID;
    property SeparatorChars: string read FSeparators write SetSeparators;
    property OmittedChars: string read FOmitted write SetOmitted;
    property Tokens: TParseTokens read FTokens;
  public
    constructor Create(const Source: string); virtual;
    destructor Destroy; override;
    ///////////////////////////////////////////////////
    procedure First;
    function NextTokenID: Integer;
    property CurrentTokenID: Integer read FCurrentTokenID;
    property ErrorState: TParserErrorState read FErrorState;
    property Source: string read FSource write SetSource;
    property SourcePosition: Integer read FSrcPos;
    property OriginalToken: string read FCurrentToken;            // ??? ?? ??????????????? ????? ?????? ??????
    property TokenName: string read GetTokenName;                 // ????????? ????????????? ??????
    property Position: TTextPosition read GetPosition;            // ??????? ???????? ?????? ??????? (row,col)
    property LinePosition: TTextPosition read GetLinePosition;
    property IdentifireType: TIdentifierType read FIdentifireType;
    function TokenLexem(TokenID: Integer): string;
    procedure GetIdentifier(var ID: TIdentifier);
    procedure GetTokenAsIdentifier(var ID: TIdentifier);
    property PrevPosition: TTextPosition read FPrevPostion;
    procedure SaveState(out State: TParserPosition);
    procedure LoadState(const State: TParserPosition);
  end;

implementation

{ TStringParser }

constructor TStringParser.Create(const Source: string);
var
  c: Char;
begin
  FTokenIDGenerator := 1000;
  SetSource(Source);
  FTokenCaptions := TStringList.Create;
  FTokens[MaxTokenChar].TokenType := ttUnicodeChars;
  FTokens['0'].Flags := [tfBinDigit];
  FTokens['1'].Flags := [tfBinDigit];
  for c := '0' to '9' do begin
    with FTokens[c] do begin
      TokenType := ttDigit;
      Flags := Flags + [tfDigit, tfHexDigit];
    end;
  end;
  for c := 'A' to 'F' do begin
    FTokens[c].Flags := [tfHexDigit];
  end;
  // UpCase ???????
  for c := Low(FUpCase) to High(FUpCase) do
    FUpCase[c] := AnsiChar(UpCase(c));
end;

destructor TStringParser.Destroy;
begin
  FTokenCaptions.Free;
  inherited;
end;

procedure TStringParser.First;
begin
  FSrcPos := 1;
  FRow := 1;
  FLastEnterPos := 0;
end;


//procedure TStringParser.ProcessUnicodeChars(Pos: Integer)

function TStringParser.NextTokenID: Integer;
type
  TNumberSymbols = set of (nsExponent, nsPoint, nsSign);
var
  Pos1, Pos2, ReadedChars, i, RemID: integer;
  c, QuoteChar: Char;
  pToken, ptmp, ptmp2: PCharToken;
  NumberSymbols: TNumberSymbols;
begin
  FPrevPostion := Position;
  FIdentifireType := itNone;
  Result := FEofID;
  pos1 := FSrcPos;
  ReadedChars := 1;
  while pos1 <= FLength do begin
    c := FSource[pos1];
    if c < MaxTokenChar then begin
      // ?????? ??????? ??????? ? ?????? Tokens ???? ? ? LowerCase ? ? UpperCase
      // ??????? ????????? ? UpperCase ?? ?????
      pToken := @Tokens[c];
      if Assigned(pToken.ChildTokens) then
      begin
        ptmp2 := pToken;
        pos2 := pos1 + 1;
        while pos2 <= FLength do begin
          // ????????? ??????? ??????? ? ?????? Tokens ?????? ? UpperCase
          // ??????? ???????? ? UpperCase
          c := FSource[pos2];
          if c > MaxTokenChar then begin
            pToken := ptmp2;
            Break;
          end;
          c := Char(FUpCase[c]);
          ptmp := nil;
          for i := 0 to Length(ptmp2.ChildTokens) - 1 do
          begin
            ptmp := @ptmp2.ChildTokens[i];
            if c = ptmp.TokenChar then begin
              ptmp2 := ptmp;
              Inc(Pos2);
              Break; // next step in multi-char block rem
            end;
          end;
          if ptmp = ptmp2 then Continue else Break;
        end;
        if ptmp2 <> pToken then begin
          if (ptmp2.TokenType <> ttNone) and ((pos2 >= FLength) or (tfSeparator in Tokens[c].Flags) or (tfSeparator in ptmp2.Flags)) then begin
            pToken := ptmp2;
          end else
            ReadedChars := Pos2 - pos1;
          pos1 := Pos2 - 1;
        end;
      end;
    end else
      pToken := @Tokens[MaxTokenChar]; // Unicode chars...

    with pToken^ do begin
      case TokenType of
        ttNone: begin // read some identifier:
          Inc(pos1);
          while pos1 <= FLength do begin
            if tfSeparator in Tokens[FSource[pos1]].Flags then Break;
            Inc(ReadedChars);
            Inc(pos1);
          end;
          // read identifier:
          FCurrentToken := Copy(FSource, Pos1 - ReadedChars, ReadedChars);
          FIdentifireType := itIdentifier;
          Result := FIdentifireID;
          Break;
        end;
        ttToken: begin
          Result := TokenID;
          FCurrentTokenID := Result;
          FSrcPos := Pos1 + 1;
          Exit;
        end;
        ttOmited: begin
          Inc(pos1);
          Continue;
        end;
        ttUnicodeChars: begin // read symbol constant
          ReadedChars := 1;
          Inc(pos1);
          while pos1 <= FLength do begin
            c := FSource[pos1];
            if (c < MaxTokenChar) and (tfSeparator in Tokens[c].Flags) then Break;
            Inc(ReadedChars);
            Inc(pos1);
          end;
          // read identifier:
          FCurrentToken := Copy(FSource, Pos1 - ReadedChars, ReadedChars);
          FIdentifireType := itIdentifier;
          Result := FIdentifireID;
          // end read;
          Break;
        end;
        ttDigit: begin
          NumberSymbols := [];
          ReadedChars := 0;
          Inc(pos1);
          Inc(ReadedChars);
          while pos1 <= FLength do begin
            c := Char(FUpCase[FSource[pos1]]);
            if c = 'E' then begin
              if nsExponent in NumberSymbols then Break;
              Include(NumberSymbols, nsExponent);
            end else
            if (c = '.') then begin
              pos2 := pos1 + 1;
              if (nsPoint in NumberSymbols) or (Pos2 > FLength) or (not (tfDigit in Tokens[FSource[pos2]].Flags)) then break;
              Include(NumberSymbols, nsPoint);
            end else
            if ((c = '+') or (c = '-')) and (nsExponent in NumberSymbols) then begin
              if nsSign in NumberSymbols then Break;
              Include(NumberSymbols, nsSign);
            end else
            if (Tokens[c].TokenType <> ttDigit) then Break;
            Inc(pos1);
            Inc(ReadedChars);
          end;
          // read identifier:
          FCurrentToken := Copy(FSource, Pos1 - ReadedChars, ReadedChars);
          if nsPoint in NumberSymbols then
            FIdentifireType := itFloat
          else
            FIdentifireType := itInteger;
          Result := FIdentifireID;
          // end read;
          Break;
        end;
        ttNewLine: begin
          // process NEWLINE:
          Inc(FRow);
          FLastEnterPos := pos1;
        end;
        ttHexPrefix: begin
          Inc(pos1);
          ReadedChars := 0;
          while (pos1 <= FLength) and (tfHexDigit in Tokens[UpCase(FSource[pos1])].Flags) do begin
            inc(pos1);
            Inc(ReadedChars);
          end;
          // read identifier:
          FCurrentToken := Copy(FSource, Pos1 - ReadedChars, ReadedChars);
          FIdentifireType := itHextNumber;
          Result := FIdentifireID;
          // end read;
          Break;
        end;
        ttBinPrefix: begin
          Inc(pos1);
          ReadedChars := 0;
          while (pos1 <= FLength) and (tfBinDigit in Tokens[UpCase(FSource[pos1])].Flags) do begin
            Inc(pos1);
            Inc(ReadedChars);
          end;
          // read identifier:
          FCurrentToken := Copy(FSource, Pos1 - ReadedChars, ReadedChars);
          FIdentifireType := itBinNumber;
          Result := FIdentifireID;
          // end read;
          Break;
        end;
        ttQuote: begin
          QuoteChar := c;
          ReadedChars := 0;
          Inc(pos1);
          while pos1 <= FLength do begin
            c := FSource[pos1];
            if (c = QuoteChar) then
            begin
              pos2 := pos1 + 1;
              if (pos2 <= FLength) and (FSource[pos2] = QuoteChar) then
              begin
                Inc(pos1); // skip second quote char
                Inc(ReadedChars);
              end else begin
                Break;
              end;
            end;
            Inc(pos1);
            Inc(ReadedChars);
          end;
          // read identifier:
          FCurrentToken := Copy(FSource, Pos1 - ReadedChars, ReadedChars);
          if ReadedChars = 1 then
            FIdentifireType := itChar
          else
            FIdentifireType := itString;
          Result := FIdentifireID;
          // end read;
          Inc(pos1);
          Break;
        end;
        ttOneLineRem: begin
          repeat
             Inc(pos1);
             if pos1 > FLength then
               Break;

             c := FSource[pos1];
             if (Ord(c) < Length(Tokens)) and (Tokens[c].TokenType = ttNewLine) then
               Break;
          until False;
          // process NEWLINE:
          Inc(FRow);
          FLastEnterPos := pos1;
        end;
        ttStartRem: begin
          RemID := TokenID;         
          while pos1 < FLength do begin
            Inc(pos1);
            pToken := @Tokens[FSource[pos1]];
            // process NEWLINE:
            if pToken.TokenType = ttNewLine then begin
              Inc(FRow);
              FLastEnterPos := pos1;
            end else
            if (tfEndBlockRem in pToken.Flags) then begin
              if pToken.TokenType = ttEndRem then begin
                 if pToken.TokenID = RemID then Break // end of "one-char" block rem                            
              end else begin                    
                pos2 := pos1 + 1;
                while pos2 < FLength do begin
                  c := FSource[pos2];
                  for i := 0 to Length(pToken.ChildTokens) - 1 do
                  begin
                    ptmp := @pToken.ChildTokens[i]; 
                    with ptmp^ do
                    if c = TokenChar then begin
                      if tfEndBlockRem in Flags then begin 
                        if (TokenID = RemID) and (TokenType = ttEndRem) then begin
                          Pos1 := Pos2;
                          Break; // end of "multi-char" block rem  
                        end;  
                      end else begin
                        Inc(Pos2);
                        Break; // next step in multi-char block rem
                      end;                                      
                    end else Continue;                  
                    Break;
                  end;
                  Break;
                end;                                                
                if pos1 = pos2 then Break; // "multi-char" block rem was successfully found
              end;              
            end;
          end;
        end;
        ttEndRem: begin
          Inc(Pos1);
          Result := FIdentifireID;// !!!
          Break;        
        end;
      end;
    end;
    Inc(Pos1);
  end;
  FCurrentTokenID := Result;
  FSrcPos := Pos1;
end;

procedure TStringParser.RegisterRemToken(const BeginToken, EndToken: string);
begin
  RegisterToken(BeginToken, FTokenIDGenerator, ttStartRem);
  RegisterToken(EndToken, FTokenIDGenerator, ttEndRem);
  Inc(FTokenIDGenerator);
end;

procedure TStringParser.RegisterToken(const Token: string; aTokenID: Integer; aTokenType: TTokenType; const TokenCaption: string);
var
  i, j, c, c2: Integer;
  pToken, pt: PCharToken;
  ch: Char;
  s: string;
begin
  c := Length(Token);
  {$IFDEF DEBUG}
  if c = 0 then raise Exception.Create('Token must be assigned');
  {$ENDIF}
  pToken := @FTokens[UpCase(Token[1])];
  if aTokenType = ttEndRem then
    Include(pToken.Flags, tfEndBlockRem);
  for i := 2 to c do begin
    ch := UpCase(Token[i]);
    pt := nil;
    with pToken^ do begin
      c2 := Length(ChildTokens);
      for j := 0 to c2 - 1 do
        if ChildTokens[j].TokenChar = ch then begin
          pt := @ChildTokens[j];
          Break;
        end;
      if not Assigned(pt) then begin
        SetLength(ChildTokens, c2 + 1);
        pt := @ChildTokens[c2];
        pt.TokenChar := ch;
        pt.Flags := FTokens[ch].Flags;
        if aTokenType = ttEndRem then
          Include(pt.Flags, tfEndBlockRem);
      end;
    end;
    pToken := pt;
  end;
  pToken.TokenType := aTokenType;
  pToken.TokenID := aTokenID;
  if TokenCaption <> '' then
    s := TokenCaption
  else
    s := Token;
  FTokenCaptions.AddObject(s, TObject(aTokenID));
  // ????????? LowerCase
  ch := Char(FUpCase[Token[1]]);
  FTokens[AnsiLowerCase(ch)[1]] := FTokens[ch];
end;

procedure TStringParser.SaveState(out State: TParserPosition);
begin
  State.SourcePosition := FSrcPos;
  State.Row := FRow;
  State.LastEnterPos := FLastEnterPos;
  State.TokenID  := FCurrentTokenID;
  State.OriginalToekn := FCurrentToken;
end;

procedure TStringParser.LoadState(const State: TParserPosition);
begin
  FSrcPos := State.SourcePosition;
  FRow := State.Row;
  FLastEnterPos := State.LastEnterPos;
  FCurrentTokenID := State.TokenID;
  FCurrentToken := State.OriginalToekn;
end;

procedure TStringParser.SetOmitted(const Value: string);
var
  i: Integer;
  c: Char;
begin
  for i := 1 to Length(Value) do begin
    c := Value[i];
    {$IFDEF DEBUG}
    if c > MaxTokenChar then
      raise Exception.CreateFmt('Token  = "%s" is out of range', [c]);
    {$ENDIF}
    with FTokens[UpCase(c)] do begin
      TokenChar := Value[i];
      TokenType := ttOmited;
    end;
  end;
  FOmitted := Value;
end;

procedure TStringParser.SetSeparators(const Value: string);
var
  i: Integer;
  c: Char;
begin
  for i := 1 to Length(Value) do begin
    c := Value[i];
    {$IFDEF DEBUG}
    if c > MaxTokenChar then
      raise Exception.CreateFmt('Token  = "%s" is out of range', [c]);
    {$ENDIF}
    Include(FTokens[c].Flags, tfSeparator);
  end;
  FSeparators := Value;
end;

procedure TStringParser.SetSource(const Value: string);
begin
  FLength := Length(Value);
  FSource := Value;
  First;
end;

function TStringParser.TokenLexem(TokenID: Integer): string;
var
  i: Integer;
begin
  i := FTokenCaptions.IndexOfObject(TObject(TokenID));
  if i <> -1 then
    Result := FTokenCaptions[i]
  else
    Result := '';
end;

function TStringParser.GetLinePosition: TTextPosition;
begin
  Result.Row := FRow;
  Result.Col := -1;
end;

procedure TStringParser.GetIdentifier(var ID: TIdentifier);
begin
  ID.Name := FCurrentToken;
  ID.TextPosition.Row := FRow;
  ID.TextPosition.Col := FSrcPos - FLastEnterPos;
end;

function TStringParser.GetPosition: TTextPosition;
begin
  Result.Row := FRow;
  Result.Col := FSrcPos - FLastEnterPos;
end;

procedure TStringParser.GetTokenAsIdentifier(var ID: TIdentifier);
begin
  ID.Name := TokenLexem(FCurrentTokenID);
  ID.TextPosition.Row := FRow;
  ID.TextPosition.Col := FSrcPos - FLastEnterPos;
end;

function TStringParser.GetTokenName: string;
begin
  if FCurrentTokenID = FIdentifireID then
    Result := FCurrentToken
  else
    Result := TokenLexem(FCurrentTokenID);
end;

end.
