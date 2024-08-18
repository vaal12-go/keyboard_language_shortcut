
unit Lexer;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LazLogger, Character, JwaWinUser;

type



  PToken = ^Token;

  Token = record
    TokenType : string;
    TokenLiteral: string;
  end;

  LineOfTokens = array of PToken;

  PTShortcutLangRec = ^TShortcutLangRec;

  TShortcutLangRec = record
    KbModifierArr : array of integer;
    Key : integer;
    langName : string;
    langIconName : string;
  end;

  TParserFunc = ^ParserFunc;
  ParserFunc = Function(tkn : PToken; shortCutRec : PTShortcutLangRec): TParserFunc;
  //ParserFunc = function:pointer;


  TLexer = class
     procedure TestCall();
     procedure StartLine(textLine: string);
     function NextToken():PToken;
     //procedure Create(textLine :string);

     private
     lexingLine : string;
     currPositionInLine : integer;
     //currSymbol : char;
     function ReadChar(): char;
     function ReadIdentifier(firstChar : char): string;
     procedure SkipWhiteSpace();
  end;//TLexer = class

  function ParseLine(line: string): PTShortcutLangRec;
  function ParseModifier(tkn : PToken; shortCutRec : PTShortcutLangRec): ParserFunc;

  const
    HASHTAG = '#';
    HYPHEN = '-';
    COLON = ':';
    OPEN_SQ_BRACKET = '[';
    CLOSE_SQ_BRACKET = ']';

    IDENTIFIER = 'IDENTIFIER';
    EOF_POSITION = -100;

    EOF_TYPE = 'EOF';
    EOFCH = char(0);





implementation

var
  PARSEMODIFIER_FUNC: ParserFunc;
    PARSEKEY_FUNC:  ParserFunc;

function TLexer.ReadIdentifier(firstChar : char): string;
var
  retStr : string;
  currChar : char;
begin
  retStr := '';
  currChar:= firstChar;
  while (currChar <> '') and (TCharacter.IsLetterOrDigit(currChar)) do begin
        retStr := retStr + lexingLine[currPositionInLine];
        currChar := ReadChar();
  end;
  currPositionInLine := currPositionInLine-1;
  exit(retStr);
end;

procedure TLexer.SkipWhiteSpace();
var
  currChar : char;
begin
  currChar := lexingLine[currPositionInLine];
  while TCharacter.IsWhiteSpace(currChar) do
     currChar := ReadChar();

end;

function TLexer.ReadChar():char;
begin
  if currPositionInLine >= (Length(lexingLine)) then begin
       currPositionInLine:=EOF_POSITION;
       exit(chr(0));
  end
  else begin
       currPositionInLine:= currPositionInLine+1;
       exit(lexingLine[currPositionInLine]);
  end;
end;

function TLexer.NextToken():PToken;
var
  tkn : PToken;
begin
  new(tkn);
  ReadChar();
  if currPositionInLine = EOF_POSITION then begin
     tkn^.TokenType := EOF_TYPE;
     tkn^.TokenLiteral := '';
     exit(tkn);
  end;

  SkipWhiteSpace();

  if  currPositionInLine = EOF_POSITION then
      exit(nil);

  case lexingLine[currPositionInLine] of
       HASHTAG : begin
            tkn^.TokenType := HASHTAG;
            tkn^.TokenLiteral := lexingLine[currPositionInLine];
       end;
       HYPHEN : begin
            tkn^.TokenType := HYPHEN;
            tkn^.TokenLiteral := lexingLine[currPositionInLine];
       end;
       COLON : begin
            tkn^.TokenType := COLON;
            tkn^.TokenLiteral := lexingLine[currPositionInLine];
       end;
       OPEN_SQ_BRACKET : begin
            tkn^.TokenType := OPEN_SQ_BRACKET;
            tkn^.TokenLiteral := lexingLine[currPositionInLine];
       end;
       CLOSE_SQ_BRACKET : begin
            tkn^.TokenType := CLOSE_SQ_BRACKET;
            tkn^.TokenLiteral := lexingLine[currPositionInLine];
       end;
       char(0) : begin
            tkn^.TokenType := EOF_TYPE;
            tkn^.TokenLiteral := '';
       end;
       else begin
            if TCharacter.IsLetterOrDigit(lexingLine[currPositionInLine]) then begin
               tkn^.TokenType := IDENTIFIER;
               tkn^.TokenLiteral := ReadIdentifier(lexingLine[currPositionInLine]);
            end;
       end;
  end;

  exit(tkn);


end;

procedure PrintToken(tkn: PToken);
begin
  if tkn = nil then
      DebugLn('Have NIL token')
  else begin
       DebugLn('Have token:'+tkn^.TokenType);
       DebugLn(#9+tkn^.TokenLiteral);
  end;
end;

function IsModifier(ident : string): integer;
begin
  ident := LowerCase(ident);
  case ident of
       'alt' : exit(MOD_ALT);
       'ctrl' : exit(MOD_CONTROL);
       'shift' : exit(MOD_SHIFT);
       'win' : exit(MOD_WIN);
       else exit(-1);
  end;
end;//function IsModifier(ident : string): integer;

function ParseKey(tkn : PToken; shortCutRec : PTShortcutLangRec): ParserFunc;
begin
  exit(nil);
end;

function ParseModifier(tkn : PToken; shortCutRec : PTShortcutLangRec): ParserFunc;
var
  currMod :integer;
begin
  case tkn^.TokenType of
        IDENTIFIER: begin
             DebugLn('ParseModifier: have modifier:'+tkn^.TokenLiteral);
            currMod := IsModifier(tkn^.TokenLiteral);
            if currMod = -1 then begin
                 exit(ParseKey(tkn, shortCutRec));
            end
            else begin
                exit(PARSEMODIFIER_FUNC);
            end;
        end;//IDENTIFIER: begin
        HYPHEN: begin
            DebugLn('ParseModifier: have hyphen');
            exit(PARSEMODIFIER_FUNC);
        end;

        COLON: begin
            DebugLn('ParseModifier: have colon');
            exit(PARSEKEY_FUNC);
            end;
        else begin
            DebugLn('ParseModifier: unknown token');
            exit(nil)
        end;

   end;
end;

function ParseLineOfTokens(tkn_array : LineOfTokens):PTShortcutLangRec;
var
  i:integer;
  currTkn : PToken;
  phase : string;
  currParserFunc, tempParserFunc : ParserFunc;
  shLangRec : PTShortcutLangRec;
  //point : pointer;

begin
  PARSEMODIFIER_FUNC:=  ParserFunc(@ParseModifier);
  PARSEKEY_FUNC:=  ParserFunc(@ParseKey);
  currParserFunc:= ParserFunc(@ParseModifier);
  new(shLangRec);
  shLangRec^.KbModifierArr := [];
  shLangRec^.Key := -1;
  shLangRec^.langName :='';
  shLangRec^.langIconName :='';

  DebugLn(sLineBreak+sLineBreak+'Parsing line of tokens');
  DebugLn('***********************************');
  for currTkn in tkn_array do begin
      PrintToken(currTkn);
      point := currParserFunc(currTkn, shLangRec);
      currParserFunc := ParserFunc(point);
      if currParserFunc = nil then break;
  end;//for currTkn in tkn_array do begin
end;

function ParseLine(line: string): PTShortcutLangRec;
var
  shLangRec : PTShortcutLangRec;
  lx : TLexer;
  currToken :PToken;
  tkn_arr : LineOfTokens;

begin
  lx := TLexer.Create();
  lx.TestCall();
  lx.StartLine(line);
  DebugLn(sLineBreak+sLineBreak+'Starting new line');
  tkn_arr := [];
  repeat
    begin
      currToken := lx.NextToken();
      PrintToken(currToken);
      if currToken^.TokenType = HASHTAG then begin
           DebugLn('Found hashtag - skipping to the end of line');
           Break;
      end;
      insert(currToken, tkn_arr, Length(tkn_arr));

    end;
  until (currToken = nil) or (currToken^.TokenType = EOF_TYPE);

  shLangRec := ParseLineOfTokens(tkn_arr);

  if  shLangRec^.langName = '' then exit(nil)
  else exit(shLangRec);
end;

procedure TLexer.StartLine(textLine :string);
begin
  lexingLine := textLine;
  DebugLn('Lexing line:'+lexingLine);
  currPositionInLine:= 0;
end;

procedure TLexer.TestCall();
begin
  DebugLn('I am Lexer');

end;

end.

