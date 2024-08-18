
unit Lexer;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LazLogger, Character, JwaWinUser, LexerConstants;

type

  TLexer = class
    procedure TestCall();
    procedure StartLine(textLine: string);
    function NextToken(): PToken;
    //procedure Create(textLine :string);

  private
    lexingLine: string;
    currPositionInLine: integer;
    //currSymbol : char;
    function ReadChar(): char;
    function ReadIdentifier(firstChar: char): string;
    procedure SkipWhiteSpace();
  end;//TLexer = class

//function ParseLine(line: string): PTShortcutLangRec;
//function ParseModifier(tkn: PToken; shortCutRec: PTShortcutLangRec): ParserFunc;

implementation

function TLexer.ReadIdentifier(firstChar: char): string;
var
  retStr: string;
  currChar: char;
begin
  retStr := '';
  currChar := firstChar;
  while (currChar <> '') and (TCharacter.IsLetterOrDigit(currChar)) do
  begin
    retStr := retStr + lexingLine[currPositionInLine];
    currChar := ReadChar();
  end;
  currPositionInLine := currPositionInLine - 1;
  exit(retStr);
end;

procedure TLexer.SkipWhiteSpace();
var
  currChar: char;
begin
  currChar := lexingLine[currPositionInLine];
  while TCharacter.IsWhiteSpace(currChar) do
    currChar := ReadChar();

end;

function TLexer.ReadChar(): char;
begin
  if currPositionInLine >= (Length(lexingLine)) then
  begin
    currPositionInLine := EOF_POSITION;
    exit(chr(0));
  end
  else
  begin
    currPositionInLine := currPositionInLine + 1;
    exit(lexingLine[currPositionInLine]);
  end;
end;

function TLexer.NextToken(): PToken;
var
  tkn: PToken;
begin
  new(tkn);
  ReadChar();
  if currPositionInLine = EOF_POSITION then
  begin
    tkn^.TokenType := EOF_TYPE;
    tkn^.TokenLiteral := '';
    exit(tkn);
  end;

  SkipWhiteSpace();

  if currPositionInLine = EOF_POSITION then
    exit(nil);

  case lexingLine[currPositionInLine] of
    HASHTAG: begin
      tkn^.TokenType := HASHTAG;
      tkn^.TokenLiteral := lexingLine[currPositionInLine];
    end;
    HYPHEN: begin
      tkn^.TokenType := HYPHEN;
      tkn^.TokenLiteral := lexingLine[currPositionInLine];
    end;
    COLON: begin
      tkn^.TokenType := COLON;
      tkn^.TokenLiteral := lexingLine[currPositionInLine];
    end;
    OPEN_SQ_BRACKET: begin
      tkn^.TokenType := OPEN_SQ_BRACKET;
      tkn^.TokenLiteral := lexingLine[currPositionInLine];
    end;
    CLOSE_SQ_BRACKET: begin
      tkn^.TokenType := CLOSE_SQ_BRACKET;
      tkn^.TokenLiteral := lexingLine[currPositionInLine];
    end;
    char(0): begin
      tkn^.TokenType := EOF_TYPE;
      tkn^.TokenLiteral := '';
    end;
    else
    begin
      if TCharacter.IsLetterOrDigit(lexingLine[currPositionInLine]) then
      begin
        tkn^.TokenType := IDENTIFIER;
        tkn^.TokenLiteral := ReadIdentifier(lexingLine[currPositionInLine]);
      end;
    end;
  end;

  exit(tkn);

end;




procedure TLexer.StartLine(textLine: string);
begin
  lexingLine := textLine;
  DebugLn('Lexing line:' + lexingLine);
  currPositionInLine := 0;
end;

procedure TLexer.TestCall();
begin
  DebugLn('I am Lexer');

end;

end.
