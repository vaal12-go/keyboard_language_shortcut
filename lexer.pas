unit Lexer;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LazLogger, Character, LexerConstants;

type

  TLexer = class
    procedure StartLine(textLine: string);
    function NextToken(): PToken;

  private
    lexingLine: string;
    currPositionInLine: integer;
    function ReadChar(): char;
    function ReadIdentifier(firstChar: char): string;
    function ReadNumber(firstChar: char): string;
    procedure SkipWhiteSpace();
  end;//TLexer = class


implementation

function TLexer.ReadNumber(firstChar: char): string;
var
  retStr: string;
  currChar: char;
begin
  retStr := '';
  currChar := firstChar;
  while TCharacter.IsDigit(currChar) do
  begin
    retStr := retStr + lexingLine[currPositionInLine];
    currChar := ReadChar();
  end;
  if currChar <> char(0) then //End of line
    currPositionInLine := currPositionInLine - 1;
  exit(retStr);
end;

function TLexer.ReadIdentifier(firstChar: char): string;
var
  retStr: string;
  currChar: char;
begin
  retStr := '';
  currChar := firstChar;
  while (TCharacter.IsLetterOrDigit(currChar)) or (currChar = '_') do
  begin
    retStr := retStr + lexingLine[currPositionInLine];
    currChar := ReadChar();
  end;
  if currPositionInLine <> EOF_POSITION then
    currPositionInLine := currPositionInLine - 1;
  exit(retStr);
end;

procedure TLexer.SkipWhiteSpace();
var
  currChar: char;
  prevPos: integer;
begin
  if currPositionInLine = EOF_POSITION then exit();
  prevPos := currPositionInLine;
  currChar := lexingLine[currPositionInLine];
  while TCharacter.IsWhiteSpace(currChar) do
    currChar := ReadChar();
  if (currPositionInLine <> EOF_POSITION) and (prevPos <> currPositionInLine) then
    currPositionInLine := currPositionInLine - 1;
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
  currChar: char;
begin
  new(tkn);
  currChar := ReadChar();

  if TCharacter.IsWhiteSpace(currChar) then
  begin
    SkipWhiteSpace();
    currChar := ReadChar();
  end;

  if currPositionInLine = EOF_POSITION then
  begin
    Dispose(tkn);
    exit(nil);
  end;

  //Dispose(tkn);
  //exit(nil);
  //tokenChar:= lexingLine[currPositionInLine];
  case currChar of
    HASHTAG: begin
      tkn^.TokenType := HASHTAG;
      tkn^.TokenLiteral := currChar;
    end;
    HYPHEN: begin
      tkn^.TokenType := HYPHEN;
      tkn^.TokenLiteral := currChar;
    end;
    COLON: begin
      tkn^.TokenType := COLON;
      tkn^.TokenLiteral := currChar;
    end;
    OPEN_SQ_BRACKET: begin
      tkn^.TokenType := OPEN_SQ_BRACKET;
      tkn^.TokenLiteral := currChar;
    end;
    CLOSE_SQ_BRACKET: begin
      tkn^.TokenType := CLOSE_SQ_BRACKET;
      tkn^.TokenLiteral := currChar;
    end;
    char(0): begin
      Dispose(tkn);
      exit(nil);
    end;

    else begin
      //
      //TEST
      //Dispose(tkn);
      //exit(nil);
      if TCharacter.IsLetterOrDigit(currChar) then
      begin

        if TCharacter.IsDigit(currChar) then
        begin

          tkn^.TokenType := NUMBER;
          tkn^.TokenLiteral := ReadNumber(currChar);
        end
        else
        begin
          tkn^.TokenType := IDENTIFIER;
          tkn^.TokenLiteral := ReadIdentifier(currChar);
        end;
      end;
      //TEST - END
    end;
  end;//case currChar of

  if tkn^.TokenLiteral = '' then
  begin
    Dispose(tkn);
    exit(nil);
  end;

  exit(tkn);
end; //function TLexer.NextToken(): PToken;

procedure TLexer.StartLine(textLine: string);
begin
  lexingLine := textLine;
  currPositionInLine := 0;
end;

end.
