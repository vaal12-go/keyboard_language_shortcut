unit LexerConstants;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LazLogger;

type

  PToken = ^Token;

  Token = record
    TokenType: string;
    TokenLiteral: string;
  end;

  LineOfTokens = array of PToken;

  PTShortcutLangRec = ^TShortcutLangRec;

  TShortcutLangRec = record
    KbModifierArr: array of integer;
    Key: integer;
    langName: string;
    langIconName: string;
  end;

procedure PrintToken(tkn: PToken);

const
  HASHTAG = '#';
  HYPHEN = '-';
  COLON = ':';
  OPEN_SQ_BRACKET = '[';
  CLOSE_SQ_BRACKET = ']';

  IDENTIFIER = 'IDENTIFIER';
  NUMBER = 'NUMBER';
  EOF_POSITION = -100;

  EOF_TYPE = 'EOF';
  EOFCH = char(0);


implementation

procedure PrintToken(tkn: PToken);
begin
  if tkn = nil then
    DebugLn('Have NIL token')
  else
  begin
    DebugLn('Have token:' + tkn^.TokenType+' Literal:'+tkn^.TokenLiteral);
  end;
end;

procedure PrintTokenArray(tknArr: LineOfTokens);
var
  currToken : PToken;
begin
  DebugLn(sLineBreak+'Token Array:');
  for currToken in tknArr do begin
    PrintToken(currToken);
  end;
end;

end.

