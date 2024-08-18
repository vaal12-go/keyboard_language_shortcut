unit LexerConstants;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

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
    DebugLn('Have token:' + tkn^.TokenType);
    DebugLn(#9 + tkn^.TokenLiteral);
  end;
end;

end.

