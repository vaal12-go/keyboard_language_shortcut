unit LexerConstants;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Windows, LazLogger, languages;

type

  PToken = ^Token;

  Token = record
    TokenType: string;
    TokenLiteral: string;
  end;

  LineOfTokens = array of PToken;

  PTShortcutLangRec = ^TShortcutLangRec;

  TShortcutLangRec = record
    KbModifierArr: array of uint;
    Key: uint;
    HotKeyID : longint;
    langName: string;
    langCode: integer;
    langIconName: string;
    LanguageRec : PTlangRec;
  end;

  PTShortcutLangRecArr = array of PTShortcutLangRec;

procedure PrintToken(tkn: PToken);
procedure PrintTokenArray(tknArr: LineOfTokens);
procedure PrintShortcutLangRec(langRec: PTShortcutLangRec);

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

procedure PrintShortcutLangRec(langRec: PTShortcutLangRec);
var
  currMod : integer;
  modifStr : string;
begin
  if langRec = nil then
    DebugLn('Have NIL ShortcutLangRec')
  else
  begin
    DebugLn();
    DebugLn('*****  Have ShortcutLangRec: *****');
    modifStr:='';
    for currMod in langRec^.KbModifierArr do begin
      modifStr := modifStr + ' | '+IntToStr(currMod);
    end;
    DebugLn('   Modifiers:'+modifStr);
    DebugLn('   Key:'+IntToStr(langRec^.Key)+'  | LangCode:'+IntToStr(langRec^.langCode));
    DebugLn('   Language name:'+langRec^.langName);
    DebugLn('   Language string code:'+langRec^.LanguageRec^.LanguageCodeStr);
  end;
end;

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

