unit Parser;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LexerConstants, LazLogger, JwaWinUser;

type
  TParserFunc = ^ParserFunc;
  ParserFunc = function(tkn: PToken; shortCutRec: PTShortcutLangRec): TParserFunc;
  //ParserFunc = function:pointer;

  function ParseLineOfTokens(tkn_array: LineOfTokens): PTShortcutLangRec;


implementation

var
  PARSEMODIFIER_FUNC: ParserFunc;
  PARSEKEY_FUNC: ParserFunc;

  function IsModifier(ident: string): integer;
  begin
    ident := LowerCase(ident);
    case ident of
      'alt': exit(MOD_ALT);
      'ctrl': exit(MOD_CONTROL);
      'shift': exit(MOD_SHIFT);
      'win': exit(MOD_WIN);
      else
        exit(-1);
    end;
  end;//function IsModifier(ident : string): integer;

function ParseKey(tkn: PToken; shortCutRec: PTShortcutLangRec): ParserFunc;
begin
  exit(nil);
end;

function ParseModifier(tkn: PToken; shortCutRec: PTShortcutLangRec): ParserFunc;
var
  currMod: integer;
begin
  case tkn^.TokenType of
    IDENTIFIER: begin
      DebugLn('ParseModifier: have modifier:' + tkn^.TokenLiteral);
      currMod := IsModifier(tkn^.TokenLiteral);
      if currMod = -1 then
      begin
        exit(ParseKey(tkn, shortCutRec));
      end
      else
      begin
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
    else
    begin
      DebugLn('ParseModifier: unknown token');
      exit(nil);
    end;

  end;
end;

function ParseLineOfTokens(tkn_array: LineOfTokens): PTShortcutLangRec;
var
  i: integer;
  currTkn: PToken;
  phase: string;
  currParserFunc, tempParserFunc: ParserFunc;
  shLangRec: PTShortcutLangRec;
  point: pointer;
begin
  PARSEMODIFIER_FUNC := ParserFunc(@ParseModifier);
  PARSEKEY_FUNC := ParserFunc(@ParseKey);
  currParserFunc := ParserFunc(@ParseModifier);
  new(shLangRec);
  shLangRec^.KbModifierArr := [];
  shLangRec^.Key := -1;
  shLangRec^.langName := '';
  shLangRec^.langIconName := '';

  DebugLn(sLineBreak + sLineBreak + 'Parsing line of tokens');
  DebugLn('***********************************');
  for currTkn in tkn_array do
  begin
    PrintToken(currTkn);
    point := currParserFunc(currTkn, shLangRec);
    currParserFunc := ParserFunc(point);
    if currParserFunc = nil then break;
  end;//for currTkn in tkn_array do begin
end;

end.
