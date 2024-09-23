unit Parser;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LexerConstants, LazLogger, JwaWinUser, lexer, Dialogs;

type

  PTVirtualCodeLang = ^TVirtualCodeLang;

  TVirtualCodeLang = record
    CodeString: string;
    CodeNumber: integer;
    //LanguageRec: PTlangRec;
  end;

  TParserFunc = ^ParserFunc;
  ParserFunc = function(): TParserFunc of object;
  //FuncPointer = function:ParserFunc of object;

  TParser = class
    function ParseKey(): TParserFunc;
    function ParseModifier(): TParserFunc;
    function ParseLangCode(): TParserFunc;
    function ParseLineOfTokens(tkn_array: LineOfTokens): PTShortcutLangRec;

  private
    shLangRec: PTShortcutLangRec;
    currToken: PToken;

  end;//  TParser = class

  TParserException = class(Exception)
  public
    Description: string;
    Token: PToken;
    constructor Create(descr: string = ''; tkn: PToken = nil);
  end;

function ParseLanguageConf(pathToApplicationFile: string): PTShortcutLangRecArr;
procedure LoadVirtualCodesFromFile(pathToApplicationFile: string);
function FindVirtualCodeString(s: string): PTVirtualCodeLang;
procedure DisposeVirtualCodeArray();


implementation

//Virtual codes: https://learn.microsoft.com/en-us/windows/win32/inputdev/virtual-key-codes

var
  PARSEMODIFIER_FUNC: ParserFunc;
  PARSELANGCODE_FUNC: ParserFunc;
  virtCodeArr: array of PTVirtualCodeLang;

constructor TParserException.Create(descr: string = ''; tkn: PToken = nil);
begin
  Description := descr;
  Token := tkn;
end;

procedure DisposeVirtualCodeArray();
var
  currRec: PTVirtualCodeLang;
begin
  for currRec in virtCodeArr do
  begin
    dispose(currRec);
  end;
  setLength(virtCodeArr, 0);
  Finalize(virtCodeArr);
end;

function FindVirtualCodeString(s: string): PTVirtualCodeLang;
var
  resVCode, currVCode: PTVirtualCodeLang;
begin
  resVCode := nil;
  for currVCode in virtCodeArr do
  begin
    if currVCode^.CodeString = s then
    begin
      new(resVCode);
      resVCode^.CodeString := currVCode^.CodeString;
      resVCode^.CodeNumber := currVCode^.CodeNumber;
      break;
    end;
  end;
  exit(resVCode);
end;


procedure LoadVirtualCodesFromFile(pathToApplicationFile: string);
var
  tfIn: TextFile;
  s: string;
  virtCode: PTVirtualCodeLang;
  splitString: TStringArray;
begin
  virtCodeArr := [];
  AssignFile(tfIn, pathToApplicationFile +
    'lang_list\Virtual key codes_transformed_19Aug2024.csv');
  try
    reset(tfIn);
    while not EOF(tfIn) do
    begin
      readln(tfIn, s);
      new(virtCode);
      splitString := s.Split(';');
      virtCode^.CodeString := splitString[0];
      Val(splitString[1], virtCode^.CodeNumber);
      insert(virtCode, virtCodeArr, Length(virtCodeArr));
    end;
    CloseFile(tfIn);
  except
    on E: EInOutError do
      writeln('File handling error occurred. Details: ', E.Message);
  end;
end;//procedure LoadVirtualCodesFromFile(pathToApplicationFile: string);

function ParseLine(line: string): PTShortcutLangRec;
var
  shLangRec: PTShortcutLangRec;
  lx: TLexer;
  currToken: PToken;
  tkn_arr: LineOfTokens = ();
  prs: TParser;
  i: integer;
begin
  lx := TLexer.Create();
  lx.StartLine(line);
  shLangRec := nil;
  currToken := nil;
  repeat
    begin
      currToken := lx.NextToken();
      //currToken := nil;
      if currToken = nil then
        break;
      if currToken^.TokenType = HASHTAG then
      begin
        Dispose(currToken);
        Break;
      end;
      insert(currToken, tkn_arr, Length(tkn_arr));
    end;
  until (currToken = nil) or (currToken^.TokenType = EOF_TYPE);//EOF is not needed
  DebugLn('Length(tkn_arr):', IntToStr(Length(tkn_arr)));

  if length(tkn_arr) > 0 then
  begin
    prs := TParser.Create();
    shLangRec := prs.ParseLineOfTokens(tkn_arr);
    //shLangRec := nil;
    i := 0;
    for currToken in tkn_arr do
    begin
      dispose(currToken);
      i := i + 1;
    end;
    setLength(tkn_arr, 0);
    Finalize(tkn_arr);
    DebugLn('Number of tokens disposed:', IntToStr(i));
    prs.Free();
  end;
  lx.Free();
  exit(shLangRec);
end; //function ParseLine(line: string): PTShortcutLangRec;

function ParseLanguageConf(pathToApplicationFile: string): PTShortcutLangRecArr;
var
  retArray: PTShortcutLangRecArr;
  currRec: PTShortcutLangRec;
  tfIn: TextFile;
  s: string;
begin
  //DebugLn('opening file:'+pathToApplicationFile+'languages.conf');
  new(retArray);
  AssignFile(tfIn, pathToApplicationFile + 'languages.conf');
  try
    reset(tfIn);
    while not EOF(tfIn) do
    begin
      readln(tfIn, s);
      DebugLn(s);
      currRec := ParseLine(s);
      //currRec := nil;
      if currRec <> nil then begin
        insert(currRec, retArray^, Length(retArray^));
        DebugLn('Inserting rec');
      end;
    end;
    CloseFile(tfIn);
  except
    on E: EInOutError do
    begin
      DebugLn('File handling error occurred. Details: ', E.Message);
      ShowMessage('Error on line: ' + s + sLineBreak + E.Message);
    end;

    on E: TParserException do
    begin
      DebugLn('File handling error occurred. Details: ', E.Message);
      ShowMessage('Error on line: ' + s + sLineBreak + E.Description +
        sLineBreak + 'Token:' + E.Token^.TokenLiteral);
    end;
  end;
  DebugLn('Number of recs in array:', IntToStr(Length(retArray^)));
  exit(retArray);
end;

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

function TParser.ParseLangCode(): TParserFunc;
var
  Code: integer;
begin
  ParseLangCode := nil;
  case currToken^.TokenType of
    COLON: begin
      exit(@PARSELANGCODE_FUNC);
    end;
    NUMBER: begin
      Val('$' + currToken^.TokenLiteral, shLangRec^.langCode, Code);
      //TODO: Add checking for error Code
    end;
  end;
end;

function TParser.ParseKey(): TParserFunc;
var
  vCode: PTVirtualCodeLang;
begin
  vCode := nil;
  case currToken^.TokenType of
    IDENTIFIER: begin
      vCode := FindVirtualCodeString(currToken^.TokenLiteral);
      if vCode = nil then
      begin
        //dispose(vCode);
        raise TParserException.Create(
          'Unknown virtual key name supplied:' + currToken^.TokenLiteral, currToken);
        exit(nil);//Should throw error as this is neither a modifier nor a key
      end
      else
      begin
        shLangRec^.Key := vCode^.CodeNumber;
        dispose(vCode);
        exit(@PARSELANGCODE_FUNC);
      end;
    end;
    else begin
      exit(nil);//Should throw error.
    end;
  end;//case currToken^.TokenType of
end;//function TParser.ParseKey(): TParserFunc;

function TParser.ParseModifier(): TParserFunc;
var
  currMod: integer;
begin
  case currToken^.TokenType of
    IDENTIFIER: begin
      currMod := IsModifier(currToken^.TokenLiteral);
      if currMod = -1 then
      begin
        exit(ParseKey());
      end
      else
      begin
        insert(currMod, shLangRec^.KbModifierArr, Length(shLangRec^.KbModifierArr));
        exit(@PARSEMODIFIER_FUNC);
      end;
    end;//IDENTIFIER: begin
    HYPHEN: begin
      exit(@PARSEMODIFIER_FUNC);
    end;
    else begin
      DebugLn('ParseModifier: unknown token');
      exit(nil);//TODO: Should throw error
    end;
  end;//case currToken^.TokenType of
end;//function TParser.ParseModifier(): TParserFunc;

function TParser.ParseLineOfTokens(tkn_array: LineOfTokens): PTShortcutLangRec;
var
  currParserFunc: ParserFunc;
  point: TParserFunc;
begin
  PARSEMODIFIER_FUNC := ParserFunc(@Self.ParseModifier);
  PARSELANGCODE_FUNC := ParserFunc(@Self.ParseLangCode);
  currParserFunc := ParserFunc(@Self.ParseModifier);
  new(shLangRec);
  shLangRec^.KbModifierArr := [];
  shLangRec^.Key := 0;
  shLangRec^.HotKeyID := -1;
  shLangRec^.langName := '';
  shLangRec^.langIconName := '';
  shLangRec^.langCode := -1;
  shLangRec^.LanguageRec := nil;
  for currToken in tkn_array do
  begin
    point := currParserFunc();
    if point = nil then break;
    currParserFunc := ParserFunc(point^);
  end;//for currTkn in tkn_array do begin
  if Length(tkn_array) = 0 then begin
    Dispose(shLangRec);
    exit(nil);
  end else
    exit(shLangRec);
end;//function TParser.ParseLineOfTokens(tkn_array: LineOfTokens): PTShortcutLangRec;

end.
