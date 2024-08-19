unit Parser;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LexerConstants, LazLogger, JwaWinUser, lexer;

type

  PTVirtualCode = ^TVirtualCode;

  TVirtualCode = record
    CodeString: string;
    CodeNumber: integer;
  end;

  TParserFunc = ^ParserFunc;
  ParserFunc = function(): TParserFunc of object;
  //FuncPointer = function:ParserFunc of object;

  TParser = class
    function ParseKey(): TParserFunc;
    function ParseModifier(): TParserFunc;
    function ParseLineOfTokens(tkn_array: LineOfTokens): PTShortcutLangRec;

  private
    shLangRec: PTShortcutLangRec;
    currToken: PToken;

  end;//  TParser = class

procedure ParseLanguageConf();
procedure LoadVirtualCodesFromFile();
function FindVirtualCodeString(s: string):PTVirtualCode;


//ParserFunc = function:pointer;

//function ParseLineOfTokens(tkn_array: LineOfTokens): PTShortcutLangRec;


implementation

//Virtual codes: https://learn.microsoft.com/en-us/windows/win32/inputdev/virtual-key-codes

var
  PARSEMODIFIER_FUNC: ParserFunc;
  PARSEKEY_FUNC: ParserFunc;
  virtCodeArr: array of ^TVirtualCode;


function FindVirtualCodeString(s: string): PTVirtualCode;
var
  resVCode, currVCode: PTVirtualCode;

begin
  resVCode := nil;
  for currVCode in virtCodeArr do begin
    if currVCode^.CodeString = s then begin
      new(resVCode);
      resVCode^.CodeString:=currVCode^.CodeString;
      resVCode^.CodeNumber:=currVCode^.CodeNumber;
      break;
    end;
  end;
  exit(resVCode);
end;


procedure LoadVirtualCodesFromFile();
var
  tfIn: TextFile;
  s: string;
  virtCode : ^TVirtualCode;
  splitString : TStringArray;
begin
  virtCodeArr := [];
  AssignFile(tfIn, 'Virtual key codes_transformed_19Aug2024.csv');

  // Embed the file handling in a try/except block to handle errors gracefully
  try
    // Open the file for reading
    reset(tfIn);

    // Keep reading lines until the end of the file is reached
    while not EOF(tfIn) do
    begin
      readln(tfIn, s);
      DebugLn(s);
      new(virtCode);
      splitString := s.Split(';');
      virtCode^.CodeString := splitString[0];
      Val(splitString[1], virtCode^.CodeNumber);
      insert(virtCode, virtCodeArr, Length(virtCodeArr));
      //ParseLine(s);
    end;

    // Done so close the file
    CloseFile(tfIn);

  except
    on E: EInOutError do
      writeln('File handling error occurred. Details: ', E.Message);
  end;
end;

function ParseLine(line: string): PTShortcutLangRec;
var
  shLangRec: PTShortcutLangRec;
  lx: TLexer;
  currToken: PToken;
  tkn_arr: LineOfTokens;
  prs: TParser;
begin
  lx := TLexer.Create();
  lx.StartLine(line);
  DebugLn(sLineBreak + sLineBreak + 'Starting new line');
  tkn_arr := [];
  repeat
    begin
      currToken := lx.NextToken();
      if currToken = nil then
        break;
      PrintToken(currToken);
      if currToken^.TokenType = HASHTAG then
      begin
        DebugLn('Found hashtag - skipping to the end of line');
        Break;
      end;
      insert(currToken, tkn_arr, Length(tkn_arr));
    end;
  until (currToken = nil) or (currToken^.TokenType = EOF_TYPE);//EOF is not needed

  PrintTokenArray(tkn_arr);

  prs := TParser.Create();
  shLangRec := prs.ParseLineOfTokens(tkn_arr);

  if shLangRec^.langName = '' then exit(nil)
  else
    exit(shLangRec);
end;

procedure ParseLanguageConf();
var
  tfIn: TextFile;
  s: string;
begin
  // Set the name of the file that will be read
  AssignFile(tfIn, 'languages.conf');

  // Embed the file handling in a try/except block to handle errors gracefully
  try
    // Open the file for reading
    reset(tfIn);

    // Keep reading lines until the end of the file is reached
    while not EOF(tfIn) do
    begin
      readln(tfIn, s);
      DebugLn(s);
      ParseLine(s);
    end;

    // Done so close the file
    CloseFile(tfIn);

  except
    on E: EInOutError do
      writeln('File handling error occurred. Details: ', E.Message);
  end;

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

function TParser.ParseKey(): TParserFunc;
begin
  exit(nil);
end;

function TParser.ParseModifier(): TParserFunc;
var
  currMod: integer;
begin
  case currToken^.TokenType of
    IDENTIFIER: begin
      DebugLn('ParseModifier: have modifier:' + currToken^.TokenLiteral);
      currMod := IsModifier(currToken^.TokenLiteral);
      if currMod = -1 then
      begin
        exit(ParseKey());
      end
      else
      begin
        exit(@PARSEMODIFIER_FUNC);
      end;
    end;//IDENTIFIER: begin
    HYPHEN: begin
      DebugLn('ParseModifier: have hyphen');
      exit(@PARSEMODIFIER_FUNC);
    end;

    COLON: begin
      DebugLn('ParseModifier: have colon');
      exit(@PARSEKEY_FUNC);
    end;
    else
    begin
      DebugLn('ParseModifier: unknown token');
      exit(nil);
    end;

  end;
end;

function TParser.ParseLineOfTokens(tkn_array: LineOfTokens): PTShortcutLangRec;
var
  i: integer;
  //currTkn: PToken;
  phase: string;
  currParserFunc, tempParserFunc: ParserFunc;
  point: TParserFunc;
  //procPointer : FuncPointer;
begin
  PARSEMODIFIER_FUNC := ParserFunc(@Self.ParseModifier);
  PARSEKEY_FUNC := ParserFunc(@Self.ParseKey);
  currParserFunc := ParserFunc(@Self.ParseModifier);
  new(shLangRec);
  shLangRec^.KbModifierArr := [];
  shLangRec^.Key := -1;
  shLangRec^.langName := '';
  shLangRec^.langIconName := '';

  DebugLn(sLineBreak + sLineBreak + 'Parsing line of tokens');
  DebugLn('***********************************');
  for currToken in tkn_array do
  begin
    PrintToken(currToken);
    if currToken^.TokenLiteral = EOF_TYPE then break;
    point := currParserFunc();
    if point = nil then break;
    currParserFunc := ParserFunc(point^);

  end;//for currTkn in tkn_array do begin
end;

end.
