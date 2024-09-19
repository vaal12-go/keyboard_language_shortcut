unit ConfigReader;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LexerConstants, Parser, languages, LazLogger;

procedure ReadConfigFile(pathToApplicationFile: string);
procedure DisposeConfigPTShortcutLangRecArr();

var
  ConfigPTShortcutLangRecArr: PTShortcutLangRecArr;



implementation

procedure DisposeConfigPTShortcutLangRecArr();
var
  currRec: PTShortcutLangRec;
begin
    for currRec in ConfigPTShortcutLangRecArr do begin
      dispose(currRec);
    end;
    setLength(ConfigPTShortcutLangRecArr, 0);
end;

procedure ReadConfigFile(pathToApplicationFile: string);
var
  currShortcutRec: PTShortcutLangRec;
  //currLangRec : PTLangRec;
  modifier: integer;
begin
  //TODO: check why    Language name: is empty
  //ConfigPTShortcutLangRecArr := array of PTShortcutLangRec
  ConfigPTShortcutLangRecArr := ParseLanguageConf(pathToApplicationFile);
  for currShortcutRec in ConfigPTShortcutLangRecArr do
  begin
    DebugLn(sLineBreak + sLineBreak + '*********************');
    DebugLn('Have lang code:' + IntToStr(currShortcutRec^.langCode));
    currShortcutRec^.LanguageRec := findLanguageByCode(currShortcutRec^.langCode);
    PrintLangRecord(currShortcutRec^.LanguageRec);
    for modifier in currShortcutRec^.KbModifierArr do
    begin
      DebugLn('   Keyboard modifier code:' + IntToStr(modifier));
    end;
    DebugLn('   Keyboard code:' + IntToStr(currShortcutRec^.Key));

  end;//  for currShortcutRec in ConfigPTShortcutLangRecArr do begin
end;//procedure ReadConfigFile(pathToApplicationFile: string);

end.
