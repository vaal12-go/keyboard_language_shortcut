unit ConfigReader;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LexerConstants, Parser, languages, LazLogger;

  procedure ReadConfigFile(pathToApplicationFile: string);

var
  ConfigPTShortcutLangRecArr : PTShortcutLangRecArr;



implementation

procedure ReadConfigFile(pathToApplicationFile: string);
var
  currShortcutRec: PTShortcutLangRec;
  currLangRec : PTLangRec;
  modifier : integer;
begin
  //ConfigPTShortcutLangRecArr := array of PTShortcutLangRec
  ConfigPTShortcutLangRecArr := ParseLanguageConf(pathToApplicationFile);
  for currShortcutRec in ConfigPTShortcutLangRecArr do begin
    DebugLn(sLineBreak+sLineBreak+'*********************');
    DebugLn('Have lang code:'+IntToStr(currShortcutRec^.langCode));
    currLangRec := findLanguageByCode(currShortcutRec^.langCode);
    PrintLangRecord(currLangRec);
    for modifier in currShortcutRec^.KbModifierArr do begin
      DebugLn('   Keyboard modifier code:'+IntToStr(modifier));
    end;
    DebugLn('   Keyboard code:'+IntToStr(currShortcutRec^.Key));

  end;//  for currShortcutRec in ConfigPTShortcutLangRecArr do begin
end;//procedure ReadConfigFile(pathToApplicationFile: string);

end.

