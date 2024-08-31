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
begin
  //ConfigPTShortcutLangRecArr := array of PTShortcutLangRec
  ConfigPTShortcutLangRecArr := ParseLanguageConf(pathToApplicationFile);
  for currShortcutRec in ConfigPTShortcutLangRecArr do begin
    DebugLn('Have lang code:'+IntToStr(currShortcutRec^.langCode));
    currLangRec := findLanguageByCode(currShortcutRec^.langCode);
    PrintLangRecord(currLangRec);
  end;


end;

end.

