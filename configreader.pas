unit ConfigReader;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LexerConstants, Parser, languages, LazLogger;

  procedure ReadConfigFile();

  var
    ConfigPTShortcutLangRecArr : PTShortcutLangRecArr;



implementation

procedure ReadConfigFile();
var
  currShortcutRec: PTShortcutLangRec;
  currLangRec : PTLangRec;
begin
  //ConfigPTShortcutLangRecArr := array of PTShortcutLangRec
  ConfigPTShortcutLangRecArr := ParseLanguageConf();
  for currShortcutRec in ConfigPTShortcutLangRecArr do begin
    DebugLn('Have lang code:'+IntToStr(currShortcutRec^.langCode));
    currLangRec := findLanguageByCode(currShortcutRec^.langCode);
    PrintLangRecord(currLangRec);
  end;


end;

end.

