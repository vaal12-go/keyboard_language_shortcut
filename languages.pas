unit languages;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, Graphics, LazLogger, FileUtil;

type

  PTLangRec = ^TLangRec;

  TLangRec = record
    LanguageName: string;
    LanguageCodeInt: integer;
    LanguageCodeShortInt: integer;
    LanguageCodeStr: string;
    LanguageIconFileName: string;
    LanguageIcon: TIcon;
  end;

var
  langList: TList;

procedure loadLanguageRecords(pathToApplicationFile: string);
function findLanguageByCode(code: integer): PTlangRec;
procedure PrintLangRecord(rec: PTLangRec);
procedure DisposeLanguageRecords();


implementation

procedure PrintLangRecord(rec: PTLangRec);
begin
  if rec = nil then
  begin
    DebugLn('LangRec. Is NIL');
    exit();
  end;
  DebugLn('LangRec. Name:' + rec^.LanguageName);
  DebugLn('   Code:' + rec^.LanguageCodeStr);

end;

function findLanguageByCode(code: integer): PTlangRec;
var
  currRec: PTlangRec;
  i: integer;
begin
  //DebugLn('findLanguageByCode. Code:'+IntToStr(code));
  i := 0;
  findLanguageByCode := nil;
  while i < langList.Count do
  begin
    currRec := langList.Items[i];
    if (currRec^.LanguageCodeInt = code) then
      Exit(langList.Items[i]);
    i := i + 1;
  end;

  for currRec in langList do
  begin
    if (currRec^.LanguageCodeShortInt = code) then
      Exit(langList.Items[i]);
  end;
end;//function findLanguageByCode(code: integer): PTlangRec;

function findIconByLanguage(langRec: PTlangRec; pathToApplicationFile: string): string;
var
  //icoFName, mask, icoFile, shortLangCode: string;
  mask, shortLangCode: string;
  icoFiles: TStringList;
begin
  mask := '*' + langRec^.LanguageCodeStr + '.ico';
  //DebugLn('Mask:'+mask);
  icoFiles := FindAllFiles(pathToApplicationFile + 'icons\', mask, False);
  //for icoFile in icoFiles do begin
  //  DebugLn('Found file:'+icoFile);
  //end;
  if icoFiles.Count > 0 then
  begin
    //DebugLn('Found good icon first try:'+icoFiles[0]);
    exit(icoFiles[0]);
  end;

  if Length(langRec^.LanguageCodeStr) >= 4 then
  begin
    shortLangCode := langRec^.LanguageCodeStr.Substring(
      langRec^.LanguageCodeStr.Length - 4);
    //DebugLn('ShortLangCode:'+shortLangCode);
    mask := '*' + shortLangCode + '.ico';
    icoFiles := FindAllFiles(pathToApplicationFile + 'icons\', mask, False);
    //for icoFile in icoFiles do begin
    //  DebugLn('Found file (shortCode):'+icoFile);
    //end;
  end;
  exit('');
  //icoFName := pathToApplicationFile + 'icons\' + rec^.LanguageCodeStr + ;
end;

procedure DisposeLanguageRecords();
var
  currRec: PTlangRec;
begin
  for currRec in langList do
  begin
    if currRec <> nil then
      if currRec^.LanguageIcon <> nil then
        currRec^.LanguageIcon.Destroy;
    dispose(currRec);
  end;
  langList.Clear;
end;

procedure loadLanguageRecords(pathToApplicationFile: string);
var
  tfIn: TextFile;
  //lName, lCode,
  s, shortLangCode: string;
  splitStr: array of string;
  rec: PTlangRec;
  i, Code: integer;
begin
  langList := TList.Create();
  AssignFile(tfIn, pathToApplicationFile + 'lang_list\Windows_lang_list_01May2024 .txt');
  try
    reset(tfIn);
    while not EOF(tfIn) do
    begin
      readln(tfIn, s);
      splitStr := s.Split(';');
      new(rec);
      rec^.LanguageName := splitStr[0];
      rec^.LanguageCodeStr := splitStr[1];
      //DebugLn('langName:' + rec^.LanguageName);
      //DebugLn('    code:' + rec^.LanguageCodeStr);
      Val('$' + rec^.LanguageCodeStr, rec^.LanguageCodeInt, Code);

      if Code <> 0 then
        ShowMessage('Error converting string:' + splitStr[1]);

      if rec^.LanguageCodeStr.Length > 4 then
      begin
        shortLangCode := rec^.LanguageCodeStr.Substring(rec^.LanguageCodeStr.Length - 4);
        Val('$' + shortLangCode, rec^.LanguageCodeShortInt, Code);
        if Code <> 0 then
          ShowMessage('Error converting string:' + splitStr[1]);
      end
      else
      begin
        rec^.LanguageCodeShortInt := rec^.LanguageCodeInt;
      end;

      rec^.LanguageIcon := TIcon.Create();

      rec^.LanguageIconFileName := findIconByLanguage(rec, pathToApplicationFile);

      if FileExists(rec^.LanguageIconFileName) then
      begin
        //DebugLn('    icoFname:' + icoFName);
        rec^.LanguageIcon := TIcon.Create();
        rec^.LanguageIcon.LoadFromFile(rec^.LanguageIconFileName);
      end;

      langList.Add(rec);
      //ShowMessage(s)
    end;
    // Done so close the file
    CloseFile(tfIn);

  except
    on E: EInOutError do
      ShowMessage('File handling error occurred. Details:' + E.Message);
  end;

  i := langList.Count;
  i := i + 1;
end;//procedure loadLanguageRecords(pathToApplicationFile: string);

end.
