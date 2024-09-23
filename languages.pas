unit languages;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, Graphics,
  LazLogger, FileUtil, LanguagesTypes;

var
  langList: TList;

procedure loadLanguageRecords(pathToApplicationFile: string);
function  findLanguageByCode(code: integer): PTlangRec;
procedure PrintLangRecord(rec: PTLangRec);
procedure DisposeLanguageRecords();
procedure InitLanguagesModule();
procedure FinishLanguagesModule();



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

procedure findIconByLanguage(langRec: PTlangRec; pathToApplicationFile: string);
var
  mask, shortLangCode: string;
  icoFiles: TStringList;
begin
  mask := '*' + langRec^.LanguageCodeStr + '.ico';
  icoFiles := FindAllFiles(pathToApplicationFile + 'icons\', mask, False);
  if icoFiles.Count > 0 then
  begin
    //findIconByLanguage := Copy(icoFiles[0], 0, Length(icoFiles[0]));
    langRec^.LanguageIconFileName:= Copy(icoFiles[0], 0, Length(icoFiles[0]));
    //icoFiles.Free();
    icoFiles.Free();
    exit();
  end;

  if Length(langRec^.LanguageCodeStr) >= 4 then
  begin
    shortLangCode := langRec^.LanguageCodeStr.Substring(
      langRec^.LanguageCodeStr.Length - 4);
    mask := '*' + shortLangCode + '.ico';
    icoFiles.Free();
    icoFiles := FindAllFiles(pathToApplicationFile + 'icons\', mask, False);
    if icoFiles.Count > 0 then
    begin
      langRec^.LanguageIconFileName:= Copy(icoFiles[0], 0, Length(icoFiles[0]));
      icoFiles.Free();
      exit();
    end;
  end;
  icoFiles.Free();
end; //function findIconByLanguage(langRec: PTlangRec; pathToApplicationFile: string): string;

procedure DisposeLanguageRecords();
var
  currRec: PTlangRec;
begin
  for currRec in langList do
  begin
    if currRec <> nil then
      if currRec^.LanguageIcon <> nil then
      begin
        //currRec^.LanguageIcon.Destroy;
        currRec^.LanguageIcon.Free;
      end;
    dispose(currRec);
  end;
  langList.Clear;
end;

procedure InitLanguagesModule();
begin
  langList := TList.Create();
end;

procedure FinishLanguagesModule();
begin
  langList.Free();
end;

procedure loadLanguageRecords(pathToApplicationFile: string);
var
  tfIn: TextFile;
  s, shortLangCode: string;
  splitStr: array of string;
  rec: PTlangRec;
  i, Code: integer;
begin
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

      //TEST
      //rec^.LanguageIcon := nil;
      rec^.LanguageIcon := TIcon.Create();
      findIconByLanguage(rec, pathToApplicationFile);
      //rec^.LanguageIconFileName := findIconByLanguage(rec, pathToApplicationFile);
      //rec^.LanguageIconFileName := 'qwe1.ico';

      if FileExists(rec^.LanguageIconFileName) then
      begin
        rec^.LanguageIcon.LoadFromFile(rec^.LanguageIconFileName);
      end;
      //TEST - END
      langList.Add(rec);
    end;
    CloseFile(tfIn);
  except
    on E: EInOutError do
      ShowMessage('File handling error occurred. Details:' + E.Message);
  end;
  //i := langList.Count;
  //i := i + 1;
end;//procedure loadLanguageRecords(pathToApplicationFile: string);

end.
