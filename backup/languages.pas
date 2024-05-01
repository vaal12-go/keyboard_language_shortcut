unit languages;

{$mode ObjFPC}{$H+}



interface
uses
  Classes, SysUtils, Dialogs;

type

PTlangRec = ^TLangRec;

TLangRec = record
    LanguageName : string;
    LanguageCode : integer;
end;

var
  langList : TList;

procedure loadLanguageRecords();
function findLanguageByCode(code: integer): PTlangRec;




implementation

function findLanguageByCode(code: integer): PTlangRec;
var
  currRec : PTlangRec;
  i : integer;
begin
  i:=0;
  while i<langList.Count do begin
  //for currRec in langList.Items do begin
    currRec := langList.Items[i];
    if (currRec^.LanguageCode = code) then
       Exit(langList.Items[i]);


    i := i+1;

    end;


  end;



procedure loadLanguageRecords();
var
  tfIn: TextFile;
  lName, lCode, s: string;
  splitStr : array of string;
  rec : PTlangRec;
  i, Code : integer;

begin

  langList := TList.Create();

  AssignFile(tfIn, 'lang_list\Windows_lang_list_01May2024 .txt');

  try
    // Open the file for reading
    reset(tfIn);

    // Keep reading lines until the end of the file is reached
    while not eof(tfIn) do
    begin
      readln(tfIn, s);
      splitStr := s.Split(';');

      new(rec);
      rec^.LanguageName:= splitStr[0];

      Val ('x'+splitStr[1],i,Code);
      If Code<>0 then
        ShowMessage('Error converting string:'+splitStr[1])
      else
        rec^.LanguageCode:= i;
      //rec^.LanguageCode:= splitStr[1];

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
      i := i+1;

  end;

end.

