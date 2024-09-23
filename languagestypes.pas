unit LanguagesTypes;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, Graphics;

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

implementation

end.

