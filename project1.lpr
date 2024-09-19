program project1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, MainUnit, languages, Lexer, RegistryRegistration, ConfigReader
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Title:='LanguageShortcut';
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TMainAppForm, Form1);
  Application.Run;
end.

