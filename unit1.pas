unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Menus, Windows, LazLogger, JwaWinUser, registry, languages;

type

  TWMHotKey = packed record
    Msg: cardinal;
    HotKey: longint;
    Unused: longint;
    Result: longint;
  end;

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Label1: TLabel;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    AddToStartMenuItem: TMenuItem;
    qwe1: TMenuItem;
    Separator1: TMenuItem;
    ExitContextMenuItem: TMenuItem;
    LanguageNameTimer: TTimer;
    TrayPopupMenu: TPopupMenu;
    TrayIcon: TTrayIcon;
    procedure AddToStartMenuItemClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure ExitContextMenuItemClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LanguageNameTimerTimer(Sender: TObject);
  private
    enIcon, ruIcon, ukrIcon : TIcon;

  public
    //ENIcon, RUIcon: Graphics.TPortableNetworkGraphic;
    procedure OnMenuHotKey(var Mes: TWMHotKey); message wm_hotkey;



  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormActivate(Sender: TObject);
var
  ico: TIcon;
   appPath : string;
begin

  LazLogger.DebugLogger.CloseLogFileBetweenWrites := True;
  DebugLn('Hello 345');
  appPath := ExtractFilePath(Application.ExeName);
    DebugLn('have handle');
    DebugLn(appPath);
    LazLogger.DebugLogger.DbgOut('Hello1');
  //ShowMessage('Form activated');
  //windows.RegisterHotKey(self.Handle, 1, MOD_CONTROL, VK_OEM_3); //'`
  Windows.RegisterHotKey(self.Handle, 1, MOD_CONTROL, VK_OEM_4);  //{

  //  VK_OEM_3  0xC0  OEM_3 (~ `)
  //http://kbdedit.com/manual/low_level_vk_list.html
  //windows.RegisterHotKey(self.Handle, 2, MOD_CONTROL, VK_1);
  //windows.RegisterHotKey(self.Handle, 3, MOD_CONTROL, VK_2);

  Windows.RegisterHotKey(self.Handle, 2, MOD_CONTROL, VK_OEM_6); //}
  Windows.RegisterHotKey(self.Handle, 3, MOD_CONTROL, VK_OEM_5); //\
  //self.ENIcon := Graphics.TPortableNetworkGraphic.Create();
  //self.ENIcon.LoadFromFile(
  //  appPath + 'icons\EN_32x32.png');
  //self.RUIcon := Graphics.TPortableNetworkGraphic.Create();
  //self.RUIcon.LoadFromFile(
  //  appPath+'icons\RU_32x32.png');


  self.enIcon := TIcon.Create();
  self.enIcon.LoadFromFile(
      appPath+'icons\EN_64x64_05Apr2024.ico');

  self.ruIcon := TIcon.Create();
  self.ruIcon.LoadFromFile(
      appPath+'icons\RU_32x32.ico');

  self.ukrIcon := TIcon.Create();
  self.ukrIcon.LoadFromFile(
      appPath+'icons\UKR_64x64_05Apr2024.ico');




  //ico := TIcon.Create()
  //ico.

end;

procedure TForm1.ExitContextMenuItemClick(Sender: TObject);
begin
  //ShowMessage('Will exit');
  self.Close();
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  self.Hide()
end;

procedure TForm1.LanguageNameTimerTimer(Sender: TObject);
var
   langKL : HKL;
   langID : integer;
   langName, langNameFull : string;
   forWindowHandle, parentHandle: HWND;
   procID, threadID : DWORD;

begin

  //ShowMessage('hello 5675');
  forWindowHandle := Windows.GetForegroundWindow();
  threadID := Windows.GetWindowThreadProcessId(forWindowHandle, procID);



  langKL := windows.GetKeyboardLayout(threadID);
  languages.loadLanguageRecords();

  langID := (langKL and $ffff0000) shr 16;

  langName := (languages.findLanguageByCode(langID))^.LanguageName;
  langNameFull := (languages.findLanguageByCode(langKL))^.LanguageName;

  //ShowMessage('Found shor language name:'+langName+' long name:'+langNameFull);


    Label1.Caption:= langName;
end;

procedure TForm1.Button1Click(Sender: TObject);


begin



end;

procedure TForm1.AddToStartMenuItemClick(Sender: TObject);
var
  Registry: TRegistry;
begin
  Registry := TRegistry.Create;
  try
    // Navigate to proper "directory":
    Registry.RootKey := HKEY_CURRENT_USER;
    //if Registry.OpenKeyReadOnly('\Software\Microsoft\Windows\CurrentVersion\Run') then
    if Registry.OpenKey('\Software\Microsoft\Windows\CurrentVersion\Run\',
      False) then
      //CompileCommand:=Registry.ReadString(''); //read the value of the default name
      Registry.WriteString('KeyboardLangChange', '"'+Application.ExeName+'"');
  finally
    Registry.Free;  // In non-Windows operating systems this flushes the reg.xml file to disk
  end;
end;

procedure ActivateLanguage(const lng_const: string);
var
  hk: HKL;
  forWindowHandle, parentHandle: HWND;
  lang_str: PChar;
begin
  //ShowMessage('h1');
  lang_str := PChar(lng_const);
  hk := Windows.LoadKeyboardLayoutA(lang_str, JwaWinUser.KLF_ACTIVATE or
    JwaWinUser.KLF_SUBSTITUTE_OK or JwaWinUser.KLF_SETFORPROCESS);
  //                 or JwaWinUser.KLF_NOTELLSHELL
  Windows.ActivateKeyboardLayout(hk, 0);

  forWindowHandle := Windows.GetForegroundWindow();
  Windows.PostMessage(forWindowHandle, Windows.WM_INPUTLANGCHANGEREQUEST, 0, hk);
  parentHandle := Windows.GetParent(forWindowHandle);
  Windows.PostMessage(parentHandle, Windows.WM_INPUTLANGCHANGEREQUEST, 0, hk);

end;//procedure ActivateLanguage(var lng_const : string);

procedure TForm1.OnMenuHotKey(var Mes: TWMHotKey);
var
  hk: HKL;
  forWindowHandle, parentHandle: HWND;
  //newIcon: TIcon;
  appPath : string;

begin
  appPath := ExtractFilePath(Application.ExeName);


  //DebugLn(String(Mes.HotKey));
  // DebugLn(String(Mes.Msg));

  //newIcon := TIcon.Create();

  if (Mes.Unused = 1) then
  begin
    ActivateLanguage('00000409');
    self.Caption := 'EN';
    //newIcon.LoadFromFile(
    //  appPath+'icons\EN_64x64_05Apr2024.ico');
    self.TrayIcon.Icon := self.enIcon;
    self.TrayIcon.Icon.AssignImage(self.ENIcon);
  end;
  if (Mes.Unused = 2) then
  begin
    ActivateLanguage('00000419');
    //hk := Windows.LoadKeyboardLayoutW('00000419', 0);
    self.Caption := 'RUS';
    //self.TrayIcon.Hide();
    //self.TrayIcon.Icon.AssignImage(self.RUIcon);
    //newIcon.LoadFromFile(
    //  appPath+'icons\RU_32x32.ico');
    self.TrayIcon.Icon := self.ruIcon;
    self.TrayIcon.Show();
    //self.TrayIcon.ShowIcon:= True;
  end;
  if (Mes.Unused = 3) then
  begin
    ActivateLanguage('00000422');
    //hk := Windows.LoadKeyboardLayoutW('00000422', 0);
    self.Caption := 'UKR';
    //newIcon.LoadFromFile(
    //  appPath+'icons\UKR_64x64_05Apr2024.ico');
    self.TrayIcon.Icon := self.ukrIcon;
  end;


  //self.Hide();
  //self.Show();
  //  00000419 - RUS
  //  00000422 - UKR


  //self.Hide();
end;//procedure TForm1.OnMenuHotKey(var Mes: TWMHotKey);


end.
