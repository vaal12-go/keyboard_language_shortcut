unit MainUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Menus, Windows, LazLogger, JwaWinUser, registry, languages,
  RegistryRegistration, Parser, ShellApi;

type

  TWMHotKey = packed record
    Msg: cardinal;
    HotKey: longint;
    Unused: longint;
    Result: longint;
  end;

  HKLArray = array [0..1000] of HKL;
  PHKL = ^HKL;

  { TMainAppForm }

  TMainAppForm = class(TForm)
    Button1: TButton;
    Label1: TLabel;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    AddToStartMenuItem: TMenuItem;
    ListCodes: TMenuItem;
    OpenConfInNotepad: TMenuItem;
    RemoveFromStartMenuItem: TMenuItem;
    Separator1: TMenuItem;
    ExitContextMenuItem: TMenuItem;
    LanguageNameTimer: TTimer;
    Separator2: TMenuItem;
    Separator3: TMenuItem;
    TrayPopupMenu: TPopupMenu;
    TrayIcon: TTrayIcon;


    procedure AddToStartMenuItemClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    //procedure FormActivate(Sender: TObject);
    procedure ExitContextMenuItemClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LanguageNameTimerTimer(Sender: TObject);
    procedure ListCodesClick(Sender: TObject);
    procedure OpenConfInNotepadClick(Sender: TObject);
    procedure RemoveFromStartMenuItemClick(Sender: TObject);


  private
    ApplicationFilePath: string;

  public
    procedure OnMenuHotKey(var Mes: TWMHotKey); message wm_hotkey;
    procedure UpdateLanguageState();
    procedure UpdateLanguageIcon(langRec: PTlangRec);

  end;//TMainAppForm = class(TForm)

var
  Form1: TMainAppForm;

implementation

{$R *.lfm}

{ TMainAppForm }

procedure TMainAppForm.ExitContextMenuItemClick(Sender: TObject);
begin
  self.Close();
end;

procedure TMainAppForm.FormShow(Sender: TObject);
begin
  LazLogger.DebugLogger.CloseLogFileBetweenWrites := True;
  self.ApplicationFilePath := ExtractFilePath(Application.ExeName);
  //DebugLn('have handle');
  //DebugLn(self.ApplicationFilePath);
  languages.loadLanguageRecords(self.ApplicationFilePath);

  Windows.RegisterHotKey(self.Handle, 1, MOD_ALT, VK_OEM_4);
  //http://kbdedit.com/manual/low_level_vk_list.html
  Windows.RegisterHotKey(self.Handle, 2, MOD_ALT, VK_OEM_6); //}
  Windows.RegisterHotKey(self.Handle, 3, MOD_ALT, VK_OEM_5); //\

  //OLD with Ctrl
  //Windows.RegisterHotKey(self.Handle, 1, MOD_CONTROL, VK_OEM_4);
  ////http://kbdedit.com/manual/low_level_vk_list.html
  //Windows.RegisterHotKey(self.Handle, 2, MOD_CONTROL, VK_OEM_6); //}
  //Windows.RegisterHotKey(self.Handle, 3, MOD_CONTROL, VK_OEM_5); //\

  //Windows.RegisterHotKey(self.Handle, 4, MOD_CONTROL, VK_K);  //{
  //self.Hide();
  self.UpdateLanguageState();
end; //procedure TMainAppForm.FormShow(Sender: TObject);

procedure TMainAppForm.UpdateLanguageIcon(langRec: PTlangRec);
var
  errStr: string;
begin
  if langRec^.LanguageIcon <> nil then
  begin
    self.TrayIcon.Icon := langRec^.LanguageIcon;
  end
  else
  begin
    errStr := 'Have language without icon:' + langRec^.LanguageName + sLineBreak;
    errStr := errStr + '    code:' + IntToStr(langRec^.LanguageCode) + sLineBreak;
    DebugLn(errStr);
    ShowMessage(errStr);
  end;

  //if lang = 1033 then  //ENglish
  //  self.TrayIcon.Icon := self.enIcon;
  ////self.TrayIcon.Icon.AssignImage(self.ENIcon);
  //if lang = 1049 then //RU
  //  self.TrayIcon.Icon := self.ruIcon;

  //if lang = 1058 then //UKR
  //  self.TrayIcon.Icon := self.ukrIcon;
end;


procedure TMainAppForm.UpdateLanguageState();
var
  langKL: HKL;
  langID: integer;
  langName, langNameFull: string;
  forWindowHandle, parentHandle: HWND;
  procID, threadID: DWORD;
  langRec: PTlangRec;
begin
  forWindowHandle := Windows.GetForegroundWindow();
  threadID := Windows.GetWindowThreadProcessId(forWindowHandle, procID);

  langKL := Windows.GetKeyboardLayout(threadID);
  //TODO: check what is upper bytes of langKL do
  langID := (langKL and $ffff0000) shr 16;
  langRec := nil;
  langRec := languages.findLanguageByCode(langID);
  if langRec <> nil then
  begin
    langName := (langRec)^.LanguageName;
    self.UpdateLanguageIcon(langRec);

    langRec := languages.findLanguageByCode(langKL);
    if langRec <> nil then
      langNameFull := (langRec)^.LanguageName;

    //ShowMessage('Found shor language name:'+langName+' long name:'+langNameFull);
    self.Label1.Caption := langName;
    self.Caption := langName;
    Application.Title := 'Language:' + langName;
  end;
end;

procedure TMainAppForm.LanguageNameTimerTimer(Sender: TObject);
begin
  self.UpdateLanguageState();
end;

procedure TMainAppForm.ListCodesClick(Sender: TObject);
begin
  ShowMessage('Not implemented');
end;

procedure TMainAppForm.OpenConfInNotepadClick(Sender: TObject);
begin
  //https://wiki.freepascal.org/Executing_External_Programs#SysUtils.ExecuteProcess
   //ShowMessage(ExtractFilePath(Application.ExeName));
   //ExecuteProcess(ExtractFilePath(Application.ExeName), 'start notepad.exe languages.conf');
   ShellExecute(0,nil, PChar('notepad.exe'),PChar('languages.conf'),nil,1)
end;

procedure TMainAppForm.RemoveFromStartMenuItemClick(Sender: TObject);
begin
  RemoveFromStartMenu();
end; //procedure TMainAppForm.RemoveFromStartMenuItemClick(Sender: TObject);

procedure TMainAppForm.Button1Click(Sender: TObject);
var
  hkArray : ^HKLArray;
  hk : ^HKL;

  ptr : pointer;
  i, res: integer;
  layoutName : string;
//  vCode: PTVirtualCode;
begin
  //ptr := &hkArray;
  new(hkArray);
  i:=0;
  while i<Length(hkArray^) do begin
    hkArray^[i]:=0;
    i:=i+1;
  end;
  //prt := PH

  //https://learn.microsoft.com/en-us/windows/win32/api/winuser/nf-winuser-getkeyboardlayoutlist
  res := GetKeyboardLayoutList(100, PHKL(hkArray));
  i:=0;
  while i<res do begin
    DebugLn('Have language handle:'+IntToStr(hkArray^[i]));
    DebugLn('Hex value:'+IntToHex(hkArray^[i]));

    i:=i+1;
  end;
  ParseLanguageConf();
end;

procedure TMainAppForm.AddToStartMenuItemClick(Sender: TObject);
//var
//  Registry: TRegistry;
begin
  AddToStartMenu(Application.ExeName);
end;

procedure ActivateLanguage(const lng_const: string);
var
  hk: HKL;
  forWindowHandle, parentHandle: HWND;
  lang_str: PChar;
begin
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

procedure TMainAppForm.OnMenuHotKey(var Mes: TWMHotKey);
var
  hk: HKL;
  forWindowHandle, parentHandle: HWND;
  langRec: PTlangRec;
  appPath: string;
begin
  //ShowMessage('h1');
  appPath := ExtractFilePath(Application.ExeName);
  //DebugLn(String(Mes.HotKey));
  // DebugLn(String(Mes.Msg));
  //newIcon := TIcon.Create();

  if (Mes.HotKey = 1) then
  begin
    ActivateLanguage('00000409');
    self.Caption := 'EN';
    langRec := languages.findLanguageByCode(1033);
    self.UpdateLanguageIcon(langRec);
    //newIcon.LoadFromFile(
    //  appPath+'icons\EN_64x64_05Apr2024.ico');
    //self.TrayIcon.Icon := self.enIcon;
    //self.TrayIcon.Icon.AssignImage(self.ENIcon);
  end;
  if (Mes.HotKey = 2) then
  begin
    ActivateLanguage('00000419');
    //hk := Windows.LoadKeyboardLayoutW('00000419', 0);
    self.Caption := 'RUS';
    langRec := languages.findLanguageByCode(1049);
    self.UpdateLanguageIcon(langRec);
    //self.TrayIcon.Hide();
    //self.TrayIcon.Icon.AssignImage(self.RUIcon);
    //newIcon.LoadFromFile(
    //  appPath+'icons\RU_32x32.ico');
    //self.TrayIcon.Icon := self.ruIcon;
    //self.TrayIcon.Show();
    //self.TrayIcon.ShowIcon:= True;
  end;
  if (Mes.HotKey = 3) then
  begin
    ActivateLanguage('00000422');
    //hk := Windows.LoadKeyboardLayoutW('00000422', 0);
    self.Caption := 'UKR';
    langRec := languages.findLanguageByCode(1058);
    self.UpdateLanguageIcon(langRec);
    //newIcon.LoadFromFile(
    //  appPath+'icons\UKR_64x64_05Apr2024.ico');
    //self.TrayIcon.Icon := self.ukrIcon;
  end;

end;//procedure TMainAppForm.OnMenuHotKey(var Mes: TWMHotKey);


end.
