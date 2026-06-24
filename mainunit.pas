unit MainUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Menus, Windows, LazLogger, JwaWinUser, ShellApi, LexerConstants, LanguagesTypes,
  languages, RegistryRegistration, Parser, ConfigReader;

type
//  This version is working for 64 bit systems
  TWMHotKey = record
    Msg: Cardinal;
    MsgFiller: TDWordFiller;
    HotKey: WPARAM;
    Unused: LPARAM;
    Result: LRESULT;
  end;

  HKLArray = array [0..200] of HKL;
  PHKL = ^HKL;

  { TMainAppForm }

  TMainAppForm = class(TForm)
    Button1: TButton;
    Label1: TLabel;
    AddToStartMenuItem: TMenuItem;
    ListCodes: TMenuItem;
    MenuItem1: TMenuItem;
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
    procedure ExitContextMenuItemClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LanguageNameTimerTimer(Sender: TObject);
    procedure ListCodesClick(Sender: TObject);
    procedure OpenConfInNotepadClick(Sender: TObject);
    procedure RemoveFromStartMenuItemClick(Sender: TObject);

  private
    ApplicationFilePath: string;
    DebugMode : Boolean;
    procedure InitApp();
    procedure ActivateLanguage(langRec: PTShortcutLangRec);
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


procedure TMainAppForm.InitApp();
var
  currDateTime : TDateTime;
  dtStr, logFName, renameLogFName, hmsStr : string;
  i, modifier: integer;
  modifiers: uint;
  currShortcutRec: PTShortcutLangRec;

//  TODO: Check if 'Remove from autostart' function in context menu is really working.
//  TODO: Check how other modifiers e.g. Alt, Shift are working
//  TODO: write more explanation about setting multiple keys for one keyboard e.g.
//    for US Intl. and Dvorak layouts

begin
//  [x]: check why .conf file is not working with numbers (0..9) in Virtual codes:

//  [x]: check why after Dvorak keyboard RUS language is not turning off. UKR does
  self.DebugMode:= False;
  LazLogger.DebugLogger.CloseLogFileBetweenWrites := True;
  currDateTime := Now();
  DateTimeToString (dtStr,'yymmmdd_ddd',currDateTime);

  self.ApplicationFilePath := ExtractFilePath(Application.ExeName);

  logFName := self.ApplicationFilePath+'\logs\langcut_'+dtStr+'.log';
  if FileExists(logFName) then begin
    DateTimeToString(hmsStr, 'hh_mm_ss', currDateTime);
    renameLogFName := self.ApplicationFilePath+'\logs\langcut_'+dtStr+'_pre_'+hmsStr+'.log';
    RenameFile(logFName, renameLogFName);
  end;

  LazLogger.DebugLogger.LogName:= logFName;

  InitLanguagesModule();
  languages.loadLanguageRecords(self.ApplicationFilePath);

  //TODO: move all helper/codes lists (and their loading) to separate unit
  LoadVirtualCodesFromFile(self.ApplicationFilePath);

  //TODO: Alt modifier leads to 'freezing' of switching languages after several switches
  //Windows.RegisterHotKey(self.Handle, 1, MOD_ALT, VK_OEM_4);
  ////http://kbdedit.com/manual/low_level_vk_list.html

  self.UpdateLanguageState();

  //Creation of new menu item
  //MItem := TMenuItem.Create(Self);
  //MItem.Caption := 'Caption';
  ////MItem.OnClick := OClick;
  ////MItem.Name := ItemName;
  //TrayPopupMenu.Items.Insert(2, MItem);

  ReadConfigFile(self.ApplicationFilePath);
  i:=1;
  modifiers :=0;
  for currShortcutRec in ConfigPTShortcutLangRecArr^ do begin
    for modifier in currShortcutRec^.KbModifierArr do begin
      modifiers := modifiers or modifier;
    end;
    currShortcutRec^.HotKeyID:=i;
    Windows.RegisterHotKey(self.Handle, i, modifiers, currShortcutRec^.Key);
    i:=i+1;
  end;

  // DebugLn('Total shortcuts registered:'+IntToStr(i-1));
  for i := 1 to paramCount() do
	begin
    if paramStr(i) = '--dbg' then
        self.DebugMode:= True;
	end;

  if not self.DebugMode then
    self.Hide();
end;//procedure TMainAppForm.InitApp();

procedure TMainAppForm.ExitContextMenuItemClick(Sender: TObject);
begin
  self.Close();
end;

procedure TMainAppForm.FormDestroy(Sender: TObject);
begin
  DebugLn('OnDestroy called');
  DisposeVirtualCodeArray();
  DisposeConfigPTShortcutLangRecArr();
  DisposeLanguageRecords();
  FinishLanguagesModule();
  DebugLn('Disposals finished');
end;

procedure TMainAppForm.FormShow(Sender: TObject);
begin
  self.InitApp();
end; 

procedure TMainAppForm.UpdateLanguageIcon(langRec: PTlangRec);
var
  errStr: string;
begin
  if langRec^.LanguageIconFileName <> '' then
  begin
    self.TrayIcon.Icon := langRec^.LanguageIcon;
  end else begin
    errStr := 'Have language without icon:' + langRec^.LanguageName + sLineBreak;
    errStr := errStr + '    code:' + IntToStr(langRec^.LanguageCodeInt) + sLineBreak;
    DebugLn(errStr);
  end;
end;//procedure TMainAppForm.UpdateLanguageIcon(langRec: PTlangRec);

function GetForegroundWindowKeyboardLayout(): HKL;
var
  langKL: HKL;
  langID: integer;
  langName: string;
  forWindowHandle: HWND;
  procID, threadID: DWORD;
  langRec: PTlangRec;
begin
  forWindowHandle := Windows.GetForegroundWindow();
  procID := 0;
  threadID := Windows.GetWindowThreadProcessId(forWindowHandle, procID);

  langKL := Windows.GetKeyboardLayout(threadID);
  //TODO: check what is upper bytes of langKL do

  exit(langKL);
end;

procedure TMainAppForm.UpdateLanguageState();
var
  langKL: HKL;
  langID: integer;
  langName: string;
  forWindowHandle: HWND;
  procID, threadID: DWORD;
  langRec: PTlangRec;
begin
  langKL := GetForegroundWindowKeyboardLayout();

  langID := (langKL and $ffff0000) shr 16;


  langRec := nil;

  langRec := languages.findLanguageByCode(langID);

  if langRec <> nil then
  begin
    langName := (langRec)^.LanguageName;
    self.UpdateLanguageIcon(langRec);

    langRec := languages.findLanguageByCode(langKL);
    self.Label1.Caption := langName;
    self.Caption := langName;
    Application.Title := 'Language:' + langName;
  end;
end;

procedure TMainAppForm.LanguageNameTimerTimer(Sender: TObject);
begin
  self.UpdateLanguageState();
  LanguageNameTimer.Interval:=100;
end;

procedure TMainAppForm.ListCodesClick(Sender: TObject);
begin
  ShowMessage('Not implemented');
end;

procedure TMainAppForm.OpenConfInNotepadClick(Sender: TObject);
begin
  //https://wiki.freepascal.org/Executing_External_Programs#SysUtils.ExecuteProcess
  ShellExecute(0, nil, PChar('notepad.exe'), PChar('languages.conf'), nil, 1);
end;

procedure TMainAppForm.RemoveFromStartMenuItemClick(Sender: TObject);
begin
  RemoveFromStartMenu();
end; //procedure TMainAppForm.RemoveFromStartMenuItemClick(Sender: TObject);

procedure TMainAppForm.Button1Click(Sender: TObject);
var
  hkArray: ^HKLArray;
  i, res: integer;
begin
  new(hkArray);
  i := 0;
  while i < Length(hkArray^) do
  begin
    hkArray^[i] := 0;
    i := i + 1;
  end;
  //https://learn.microsoft.com/en-us/windows/win32/api/winuser/nf-winuser-getkeyboardlayoutlist
  res := GetKeyboardLayoutList(100, PHKL(hkArray));
  i := 0;
  while i < res do
  begin
    DebugLn('Have language handle:' + IntToStr(hkArray^[i]));
    DebugLn('Hex value:' + IntToHex(hkArray^[i]));
    i := i + 1;
  end;

end;

procedure TMainAppForm.AddToStartMenuItemClick(Sender: TObject);
begin
  AddToStartMenu(Application.ExeName);
end;

procedure TMainAppForm.ActivateLanguage(langRec: PTShortcutLangRec);
var
  hk, old_hkl: HKL;
  forWindowHandle, parentHandle: HWND;
  lang_str: PChar;
  lang_str_wide : PWideChar;
  err_code, hk_dword: DWORD;
  hk_qword: QWord;
begin
  if langRec = nil then begin
     ShowMessage('Language record is empty');
     exit;
  end;

//  Some layouts (e.g. Dvorak) redeclare where VK_OEM_4 buttons are located on the keyboard - e.g.
//    on Dvorak it becomes '-' button on the US keyboard.
//  TODO: this creates a need to have several keyboard shortcuts for one language.

  old_hkl := GetForegroundWindowKeyboardLayout();
  //hk := Windows.LoadKeyboardLayout(lang_str, JwaWinUser.KLF_ACTIVATE or
  //  JwaWinUser.KLF_SUBSTITUTE_OK or JwaWinUser.KLF_SETFORPROCESS or KLF_REPLACELANG);
  hk := Windows.LoadKeyboardLayout(LPCSTR(langRec^.LanguageRec^.LanguageCodeStr), KLF_ACTIVATE);
  err_code := GetLastError();
  //                 or JwaWinUser.KLF_NOTELLSHELL
  //hk := hk and $1111111111111111;
  //Windows.ActivateKeyboardLayout(hk, JwaWinUser.KLF_SETFORPROCESS);
  forWindowHandle := Windows.GetForegroundWindow();

  Windows.PostMessage(forWindowHandle, Windows.WM_INPUTLANGCHANGEREQUEST, 0, LPARAM(hk));
  parentHandle := Windows.GetParent(forWindowHandle);
  Windows.PostMessage(parentHandle, Windows.WM_INPUTLANGCHANGEREQUEST, 0, LPARAM(hk));

  self.UpdateLanguageIcon(langRec^.LanguageRec);
end;//procedure ActivateLanguage(var lng_const : string);

function findShortcutRecByHotkey(hotkey : longint) : PTShortcutLangRec;
var
  currRec : PTShortcutLangRec;
begin
  for currRec in ConfigPTShortcutLangRecArr^ do begin
    if currRec^.HotKeyID = hotkey then
      exit(currRec);
  end;
  exit(nil);
end;

procedure TMainAppForm.OnMenuHotKey(var Mes: TWMHotKey);
var
  shortcutRec :PTShortcutLangRec;
begin
  shortcutRec:=findShortcutRecByHotkey(Mes.HotKey);
  // DebugLn('---------------------');
  PrintShortcutLangRec(shortcutRec);
  LanguageNameTimer.Enabled:=False;
  self.ActivateLanguage(shortcutRec);
  LanguageNameTimer.Interval:= 1000;
  LanguageNameTimer.Enabled:=True;
end;//procedure TMainAppForm.OnMenuHotKey(var Mes: TWMHotKey);

//c46c48ddaccc53835237b551df240c1dc51ca78911fdec845481c04bb2bb438b71ed15431131850dd2654a136047dbe3129c728d4148f971d3bc0d7568124a86
end.
