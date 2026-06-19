unit MainUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Menus, Windows, LazLogger, JwaWinUser, ShellApi, LexerConstants, LanguagesTypes,
  languages, RegistryRegistration, Parser, ConfigReader;

type
  //TWMHotKey = packed record
  //  Msg: cardinal;
  //  HotKey: longint;
  //  Unused: longint;
  //  Result: longint;
  //end;
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
    //destructor Destroy();

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

begin
//  TODO: check why .conf file is not working with numbers (0..9) in Virtual codes:
//  https://learn.microsoft.com/en-us/windows/win32/inputdev/virtual-key-codes
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
  //Windows.RegisterHotKey(self.Handle, 2, MOD_ALT, VK_OEM_6); //}
  //Windows.RegisterHotKey(self.Handle, 3, MOD_ALT, VK_OEM_5); //\

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

  DebugLn('Total shortcuts registered:'+IntToStr(i-1));

  //DebugLn('Exiting Init2');
  //exit();
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


  // By default information is written to standard output,
  // this function allows you to redirect the information to a file
  //SetHeapTraceOutput('heaptrace.log');
  //
  //// normally the heap dump will be written automatically at the end,
  //// but can also be written on demand any time
  //DumpHeap;

  DebugLn('Disposals finished');
end;

procedure TMainAppForm.FormShow(Sender: TObject);
begin
  self.InitApp();
end; //procedure TMainAppForm.FormShow(Sender: TObject);

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
    //ShowMessage(errStr);
  end;
end;//procedure TMainAppForm.UpdateLanguageIcon(langRec: PTlangRec);

procedure TMainAppForm.UpdateLanguageState();
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
  //langRec: PTlangRec;
  shortcutRec :PTShortcutLangRec;
begin
//  Since some version of Windows 10 Mes.HotKey stopped returning Hotkey ID
  // Instead it is returned in Mes.Unused
  shortcutRec:=findShortcutRecByHotkey(Mes.HotKey);
  DebugLn('---------------------');
  PrintShortcutLangRec(shortcutRec);

  LanguageNameTimer.Enabled:=False;

  self.ActivateLanguage(shortcutRec);
  LanguageNameTimer.Interval:= 1000;
  LanguageNameTimer.Enabled:=True;
end;//procedure TMainAppForm.OnMenuHotKey(var Mes: TWMHotKey);
//34563fa7993331f673895a167bf2aab2542a409fe6bb765bf0226c582d54a7e226173c050e6f2c12d0d24b3a331fec0ae13617f328d2c192a4c3802fa21f06ae

end.
