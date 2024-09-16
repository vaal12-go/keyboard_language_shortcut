unit MainUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Menus, Windows, LazLogger, JwaWinUser, registry, languages,
  RegistryRegistration, Parser, ShellApi, ConfigReader;

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
    //procedure FormActivate(Sender: TObject);
    procedure ExitContextMenuItemClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LanguageNameTimerTimer(Sender: TObject);
    procedure ListCodesClick(Sender: TObject);
    procedure OpenConfInNotepadClick(Sender: TObject);
    procedure RemoveFromStartMenuItemClick(Sender: TObject);



  private
    ApplicationFilePath: string;
    DebugMode : Boolean;
    procedure InitApp();
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
  MItem: TMenuItem;
  currDateTime : TDateTime;
  dtStr, logFName, renameLogFName, hmsStr : string;
  i : integer;

begin
  self.DebugMode:= False;
  LazLogger.DebugLogger.CloseLogFileBetweenWrites := True;
  currDateTime := Now();
  DateTimeToString (dtStr,'yymmmdd_ddd',currDateTime);

  self.ApplicationFilePath := ExtractFilePath(Application.ExeName);

  logFName := self.ApplicationFilePath+'langcut_'+dtStr+'.log';
  if FileExists(logFName) then begin
    DateTimeToString(hmsStr, 'hh_mm_ss', currDateTime);
    renameLogFName := self.ApplicationFilePath+'langcut_'+dtStr+'_pre_'+hmsStr+'.log';
    RenameFile(logFName, renameLogFName);
  end;



  LazLogger.DebugLogger.LogName:= logFName;

  DebugLn('Current time:'+dtStr+'  '+hmsStr);

  languages.loadLanguageRecords(self.ApplicationFilePath);
  LoadVirtualCodesFromFile(self.ApplicationFilePath);


  //TODO: Alt modifier leads to 'freezing' of switching languages after several switches
  //Windows.RegisterHotKey(self.Handle, 1, MOD_ALT, VK_OEM_4);
  ////http://kbdedit.com/manual/low_level_vk_list.html
  //Windows.RegisterHotKey(self.Handle, 2, MOD_ALT, VK_OEM_6); //}
  //Windows.RegisterHotKey(self.Handle, 3, MOD_ALT, VK_OEM_5); //\

  //OLD with Ctrl
  Windows.RegisterHotKey(self.Handle, 1, MOD_CONTROL, VK_OEM_4);
  //http://kbdedit.com/manual/low_level_vk_list.html
  Windows.RegisterHotKey(self.Handle, 2, MOD_CONTROL, VK_OEM_6); //}
  Windows.RegisterHotKey(self.Handle, 3, MOD_CONTROL, VK_OEM_5); //\

  //Windows.RegisterHotKey(self.Handle, 4, MOD_CONTROL, VK_K);  //{
  //self.Hide();
  self.UpdateLanguageState();

  //Creation of new menu item
  //MItem := TMenuItem.Create(Self);
  //MItem.Caption := 'Caption';
  ////MItem.OnClick := OClick;
  ////MItem.Name := ItemName;
  //TrayPopupMenu.Items.Insert(2, MItem);

  ReadConfigFile(self.ApplicationFilePath);

  DebugLn('param[0]:'+paramStr(0));
  for i := 1 to paramCount() do
	begin
		DebugLn('. argument: ', paramStr(i));
    if paramStr(i) = '--dbg' then
        DebugLn('Running in debug mode');
        self.DebugMode:= True;
	end;

  if not self.DebugMode then
    self.Hide();


end;//procedure TMainAppForm.InitApp();

procedure TMainAppForm.ExitContextMenuItemClick(Sender: TObject);
begin
  self.Close();
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
  langName: string;
  forWindowHandle: HWND;
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
    //if langRec <> nil then
    //  langNameFull := (langRec)^.LanguageName;

    //ShowMessage('Found shor language name:'+langName+' long name:'+langNameFull);
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
  //hk: ^HKL;
  //ptr: pointer;
  i, res: integer;
  //layoutName: string;
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
  //hk: HKL;
  //forWindowHandle, parentHandle: HWND;
  langRec: PTlangRec;
  //appPath: string;
begin
  //appPath := ExtractFilePath(Application.ExeName);

  LanguageNameTimer.Enabled:=False;

  if (Mes.HotKey = 1) then
  begin
    ActivateLanguage('00000409');
    self.Caption := 'EN';
    langRec := languages.findLanguageByCode(1033);
    self.UpdateLanguageIcon(langRec);
  end;
  if (Mes.HotKey = 2) then
  begin
    ActivateLanguage('00000419');
    self.Caption := 'RUS';
    langRec := languages.findLanguageByCode(1049);
    self.UpdateLanguageIcon(langRec);
  end;
  if (Mes.HotKey = 3) then
  begin
    ActivateLanguage('00000422');
    self.Caption := 'UKR';
    langRec := languages.findLanguageByCode(1058);
    self.UpdateLanguageIcon(langRec);
  end;
  LanguageNameTimer.Interval:= 1000;
  LanguageNameTimer.Enabled:=True;
end;//procedure TMainAppForm.OnMenuHotKey(var Mes: TWMHotKey);


end.
