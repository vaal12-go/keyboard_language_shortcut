unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Menus, windows, LazLogger;

type

  TWMHotKey = packed record
    Msg: Cardinal;
    HotKey: Longint;
    Unused: Longint;
    Result: Longint;
  end;

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    Separator1: TMenuItem;
    MenuItem5: TMenuItem;
    TrayPopupMenu: TPopupMenu;
    TrayIcon: TTrayIcon;
    procedure FormActivate(Sender: TObject);
  private

  public
    ENIcon, RUIcon : Graphics.TPortableNetworkGraphic;
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
begin
  //ShowMessage('Form activated');
  windows.RegisterHotKey(self.Handle, 1, MOD_CONTROL, VK_OEM_3);
//  VK_OEM_3	0xC0	OEM_3 (~ `)
//http://kbdedit.com/manual/low_level_vk_list.html
  windows.RegisterHotKey(self.Handle, 2, MOD_CONTROL, VK_1);
  windows.RegisterHotKey(self.Handle, 3, MOD_CONTROL, VK_2);
  self.ENIcon := Graphics.TPortableNetworkGraphic.Create();
  self.ENIcon.LoadFromFile('c:\Users\may13\AGVDocs\Dev\04.Lazarus-projects\01.LangShortcut\icons\EN_32x32.png');
  self.RUIcon := Graphics.TPortableNetworkGraphic.Create();
  self.RUIcon.LoadFromFile('c:\Users\may13\AGVDocs\Dev\04.Lazarus-projects\01.LangShortcut\icons\RU_32x32.png');

  //ico := TIcon.Create()
  //ico.

end;

procedure TForm1.OnMenuHotKey(var Mes: TWMHotKey);
var
   hk: HKL;
   forWindowHandle, parentHandle : HWND;
   newIcon : TIcon;
begin
 //DebugLn(String(Mes.HotKey));
 // DebugLn(String(Mes.Msg));
 //ShowMessage('h1');
  newIcon := TIcon.Create();
  forWindowHandle := windows.GetForegroundWindow();
  if(Mes.Unused = 1) then begin
                  hk := windows.LoadKeyboardLayoutW('00000409', 0);
                  self.Caption:= 'EN';
                  newIcon.LoadFromFile('c:\Users\may13\AGVDocs\Dev\04.Lazarus-projects\01.LangShortcut\icons\EN_64x64_05Apr2024.ico');
                                self.TrayIcon.Icon := newIcon;
                                     //self.TrayIcon.Icon.AssignImage(self.ENIcon);
    end;
  if(Mes.Unused = 2) then begin
                  hk := windows.LoadKeyboardLayoutW('00000419', 0);
                                    self.Caption:= 'RUS';
                                            //self.TrayIcon.Hide();
                              //self.TrayIcon.Icon.AssignImage(self.RUIcon);
                              newIcon.LoadFromFile('c:\Users\may13\AGVDocs\Dev\04.Lazarus-projects\01.LangShortcut\icons\RU_32x32.ico');
                                self.TrayIcon.Icon := newIcon;
                              //self.TrayIcon.Show();
                                                             //self.TrayIcon.ShowIcon:= True;
    end;
    if(Mes.Unused = 3) then begin
                  hk := windows.LoadKeyboardLayoutW('00000422', 0);
                                    self.Caption:= 'UKR';
                                    newIcon.LoadFromFile('c:\Users\may13\AGVDocs\Dev\04.Lazarus-projects\01.LangShortcut\icons\UKR_64x64.ico');
                                self.TrayIcon.Icon := newIcon;
    end;
 LazLogger.DebugLogger.CloseLogFileBetweenWrites:= True;
 DebugLn('have handle');
 //self.Hide();
 //self.Show();
//  00000419 - RUS
//  00000422 - UKR
 windows.ActivateKeyboardLayout(hk, 0);
 windows.PostMessage(forWindowHandle, windows.WM_INPUTLANGCHANGEREQUEST, 0, hk);

               parentHandle:= windows.GetParent(forWindowHandle);
                windows.PostMessage(parentHandle, windows.WM_INPUTLANGCHANGEREQUEST, 0, hk);

  //self.Hide();
end;//procedure TForm1.OnMenuHotKey(var Mes: TWMHotKey);


end.

