unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, windows, LazLogger;

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
    procedure FormActivate(Sender: TObject);
  private

  public
    procedure OnMenuHotKey(var Mes: TWMHotKey); message wm_hotkey;


  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormActivate(Sender: TObject);
begin
  //ShowMessage('Form activated');
  windows.RegisterHotKey(self.Handle, 1, MOD_CONTROL, VK_4);
  windows.RegisterHotKey(self.Handle, 2, MOD_CONTROL, VK_5);
  windows.RegisterHotKey(self.Handle, 3, MOD_CONTROL, VK_6);

end;

procedure TForm1.OnMenuHotKey(var Mes: TWMHotKey);
var
   hk: HKL;
   forWindowHandle : HWND;

begin
 //DebugLn(String(Mes.HotKey));
 // DebugLn(String(Mes.Msg));
 //ShowMessage('h1');
  forWindowHandle := windows.GetForegroundWindow();
  if(Mes.Unused = 1) then begin
                  hk := windows.LoadKeyboardLayoutW('00000409', 0);
                  self.Caption:= 'EN';
    end;
  if(Mes.Unused = 2) then begin
                  hk := windows.LoadKeyboardLayoutW('00000419', 0);
                                    self.Caption:= 'RUS';
    end;
    if(Mes.Unused = 3) then begin
                  hk := windows.LoadKeyboardLayoutW('00000422', 0);
                                    self.Caption:= 'UKR';
    end;
 LazLogger.DebugLogger.CloseLogFileBetweenWrites:= True;
 DebugLn('have handle');
 //self.Hide();
 //self.Show();
//  00000419 - RUS
//  00000422 - UKR
 windows.ActivateKeyboardLayout(hk, 0);
 windows.PostMessage(forWindowHandle, windows.WM_INPUTLANGCHANGEREQUEST, 0, hk);
  //self.Hide();
end;//procedure TForm1.OnMenuHotKey(var Mes: TWMHotKey);


end.

