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
  windows.RegisterHotKey(self.Handle, 1, MOD_ALT, VK_4);
end;

procedure TForm1.OnMenuHotKey(var Mes: TWMHotKey);
var
   hk: HKL;
   forWindowHandle : HWND;

begin
 //ShowMessage('h1');
 LazLogger.DebugLogger.CloseLogFileBetweenWrites:= True;
 forWindowHandle := windows.GetForegroundWindow();
 DebugLn('have handle');
 //self.Hide();
 //self.Show();
  hk := windows.LoadKeyboardLayoutW('00000409', 0);
 windows.ActivateKeyboardLayout(hk, 0);

 windows.PostMessage(forWindowHandle, windows.WM_INPUTLANGCHANGEREQUEST, 0, hk);

  //self.Hide();

end;//procedure TForm1.OnMenuHotKey(var Mes: TWMHotKey);


end.

