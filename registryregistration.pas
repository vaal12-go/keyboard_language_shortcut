unit RegistryRegistration;

{$mode ObjFPC}{$H+}

interface



uses
  Classes, SysUtils, registry;

procedure AddToStartMenu(exeName: string);
procedure RemoveFromStartMenu();

implementation

procedure RemoveFromStartMenu();
var
  Registry: TRegistry;
begin
  //ShowMessage('Will remove from autostart');
  Registry := TRegistry.Create;
  try
    // Navigate to proper "directory":
    Registry.RootKey := HKEY_CURRENT_USER;
    //if Registry.OpenKeyReadOnly('\Software\Microsoft\Windows\CurrentVersion\Run') then
    if Registry.OpenKey('\Software\Microsoft\Windows\CurrentVersion\Run\',
      False) then
      //CompileCommand:=Registry.ReadString(''); //read the value of the default name
      Registry.DeleteValue('KeyboardLangChange')
    //Registry.WriteString(, '');
  finally
    Registry.Free  // In non-Windows operating systems this flushes the reg.xml file to disk
  end;
end;


procedure AddToStartMenu(exeName: string);
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
      Registry.WriteString('KeyboardLangChange', '"' + exeName + '"');
  finally
    Registry.Free;  // In non-Windows operating systems this flushes the reg.xml file to disk
  end;
end;

end.
