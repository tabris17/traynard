unit Traynard.Hotkey;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, LMessages, Traynard.Types;

type

  { THotkeyManager }

  THotkeyManager = class(TComponent)
  private
    FMainForm: TForm;
    FOriginalWindowProc: TWndMethod;
    FHotkeys: array[THotkeyID] of THotkeyInfo;
    function GetHotkey(HotkeyID: THotkeyID): THotkeyInfo;
    procedure WindowProc(var TheMessage: TLMessage);
    procedure RegisterHotkeys;
    procedure HotkeyChanged(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    property Hotkey[HotkeyID: THotkeyID]: THotkeyInfo read GetHotkey;
  end;

var
  HotkeyManager: THotkeyManager = nil;

implementation

uses
  Windows, Traynard.Strings, Traynard.Window, Traynard.Settings, Traynard.Form.Main;

{ THotkeyManager }

procedure THotkeyManager.WindowProc(var TheMessage: TLMessage);
var
  TheWnd: HWND;
  ExStyle: LONG;
begin
  if TheMessage.msg = WM_HOTKEY then
  begin
    case TheMessage.wParam of
      Ord(hiMinimizeToIcon):
      begin
        if not WindowManager.TryMinimizeWindow(GetForegroundWindow, tpIcon) then MessageBeep(MB_ICONWARNING);
      end;
      Ord(hiMinimizeToMenu):
      begin
        if not WindowManager.TryMinimizeWindow(GetForegroundWindow, tpMenu) then MessageBeep(MB_ICONWARNING);
      end;
      Ord(hiRestoreLastWindow):
      begin
        if not WindowManager.TryRestoreLastWindow then MessageBeep(MB_ICONWARNING);
      end;
      Ord(hiRestoreAll):
        WindowManager.TryRestoreAllWindows;
      Ord(hiToggleTopmost):
      begin
        TheWnd := GetForegroundWindow;
        ExStyle := GetWindowLong(TheWnd, GWL_EXSTYLE);
        if not SetWindowPos(
          GetForegroundWindow,
          specialize IfThen<HWND>(ExStyle and WS_EX_TOPMOST = 0, HWND_TOPMOST, HWND_NOTOPMOST),
          0, 0, 0, 0,
          SWP_NOMOVE or SWP_NOSIZE
        ) then MessageBeep(MB_ICONWARNING);
      end;
      Ord(hiOpenClose):
      begin
        if Assigned(FormMain) then
          FormMain.Close
        else
        begin
          Application.CreateForm(TFormMain, FormMain);
          FormMain.Show;
          FormMain.BringToFront;
        end;
      end;
      Ord(hiToggleAutoMinimize):
        Settings.AutoMinimize := not Settings.AutoMinimize;
      Ord(hiToggleRules):
        Settings.ApplyRules := not Settings.ApplyRules;
    end;
  end
  else
    FOriginalWindowProc(TheMessage);
end;

function THotkeyManager.GetHotkey(HotkeyID: THotkeyID): THotkeyInfo;
begin
  Result := FHotkeys[HotkeyID];
end;

procedure THotkeyManager.RegisterHotkeys;
var
  HotkeyID: THotkeyID;
  HotkeyInfo: PHotkeyInfo;
  TheHotkey: THotkey;
  HotkeyState: THotkeyState;
begin
  for HotkeyID := Low(THotkeyID) to High(THotkeyID) do
  begin
    HotkeyInfo := @FHotkeys[HotkeyID];
    TheHotkey := Settings.Hotkey[HotkeyID];

    if HotkeyInfo^.Hotkey.Value = TheHotkey.Value then Continue;

    HotkeyInfo^.Hotkey := TheHotkey;
    HotkeyState := HotkeyInfo^.State;
    if HotkeyState = hsSucceeded then
      UnregisterHotkey(FMainForm.Handle, Ord(HotkeyID));

    if TheHotkey.Value = 0 then
      HotkeyInfo^.State := hsNone
    else
      HotkeyInfo^.State := specialize IfThen<THotkeyState>(
        RegisterHotkey(FMainForm.Handle, Ord(HotkeyID), TheHotkey.Modifiers or MOD_NOREPEAT, TheHotkey.Key),
        hsSucceeded, hsFailed
      );
  end;
end;

procedure THotkeyManager.HotkeyChanged(Sender: TObject);
begin
  RegisterHotkeys;
end;

constructor THotkeyManager.Create(AOwner: TComponent);
var
  HotkeyID: THotkeyID;
  HotkeyInfo: PHotkeyInfo;
begin
  inherited Create(AOwner);

  FMainForm := AOwner as TForm;
  FOriginalWindowProc := FMainForm.WindowProc;
  FMainForm.WindowProc := @WindowProc;
  for HotkeyID := Low(THotkeyID) to High(THotkeyID) do
  begin
    HotkeyInfo := @FHotkeys[HotkeyID];
    HotkeyInfo^.Hotkey.Value := 0;
    HotkeyInfo^.State := hsNone;
  end;
  RegisterHotkeys;
  Settings.AddListener(siHotkey, @HotkeyChanged);
end;

end.

