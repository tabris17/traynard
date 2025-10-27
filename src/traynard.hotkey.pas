unit Traynard.Hotkey;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, LMessages, Traynard.Types;

const
  TEST_HOTKEY_ID = 0;
  HOTKEYS_LENGTH = Ord(High(THotkeyID)) + 1;

type

  { THotkeyManager }

  THotkeyManager = class(TComponent)
  private
    FMainForm: TForm;
    FOriginalWindowProc: TWndMethod;
    FHotkeys: array of THotkeyInfo;
    function GetHotkey(HotkeyID: longint): THotkeyInfo;
    procedure HandleRuleHotkey(const HotkeyID: longint);
    procedure WindowProc(var TheMessage: TLMessage);
    procedure RegisterSettingsHotkeys;
    procedure SettingsHotkeyChanged(Sender: TObject);
    procedure RegisterRuleHotkeys;
  protected
    procedure RuleHotkeyAdded(const AHotkey: THotkey; out HotkeyID: longint);
    procedure RuleHotkeyRemoved(const HotkeyID: longint);
  public
    constructor Create(AOwner: TComponent); override;
    property Hotkey[HotkeyID: longint]: THotkeyInfo read GetHotkey;
    function TestHotkey(const AHotkey: THotkey): boolean;
    function Register(const AHotkey: THotkey; out HotkeyID: longint): boolean;
    function Unregister(const HotkeyID: longint): boolean;
  end;

var
  HotkeyManager: THotkeyManager = nil;

implementation

uses
  Windows, Traynard.Window, Traynard.Helpers, Traynard.Settings, Traynard.Rule, Traynard.Form.Main;

{ THotkeyManager }

procedure THotkeyManager.HandleRuleHotkey(const HotkeyID: longint);
var
  Rule: TRule;
  Window: TWindow;
begin
  if HotkeyID >= Length(FHotkeys) then Exit;

  for Rule in Rules.Filter(FHotkeys[HotkeyID].Hotkey) do
  begin
    for Window in WindowManager.Desktop do
    begin
      if Rules.Match(Window, Rule) then
        WindowManager.TryMinimizeWindow(Window.Handle, Rule.Position);
    end;
  end;
end;

procedure THotkeyManager.WindowProc(var TheMessage: TLMessage);
var
  TheWnd: HWND;
  ExStyle: LONG;
  HotkeyID: longint;
begin
  if TheMessage.msg = WM_HOTKEY then
  begin
    HotkeyID := TheMessage.wParam;
    case HotkeyID of
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
    else
      HandleRuleHotkey(HotkeyID);
    end;
  end
  else
    FOriginalWindowProc(TheMessage);
end;

function THotkeyManager.GetHotkey(HotkeyID: longint): THotkeyInfo;
begin
  Result := FHotkeys[HotkeyID];
end;

procedure THotkeyManager.RegisterSettingsHotkeys;
var
  HotkeyID: THotkeyID;
  HotkeyInfo: PHotkeyInfo;
  TheHotkey: THotkey;
  HotkeyState: THotkeyState;
begin
  for HotkeyID := Low(THotkeyID) to High(THotkeyID) do
  begin
    HotkeyInfo := @FHotkeys[Ord(HotkeyID)];
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

procedure THotkeyManager.SettingsHotkeyChanged(Sender: TObject);
begin
  RegisterSettingsHotkeys;
end;

procedure THotkeyManager.RegisterRuleHotkeys;
var
  HotkeyPair: TRules.THotkeyPair;
  HotkeyID: longint;
begin
  for HotkeyPair in Rules.Hotkeys do
  begin
    if Register(HotkeyPair.Key, HotkeyID) then
      HotkeyPair.Value.HotkeyID := HotkeyID;
  end;
end;

procedure THotkeyManager.RuleHotkeyAdded(const AHotkey: THotkey; out HotkeyID: longint);
begin
  Register(AHotkey, HotkeyID);
end;

procedure THotkeyManager.RuleHotkeyRemoved(const HotkeyID: longint);
begin
  Unregister(HotkeyID);
end;

constructor THotkeyManager.Create(AOwner: TComponent);
var
  HotkeyID: THotkeyID;
  HotkeyInfo: PHotkeyInfo;
begin
  inherited Create(AOwner);

  SetLength(FHotkeys, HOTKEYS_LENGTH);
  FMainForm := AOwner as TForm;
  FOriginalWindowProc := FMainForm.WindowProc;
  FMainForm.WindowProc := @WindowProc;
  for HotkeyID := Low(THotkeyID) to High(THotkeyID) do
  begin
    HotkeyInfo := @FHotkeys[Ord(HotkeyID)];
    HotkeyInfo^.Hotkey.Value := 0;
    HotkeyInfo^.State := hsNone;
  end;
  RegisterSettingsHotkeys;
  Settings.AddListener(siHotkey, @SettingsHotkeyChanged);
  RegisterRuleHotkeys;
  Rules.OnHotkeyAddedNotify := @RuleHotkeyAdded;
  Rules.OnHotkeyRemovedNotify := @RuleHotkeyRemoved;
end;

function THotkeyManager.TestHotkey(const AHotkey: THotkey): boolean;
begin
  Result := RegisterHotkey(FMainForm.Handle, TEST_HOTKEY_ID, AHotkey.Modifiers or MOD_NOREPEAT, AHotkey.Key);
  if Result then UnregisterHotkey(FMainForm.Handle, TEST_HOTKEY_ID);
end;

function THotkeyManager.Register(const AHotkey: THotkey; out HotkeyID: longint): boolean;
var
  HotkeyInfo: PHotkeyInfo;
begin
  HotkeyID := Length(FHotkeys);
  if (AHotkey.Value = 0) or
     not RegisterHotkey(FMainForm.Handle, HotkeyID, AHotkey.Modifiers or MOD_NOREPEAT, AHotkey.Key) then
    Exit(False);
  SetLength(FHotkeys, HotkeyID + 1);
  HotkeyInfo := @FHotkeys[HotkeyID];
  HotkeyInfo^.Hotkey := AHotkey;
  HotkeyInfo^.State := hsSucceeded;
  Result := True;
end;

function THotkeyManager.Unregister(const HotkeyID: longint): boolean;
var
  HotkeyInfo: PHotkeyInfo;
begin
  if (HotkeyID > Length(FHotkeys)) or (HotkeyID = TEST_HOTKEY_ID) then Exit(False);
  HotkeyInfo := @FHotkeys[HotkeyID];
  if HotkeyInfo^.State <> hsSucceeded then Exit(False);
  Result := UnregisterHotkey(FMainForm.Handle, HotkeyID);
  HotkeyInfo^.State := hsNone;
  HotkeyInfo^.Hotkey.Value := 0;
end;

end.

