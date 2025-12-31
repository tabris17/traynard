unit Traynard.Settings;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, LazMethodList, Traynard.Types, Traynard.Storage;

type

  { TSystemMenuPair }

  TSystemMenuPair = record
    Key: string;
    Item: TSystemMenuItem;
  end;

  { TSettingAutorun }

  TSettingAutorun = class
  private
    FValue: boolean;
    FRunAsAdministrator: boolean;
    procedure SetRunAsAdministrator(AValue: boolean);
    procedure SetValue(AValue: boolean);
  public
    constructor Create;
    property Value: boolean read FValue write SetValue;
    property RunAsAdministrator: boolean read FRunAsAdministrator write SetRunAsAdministrator;
  end;

  { TSettings }

  TSettings = class
  private
    FAutorun: TSettingAutorun;
    FHotkeys: THotkeys;
    FIconGrouped: boolean;
    FMenuGrouped: boolean;
    FSystemMenuItems: TSystemMenuItems;
    FLanguage: string;
    FAutoMinimize: boolean;
    FApplyRules: boolean;
    FShowNotification: boolean;
    FApplyRulesOnStartup: boolean;
    FEnableLauncher: boolean;
    FLauncherMultiprocess: boolean;
    FHighlightTopmost: boolean;
    FHighlightTopmostColor: TColor;
    FHighlightTopmostThickness: byte;
    FListeners: array[TSettingsItem] of TMethodList;
    FConfig: TConfig;
    function GetAutorun: boolean;
    function GetHotkey(HotkeyID: THotkeyID): THotkey;
    function GetRunAsAdministrator: boolean;
    procedure SetEnableLauncher(AValue: boolean);
    procedure SetAutoMinimize(AValue: boolean);
    procedure SetAutorun(AValue: boolean);
    procedure SetRunAsAdministrator(AValue: boolean);
    procedure SetHighlightTopmost(AValue: boolean);
    procedure SetHighlightTopmostColor(AValue: TColor);
    procedure SetHighlightTopmostThickness(AValue: byte);
    procedure SetHotkey(HotkeyID: THotkeyID; AValue: THotkey);
    procedure SetIconGrouped(AValue: boolean);
    procedure SetLanguage(AValue: string);
    procedure SetLauncherMultiprocess(AValue: boolean);
    procedure SetMenuGrouped(AValue: boolean);
    procedure SetRuleOnStartup(AValue: boolean);
    procedure SetShowNotification(AValue: boolean);
    procedure SetSystemMenuItems(AValue: TSystemMenuItems); 
    procedure SetApplyRules(AValue: boolean);
  public
    constructor Create;
    destructor Destroy; override;
    property Autorun: boolean read GetAutorun write SetAutorun;
    property RunAsAdministrator: boolean read GetRunAsAdministrator write SetRunAsAdministrator;
    property Language: string read FLanguage write SetLanguage;
    property SystemMenuItems: TSystemMenuItems read FSystemMenuItems write SetSystemMenuItems;
    property IconGrouped: boolean read FIconGrouped write SetIconGrouped;
    property MenuGrouped: boolean read FMenuGrouped write SetMenuGrouped;
    property AutoMinimize: boolean read FAutoMinimize write SetAutoMinimize;
    property ApplyRules: boolean read FApplyRules write SetApplyRules;
    property ShowNotification: boolean read FShowNotification write SetShowNotification;
    property RuleOnStartup: boolean read FApplyRulesOnStartup write SetRuleOnStartup;
    property EnableLauncher: boolean read FEnableLauncher write SetEnableLauncher;
    property LauncherMultiprocess: boolean read FLauncherMultiprocess write SetLauncherMultiprocess;
    property Hotkey[HotkeyID: THotkeyID]: THotkey read GetHotkey write SetHotkey;
    property HighlightTopmost: boolean read FHighlightTopmost write SetHighlightTopmost;
    property HighlightTopmostColor: TColor read FHighlightTopmostColor write SetHighlightTopmostColor;
    property HighlightTopmostThickness: byte read FHighlightTopmostThickness write SetHighlightTopmostThickness;
    procedure Load;
    procedure AddListener(const Item: TSettingsItem; const Listener: TNotifyEvent);
    procedure RemoveListener(const Item: TSettingsItem; const Listener: TNotifyEvent);
    procedure RemoveListeners(const Item: TSettingsItem; const Listener: TObject);
    procedure RemoveListeners(const Listener: TObject); overload;
  end;

const
  CONFIG_NAME = 'settings';
  KEY_GENERAL_LANGUAGE = 'general.language';
  KEY_GENERAL_ICON_GROUPED = 'general.enable_icon_grouped';
  KEY_GENERAL_MENU_GROUPED = 'general.enable_menu_grouped';
  KEY_GENERAL_NOTIFICATION = 'general.enable_notification';
  KEY_GENERAL_HIGHLIGHT_TOPMOST = 'general.highlight_topmost';
  KEY_GENERAL_HIGHLIGHT_TOPMOST_ENABLED = KEY_GENERAL_HIGHLIGHT_TOPMOST + '.enabled';
  KEY_GENERAL_HIGHLIGHT_TOPMOST_COLOR = KEY_GENERAL_HIGHLIGHT_TOPMOST + '.color'; 
  KEY_GENERAL_HIGHLIGHT_TOPMOST_THICKNESS = KEY_GENERAL_HIGHLIGHT_TOPMOST + '.thickness';
  KEY_HOTKEYS = 'hotkeys';
  KEY_ADVANCE_AUTO_MINIMIZE = 'advance.enable_auto_minimize';
  KEY_ADVANCE_CUSTOM_RULES = 'advance.enable_custom_rules';
  KEY_ADVANCE_RULES_ON_STARTUP = 'advance.enable_rule_on_startup';
  KEY_ADVANCE_SYSTEM_MENU = 'advance.system_menu.';
  KEY_TRAY_ICON = 'enable_tray_icon';
  KEY_TRAY_MENU = 'enable_tray_menu';
  KEY_ALWAYS_ON_TOP = 'enable_always_on_top';
  KEY_ADVANCE_LAUNCHER = 'advance.launcher';
  KEY_ADVANCE_LAUNCHER_ENABLED = KEY_ADVANCE_LAUNCHER + '.enabled';
  KEY_ADVANCE_LAUNCHER_MULTIPROCESS = KEY_ADVANCE_LAUNCHER + '.multiprocess';

  REG_AUTORUN = 'SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\Run';

  SYSTEM_MENU_PAIRS: array of TSystemMenuPair = (
    (Key: KEY_TRAY_ICON; Item: smiTrayIcon),
    (Key: KEY_TRAY_MENU; Item: smiTrayMenu),
    (Key: KEY_ALWAYS_ON_TOP; Item: smiTopmost)
  );

  HOTKEY_NONE = 0;
  HOTKEY_DEFAULT = -1;

  DEFAULT_HOTKEY_VALUES: THotkeyValues = (
    5898252, HOTKEY_NONE, HOTKEY_NONE, HOTKEY_NONE, HOTKEY_NONE, HOTKEY_NONE, HOTKEY_NONE, HOTKEY_NONE
  );

  DEFAULT_HIGHLIGHT_TOPMOST_THICKNESS = 10;

var
  Settings: TSettings = nil;

implementation

uses
  Windows, JwaWinReg, ShellApi, StrUtils, Traynard.Helpers, Traynard.Strings;

{ TSettings }

procedure TSettings.SetAutorun(AValue: boolean);
begin
  if FAutorun.Value = AValue then Exit;
  FAutorun.Value := AValue;
  FListeners[siAutorun].CallNotifyEvents(Self);
end;

procedure TSettings.SetRunAsAdministrator(AValue: boolean);
begin
  if FAutorun.RunAsAdministrator = AValue then Exit;
  FAutorun.RunAsAdministrator := AValue;
  FListeners[siRunAsAdministrator].CallNotifyEvents(Self);
end;

procedure TSettings.SetHighlightTopmost(AValue: boolean);
begin
  if FHighlightTopmost = AValue then Exit;
  FHighlightTopmost := AValue;
  FConfig.SetBoolean(KEY_GENERAL_HIGHLIGHT_TOPMOST_ENABLED, AValue);
  Storage.SaveConfig(CONFIG_NAME, FConfig);
  FListeners[siHighlightTopmost].CallNotifyEvents(Self);
end;

procedure TSettings.SetHighlightTopmostColor(AValue: TColor);
begin
  if FHighlightTopmostColor = AValue then Exit;
  FHighlightTopmostColor := AValue;
  FConfig.SetInteger(KEY_GENERAL_HIGHLIGHT_TOPMOST_COLOR, AValue);
  Storage.SaveConfig(CONFIG_NAME, FConfig);
  FListeners[siHighlightTopmostColor].CallNotifyEvents(Self);
end;

procedure TSettings.SetHighlightTopmostThickness(AValue: byte);
begin
  if FHighlightTopmostThickness = AValue then Exit;
  FHighlightTopmostThickness := AValue;
  FConfig.SetInteger(KEY_GENERAL_HIGHLIGHT_TOPMOST_THICKNESS, AValue);
  Storage.SaveConfig(CONFIG_NAME, FConfig);
  FListeners[siHighlightTopmostThickness].CallNotifyEvents(Self);
end;

procedure TSettings.SetHotkey(HotkeyID: THotkeyID; AValue: THotkey);
begin
  if FHotkeys[Ord(HotkeyID)].Value = AValue.Value then Exit;
  FHotkeys[Ord(HotkeyID)] := AValue;
  FConfig.SetIntegerArrayItem(KEY_HOTKEYS, specialize EnumToIndex<THotkeyID>(HotkeyID), AValue.Value, HOTKEY_DEFAULT);
  Storage.SaveConfig(CONFIG_NAME, FConfig);
  FListeners[siHotkey].CallNotifyEvents(Self);
end;

procedure TSettings.SetAutoMinimize(AValue: boolean);
begin
  if FAutoMinimize = AValue then Exit;
  FAutoMinimize := AValue;
  FConfig.SetBoolean(KEY_ADVANCE_AUTO_MINIMIZE, AValue);
  Storage.SaveConfig(CONFIG_NAME, FConfig);
  FListeners[siAutoMinimize].CallNotifyEvents(Self);
end;

function TSettings.GetHotkey(HotkeyID: THotkeyID): THotkey;
begin
  Result := FHotkeys[Ord(HotkeyID)];
end;

function TSettings.GetRunAsAdministrator: boolean;
begin
  Result := FAutorun.RunAsAdministrator;
end;

function TSettings.GetAutorun: boolean;
begin
  Result := FAutorun.Value;
end;

procedure TSettings.SetIconGrouped(AValue: boolean);
begin
  if FIconGrouped = AValue then Exit;
  FIconGrouped := AValue;
  FConfig.SetBoolean(KEY_GENERAL_ICON_GROUPED, AValue);
  Storage.SaveConfig(CONFIG_NAME, FConfig);
  FListeners[siIconGrouped].CallNotifyEvents(Self);
end;

procedure TSettings.SetMenuGrouped(AValue: boolean);
begin
  if FMenuGrouped = AValue then Exit;
  FMenuGrouped := AValue;
  FConfig.SetBoolean(KEY_GENERAL_MENU_GROUPED, AValue);
  Storage.SaveConfig(CONFIG_NAME, FConfig);
  FListeners[siMenuGrouped].CallNotifyEvents(Self);
end;

procedure TSettings.SetLanguage(AValue: string);
begin
  if FLanguage = AValue then Exit;
  FLanguage := AValue;
  FConfig.SetString(KEY_GENERAL_LANGUAGE, AValue);
  Storage.SaveConfig(CONFIG_NAME, FConfig);
  FListeners[siLanguage].CallNotifyEvents(Self);
end;

procedure TSettings.SetLauncherMultiprocess(AValue: boolean);
begin
  if FLauncherMultiprocess = AValue then Exit;
  FLauncherMultiprocess := AValue;
  FConfig.SetBoolean(KEY_ADVANCE_LAUNCHER_MULTIPROCESS, AValue);
  Storage.SaveConfig(CONFIG_NAME, FConfig);
  FListeners[siMultiprocessLaunch].CallNotifyEvents(Self);
end;

procedure TSettings.SetRuleOnStartup(AValue: boolean);
begin
  if FApplyRulesOnStartup = AValue then Exit;
  FApplyRulesOnStartup := AValue;
  FConfig.SetBoolean(KEY_ADVANCE_RULES_ON_STARTUP, AValue);
  Storage.SaveConfig(CONFIG_NAME, FConfig);
  FListeners[siRuleOnStartup].CallNotifyEvents(Self);
end;

procedure TSettings.SetShowNotification(AValue: boolean);
begin
  if FShowNotification = AValue then Exit;
  FShowNotification := AValue;
  FConfig.SetBoolean(KEY_GENERAL_NOTIFICATION, AValue);
  Storage.SaveConfig(CONFIG_NAME, FConfig);
  FListeners[siShowNotification].CallNotifyEvents(Self);
end;

procedure TSettings.SetApplyRules(AValue: boolean);
begin
  if FApplyRules = AValue then Exit;
  FApplyRules := AValue;
  FConfig.SetBoolean(KEY_ADVANCE_CUSTOM_RULES, AValue);
  Storage.SaveConfig(CONFIG_NAME, FConfig);
  FListeners[siUseRules].CallNotifyEvents(Self);
end;

procedure TSettings.SetEnableLauncher(AValue: boolean);
begin
  if FEnableLauncher = AValue then Exit;
  FEnableLauncher := AValue;
  FConfig.SetBoolean(KEY_ADVANCE_LAUNCHER_ENABLED, AValue);
  Storage.SaveConfig(CONFIG_NAME, FConfig);
  FListeners[siUseLauncher].CallNotifyEvents(Self);
end;

procedure TSettings.SetSystemMenuItems(AValue: TSystemMenuItems);
var
  SystemMenuPair: TSystemMenuPair;
begin
  if smiSeparator in AValue then Exclude(AValue, smiSeparator);
  if FSystemMenuItems = AValue then Exit;
  FSystemMenuItems := AValue;
  for SystemMenuPair in SYSTEM_MENU_PAIRS do
    FConfig.SetBoolean(KEY_ADVANCE_SYSTEM_MENU + SystemMenuPair.Key, SystemMenuPair.Item in AValue);
  Storage.SaveConfig(CONFIG_NAME, FConfig);
  FListeners[siSystemMenuItems].CallNotifyEvents(Self);
end;

constructor TSettings.Create;
var
  Item: TSettingsItem;
begin
  for Item := Low(TSettingsItem) to High(TSettingsItem) do
    FListeners[Item] := TMethodList.Create;

  FAutorun := TSettingAutorun.Create;
end;

destructor TSettings.Destroy;
var
  Item: TSettingsItem;
begin
  for Item := Low(TSettingsItem) to High(TSettingsItem) do
    FreeAndNil(FListeners[Item]);

  if Assigned(FConfig) then
  begin
    Storage.SaveConfig(CONFIG_NAME, FConfig);
    FreeAndNil(FConfig);
  end;

  FreeAndNil(FAutorun);

  inherited Destroy;
end;

procedure TSettings.Load;
var
  HotkeyID: THotkeyID;
  HotkeyValue, HotkeyDefaultValue: integer;
  SystemMenuPair: TSystemMenuPair;
begin
  if Assigned(FConfig) then
    raise Exception.Create('Duplicate settings loading');

  Storage.LoadConfig(CONFIG_NAME, FConfig);
  FLanguage := FConfig.GetString(KEY_GENERAL_LANGUAGE);
  FIconGrouped := FConfig.GetBoolean(KEY_GENERAL_ICON_GROUPED, True);
  FMenuGrouped := FConfig.GetBoolean(KEY_GENERAL_MENU_GROUPED, True);
  FShowNotification := FConfig.GetBoolean(KEY_GENERAL_NOTIFICATION, True);
  FHighlightTopmost := FConfig.GetBoolean(KEY_GENERAL_HIGHLIGHT_TOPMOST_ENABLED, False);
  FHighlightTopmostColor := TColor(FConfig.GetInteger(KEY_GENERAL_HIGHLIGHT_TOPMOST_COLOR, clHighlight));
  FHighlightTopmostThickness := Byte(FConfig.GetInteger(KEY_GENERAL_HIGHLIGHT_TOPMOST_THICKNESS, DEFAULT_HIGHLIGHT_TOPMOST_THICKNESS));
  for HotkeyID := Low(THotkeyID) to High(THotkeyID) do
  begin
    HotkeyDefaultValue := DEFAULT_HOTKEY_VALUES[Ord(HotkeyID)];
    HotkeyValue := FConfig.GetIntegerArrayItem(KEY_HOTKEYS, specialize EnumToIndex<THotkeyID>(HotkeyID), HotkeyDefaultValue);
    FHotkeys[Ord(HotkeyID)].Value := specialize IfThen<integer>(HotkeyValue = HOTKEY_DEFAULT, HotkeyDefaultValue, HotkeyValue);
  end;
  FAutoMinimize := FConfig.GetBoolean(KEY_ADVANCE_AUTO_MINIMIZE, True);
  FApplyRules := FConfig.GetBoolean(KEY_ADVANCE_CUSTOM_RULES, True);
  FApplyRulesOnStartup := FConfig.GetBoolean(KEY_ADVANCE_RULES_ON_STARTUP, True);
  FEnableLauncher := FConfig.GetBoolean(KEY_ADVANCE_LAUNCHER_ENABLED, False);
  FLauncherMultiprocess := FConfig.GetBoolean(KEY_ADVANCE_LAUNCHER_MULTIPROCESS, False);
  FSystemMenuItems := [];
  for SystemMenuPair in SYSTEM_MENU_PAIRS do
  begin
    if FConfig.GetBoolean(KEY_ADVANCE_SYSTEM_MENU + SystemMenuPair.Key, False) then
      Include(FSystemMenuItems, SystemMenuPair.Item);
  end;
end;

procedure TSettings.AddListener(const Item: TSettingsItem; const Listener: TNotifyEvent);
begin
  FListeners[Item].Add(TMethod(Listener), False);
end;

procedure TSettings.RemoveListener(const Item: TSettingsItem; const Listener: TNotifyEvent);
begin
  FListeners[Item].Remove(TMethod(Listener));
end;

procedure TSettings.RemoveListeners(const Item: TSettingsItem; const Listener: TObject);
begin
  FListeners[Item].RemoveAllMethodsOfObject(Listener);
end;

procedure TSettings.RemoveListeners(const Listener: TObject);
var
  Item: TSettingsItem;
begin
  for Item := Low(TSettingsItem) to High(TSettingsItem) do
    FListeners[Item].RemoveAllMethodsOfObject(Listener);
end;

{ TSettingAutorun }

constructor TSettingAutorun.Create;
var
  AutorunKey, AppName: unicodestring;
  DataSize: DWORD = 0;
  CommandLine: unicodestring = '';
begin
  AutorunKey := unicodestring(REG_AUTORUN);
  AppName := unicodestring(APP_NAME);
  FValue := ERROR_SUCCESS = RegGetValueW(HKEY_CURRENT_USER,
                                         PWideChar(AutorunKey), PWideChar(AppName),
                                         RRF_RT_REG_SZ, nil, nil, @DataSize);
  SetLength(CommandLine, DataSize div SizeOf(WideChar));
  if ERROR_SUCCESS = RegGetValueW(HKEY_CURRENT_USER,
                                  PWideChar(AutorunKey), PWideChar(AppName),
                                  RRF_RT_REG_SZ, nil,
                                  PWideChar(CommandLine), @DataSize) then
  begin
    SetLength(CommandLine, DataSize div SizeOf(WideChar) - 1);
    FRunAsAdministrator := EndsStr('--' + ARGUMENT_RUN_TASK, string(CommandLine));
  end
  else
    FRunAsAdministrator := False;
end;

procedure TSettingAutorun.SetValue(AValue: boolean);
var
  RegKey: HKEY;
  CommandLine: unicodestring;
begin
  if ERROR_SUCCESS <> RegOpenKeyW(HKEY_CURRENT_USER, unicodestring(REG_AUTORUN), RegKey) then Exit;
  if AValue then
  begin
    CommandLine := unicodestring(specialize IfThen<string>(
      FRunAsAdministrator,
      Format('"%s" --%s', [ParamStr(0), ARGUMENT_RUN_TASK]),
      Format('"%s" -%s', [ParamStr(0), ARGUMENT_SILENT_CHAR])
    ));
    RegSetValueExW(RegKey, unicodestring(APP_NAME), 0, REG_SZ, PByte(CommandLine), Length(CommandLine) * SizeOf(WideChar));
  end
  else
    RegDeleteValueW(RegKey, unicodestring(APP_NAME));
  RegCloseKey(RegKey);
  FValue := AValue;
end;

procedure TSettingAutorun.SetRunAsAdministrator(AValue: boolean);
var
  FilePath, Parameters, Directory: unicodestring;
begin
  FilePath := unicodestring(ParamStr(0));
  Parameters := unicodestring('--' + specialize IfThen<string>(AValue, ARGUMENT_CREATE_TASK, ARGUMENT_REMOVE_TASK));
  Directory := unicodestring(GetCurrentDir);
  if ShellExecuteW(0, 'runas', PWideChar(FilePath), PWideChar(Parameters), PWideChar(Directory), SW_NORMAL) > 32 then
  begin
    FRunAsAdministrator := AValue;
    SetValue(True);
  end;
end;

initialization

Settings := TSettings.Create;

finalization

FreeAndNil(Settings);

end.

