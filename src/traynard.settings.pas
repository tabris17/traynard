unit Traynard.Settings;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LazMethodList, Traynard.Types, Traynard.Storage;

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
    procedure SetValue(AValue: boolean);
  public
    constructor Create;
    property Value: boolean read FValue write SetValue;
  end;

  { TSettings }

  TSettings = class
  private
    FFirstRun: boolean;
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
    FListeners: array[TSettingsItem] of TMethodList;
    FConfig: TConfig;
    function GetAutorun: boolean;
    function GetHotkey(HotkeyID: THotkeyID): THotkey;
    procedure SetAutoMinimize(AValue: boolean);
    procedure SetAutorun(AValue: boolean);
    procedure SetHotkey(HotkeyID: THotkeyID; AValue: THotkey);
    procedure SetIconGrouped(AValue: boolean);
    procedure SetLanguage(AValue: string);
    procedure SetMenuGrouped(AValue: boolean);
    procedure SetRuleOnStartup(AValue: boolean);
    procedure SetShowNotification(AValue: boolean);
    procedure SetSystemMenuItems(AValue: TSystemMenuItems); 
    procedure SetApplyRules(AValue: boolean);
  public
    constructor Create;
    destructor Destroy; override;
    property FirstRun: boolean read FFirstRun;
    property Autorun: boolean read GetAutorun write SetAutorun;
    property Language: string read FLanguage write SetLanguage;
    property SystemMenuItems: TSystemMenuItems read FSystemMenuItems write SetSystemMenuItems;
    property IconGrouped: boolean read FIconGrouped write SetIconGrouped;
    property MenuGrouped: boolean read FMenuGrouped write SetMenuGrouped;
    property AutoMinimize: boolean read FAutoMinimize write SetAutoMinimize;
    property ApplyRules: boolean read FApplyRules write SetApplyRules;
    property ShowNotification: boolean read FShowNotification write SetShowNotification;
    property RuleOnStartup: boolean read FApplyRulesOnStartup write SetRuleOnStartup;
    property Hotkey[HotkeyID: THotkeyID]: THotkey read GetHotkey write SetHotkey;
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
  KEY_HOTKEYS = 'hotkeys';
  KEY_ADVANCE_AUTO_MINIMIZE = 'advance.enable_auto_minimize';
  KEY_ADVANCE_CUSTOM_RULES = 'advance.enable_custom_rules';
  KEY_ADVANCE_RULES_ON_STARTUP = 'advance.enable_rule_on_startup';
  KEY_ADVANCE_SYSTEM_MENU = 'advance.system_menu.';
  KEY_TRAY_ICON = 'enable_tray_icon';
  KEY_TRAY_MENU = 'enable_tray_menu';
  KEY_ALWAYS_ON_TOP = 'enable_always_on_top';

  REG_AUTORUN = 'SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\Run';

  SYSTEM_MENU_PAIRS: array of TSystemMenuPair = (
    (Key: KEY_TRAY_ICON; Item: smiTrayIcon),
    (Key: KEY_TRAY_MENU; Item: smiTrayMenu),
    (Key: KEY_ALWAYS_ON_TOP; Item: smiTopmost)
  );

  HOTKEY_NONE = 0;
  HOTKEY_DEFAULT = -1;

  DEFAULT_HOTKEY_VALUES: array[THotkeyID] of integer = (
    5898252, HOTKEY_NONE, HOTKEY_NONE, HOTKEY_NONE, HOTKEY_NONE, HOTKEY_NONE, HOTKEY_NONE, HOTKEY_NONE
  );

var
  Settings: TSettings = nil;

implementation

uses
  Windows, JwaWinReg, Traynard.Helpers, Traynard.Strings;

{ TSettings }

procedure TSettings.SetAutorun(AValue: boolean);
begin
  if FAutorun.Value = AValue then Exit;
  FAutorun.Value := AValue;
  FListeners[siAutorun].CallNotifyEvents(Self);
end;

procedure TSettings.SetHotkey(HotkeyID: THotkeyID; AValue: THotkey);
begin
  if FHotkeys[HotkeyID].Value = AValue.Value then Exit;
  FHotkeys[HotkeyID] := AValue;
  FConfig.SetIntegerArrayItem(KEY_HOTKEYS, Ord(HotkeyID), AValue.Value, HOTKEY_DEFAULT);
  FListeners[siHotkey].CallNotifyEvents(Self);
end;

procedure TSettings.SetAutoMinimize(AValue: boolean);
begin
  if FAutoMinimize = AValue then Exit;
  FAutoMinimize := AValue;
  FConfig.SetBoolean(KEY_ADVANCE_AUTO_MINIMIZE, AValue);
  FListeners[siAutoMinimize].CallNotifyEvents(Self);
end;

function TSettings.GetHotkey(HotkeyID: THotkeyID): THotkey;
begin
  Result := FHotkeys[HotkeyID];
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
  FListeners[siIconGrouped].CallNotifyEvents(Self);
end;

procedure TSettings.SetMenuGrouped(AValue: boolean);
begin
  if FMenuGrouped = AValue then Exit;
  FMenuGrouped := AValue;
  FConfig.SetBoolean(KEY_GENERAL_MENU_GROUPED, AValue);
  FListeners[siMenuGrouped].CallNotifyEvents(Self);
end;

procedure TSettings.SetLanguage(AValue: string);
begin
  if FLanguage = AValue then Exit;
  FLanguage := AValue;
  FConfig.SetString(KEY_GENERAL_LANGUAGE, AValue);
  FListeners[siLanguage].CallNotifyEvents(Self);
end;

procedure TSettings.SetRuleOnStartup(AValue: boolean);
begin
  if FApplyRulesOnStartup = AValue then Exit;
  FApplyRulesOnStartup := AValue;
  FConfig.SetBoolean(KEY_ADVANCE_RULES_ON_STARTUP, AValue);
  FListeners[siRuleOnStartup].CallNotifyEvents(Self);
end;

procedure TSettings.SetShowNotification(AValue: boolean);
begin
  if FShowNotification = AValue then Exit;
  FShowNotification := AValue;
  FConfig.SetBoolean(KEY_GENERAL_NOTIFICATION, AValue);
  FListeners[siShowNotification].CallNotifyEvents(Self);
end;

procedure TSettings.SetApplyRules(AValue: boolean);
begin
  if FApplyRules = AValue then Exit;
  FApplyRules := AValue;
  FConfig.SetBoolean(KEY_ADVANCE_CUSTOM_RULES, AValue);
  FListeners[siUseRules].CallNotifyEvents(Self);
end;

procedure TSettings.SetSystemMenuItems(AValue: TSystemMenuItems);
var
  SystemMenuPair: TSystemMenuPair;
begin
  if FSystemMenuItems = AValue then Exit;
  FSystemMenuItems := AValue;
  for SystemMenuPair in SYSTEM_MENU_PAIRS do
    FConfig.SetBoolean(KEY_ADVANCE_SYSTEM_MENU + SystemMenuPair.Key, SystemMenuPair.Item in AValue);
  FListeners[siSystemMenuItems].CallNotifyEvents(Self);
end;

constructor TSettings.Create;
var
  Item: TSettingsItem;
begin
  for Item := Low(TSettingsItem) to High(TSettingsItem) do
    FListeners[Item] := TMethodList.Create;
end;

destructor TSettings.Destroy;
var
  Item: TSettingsItem;
begin
  for Item := Low(TSettingsItem) to High(TSettingsItem) do
    FreeAndNil(FListeners[Item]);

  Storage.Save(CONFIG_NAME, FConfig);
  FreeAndNil(FConfig);
  FreeAndNil(FAutorun);

  inherited Destroy;
end;

procedure TSettings.Load;
var
  HotkeyID: THotkeyID;
  HotkeyValue, HotkeyDefaultValue: integer;
  SystemMenuPair: TSystemMenuPair;
begin
  FAutorun := TSettingAutorun.Create;
  FFirstRun := not Storage.Load(CONFIG_NAME, FConfig);
  FLanguage := FConfig.GetString(KEY_GENERAL_LANGUAGE);
  FIconGrouped := FConfig.GetBoolean(KEY_GENERAL_ICON_GROUPED, True);
  FMenuGrouped := FConfig.GetBoolean(KEY_GENERAL_MENU_GROUPED, True);
  FShowNotification := FConfig.GetBoolean(KEY_GENERAL_NOTIFICATION, True);
  for HotkeyID := Low(THotkeyID) to High(THotkeyID) do
  begin
    HotkeyDefaultValue := DEFAULT_HOTKEY_VALUES[HotkeyID];
    HotkeyValue := FConfig.GetIntegerArrayItem(KEY_HOTKEYS, Ord(HotkeyID), HotkeyDefaultValue);
    FHotkeys[HotkeyID].Value := specialize IfThen<integer>(HotkeyValue = HOTKEY_DEFAULT, HotkeyDefaultValue, HotkeyValue);
  end;
  FAutoMinimize := FConfig.GetBoolean(KEY_ADVANCE_AUTO_MINIMIZE, True);
  FApplyRules := FConfig.GetBoolean(KEY_ADVANCE_CUSTOM_RULES, True);
  FApplyRulesOnStartup := FConfig.GetBoolean(KEY_ADVANCE_RULES_ON_STARTUP, True);
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
begin
  FValue := ERROR_SUCCESS = RegGetValueW(
    HKEY_CURRENT_USER,
    unicodestring(REG_AUTORUN),
    unicodestring(APP_NAME),
    RRF_RT_REG_SZ,
    nil, nil, nil
  );
end;

procedure TSettingAutorun.SetValue(AValue: boolean);
var
  RegKey: HKEY;
  CommandLine: unicodestring;
begin
  if ERROR_SUCCESS <> RegOpenKeyW(HKEY_CURRENT_USER, unicodestring(REG_AUTORUN), RegKey) then Exit;
  if AValue then
  begin
    CommandLine := unicodestring(Format('"%s" -%s', [ParamStr(0), ARGUMENT_SILENT_CHAR]));
    RegSetValueExW(RegKey, unicodestring(APP_NAME), 0, REG_SZ, PByte(CommandLine), Length(CommandLine) * SizeOf(WideChar));
  end
  else
    RegDeleteValueW(RegKey, unicodestring(APP_NAME));
  RegCloseKey(RegKey);
  FValue := AValue;
end;

initialization

Settings := TSettings.Create;

finalization

FreeAndNil(Settings);

end.

