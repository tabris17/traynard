unit Traynard.Types;

{$mode ObjFPC}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, Windows, {$IFNDEF LIBRARY}Controls, JwaWinternl,{$ENDIF} Traynard.Strings;

type

  THookID = (hiCallWndProc, hiGetMessage);

  THotkeyID = (hiMinimizeToIcon = 1, hiMinimizeToMenu, hiRestoreLastWindow, hiRestoreAll,
               hiToggleTopmost, hiOpenClose, hiToggleAutoMinimize, hiToggleRules);

  THotkeyState = (hsNone, hsSucceeded, hsFailed);

  { THotkey }

  THotkey = record
    case Byte of
      0: (Value: LongWord);
      1: (Modifiers, Key: Word);
  end;

  PHotkey = ^THotkey;

  THotkeys = array[Ord(Low(THotkeyID))..Ord(High(THotkeyID))] of THotkey;

  THotkeyValues = array[Ord(Low(THotkeyID))..Ord(High(THotkeyID))] of integer;

  THotkeyDescriptions = array[Ord(Low(THotkeyID))..Ord(High(THotkeyID))] of string;

  { THotkeyInfo }

  THotkeyInfo = record
    Hotkey: THotkey;
    State: THotkeyState;
  end;

  PHotkeyInfo = ^THotkeyInfo;

  TTrayPosition = (tpMenu, tpIcon, tpGlobal);

  TSystemMenuItem = (smiSeparator, smiTrayIcon, smiTrayMenu, smiTopmost);

  TSystemMenuItems = set of TSystemMenuItem;

  TSettingsItem = (siAutorun, siIconGrouped, siMenuGrouped, siLanguage, siSystemMenuItems,
                   siAutoMinimize, siUseRules, siShowNotification, siRuleOnStartup, siHotkey,
                   siUseLauncher, siMultiprocessLaunch, siHighlightTopmost, siHighlightTopmostColor,
                   siHighlightTopmostThickness, siRunAsAdministrator);

  { TShellHookInfo }

  TShellHookInfo = record
    hwnd: HWND;
    rc: RECT;
  end;

  PShellHookInfo = ^TShellHookInfo;

  { TSystemMenuItemDetail }

  TSystemMenuItemDetail = record
    ID: UINT;
    Flag: UINT;
    Text: string;
  end;

  PSystemMenuItemDetail = ^TSystemMenuItemDetail;

  { TNullable }

  generic TNullable<T> = record
  type
    ENullReference = class(Exception);
  private
    FValue: T;
    FHasValue: boolean;
    function GetValue: T;
    procedure SetValue(AValue: T);
  public
    constructor Create(const AValue: T);
    property Value: T read GetValue write SetValue;
    property HasValue: boolean read FHasValue write FHasValue;
    function GetValue(const Default: T): T;
    class function Create: specialize TNullable<T>; static;
  end;

  TNullableString = specialize TNullable<string>;
  TNullableBoolean = specialize TNullable<boolean>;

  { TParamUnion }

  TParamUnion = record
    case Byte of
      0: (WParam: WPARAM);
      1: (LParam: LPARAM);
      2: (MenuItems: TSystemMenuItems);
  end;

  { ERuntimeException }

  ERuntimeException = class (Exception)
  private
    FTitle: string;
  public
    constructor Create(const Title, Msg: string); overload;
    property Title: string read FTitle;
  end;

  ERuntimeInfo = class (ERuntimeException);
  ERuntimeWarning = class (ERuntimeException);
  ERuntimeError = class (ERuntimeException);

  TPopupType = (ptWarning, ptError, ptInformation, ptConfirmation, ptShield, ptNone);

  THotkeyAddedNotify = procedure(const Hotkey: THotkey; out HotkeyID: longint) of object;

  THotkeyRemovedNotify = procedure(const HotkeyID: longint) of object;

  TWindowAction = (waCreation, waChange, waMinimizing, waHotkey, waDeactivated, waExisting);

  TRuleTriggerOn = set of TWindowAction;

  TRuleTextComparison = (rtcEquals, rtcContains, rtcStartsWith, rtcEndsWith, rtcRegexMatch, rtcAny);

  TRuleNotification = (rnNever, rnOnce, rnAlways, rnGlobal);

  { TRuleText }

  TRuleText = record
    Text: string;
    Comparison: TRuleTextComparison;
  end;

  TLaunchMethod = (lmAutomatic, lmManual, lmHotkey);

  TLaunchWindowAction = (lwaCreation, lwaMinimizing, lwaDeactivated);

  TLaunchTriggerOn = set of TLaunchWindowAction;

  TLaunchMethods = set of TLaunchMethod;

  THandleArray = specialize TArray<HWND>;

  TEditState = (esNone, esOpen, esNew);

  {$IFNDEF LIBRARY}

  { EFieldInvalid }

  EFieldInvalid = class(Exception)
  private
    FTitle: string;
    FControl: TControl;
  public
    constructor Create(const ATitle, AMessage: string; AControl: TControl);
    property Title: string read FTitle;
    property Control: TControl read FControl;
  end;

  { TProcessBasicInformation }

  TProcessBasicInformation = record
    ExitStatus: NTSTATUS;
    PebBaseAddress: PPEB;
    AffinityMask: ULONG_PTR;
    BasePriority: LONG;
    UniqueProcessId: ULONG_PTR;
    InheritedFromUniqueProcessId: ULONG_PTR;
  end;

  {$ENDIF}

const
  MOD_NOREPEAT = $4000;

  MAX_LONG_PATH = High(WORD) div 2;
  LOGO_PNG = 'LOGO_PNG';
  ABOUT_HTML = 'ABOUT_HTML';
  LICENSE_TXT = 'LICENSE_TXT';
  TASK_XML = 'TASK_XML';

  EVENT_OBJECT_CREATE = $8000;
  EVENT_OBJECT_DESTROY = $8001;
  EVENT_OBJECT_SHOW = $8002;
  EVENT_OBJECT_HIDE = $8003;
  EVENT_OBJECT_CLOAKED = $8017;
  EVENT_OBJECT_UNCLOAKED = $8018;
  EVENT_SYSTEM_MINIMIZESTART = $0016;

  HSHELL_WINDOWCREATED       = 1;
  HSHELL_WINDOWDESTROYED     = 2;
  HSHELL_ACTIVATESHELLWINDOW = 3;
  HSHELL_WINDOWACTIVATED     = 4;
  HSHELL_GETMINRECT          = 5;
  HSHELL_REDRAW              = 6;
  HSHELL_TASKMAN             = 7;
  HSHELL_LANGUAGE            = 8;
  HSHELL_SYSMENU             = 9;
  HSHELL_ENDTASK             = 10;
  HSHELL_ACCESSIBILITYSTATE  = 11;
  HSHELL_APPCOMMAND          = 12;
  HSHELL_WINDOWREPLACED      = 13;
  HSHELL_WINDOWREPLACING     = 14;
  HSHELL_MONITORCHANGED      = 16;
  HSHELL_FULLSCREENENTER     = 53;
  HSHELL_FULLSCREENEXIT      = 54;
  HSHELL_HIGHBIT             = $8000;
  HSHELL_FLASH               = HSHELL_REDRAW or HSHELL_HIGHBIT;
  HSHELL_RUDEAPPACTIVATED    = HSHELL_WINDOWACTIVATED or HSHELL_HIGHBIT;

  TOP_LEVEL_WINDOW_ERROR = $10000001;

  IDM_SYSTEM_SEPARATOR   = $DF00;
  IDM_SYSTEM_TRAY_ICON   = $DF01;
  IDM_SYSTEM_TRAY_MENU   = $DF02;
  IDM_SYSTEM_TOPMOST     = $DF03;
  IDM_SYSTEM_TOPMOST_CHECKED = $DF04;

  COLOR_MANAGED_WINDOW = $FFCCCC;

  ICON_SMALL2 = 2;
  DWMWA_CLOAKED = 14;

  RRF_RT_REG_SZ = 2;                 

  SYSTEM_MENU_LANG_DATA_TYPE = $21120903;
  SYSTEM_MENU_LANG_DATA_ITEMS = [smiTrayIcon, smiTrayMenu, smiTopmost];
  SYSTEM_MENU_LANG_DATA_MIN_SIZE = SizeOf(DWORD) * SizeOf(SYSTEM_MENU_LANG_DATA_ITEMS);

  SYSTEM_MENU_ITEM_DETAILS: array [TSystemMenuItem] of TSystemMenuItemDetail = (
    (ID: IDM_SYSTEM_SEPARATOR; Flag: MF_SEPARATOR; Text: '-'),
    (ID: IDM_SYSTEM_TRAY_ICON; Flag: MF_STRING; Text: MENU_ITEM_MINIMIZE_TO_TRAY_ICON),
    (ID: IDM_SYSTEM_TRAY_MENU; Flag: MF_STRING; Text: MENU_ITEM_MINIMIZE_TO_TRAY_MENU),
    (ID: IDM_SYSTEM_TOPMOST; Flag: MF_STRING; Text: MENU_ITEM_ALWAYS_ON_TOP)
  );

  HOTKEY_NONE = 0;

{$IFNDEF LIBRARY}
  HOTKEY_DESCRIPTIONS: THotkeyDescriptions = (
    HOTKEY_MINIMIZE_TO_ICON,
    HOTKEY_MINIMIZE_TO_MENU,
    HOTKEY_RESTORE_LAST_WINDOW,
    HOTKEY_RESTORE_ALL,
    HOTKEY_TOGGLE_TOPMOST,
    HOTKEY_OPEN_CLOSE,
    HOTKEY_TOGGLE_AUTOMINIMIZE,
    HOTKEY_TOGGLE_RULES
  );

  RULE_NOTIFICATIONS: array[TRuleNotification] of string = (
    TEXT_NEVER,
    TEXT_FIRST_TIME_ONLY,
    TEXT_ALWAYS,
    TEXT_FOLLOW_GLOBAL_SETTINGS
  );

  RULE_TRIGGER_ON_BEGIN = waCreation;
  RULE_TRIGGER_ON_END = waDeactivated;

  RULE_TRIGGER_ON: array[RULE_TRIGGER_ON_BEGIN..RULE_TRIGGER_ON_END] of string = (
    TEXT_WINDOW_CREATION,
    TEXT_WINDOW_TITLE_CHANGE,
    TEXT_WINDOW_MINIMIZING,
    TEXT_HOTKEY,
    TEXT_WINDOW_DEACTIVATED
  );

  RULE_COMPARISONS: array[0..5] of string = (
    TEXT_EQUALS, TEXT_CONTAINS, TEXT_STARTS_WITH,
    TEXT_ENDS_WITH, TEXT_REGEX_MATCH, TEXT_ANY
  );

  RULE_MINIMIZE_POSITIONS: array[TTrayPosition] of string = (
    TEXT_TRAY_MENU,
    TEXT_TRAY_ICON,
    TEXT_FOLLOW_GLOBAL_SETTINGS
  );

  OPTION_SYSTEM_MENUS: array[0..2] of string = (
    TEXT_SYSTEM_MENU_TRAY_ICON,
    TEXT_SYSTEM_MENU_TRAY_MENU,
    TEXT_SYSTEM_MENU_TOPMOST
  );

  LAUNCH_METHODS: array[TLaunchMethod] of string = (
    TEXT_AUTOMATIC,
    TEXT_MANUAL,
    TEXT_HOTKEY
  );

  LAUNCH_TRIGGER_ON: array[lwaCreation..lwaDeactivated] of string = (
    TEXT_WINDOW_CREATION,
    TEXT_WINDOW_MINIMIZING,
    TEXT_WINDOW_DEACTIVATED
  );
{$ENDIF}

implementation

{ TNullable }

function TNullable.GetValue: T;
begin
  if FHasValue then
    Result := FValue
  else
    raise ENullReference.Create('Null reference');
end;

procedure TNullable.SetValue(AValue: T);
begin
  FValue := AValue;
  FHasValue := True;
end;

constructor TNullable.Create(const AValue: T);
begin
  SetValue(AValue);
end;

function TNullable.GetValue(const Default: T): T;
begin
  Result := specialize IfThen<T>(FHasValue, FValue, Default);
end;

class function TNullable.Create: specialize TNullable<T>;
begin
  Result.FHasValue := False;
end;

{ ERuntimeException }

constructor ERuntimeException.Create(const Title, Msg: string);
begin
  inherited Create(Msg);
  FTitle := Title;
end;

{$IFNDEF LIBRARY}

{ EFieldInvalid }

constructor EFieldInvalid.Create(const ATitle, AMessage: string; AControl: TControl);
begin
  inherited Create(AMessage);
  FTitle := ATitle;
  FControl := AControl;
end;

{$ENDIF}

end.

