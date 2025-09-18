unit Traynard.Window;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Menus, Controls, ExtCtrls, Windows, JwaWinAble, LMessages, Generics.Collections,
  Traynard.Types, Traynard.Strings;

type

  { TWindow }

  TWindow = class
  private
    FHandle: HWND;
    FPID: DWORD;
    FExStyle: LONG;
    FStyle: LONG;
    FAppPath: TNullableString;
    FText: TNullableString;
    FClassName: TNullableString;
    FIcon: HICON;
    FSystemMenu: HMENU;
    FShowInTaskBar: TNullableBoolean;
    FCanAddToTaskBar: TNullableBoolean;
    function GetClassName: string;
    function GetExStyle: LONG;
    function GetIcon: HICON;
    function GetAppPath: string;
    function GetPID: DWORD;
    function GetStyle: LONG;
    function GetText: string;
    function GetSystemMenu: HMENU;
  protected
    procedure Init; virtual;
  public
    constructor Create(AHandle: HWND);
    property Handle: HWND read FHandle;
    property PID: DWORD read GetPID;
    property Text: string read GetText;
    property AppPath: string read GetAppPath;
    property ClassName: string read GetClassName;
    property Style: LONG read GetStyle;
    property ExStyle: LONG read GetExStyle;
    property Icon: HICON read GetIcon;
    property SystemMenu: HMENU read GetSystemMenu;
    procedure Renew; virtual;
    function ShowInTaskBar: boolean;
    function CanAddToTaskBar: boolean;
    function IsUWP: boolean;
    function IsImmersiveShellWindow: boolean;
  end;

  { TDesktopWindow }

  TDesktopWindow = class(TWindow)
  private
    FRestored: boolean;
  public
    property Restored: boolean read FRestored;
    constructor Create(AHandle: HWND; ARestored: boolean);
  end;

  { TTrayWindow }

  TTrayWindow = class (TWindow)
  private
    FPosition: TTrayPosition;
  public
    property Position: TTrayPosition read FPosition;
    constructor Create(AHandle: HWND; APosition: TTrayPosition);
  end;

  { TWindowCollection }

  TWindowCollection = class(TPersistent)
  type
    TWindowFilter = function (Window: TWindow): Boolean of object;
    TOrderedWindowList = specialize TList<HWND>;
    TWindowEnumerator = specialize TEnumerator<TWindow>;
  private
    FWindows: specialize TObjectDictionary<HWND, TWindow>;
    FOrderedWindowList: TOrderedWindowList;
    function GetLastWindow: TWindow;
    function GetWindowCount: integer;
  protected
    function DoGetEnumerator: TWindowEnumerator; virtual;
    procedure WindowNotifyEvent(ASender: TObject; constref AItem: HWND; AAction: TCollectionNotification); virtual;
  public
    constructor Create;
    destructor Destroy; override;
    function GetEnumerator: TWindowEnumerator;
    function Find(Handle: HWND; out Window: TWindow): boolean;
    property LastWindow: TWindow read GetLastWindow;
    property WindowCount: integer read GetWindowCount;
  end;

  { TDesktopWindowCollection }

  TDesktopWindowCollection = class(TWindowCollection)
  type

    { TShowInTaskBarEnumerator }

    TShowInTaskBarEnumerator = class(TWindowEnumerator)
    private
      FEnumerator: TWindowEnumerator;
    public
      constructor Create(AEnumerator: TWindowEnumerator);
      destructor Destroy; override;
      function DoGetCurrent: TWindow; override;
      function DoMoveNext: boolean; override;
    end;
  protected
    function DoGetEnumerator: TWindowEnumerator; override;
  end;

  { TTrayWindowCollection }

  TTrayWindowCollection = class(TWindowCollection)

  end;

  { TWindowManager }

  TWindowManager = class(TComponent)
  type

    { Exception }

    Exception = class(SysUtils.Exception);

    TEventHookType = (ehtCloaked, ehtMinimize, ehtDestroy);
  private
    FSelf: TWindowManager; static;
    FMainForm: TForm;
    FCurrentPID: DWORD;
    FSystemMenuMessage: UINT;
    FDesktop: TDesktopWindowCollection;
    FTray: TTrayWindowCollection;
    FSystemMenuItems: TSystemMenuItems;
    FEventHooks: array[TEventHookType] of HWINEVENTHOOK;
    FHookInstalled: boolean;
    FOriginalWindowProc: TWndMethod;
    FAutoMinimizeWindows: specialize TDictionary<HWND, TTrayPosition>;
    FRestoredWindows: specialize TList<HWND>;
    function GetAutoMinimize: boolean;
    procedure SetAutoMinimize(AValue: boolean);
    procedure SetSystemMenuItems(AValue: TSystemMenuItems);
    procedure SetWindowSystemMenu(Window: TWindow; Items: TSystemMenuItems = []);
    procedure SetSystemMenuLanguage;
    procedure InstallHook;
    procedure UninstallHook;
    procedure WindowProc(var TheMessage: TLMessage);
    procedure ShellHookProc(var TheMessage: TLMessage);
    procedure AutoMinimizeChanged(Sender: TObject);
    procedure SystemMenuItemsChanged(Sender: TObject);
    procedure LanguageChanged(Sender: TObject);
    procedure TrayChanged;
    function FindWindowOnDesktop(Handle: HWND): TWindow;
    function AddWindow(const Handle: HWND): boolean;
    function RemoveWindow(const Handle: HWND): boolean;
    function UpdateWindow(const Handle: HWND): boolean;
  private
    class function EnumAndMinimizeWindowsProc(Handle: HWND; Param: LPARAM): WINBOOL; stdcall; static;
    class function EnumWindowsProc(Handle: HWND; Param: LPARAM): WINBOOL; stdcall; static;
    class function MinimizeOwnedWindowsProc(Handle: HWND; Param: LPARAM): WINBOOL; stdcall; static;
    class procedure WinEventProc(hEventHook: HWINEVENTHOOK; event: DWORD; hwnd: HWND; idObject: LONG; idChild: LONG;
      idEventThread: DWORD; dwmsEventTime: DWORD); stdcall; static;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property SystemMenuItems: TSystemMenuItems read FSystemMenuItems write SetSystemMenuItems;
    property SystemMenuMessage: UINT read FSystemMenuMessage;
    property Desktop: TDesktopWindowCollection read FDesktop;
    property Tray: TTrayWindowCollection read FTray;
    property AutoMinimize: boolean read GetAutoMinimize write SetAutoMinimize;
    procedure RefreshDesktop;
    procedure EnableWindowAutoMinimize(const Handle: HWND; const Position: TTrayPosition);
    procedure DisableWindowAutoMinimize(const Handle: HWND);
    procedure MinimizeWindow(const Handle: HWND; const Position: TTrayPosition; out Window: TWindow);
    procedure MinimizeWindow(const Handle: HWND; const Position: TTrayPosition); overload;
    procedure RestoreWindow(const Handle: HWND);
    function TryRestoreAllWindows: integer;
    function IsAutoMinimizeWindow(const Handle: HWND; out TrayPosition: TTrayPosition): boolean;
    function IsAutoMinimizeWindow(const Handle: HWND): boolean; overload;
    function TryMinimizeWindow(const Handle: HWND; const Position: TTrayPosition; out Window: TWindow): boolean;
    function TryMinimizeWindow(const Handle: HWND; const Position: TTrayPosition): boolean;
    function TryRestoreWindow(const Handle: HWND): boolean;
    function TryRestoreLastWindow: boolean;
  end;

var
  WindowManager: TWindowManager = nil;

implementation

uses
  LazLogger, LazFileUtils, JwaPsApi, DwmApi, Graphics,
  Traynard.Helpers, Traynard.Settings, Traynard.Rule, Traynard.Notification, Traynard.Session;

var
  WM_SHELLHOOKMESSAGE: LONG;

{ TWindow }

function TWindow.GetText: string;
var
  TextStr: UnicodeString = '';
  TextLen: LongInt;
begin
  if not FText.HasValue then
  begin
    TextLen := GetWindowTextLengthW(FHandle);
    SetLength(TextStr, TextLen);
    GetWindowTextW(FHandle, PWideChar(TextStr), TextLen + 1);
    FText.Value := string(TextStr);
  end;
  Result := FText.Value;
end;

function TWindow.GetSystemMenu: HMENU;
begin
  if FSystemMenu = 0 then FSystemMenu := Windows.GetSystemMenu(Handle, False);
  if (FSystemMenu <> 0) and not IsMenu(FSystemMenu) then
  begin
    Windows.GetSystemMenu(Handle, True);
    FSystemMenu := Windows.GetSystemMenu(Handle, False);
  end;
  Result := FSystemMenu;
end;

function TWindow.IsImmersiveShellWindow: boolean;
begin
  Result := False;
  if (ClassName = 'ApplicationFrameWindow') or
     (ClassName = 'Windows.UI.Core.CoreWindow') or
     (ClassName = 'StartMenuSizingFrame') or
     (ClassName = 'Shell_LightDismissOverlay') then
  begin
    if ExStyle and WS_EX_WINDOWEDGE = 0 then Exit(True);
  end
  else if TWinVer.IsWin10OrLater and (
            (ClassName = 'ImmersiveBackgroundWindow') or
            (ClassName = 'SearchPane') or
            (ClassName = 'NativeHWNDHost') or
            (ClassName = 'Shell_CharmWindow') or
            (ClassName = 'ImmersiveLauncher')
          ) then
    Exit(True);
end;

procedure TWindow.Init;
begin
  FPID := 0;
  FStyle := -1;
  FExStyle := -1;
  FAppPath.HasValue := False;
  FText.HasValue := False;
  FClassName.HasValue := False;
  FIcon := 0;
  FSystemMenu := 0;
  FShowInTaskBar.HasValue := False;
  FCanAddToTaskBar.HasValue := False;
end;

function TWindow.GetAppPath: string;
var
  Process: Windows.Handle;
  PathStr: UnicodeString = '';
begin
  if not FAppPath.HasValue then
  begin
    SetLength(PathStr, MAX_LONG_PATH);
    Process := OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ, False, PID);
    try
      SetLength(PathStr, GetModuleFileNameExW(Process, 0, PWideChar(PathStr), MAX_LONG_PATH));
    finally
      CloseHandle(Process);
    end;
    FAppPath.Value := string(PathStr);
  end;
  Result := FAppPath.Value;
end;

function TWindow.GetPID: DWORD;
begin
  if FPID = 0 then GetWindowThreadProcessId(FHandle, FPID);
  Result := FPID;
end;

function TWindow.GetStyle: LONG;
begin
  if FStyle = -1 then FStyle := GetWindowLong(Handle, GWL_STYLE);
  Result := FStyle;
end;

function TWindow.GetClassName: string;
var
  ClassStr: UnicodeString = '';
  ClassLen: integer;
begin
  if not FClassName.HasValue then
  begin
    ClassLen := High(Byte);
    SetLength(ClassStr, ClassLen);
    SetLength(ClassStr, GetClassNameW(FHandle, PWideChar(ClassStr), ClassLen));
    FClassName.Value := string(ClassStr);
  end;
  Result := FClassName.Value;
end;

function TWindow.GetExStyle: LONG;
begin
  if FExStyle = -1 then FExStyle := GetWindowLong(Handle, GWL_EXSTYLE);
  Result := FExStyle;
end;

function TWindow.GetIcon: HICON;
begin
  if FIcon = 0 then
  begin
    FIcon := HICON(SendMessageW(FHandle, WM_GETICON, ICON_BIG, 0));
    if FIcon = 0 then FIcon := HICON(SendMessageW(FHandle, WM_GETICON, ICON_SMALL2, 0));
    if FIcon = 0 then FIcon := HICON(GetClassLongPtrW(FHandle, GCLP_HICON));
    if FIcon = 0 then FIcon := HICON(GetClassLongPtrW(FHandle, GCLP_HICONSM));
  end;
  Result := FIcon;
end;

constructor TWindow.Create(AHandle: HWND);
begin
  FHandle := AHandle;
  Init;
end;

procedure TWindow.Renew;
begin
  Init;
end;

function TWindow.ShowInTaskBar: boolean;
var
  Cloaked: UINT = 0;
begin
  if not FShowInTaskBar.HasValue then
  begin
    FShowInTaskBar.Value := CanAddToTaskBar;
    if TWinVer.IsWin8OrLater then
    begin
      DwmGetWindowAttribute(Handle, DWMWA_CLOAKED, @Cloaked, SizeOf(Cloaked));
      if (Cloaked > 0) or IsImmersiveShellWindow then
        FShowInTaskBar.Value := False;
    end;
  end;
  Result := FShowInTaskBar.Value;
end;

function TWindow.CanAddToTaskBar: boolean;
var
  Owner: HWND;
  IsToolWindow, IsAppWindow, IsNoActivate, IsDeleted: Boolean;
begin
  if not FCanAddToTaskBar.HasValue then
  begin
    if not (IsWindow(Handle) and IsWindowVisible(Handle)) then Exit(False);

    IsToolWindow := ExStyle and WS_EX_TOOLWINDOW <> 0;
    IsAppWindow := ExStyle and WS_EX_APPWINDOW <> 0;
    IsNoActivate := ExStyle and WS_EX_NOACTIVATE <> 0;
    IsDeleted := GetProp(Handle, 'ITaskList_Deleted') <> 0;
    Owner := GetWindow(Handle, GW_OWNER);
    FCanAddToTaskBar.Value := ((Owner = 0) or IsAppWindow) and (not IsNoActivate or IsAppWindow) and not IsToolWindow and not IsDeleted;
  end;
  Result := FCanAddToTaskBar.Value;
end;

function TWindow.IsUWP: boolean;
const
  APPLICATION_FRAME_HOST = 'applicationframehost.exe';
begin
  Result := StrEndsWith(AppPath, APPLICATION_FRAME_HOST, False);
end;

{ TDesktopWindow }

constructor TDesktopWindow.Create(AHandle: HWND; ARestored: boolean);
begin
  inherited Create(AHandle);
  FRestored := ARestored;
end;

{ TTrayWindow }

constructor TTrayWindow.Create(AHandle: HWND; APosition: TTrayPosition);
begin
  inherited Create(AHandle);
  FPosition := APosition;
end;

{ TWindowCollection }

function TWindowCollection.GetLastWindow: TWindow;
begin
  Result := FWindows[FOrderedWindowList.Last];
end;

function TWindowCollection.GetWindowCount: integer;
begin
  Result := FWindows.Count;
end;

function TWindowCollection.DoGetEnumerator: TWindowEnumerator;
begin
  Result := FWindows.Values.GetEnumerator;
end;

procedure TWindowCollection.WindowNotifyEvent(ASender: TObject; constref AItem: HWND; AAction: TCollectionNotification);
begin
  case AAction of
    cnAdded: FOrderedWindowList.Add(AItem);
    cnRemoved: FOrderedWindowList.Remove(AItem);
  end;
end;

constructor TWindowCollection.Create;
begin
  FWindows := specialize TObjectDictionary<HWND, TWindow>.Create([doOwnsValues]);
  FWindows.OnKeyNotify := @WindowNotifyEvent;
  FOrderedWindowList := TOrderedWindowList.Create;
end;

destructor TWindowCollection.Destroy;
begin
  FreeAndNil(FWindows);
  FreeAndNil(FOrderedWindowList);
end;

function TWindowCollection.GetEnumerator: TWindowEnumerator;
begin
  Result := DoGetEnumerator;
end;

function TWindowCollection.Find(Handle: HWND; out Window: TWindow): boolean;
begin
  Result := FWindows.TryGetValue(Handle, Window);
end;

{ TDesktopWindowCollection }

function TDesktopWindowCollection.DoGetEnumerator: TWindowEnumerator;
begin
  Result := TShowInTaskBarEnumerator.Create(FWindows.Values.GetEnumerator);
end;

{ TDesktopWindowCollection.TShowInTaskBarEnumerator }

constructor TDesktopWindowCollection.TShowInTaskBarEnumerator.Create(AEnumerator: TWindowEnumerator);
begin
  FEnumerator := AEnumerator;
end;

destructor TDesktopWindowCollection.TShowInTaskBarEnumerator.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FEnumerator);
end;

function TDesktopWindowCollection.TShowInTaskBarEnumerator.DoGetCurrent: TWindow;
begin
  Result := FEnumerator.Current;
end;

function TDesktopWindowCollection.TShowInTaskBarEnumerator.DoMoveNext: boolean;
begin
  repeat
    Result := FEnumerator.MoveNext;
    if not Result then Exit;
  until FEnumerator.Current.ShowInTaskBar;
end;

{ TWindowManager }

function TWindowManager.GetAutoMinimize: boolean;
begin
  Result := FEventHooks[ehtMinimize] <> 0;
end;

procedure TWindowManager.SetAutoMinimize(AValue: boolean);
var
  MinimizeEventHook: HWINEVENTHOOK;
begin
  MinimizeEventHook := FEventHooks[ehtMinimize];
  if AValue then
  begin
    if MinimizeEventHook <> 0 then Exit;
    FEventHooks[ehtMinimize] := SetWinEventHook(
      EVENT_SYSTEM_MINIMIZESTART,
      EVENT_SYSTEM_MINIMIZESTART,
      0,
      @WinEventProc,
      0,
      0,
      WINEVENT_OUTOFCONTEXT or WINEVENT_SKIPOWNPROCESS
    );
  end
  else
  begin
    if MinimizeEventHook = 0 then Exit;
    if UnhookWinEvent(MinimizeEventHook) then FEventHooks[ehtMinimize] := 0;
  end;
end;

procedure TWindowManager.SetSystemMenuItems(AValue: TSystemMenuItems);
var
  Window: TWindow;
begin
  if FSystemMenuItems = AValue then Exit;
  FSystemMenuItems := AValue;
  if AValue = [] then
  begin
    for Window in FDesktop.FWindows.Values do SetWindowSystemMenu(Window, AValue);
    Self.UninstallHook;
  end
  else
  begin
    Self.InstallHook;
    for Window in FDesktop.FWindows.Values do SetWindowSystemMenu(Window, AValue);
  end;
end;

procedure TWindowManager.SetWindowSystemMenu(Window: TWindow; Items: TSystemMenuItems);
var
  ParamUnion: TParamUnion;
begin
  if (Items <> []) and not (smiSeparator in Items) then Include(Items, smiSeparator);
  ParamUnion.MenuItems := Items;
  SendMessage(Window.Handle, FSystemMenuMessage, ParamUnion.WParam, LPARAM(FMainForm.Handle));
end;

procedure TWindowManager.SetSystemMenuLanguage;
var
  CopyData: COPYDATASTRUCT;
  Window: TWindow;
  LangData: TBytes;
  DataSize: DWORD = SYSTEM_MENU_LANG_DATA_MIN_SIZE;
  TextLen: DWORD;
  MenuItem: TSystemMenuItem;
  MenuItemDetail: PSystemMenuItemDetail;
  Ptr: PByte;
begin
  if not FHookInstalled then Exit;
  for MenuItem in SYSTEM_MENU_LANG_DATA_ITEMS do
  begin
    MenuItemDetail := @SYSTEM_MENU_ITEM_DETAILS[MenuItem];
    Inc(DataSize, Length(MenuItemDetail^.Text));
  end;
  SetLength(LangData, DataSize);
  Ptr := @LangData[0];
  for MenuItem in SYSTEM_MENU_LANG_DATA_ITEMS do
  begin
    MenuItemDetail := @SYSTEM_MENU_ITEM_DETAILS[MenuItem];
    TextLen := Length(MenuItemDetail^.Text);
    PDWORD(Ptr)^ := TextLen;
    Inc(Ptr, SizeOf(DWORD));
    Move(Pointer(MenuItemDetail^.Text)^, Ptr^, TextLen);
    Inc(Ptr, TextLen);
  end;
  CopyData.dwData := SYSTEM_MENU_LANG_DATA_TYPE;
  CopyData.cbData := DataSize;
  CopyData.lpData := @LangData[0];
  for Window in FDesktop.FWindows.Values do
    SendMessage(Window.Handle, WM_COPYDATA, WPARAM(Application.MainFormHandle), LPARAM(@CopyData));
end;

procedure TWindowManager.InstallHook;
begin
  if FHookInstalled then Exit;
  if not Traynard.Helpers.InstallHook() then
    raise Exception.Create(GetLastErrorMsg);
  FHookInstalled := True;
  {$IFDEF DEBUG}
  DebugLn('Hook installed');
  {$ENDIF}
end;

procedure TWindowManager.UninstallHook;
begin
  if not FHookInstalled then Exit;
  if not Traynard.Helpers.UninstallHook() then
    raise Exception.Create(GetLastErrorMsg);
  FHookInstalled := False;
  {$IFDEF DEBUG}
  DebugLn('Hook uninstalled');
  {$ENDIF}
end;

procedure TWindowManager.WindowProc(var TheMessage: TLMessage);
begin
  if TheMessage.msg = WM_SHELLHOOKMESSAGE then
    ShellHookProc(TheMessage)
  else if TheMessage.msg = SystemMenuMessage then
  begin
    case TheMessage.wParam of
      IDM_SYSTEM_TRAY_ICON, IDM_SYSTEM_TRAY_MENU:
      begin
        TryMinimizeWindow(
          HWND(TheMessage.lParam),
          specialize IfThen<TTrayPosition>(TheMessage.wParam = IDM_SYSTEM_TRAY_ICON, tpIcon, tpMenu)
        );
      end;
      IDM_SYSTEM_TOPMOST:
        SetWindowPos(HWND(TheMessage.lParam), HWND_TOPMOST, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE);
      IDM_SYSTEM_TOPMOST_CHECKED:
        SetWindowPos(HWND(TheMessage.lParam), HWND_NOTOPMOST, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE);
    end;
  end
  else
    FOriginalWindowProc(TheMessage);
end;

procedure TWindowManager.ShellHookProc(var TheMessage: TLMessage);
var
  HandleParam: HWND;
begin
  HandleParam := HWND(TheMessage.lParam);

  {$IFDEF DEBUG}
  case TheMessage.wParam of
    HSHELL_WINDOWCREATED:       DebugLn('[WINDOWCREATED]: ',       IntToStr(HandleParam));
    HSHELL_WINDOWDESTROYED:     DebugLn('[WINDOWDESTROYED]: ',     IntToStr(HandleParam));
    HSHELL_ACTIVATESHELLWINDOW: DebugLn('[ACTIVATESHELLWINDOW]: ', IntToStr(HandleParam));
    HSHELL_WINDOWACTIVATED:     DebugLn('[WINDOWACTIVATED]: ',     IntToStr(HandleParam));
    HSHELL_GETMINRECT:          DebugLn('[GETMINRECT]: ',          IntToStr(HandleParam));
    HSHELL_REDRAW:              DebugLn('[REDRAW]: ',              IntToStr(HandleParam));
    HSHELL_TASKMAN:             DebugLn('[TASKMAN]: ',             IntToStr(HandleParam));
    HSHELL_LANGUAGE:            DebugLn('[LANGUAGE]: ',            IntToStr(HandleParam));
    HSHELL_SYSMENU:             DebugLn('[SYSMENU]: ',             IntToStr(HandleParam));
    HSHELL_ENDTASK:             DebugLn('[ENDTASK]: ',             IntToStr(HandleParam));
    HSHELL_ACCESSIBILITYSTATE:  DebugLn('[ACCESSIBILITYSTATE]: ',  IntToStr(HandleParam));
    HSHELL_APPCOMMAND:          DebugLn('[APPCOMMAND]: ',          IntToStr(HandleParam));
    HSHELL_WINDOWREPLACED:      DebugLn('[WINDOWREPLACED]: ',      IntToStr(HandleParam));
    HSHELL_WINDOWREPLACING:     DebugLn('[WINDOWREPLACING]: ',     IntToStr(HandleParam));
    HSHELL_MONITORCHANGED:      DebugLn('[MONITORCHANGED]: ',      IntToStr(HandleParam));
    HSHELL_FLASH:               DebugLn('[FLASH]: ',               IntToStr(HandleParam));
    HSHELL_RUDEAPPACTIVATED:    DebugLn('[RUDEAPPACTIVATED]: ',    IntToStr(HandleParam));
    HSHELL_FULLSCREENENTER:     DebugLn('[FULLSCREENENTER]: ',     IntToStr(HandleParam));
    HSHELL_FULLSCREENEXIT:      DebugLn('[FULLSCREENEXIT]: ',      IntToStr(HandleParam));
  else
    DebugLn('[', IntToStr(TheMessage.wParam), ']: ', IntToStr(HandleParam));
  end;
  {$ENDIF}

  case TheMessage.wParam of
    HSHELL_WINDOWCREATED:
    begin
      AddWindow(HandleParam);
    end;

    HSHELL_WINDOWDESTROYED:
    begin
      RemoveWindow(HandleParam);
    end;

    HSHELL_WINDOWREPLACING:
    begin
      AddWindow(HandleParam);
    end;

    HSHELL_WINDOWREPLACED:
    begin
      RemoveWindow(HandleParam);
    end;

    HSHELL_REDRAW:
    begin
      UpdateWindow(HandleParam);
    end;

    HSHELL_GETMINRECT:
    begin
      HandleParam := PShellHookInfo(TheMessage.lParam)^.hwnd;
    end;
  end;
end;

procedure TWindowManager.AutoMinimizeChanged(Sender: TObject);
begin
  AutoMinimize := (Sender as TSettings).AutoMinimize;
end;

procedure TWindowManager.SystemMenuItemsChanged(Sender: TObject);
var
  IsFirstTime: boolean;
begin
  IsFirstTime := SystemMenuItems = [];
  SystemMenuItems := (Sender as TSettings).SystemMenuItems;
  if IsFirstTime then SetSystemMenuLanguage;
end;

procedure TWindowManager.LanguageChanged(Sender: TObject);
begin
  SetSystemMenuLanguage;
end;

procedure TWindowManager.TrayChanged;
begin
  Session.Handles := specialize IfThen<THandleArray>(Assigned(FTray), FTray.FWindows.Keys.ToArray, []);
end;

function TWindowManager.FindWindowOnDesktop(Handle: HWND): TWindow;
begin
  while not FDesktop.FWindows.TryGetValue(Handle, Result) do
  begin
    Handle := GetWindow(Handle, GW_OWNER);
    if Handle = 0 then raise Exception.Create(ERROR_WINDOW_NOT_FOUND);
  end;
  if Result.IsUWP then raise Exception.Create(ERROR_UWP_WINDOW);
end;

function TWindowManager.AddWindow(const Handle: HWND): boolean;
var
  Window: TWindow;
  IsRestored: boolean;
  OriginalWindowText: string;
  Rule: TRule;
begin
  {$IFDEF DEBUG}
  DebugLn('[AddWindow]: ', IntToStr(Handle));
  {$ENDIF}

  if FDesktop.FWindows.TryGetValue(Handle, Window) then
  begin
    { Window already exists on the desktop }
    OriginalWindowText := Window.Text;
    Window.Renew;
    FDesktop.FPONotifyObservers(Self, ooChange, Pointer(Handle));
    if Settings.ApplyRules and (OriginalWindowText <> Window.Text) and Rules.Match(Window, Rule, waChange) then
    begin
      TryMinimizeWindow(Handle, Rule.Position);
    end;
    Exit(False);
  end
  else if FTray.FWindows.TryGetValue(Handle, Window) then
  begin
    { Window already exists in the tray }
    FTray.FPONotifyObservers(Self, ooDeleteItem, Pointer(Handle)); 
    FTray.FWindows.Remove(Handle);
    TrayChanged;
    IsRestored := True;
  end
  else
  begin
    IsRestored := FRestoredWindows.Contains(Handle);
    if IsRestored then FRestoredWindows.Remove(Handle);
  end;

  Window := TDesktopWindow.Create(Handle, IsRestored);
  if Window.CanAddToTaskBar and (FCurrentPID <> Window.PID) then
  begin
    SetWindowSystemMenu(Window, FSystemMenuItems);
    FDesktop.FWindows.Add(Handle, Window);
    FDesktop.FPONotifyObservers(Self, ooAddItem, Pointer(Handle));

    if not IsRestored and Settings.ApplyRules and Rules.Match(Window, Rule, waCreation) then
    begin
      if TryMinimizeWindow(Handle, Rule.Position) then
        NotificationManager.Notify(MSG_WINDOW_MINIMIZED, Window.Text);
    end;

    Exit(True);
  end;

  Window.Free;
  Result := False;
end;

function TWindowManager.RemoveWindow(const Handle: HWND): boolean;
var
  Window: TWindow;
begin
  {$IFDEF DEBUG}
  DebugLn('[RemoveWindow]: ', IntToStr(Handle));
  {$ENDIF}
  if FDesktop.FWindows.TryGetValue(Handle, Window) then
  begin
    if Self.FSystemMenuItems <> [] then SetWindowSystemMenu(Window);
    FDesktop.FPONotifyObservers(Self, ooDeleteItem, Pointer(Handle));
    FDesktop.FWindows.Remove(Handle);
    Exit(True);
  end
  else if FTray.FWindows.TryGetValue(Handle, Window) and not IsWindow(Handle) then
  begin
    FTray.FPONotifyObservers(Self, ooDeleteItem, Pointer(Handle));
    FTray.FWindows.Remove(Handle);
    TrayChanged;
    Exit(True);
  end;
  Result := False;
end;

function TWindowManager.UpdateWindow(const Handle: HWND): boolean;
var
  Window: TWindow;
  Rule: TRule;
  OriginalWindowText: string;
begin
  {$IFDEF DEBUG}
  DebugLn('[UpdateWindow]: ', IntToStr(Handle));
  {$ENDIF}

  if FDesktop.FWindows.TryGetValue(Handle, Window) then
  begin
    OriginalWindowText := Window.Text;
    Window.Renew;
    FDesktop.FPONotifyObservers(Self, ooChange, Pointer(Handle));
    if Settings.ApplyRules and (OriginalWindowText <> Window.Text) and Rules.Match(Window, Rule, waChange) then
    begin
      TryMinimizeWindow(Handle, Rule.Position);
    end;
    Exit(True);
  end
  else if FTray.FWindows.TryGetValue(Handle, Window) then
  begin
    Window.Renew;
    FTray.FPONotifyObservers(Self, ooChange, Pointer(Handle));
    Exit(True);
  end;

  Result := False;
end;

class function TWindowManager.EnumAndMinimizeWindowsProc(Handle: HWND; Param: LPARAM): WINBOOL; stdcall;
var
  Self: TWindowManager;
  Window: TWindow;
  Rule: TRule;
begin
  Result := True;
  Self := TWindowManager(Param);
  Window := TDesktopWindow.Create(Handle, False);
  if Window.ShowInTaskBar and (Self.FCurrentPID <> Window.PID) then
  begin
    Self.FDesktop.FWindows.Add(Window.Handle, Window);
    if Settings.ApplyRules and Rules.Match(Window, Rule, waExisting) then
    begin
      Self.TryMinimizeWindow(Window.Handle, Rule.Position);
    end;
  end
  else
    Window.Free;
end;

class function TWindowManager.EnumWindowsProc(Handle: HWND; Param: LPARAM): WINBOOL; stdcall;
var
  Self: TWindowManager;
  Window: TWindow;
begin
  Result := True;
  Self := TWindowManager(Param);
  Window := TDesktopWindow.Create(Handle, False);
  if Window.ShowInTaskBar and (Self.FCurrentPID <> Window.PID) then
    Self.FDesktop.FWindows.Add(Window.Handle, Window)
  else
    Window.Free;
end;

class function TWindowManager.MinimizeOwnedWindowsProc(Handle: HWND; Param: LPARAM): WINBOOL; stdcall;
begin
  if (Handle <> HWND(Param)) and IsWindowVisible(Handle) and (HWND(Param) = GetWindow(Handle, GW_OWNER)) then
    SendMessage(Handle, WM_SYSCOMMAND, SC_MINIMIZE, 0);
  Result := True;
end;

class procedure TWindowManager.WinEventProc(hEventHook: HWINEVENTHOOK; event: DWORD; hwnd: HWND; idObject: LONG; idChild: LONG;
  idEventThread: DWORD; dwmsEventTime: DWORD); stdcall;
var
  TrayPosition: TTrayPosition;
  Rule: TRule;
  Window: TWindow;
begin
  if (hwnd = 0) or (idObject <> OBJID_WINDOW) or (idChild <> CHILDID_SELF) then Exit;

  {$IFDEF DEBUG}
  case event of
    EVENT_OBJECT_CLOAKED:       DebugLn('[EVENT_OBJECT_CLOAKED]: ',       IntToStr(hwnd));
    EVENT_OBJECT_UNCLOAKED:     DebugLn('[EVENT_OBJECT_UNCLOAKED]: ',     IntToStr(hwnd));
    EVENT_OBJECT_CREATE:        DebugLn('[EVENT_OBJECT_CREATE]: ',        IntToStr(hwnd));
    EVENT_OBJECT_DESTROY:       DebugLn('[EVENT_OBJECT_DESTROY]: ',       IntToStr(hwnd));
    EVENT_SYSTEM_MINIMIZESTART: DebugLn('[EVENT_SYSTEM_MINIMIZESTART]: ', IntToStr(hwnd));
  end;
  {$ENDIF}

  case event of
    EVENT_OBJECT_CLOAKED,
    EVENT_OBJECT_UNCLOAKED:
    begin
      FSelf.UpdateWindow(hwnd);
    end;
    EVENT_SYSTEM_MINIMIZESTART:
    begin
      if FSelf.FAutoMinimizeWindows.TryGetValue(hwnd, TrayPosition) then
      begin
        if FSelf.TryMinimizeWindow(hwnd, TrayPosition, Window) and Settings.ShowNotification then
        begin
          NotificationManager.Notify(MSG_WINDOW_MINIMIZED, Window.Text);
        end;
      end
      else if FSelf.FDesktop.FWindows.TryGetValue(hwnd, Window) and Rules.Match(Window, Rule, waMinimizing) then
      begin
        FSelf.TryMinimizeWindow(hwnd, Rule.Position);
      end;
    end;
    EVENT_OBJECT_DESTROY:
    begin
      FSelf.FAutoMinimizeWindows.Remove(hwnd);
      FSelf.FRestoredWindows.Remove(hwnd);
    end;
  end;
end;

constructor TWindowManager.Create(AOwner: TComponent);
var
  EventHookType: TEventHookType;
  TheHandle: HWND;
begin
  inherited Create(AOwner);
  FSelf := Self;           
  FMainForm := AOwner as TForm;
  FCurrentPID := GetCurrentProcessId;
  FAutoMinimizeWindows := specialize TDictionary<HWND, TTrayPosition>.Create;
  FRestoredWindows := specialize TList<HWND>.Create;
  FDesktop := TDesktopWindowCollection.Create;
  FTray := TTrayWindowCollection.Create;
  FSystemMenuItems := [];
  FSystemMenuMessage := RegisterWindowMessageW(MSG_SYSTEM_MENU);
  FHookInstalled := False;
  FOriginalWindowProc := FMainForm.WindowProc;
  FMainForm.WindowProc := @WindowProc;

  for EventHookType := Low(TEventHookType) to High(TEventHookType) do
    FEventHooks[EventHookType] := 0;

  for TheHandle in Session.Handles do
  begin
    ShowWindow(TheHandle, SW_NORMAL);
  end;

  if not RegisterShellHookWindow(FMainForm.Handle) then
    raise Exception.Create(ERROR_REGISTER_SHELL_HOOK_WINDOW);

  if TWinVer.IsWin8OrLater then
  begin
    FEventHooks[ehtCloaked] := SetWinEventHook(
      EVENT_OBJECT_CLOAKED,
      EVENT_OBJECT_UNCLOAKED,
      0,
      @WinEventProc,
      0,
      0,
      WINEVENT_OUTOFCONTEXT or WINEVENT_SKIPOWNPROCESS
    );
  end;

  if Settings.AutoMinimize then
  begin
    FEventHooks[ehtMinimize] := SetWinEventHook(
      EVENT_SYSTEM_MINIMIZESTART,
      EVENT_SYSTEM_MINIMIZESTART,
      0,
      @WinEventProc,
      0,
      0,
      WINEVENT_OUTOFCONTEXT or WINEVENT_SKIPOWNPROCESS
    );
  end;
  Settings.AddListener(siAutoMinimize, @AutoMinimizeChanged);

  FEventHooks[ehtDestroy] := SetWinEventHook(
    EVENT_OBJECT_DESTROY,
    EVENT_OBJECT_DESTROY,
    0,
    @WinEventProc,
    0,
    0,
    WINEVENT_OUTOFCONTEXT or WINEVENT_SKIPOWNPROCESS
  );

  Settings.AddListener(siSystemMenuItems, @SystemMenuItemsChanged);
  Settings.AddListener(siLanguage, @LanguageChanged);
  EnumWindows(@EnumAndMinimizeWindowsProc, LPARAM(Self));
  try
    SystemMenuItems := Settings.SystemMenuItems;
    SetSystemMenuLanguage;
  except
  end;
end;

destructor TWindowManager.Destroy;
var
  WinEventHook: HWINEVENTHOOK;
begin
  TryRestoreAllWindows;
  Settings.RemoveListeners(Self);
  SystemMenuItems := [];
  UninstallHook;
  for WinEventHook in FEventHooks do
  begin
    if WinEventHook <> 0 then
      UnhookWinEvent(WinEventHook);
  end;
  DeregisterShellHookWindow(FMainForm.Handle);
  FMainForm.WindowProc := FOriginalWindowProc;
  FreeAndNil(FDesktop);
  FreeAndNil(FTray);
  FreeAndNil(FAutoMinimizeWindows);
  FreeAndNil(FRestoredWindows);
  inherited Destroy;
end;

procedure TWindowManager.RefreshDesktop;
begin
  FDesktop.FWindows.Clear;
  EnumWindows(@EnumWindowsProc, LPARAM(Self));
end;

procedure TWindowManager.EnableWindowAutoMinimize(const Handle: HWND; const Position: TTrayPosition);
var
  Window: TWindow;
begin
  Window := FindWindowOnDesktop(Handle);
  FAutoMinimizeWindows.AddOrSetValue(Window.Handle, Position);
end;

procedure TWindowManager.DisableWindowAutoMinimize(const Handle: HWND);
begin
  if FAutoMinimizeWindows.ContainsKey(Handle) then
    FAutoMinimizeWindows.Remove(Handle)
  else
    raise Exception.Create(ERROR_WINDOW_NOT_FOUND);
end;

function TWindowManager.IsAutoMinimizeWindow(const Handle: HWND; out TrayPosition: TTrayPosition): boolean;
begin
  Result := Self.FAutoMinimizeWindows.TryGetValue(Handle, TrayPosition);
end;

function TWindowManager.IsAutoMinimizeWindow(const Handle: HWND): boolean;
var
  TrayPosition: TTrayPosition;
begin
  Result := IsAutoMinimizeWindow(Handle, TrayPosition);
end;

procedure TWindowManager.MinimizeWindow(const Handle: HWND; const Position: TTrayPosition; out Window: TWindow);
var
  TrayWindow: TTrayWindow;
begin
  {$IFDEF DEBUG}
  DebugLn('[MinimizeWindow]: ', IntToStr(Handle));
  {$ENDIF}

  Window := FindWindowOnDesktop(Handle);
  EnumWindows(@MinimizeOwnedWindowsProc, Window.Handle);
  if ShowWindow(Window.Handle, SW_HIDE) then
  begin
    TrayWindow := TTrayWindow.Create(Window.Handle, Position);
    FTray.FWindows.Add(Window.Handle, TrayWindow);
    FTray.FPONotifyObservers(Self, ooAddItem, Pointer(Window.Handle));
    TrayChanged;
  end
  else
    raise Exception.Create(GetLastErrorMsg);
end;

procedure TWindowManager.MinimizeWindow(const Handle: HWND; const Position: TTrayPosition);
var
  Window: TWindow;
begin
  MinimizeWindow(Handle, Position, Window);
end;

procedure TWindowManager.RestoreWindow(const Handle: HWND);
var
  Window: TWindow;
begin
  {$IFDEF DEBUG}
  DebugLn('[RestoreWindow]: ', IntToStr(Handle));
  {$ENDIF}

  if not FTray.FWindows.TryGetValue(Handle, Window) then
    raise Exception.Create(ERROR_WINDOW_NOT_FOUND);

  if ShowWindow(Handle, SW_NORMAL) then
  begin
    { Failed to restore window }
    FTray.FPONotifyObservers(Self, ooDeleteItem, Pointer(Handle));
    FTray.FWindows.Remove(Handle);
    TrayChanged;
    raise Exception.Create(ERROR_RESTORE_WINDOW);
  end;

  if not FRestoredWindows.Contains(Handle) then FRestoredWindows.Add(Handle);
end;

function TWindowManager.TryRestoreAllWindows: integer;
var
  Window: TWindow;
begin
  Result := 0;

  for Window in FTray.FWindows.Values do
  begin
    try
      RestoreWindow(Window.Handle);
      Inc(Result);
    except
      { pass }
    end;
  end;
end;

function TWindowManager.TryMinimizeWindow(const Handle: HWND; const Position: TTrayPosition; out Window: TWindow): boolean;
begin
  try
    MinimizeWindow(Handle, Position, Window);
    Result := True;
  except
    Result := False;
  end;
end;

function TWindowManager.TryMinimizeWindow(const Handle: HWND; const Position: TTrayPosition): boolean;
begin
  try
    MinimizeWindow(Handle, Position);
    Result := True;
  except
    Result := False;
  end;
end;

function TWindowManager.TryRestoreWindow(const Handle: HWND): boolean;
begin
  try
    RestoreWindow(Handle);
    Result := True;
  except
    Result := False;
  end;
end;

function TWindowManager.TryRestoreLastWindow: boolean;
begin
  try
    RestoreWindow(FTray.LastWindow.Handle);
    Result := True;
  except
    Result := False;
  end;
end;


initialization

WM_SHELLHOOKMESSAGE := RegisterWindowMessageW('SHELLHOOK');

end.

