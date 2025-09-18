unit Traynard.Helpers;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls, ExtCtrls, LCLType, Windows;

type

  { TWinVer }

  TWinVer = class
  private
    class var
      FMajor: integer;
      FMinor: integer;
      FBuild: integer;
  public
    class function IsWin8OrLater: boolean;
    class function IsWin81OrLater: boolean;
    class function IsWin10OrLater: boolean;
    class function IsWin11OrLater: boolean;
    class procedure Initialize;
  end;

  { TFPTemplateParams }

  TFPTemplateParams = class (TStringList)
  public
    procedure GetParam(Sender: TObject; const ParamName: string; out AValue: string);
  end;

  { TImageListHelper }

  TImageListHelper = class helper for TImageList
  public
    function AddIcon(HIcon: HICON; DefaultIndex: integer = -1): integer; overload;
    function AddIcon(Instance: TLCLHandle; const ResName: string): integer; overload;
    procedure Reset(Preserve: integer);
  end;

  { TIconHelper }

  TIconHelper = class helper for TIcon
  public
    function LoadFromExe(const ExePath: string): boolean;
  end;

  { TBitmapHelper }

  TBitmapHelper = class helper for Graphics.TBitmap
  public
    procedure LoadFromHIcon(const Icon: HICON; const Width, Height: Integer);
    function LoadFromExe(const ExePath: string; const Width, Height: Integer): boolean;
  end;

  { TCheckGroupHelper }

  TCheckGroupHelper = class helper for TCheckGroup
  public
    procedure UncheckAll;
    function IsAllUnchecked: boolean;
  end;

  TIsTopLevelWindowFunc = function (hwnd: HWND): BOOL; stdcall;
  TInstallHookFunc = function: BOOL; stdcall;
  TUninstallHookFunc = function: BOOL; stdcall;

procedure ShortCutToWinHotkey(const ShortCut: TShortCut; out Key: Word; out Modifiers: Word);
function KeyAndShiftStateToKeyString(const Key: Word; const Shift: TShiftState): String; inline;
function ShortCutToText(const ShortCut: TShortCut): string; inline;
function WinHotkeyToText(const Modifiers: Word; const Key: Word): string;
function StrEndsWith(const Str, EndStr: string; CaseSensitive: boolean = True): boolean; inline;
function StrStartsWith(const Str, StartStr: string; CaseSensitive: boolean = True): boolean; inline;
function GetPersistentPath(APersistent: TPersistent): string;
function GetLastErrorMsg: string; inline;
function RegisterShellHookWindow(hwnd: HWND): BOOL; stdcall; external 'user32' name 'RegisterShellHookWindow';
function DeregisterShellHookWindow(hwnd: HWND): BOOL; stdcall; external 'user32' name 'DeregisterShellHookWindow';
function RegGetValueW(hkey: HKEY; lpSubKey: LPCWSTR; lpValue: LPCWSTR;
  dwFlags: DWORD; pdwType: LPDWORD; pvData: PVOID; pcbData: LPDWORD): LONG; stdcall; external 'advapi32' name 'RegGetValueW';

var
  IsTopLevelWindow: TIsTopLevelWindowFunc = nil;
  InstallHook: TInstallHookFunc = nil;
  UninstallHook: TUninstallHookFunc = nil;

implementation

uses
  Forms, Menus, CommCtrl, LCLProc, fpTemplate, JwaWinUser, Traynard.Types, Traynard.Strings, Traynard.Storage;

type
  TPersistentAccess = class(TPersistent);

function KeyAndShiftStateToKeyString(const Key: Word; const Shift: TShiftState): String;
begin
  Result := LCLProc.KeyAndShiftStateToKeyString(Key, Shift);
  Result := StringReplace(Result, 'Meta', 'Win', [rfReplaceAll, rfIgnoreCase]);
end;

function ShortCutToText(const ShortCut: TShortCut): string;
begin
  Result := LCLProc.ShortCutToText(ShortCut);
  Result := StringReplace(Result, 'Meta', 'Win', [rfReplaceAll, rfIgnoreCase]);
end;

function WinHotkeyToText(const Modifiers: Word; const Key: Word): string;
var
  ShiftState: TShiftState = [];
begin
  if Key = 0 then Exit('');
  if Modifiers and MOD_ALT > 0 then Include(ShiftState, ssAlt);
  if Modifiers and MOD_CONTROL > 0 then Include(ShiftState, ssCtrl);
  if Modifiers and MOD_SHIFT > 0 then Include(ShiftState, ssShift);
  if Modifiers and MOD_WIN > 0 then Include(ShiftState, ssMeta);
  Result := KeyAndShiftStateToKeyString(Key, ShiftState);
end;

function StrEndsWith(const Str, EndStr: string; CaseSensitive: boolean): boolean;
var
  TheRightStr: string;
begin
  TheRightStr := RightStr(Str, Length(EndStr));
  Result := specialize IfThen<boolean>(CaseSensitive, TheRightStr = EndStr, SameText(TheRightStr, EndStr));
end;

function StrStartsWith(const Str, StartStr: string; CaseSensitive: boolean): boolean;
var
  TheLeftStr: string;
begin
  TheLeftStr := LeftStr(Str, Length(StartStr));
  Result := specialize IfThen<boolean>(CaseSensitive, TheLeftStr = StartStr, SameText(TheLeftStr, StartStr));
end;

function GetPersistentPath(APersistent: TPersistent): string;
var
  TempPersistent: TPersistent;
  NamePath: string;
begin
  Result := '';
  TempPersistent := APersistent;
  while Assigned(TempPersistent) do
  begin
    NamePath := TempPersistent.GetNamePath;
    if NamePath <> '' then Result := NamePath + '.' + Result;
    {$IFDEF DEBUG}{$objectChecks-}{$ENDIF}
    TempPersistent := TPersistentAccess(TempPersistent).GetOwner;
    {$IFDEF DEBUG}{$objectChecks+}{$ENDIF}
  end;
end;

function GetLastErrorMsg: string;
var
  ErrCode: DWORD;
begin
  ErrCode := GetLastError;
  Result := Format('[%d] %s', [ErrCode, SysErrorMessage(ErrCode)])
end;

procedure ShortCutToWinHotkey(const ShortCut: TShortCut; out Key: Word; out Modifiers: Word);
var
  Shift: TShiftState;
begin
  ShortCutToKey(ShortCut, Key, Shift);
  Modifiers := 0;
  if ssShift in Shift then Inc(Modifiers, MOD_SHIFT);
  if ssAlt in Shift then Inc(Modifiers, MOD_ALT);
  if ssCtrl in Shift then Inc(Modifiers, MOD_CONTROL);
  if ssMeta in Shift then Inc(Modifiers, MOD_WIN);
end;

function FindTopLevelWindow(Handle: HWND; Param: LPARAM): BOOL; stdcall;
begin
  if Handle = HWND(Param) then
  begin
    SetLastError(TOP_LEVEL_WINDOW_ERROR);
    Exit(False);
  end;
  Result := True;
end;

function UserDefinedIsTopLevelWindow(hwnd: HWND): BOOL; stdcall;
begin
  if not EnumWindows(@FindTopLevelWindow, LPARAM(hwnd)) and (GetLastError = TOP_LEVEL_WINDOW_ERROR) then Exit(True);
  Result := False;
end;

function InstallHookError: BOOL; stdcall;
begin
  Result := False;
  raise Exception.Create(ERROR_INSTALL_HOOK);
end;

function UninstallHookError: BOOL; stdcall;
begin
  Result := False;
  raise Exception.Create(ERROR_UNINSTALL_HOOK);
end;

procedure InitializeIsTopLevelWindowFunc; inline;
begin
  IsTopLevelWindow := TIsTopLevelWindowFunc(GetProcAddress(GetModuleHandleW(USER32_DLL), IS_TOP_LEVEL_WINDOW));
  if IsTopLevelWindow = nil then IsTopLevelWindow := @UserDefinedIsTopLevelWindow;
end;

procedure InitializeHookFunc; inline;
var
  HookModule: HINST;
begin
  HookModule := LoadLibraryW(HOOK_DLL);
  if HookModule = 0 then
    HookModule := LoadLibraryW(PWideChar(unicodestring(Storage.AppDataDir + HOOK_DLL)));

  if HookModule = 0 then
  begin
    InstallHook := @InstallHookError;
    UninstallHook := @UninstallHookError;
  end
  else
  begin
    InstallHook := TInstallHookFunc(GetProcAddress(HookModule, HOOK_INSTALL));
    UninstallHook := TUninstallHookFunc(GetProcAddress(HookModule, HOOK_UNINSTALL));
  end;
end;

{ TWinVer }

class function TWinVer.IsWin8OrLater: boolean;
begin
  Result := (FMajor > 6) or ((FMajor = 6) and (FMinor >= 2));
end;

class function TWinVer.IsWin81OrLater: boolean;
begin
  Result := (FMajor > 6) or ((FMajor = 6) and (FMinor >= 2) and (FBuild >= 9600));
end;

class function TWinVer.IsWin10OrLater: boolean;
begin
  Result := FMajor >= 10;
end;

class function TWinVer.IsWin11OrLater: boolean;
begin
  Result := (FMajor >= 10) and (FBuild >= 22000);
end;

class procedure TWinVer.Initialize;
var
  OSVerInfo: TOSVersionInfo;
begin
  OSVerInfo.dwOSVersionInfoSize := SizeOf(OSVerInfo);
  if not GetVersionEx(OSVerInfo) then raise Exception.Create(ERROR_GET_OS_VERSION);
  FMajor := OSVerInfo.dwMajorVersion;
  FMinor := OSVerInfo.dwMinorVersion;
  FBuild := OSVerInfo.dwBuildNumber;
end;

{ TFPTemplateParams }

procedure TFPTemplateParams.GetParam(Sender: TObject; const ParamName: string; out AValue: string);
begin
  AValue := Values[Trim(ParamName)];
end;

{ TImageListHelper }

function TImageListHelper.AddIcon(HIcon: HICON; DefaultIndex: integer): integer;
var
  Icon: TIcon;
begin
  if HIcon = 0 then Exit(DefaultIndex);
  Icon := TIcon.Create;
  try
    Icon.Handle := CopyIcon(HIcon);
    Result := AddIcon(Icon);
    Result := ImageList_ReplaceIcon(Handle, Result, Icon.Handle);
  finally
    Icon.Free;
  end;
end;

function TImageListHelper.AddIcon(Instance: TLCLHandle; const ResName: string): integer;
var
  Icon: TIcon;
begin
  Icon := TIcon.Create;
  try
    Icon.LoadFromResourceName(Instance, ResName);
    Result := AddIcon(Icon);
  finally
    Icon.Free;
  end;
end;

procedure TImageListHelper.Reset(Preserve: integer);
begin
  if Count <= Preserve then Exit;
  BeginUpdate;
  while Count > Preserve do Delete(Count - 1);
  EndUpdate;
end;

{ TIconHelper }

function TIconHelper.LoadFromExe(const ExePath: string): boolean;
var
  IconHandle: HICON;
begin
  IconHandle := ExtractIcon(hInstance, PChar(ExePath), 0);
  if IconHandle <= 1 then Exit(False);
  Self.Handle := IconHandle;
  Result := True;
end;

{ TBitmapHelper }

procedure TBitmapHelper.LoadFromHIcon(const Icon: HICON; const Width, Height: Integer);
var
  TheDC, MemDC: HDC;
  ABitmapInfo: BITMAPINFO;
  MemBmp: HBITMAP;
  OldObj: HGDIOBJ;
  APoint: Pointer;
begin
  TheDC := GetDC(0);
  MemDC := CreateCompatibleDC(TheDC);
  FillChar(ABitmapInfo, SizeOf(BITMAPINFO), 0);
  with ABitmapInfo.bmiHeader do
  begin
    biSize := SizeOf(BITMAPINFOHEADER);
    biWidth := Width;
    biHeight := Height;
    biPlanes := 1;
    biBitCount := 32;
    biCompression := BI_RGB;
  end;
  MemBmp := CreateDIBSection(MemDC, ABitmapInfo, DIB_RGB_COLORS, APoint, 0, 0);
  if MemBmp <> 0 then
  begin
    OldObj := SelectObject(MemDC, MemBmp);
    DrawIconEx(MemDC, 0, 0, icon, width, height, 0, 0, DI_NORMAL);
    SelectObject(MemDC, OldObj);
    Self.Handle := MemBmp;
  end;
  DeleteDC(MemDC);
  ReleaseDC(0, TheDC);
end;

function TBitmapHelper.LoadFromExe(const ExePath: string; const Width, Height: Integer): boolean;
var
  IconHandle: HICON;
begin
  IconHandle := ExtractIcon(hInstance, PChar(ExePath), 0);
  if IconHandle <= 1 then Exit(False);
  LoadFromHIcon(IconHandle, Width, Height);
  Result := True;
end;

{ TCheckGroupHelper }

procedure TCheckGroupHelper.UncheckAll;
var
  i: integer;
begin
  for i := 0 to Items.Count - 1 do
    Checked[i] := False;
end;

function TCheckGroupHelper.IsAllUnchecked: boolean;
var
  i: integer;
begin
  for i := 0 to Items.Count - 1 do
  begin
    if Checked[i] then Exit(False);
  end;
  Result := True;
end;

initialization

fpTemplate.DefaultStartDelimiter:='{{';
fpTemplate.DefaultStartDelimiter:='{{';
fpTemplate.DefaultEndDelimiter:='}}';
fpTemplate.DefaultParamStartDelimiter:=' ';
fpTemplate.DefaultParamEndDelimiter:='"';
fpTemplate.DefaultParamValueSeparator:='="';

InitializeIsTopLevelWindowFunc;
InitializeHookFunc;
TWinVer.Initialize;

end.

