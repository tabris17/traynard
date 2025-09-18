library Traynard.Lib;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, Windows, Traynard.Types, Traynard.Strings;

var
  Hooks: array [THookID] of HHOOK = (0, 0);
  SystemMenuMessage: UINT = 0;

threadvar
  MainWindow: HWND;

function _T(Str: string): PWideChar; inline;
begin
  Result := PWideChar(UTF8Decode(Str));
end;

procedure SetSystemMenu(Wnd: HWND; Items: TSystemMenuItems);
var
  SystemMenu: HMENU;
  MenuItem: TSystemMenuItem;
  MenuItemDetail: TSystemMenuItemDetail;
  PMenuItemDetail: PSystemMenuItemDetail;
begin
  SystemMenu := GetSystemMenu(Wnd, False);
  if not IsMenu(SystemMenu) then Exit;
  for MenuItemDetail in SYSTEM_MENU_ITEM_DETAILS do
    DeleteMenu(SystemMenu, MenuItemDetail.ID, MF_BYCOMMAND);
  for MenuItem in Items do
  begin
    PMenuItemDetail := @SYSTEM_MENU_ITEM_DETAILS[MenuItem];
    AppendMenuW(SystemMenu, PMenuItemDetail^.Flag, PMenuItemDetail^.ID, _T(PMenuItemDetail^.Text));
  end;
end;

procedure HandleCommand(Wnd: HWND; WParam: WPARAM);
var
  SystemMenu: HMENU;
  MenuItemState: UINT;
begin
  case WParam of
    IDM_SYSTEM_TRAY_ICON, IDM_SYSTEM_TRAY_MENU:
      PostMessage(MainWindow, SystemMenuMessage, WParam, LPARAM(Wnd));
    IDM_SYSTEM_TOPMOST:
    begin
      SystemMenu := GetSystemMenu(Wnd, False);
      MenuItemState := GetMenuState(SystemMenu, IDM_SYSTEM_TOPMOST, MF_BYCOMMAND);
      if MenuItemState and MF_CHECKED <> 0 then WParam := IDM_SYSTEM_TOPMOST_CHECKED;
      PostMessage(MainWindow, SystemMenuMessage, WParam, LPARAM(Wnd));
    end;
  end;
end;

procedure AutoCheckTopmostMenuItem(Wnd: HWND; SystemMenu: HMENU);
var
  WindowExStyle: LONG;
  CheckedFlag: UINT;
begin
  if GetMenuState(SystemMenu, IDM_SYSTEM_TOPMOST, MF_BYCOMMAND) = UINT(-1) then Exit;
  WindowExStyle := GetWindowLong(Wnd, GWL_EXSTYLE);
  CheckedFlag := specialize IfThen<UINT>(WindowExStyle and WS_EX_TOPMOST = 0, MF_UNCHECKED, MF_CHECKED);
  CheckMenuItem(SystemMenu, IDM_SYSTEM_TOPMOST, MF_BYCOMMAND or CheckedFlag);
end;

function CallWndProc(Code: longint; WParam: WPARAM; LParam: LPARAM): LRESULT; stdcall;

  procedure Translate(Data: PByte; const DataSize: DWORD);
  var
    MenuItem: TSystemMenuItem;
    Count: DWORD = 0;
    Len: DWORD;
    Text: string;
  begin
    for MenuItem in SYSTEM_MENU_LANG_DATA_ITEMS do
    begin
      Len := PDWORD(Data)^;
      Inc(Count, SizeOf(DWORD) + Len);
      if Count > DataSize then Break;
      Inc(Data, SizeOf(DWORD));
      SetLength(Text, Len);
      Move(Data^, Pointer(Text)^, Len);
      SYSTEM_MENU_ITEM_DETAILS[MenuItem].Text := Text;
      Inc(Data, Len);
    end;
  end;

  procedure UpdateSystemMenu(Wnd: HWND);
  var
    SystemMenu: HMENU;
    MenuItem: TSystemMenuItem;
    PMenuItemDetail: PSystemMenuItemDetail;
  begin
    SystemMenu := GetSystemMenu(Wnd, False);
    if not IsMenu(SystemMenu) then Exit;
    for MenuItem in SYSTEM_MENU_LANG_DATA_ITEMS do
    begin
      PMenuItemDetail := @SYSTEM_MENU_ITEM_DETAILS[MenuItem];
      ModifyMenuW(SystemMenu, PMenuItemDetail^.ID, MF_BYCOMMAND or PMenuItemDetail^.Flag, PMenuItemDetail^.ID, _T(PMenuItemDetail^.Text));
    end;
  end;

var
  Msg: PCWPSTRUCT;          
  CopyData: PCOPYDATASTRUCT;
  ParamUnion: TParamUnion;
begin
  if Code >= HC_ACTION then
  begin
    Msg := PCWPSTRUCT(LParam);
    case Msg^.message of
      WM_SYSCOMMAND:
        HandleCommand(Msg^.hwnd, Msg^.wParam);
      WM_INITMENUPOPUP:
        if HIWORD(Msg^.lParam) > 0 then AutoCheckTopmostMenuItem(Msg^.hwnd, HMENU(Msg^.wParam));
      WM_COPYDATA:
      begin
        CopyData := PCOPYDATASTRUCT(Msg^.lParam);
        if (CopyData^.dwData = SYSTEM_MENU_LANG_DATA_TYPE) and
           (CopyData^.cbData > SYSTEM_MENU_LANG_DATA_MIN_SIZE) then
        begin
          Translate(PByte(CopyData^.lpData), CopyData^.cbData);
          UpdateSystemMenu(Msg^.hwnd);
        end;
      end
    else
      if Msg^.message = SystemMenuMessage then
      begin
        ParamUnion.WParam := Msg^.wParam;
        MainWindow := Msg^.lParam;
        SetSystemMenu(Msg^.hwnd, ParamUnion.MenuItems);
      end;
    end;
  end;
  Result := CallNextHookEx(0, Code, WParam, LParam);
end;

function GetMsgProc(Code: longint; WParam: WPARAM; LParam: LPARAM): LRESULT; stdcall;
var
  Msg: PMSG;
begin
  if Code = HC_ACTION then
  begin
    Msg := PMSG(LParam);
    case Msg^.message of
      WM_SYSCOMMAND:
        HandleCommand(Msg^.hwnd, Msg^.wParam);
    end;
  end;
  Result := CallNextHookEx(0, Code, WParam, LParam);
end;


function Install: BOOL; stdcall;
var
  Hook: HHOOK;
begin
  Hook := SetWindowsHookExW(WH_CALLWNDPROC, @CallWndProc, HInstance, 0);
  if Hook = 0 then Exit(False);
  Hooks[hiCallWndProc] := Hook;
  Hook := SetWindowsHookExW(WH_GETMESSAGE, @GetMsgProc, HInstance, 0);
  if Hook = 0 then
  begin
    UnhookWindowsHookEx(Hooks[hiCallWndProc]);
    Hooks[hiCallWndProc] := 0;
    Exit(False);
  end;
  Hooks[hiGetMessage] := Hook;
  Result := True;
end;

function Uninstall: BOOL; stdcall;
var
  Hook: HHOOK;
begin
  Hook := Hooks[hiCallWndProc];
  if Hook = 0 then Exit(False);
  Result := UnhookWindowsHookEx(Hook);
  Hook := Hooks[hiGetMessage];
  Result := Result and UnhookWindowsHookEx(Hook);
end;

exports
  Install, Uninstall;

procedure ProcessDetach(Reserved : PtrInt);
begin
  { Unloading }
end;

begin
  Dll_Process_Detach_Hook := @ProcessDetach;
  SystemMenuMessage := RegisterWindowMessageW(MSG_SYSTEM_MENU);
end.

