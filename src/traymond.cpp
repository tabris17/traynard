#define TRAYMON_MAIN

#include <windows.h>
#include <windowsx.h>
#include <tchar.h>
#include <string>
#include <vector>

#include "resource.h"
#include "traymond.h"
#include "options.h"
#include "winevent.h"
#include "rules.h"
#include "logging.h"
#include "icons.h"

HANDLE saveFile;
TRCONTEXT appContext = {};

// Saves our hidden windows so they can be restored in case
// of crashing.
static void save(const TRCONTEXT *context) {
  DWORD numbytes;
  // Truncate file
  SetFilePointer(saveFile, 0, NULL, FILE_BEGIN);
  SetEndOfFile(saveFile);
  if (!context->iconIndex) {
    return;
  }
  for (int i = 0; i < context->iconIndex; i++) {
    if (context->icons[i].window) {
      std::string str;
      str = std::to_string((long)context->icons[i].window);
      str += ',';
      const char *handleString = str.c_str();
      WriteFile(saveFile, handleString, strlen(handleString), &numbytes, NULL);
    }
  }
}

// Creates our tray icon menu
static HMENU createTrayMenu(const TRCONTEXT* context) {
    HMENU popupMenu = GetSubMenu(LoadMenu(context->instance, MAKEINTRESOURCE(IDM_POPUP)), 0);
    MENUINFO mi;
    mi.cbSize = sizeof(MENUINFO);
    mi.fMask = MIM_STYLE;
    GetMenuInfo(popupMenu, &mi);
    mi.dwStyle |= MNS_NOTIFYBYPOS;
    SetMenuInfo(popupMenu, &mi);
    SetMenuDefaultItem(popupMenu, IDM_OPTIONS, FALSE);
    return popupMenu;
}

// Remove menu item from tray popup menu
static void removeMenuItem(TRCONTEXT* context, int index) {
    DestroyMenu(context->trayMenu);
    context->trayMenu = createTrayMenu(context);

    for (int i = 0; i < context->iconIndex; i++) {
        auto hw = context->icons + i;
        if (hw->hideType != HideMenu) {
            continue;
        }
        if (i == index) {
            DeleteObject(hw->menu.info.hbmpItem);
            continue;
        }
        hw->menu.info.dwTypeData = hw->menu.caption;
        InsertMenuItem(context->trayMenu, 0, TRUE, &hw->menu.info);
    }
}

HICON getWindowIcon(const TRCONTEXT* context, HWND hwnd) {
    HICON icon = (HICON)SendMessage(hwnd, WM_GETICON, ICON_SMALL2, NULL);
    if (!icon) {
        icon = (HICON)GetClassLongPtr(hwnd, GCLP_HICONSM);
        if (!icon) {
            return context->mainIcon;
        }
    }
    return icon;
}

static HBITMAP IconToBitmap(HICON icon, int width = 0, int height = 0) {
    if (!width) width = GetSystemMetrics(SM_CXSMICON);
    if (!height) height = GetSystemMetrics(SM_CYSMICON);
    HDC hdc = GetDC(NULL);
    HDC hdcMem = CreateCompatibleDC(hdc);
    BITMAPINFO bmi;
    memset(&bmi, 0, sizeof(BITMAPINFO));
    bmi.bmiHeader.biSize = sizeof(BITMAPINFOHEADER);
    bmi.bmiHeader.biWidth = width;
    bmi.bmiHeader.biHeight = height;
    bmi.bmiHeader.biPlanes = 1;
    bmi.bmiHeader.biBitCount = 32;
    bmi.bmiHeader.biCompression = BI_RGB;
#pragma warning(push)
#pragma warning(disable:6387)
    HBITMAP hbmpMem = CreateDIBSection(hdcMem, &bmi, DIB_RGB_COLORS, NULL, NULL, 0);
#pragma warning(pop)
    if (hbmpMem) {
        auto oldObj = SelectObject(hdcMem, hbmpMem);
        DrawIconEx(hdcMem, 0, 0, icon, width, height, 0, NULL, DI_NORMAL);
        SelectObject(hdcMem, oldObj);
    }
    DeleteDC(hdcMem);
    ReleaseDC(NULL, hdc);
    return hbmpMem;
}

// Revise hidden window icon hide type
// Return the number of icons moved
int reviseHiddenWindowIcon(TRCONTEXT* context) {
    int count = 0;
    HIDDEN_WINDOW hiddenWindow;
    for (int i = 0; i < context->iconIndex; i++) {
        auto hideType = context->icons[i].hideType;
        if (hideType == context->hideType) {
            continue;
        }
        memset(&hiddenWindow, 0, sizeof(HIDDEN_WINDOW));
        auto currWin = hiddenWindow.window = context->icons[i].window;
        switch (hideType) {
        case HideMenu:
            removeMenuItem(context, i);
            switch (context->hideType) {
            case HideTray:
                hiddenWindow.hideType = HideTray;
                auto nid = &hiddenWindow.icon;
                nid->cbSize = sizeof(NOTIFYICONDATA);
                nid->hWnd = context->mainWindow;
                nid->hIcon = getWindowIcon(context, currWin);
                nid->uFlags = NIF_MESSAGE | NIF_ICON | NIF_TIP | NIF_SHOWTIP;
                nid->uVersion = NOTIFYICON_VERSION;
                nid->uID = reinterpret_cast<UINT>(currWin);
                nid->uCallbackMessage = WM_ICON;
                GetWindowText(currWin, hiddenWindow.icon.szTip, MAX_WINDOW_TEXT);
                Shell_NotifyIcon(NIM_ADD, &hiddenWindow.icon);
                Shell_NotifyIcon(NIM_SETVERSION, &hiddenWindow.icon);
                context->icons[i] = hiddenWindow;
                count++;
                break;
            }
            break;
        case HideTray:
            Shell_NotifyIcon(NIM_DELETE, &context->icons[i].icon);
            switch (context->hideType) {
            case HideMenu:
                auto mid = &hiddenWindow.menu;
                auto mii = &mid->info;
                hiddenWindow.hideType = HideMenu;
                mii->hbmpItem = IconToBitmap(getWindowIcon(context, currWin));
                mii->cbSize = sizeof(MENUITEMINFO);
                mii->fMask = MIIM_STRING | MIIM_ID | MIIM_BITMAP | MIIM_DATA;
                mii->fType = MFT_STRING | MFT_BITMAP;
                mii->dwTypeData = mid->caption;
                mii->cch = GetWindowText(currWin, mii->dwTypeData, MAX_WINDOW_TEXT);
                mii->wID = 0;
                mii->dwItemData = reinterpret_cast<UINT>(currWin);
                InsertMenuItem(context->trayMenu, 0, TRUE, mii);
                context->icons[i] = hiddenWindow;
                count++;
                break;
            }
            break;
        }
    }
    save(context);    // Maybe not necessary
    return count;
}

// Restores a window
bool restoreWindow(TRCONTEXT *context, UINT xID, HWND hwnd) {
  for (int i = 0; i < context->iconIndex; i++) {
    auto icon = context->icons + i;
    switch (icon->hideType) {
    case HideTray: 
      if (icon->icon.uID != xID && icon->window != hwnd) continue;
      Shell_NotifyIcon(NIM_DELETE, &icon->icon);
      break;
    case HideMenu:
      if (icon->menu.info.dwItemData != xID && icon->window != hwnd) continue;
      removeMenuItem(context, i);
      break;
    default:
      continue;
    }

    auto currWin = icon->window;
    if (IsWindow(currWin)) {
      ShowWindow(currWin, SW_SHOW);
      SetForegroundWindow(currWin);
    }

    *icon = {};
    std::vector<HIDDEN_WINDOW> temp = std::vector<HIDDEN_WINDOW>(context->iconIndex);
    // Restructure array so there are no holes
    for (int j = 0, x = 0; j < context->iconIndex; j++) {
      if (context->icons[j].window) {
        temp[x] = context->icons[j];
        x++;
      }
    }
    memcpy_s(context->icons, sizeof(context->icons), &temp.front(), sizeof(HIDDEN_WINDOW)*context->iconIndex);
    context->iconIndex--;
    save(context);
    context->hiddenWindows.erase(currWin);
    context->freeWindows.insert(currWin);
    return true;
  }
  return false;
}

// Minimizes the current window to tray or menu.
// Uses currently focused window unless supplied a handle as the argument.
bool minimizeWindow(TRCONTEXT *context, HWND currWin, bool restored) {
  // Taskbar and desktop windows are restricted from hiding.
  const TCHAR restrictWins[][14] = { {_T("WorkerW")}, {_T("Shell_TrayWnd")} };

  if (!currWin) {
    currWin = GetForegroundWindow();
  }

  DWORD processId = 0;
  GetWindowThreadProcessId(currWin, &processId);
  if (!currWin || !isTopLevelWindow(currWin) || GetCurrentProcessId() == processId) {
    return false;
  }

  for (int i = 0; i < context->iconIndex; i++) {
    if (currWin == context->icons[i].window) {
      return IsWindowVisible(currWin) && !ShowWindow(currWin, SW_HIDE);
    }
  }

  TCHAR className[MAX_CLASS_NAME];
  if (!GetClassName(currWin, className, MAX_CLASS_NAME)) {
    return false;
  }
  for (int i = 0; i < sizeof(restrictWins) / sizeof(*restrictWins); i++) {
    if (_tcscmp(restrictWins[i], className) == 0) {
      return false;
    }
  }

  if (context->iconIndex == MAXIMUM_WINDOWS) {
    MessageBox(NULL, MSG_TOO_MANY_HIDDEN_WINDOWS, APP_NAME, MB_OK | MB_ICONERROR);
    return false;
  }

  if (IsWindowVisible(currWin) && !ShowWindow(currWin, SW_HIDE)) {
    MessageBeep(MB_ICONWARNING);
    return false;
  }
  switch (context->icons[context->iconIndex].hideType = context->hideType) {
  case HideTray:
    NOTIFYICONDATA nid;
    nid.cbSize = sizeof(NOTIFYICONDATA);
    nid.hWnd = context->mainWindow;
    nid.hIcon = getWindowIcon(context, currWin);
    nid.uFlags = NIF_MESSAGE | NIF_ICON | NIF_TIP | NIF_SHOWTIP;
    nid.uVersion = NOTIFYICON_VERSION;
    nid.uID = reinterpret_cast<UINT>(currWin);
    nid.uCallbackMessage = WM_ICON;
    GetWindowText(currWin, nid.szTip, MAX_WINDOW_TEXT);
    context->icons[context->iconIndex].icon = nid;
    Shell_NotifyIcon(NIM_ADD, &nid);
    Shell_NotifyIcon(NIM_SETVERSION, &nid);
    break;
  case HideMenu:
    MENUITEMINFO mii;
    mii.hbmpItem = IconToBitmap(getWindowIcon(context, currWin));
    mii.cbSize = sizeof(MENUITEMINFO);
    mii.fMask = MIIM_STRING | MIIM_ID | MIIM_BITMAP | MIIM_DATA;
    mii.fType = MFT_STRING | MFT_BITMAP;
    mii.dwTypeData = context->icons[context->iconIndex].menu.caption;
    mii.cch = GetWindowText(currWin, mii.dwTypeData, MAX_WINDOW_TEXT);
    mii.wID = 0;
    mii.dwItemData = reinterpret_cast<UINT>(currWin);
    context->icons[context->iconIndex].menu.info = mii;
    InsertMenuItem(context->trayMenu, 0, TRUE, &mii);
    break;
  }
  context->icons[context->iconIndex].window = currWin;
  context->iconIndex++;
  if (!restored) {
      save(context);
  }
  context->hiddenWindows.insert(currWin);
  context->freeWindows.erase(currWin);
  return true;
}

// Adds our own icon to tray
static void createTrayIcon(HWND mainWindow, NOTIFYICONDATA* icon) {
  icon->cbSize = sizeof(NOTIFYICONDATA);
  icon->hWnd = mainWindow;
  icon->uFlags = NIF_ICON | NIF_TIP | NIF_SHOWTIP | NIF_MESSAGE;
  icon->uVersion = NOTIFYICON_VERSION;
  icon->uID = reinterpret_cast<UINT>(mainWindow);
  icon->uCallbackMessage = WM_OURICON;
  _tcscpy_s(icon->szTip, APP_NAME);
  Shell_NotifyIcon(NIM_ADD, icon);
  Shell_NotifyIcon(NIM_SETVERSION, icon);
}

BOOL notifyHidingWindow(TRCONTEXT* context, HWND hwnd)
{
    TCHAR message[MAX_MSG]{ NULL };
    TCHAR windowText[MAX_WINDOW_TEXT]{ NULL };
    TCHAR exeFileName[MAX_PATH]{ NULL };
    GetWindowText(hwnd, windowText, MAX_WINDOW_TEXT);
    GetWindowExeFileName(hwnd, exeFileName, MAX_PATH);
    _stprintf_s(message, _T("%s:\n%s"), windowText, exeFileName);
    auto mainWindow = context->mainWindow;
    NOTIFYICONDATA icon{};
    icon.cbSize = sizeof(icon);
    icon.hWnd = mainWindow;
    icon.uFlags = NIF_INFO;
    icon.uVersion = NOTIFYICON_VERSION;
    icon.uID = reinterpret_cast<UINT>(mainWindow);
    _tcscpy_s(icon.szInfo, message);
    _tcscpy_s(icon.szInfoTitle, MSG_HIDING_WINDOW);
    return Shell_NotifyIcon(NIM_MODIFY, &icon);
}

// Shows all hidden windows;
static void showAllWindows(TRCONTEXT *context) {
  context->hiddenWindows.clear();
  for (int i = 0; i < context->iconIndex; i++)
  {
    auto currWin = context->icons[i].window;
    ShowWindow(context->icons[i].window, SW_SHOW);
    context->freeWindows.insert(currWin);
    switch (context->icons[i].hideType) {
    case HideTray: Shell_NotifyIcon(NIM_DELETE, &context->icons[i].icon); break;
    case HideMenu: removeMenuItem(context, i); break;
    }
    context->icons[i] = {};
  }
  save(context);
  context->iconIndex = 0;
}

static void exitApp() {
  PostQuitMessage(0);
}

static BOOL CALLBACK EnumWindowsProc(HWND hwnd, LPARAM lParam) {
    if (hwnd == reinterpret_cast<HWND>(lParam)) {
        SetLastError(TOP_LEVEL_WINDOW_ERROR);
        return FALSE;
    }
    return TRUE;
}

static BOOL WINAPI _IsTopLevelWindow(HWND hwnd) {
    if (!EnumWindows(EnumWindowsProc, reinterpret_cast<LPARAM>(hwnd)) && GetLastError() == TOP_LEVEL_WINDOW_ERROR) {
        return TRUE;
    }
    return FALSE;
}

static void setupIsTopLevelWindow()
{
    auto user32Module = GetModuleHandle(_T("user32.dll"));
    if (user32Module) {
        isTopLevelWindow = (IsTopLevelWindow)GetProcAddress(user32Module, "IsTopLevelWindow");
    }
    if (NULL == isTopLevelWindow) {
        isTopLevelWindow = _IsTopLevelWindow;
    }
}

// Creates and reads the save file to restore hidden windows in case of unexpected termination
static void startup(TRCONTEXT *context) {
  setupIsTopLevelWindow();
  loadRules(context);
  applyRules(context);
  hookWinEvent(context);
  TCHAR currDir[MAX_PATH] = { NULL };
  auto currDirLen = GetModuleFileName(NULL, currDir, MAX_PATH);
  for (int i = currDirLen; i > 0; i--) {
    if (currDir[i] == '\\') {
      currDir[i] = NULL;
      break;
    }
  }
  SetCurrentDirectory(currDir);
  if ((saveFile = CreateFile(SAVE_FILE_NAME, GENERIC_READ | GENERIC_WRITE, \
    0, NULL, OPEN_ALWAYS, FILE_ATTRIBUTE_NORMAL, NULL)) == INVALID_HANDLE_VALUE) {
    MessageBox(NULL, MSG_SAVE_FILE_ERROR, APP_NAME, MB_OK | MB_ICONERROR);
    exitApp();
  }
  // Check if we've crashed (i. e. there is a save file) during current uptime and
  // if there are windows to restore, in which case restore them and
  // display a reassuring message.
  if (GetLastError() == ERROR_ALREADY_EXISTS) {
    DWORD numbytes;
    DWORD fileSize = GetFileSize(saveFile, NULL);

    if (!fileSize) {
      return;
    };

    FILETIME saveFileWriteTime;
    GetFileTime(saveFile, NULL, NULL, &saveFileWriteTime);
    uint64_t writeTime = ((uint64_t)saveFileWriteTime.dwHighDateTime << 32 | (uint64_t)saveFileWriteTime.dwLowDateTime) / 10000;
    GetSystemTimeAsFileTime(&saveFileWriteTime);
    writeTime = (((uint64_t)saveFileWriteTime.dwHighDateTime << 32 | (uint64_t)saveFileWriteTime.dwLowDateTime) / 10000) - writeTime;

    if (GetTickCount64() < writeTime) {
      return;
    }

    std::vector<char> contents = std::vector<char>(fileSize);
    if (ReadFile(saveFile, &contents.front(), fileSize, &numbytes, NULL)) {
      char handle[10] = { NULL };
      int index = 0;
      for (size_t i = 0; i < fileSize; i++) {
        if (contents[i] != ',') {
          handle[index] = contents[i];
          index++;
        }
        else {
          index = 0;
          minimizeWindow(context, reinterpret_cast<HWND>(std::stoi(std::string(handle))), true);
          memset(handle, 0, sizeof(handle));
        }
      }
      TCHAR restoreMessage[MAX_MSG];
      _sntprintf_s(restoreMessage, _countof(restoreMessage) - 1, MSG_RESTORE_FROM_UNEXPECTED_TERMINATION, context->iconIndex);
      MessageBox(NULL, restoreMessage, APP_NAME, MB_OK);
    }
  }
}

static void shutdown(TRCONTEXT* context)
{
  CloseHandle(saveFile);
  DeleteFile(SAVE_FILE_NAME); // No save file means we have exited gracefully
  unhookWinEvent(context);
  clearRules(context);
}

static void onTaskbarRestart(TRCONTEXT* context)
{
    NOTIFYICONDATA icon {};
    icon.hIcon = context->mainIcon;
    createTrayIcon(context->mainWindow, &icon);
    for (int i = 0; i < context->iconIndex; i++) {
        auto hiddenWindow = &context->icons[i];
        if (hiddenWindow->hideType != HideTray) {
            continue;
        }
        Shell_NotifyIcon(NIM_ADD, &hiddenWindow->icon);
        Shell_NotifyIcon(NIM_SETVERSION, &hiddenWindow->icon);
    }
}

static LRESULT CALLBACK WindowProc(HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
  static UINT taskbarRestart;
  TRCONTEXT* context = reinterpret_cast<TRCONTEXT*>(GetWindowLongPtr(hwnd, GWLP_USERDATA));
  POINT pt;
  switch (uMsg)
  {
  case WM_CREATE:
    taskbarRestart = RegisterWindowMessage(_T("TaskbarCreated"));
    break;
  case WM_ICON:
    if (lParam == WM_LBUTTONDBLCLK) {
      restoreWindow(context, wParam);
    }
    break;
  case WM_KEYDOWN:debugf("KEY DOWN"); break;
  case WM_OURICON:
    switch (lParam) {
    case WM_RBUTTONUP:
      SetForegroundWindow(hwnd);
      GetCursorPos(&pt);
      TrackPopupMenuEx(
        context->trayMenu,
        TPM_BOTTOMALIGN | (GetSystemMetrics(SM_MENUDROPALIGNMENT) ? TPM_RIGHTALIGN : TPM_LEFTALIGN),
        pt.x, pt.y, hwnd, NULL
      );
      break;
    case WM_LBUTTONDBLCLK: 
      showOptionsDlg(context);
      break;
    }
    break;
  case WM_MENUCOMMAND:
    HMENU menu; 
    menu = reinterpret_cast<HMENU>(lParam);
    if (menu == context->trayMenu) {
      MENUITEMINFO mii;
      mii.cbSize = sizeof(MENUITEMINFO);
      mii.fMask = MIIM_ID | MIIM_DATA;
      if (GetMenuItemInfo(menu, wParam, TRUE, &mii)) {
        switch (mii.wID) {
        case IDM_EXIT:
          exitApp();
          break;
        case IDM_OPTIONS:
          showOptionsDlg(context);
          break;
        case IDM_RESTORE_ALL_WINDOWS:
          showAllWindows(context);
          break;
        case IDM_RESTORE_LAST_WINDOW:
          restoreWindow(context, 0, context->icons[context->iconIndex - 1].window);
          break;
        default:
          restoreWindow(context, mii.dwItemData);
          break;
        }
      }
    }
    break;
  case WM_HOTKEY:
    switch (wParam) {
    case IDHOT_HIDE_WINDOW:
      minimizeWindow(context, NULL);
      break;
    case IDHOT_POPUP_ICONS:
      showIconsDlg(context);
      break;
    case IDHOT_RESTORE_LAST_WINDOW:
      restoreWindow(context, 0, context->icons[context->iconIndex - 1].window);
      break;
    }
    break;
  default:
    if (uMsg == taskbarRestart) {
      onTaskbarRestart(context);
    }
    return DefWindowProc(hwnd, uMsg, wParam, lParam);
  }
  return 0;
}

PTCHAR getHotkeyText(PTCHAR text, rsize_t textSize, UINT modifiers, UINT vkey)
{
    constexpr TCHAR KEY_WIN[] = _T("Win");
    constexpr TCHAR KEY_ALT[] = _T("Alt");
    constexpr TCHAR KEY_CTRL[] = _T("Ctrl");
    constexpr TCHAR KEY_SHIFT[] = _T("Shift");
    constexpr TCHAR KEY_APPEND_ALT[] = _T(" + Alt");
    constexpr TCHAR KEY_APPEND_CTRL[] = _T(" + Ctrl");
    constexpr TCHAR KEY_APPEND_SHIFT[] = _T(" + Shift");
    constexpr TCHAR KEY_PLUS[] = _T(" + ");

    text[0] = NULL;

    if (modifiers & MOD_WIN) {
        _tcsnccat_s(text, textSize, KEY_WIN, _countof(KEY_WIN));
    }
    if (modifiers & MOD_CONTROL) {
        _tcsnlen(text, textSize) ?
            _tcsnccat_s(text, textSize, KEY_APPEND_CTRL, _countof(KEY_APPEND_CTRL)) :
            _tcsnccat_s(text, textSize, KEY_CTRL, _countof(KEY_CTRL));
    }
    if (modifiers & MOD_SHIFT) {
        _tcsnlen(text, textSize) ?
            _tcsnccat_s(text, textSize, KEY_APPEND_SHIFT, _countof(KEY_APPEND_SHIFT)) :
            _tcsnccat_s(text, textSize, KEY_SHIFT, _countof(KEY_SHIFT));
    }
    if (modifiers & MOD_ALT) {
        _tcsnlen(text, textSize) ?
            _tcsnccat_s(text, textSize, KEY_APPEND_ALT, _countof(KEY_APPEND_ALT)) :
            _tcsnccat_s(text, textSize, KEY_ALT, _countof(KEY_ALT));
    }
    size_t len = _tcsnlen(text, textSize);
    if (len > 0) {
        _tcsnccat_s(text, textSize, KEY_PLUS, _countof(KEY_PLUS));
        len += _countof(KEY_PLUS) - 1;
    }
    GetKeyNameText(MapVirtualKey(vkey, MAPVK_VK_TO_VSC) << 0x10, text + len, textSize - len);
    return text;
}

bool tryRegisterHotkey(HWND hwnd, int id, UINT modifiers, UINT vkey)
{
  if (modifiers == 0 || vkey == 0) {
      return false;
  }
  if (RegisterHotKey(hwnd, id, modifiers | MOD_NOREPEAT, vkey)) {
    return true;
  }

  TCHAR errMsg[MAX_MSG]{ NULL };
  TCHAR hotkeyText[MAX_HOTKEY_TEXT]{ NULL };
  getHotkeyText(hotkeyText, _countof(hotkeyText) - 1, modifiers, vkey);
  _sntprintf_s(errMsg, _countof(errMsg) - 1, MSG_HOTKEY_ERROR, hotkeyText);
  MessageBox(NULL, errMsg, APP_NAME, MB_OK | MB_ICONWARNING);
  return false;
}

#pragma warning(push)
#pragma warning(disable:4100)
#pragma warning(disable:4189)
#pragma warning(disable:4996)
int WINAPI WinMain(_In_ HINSTANCE hInstance, _In_opt_ HINSTANCE hPrevInstance, _In_ LPSTR lpCmdLine, _In_ int nShowCmd) {
    __LOGGING__;
#pragma warning(pop)

  // Mutex to allow only one instance
  HANDLE mutex = CreateMutex(NULL, TRUE, MUTEX_NAME);
  if (mutex == NULL) {
    MessageBox(NULL, MSG_MUTEX_ERROR, APP_NAME, MB_OK | MB_ICONERROR);
    return 1;
  }
  else if (GetLastError() == ERROR_ALREADY_EXISTS) {
    MessageBox(NULL, MSG_ALREADY_RUNNING, APP_NAME, MB_OK | MB_ICONERROR);
    return 1;
  }

  WNDCLASS wc = {};
  wc.lpfnWndProc = WindowProc;
  wc.hInstance = hInstance;
  wc.lpszClassName = APP_NAME;
  if (!RegisterClass(&wc)) {
      return 1;
  }

  GetClassInfo(NULL, WC_DIALOG, &wc);
  wc.style |= CS_DROPSHADOW;
  wc.lpszClassName = _T(POPUP_CLASS);
  if (!RegisterClass(&wc)) {
      return 1;
  }

  auto context = &appContext;
  context->instance = hInstance;
  context->cmdLine = GetCommandLine();
  loadOptions(context);

  context->mainWindow = CreateWindow(APP_NAME, NULL, NULL, 0, 0, 0, 0, NULL, NULL, hInstance, NULL);
  if (!context->mainWindow) {
    return 1;
  }
  ShowWindow(context->mainWindow, SW_HIDE);

  // Store our context in main window for retrieval by WindowProc
  SetWindowLongPtr(context->mainWindow, GWLP_USERDATA, reinterpret_cast<LONG>(context));

  NOTIFYICONDATA icon = {};
  context->mainIcon = icon.hIcon = LoadIcon(hInstance, MAKEINTRESOURCE(IDI_TRAYMOND));
  createTrayIcon(context->mainWindow, &icon);
  context->trayMenu = createTrayMenu(context);
  startup(context);

  tryRegisterHotkey(context->mainWindow, IDHOT_HIDE_WINDOW, context->hotkey.modifiers, context->hotkey.vkey);
  tryRegisterHotkey(context->mainWindow, IDHOT_POPUP_ICONS, context->hotkey2.modifiers, context->hotkey2.vkey);
  tryRegisterHotkey(context->mainWindow, IDHOT_RESTORE_LAST_WINDOW, context->hotkey3.modifiers, context->hotkey3.vkey);

  BOOL bRet;
  MSG msg;
  while ((bRet = GetMessage(&msg, 0, 0, 0)) != 0)
  {
    if (bRet != -1) {
      TranslateMessage(&msg);
      DispatchMessage(&msg);
    }
  }
  // Clean up on exit;
  UnregisterHotKey(context->mainWindow, IDHOT_HIDE_WINDOW);
  UnregisterHotKey(context->mainWindow, IDHOT_POPUP_ICONS);
  UnregisterHotKey(context->mainWindow, IDHOT_RESTORE_LAST_WINDOW);
  showAllWindows(context);
  Shell_NotifyIcon(NIM_DELETE, &icon);
  DestroyMenu(context->trayMenu);
  DestroyWindow(context->mainWindow);
  shutdown(context);
  ReleaseMutex(mutex);
  CloseHandle(mutex);
  return msg.wParam;
}

TRCONTEXT* AppContext()
{
  return &appContext;
}
