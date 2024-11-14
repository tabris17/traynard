#include <windows.h>
#include <windowsx.h>
#include <string>
#include <vector>

#include "resource.h"
#include "traymond.h"
#include "options.h"

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
  for (int i = 0; i < context->iconIndex; i++)
  {
    if (context->icons[i].window) {
      std::string str;
      str = std::to_string((long)context->icons[i].window);
      str += ',';
      const char *handleString = str.c_str();
      WriteFile(saveFile, handleString, strlen(handleString), &numbytes, NULL);
    }
  }
}

// Remove menu item from tray popup menu
static void removeMenuItem(const TRCONTEXT* context, int index) {
    auto mii = &context->icons[index].menu.info;
    DeleteObject(mii->hbmpItem);
    DeleteMenu(context->trayMenu, mii->wID, MF_BYCOMMAND);
}

static HICON getWindowIcon(const TRCONTEXT* context, HWND hwnd) {
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
    HBITMAP hbmpMem = CreateDIBSection(hdcMem, &bmi, DIB_RGB_COLORS, NULL, NULL, 0);
    if (hbmpMem) {
        SelectObject(hdcMem, hbmpMem);
        DrawIconEx(hdcMem, 0, 0, icon, width, height, 0, NULL, DI_NORMAL);
    }
    DeleteDC(hdcMem);
    ReleaseDC(NULL, hdc);
    return hbmpMem;

    ICONINFO iconInfo;
    GetIconInfo(icon, &iconInfo);
    DeleteObject(iconInfo.hbmMask);
    return iconInfo.hbmColor;
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
                mii->fMask = MIIM_STRING | MIIM_ID | MIIM_BITMAP;
                mii->fType = MFT_STRING | MFT_BITMAP;
                mii->dwTypeData = mid->caption;
                mii->cch = GetWindowText(currWin, mii->dwTypeData, MAX_WINDOW_TEXT);
                mii->wID = reinterpret_cast<UINT>(currWin);
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
static void showWindow(TRCONTEXT *context, UINT xID) {
  for (int i = 0; i < context->iconIndex; i++) {
    switch (context->icons[i].hideType) {
    case HideTray: 
      if (context->icons[i].icon.uID != xID) continue;
      Shell_NotifyIcon(NIM_DELETE, &context->icons[i].icon);
      break;
    case HideMenu:
      if (context->icons[i].menu.info.wID != xID) continue;
      removeMenuItem(context, i);
      break;
    default:
      continue;
    }

    ShowWindow(context->icons[i].window, SW_SHOW);
    SetForegroundWindow(context->icons[i].window);
    context->icons[i] = {};
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
    break;
}
}

// Minimizes the current window to tray or menu.
// Uses currently focused window unless supplied a handle as the argument.
static void minimizeWindow(TRCONTEXT *context, long restoreWindow) {
  // Taskbar and desktop windows are restricted from hiding.
  const char restrictWins[][14] = { {"WorkerW"}, {"Shell_TrayWnd"} };

  HWND currWin = 0;
  if (!restoreWindow) {
    currWin = GetForegroundWindow();
  }
  else {
    currWin = reinterpret_cast<HWND>(restoreWindow);
  }

  if (!currWin) {
    return;
  }

  char className[256];
  if (!GetClassName(currWin, className, 256)) {
    return;
  }
  else {
    for (int i = 0; i < sizeof(restrictWins) / sizeof(*restrictWins); i++)
    {
      if (strcmp(restrictWins[i], className) == 0) {
        return;
      }
    }
  }
  if (context->iconIndex == MAXIMUM_WINDOWS) {
    MessageBox(NULL, MSG_TOO_MANY_HIDDEN_WINDOWS, APP_NAME, MB_OK | MB_ICONERROR);
    return;
  }

  if (!ShowWindow(currWin, SW_HIDE)) {
    MessageBeep(MB_ICONWARNING);
    return;
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
    mii.fMask = MIIM_STRING | MIIM_ID | MIIM_BITMAP;
    mii.fType = MFT_STRING | MFT_BITMAP;
    mii.dwTypeData = context->icons[context->iconIndex].menu.caption;
    mii.cch = GetWindowText(currWin, mii.dwTypeData, MAX_WINDOW_TEXT);
    mii.wID = reinterpret_cast<UINT>(currWin);
    context->icons[context->iconIndex].menu.info = mii;
    InsertMenuItem(context->trayMenu, 0, TRUE, &mii);
    break;
  }
  context->icons[context->iconIndex].window = currWin;
  context->iconIndex++;
  if (!restoreWindow) {
      save(context);
  }
}

// Adds our own icon to tray
static void createTrayIcon(HWND mainWindow, NOTIFYICONDATA* icon) {
  icon->cbSize = sizeof(NOTIFYICONDATA);
  icon->hWnd = mainWindow;
  icon->uFlags = NIF_ICON | NIF_TIP | NIF_SHOWTIP | NIF_MESSAGE;
  icon->uVersion = NOTIFYICON_VERSION;
  icon->uID = reinterpret_cast<UINT>(mainWindow);
  icon->uCallbackMessage = WM_OURICON;
  strcpy_s(icon->szTip, APP_NAME);
  Shell_NotifyIcon(NIM_ADD, icon);
  Shell_NotifyIcon(NIM_SETVERSION, icon);
}

// Creates our tray icon menu
static void createTrayMenu(HMENU* trayMenu) {
  *trayMenu = CreatePopupMenu();

  MENUITEMINFO showAllMenuItem;
  MENUITEMINFO exitMenuItem;
  MENUITEMINFO optionsMenuItem;
  MENUITEMINFO separatorMenuItem;

  exitMenuItem.cbSize = sizeof(MENUITEMINFO);
  exitMenuItem.fMask = MIIM_STRING | MIIM_ID;
  exitMenuItem.fType = MFT_STRING;
  exitMenuItem.dwTypeData = MENU_EXIT;
  exitMenuItem.cch = sizeof(MENU_EXIT);
  exitMenuItem.wID = MI_EXIT_ID;

  showAllMenuItem.cbSize = sizeof(MENUITEMINFO);
  showAllMenuItem.fMask = MIIM_STRING | MIIM_ID;
  showAllMenuItem.fType = MFT_STRING;
  showAllMenuItem.dwTypeData = MENU_RESTORE_ALL_WINDOWS;
  showAllMenuItem.cch = sizeof(MENU_RESTORE_ALL_WINDOWS);
  showAllMenuItem.wID = MI_SHOW_ALL_ID;

  optionsMenuItem.cbSize = sizeof(MENUITEMINFO);
  optionsMenuItem.fMask = MIIM_STRING | MIIM_ID;
  optionsMenuItem.fType = MFT_STRING;
  optionsMenuItem.dwTypeData = MENU_OPTIONS;
  optionsMenuItem.cch = sizeof(MENU_OPTIONS);
  optionsMenuItem.wID = MI_OPTIONS_ID;

  separatorMenuItem.cbSize = sizeof(MENUITEMINFO);
  separatorMenuItem.fMask = MIIM_FTYPE | MIIM_ID;
  separatorMenuItem.fType = MFT_SEPARATOR;
  separatorMenuItem.wID = MI_SEPARATOR_ID;

  InsertMenuItem(*trayMenu, 0, FALSE, &showAllMenuItem);
  InsertMenuItem(*trayMenu, MI_SHOW_ALL_ID, TRUE, &optionsMenuItem);
  InsertMenuItem(*trayMenu, MI_OPTIONS_ID, TRUE, &separatorMenuItem);
  InsertMenuItem(*trayMenu, MI_SEPARATOR_ID, TRUE, &exitMenuItem);
}

// Shows all hidden windows;
static void showAllWindows(TRCONTEXT *context) {
  for (int i = 0; i < context->iconIndex; i++)
  {
    ShowWindow(context->icons[i].window, SW_SHOW);
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

// Creates and reads the save file to restore hidden windows in case of unexpected termination
static void startup(TRCONTEXT *context) {
  char currDir[MAX_PATH] = { NULL };
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
          minimizeWindow(context, std::stoi(std::string(handle)));
          memset(handle, 0, sizeof(handle));
        }
      }
      char restoreMessage[MAX_MSG];
      snprintf(restoreMessage, MAX_MSG, MSG_RESTORE_FROM_UNEXPECTED_TERMINATION, context->iconIndex);
      MessageBox(NULL, restoreMessage, APP_NAME, MB_OK);
    }
  }
}

static LRESULT CALLBACK WindowProc(HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam) {

  TRCONTEXT* context = reinterpret_cast<TRCONTEXT*>(GetWindowLongPtr(hwnd, GWLP_USERDATA));
  POINT pt;
  switch (uMsg)
  {
  case WM_ICON:
    if (lParam == WM_LBUTTONDBLCLK) {
      showWindow(context, wParam);
    }
    break;
  case WM_OURICON:
    switch (lParam) {
    case WM_RBUTTONUP:
      SetForegroundWindow(hwnd);
      GetCursorPos(&pt);
      TrackPopupMenuEx(
        context->trayMenu,
        (GetSystemMetrics(SM_MENUDROPALIGNMENT) ? TPM_RIGHTALIGN : TPM_LEFTALIGN) | TPM_BOTTOMALIGN, 
        pt.x, pt.y, hwnd, NULL
      );
      break;
    case WM_LBUTTONDBLCLK: 
      showOptionsDlg(context);
      break;
    }
    break;
  case WM_COMMAND:
    switch (wParam) {
    case MI_SHOW_ALL_ID:
      showAllWindows(context);
      break;
    case MI_EXIT_ID:
      exitApp();
      break;
    case MI_OPTIONS_ID:
      showOptionsDlg(context);
      break;
    default:
      showWindow(context, wParam);
    }
    break;
  case WM_HOTKEY: // We only have one hotkey, so no need to check the message
    minimizeWindow(context, NULL);
    break;
  default:
    return DefWindowProc(hwnd, uMsg, wParam, lParam);
  }
  return 0;
}

#pragma warning( push )
#pragma warning( disable : 4100 )
int WINAPI WinMain(_In_ HINSTANCE hInstance, _In_opt_ HINSTANCE hPrevInstance, _In_ LPSTR lpCmdLine, _In_ int nShowCmd) {
#pragma warning( pop )
  auto context = &appContext;
  context->instance = hInstance;
  context->cmdLine = GetCommandLine();
  loadOptions(context);

  NOTIFYICONDATA icon = {};

  // Mutex to allow only one instance
  HANDLE mutex = CreateMutex(NULL, TRUE, MUTEX_NAME);
  if (mutex == NULL) {
    switch (GetLastError()) {
    case ERROR_ALREADY_EXISTS:
      MessageBox(NULL, MSG_ALREADY_RUNNING, APP_NAME, MB_OK | MB_ICONERROR);
      break;
    default:
      MessageBox(NULL, MSG_MUTEX_ERROR, APP_NAME, MB_OK | MB_ICONERROR);
    }
    return 1;
  }

  BOOL bRet;
  MSG msg;

  WNDCLASS wc = {};
  wc.lpfnWndProc = WindowProc;
  wc.hInstance = hInstance;
  wc.lpszClassName = APP_NAME;

  if (!RegisterClass(&wc)) {
    return 1;
  }
  
  context->mainWindow = CreateWindow(APP_NAME, NULL, NULL, 0, 0, 0, 0, HWND_MESSAGE, NULL, hInstance, NULL);
  context->mainIcon = icon.hIcon = LoadIcon(hInstance, MAKEINTRESOURCE(TRAYMOND_ICON));

  if (!context->mainWindow) {
    return 1;
  }

  // Store our context in main window for retrieval by WindowProc
  SetWindowLongPtr(context->mainWindow, GWLP_USERDATA, reinterpret_cast<LONG>(context));

  if (!RegisterHotKey(context->mainWindow, HIDE_WINDOW_HOTKEY_ID, context->hotkey.modifiers | MOD_NOREPEAT, context->hotkey.vkey)) {
    MessageBox(NULL, MSG_HOTKEY_ERROR, APP_NAME, MB_OK | MB_ICONERROR);
    return 1;
  }

  createTrayIcon(context->mainWindow, &icon);
  createTrayMenu(&context->trayMenu);
  startup(context);

  while ((bRet = GetMessage(&msg, 0, 0, 0)) != 0)
  {
    if (bRet != -1) {
      TranslateMessage(&msg);
      DispatchMessage(&msg);
    }
  }
  // Clean up on exit;
  showAllWindows(context);
  Shell_NotifyIcon(NIM_DELETE, &icon);
  ReleaseMutex(mutex);
  CloseHandle(mutex);
  CloseHandle(saveFile);
  DestroyMenu(context->trayMenu);
  DestroyWindow(context->mainWindow);
  DeleteFile(SAVE_FILE_NAME); // No save file means we have exited gracefully
  UnregisterHotKey(context->mainWindow, HIDE_WINDOW_HOTKEY_ID);
  return msg.wParam;
}
