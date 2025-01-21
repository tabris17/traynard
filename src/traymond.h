#pragma once
#include <windows.h>
#include <tchar.h>
#include <unordered_set>
#include <vector>

#include "resource.h"
#include "i18n.h"


#define VK_Z_KEY 0x5A
#define VK_Q_KEY 0x51
// These keys are used to send windows to tray
#define TRAY_KEY VK_Z_KEY
#define TRAY_KEY2 VK_Q_KEY
#define MOD_KEY MOD_WIN + MOD_SHIFT

#define WM_ICON 0x1C0A
#define WM_OURICON 0x1C0B
#define MAXIMUM_WINDOWS 100
#define IDHOT_HIDE_WINDOW 0
#define IDHOT_POPUP_ICONS 1
#define IDHOT_RESTORE_LAST_WINDOW 2
#define TEST_HOTKEY_ID 0xBFFF
#define MAX_MSG 1024
#define MAX_HOTKEY_TEXT 64
#define MAX_WINDOW_TEXT 128
#define MAX_CLASS_NAME 256
#define MAX_RULE_NAME 128

#define APP_NAME _T(PROJECT_NAME)
#define SAVE_FILE_NAME _T(PROJECT_NAME_LC ".dat")
#define MUTEX_NAME _T(PROJECT_NAME_LC "_mutex")
#define REG_KEY_SOFTWARE _T("SOFTWARE\\" PROJECT_NAME)
#define REG_KEY_RUN _T("SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\Run")
#define HELP_URL _T("https://github.com/tabris17/traymond/wiki")

#define APPLICATION_ERROR_CODE 0x10000000
#define TOP_LEVEL_WINDOW_ERROR (APPLICATION_ERROR_CODE | 1)

#define SET_CONTAINS(_THE_SET_, _THE_VALUE_) ((_THE_SET_).find(_THE_VALUE_) != (_THE_SET_).end())
#define VECTOR_ERASE(_THE_VECTOR_, _THE_INDEX_) ((_THE_VECTOR_).erase((_THE_VECTOR_).begin() + (_THE_INDEX_)))

typedef BOOL(WINAPI *IsTopLevelWindow)(HWND hwnd);

typedef struct {
    UINT modifiers;
    UINT vkey;
} HOTKEY;

typedef enum {
    HideTray = 0,
    HideMenu = 1,
} HIDE_TYPE;

typedef struct {
    MENUITEMINFO info;
    TCHAR caption[MAX_WINDOW_TEXT];
} MENUITEMDATA;

// Stores hidden window record.
typedef struct HIDDEN_WINDOW {
    HIDE_TYPE hideType;
    union {
        NOTIFYICONDATA icon;
        MENUITEMDATA menu;
    };
    HWND window;
} HIDDEN_WINDOW;

#define RULE_REGEX_WINDOW_TEXT      1
#define RULE_REGEX_WINDOW_CLASS     (1 << 8)
#define RULE_REGEX_EXE_FILENAME     (1 << 16)
#define RULE_SHOW_NOTIFICATION      (1 << 24)
#define RULE_ON_MINIMIZE            (1 << 1)
#define RULE_AUTO_OFF               (1 << 2)

// Auto hiding window rule
#pragma warning(push)
#pragma warning(disable:4200)
typedef struct HIDING_RULE {
    size_t size;
    DWORD flag;
    TCHAR ruleData[0];
} HIDING_RULE;
#pragma warning(push)

typedef std::unordered_set<HWND> HWND_SET;

typedef std::vector<HIDING_RULE*> HIDING_RULES;

// Current execution context
typedef struct TRCONTEXT {
    HIDE_TYPE hideType;
    BOOL autorun;
    BOOL autoHiding;
    HOTKEY hotkey;
    HOTKEY hotkey2;
    HOTKEY hotkey3;
    HWINEVENTHOOK hook;
    HICON mainIcon;
    HINSTANCE instance;
    LPTSTR cmdLine;
    HWND mainWindow;
    HIDDEN_WINDOW icons[MAXIMUM_WINDOWS];
    HMENU trayMenu;
    int iconIndex; // How many windows are currently hidden
    HWND_SET freeWindows;
    HWND_SET hiddenWindows;
    HIDING_RULES hidingRules;
} TRCONTEXT;

int reviseHiddenWindowIcon(TRCONTEXT* context);
bool minimizeWindow(TRCONTEXT* context, HWND currWin, bool restored = false);
bool restoreWindow(TRCONTEXT* context, UINT xID, HWND hwnd = NULL);
PTCHAR getHotkeyText(PTCHAR text, rsize_t textSize, UINT modifiers, UINT vkey);
bool tryRegisterHotkey(HWND hwnd, int id, UINT modifiers, UINT vkey);
HICON getWindowIcon(const TRCONTEXT* context, HWND hwnd);
BOOL notifyHidingWindow(TRCONTEXT* context, HWND hwnd);

TRCONTEXT* AppContext();

#ifdef TRAYMON_MAIN
IsTopLevelWindow isTopLevelWindow = NULL;
I18n i18n;
#else
extern IsTopLevelWindow isTopLevelWindow;
extern I18n i18n;
#endif
