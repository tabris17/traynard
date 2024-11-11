#pragma once
#include <windows.h>


#define VK_Z_KEY 0x5A
// These keys are used to send windows to tray
#define TRAY_KEY VK_Z_KEY
#define MOD_KEY MOD_WIN + MOD_SHIFT

#define WM_ICON 0x1C0A
#define WM_OURICON 0x1C0B
#define SHOW_ALL_ID 0x97
#define OPTIONS_ID 0x98
#define SEPARATOR_ID 0x99
#define EXIT_ID 0x100
#define MAXIMUM_WINDOWS 100
#define HIDE_WINDOW_HOTKEY_ID 0
#define MAX_MSG 1024

#define APP_NAME "Traymond"
#define SAVE_FILE_NAME "traymond.dat"
#define MUTEX_NAME "traymond_mutex"
#define REG_KEY_SOFTWARE "SOFTWARE\\Traymond"
#define REG_KEY_RUN "SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\Run"

#define MENU_RESTORE_ALL_WINDOWS "恢复所有窗口"
#define MENU_OPTIONS "选项"
#define MENU_EXIT "退出"

#define MSG_HOTKEY_ERROR "无法注册系统热键，可能已被占用。"
#define MSG_MUTEX_ERROR "创建互斥对象失败，无法启动程序。"
#define MSG_ALREADY_RUNNING "程序已经有实例在运行。"
#define MSG_SAVE_FILE_ERROR "无法创建保存文件。"
#define MSG_TOO_MANY_HIDDEN_WINDOWS "隐藏太多窗口，请先释放一些。"
#define MSG_RESTORE_FROM_UNEXPECTED_TERMINATION "程序先前意外终止。恢复 %d 个托盘图标。"

typedef struct HIDE_WINDOW_HOTKEY {
    UINT modifiers;
    UINT vkey;
} HIDE_WINDOW_HOTKEY;

// Stores hidden window record.
typedef struct HIDDEN_WINDOW {
    NOTIFYICONDATA icon;
    HWND window;
} HIDDEN_WINDOW;

// Current execution context
typedef struct TRCONTEXT {
    BOOL autorun;
    HIDE_WINDOW_HOTKEY hotkey;
    HICON mainIcon;
    HINSTANCE instance;
    LPSTR cmdLine;
    HWND mainWindow;
    HIDDEN_WINDOW icons[MAXIMUM_WINDOWS];
    HMENU trayMenu;
    int iconIndex; // How many windows are currently hidden
} TRCONTEXT;
