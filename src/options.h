//
#pragma once

#define TEXT_TRAY _T("系统托盘")
#define TEXT_MENU _T("右键菜单")
#define TEXT_HOTKEY_HIDE _T("隐藏窗口热键")
#define TEXT_HOTKEY_MENU _T("菜单热键")

INT_PTR showOptionsDlg(TRCONTEXT* context);
void loadOptions(TRCONTEXT* context);
void saveOptions(TRCONTEXT* context);
UINT HotkeyToMod(UINT fsModifiers);
UINT ModToHotkey(UINT fsModifiers);

typedef enum {
    HOTKEY_OPTIONS_IGNORE,
    HOTKEY_OPTIONS_INVALID,
    HOTKEY_OPTIONS_SUCCESS,
} HOTKEY_OPTIONS;
