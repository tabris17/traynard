//
#pragma once

#define TEXT_TRAY _T("系统托盘")
#define TEXT_MENU _T("右键菜单")
#define TEXT_HOTKEY_HIDE _T("隐藏窗口热键")
#define TEXT_HOTKEY_MENU _T("菜单热键")
#define TEXT_COL_KEY _T("按键")
#define TEXT_COL_ACTION _T("行为")
#define TEXT_ACT_1 _T("最小化前台窗口")
#define TEXT_ACT_2 _T("弹出最小化窗口列表")
#define TEXT_ACT_3 _T("还原最后一个窗口")

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
