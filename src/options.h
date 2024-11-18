//
#pragma once

#define COMBO_TEXT_TRAY _T("系统托盘")
#define COMBO_TEXT_MENU _T("右键菜单")

void showOptionsDlg(TRCONTEXT* context);
void loadOptions(TRCONTEXT* context);
void saveOptions(TRCONTEXT* context);
UINT HotkeyToMod(UINT fsModifiers);
UINT ModToHotkey(UINT fsModifiers);
