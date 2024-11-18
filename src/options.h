//
#pragma once

#define COMBO_TEXT_TRAY _T("ÏµÍ³ÍÐÅÌ")
#define COMBO_TEXT_MENU _T("ÓÒ¼ü²Ëµ¥")

void showOptionsDlg(TRCONTEXT* context);
void loadOptions(TRCONTEXT* context);
void saveOptions(TRCONTEXT* context);
UINT HotkeyToMod(UINT fsModifiers);
UINT ModToHotkey(UINT fsModifiers);
