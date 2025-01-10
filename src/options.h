#pragma once

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
