#include <windows.h>
#include <commctrl.h>
#include <string>
#include "traymond.h"
#include "resource.h"


UINT HotkeyToMod(UINT fsModifiers) {
    if ((fsModifiers & HOTKEYF_SHIFT) && !(fsModifiers & HOTKEYF_ALT)) {
        fsModifiers &= ~HOTKEYF_SHIFT;
        fsModifiers |= MOD_SHIFT;
    }
    else if (!(fsModifiers & HOTKEYF_SHIFT) && (fsModifiers & HOTKEYF_ALT)) {
        fsModifiers &= ~HOTKEYF_ALT;
        fsModifiers |= MOD_ALT;
    }
    return fsModifiers;
}


UINT ModToHotkey(UINT fsModifiers) {
    if ((fsModifiers & MOD_SHIFT) && !(fsModifiers & MOD_ALT)) {
        fsModifiers &= ~MOD_SHIFT;
        fsModifiers |= HOTKEYF_SHIFT;
    } else if (!(fsModifiers & MOD_SHIFT) && (fsModifiers & MOD_ALT)) {
        fsModifiers &= ~MOD_ALT;
        fsModifiers |= HOTKEYF_ALT;
    }
    return fsModifiers;
}


void loadOptions(TRCONTEXT* context) {
    context->hotkey.modifiers = MOD_KEY;
    context->hotkey.vkey = TRAY_KEY;
    context->autorun = FALSE;

    DWORD data = 0, size = sizeof(DWORD);
    if (ERROR_SUCCESS == RegGetValue(HKEY_CURRENT_USER, REG_KEY_SOFTWARE, "Hotkey", RRF_RT_REG_DWORD, NULL, &data, &size)) {
        auto vkey = LOWORD(data), modifiers = HIWORD(data);
        if (vkey > 0 && modifiers > 0) {
            context->hotkey.modifiers = modifiers;
            context->hotkey.vkey = vkey;
        }
    }

    if (ERROR_SUCCESS == RegGetValue(HKEY_CURRENT_USER, REG_KEY_RUN, APP_NAME, RRF_RT_REG_SZ, NULL, NULL, NULL)) {
        context->autorun = TRUE;
    }
}


void saveOptions(TRCONTEXT* context) {
    HKEY regKey = NULL;

    if (ERROR_SUCCESS == RegCreateKey(HKEY_CURRENT_USER, REG_KEY_SOFTWARE, &regKey)) {
        DWORD data = MAKELONG(context->hotkey.vkey, context->hotkey.modifiers);
        RegSetValueEx(regKey, "Hotkey", 0, REG_DWORD, (BYTE*)&data, sizeof(DWORD));
        RegCloseKey(regKey);
    }

    if (ERROR_SUCCESS == RegOpenKey(HKEY_CURRENT_USER, REG_KEY_RUN, &regKey)) {
        if (context->autorun) {
            RegSetValueEx(regKey, APP_NAME, 0, REG_SZ, (BYTE*)context->cmdLine, strlen(context->cmdLine));
        }
        else {
            RegDeleteValue(regKey, APP_NAME);
        }
        RegCloseKey(regKey);
    }
}


static BOOL initDialog(HWND hwnd, TRCONTEXT* context) {
    SetWindowLongPtr(hwnd, GWLP_USERDATA, reinterpret_cast<LONG>(context));
    SendMessage(hwnd, WM_SETICON, TRUE, (LPARAM)context->mainIcon);
    SendMessage(hwnd, WM_SETICON, FALSE, (LPARAM)context->mainIcon);
    HWND hotkeyEdit = GetDlgItem(hwnd, IDC_EDIT_HOTKEY);
    char hotkeyText[MAX_MSG] = { NULL };
    UINT modifiers = context->hotkey.modifiers, vkey = context->hotkey.vkey;
    if (modifiers & MOD_WIN) {
        strncat_s(hotkeyText, "Win", MAX_MSG);
    }
    if (modifiers & MOD_CONTROL) {
        strncat_s(hotkeyText, strnlen(hotkeyText, MAX_MSG) ? " + Ctrl" : "Ctrl", MAX_MSG);
    }
    if (modifiers & MOD_SHIFT) {
        strncat_s(hotkeyText, strnlen(hotkeyText, MAX_MSG) ? " + Shift" : "Shift", MAX_MSG);
    }
    if (modifiers & MOD_ALT) {
        strncat_s(hotkeyText, strnlen(hotkeyText, MAX_MSG) ? " + Alt" : "Alt", MAX_MSG);
    }
    size_t l = strnlen(hotkeyText, MAX_MSG);
    if (l > 0) {
        strncat_s(hotkeyText, " + ", MAX_MSG);
        l += 3;
    }
    GetKeyNameText(MapVirtualKey(vkey, MAPVK_VK_TO_VSC) << 16, hotkeyText + l, MAX_MSG - l);
    SetWindowText(hotkeyEdit, hotkeyText);
    CheckDlgButton(hwnd, IDC_CHECK_AUTORUN, context->autorun);
    return TRUE;
}


static BOOL setOptions(HWND hwnd, TRCONTEXT* context, WPARAM wParam) {
    DWORD result = SendMessage(GetDlgItem(hwnd, IDC_HOTKEY), HKM_GETHOTKEY, 0, 0);
    UINT vkey = LOBYTE(LOWORD(result));
    UINT modifiers = HotkeyToMod(HIBYTE(LOWORD(result)));

    if (IsDlgButtonChecked(hwnd, IDC_CHECK_USE_WIN)) {
        modifiers |= MOD_WIN;
    }

    if (vkey > 0 && modifiers > 0) {
        UnregisterHotKey(context->mainWindow, HIDE_WINDOW_HOTKEY_ID);
        if (!RegisterHotKey(context->mainWindow, HIDE_WINDOW_HOTKEY_ID, modifiers | MOD_NOREPEAT, vkey)) {
            RegisterHotKey(context->mainWindow, HIDE_WINDOW_HOTKEY_ID, context->hotkey.modifiers | MOD_NOREPEAT, context->hotkey.vkey);
            MessageBox(hwnd, MSG_HOTKEY_ERROR, APP_NAME, MB_OK | MB_ICONERROR);
            return FALSE;
        }
        context->hotkey.modifiers = modifiers;
        context->hotkey.vkey = vkey;
    }
    
    context->autorun = IsDlgButtonChecked(hwnd, IDC_CHECK_AUTORUN);
    saveOptions(context);
    return EndDialog(hwnd, wParam);
}


static BOOL CALLBACK OptionsDialogProc(HWND hwndDlg, UINT message, WPARAM wParam, LPARAM lParam) {
    TRCONTEXT* context = reinterpret_cast<TRCONTEXT*>(GetWindowLongPtr(hwndDlg, GWLP_USERDATA));
    switch (message) {
    case WM_COMMAND:
        switch (LOWORD(wParam)) {
        case IDOK:
            return setOptions(hwndDlg, context, wParam);
        case IDCANCEL:
            return EndDialog(hwndDlg, wParam);
        }
        break;
    case WM_INITDIALOG:
        context = reinterpret_cast<TRCONTEXT*>(lParam);
        return initDialog(hwndDlg, context);
    }
    return FALSE;
}


void showOptionsDlg(TRCONTEXT* context) {
    if (DialogBoxParam(context->instance, 
                       MAKEINTRESOURCE(IDD_DIALOG_OPTIONS), 
                       NULL, 
                       (DLGPROC)OptionsDialogProc, 
                       (LPARAM)context) != IDOK) {
        return;
    }
}
