#include <windows.h>
#include <windowsx.h>
#include <commctrl.h>
#include <string>

#include "resource.h"
#include "logging.h"
#include "traymond.h"
#include "options.h"
#include "rules.h"


UINT HotkeyToMod(UINT fsModifiers) 
{
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


UINT ModToHotkey(UINT fsModifiers) 
{
    if ((fsModifiers & MOD_SHIFT) && !(fsModifiers & MOD_ALT)) {
        fsModifiers &= ~MOD_SHIFT;
        fsModifiers |= HOTKEYF_SHIFT;
    } else if (!(fsModifiers & MOD_SHIFT) && (fsModifiers & MOD_ALT)) {
        fsModifiers &= ~MOD_ALT;
        fsModifiers |= HOTKEYF_ALT;
    }
    return fsModifiers;
}


void loadOptions(TRCONTEXT* context) 
{
    context->hotkey.modifiers = MOD_KEY;
    context->hotkey.vkey = TRAY_KEY;
    context->autorun = FALSE;
    context->hideType = HideTray;
    context->autoHiding = FALSE;
    context->hook = NULL;

    DWORD data = 0, size = sizeof(DWORD);
    if (ERROR_SUCCESS == RegGetValue(HKEY_CURRENT_USER, REG_KEY_SOFTWARE, _T("Hotkey"), RRF_RT_REG_DWORD, NULL, &data, &size)) {
        auto vkey = LOWORD(data), modifiers = HIWORD(data);
        if (vkey > 0 && modifiers > 0) {
            context->hotkey.modifiers = modifiers;
            context->hotkey.vkey = vkey;
        }
    }

    if (ERROR_SUCCESS == RegGetValue(HKEY_CURRENT_USER, REG_KEY_SOFTWARE, _T("HideType"), RRF_RT_REG_DWORD, NULL, &data, &size)) {
        context->hideType = data ? HideMenu : HideTray;
    }

    if (ERROR_SUCCESS == RegGetValue(HKEY_CURRENT_USER, REG_KEY_SOFTWARE, _T("AutoHiding"), RRF_RT_REG_DWORD, NULL, &data, &size)) {
        context->autoHiding = (BOOL)data;
    }

    if (ERROR_SUCCESS == RegGetValue(HKEY_CURRENT_USER, REG_KEY_RUN, APP_NAME, RRF_RT_REG_SZ, NULL, NULL, NULL)) {
        context->autorun = TRUE;
    }
}


void saveOptions(TRCONTEXT* context) 
{
    HKEY regKey = NULL;

    if (ERROR_SUCCESS == RegCreateKey(HKEY_CURRENT_USER, REG_KEY_SOFTWARE, &regKey)) {
        DWORD data = MAKELONG(context->hotkey.vkey, context->hotkey.modifiers);
        RegSetValueEx(regKey, _T("Hotkey"), 0, REG_DWORD, (BYTE*)&data, sizeof(DWORD));
        data = context->hideType;
        RegSetValueEx(regKey, _T("HideType"), 0, REG_DWORD, (BYTE*)&data, sizeof(DWORD));
        data = context->autoHiding;
        RegSetValueEx(regKey, _T("AutoHiding"), 0, REG_DWORD, (BYTE*)&data, sizeof(DWORD));
        RegCloseKey(regKey);
    }

    if (ERROR_SUCCESS == RegOpenKey(HKEY_CURRENT_USER, REG_KEY_RUN, &regKey)) {
        if (context->autorun) {
            RegSetValueEx(regKey, APP_NAME, 0, REG_SZ, (BYTE*)context->cmdLine, _tcslen(context->cmdLine) * sizeof(TCHAR));
        }
        else {
            RegDeleteValue(regKey, APP_NAME);
        }
        RegCloseKey(regKey);
    }
}


static BOOL setOptions(HWND hwnd, TRCONTEXT* context, WPARAM wParam) 
{
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
    context->autoHiding = IsDlgButtonChecked(hwnd, IDC_CHECK_AUTO_HIDING);
    context->hideType = ComboBox_GetCurSel(GetDlgItem(hwnd, IDC_COMBO_HIDE_TYPE)) ? HideMenu : HideTray;
    reviseHiddenWindowIcon(context);
    saveOptions(context);
    return EndDialog(hwnd, wParam);
}


static BOOL initDialog(HWND hwnd, TRCONTEXT* context) 
{
    SetWindowLongPtr(hwnd, GWLP_USERDATA, reinterpret_cast<LONG>(context));
    SendMessage(hwnd, WM_SETICON, TRUE, (LPARAM)context->mainIcon);
    SendMessage(hwnd, WM_SETICON, FALSE, (LPARAM)context->mainIcon);

    HWND hotkeyEdit = GetDlgItem(hwnd, IDC_EDIT_HOTKEY);
    TCHAR hotkeyText[MAX_HOTKEY_TEXT] = { NULL };
    getHotkeyText(hotkeyText, _countof(hotkeyText), context->hotkey.modifiers, context->hotkey.vkey);
    SetWindowText(hotkeyEdit, hotkeyText);
    CheckDlgButton(hwnd, IDC_CHECK_AUTORUN, context->autorun);
    CheckDlgButton(hwnd, IDC_CHECK_AUTO_HIDING, context->autoHiding);
    Button_Enable(GetDlgItem(hwnd, IDC_BUTTON_WNDLST), context->autoHiding);

    HWND hideTypeCombo = GetDlgItem(hwnd, IDC_COMBO_HIDE_TYPE);
    ComboBox_AddItemData(hideTypeCombo, COMBO_TEXT_TRAY);
    ComboBox_AddItemData(hideTypeCombo, COMBO_TEXT_MENU);
    ComboBox_SetCurSel(hideTypeCombo, context->hideType);

    return TRUE;
}


static BOOL CALLBACK DialogProc(HWND hwndDlg, UINT message, WPARAM wParam, LPARAM lParam) 
{
    TRCONTEXT* context = reinterpret_cast<TRCONTEXT*>(GetWindowLongPtr(hwndDlg, GWLP_USERDATA));
    switch (message) {
    case WM_COMMAND:
        switch (LOWORD(wParam)) {
        case IDOK:
            return setOptions(hwndDlg, context, wParam);
        case IDCANCEL:
            return EndDialog(hwndDlg, wParam);
        case IDC_CHECK_AUTO_HIDING:
            return Button_Enable(GetDlgItem(hwndDlg, IDC_BUTTON_WNDLST), IsDlgButtonChecked(hwndDlg, IDC_CHECK_AUTO_HIDING));
        case IDC_BUTTON_WNDLST:
            showRulesDlg(hwndDlg, context);
            return TRUE;
        }
        break;
    case WM_INITDIALOG:
        context = reinterpret_cast<TRCONTEXT*>(lParam);
        return initDialog(hwndDlg, context);
    }
    return FALSE;
}


void showOptionsDlg(HWND parent, TRCONTEXT* context)
{
    static bool dialogOpened = false;
    if (dialogOpened) {
        return;
    }

    dialogOpened = true;
    DialogBoxParam(context->instance,
                   MAKEINTRESOURCE(IDD_DIALOG_OPTIONS),
                   parent,
                   (DLGPROC)DialogProc,
                   (LPARAM)context);
    dialogOpened = false;
}
