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


static void loadHotkey(PTCHAR key, HOTKEY *hotkey)
{
    DWORD data = 0, size = sizeof(DWORD);
    if (ERROR_SUCCESS == RegGetValue(HKEY_CURRENT_USER, REG_KEY_SOFTWARE, key, RRF_RT_REG_DWORD, NULL, &data, &size)) {
        auto vkey = LOWORD(data), modifiers = HIWORD(data);
        if (vkey > 0 && modifiers > 0) {
            hotkey->modifiers = modifiers;
            hotkey->vkey = vkey;
        }
    }
}


void loadOptions(TRCONTEXT* context) 
{
    context->hotkey.modifiers = MOD_KEY;
    context->hotkey.vkey = TRAY_KEY;
    context->hotkey2.modifiers = 0;
    context->hotkey2.vkey = 0;
    context->hotkey3.modifiers = 0;
    context->hotkey3.vkey = 0;
    context->autorun = FALSE;
    context->hideType = HideTray;
    context->autoHiding = FALSE;
    context->hook = NULL;

    loadHotkey(_T("Hotkey"), &context->hotkey);
    loadHotkey(_T("Hotkey2"), &context->hotkey2);
    loadHotkey(_T("Hotkey3"), &context->hotkey3);

    DWORD data = 0, size = sizeof(DWORD);

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
        data = MAKELONG(context->hotkey2.vkey, context->hotkey2.modifiers);
        RegSetValueEx(regKey, _T("Hotkey2"), 0, REG_DWORD, (BYTE*)&data, sizeof(DWORD));
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


static HOTKEY_OPTIONS readHotkey(HWND dialog, int hotkeyId, UINT *modifiers, UINT *vkey)
{
    static struct {
        int ctrlHotkeyId;
        int ctrlCheckboxId;
        PTCHAR caption;
    } hotkeyInfoTable[] = {
        { IDC_HOTKEY , IDC_CHECK_USE_WIN, TEXT_HOTKEY_HIDE },
        { IDC_HOTKEY_2, IDC_CHECK_USE_WIN_2, TEXT_HOTKEY_MENU },
    };
    auto hotkeyInfo = hotkeyInfoTable + hotkeyId;
    DWORD result = SendMessage(GetDlgItem(dialog, hotkeyInfo->ctrlHotkeyId), HKM_GETHOTKEY, 0, 0);
    *vkey = LOBYTE(LOWORD(result));
    *modifiers = HotkeyToMod(HIBYTE(LOWORD(result)));
    IsDlgButtonChecked(dialog, hotkeyInfo->ctrlCheckboxId) && (*modifiers |= MOD_WIN);
    if (*vkey == 0 && *modifiers == 0) {
        return HOTKEY_OPTIONS_IGNORE;
    }

    TCHAR errMsg[MAX_MSG]{ NULL };
    if (*vkey == 0 || *modifiers == 0) {
        _sntprintf_s(errMsg, _countof(errMsg) - 1, MSG_INVALID_HOTKEY, hotkeyInfo->caption);
        MessageBox(dialog, errMsg, APP_NAME, MB_OK | MB_ICONWARNING);
        return HOTKEY_OPTIONS_INVALID;
    }

    if (!tryRegisterHotkey(dialog, TEST_HOTKEY_ID, *modifiers, *vkey)) {
        return HOTKEY_OPTIONS_INVALID;
    }
    UnregisterHotKey(dialog, TEST_HOTKEY_ID);
    
    return HOTKEY_OPTIONS_SUCCESS;
}


static BOOL setOptions(HWND hwnd, TRCONTEXT* context, WPARAM wParam) 
{
    UINT modifiers, vkey, modifiers2, vkey2;
    auto hotkeyOptions = readHotkey(hwnd, IDHOT_HIDE_WINDOW, &modifiers, &vkey),
         hotkeyOptions2 = readHotkey(hwnd, IDHOT_POPUP_ICONS, &modifiers2, &vkey2);
    if (HOTKEY_OPTIONS_INVALID == hotkeyOptions || HOTKEY_OPTIONS_INVALID == hotkeyOptions2) {
        return FALSE;
    }

    if (HOTKEY_OPTIONS_SUCCESS == hotkeyOptions) {
        UnregisterHotKey(context->mainWindow, IDHOT_HIDE_WINDOW);
        RegisterHotKey(context->mainWindow, IDHOT_HIDE_WINDOW, modifiers | MOD_NOREPEAT, vkey);
        context->hotkey.modifiers = modifiers;
        context->hotkey.vkey = vkey;
    }
    if (HOTKEY_OPTIONS_SUCCESS == hotkeyOptions2) {
        UnregisterHotKey(context->mainWindow, IDHOT_POPUP_ICONS);
        RegisterHotKey(context->mainWindow, IDHOT_POPUP_ICONS, modifiers2 | MOD_NOREPEAT, vkey2);
        context->hotkey2.modifiers = modifiers2;
        context->hotkey2.vkey = vkey2;
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
    Button_Enable(GetDlgItem(hwnd, IDC_BUTTON_RULES), context->autoHiding);

    HWND hideTypeCombo = GetDlgItem(hwnd, IDC_COMBO_HIDE_TYPE);
    ComboBox_AddItemData(hideTypeCombo, TEXT_TRAY);
    ComboBox_AddItemData(hideTypeCombo, TEXT_MENU);
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
            return Button_Enable(GetDlgItem(hwndDlg, IDC_BUTTON_RULES), IsDlgButtonChecked(hwndDlg, IDC_CHECK_AUTO_HIDING));
        case IDC_BUTTON_RULES:
            showRulesDlg(hwndDlg, context);
            return TRUE;
        }
        break;
    case WM_INITDIALOG:
        return initDialog(hwndDlg, reinterpret_cast<TRCONTEXT*>(lParam));
    }
    return FALSE;
}


INT_PTR showOptionsDlg(TRCONTEXT* context)
{
    static bool dialogOpened = false;
    if (dialogOpened) {
        return FALSE;
    }

    dialogOpened = true;
    auto result = DialogBoxParam(
        context->instance,
        MAKEINTRESOURCE(IDD_OPTIONS),
        HWND_DESKTOP,
        (DLGPROC)DialogProc,
        (LPARAM)context
    );
    dialogOpened = false;
    return result;
}
