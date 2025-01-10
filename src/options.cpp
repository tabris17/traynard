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
        data = MAKELONG(context->hotkey3.vkey, context->hotkey3.modifiers);
        RegSetValueEx(regKey, _T("Hotkey3"), 0, REG_DWORD, (BYTE*)&data, sizeof(DWORD));
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
    UINT hotkeyIds[] = { IDHOT_HIDE_WINDOW, IDHOT_POPUP_ICONS, IDHOT_RESTORE_LAST_WINDOW };
    HOTKEY* hotkeys[] = { &context->hotkey, &context->hotkey2, &context->hotkey3 };
    HOTKEY readHotkeys[_countof(hotkeyIds)]{0};
    auto listView = GetDlgItem(hwnd, IDC_HOTKEY_LIST);
    LVITEM lvi{};
    lvi.mask = LVIF_PARAM;
    lvi.iSubItem = 0;
    for (int i = 0; i < _countof(readHotkeys); i++) {
        auto readHotkey = &readHotkeys[i];
        auto hotkey = hotkeys[i];
        lvi.iItem = i;
        if (ListView_GetItem(listView, &lvi)) {
            readHotkey->modifiers = HotkeyToMod(HIBYTE(LOWORD(lvi.lParam)));
            readHotkey->vkey = LOBYTE(LOWORD(lvi.lParam));
            if (hotkey->modifiers == readHotkey->modifiers && hotkey->vkey == readHotkey->vkey) {
                hotkeys[i] = nullptr;
                continue;
            }
            if (lvi.lParam == 0) {
                continue;
            }
            if (!tryRegisterHotkey(hwnd, TEST_HOTKEY_ID, readHotkey->modifiers, readHotkey->vkey)) {
                return FALSE;
            }
            UnregisterHotKey(hwnd, TEST_HOTKEY_ID);
        }
    }
    for (int i = 0; i < _countof(hotkeys); i++) {
        auto hotkeyId = hotkeyIds[i];
        auto readHotkey = readHotkeys[i];
        auto hotkey = hotkeys[i];
        if (hotkey == nullptr) {
            continue;
        }
        UnregisterHotKey(context->mainWindow, hotkeyId);
        if (readHotkey.modifiers > 0 && readHotkey.vkey > 0) {
            RegisterHotKey(context->mainWindow, hotkeyId, readHotkey.modifiers | MOD_NOREPEAT, readHotkey.vkey);
        }
        hotkey->modifiers = readHotkey.modifiers;
        hotkey->vkey = readHotkey.vkey;
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

    auto listView = GetDlgItem(hwnd, IDC_HOTKEY_LIST);
    ListView_SetExtendedListViewStyle(listView, LVS_EX_BORDERSELECT | LVS_EX_FULLROWSELECT);
    LVCOLUMN lvc{};
    lvc.mask = LVCF_FMT | LVCF_WIDTH | LVCF_TEXT;
    lvc.fmt = LVCFMT_LEFT;
    lvc.cx = 148;
    lvc.pszText = i18n[IDS_COL_KEY];
    ListView_InsertColumn(listView, 0, &lvc);
    lvc.mask = LVCF_FMT | LVCF_WIDTH | LVCF_TEXT;
    lvc.fmt = LVCFMT_LEFT;
    lvc.cx = 120;
    lvc.pszText = i18n[IDS_COL_ACTION];
    ListView_InsertColumn(listView, 1, &lvc);

    TCHAR hotkeyText[MAX_HOTKEY_TEXT]{ NULL };
    PTCHAR actions[] = { i18n[IDS_ACT_1], i18n[IDS_ACT_2], i18n[IDS_ACT_3] };
    LVITEM lvi{};
    lvi.mask = LVIF_TEXT;
    for (int i = 0; i < _countof(actions); i++) {
        lvi.iSubItem = 0;
        lvi.iItem = i;
        lvi.pszText = NULL;
        ListView_InsertItem(listView, &lvi);
        lvi.iSubItem = 1;
        lvi.pszText = actions[i];
        ListView_SetItem(listView, &lvi);
    }

    HOTKEY* hotkeys[] = { &context->hotkey, &context->hotkey2, &context->hotkey3 };
    lvi.mask = LVIF_TEXT | LVIF_PARAM;
    lvi.iSubItem = 0;
    for (int i = 0; i < _countof(hotkeys); i++) {
        auto hotkey = hotkeys[i];
        if (hotkey->modifiers == 0 || hotkey->vkey == 0) {
            continue;
        }
        getHotkeyText(hotkeyText, _countof(hotkeyText), hotkey->modifiers, hotkey->vkey);
        lvi.pszText = hotkeyText;
        lvi.iItem = i;
        lvi.lParam = MAKEWORD(hotkey->vkey, ModToHotkey(hotkey->modifiers));
        ListView_SetItem(listView, &lvi);
    }

    CheckDlgButton(hwnd, IDC_CHECK_AUTORUN, context->autorun);
    CheckDlgButton(hwnd, IDC_CHECK_AUTO_HIDING, context->autoHiding);
    Button_Enable(GetDlgItem(hwnd, IDC_BUTTON_RULES), context->autoHiding);

    auto hideTypeCombo = GetDlgItem(hwnd, IDC_COMBO_HIDE_TYPE);
    ComboBox_AddItemData(hideTypeCombo, i18n[IDS_TRAY]);
    ComboBox_AddItemData(hideTypeCombo, i18n[IDS_MENU]);
    ComboBox_SetCurSel(hideTypeCombo, context->hideType);

    return TRUE;
}


static BOOL onHotkeyListItemChanged(HWND hwndDlg)
{
    auto hotkeyListView = GetDlgItem(hwndDlg, IDC_HOTKEY_LIST);
    auto hotkeyEdit = GetDlgItem(hwndDlg, IDC_HOTKEY);
    auto hotkeyUseWinCheck = GetDlgItem(hwndDlg, IDC_CHECK_USE_WIN);
    auto selectedIndex = ListView_GetNextItem(GetDlgItem(hwndDlg, IDC_HOTKEY_LIST), -1, LVNI_SELECTED);
    if (selectedIndex < 0) {
        return FALSE;
    }

    LVITEM lvi{};
    lvi.mask = LVIF_PARAM;
    lvi.iItem = selectedIndex;
    lvi.iSubItem = 0;
    if (!ListView_GetItem(hotkeyListView, &lvi)) {
        return FALSE;
    }

    EnableWindow(hotkeyEdit, TRUE);
    EnableWindow(hotkeyUseWinCheck, TRUE);

    SendMessage(hotkeyEdit, HKM_SETHOTKEY, lvi.lParam, 0);

    auto modifers = HotkeyToMod(HIBYTE(LOWORD(lvi.lParam)));
    return CheckDlgButton(hwndDlg, IDC_CHECK_USE_WIN, (modifers & MOD_WIN) ? BST_CHECKED : BST_UNCHECKED);
}


static BOOL onHotkeyChanged(HWND hwndDlg)
{
    auto hotkeyListView = GetDlgItem(hwndDlg, IDC_HOTKEY_LIST);
    auto hotkeyEdit = GetDlgItem(hwndDlg, IDC_HOTKEY);
    auto selectedIndex = ListView_GetNextItem(GetDlgItem(hwndDlg, IDC_HOTKEY_LIST), -1, LVNI_SELECTED);
    if (selectedIndex < 0) {
        return FALSE;
    }

    UINT vkey, modifiers;
    DWORD result = SendMessage(hotkeyEdit, HKM_GETHOTKEY, 0, 0);
    vkey = LOBYTE(LOWORD(result));
    modifiers = HotkeyToMod(HIBYTE(LOWORD(result)));
    if (BST_UNCHECKED != IsDlgButtonChecked(hwndDlg, IDC_CHECK_USE_WIN)) {
        modifiers |= MOD_WIN;
    }
    else {
        modifiers &= ~MOD_WIN;
    }

    if (vkey + modifiers > 0 && (vkey == 0 || modifiers == 0)) {
        return FALSE;
    }
    TCHAR hotkeyText[MAX_HOTKEY_TEXT]{ NULL };
    LVITEM lvi{};
    lvi.mask = LVIF_TEXT | LVIF_PARAM;
    lvi.iSubItem = 0;
    lvi.pszText = hotkeyText;
    lvi.iItem = selectedIndex;
    getHotkeyText(hotkeyText, _countof(hotkeyText), modifiers, vkey);
    lvi.lParam = MAKEWORD(vkey, ModToHotkey(modifiers));
    ListView_SetItem(hotkeyListView, &lvi);
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
        case IDC_HOTKEY:
            if (HIWORD(wParam) == EN_CHANGE) {
                return onHotkeyChanged(hwndDlg);
            }
            break;
        case IDC_CHECK_USE_WIN:
            return onHotkeyChanged(hwndDlg);
        }
        break;
    case WM_NOTIFY:
        switch (LOWORD(wParam)) {
        case IDC_HOTKEY_LIST:
            if (LVN_ITEMCHANGED == reinterpret_cast<LPNMHDR>(lParam)->code &&
                (reinterpret_cast<LPNMLISTVIEW>(lParam)->uChanged & LVIF_STATE) &&
                (reinterpret_cast<LPNMLISTVIEW>(lParam)->uNewState & LVIS_SELECTED)) {

                onHotkeyListItemChanged(hwndDlg);
            }
            break;
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
        i18n.lang(context->instance),
        MAKEINTRESOURCE(IDD_OPTIONS),
        HWND_DESKTOP,
        (DLGPROC)DialogProc,
        (LPARAM)context
    );
    dialogOpened = false;
    return result;
}
