#include <windows.h>
#include <windowsx.h>
#include <regex>

#include "resource.h"
#include "traymond.h"
#include "winevent.h"
#include "rules.h"

static bool serializeRules(HIDING_RULES* rules, LPBYTE data, _Inout_ size_t* size)
{
    size_t realSize = 0;

    for (HIDING_RULE* rule : *rules) {
        realSize += rule->size;
    }

    if (data) {
        if (realSize > *size) {
            return false;
        }
        auto offset = data;
        for (HIDING_RULE* rule : *rules) {
            memcpy(offset, rule, rule->size);
            offset += rule->size;
        }
    }
    else {
        *size = realSize;
    }
    return true;
}

static bool unserializeRules(HIDING_RULES* rules, LPBYTE data, size_t size)
{
    HIDING_RULE* rule = NULL;

    for (size_t i = 0; i < size; i += rule->size) {
        rule = (HIDING_RULE*)(data + i);
        if (i + rule->size > size) {
            return false;
        }
        HIDING_RULE* ruleDest = (HIDING_RULE*) new BYTE[rule->size];
        memcpy(ruleDest, rule, rule->size);
        rules->push_back(ruleDest);
    }
    return true;
}

bool saveRules(TRCONTEXT* context)
{
    bool result = false;
    HKEY regKey = NULL;

    if (ERROR_SUCCESS == RegCreateKey(HKEY_CURRENT_USER, REG_KEY_SOFTWARE, &regKey)) {
        size_t rulesSize = 0;
        if (serializeRules(&context->hidingRules, NULL, &rulesSize)) {
            BYTE* rules = new BYTE[rulesSize];
            if (serializeRules(&context->hidingRules, rules, &rulesSize)) {
                result = ERROR_SUCCESS == RegSetValueEx(regKey, _T("Rules"), 0, REG_BINARY, rules, rulesSize);
            }
            delete[] rules;
        }
        RegCloseKey(regKey);
    }
    return result;
}

bool loadRules(TRCONTEXT* context)
{
    bool result = false;
    DWORD dataSize = 0;

    clearRules(context);

    if (ERROR_SUCCESS == RegGetValue(HKEY_CURRENT_USER, REG_KEY_SOFTWARE, _T("Rules"), RRF_RT_REG_BINARY, NULL, NULL, &dataSize)) {
        BYTE* data = new BYTE[dataSize];
        if (ERROR_SUCCESS == RegGetValue(HKEY_CURRENT_USER, REG_KEY_SOFTWARE, _T("Rules"), RRF_RT_REG_BINARY, NULL, data, &dataSize)) {
            result = unserializeRules(&context->hidingRules, data, dataSize);
        }
        delete[] data;
    }
    return result;
}

static BOOL CALLBACK EnumWindowsProc(HWND hwnd, LPARAM lParam) {
    auto context = reinterpret_cast<TRCONTEXT*>(lParam);
    if (IsWindowVisible(hwnd) && matchRule(context, hwnd)) {
        minimizeWindow(context, hwnd);
    }
    return TRUE;
}

bool applyRules(TRCONTEXT* context)
{
    if (!context->autoHiding) {
        return false;
    }

    return (bool)EnumWindows(EnumWindowsProc, reinterpret_cast<LPARAM>(context));
}

bool clearRules(TRCONTEXT* context)
{
    if (context->hidingRules.empty()) { 
        return false; 
    }

    for (HIDING_RULE* rule : context->hidingRules) {
        delete[] (BYTE*)rule;
    }

    context->hidingRules.clear();
    return true;
}

inline static bool compareText(PTCHAR text, PTCHAR pattern, bool isRegex)
{
    if (isRegex) {
        try {
            TREGEX expr(pattern);
            return std::regex_match(text, expr);
        }
        catch (const std::regex_error&) {
            return false;
        }
    }
    else {
        return _tcscmp(text, pattern) == 0;
    }
    return false;
}

bool matchRule(TRCONTEXT* context, HWND hwnd)
{
    TCHAR windowText[MAX_WINDOW_TEXT] {};
    TCHAR className[MAX_CLASS_NAME] {};
    TCHAR exeFileName[MAX_PATH] {};
    if (!GetWindowText(hwnd, windowText, MAX_WINDOW_TEXT) ||
        !GetClassName(hwnd, className, MAX_CLASS_NAME) || 
        !GetWindowExeFileName(hwnd, exeFileName, MAX_PATH)) {

        return false;
    }
    for (auto rule : context->hidingRules) {
        auto text = rule->ruleData;
        text += _tcsclen(text) + 1;
        if (!compareText(windowText, text, rule->isWindowTextRegex)) {
            continue;
        }
        text += _tcsclen(text) + 1;
        if (!compareText(className, text, rule->isWindowClassNameRegex)) {
            continue;
        }
        text += _tcsclen(text) + 1;
        if (!compareText(exeFileName, text, rule->isExeFileNameRegex)) {
            continue;
        }
        return true;
    }
    return false;
}

static BOOL CALLBACK DialogProc(HWND hwndDlg, UINT message, WPARAM wParam, LPARAM lParam) 
{
    RuleEditor* editor;
    if (WM_INITDIALOG == message) {
        editor = reinterpret_cast<RuleEditor*>(lParam);
        editor->initialize(hwndDlg);
        return TRUE;
    }
    editor = reinterpret_cast<RuleEditor*>(GetWindowLongPtr(hwndDlg, GWLP_USERDATA));
    return editor->dispatchMessage(message, wParam, lParam);
}

void showRulesDlg(HWND parent, TRCONTEXT* context)
{
    static bool dialogOpened = false;
    if (dialogOpened) {
        return;
    }
    auto editor = RuleEditor(context);
    dialogOpened = true;
    DialogBoxParam(context->instance,
        MAKEINTRESOURCE(IDD_DIALOG_WNDLST),
        parent,
        (DLGPROC)DialogProc,
        (LPARAM)&editor);
    dialogOpened = false;
}

inline static bool testRegex(HWND hwnd, PTCHAR pattern)
{
    TCHAR errMsg[MAX_MSG]{};
    try {
        TREGEX expr(pattern);
    }
    catch (const std::regex_error&) {
        _sntprintf_s(errMsg, MAX_MSG, TEXT_INVALID_REGEX, pattern);
        MessageBox(hwnd, errMsg, APP_NAME, MB_OK | MB_ICONWARNING);
        return false;
    }
    return true;
}

HIDING_RULE* RuleEditor::newRule()
{
    size_t size = sizeof(HIDING_RULE);
    TCHAR ruleName[MAX_RULE_NAME], 
          windowText[MAX_WINDOW_TEXT], 
          windowClassName[MAX_CLASS_NAME],
          exeFileName[MAX_PATH];
    
    auto ruleNameSize = Edit_GetText(nameEdit, ruleName, MAX_RULE_NAME) + 1,
         windowTextSize = Edit_GetText(textEdit, windowText, MAX_WINDOW_TEXT) + 1,
         windowClassNameSize = Edit_GetText(classEdit, windowClassName, MAX_CLASS_NAME) + 1,
         exeFileNameSize = Edit_GetText(pathEdit, exeFileName, MAX_PATH) + 1;
    bool isWindowTextRegex = Button_GetCheck(textCheckBox) == BST_CHECKED,
         isWindowClassNameRegex = Button_GetCheck(classCheckBox) == BST_CHECKED,
         isExeFileNameRegex = Button_GetCheck(pathCheckBox) == BST_CHECKED;
    
    if (ruleNameSize == 1 || windowTextSize == 1 || windowClassNameSize == 1 || exeFileNameSize == 1) {
        MessageBox(window, TEXT_RULE_INFO_REQUIRED, APP_NAME, MB_OK | MB_ICONWARNING);
        return NULL;
    }

    if (isWindowTextRegex && !testRegex(window, windowText)) {
        return NULL;
    }
    if (isWindowClassNameRegex && !testRegex(window, windowClassName)) {
        return NULL;
    }
    if (isExeFileNameRegex && !testRegex(window, exeFileName)) {
        return NULL;
    }
    
    size += (ruleNameSize + windowTextSize + windowClassNameSize + exeFileNameSize) * sizeof(TCHAR);
    HIDING_RULE* rule = (HIDING_RULE*)new BYTE[size];
    rule->size = size;
    rule->isWindowTextRegex = isWindowTextRegex;
    rule->isWindowClassNameRegex = isWindowClassNameRegex;
    rule->isExeFileNameRegex = isExeFileNameRegex;
    PTCHAR ruleDataOffset = rule->ruleData;
    memcpy(ruleDataOffset, ruleName, ruleNameSize * sizeof(TCHAR));
    ruleDataOffset += ruleNameSize;
    memcpy(ruleDataOffset, windowText, windowTextSize * sizeof(TCHAR));
    ruleDataOffset += windowTextSize;
    memcpy(ruleDataOffset, windowClassName, windowClassNameSize * sizeof(TCHAR));
    ruleDataOffset += windowClassNameSize;
    memcpy(ruleDataOffset, exeFileName, exeFileNameSize * sizeof(TCHAR));
    return rule;
}

RuleEditor::RuleEditor(TRCONTEXT* context)
{
    this->context = context;
}

TRCONTEXT* RuleEditor::getContext()
{
    return context;
}

void RuleEditor::initialize(HWND hwnd)
{
    SetWindowLongPtr(hwnd, GWLP_USERDATA, reinterpret_cast<LONG>(this));
    window = hwnd;
    ruleList = GetDlgItem(hwnd, IDC_LIST_RULES);
    nameEdit = GetDlgItem(hwnd, IDC_EDIT_NAME),
    textEdit = GetDlgItem(hwnd, IDC_EDIT_TEXT),
    classEdit = GetDlgItem(hwnd, IDC_EDIT_CLASS),
    pathEdit = GetDlgItem(hwnd, IDC_EDIT_PATH);
    textCheckBox = GetDlgItem(hwnd, IDC_CHECK_REGEX_TEXT);
    classCheckBox = GetDlgItem(hwnd, IDC_CHECK_REGEX_CLASS);
    pathCheckBox = GetDlgItem(hwnd, IDC_CHECK_REGEX_PATH);
    saveButton = GetDlgItem(hwnd, IDSAVE);
    removeButton = GetDlgItem(hwnd, IDREMOVE);
    dropButton = GetDlgItem(hwnd, IDDROP);
    windowsCombo = GetDlgItem(hwnd, IDC_COMBO_WINDOWS);
    Edit_LimitText(nameEdit, MAX_RULE_NAME);
    Edit_LimitText(textEdit, MAX_WINDOW_TEXT);
    Edit_LimitText(classEdit, MAX_CLASS_NAME);
    Edit_LimitText(pathEdit, MAX_PATH);
    sync();
}

#pragma warning(push)
#pragma warning(disable:4100)
bool RuleEditor::dispatchMessage(UINT message, WPARAM wParam, LPARAM lParam)
#pragma warning(pop)
{
    switch (message) {
    case WM_COMMAND:
        switch (LOWORD(wParam)) {
        case IDHELP:
            ShellExecute(window, _T("open"), HELP_URL, NULL, NULL, SW_SHOWNORMAL);
            break;
        case IDDROP:
            drop();
            return TRUE;
        case IDCANCEL:
            return EndDialog(window, wParam);
        case IDNEW:
            return append();
        case IDREMOVE:
            return remove();
        case IDSAVE:
            return save();
        case IDC_LIST_RULES:
            switch (HIWORD(wParam)) {
            case LBN_SELCANCEL:
            case LBN_SELCHANGE:
                select();
                break;
            }
            break;
        case IDC_EDIT_NAME:
        case IDC_EDIT_TEXT:
        case IDC_EDIT_CLASS:
        case IDC_EDIT_PATH:
            if (HIWORD(wParam) == EN_CHANGE) {
                touch();
            }
            break;
        case IDC_CHECK_REGEX_CLASS:
        case IDC_CHECK_REGEX_PATH:
        case IDC_CHECK_REGEX_TEXT:
            touch();
            break;
        case IDC_COMBO_WINDOWS:
            if (HIWORD(wParam) == CBN_SELCHANGE) {
                fill();
            }
            break;
        }
        break;
    }
    return FALSE;
}

bool RuleEditor::append()
{
    if (dirty()) {
        switch (MessageBox(window, TEXT_UNSAVED, APP_NAME, MB_YESNOCANCEL | MB_ICONQUESTION)) {
        case IDYES:
            save();
            break;
        case IDNO:
            drop();
            break;
        case IDCANCEL: 
            return FALSE;
        }
    }
    sync();
    TCHAR ruleName[MAX_RULE_NAME];
    _sntprintf_s(ruleName, MAX_RULE_NAME, _T("* %s"), TEXT_NEW_RULE);
    auto index = ListBox_AddString(ruleList, ruleName);
    ListBox_SetCurSel(ruleList, index);
    select();
    enable();
    Edit_SetText(nameEdit, TEXT_NEW_RULE);
    return TRUE;
}

bool RuleEditor::remove()
{
    auto index = ListBox_GetCurSel(ruleList);
    if (index < 0) {
        return false;
    }
    if ((size_t)index < context->hidingRules.size()) {
        auto rule = context->hidingRules[index];
        delete[] (BYTE*)rule;
        VECTOR_ERASE(context->hidingRules, index);
    }
    ListBox_DeleteString(ruleList, index);
    ListBox_SetCurSel(ruleList, -1);
    select();
    enable(false);
    clean();
    return saveRules(context);
}

void RuleEditor::enable(bool val)
{
    Edit_Enable(nameEdit, val);
    Edit_Enable(textEdit, val);
    Edit_Enable(classEdit, val);
    Edit_Enable(pathEdit, val);
    Button_SetCheck(textCheckBox, BST_UNCHECKED);
    Button_SetCheck(classCheckBox, BST_UNCHECKED);
    Button_SetCheck(pathCheckBox, BST_UNCHECKED);
    Button_Enable(textCheckBox, val);
    Button_Enable(classCheckBox, val);
    Button_Enable(pathCheckBox, val);
    ComboBox_Enable(windowsCombo, val);
    Edit_SetText(nameEdit, NULL);
    Edit_SetText(textEdit, NULL);
    Edit_SetText(classEdit, NULL);
    Edit_SetText(pathEdit, NULL);
}

void RuleEditor::touch()
{
    if (!isDirty && !isBusy) {
        isDirty = true;
        Button_Enable(saveButton, TRUE);
        Button_Enable(dropButton, TRUE);
        ListBox_Enable(ruleList, FALSE);
    }
}

void RuleEditor::clean()
{
    if (isDirty) {
        isDirty = false;
        Button_Enable(saveButton, FALSE);
        Button_Enable(dropButton, FALSE);
        ListBox_Enable(ruleList, TRUE);
    }
}

void RuleEditor::sync()
{
    ListBox_ResetContent(ruleList);
    for (HIDING_RULE* rule : context->hidingRules) {
        ListBox_AddString(ruleList, rule->ruleData);
    }

    ComboBox_ResetContent(windowsCombo);
    TCHAR windowText[MAX_WINDOW_TEXT]{};
    for (int i = 0; i < context->iconIndex; i++) {
        auto hiddenWindow = context->icons[i].window;
        if (GetWindowText(hiddenWindow, windowText, MAX_WINDOW_TEXT) == 0) {
            continue;
        }
        ComboBox_SetItemData(windowsCombo, ComboBox_AddString(windowsCombo, windowText), i);
    }
}

void RuleEditor::drop()
{
    auto index = ListBox_GetCurSel(ruleList);
    if ((size_t)index >= context->hidingRules.size()) {
        ListBox_DeleteString(ruleList, index);
    }
    ListBox_SetCurSel(ruleList, -1);
    select();
    enable(false);
    clean();
}

void RuleEditor::select()
{
    isBusy = true;
    auto index = ListBox_GetCurSel(ruleList);
    Button_Enable(removeButton, index > -1 ? TRUE : FALSE);
    if (index > -1 && (size_t)index < context->hidingRules.size()) {
        enable();
        auto rule = context->hidingRules[index];
        auto text = rule->ruleData;
        Edit_SetText(nameEdit, text);
        text += _tcsclen(text) + 1;
        Edit_SetText(textEdit, text);
        text += _tcsclen(text) + 1;
        Edit_SetText(classEdit, text);
        text += _tcsclen(text) + 1;
        Edit_SetText(pathEdit, text);
        Button_SetCheck(textCheckBox, rule->isWindowTextRegex ? BST_CHECKED : BST_UNCHECKED);
        Button_SetCheck(classCheckBox, rule->isWindowClassNameRegex ? BST_CHECKED : BST_UNCHECKED);
        Button_SetCheck(pathCheckBox, rule->isExeFileNameRegex ? BST_CHECKED : BST_UNCHECKED);
    }
    isBusy = false;
}

void RuleEditor::fill()
{
    Button_SetCheck(textCheckBox, BST_UNCHECKED);
    Button_SetCheck(classCheckBox, BST_UNCHECKED);
    Button_SetCheck(pathCheckBox, BST_UNCHECKED);
    
    auto index = ComboBox_GetItemData(windowsCombo, ComboBox_GetCurSel(windowsCombo));
    if (index >= context->iconIndex) {
        return;
    }

    auto hwnd = context->icons[index].window;
    TCHAR windowText[MAX_WINDOW_TEXT]{};
    TCHAR className[MAX_CLASS_NAME]{};
    TCHAR exeFileName[MAX_PATH]{};
    if (GetWindowText(hwnd, windowText, MAX_WINDOW_TEXT)) {
        Edit_SetText(textEdit, windowText);
    }
    if (GetClassName(hwnd, className, MAX_CLASS_NAME)) {
        Edit_SetText(classEdit, className);
    }
    if (GetWindowExeFileName(hwnd, exeFileName, MAX_PATH)) {
        Edit_SetText(pathEdit, exeFileName);
    }
}

bool RuleEditor::dirty()
{
    return isDirty;
}

bool RuleEditor::save()
{
    auto index = ListBox_GetCurSel(ruleList);
    HIDING_RULE* rule = newRule();
    if (NULL == rule) {
        return false;
    }
    if ((size_t)index >= context->hidingRules.size()) {
        context->hidingRules.push_back(rule);
    }
    else {
        auto originalRule = context->hidingRules[index];
        delete[] (BYTE*)originalRule;
        context->hidingRules[index] = rule;
    }
    ListBox_DeleteString(ruleList, index);
    ListBox_InsertString(ruleList, index, rule->ruleData);
    select();
    clean();
    return saveRules(context);
}
