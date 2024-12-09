#pragma once

#define TEXT_NEW_RULE _T("新规则")
#define TEXT_UNSAVED _T("当前编辑的规则未保存。是否要保存？")
#define TEXT_RULE_INFO_REQUIRED _T("必须输入完整的规则信息")
#define TEXT_INVALID_REGEX _T("\"%s\" 不是正经的正则表达式")
#define ENTER_EDIT(_WND_)
#define LEAVE_EDIT(_WND_)

#ifdef UNICODE
#define TREGEX std::wregex
#else
#define TREGEX std::regex
#endif

bool loadRules(TRCONTEXT* context);
bool applyRules(TRCONTEXT* context);
bool saveRules(TRCONTEXT* context);
bool clearRules(TRCONTEXT* context);
bool matchRule(TRCONTEXT* context, HWND hwnd);
void showRulesDlg(HWND parent, TRCONTEXT* context);

class RuleEditor 
{
private:
    TRCONTEXT* context;
    HWND window = NULL;
    HWND ruleList = NULL;
    HWND nameEdit = NULL;
    HWND textEdit = NULL;
    HWND classEdit = NULL;
    HWND pathEdit = NULL;
    HWND textCheckBox = NULL;
    HWND classCheckBox = NULL;
    HWND pathCheckBox = NULL;
    HWND saveButton = NULL;
    HWND removeButton = NULL;
    HWND dropButton = NULL;
    HWND windowsCombo = NULL;
    int ruleId = -1;
    bool isDirty = false;
    bool isBusy = false;
private:
    HIDING_RULE* newRule();
public:
    RuleEditor(TRCONTEXT* context);
    void initialize(HWND hwnd);
    void enable(bool val = true);
    void touch();
    void clean();
    void sync();
    void drop();
    void select();
    void fill();
    bool dirty();
    bool save();
    bool append();
    bool remove();
    bool dispatchMessage(UINT message, WPARAM wParam, LPARAM lParam);
    TRCONTEXT* getContext();
};
