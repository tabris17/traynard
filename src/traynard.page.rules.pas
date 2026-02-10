unit Traynard.Page.Rules;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, PairSplitter, StdCtrls, ActnList,
  Traynard.Page, Traynard.Types;

type

  { TPageRules }

  TPageRules = class(TFramePage)
    ActionOpen: TAction;
    ActionClose: TAction;
    ActionDelete: TAction;
    ActionSave: TAction;
    ActionNew: TAction;
    ActionList: TActionList;
    ButtonHotkeyBind: TButton;
    ButtonHotkeyClear: TButton;
    ButtonClose: TButton;
    ButtonNew: TButton;
    ButtonSave: TButton;
    ButtonDelete: TButton;
    ButtonPanel: TPanel;
    CheckGroupTriggerOn: TCheckGroup;
    ComboBoxAppPath: TComboBox;
    ComboBoxWindowClass: TComboBox;
    ComboBoxWindowTitle: TComboBox;
    EditAppPath: TEdit;
    EditRuleName: TEdit;
    EditWindowClass: TEdit;
    EditWindowTitle: TEdit;
    GroupBoxHotkey: TGroupBox;
    LabelHotkey: TLabel;
    LabelAppPath: TLabel;
    LabelRuleName: TLabel;
    LabelWindowClass: TLabel;
    LabelWindowTitle: TLabel;
    ListBoxRules: TListBox;
    PairSplitter: TPairSplitter;
    PairSplitterSideLeft: TPairSplitterSide;
    PairSplitterSideRight: TPairSplitterSide;
    PanelLeft: TPanel;
    PanelWelcome: TPanel;
    RadioGroupMinimizeTo: TRadioGroup;
    RadioGroupNotification: TRadioGroup;
    ScrollBoxEditor: TScrollBox;
    procedure ActionCloseExecute(Sender: TObject);
    procedure ActionDeleteExecute(Sender: TObject);
    procedure ActionNewExecute(Sender: TObject);
    procedure ActionOpenExecute(Sender: TObject);
    procedure ActionSaveExecute(Sender: TObject);
    procedure ButtonHotkeyBindClick(Sender: TObject);
    procedure ButtonHotkeyClearClick(Sender: TObject);
    procedure CheckItemClick(Sender: TObject; Index: integer);
    procedure ComboBoxAppPathChange(Sender: TObject);
    procedure ComboBoxWindowClassChange(Sender: TObject);
    procedure ComboBoxWindowTitleChange(Sender: TObject);
    procedure ListBoxRulesSelectionChange(Sender: TObject; User: boolean);
    procedure RuleChange(Sender: TObject);
    procedure PairSplitterResize(Sender: TObject);
    procedure PageCloseQuery(Sender: TObject; var CanClose: boolean);
  private
    FEditState: TEditState;
    FUnsaved: boolean;
    FOriginalEditWindowTitleMaxWidth: TConstraintSize;
    FOriginalEditWindowClassMaxWidth: TConstraintSize;
    FOriginalEditAppPathMaxWidth: TConstraintSize;
    FComboBoxWindowTitleItemIndex: longint;
    FComboBoxWindowClassItemIndex: longint;
    FComboBoxAppPathItemIndex: longint;
    FHotkey: THotkey;
    procedure SetEditState(AValue: TEditState);
    procedure SetUnsaved(AValue: boolean);
    procedure ToggleEditor(AEnabled: boolean);
    procedure ToggleList(const AEnabled, Unselected: boolean);
    procedure LanguageChanged(Sender: TObject);
    procedure RestoreLabelHotkey;
    procedure TestHotkey(constref Hotkey: THotkey);
  public
    property Unsaved: boolean read FUnsaved write SetUnsaved;
    property EditState: TEditState read FEditState write SetEditState;
    procedure Initialize; override;
    procedure Finalize; override;
    procedure NewRule(Sender: TObject;
      RuleName: string = ''; WindowTitle: string = ''; WindowClass: string = ''; AppPath: string = '');
    procedure ClearEditor;
  end;

implementation

uses
  Windows, Traynard.Strings, Traynard.Rule, Traynard.Form.Main, Traynard.Helpers, Traynard.Settings, Traynard.Form.Hotkey;

{$R *.lfm}

{ TPageRules }

procedure TPageRules.PairSplitterResize(Sender: TObject);
var
  LeftMaxWidth: Integer;
begin
  LeftMaxWidth := PairSplitter.Width - PairSplitterSideRight.Constraints.MinWidth;
  if PairSplitterSideLeft.Width > LeftMaxWidth then
    PairSplitterSideLeft.Width := LeftMaxWidth;
end;

procedure TPageRules.PageCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  ActionCloseExecute(Sender);
  CanClose := not Unsaved;
  if not CanClose then FormMain.Navigate(piRules);
end;

procedure TPageRules.SetUnsaved(AValue: boolean);
begin
  if FUnsaved = AValue then Exit;
  FUnsaved := AValue;
  ActionSave.Enabled := AValue;
  ToggleList(not AValue, False);
end;

procedure TPageRules.ToggleEditor(AEnabled: boolean);
begin
  EditRuleName.Enabled := AEnabled;
  ComboBoxWindowTitle.Enabled := AEnabled;
  EditWindowTitle.Enabled := AEnabled;
  ComboBoxWindowClass.Enabled := AEnabled;
  EditWindowClass.Enabled := AEnabled;
  ComboBoxAppPath.Enabled := AEnabled;
  EditAppPath.Enabled := AEnabled;
  RadioGroupNotification.Enabled := AEnabled;
  RadioGroupMinimizeTo.Enabled := AEnabled;
  CheckGroupTriggerOn.Enabled := AEnabled;
end;

procedure TPageRules.ToggleList(const AEnabled, Unselected: boolean);
begin
  ListBoxRules.Enabled := AEnabled;
  if AEnabled then
    ListBoxRules.Color := clDefault
  else
    ListBoxRules.Color := clForm;

  if Unselected then
    ListBoxRules.ItemIndex := -1;
end;

procedure TPageRules.LanguageChanged(Sender: TObject);
var
  i, ComboBoxMaxWidth, ComboBoxItemWidth: integer;
  ComboBox: TComboBox;
  ComboBoxItem: string;
begin
  for i := 0 to RadioGroupNotification.Items.Count - 1 do
    RadioGroupNotification.Items[i] := RULE_NOTIFICATIONS[TRuleNotification(i)];
  for i := 0 to CheckGroupTriggerOn.Items.Count - 1 do
    CheckGroupTriggerOn.Items[i] := RULE_TRIGGER_ON[TWindowAction(i)];
  for i := 0 to RadioGroupMinimizeTo.Items.Count - 1 do
    RadioGroupMinimizeTo.Items[i] := RULE_MINIMIZE_POSITIONS[TTrayPosition(i)];

  for ComboBox in [ComboBoxWindowTitle, ComboBoxWindowClass, ComboBoxAppPath] do
  begin
    ComboBoxMaxWidth := 0;
    for i := 0 to ComboBox.Items.Count - 1 do
    begin
      ComboBoxItem := RULE_COMPARISONS[i];
      ComboBox.Items[i] := ComboBoxItem;
      ComboBoxItemWidth := ComboBox.Canvas.TextWidth(ComboBoxItem);
      if ComboBoxItemWidth > ComboBoxMaxWidth then ComboBoxMaxWidth := ComboBoxItemWidth;
    end;
    ComboBox.Width := ComboBoxMaxWidth + VScrollWidth;
  end;
  EditWindowTitle.Constraints.MaxWidth := FOriginalEditWindowTitleMaxWidth - ComboBoxWindowTitle.Width;
  EditWindowClass.Constraints.MaxWidth := FOriginalEditWindowClassMaxWidth - ComboBoxWindowClass.Width;
  EditAppPath.Constraints.MaxWidth := FOriginalEditAppPathMaxWidth - ComboBoxAppPath.Width;
  ComboBoxWindowTitle.ItemIndex := FComboBoxWindowTitleItemIndex;
  ComboBoxWindowClass.ItemIndex := FComboBoxWindowClassItemIndex;
  ComboBoxAppPath.ItemIndex := FComboBoxAppPathItemIndex;
end;

procedure TPageRules.RestoreLabelHotkey;
begin
  with LabelHotkey do
  begin
    Caption := TEXT_BRACKETED_NO_SET;
    Font.Color := clDefault;
    ShowHint := False;
  end;
end;

procedure TPageRules.TestHotkey(constref Hotkey: THotkey);
var
  RuleList: TRules.THotkeyRules;
begin
  LabelHotkey.Caption := WinHotkeyToText(Hotkey.Modifiers, Hotkey.Key);
  if not Rules.Hotkeys.TryGetValue(Hotkey, RuleList) or (RuleList.HotkeyID = HOTKEY_NONE) then
  begin
    LabelHotkey.Font.Color := clRed;
    LabelHotkey.ShowHint := True;
  end;
end;

procedure TPageRules.SetEditState(AValue: TEditState);
begin
  if FEditState = AValue then Exit;
  FEditState := AValue;

  case AValue of
    esNone:
    begin
      ClearEditor;
      ListBoxRules.ItemIndex := -1;
      PanelWelcome.Visible := True;
      ScrollBoxEditor.Visible := False;
      ActionDelete.Enabled := False;
      ActionClose.Enabled := False;
      ToggleList(True, False);
    end;
    esNew:
    begin
      ClearEditor;
      PanelWelcome.Visible := False;
      ScrollBoxEditor.Visible := True;
      ActionDelete.Enabled := False;
      ActionClose.Enabled := True;
      ToggleList(False, True);
    end;
    esOpen:
    begin
      PanelWelcome.Visible := False;
      ScrollBoxEditor.Visible := True;
      ActionDelete.Enabled := True;
      ActionClose.Enabled := True;
    end;
  end;
end;

procedure TPageRules.Initialize;
var
  ComboBox: TComboBox;
  ComboBoxItem, RuleNotification, RuleWindowAction, RuleMinimizePosition: string;
  ComboBoxItemWidth, ComboBoxMaxWidth: integer;
  Rule: TRule;
begin
  inherited;

  FUnsaved := False;
  FEditState := esNone;
  FOriginalEditWindowTitleMaxWidth := EditWindowTitle.Constraints.MaxWidth;
  FOriginalEditWindowClassMaxWidth := EditWindowClass.Constraints.MaxWidth;
  FOriginalEditAppPathMaxWidth := EditAppPath.Constraints.MaxWidth;
  OnCloseQuery := @PageCloseQuery;

  for RuleNotification in RULE_NOTIFICATIONS do
    RadioGroupNotification.Items.Add(RuleNotification);
  for RuleWindowAction in RULE_TRIGGER_ON do
    CheckGroupTriggerOn.Items.Add(RuleWindowAction);
  for RuleMinimizePosition in RULE_MINIMIZE_POSITIONS do
    RadioGroupMinimizeTo.Items.Add(RuleMinimizePosition);

  for ComboBox in [ComboBoxWindowTitle, ComboBoxWindowClass, ComboBoxAppPath] do
  begin
    ComboBoxMaxWidth := 0;
    for ComboBoxItem in RULE_COMPARISONS do
    begin
      ComboBox.Items.Add(ComboBoxItem);
      ComboBoxItemWidth := ComboBox.Canvas.TextWidth(ComboBoxItem);
      if ComboBoxItemWidth > ComboBoxMaxWidth then ComboBoxMaxWidth := ComboBoxItemWidth;
    end;
    ComboBox.Width := ComboBoxMaxWidth + VScrollWidth;
    ComboBox.ItemIndex := 0;
  end;
  EditWindowTitle.Constraints.MaxWidth := FOriginalEditWindowTitleMaxWidth - ComboBoxWindowTitle.Width;
  EditWindowClass.Constraints.MaxWidth := FOriginalEditWindowClassMaxWidth - ComboBoxWindowClass.Width;
  EditAppPath.Constraints.MaxWidth := FOriginalEditAppPathMaxWidth - ComboBoxAppPath.Width;

  for Rule in Rules do
  begin
    ListBoxRules.Items.Add(Rule.Name);
  end;

  Settings.AddListener(siLanguage, @LanguageChanged);
end;

procedure TPageRules.Finalize;
begin
  Settings.RemoveListeners(Self);
  inherited Finalize;
end;

procedure TPageRules.NewRule(Sender: TObject; RuleName: string; WindowTitle: string; WindowClass: string; AppPath: string);
begin
  ActionNewExecute(Sender);
  if Unsaved then Exit;
  if (RuleName <> '') or (WindowTitle <> '') or (WindowClass <> '') or (AppPath <> '') then Unsaved := True;
  EditRuleName.Text := RuleName;
  EditWindowTitle.Text := WindowTitle;
  EditWindowClass.Text := WindowClass;
  EditAppPath.Text := AppPath;
end;

procedure TPageRules.ClearEditor;
begin
  ToggleEditor(False);

  EditRuleName.Clear;
  EditRuleName.Color := clDefault;
  ComboBoxWindowTitle.ItemIndex := 0;
  FComboBoxWindowTitleItemIndex := 0;
  EditWindowTitle.Clear;
  EditWindowTitle.Color := clDefault;
  ComboBoxWindowClass.ItemIndex := 0;
  FComboBoxWindowClassItemIndex := 0;
  EditWindowClass.Clear;
  EditWindowClass.Color := clDefault;
  ComboBoxAppPath.ItemIndex := 0;
  FComboBoxAppPathItemIndex := 0;
  EditAppPath.Clear;
  EditAppPath.Color := clDefault;
  RadioGroupNotification.ItemIndex := 0;
  RadioGroupMinimizeTo.ItemIndex := 0;
  CheckGroupTriggerOn.UncheckAll;
  GroupBoxHotkey.Enabled := False;
  RestoreLabelHotkey;
  FHotkey.Value := 0;
  ToggleEditor(True);
end;

procedure TPageRules.ActionNewExecute(Sender: TObject);
begin
  if Unsaved then
  begin
    ActionCloseExecute(Sender);
    if Unsaved then Exit;
  end;

  ClearEditor;
  EditState := esNew;
end;

procedure TPageRules.ActionOpenExecute(Sender: TObject);
var
  RuleIndex: integer;
  Rule: TRule;
  WindowAction: TWindowAction;
begin
  RuleIndex := ListBoxRules.ItemIndex;
  if RuleIndex = -1 then
  begin
    ClearEditor;
    Exit;
  end;
  Rule := Rules[RuleIndex];

  ToggleEditor(False);
  EditRuleName.Text := Rule.Name;
  FComboBoxWindowTitleItemIndex := Ord(Rule.WindowTitle.Comparison);
  ComboBoxWindowTitle.ItemIndex := FComboBoxWindowTitleItemIndex;
  EditWindowTitle.Text := Rule.WindowTitle.Text;
  FComboBoxWindowClassItemIndex := Ord(Rule.WindowClass.Comparison);
  ComboBoxWindowClass.ItemIndex := FComboBoxWindowClassItemIndex;
  EditWindowClass.Text := Rule.WindowClass.Text;
  FComboBoxAppPathItemIndex := Ord(Rule.AppPath.Comparison);
  ComboBoxAppPath.ItemIndex := FComboBoxAppPathItemIndex;
  EditAppPath.Text := Rule.AppPath.Text;
  RadioGroupNotification.ItemIndex := Ord(Rule.Notification);
  RadioGroupMinimizeTo.ItemIndex := Ord(Rule.Position);
  for WindowAction := RULE_TRIGGER_ON_BEGIN to RULE_TRIGGER_ON_END do
    CheckGroupTriggerOn.Checked[Ord(WindowAction)] := WindowAction in Rule.TriggerOn;
  GroupBoxHotkey.Enabled := waHotkey in Rule.TriggerOn;
  FHotkey := Rule.Hotkey;
  if Rule.Hotkey.Value = 0 then
  begin
    ButtonHotkeyClear.Enabled := False;
    LabelHotkey.Caption := TEXT_BRACKETED_NO_SET;
  end
  else
  begin
    ButtonHotkeyClear.Enabled := True;
    TestHotkey(Rule.Hotkey);
  end;
  ToggleEditor(True);

  EditWindowTitle.Enabled := Rule.WindowTitle.Comparison <> rtcAny;
  EditWindowClass.Enabled := Rule.WindowClass.Comparison <> rtcAny;
  EditAppPath.Enabled := Rule.AppPath.Comparison <> rtcAny;

  if EditState = esNone then
  begin
    PanelWelcome.Visible := False;
    ScrollBoxEditor.Visible := True;
  end;
  EditState := esOpen;
end;

procedure TPageRules.ActionDeleteExecute(Sender: TObject);
var
  RuleIndex: Integer;
begin
  RuleIndex := ListBoxRules.ItemIndex;
  if (RuleIndex >= 0) and
     (MessageBoxW(Handle,
                  PWideChar(UnicodeString(Format(MSG_QUESTION_DELETE_RULE, [ListBoxRules.GetSelectedText]))),
                  PWideChar(UnicodeString(Application.Title)),
                  MB_YESNO or MB_ICONQUESTION or MB_DEFBUTTON2) = IDYES) then
  begin
    if RuleIndex < Rules.Count then
      Rules.RemoveRule(RuleIndex);
    ListBoxRules.Items.Delete(RuleIndex);
    ListBoxRules.ItemIndex := - 1;
    Unsaved := False;
    EditState := esNone;
  end;
end;

procedure TPageRules.ActionCloseExecute(Sender: TObject);
begin
  if Unsaved then
  begin
    case MessageBoxW(Handle,
                     PWideChar(UnicodeString(Format(MSG_QUESTION_SAVE_RULE, [EditRuleName.Text]))),
                     PWideChar(UnicodeString(Application.Title)),
                     MB_YESNOCANCEL or MB_ICONQUESTION or MB_DEFBUTTON3) of
      IDYES: ActionSaveExecute(Sender);
      IDNO: Unsaved := False;
      else Exit;
    end;
  end;

  EditState := esNone;
end;

procedure TPageRules.ActionSaveExecute(Sender: TObject);
var
  Rule: TRule;
  RuleIndex: SizeInt;
  WindowAction: TWindowAction;
begin
  try
    if EditRuleName.Text = '' then
      raise EFieldInvalid.Create(LabelRuleName.Caption, MSG_RULE_NAME_REQUIRED, EditRuleName);
    if Rules.HasRule(EditRuleName.Text, RuleIndex) and (RuleIndex <> ListBoxRules.ItemIndex) then
      raise EFieldInvalid.Create(LabelRuleName.Caption, MSG_RULE_NAME_DUPLICATE, EditRuleName);
    if (EditWindowTitle.Text = '') and (ComboBoxWindowTitle.ItemIndex <> Ord(rtcAny)) then
      raise EFieldInvalid.Create(LabelWindowTitle.Caption, MSG_RULE_WINDOW_TITLE_REQUIRED, EditWindowTitle);
    if (EditWindowClass.Text = '') and (ComboBoxWindowClass.ItemIndex <> Ord(rtcAny)) then
      raise EFieldInvalid.Create(LabelWindowClass.Caption, MSG_RULE_WINDOW_CLASS_REQUIRED, EditWindowClass);
    if (EditAppPath.Text = '') and (ComboBoxAppPath.ItemIndex <> Ord(rtcAny)) then
      raise EFieldInvalid.Create(LabelAppPath.Caption, MSG_RULE_APP_PATH_REQUIRED, EditAppPath);
    if CheckGroupTriggerOn.IsAllUnchecked then
      raise EFieldInvalid.Create(CheckGroupTriggerOn.Caption, MSG_RULE_TRIGGER_ON_REQUIRED, CheckGroupTriggerOn);
    if RadioGroupNotification.ItemIndex = -1 then
      raise EFieldInvalid.Create(RadioGroupNotification.Caption, MSG_RULE_NOTIFICATION_REQUIRED, RadioGroupNotification);
  except
    on Exc: EFieldInvalid do
    begin
      (Owner as TFormMain).Popup(Exc.Title, Exc.Message);
      if Assigned(Exc.Control) then Exc.Control.Color := clInfoBk;
      Exit;
    end;
  end;

  Rule.Name := EditRuleName.Text;
  Rule.WindowTitle.Text := EditWindowTitle.Text;
  Rule.WindowTitle.Comparison := TRuleTextComparison(ComboBoxWindowTitle.ItemIndex);
  Rule.WindowClass.Text := EditWindowClass.Text;
  Rule.WindowClass.Comparison := TRuleTextComparison(ComboBoxWindowClass.ItemIndex);
  Rule.AppPath.Text := EditAppPath.Text;
  Rule.AppPath.Comparison := TRuleTextComparison(ComboBoxAppPath.ItemIndex);
  Rule.Notification := TRuleNotification(RadioGroupNotification.ItemIndex);
  Rule.TriggerOn := [];
  for WindowAction := RULE_TRIGGER_ON_BEGIN to RULE_TRIGGER_ON_END do
  begin
    if CheckGroupTriggerOn.Checked[Ord(WindowAction)] then
      Include(Rule.TriggerOn, WindowAction);
  end;
  Rule.Position := TTrayPosition(RadioGroupMinimizeTo.ItemIndex);
  Rule.Hotkey := FHotkey;

  case EditState of
    esNew:
    begin
      RuleIndex := Rules.AddRule(Rule);
      EditState := esOpen;
    end;
    esOpen:
    begin
      RuleIndex := ListBoxRules.ItemIndex;
      Rules.UpdateRule(RuleIndex, Rule);
    end;
  else
    Exit;
  end;

  ListBoxRules.Items[RuleIndex] := Rule.Name;
  ListBoxRules.ItemIndex := RuleIndex;
  TestHotkey(Rule.Hotkey);
  Unsaved := False;
end;

procedure TPageRules.ButtonHotkeyBindClick(Sender: TObject);
var
  FormHotkey: TFormHotkey;
begin
  FormHotkey := TFormHotkey.Create(Self.Owner);
  try
    if (FormHotkey.ShowModal = mrOK) and (FormHotkey.ShortCut <> 0) then
    begin
      RestoreLabelHotkey;
      FHotkey := FormHotkey.Hotkey;
      LabelHotkey.Caption := WinHotkeyToText(FHotkey.Modifiers, FHotkey.Key);
      ButtonHotkeyClear.Enabled := True;
      RuleChange(Sender);
    end;
  finally
    FormHotkey.Free;
  end;
end;

procedure TPageRules.ButtonHotkeyClearClick(Sender: TObject);
begin
  if FHotkey.Value = 0 then Exit;
  RestoreLabelHotkey;
  FHotkey.Value := 0;
  RuleChange(Sender);
  ButtonHotkeyClear.Enabled := False;
end;

procedure TPageRules.CheckItemClick(Sender: TObject; Index: integer);
begin
  if (Sender = CheckGroupTriggerOn) and (Index = Ord(waHotkey)) then
    GroupBoxHotkey.Enabled := CheckGroupTriggerOn.Checked[Ord(waHotkey)];
  RuleChange(Sender);
end;

procedure TPageRules.ComboBoxAppPathChange(Sender: TObject);
begin
  FComboBoxAppPathItemIndex := (Sender as TComboBox).ItemIndex;
  EditAppPath.Enabled := FComboBoxAppPathItemIndex <> Ord(rtcAny);
  if not EditAppPath.Enabled then EditAppPath.Clear;
  RuleChange(Sender);
end;

procedure TPageRules.ComboBoxWindowClassChange(Sender: TObject);
begin
  FComboBoxWindowClassItemIndex := (Sender as TComboBox).ItemIndex;
  EditWindowClass.Enabled := FComboBoxWindowClassItemIndex <> Ord(rtcAny);
  if not EditWindowClass.Enabled then EditWindowClass.Clear;
  RuleChange(Sender);
end;

procedure TPageRules.ComboBoxWindowTitleChange(Sender: TObject);
begin
  FComboBoxWindowTitleItemIndex := (Sender as TComboBox).ItemIndex;
  EditWindowTitle.Enabled := FComboBoxWindowTitleItemIndex <> Ord(rtcAny);
  if not EditWindowTitle.Enabled then EditWindowTitle.Clear;
  RuleChange(Sender);
end;

procedure TPageRules.ListBoxRulesSelectionChange(Sender: TObject; User: boolean);
begin
  if (not User) or (EditState = esNew) or (EditState = esOpen) and Unsaved then Exit;
  ActionOpenExecute(Sender);
end;

procedure TPageRules.RuleChange(Sender: TObject);
begin
  with Sender as TControl do
  begin
    if not Enabled then Exit;
    Color := clDefault;
  end;
  Unsaved := True;
end;

end.

