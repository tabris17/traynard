unit Traynard.Page.Rules;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, PairSplitter, StdCtrls, ActnList,
  Traynard.Page, Traynard.Rule;

type

  { TPageRules }

  TPageRules = class(TFramePage)
    ActionOpen: TAction;
    ActionClose: TAction;
    ActionDelete: TAction;
    ActionSave: TAction;
    ActionNew: TAction;
    ActionList: TActionList;
    ButtonClose: TButton;
    ButtonNew: TButton;
    ButtonSave: TButton;
    ButtonDelete: TButton;
    ButtonPanel: TPanel;
    CheckGroupTriggerOn: TCheckGroup;
    ComboBoxAppPath: TComboBox;
    ComboBoxMinimizeTo: TComboBox;
    ComboBoxWindowClass: TComboBox;
    ComboBoxWindowTitle: TComboBox;
    EditAppPath: TEdit;
    EditRuleName: TEdit;
    EditWindowClass: TEdit;
    EditWindowTitle: TEdit;
    LabelAppPath: TLabel;
    LabelMinimizeTo: TLabel;
    LabelRuleName: TLabel;
    LabelWindowClass: TLabel;
    LabelWindowTitle: TLabel;
    ListBoxRules: TListBox;
    PairSplitter: TPairSplitter;
    PairSplitterSideLeft: TPairSplitterSide;
    PairSplitterSideRight: TPairSplitterSide;
    PanelLeft: TPanel;
    PanelWelcome: TPanel;
    RadioGroupNotification: TRadioGroup;
    ScrollBoxEditor: TScrollBox;
    procedure ActionCloseExecute(Sender: TObject);
    procedure ActionDeleteExecute(Sender: TObject);
    procedure ActionNewExecute(Sender: TObject);
    procedure ActionOpenExecute(Sender: TObject);
    procedure ActionSaveExecute(Sender: TObject);
    procedure CheckItemClick(Sender: TObject; Index: integer);
    procedure ComboBoxAppPathChange(Sender: TObject);
    procedure ComboBoxWindowClassChange(Sender: TObject);
    procedure ComboBoxWindowTitleChange(Sender: TObject);
    procedure ListBoxRulesSelectionChange(Sender: TObject; User: boolean);
    procedure RuleChange(Sender: TObject);
    procedure PairSplitterResize(Sender: TObject);
    procedure PageCloseQuery(Sender: TObject; var CanClose: boolean);
  type
    TEditState = (esNone, esOpen, esNew);

    { EFieldInvalid }

    EFieldInvalid = class(Exception)
    private
      FTitle: string;
      FControl: TControl;
    public
      constructor Create(const ATitle, AMessage: string; AControl: TControl);
      property Title: string read FTitle;
      property Control: TControl read FControl;
    end;
  private
    FEditState: TEditState;
    FUnsaved: boolean;
    procedure SetEditState(AValue: TEditState);
    procedure SetUnsaved(AValue: boolean);
    procedure ToggleEditor(AEnabled: boolean);
    procedure ToggleList(AEnabled: boolean);
    procedure LanguageChanged(Sender: TObject);
  public
    property Unsaved: boolean read FUnsaved write SetUnsaved;
    property EditState: TEditState read FEditState write SetEditState;
    procedure Initialize; override;
    procedure Finalize; override;
    procedure NewRule(Sender: TObject;
      RuleName: string = ''; WindowTitle: string = ''; WindowClass: string = ''; AppPath: string = '');
    procedure ClearEditor;
  end;

var
  VScrollWidth: longint;

implementation

uses
  Windows, Traynard.Strings, Traynard.Form.Main, Traynard.Types, Traynard.Helpers, Traynard.Settings;

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
  ComboBoxMinimizeTo.Enabled := AEnabled;
  CheckGroupTriggerOn.Enabled := AEnabled;
end;

procedure TPageRules.ToggleList(AEnabled: boolean);
begin
  ListBoxRules.Enabled := AEnabled;
  if AEnabled then
    ListBoxRules.Color := clDefault
  else
  begin
    ListBoxRules.Color := clForm;
    ListBoxRules.ItemIndex := -1;
  end;
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

  ComboBoxMaxWidth := 0;
  for i := 0 to ComboBoxMinimizeTo.Items.Count - 1 do
  begin
    ComboBoxItem := RULE_POSITIONS[i];
    ComboBoxMinimizeTo.Items[i] := ComboBoxItem;
    ComboBoxItemWidth := ComboBoxMinimizeTo.Canvas.TextWidth(ComboBoxItem);
    if ComboBoxItemWidth > ComboBoxMaxWidth then ComboBoxMaxWidth := ComboBoxItemWidth;
  end;
  ComboBoxMinimizeTo.Width := ComboBoxMaxWidth + VScrollWidth;
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
      ToggleList(True);
    end;
    esNew:
    begin
      ClearEditor;
      PanelWelcome.Visible := False;
      ScrollBoxEditor.Visible := True;
      ActionDelete.Enabled := False;
      ActionClose.Enabled := True;
      ToggleList(False);
    end;
    esOpen:
    begin
      PanelWelcome.Visible := False;
      ScrollBoxEditor.Visible := True;
      ActionDelete.Enabled := True;
      ActionClose.Enabled := True;
      ToggleList(True);
    end;
  end;
end;

procedure TPageRules.Initialize;
var
  ComboBox: TComboBox;
  ComboBoxItem, RuleNotification, RuleWindowAction: string;
  ComboBoxItemWidth, ComboBoxMaxWidth: integer;
  Rule: TRule;
begin
  inherited;

  FUnsaved := False;
  FEditState := esNone;
  OnCloseQuery := @PageCloseQuery;

  for RuleNotification in RULE_NOTIFICATIONS do
    RadioGroupNotification.Items.Add(RuleNotification);
  for RuleWindowAction in RULE_TRIGGER_ON do
    CheckGroupTriggerOn.Items.Add(RuleWindowAction);

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
  EditWindowTitle.Constraints.MaxWidth := EditWindowTitle.Constraints.MaxWidth - ComboBoxWindowTitle.Width;
  EditWindowClass.Constraints.MaxWidth := EditWindowClass.Constraints.MaxWidth - ComboBoxWindowClass.Width;
  EditAppPath.Constraints.MaxWidth := EditAppPath.Constraints.MaxWidth - ComboBoxAppPath.Width;

  ComboBoxMaxWidth := 0;
  for ComboBoxItem in RULE_POSITIONS do
  begin
    ComboBoxMinimizeTo.Items.Add(ComboBoxItem);
    ComboBoxItemWidth := ComboBoxMinimizeTo.Canvas.TextWidth(ComboBoxItem);
    if ComboBoxItemWidth > ComboBoxMaxWidth then ComboBoxMaxWidth := ComboBoxItemWidth;
  end;
  ComboBoxMinimizeTo.Width := ComboBoxMaxWidth + VScrollWidth;
  ComboBoxMinimizeTo.ItemIndex := 0;

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
  EditWindowTitle.Clear;
  EditWindowTitle.Color := clDefault;
  ComboBoxWindowClass.ItemIndex := 0;
  EditWindowClass.Clear;
  EditWindowClass.Color := clDefault;
  ComboBoxAppPath.ItemIndex := 0;
  EditAppPath.Clear;
  EditAppPath.Color := clDefault;
  RadioGroupNotification.ItemIndex := 0;
  ComboBoxMinimizeTo.ItemIndex := 0;
  CheckGroupTriggerOn.UncheckAll;
  ToggleEditor(True);
end;

procedure TPageRules.ActionNewExecute(Sender: TObject);
begin
  if Unsaved then
  begin
    ActionCloseExecute(Sender);
    if Unsaved then Exit;
  end;

  ToggleList(False);
  ClearEditor;
  EditState := esNew;
end;

procedure TPageRules.ActionOpenExecute(Sender: TObject);
var
  RuleIndex: integer;
  Rule: TRule;
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
  ComboBoxWindowTitle.ItemIndex := Ord(Rule.WindowTitle.Comparison);
  EditWindowTitle.Text := Rule.WindowTitle.Text;
  ComboBoxWindowClass.ItemIndex := Ord(Rule.WindowClass.Comparison);
  EditWindowClass.Text := Rule.WindowClass.Text;
  ComboBoxAppPath.ItemIndex := Ord(Rule.AppPath.Comparison);
  EditAppPath.Text := Rule.AppPath.Text;
  RadioGroupNotification.ItemIndex := Ord(Rule.Notification);
  ComboBoxMinimizeTo.ItemIndex := Ord(Rule.Position);
  CheckGroupTriggerOn.Checked[Ord(waCreation)] := waCreation in Rule.TriggerOn;
  CheckGroupTriggerOn.Checked[Ord(waChange)] := waChange in Rule.TriggerOn;
  CheckGroupTriggerOn.Checked[Ord(waMinimizing)] := waMinimizing in Rule.TriggerOn;
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
  RuleIndex: Integer;
  RuleTriggerOn: TRuleTriggerOn;
  RuleName: string;
  WindowAction: TWindowAction;
begin
  try
    if EditRuleName.Text = '' then
      raise EFieldInvalid.Create(LabelRuleName.Caption, MSG_RULE_NAME_REQUIRED, EditRuleName);
    for RuleIndex := 0 to ListBoxRules.Count - 1 do
    begin
      RuleName := ListBoxRules.Items[RuleIndex];
      if (RuleName = EditRuleName.Text) and (ListBoxRules.ItemIndex <> RuleIndex) then
        raise EFieldInvalid.Create(LabelRuleName.Caption, MSG_RULE_NAME_DUPLICATE, EditRuleName);
    end;
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
  for WindowAction := Low(TWindowAction) to waMinimizing do
  begin
    if CheckGroupTriggerOn.Checked[Ord(WindowAction)] then
      Include(Rule.TriggerOn, WindowAction);
  end;
  Rule.Position := TTrayPosition(ComboBoxMinimizeTo.ItemIndex);

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
  Unsaved := False;
end;

procedure TPageRules.CheckItemClick(Sender: TObject; Index: integer);
begin
  RuleChange(Sender);
end;

procedure TPageRules.ComboBoxAppPathChange(Sender: TObject);
begin
  EditAppPath.Enabled := (Sender as TComboBox).ItemIndex <> Ord(rtcAny);
  if not EditAppPath.Enabled then EditAppPath.Clear;
  RuleChange(Sender);
end;

procedure TPageRules.ComboBoxWindowClassChange(Sender: TObject);
begin
  EditWindowClass.Enabled := (Sender as TComboBox).ItemIndex <> Ord(rtcAny);
  if not EditWindowClass.Enabled then EditWindowClass.Clear;
  RuleChange(Sender);
end;

procedure TPageRules.ComboBoxWindowTitleChange(Sender: TObject);
begin
  EditWindowTitle.Enabled := (Sender as TComboBox).ItemIndex <> Ord(rtcAny);
  if not EditWindowTitle.Enabled then EditWindowTitle.Clear;
  RuleChange(Sender);
end;

procedure TPageRules.ListBoxRulesSelectionChange(Sender: TObject; User: boolean);
begin
  if (not User) or (EditState = esNew) then Exit;
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

{ TPageRules.EFieldInvalid }

constructor TPageRules.EFieldInvalid.Create(const ATitle, AMessage: string; AControl: TControl);
begin
  inherited Create(AMessage);
  FTitle := ATitle;
  FControl := AControl;
end;

initialization

VScrollWidth := GetSystemMetrics(SM_CXVSCROLL) + 10;

end.

