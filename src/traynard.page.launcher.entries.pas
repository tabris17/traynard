unit Traynard.Page.Launcher.Entries;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, ComCtrls, ActnList, Menus, PairSplitter, EditBtn,
  Traynard.Page, Traynard.Types;

type

  { TPageLauncherEntries }

  TPageLauncherEntries = class(TFramePage)
    ActionRunning: TAction;
    ActionLaunch: TAction;
    ActionOpen: TAction;
    ActionClose: TAction;
    ActionDelete: TAction;
    ActionSave: TAction;
    ActionNew: TAction;
    ActionList: TActionList;
    ButtonLaunch: TButton;
    ButtonHotkeyBind: TButton;
    ButtonHotkeyClear: TButton;
    ButtonNew: TButton;
    ButtonSave: TButton;
    ButtonPanel: TPanel;
    ButtonDelete: TButton;
    ButtonClose: TButton;
    CheckBoxShowWindow: TCheckBox;
    CheckGroupMethods: TCheckGroup;
    ComboBoxMinimizeTo: TComboBox;
    EditEntryName: TEdit;
    GroupBoxHotkey: TGroupBox;
    LabelHotkey: TLabel;
    LabelEntryName: TLabel;
    LabelMinimizeTo: TLabel;
    MenuItemDelete: TMenuItem;
    MenuItemLaunch: TMenuItem;
    MenuItemClose: TMenuItem;
    MenuItemSave: TMenuItem;
    MenuItemNew: TMenuItem;
    MenuItemLauncher: TMenuItem;
    NavMenu: TPopupMenu;
    RadioGroupNotification: TRadioGroup;
    Separator1: TMenuItem;
    Separator2: TMenuItem;
    Separator3: TMenuItem;
    WorkingDirectoryEdit: TDirectoryEdit;
    ExecutableFileNameEdit: TFileNameEdit;
    LabelWorkingDirectory: TLabel;
    LabelArguments: TLabel;
    LabelExecutablePath: TLabel;
    ListBoxEntries: TListBox;
    MemoArguments: TMemo;
    PairSplitter: TPairSplitter;
    PairSplitterSideLeft: TPairSplitterSide;
    PairSplitterSideRight: TPairSplitterSide;
    PanelWelcome: TPanel;
    PanelLeft: TPanel;
    ScrollBoxEditor: TScrollBox;
    procedure ActionCloseExecute(Sender: TObject);
    procedure ActionDeleteExecute(Sender: TObject);
    procedure ActionRunningExecute(Sender: TObject);
    procedure ActionLaunchExecute(Sender: TObject);
    procedure ActionOpenExecute(Sender: TObject);
    procedure ActionSaveExecute(Sender: TObject);
    procedure ActionNewExecute(Sender: TObject);
    procedure ButtonHotkeyBindClick(Sender: TObject);
    procedure ButtonHotkeyClearClick(Sender: TObject);
    procedure CheckItemClick(Sender: TObject; Index: integer);
    procedure EntryChange(Sender: TObject);
    procedure ListBoxEntriesSelectionChange(Sender: TObject; User: boolean);
    procedure PageCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure PairSplitterResize(Sender: TObject);
  private
    FEditState: TEditState;
    FUnsaved: boolean;
    FHotkey: THotkey;
    FComboBoxMinimizeToItemIndex: longint;
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
    procedure ClearEditor;
    procedure Initialize; override;
    procedure Finalize; override;
  end;

implementation

{$R *.lfm}

uses
  Windows, Traynard.Settings, Traynard.Form.Main, Traynard.Helpers, Traynard.Strings, Traynard.Launcher, Traynard.Form.Hotkey;

{ TPageLauncherEntries }

procedure TPageLauncherEntries.ButtonHotkeyClearClick(Sender: TObject);
begin
  if FHotkey.Value = 0 then Exit;
  RestoreLabelHotkey;
  FHotkey.Value := 0;
  EntryChange(Sender);
  ButtonHotkeyClear.Enabled := False;
end;

procedure TPageLauncherEntries.CheckItemClick(Sender: TObject; Index: integer);
begin
  if (Sender = CheckGroupMethods) and (Index = Ord(lmHotkey)) then
    GroupBoxHotkey.Enabled := CheckGroupMethods.Checked[Ord(lmHotkey)];
  EntryChange(Sender);
end;

procedure TPageLauncherEntries.EntryChange(Sender: TObject);
begin
  with Sender as TControl do
  begin
    if not Enabled then Exit;
    if Sender is TCustomEditButton then
      (Sender as TCustomEditButton).Color := clDefault
    else
      Color := clDefault;
  end;
  Unsaved := True;
end;

procedure TPageLauncherEntries.ListBoxEntriesSelectionChange(Sender: TObject; User: boolean);
begin
  if (not User) or (EditState = esNew) or (EditState = esOpen) and Unsaved then Exit;
  ActionOpenExecute(Sender);
end;

procedure TPageLauncherEntries.PageCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  ActionCloseExecute(Sender);
  CanClose := not Unsaved;
  if not CanClose then FormMain.Navigate(piLaunchEntries);
end;

procedure TPageLauncherEntries.PairSplitterResize(Sender: TObject);
var
  LeftMaxWidth: Integer;
begin
  LeftMaxWidth := PairSplitter.Width - PairSplitterSideRight.Constraints.MinWidth;
  if PairSplitterSideLeft.Width > LeftMaxWidth then
    PairSplitterSideLeft.Width := LeftMaxWidth;
end;

procedure TPageLauncherEntries.SetUnsaved(AValue: boolean);
begin
  if FUnsaved = AValue then Exit;
  FUnsaved := AValue;
  ActionSave.Enabled := AValue;
  ActionLaunch.Enabled := not AValue;
  ToggleList(not AValue, False);
end;

procedure TPageLauncherEntries.ToggleEditor(AEnabled: boolean);
begin
  EditEntryName.Enabled := AEnabled;
  ExecutableFileNameEdit.Enabled := AEnabled;
  MemoArguments.Enabled := AEnabled;
  WorkingDirectoryEdit.Enabled := AEnabled;
  RadioGroupNotification.Enabled := AEnabled;
  CheckGroupMethods.Enabled := AEnabled;
  CheckBoxShowWindow.Enabled := AEnabled;
  ComboBoxMinimizeTo.Enabled := AEnabled;
end;

procedure TPageLauncherEntries.ToggleList(const AEnabled, Unselected: boolean);
begin
  ListBoxEntries.Enabled := AEnabled;
  if AEnabled then
    ListBoxEntries.Color := clDefault
  else
    ListBoxEntries.Color := clForm;

  if Unselected then
    ListBoxEntries.ItemIndex := -1;
end;

procedure TPageLauncherEntries.LanguageChanged(Sender: TObject);
var
  i, ComboBoxMaxWidth, ComboBoxItemWidth: integer;
  ComboBoxItem: string;
begin
  for i := 0 to RadioGroupNotification.Items.Count - 1 do
    RadioGroupNotification.Items[i] := RULE_NOTIFICATIONS[TRuleNotification(i)];
  for i := 0 to CheckGroupMethods.Items.Count - 1 do
    CheckGroupMethods.Items[i] := LAUNCH_METHODS[TLaunchMethod(i)];

  ComboBoxMinimizeTo.ItemIndex := FComboBoxMinimizeToItemIndex;

  ComboBoxMaxWidth := 0;
  for i := 0 to ComboBoxMinimizeTo.Items.Count - 1 do
  begin
    ComboBoxItem := RULE_POSITIONS[i];
    ComboBoxMinimizeTo.Items[i] := ComboBoxItem;
    ComboBoxItemWidth := ComboBoxMinimizeTo.Canvas.TextWidth(ComboBoxItem);
    if ComboBoxItemWidth > ComboBoxMaxWidth then ComboBoxMaxWidth := ComboBoxItemWidth;
  end;
  ComboBoxMinimizeTo.Width := ComboBoxMaxWidth + VScrollWidth;
  ComboBoxMinimizeTo.ItemIndex := FComboBoxMinimizeToItemIndex;
end;

procedure TPageLauncherEntries.RestoreLabelHotkey;
begin
  with LabelHotkey do
  begin
    Caption := TEXT_BRACKETED_NO_SET;
    Font.Color := clDefault;
    ShowHint := False;
  end;
end;

procedure TPageLauncherEntries.TestHotkey(constref Hotkey: THotkey);
var
  HotkeyEntry: TLauncher.THotkeyEntry;
begin
  LabelHotkey.Caption := WinHotkeyToText(Hotkey.Modifiers, Hotkey.Key);
  if not Launcher.Hotkeys.TryGetValue(Hotkey, HotkeyEntry) or (HotkeyEntry.HotkeyID = HOTKEY_NONE) then
  begin
    LabelHotkey.Font.Color := clRed;
    LabelHotkey.ShowHint := True;
  end;
end;

procedure TPageLauncherEntries.ClearEditor;
begin
  ToggleEditor(False);

  EditEntryName.Clear;
  EditEntryName.Color := clDefault;
  ExecutableFileNameEdit.Clear;
  ExecutableFileNameEdit.Color := clDefault;
  MemoArguments.Clear;
  MemoArguments.Color := clDefault;
  WorkingDirectoryEdit.Clear;
  WorkingDirectoryEdit.Color := clDefault;
  RadioGroupNotification.ItemIndex := 0;
  CheckGroupMethods.UncheckAll;
  GroupBoxHotkey.Enabled := False;
  LabelHotkey.Caption := TEXT_BRACKETED_NO_SET;
  RestoreLabelHotkey;
  FHotkey.Value := 0;
  ComboBoxMinimizeTo.ItemIndex := 0;
  FComboBoxMinimizeToItemIndex := 0;
  ToggleEditor(True);
end;

procedure TPageLauncherEntries.Initialize;
var
  EntryNotification, LaunchMethod, ComboBoxItem: string;
  ComboBoxMaxWidth, ComboBoxItemWidth: Integer;
  Entry: TLaunchEntry;
begin
  inherited Initialize;
  FUnsaved := False;
  FEditState := esNone;
  OnCloseQuery := @PageCloseQuery;

  for EntryNotification in RULE_NOTIFICATIONS do
    RadioGroupNotification.Items.Add(EntryNotification);
  for LaunchMethod in LAUNCH_METHODS do
    CheckGroupMethods.Items.Add(LaunchMethod);

  ComboBoxMaxWidth := 0;
  for ComboBoxItem in RULE_POSITIONS do
  begin
    ComboBoxMinimizeTo.Items.Add(ComboBoxItem);
    ComboBoxItemWidth := ComboBoxMinimizeTo.Canvas.TextWidth(ComboBoxItem);
    if ComboBoxItemWidth > ComboBoxMaxWidth then ComboBoxMaxWidth := ComboBoxItemWidth;
  end;
  ComboBoxMinimizeTo.Width := ComboBoxMaxWidth + VScrollWidth;
  ComboBoxMinimizeTo.ItemIndex := 0;

  for Entry in Launcher do
  begin
    ListBoxEntries.Items.Add(Entry.Name);
  end;

  Settings.AddListener(siLanguage, @LanguageChanged);
end;

procedure TPageLauncherEntries.Finalize;
begin
  Settings.RemoveListeners(Self);
  inherited Finalize;
end;

procedure TPageLauncherEntries.SetEditState(AValue: TEditState);
begin
  if FEditState = AValue then Exit;
  FEditState := AValue;

  case AValue of
    esNone:
    begin
      ClearEditor;
      ListBoxEntries.ItemIndex := -1;
      PanelWelcome.Visible := True;
      ScrollBoxEditor.Visible := False;
      ActionDelete.Enabled := False;
      ActionClose.Enabled := False;
      ActionLaunch.Enabled := False;
      ToggleList(True, False);
    end;
    esNew:
    begin
      ClearEditor;
      PanelWelcome.Visible := False;
      ScrollBoxEditor.Visible := True;
      ActionDelete.Enabled := False;
      ActionClose.Enabled := True;
      ActionLaunch.Enabled := False;
      ToggleList(False, True);
    end;
    esOpen:
    begin
      PanelWelcome.Visible := False;
      ScrollBoxEditor.Visible := True;
      ActionDelete.Enabled := True;
      ActionClose.Enabled := True;
      ActionLaunch.Enabled := True;
    end;
  end;
end;

procedure TPageLauncherEntries.ActionSaveExecute(Sender: TObject);
var
  Entry: TLaunchEntry;
  EntryIndex: SizeInt;
  LaunchMethod: TLaunchMethod;
begin
  try
    if EditEntryName.Text = '' then
      raise EFieldInvalid.Create(LabelEntryName.Caption, MSG_LAUNCHER_ENTRY_NAME_REQUIRED, EditEntryName);
    if Launcher.HasEntry(EditEntryName.Text, EntryIndex) and (EntryIndex <> ListBoxEntries.ItemIndex) then
      raise EFieldInvalid.Create(LabelEntryName.Caption, MSG_LAUNCHER_ENTRY_NAME_DUPLICATE, EditEntryName);
    if (ExecutableFileNameEdit.Text = '') then
      raise EFieldInvalid.Create(LabelExecutablePath.Caption, MSG_LAUNCHER_ENTRY_APPLICATION_REQUIRED, ExecutableFileNameEdit);
    if CheckGroupMethods.IsAllUnchecked then
      raise EFieldInvalid.Create(CheckGroupMethods.Caption, MSG_LAUNCHER_ENTRY_LAUNCH_METHODS_REQUIRED, CheckGroupMethods);
    if RadioGroupNotification.ItemIndex = -1 then
      raise EFieldInvalid.Create(RadioGroupNotification.Caption, MSG_RULE_NOTIFICATION_REQUIRED, RadioGroupNotification);
  except
    on Exc: EFieldInvalid do
    begin
      (Owner as TFormMain).Popup(Exc.Title, Exc.Message);
      if Assigned(Exc.Control) then
      begin
        if Exc.Control is TCustomEditButton then
          (Exc.Control as TCustomEditButton).Color := clInfoBk
        else
          Exc.Control.Color := clInfoBk;
      end;
      Exit;
    end;
  end;

  Entry.Name := EditEntryName.Text;
  Entry.Application := ExecutableFileNameEdit.Text;
  Entry.Arguments := MemoArguments.Text;
  Entry.WorkingDirectory := WorkingDirectoryEdit.Text;
  Entry.Notification := TRuleNotification(RadioGroupNotification.ItemIndex);
  Entry.LaunchMethods := [];
  for LaunchMethod := Low(TLaunchMethod) to High(TLaunchMethod) do
  begin
    if CheckGroupMethods.Checked[Ord(LaunchMethod)] then
      Include(Entry.LaunchMethods, LaunchMethod);
  end;
  Entry.ShowWindow := CheckBoxShowWindow.Checked;
  Entry.Position := TTrayPosition(ComboBoxMinimizeTo.ItemIndex);
  Entry.Hotkey := FHotkey;

  case EditState of
    esNew:
    begin
      EntryIndex := Launcher.AddEntry(Entry);
      EditState := esOpen;
    end;
    esOpen:
    begin
      EntryIndex := ListBoxEntries.ItemIndex;
      Launcher.UpdateEntry(EntryIndex, Entry);
    end;
  else
    Exit;
  end;

  ListBoxEntries.Items[EntryIndex] := Entry.Name;
  ListBoxEntries.ItemIndex := EntryIndex;
  TestHotkey(Entry.Hotkey);
  Unsaved := False;
end;

procedure TPageLauncherEntries.ActionDeleteExecute(Sender: TObject);
var
  EntryIndex: Integer;
begin
  EntryIndex := ListBoxEntries.ItemIndex;
  if (EntryIndex >= 0) and
     (MessageBoxW(Handle,
                  PWideChar(UnicodeString(Format(MSG_QUESTION_DELETE_LAUNCHER_ENTRY, [ListBoxEntries.GetSelectedText]))),
                  PWideChar(UnicodeString(Application.Title)),
                  MB_YESNO or MB_ICONQUESTION or MB_DEFBUTTON2) = IDYES) then
  begin
    if EntryIndex < Launcher.Count then
      Launcher.RemoveEntry(EntryIndex);
    ListBoxEntries.Items.Delete(EntryIndex);
    ListBoxEntries.ItemIndex := - 1;
    Unsaved := False;
    EditState := esNone;
  end;
end;

procedure TPageLauncherEntries.ActionRunningExecute(Sender: TObject);
begin
  FormMain.Navigate(piLauncher);
end;

procedure TPageLauncherEntries.ActionLaunchExecute(Sender: TObject);
var
  EntryIndex: Integer;
begin
  EntryIndex := ListBoxEntries.ItemIndex;
  if (EntryIndex < 0) or (EntryIndex >= Launcher.Count) then Exit;
  try
    Launcher.Launch(EntryIndex);
  except
    on Exc: Exception do raise ERuntimeWarning.Create(Exc.Message);
  end;
end;

procedure TPageLauncherEntries.ActionOpenExecute(Sender: TObject);
var
  EntryIndex: integer;
  Entry: TLaunchEntry;
begin
  EntryIndex := ListBoxEntries.ItemIndex;
  if EntryIndex = -1 then
  begin
    ClearEditor;
    Exit;
  end;
  Entry := Launcher[EntryIndex];

  ToggleEditor(False);
  EditEntryName.Text := Entry.Name;
  ExecutableFileNameEdit.Text := Entry.Application; 
  MemoArguments.Text := Entry.Arguments;
  WorkingDirectoryEdit.Text := Entry.WorkingDirectory;
  RadioGroupNotification.ItemIndex := Ord(Entry.Notification);
  FComboBoxMinimizeToItemIndex := Ord(Entry.Position);
  ComboBoxMinimizeTo.ItemIndex := FComboBoxMinimizeToItemIndex;
  CheckGroupMethods.Checked[Ord(lmAutomatic)] := lmAutomatic in Entry.LaunchMethods;
  CheckGroupMethods.Checked[Ord(lmManual)] := lmManual in Entry.LaunchMethods;
  CheckGroupMethods.Checked[Ord(lmHotkey)] := lmHotkey in Entry.LaunchMethods;
  GroupBoxHotkey.Enabled := lmHotkey in Entry.LaunchMethods;
  FHotkey := Entry.Hotkey;
  if Entry.Hotkey.Value = 0 then
  begin
    ButtonHotkeyClear.Enabled := False;
    LabelHotkey.Caption := TEXT_BRACKETED_NO_SET;
  end
  else
  begin
    ButtonHotkeyClear.Enabled := True;
    TestHotkey(Entry.Hotkey);
  end;
  CheckBoxShowWindow.Checked := Entry.ShowWindow;
  ToggleEditor(True);

  if EditState = esNone then
  begin
    PanelWelcome.Visible := False;
    ScrollBoxEditor.Visible := True;
  end;
  EditState := esOpen;
end;

procedure TPageLauncherEntries.ActionCloseExecute(Sender: TObject);
begin
  if Unsaved then
  begin
    case MessageBoxW(Handle,
                     PWideChar(UnicodeString(Format(MSG_QUESTION_SAVE_LAUNCHER_ENTRY, [EditEntryName.Text]))),
                     PWideChar(UnicodeString(Application.Title)),
                     MB_YESNOCANCEL or MB_ICONQUESTION or MB_DEFBUTTON3) of
      IDYES: ActionSaveExecute(Sender);
      IDNO: Unsaved := False;
      else Exit;
    end;
  end;

  EditState := esNone;
end;

procedure TPageLauncherEntries.ActionNewExecute(Sender: TObject);
begin
  if Unsaved then
  begin
    ActionCloseExecute(Sender);
    if Unsaved then Exit;
  end;

  ClearEditor;
  EditState := esNew;
end;

procedure TPageLauncherEntries.ButtonHotkeyBindClick(Sender: TObject);
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
      EntryChange(Sender);
    end;
  finally
    FormHotkey.Free;
  end;
end;

end.

