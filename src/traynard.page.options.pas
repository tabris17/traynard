unit Traynard.Page.Options;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls, StdCtrls, Menus, ActnList, DividerBevel,
  Traynard.Page, Traynard.Types;

type

  { TPageOptions }

  TPageOptions = class(TFramePage)
    ActionLanguageReset: TAction;
    ActionLanguageApply: TAction;
    ActionLanguageRefresh: TAction;
    ActionConfigDir: TAction;
    ActionHotkeyReset: TAction;
    ActionHotkeyClear: TAction;
    ActionHotkeyBind: TAction;
    ActionList: TActionList;
    ButtonConfigDir: TButton;
    ButtonEditRules: TButton;
    ButtonHotkeyBind: TButton;
    ButtonHotkeyClear: TButton;
    ButtonHotkeyReset: TButton;
    ButtonApplyLanguage: TButton;
    ButtonRefreshLanguage: TButton;
    CheckBoxRuleOnStartup: TCheckBox;
    CheckBoxUseRules: TCheckBox;
    CheckBoxAutoMinimize: TCheckBox;
    CheckBoxIconGrouped: TCheckBox;
    CheckBoxMenuGrouped: TCheckBox;
    CheckBoxNotification: TCheckBox;
    CheckBoxAutorun: TCheckBox;
    CheckGroupSystemMenu: TCheckGroup;
    ComboBoxLanguages: TComboBox;
    DividerBevelGeneral: TDividerBevel;
    DividerBevelHotkey: TDividerBevel;
    DividerBevelAdvanced: TDividerBevel;
    EditConfigDir: TEdit;
    LabelConfigDir: TLabel;
    LabelLanguage: TLabel;
    ListViewHotkeys: TListView;
    MenuItemReset: TMenuItem;
    MenuItemModify: TMenuItem;
    MenuItemClear: TMenuItem;
    HotkeyMenu: TPopupMenu;
    PanelConfigDir: TPanel;
    procedure ActionConfigDirExecute(Sender: TObject);
    procedure ActionHotkeyClearExecute(Sender: TObject);
    procedure ActionHotkeyBindExecute(Sender: TObject);
    procedure ActionHotkeyResetExecute(Sender: TObject);
    procedure ActionLanguageApplyExecute(Sender: TObject);
    procedure ActionLanguageRefreshExecute(Sender: TObject);
    procedure ActionLanguageResetExecute(Sender: TObject);
    procedure ButtonEditRulesClick(Sender: TObject);
    procedure CheckBoxAutoMinimizeChange(Sender: TObject);
    procedure CheckBoxAutorunChange(Sender: TObject);
    procedure CheckBoxIconGroupedChange(Sender: TObject);
    procedure CheckBoxMenuGroupedChange(Sender: TObject);
    procedure CheckBoxNotificationChange(Sender: TObject);
    procedure CheckBoxUseRulesChange(Sender: TObject);
    procedure CheckGroupSystemMenuItemClick(Sender: TObject; Index: integer);
    procedure ComboBoxLanguagesChange(Sender: TObject);
    procedure ListViewHotkeysDblClick(Sender: TObject);
    procedure ListViewHotkeysSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
  public
    procedure Initialize; override;
    procedure Finalize; override;
  private
    procedure PageActivate(Sender: TObject);
    procedure AddListener(const Item: TSettingsItem; const Listener: TNotifyEvent);
    procedure AutorunChanged(Sender: TObject);
    procedure SystemMenuItemsChanged(Sender: TObject); 
    procedure IconGroupedChanged(Sender: TObject);
    procedure MenuGroupedChanged(Sender: TObject);
    procedure AutoMinimizeChanged(Sender: TObject);
    procedure UseRulesChanged(Sender: TObject); 
    procedure ShowNotificationChanged(Sender: TObject);
    procedure RuleOnStartupChanged(Sender: TObject);
    procedure HotkeyChanged(Sender: TObject);
    procedure LanguageChanged(Sender: TObject);
  end;

implementation

uses
  Windows,
  Traynard.Form.Main, Traynard.Helpers, Traynard.Settings, Traynard.Form.Hotkey,
  Traynard.Hotkey, Traynard.Strings, Traynard.Storage, Traynard.I18n;

{$R *.lfm}

{ TPageOptions }

procedure TPageOptions.ListViewHotkeysSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
  ActionHotkeyBind.Enabled := Selected;
  ActionHotkeyClear.Enabled := Selected;
  ActionHotkeyReset.Enabled := Selected;
end;

procedure TPageOptions.ActionHotkeyBindExecute(Sender: TObject);
var
  FormHotkey: TFormHotkey;
  HotkeyItem: TListItem;
begin
  HotkeyItem := ListViewHotkeys.Selected;
  FormHotkey := TFormHotkey.Create(Self.Owner);
  try
    FormHotkey.LabelTitle.Caption := HotkeyItem.Caption;
    FormHotkey.PopupMode:=pmAuto;
    FormHotkey.PopupParent:=Self.Owner as TForm;
    if FormHotkey.ShowModal = mrOK then
    begin
      if FormHotkey.ShortCut <> 0 then
      begin
        Settings.Hotkey[THotkeyID(HotkeyItem.Index)] := FormHotkey.Hotkey;
      end;
    end;
  finally
    FormHotkey.Free;
  end;
end;

procedure TPageOptions.ActionHotkeyClearExecute(Sender: TObject);
var
  HotkeyItem: TListItem;
  HotkeyID: THotkeyID;
  Hotkey: THotkey;
begin
  HotkeyItem := ListViewHotkeys.Selected;
  if not Assigned(HotkeyItem) then Exit;
  HotkeyID := THotkeyID(HotkeyItem.Index);
  Hotkey.Value := 0;
  Settings.Hotkey[HotkeyID] := Hotkey;
end;

procedure TPageOptions.ActionHotkeyResetExecute(Sender: TObject);
var
  HotkeyItem: TListItem;
  HotkeyID: THotkeyID;
  Hotkey: THotkey;
begin
  HotkeyItem := ListViewHotkeys.Selected;
  if not Assigned(HotkeyItem) then Exit;
  HotkeyID := THotkeyID(HotkeyItem.Index);
  Hotkey.Value := DEFAULT_HOTKEY_VALUES[HotkeyID];
  Settings.Hotkey[HotkeyID] := Hotkey;
end;

procedure TPageOptions.ActionLanguageApplyExecute(Sender: TObject);
var
  SelectedIndex: Integer;
begin
  SelectedIndex := ComboBoxLanguages.ItemIndex;
  Settings.Language := specialize IfThen<string>(SelectedIndex >= 0, I18n.AvailableLanguages[SelectedIndex].Code, '');
  (Sender as TAction).Enabled := False;
end;

procedure TPageOptions.ActionLanguageRefreshExecute(Sender: TObject);
var
  Lang: TLanguage;
  Index, SelectedIndex: integer;
  LangCode: string = '';
begin
  SelectedIndex := ComboBoxLanguages.ItemIndex;
  if SelectedIndex >= 0 then
    LangCode := I18n.AvailableLanguages[SelectedIndex].Code;

  I18n.RefreshAvailableLanguages;
  ComboBoxLanguages.Clear;
  SelectedIndex := 0;
  for Index := 0 to High(I18n.AvailableLanguages) do
  begin
    Lang := I18n.AvailableLanguages[Index];
    ComboBoxLanguages.Items.Add(Lang.ToString);
    if Lang.Code = LangCode then SelectedIndex := Index;
  end;
  ComboBoxLanguages.ItemIndex := SelectedIndex;
end;

procedure TPageOptions.ActionLanguageResetExecute(Sender: TObject);
var
  Index, SelectedIndex: integer;
  Lang: TLanguage;
  LangCode: string;
begin
  I18n.RefreshAvailableLanguages;
  ComboBoxLanguages.Clear;

  SelectedIndex := 0;
  LangCode := Settings.Language;
  for Index := 0 to High(I18n.AvailableLanguages) do
  begin
    Lang := I18n.AvailableLanguages[Index];
    ComboBoxLanguages.Items.Add(Lang.ToString);
    if Lang.Code = LangCode then SelectedIndex := Index;
  end;
  ComboBoxLanguages.ItemIndex := SelectedIndex;
end;

procedure TPageOptions.ButtonEditRulesClick(Sender: TObject);
begin
  FormMain.Navigate(piRules);
end;

procedure TPageOptions.CheckBoxAutoMinimizeChange(Sender: TObject);
begin
  Settings.AutoMinimize := (Sender as TCheckBox).Checked;
end;

procedure TPageOptions.CheckBoxAutorunChange(Sender: TObject);
begin
  Settings.Autorun := (Sender as TCheckBox).Checked;
end;

procedure TPageOptions.CheckBoxIconGroupedChange(Sender: TObject);
begin
  Settings.IconGrouped := (Sender as TCheckBox).Checked;
end;

procedure TPageOptions.CheckBoxMenuGroupedChange(Sender: TObject);
begin
  Settings.MenuGrouped := (Sender as TCheckBox).Checked;
end;

procedure TPageOptions.CheckBoxNotificationChange(Sender: TObject);
begin
  Settings.ShowNotification := (Sender as TCheckBox).Checked;
end;

procedure TPageOptions.CheckBoxUseRulesChange(Sender: TObject);
begin
  Settings.ApplyRules := (Sender as TCheckBox).Checked;
end;

procedure TPageOptions.CheckGroupSystemMenuItemClick(Sender: TObject; Index: integer);
var
  SystemMenuItems: TSystemMenuItems = [];
begin
  with CheckGroupSystemMenu do
  begin
    if Checked[0] then Include(SystemMenuItems, smiTrayIcon);
    if Checked[1] then Include(SystemMenuItems, smiTrayMenu);
    if Checked[2] then Include(SystemMenuItems, smiTopmost);
  end;
  Settings.SystemMenuItems := SystemMenuItems;
end;

procedure TPageOptions.ComboBoxLanguagesChange(Sender: TObject);
begin
  ActionLanguageApply.Enabled := True;
end;

procedure TPageOptions.ListViewHotkeysDblClick(Sender: TObject);
begin
  ActionHotkeyBind.Execute;
end;

procedure TPageOptions.ActionConfigDirExecute(Sender: TObject);
begin
  ShellExecuteW(0, 'open', PWideChar(unicodestring(Storage.ConfigDir)), nil, nil, SW_SHOWNORMAL);
end;

procedure TPageOptions.Initialize;
var
  OptionLabel: String;
begin
  inherited Initialize;
  OnPageActivate := @PageActivate;

  for OptionLabel in OPTION_SYSTEM_MENUS do
    CheckGroupSystemMenu.Items.Add(OptionLabel);

  AddListener(siIconGrouped, @IconGroupedChanged);
  AddListener(siMenuGrouped, @MenuGroupedChanged);
  AddListener(siSystemMenuItems, @SystemMenuItemsChanged);
  AddListener(siAutorun, @AutorunChanged);
  AddListener(siAutoMinimize, @AutoMinimizeChanged);
  AddListener(siUseRules, @UseRulesChanged);
  AddListener(siShowNotification, @ShowNotificationChanged);
  AddListener(siRuleOnStartup, @RuleOnStartupChanged);
  AddListener(siHotkey, @HotkeyChanged);

  EditConfigDir.Text := Storage.ConfigDir;

  Settings.AddListener(siLanguage, @LanguageChanged);
end;

procedure TPageOptions.Finalize;
begin
  Settings.RemoveListeners(Self);
  inherited Finalize;
end;

procedure TPageOptions.PageActivate(Sender: TObject);
begin
  ActionLanguageReset.Execute;
end;

procedure TPageOptions.AddListener(const Item: TSettingsItem; const Listener: TNotifyEvent);
begin
  Listener(Settings);
  Settings.AddListener(Item, Listener);
end;

procedure TPageOptions.AutorunChanged(Sender: TObject);
begin
  CheckBoxAutorun.Checked := (Sender as TSettings).Autorun;
end;

procedure TPageOptions.SystemMenuItemsChanged(Sender: TObject);
var
  SystemMenuItems: TSystemMenuItems;
begin
  SystemMenuItems := (Sender as TSettings).SystemMenuItems;
  with CheckGroupSystemMenu do
  begin
    Checked[0] := smiTrayIcon in SystemMenuItems;
    Checked[1] := smiTrayMenu in SystemMenuItems;
    Checked[2] := smiTopmost in SystemMenuItems;
  end;
end;

procedure TPageOptions.IconGroupedChanged(Sender: TObject);
begin
  CheckBoxIconGrouped.Checked := (Sender as TSettings).IconGrouped;
end;

procedure TPageOptions.MenuGroupedChanged(Sender: TObject);
begin
  CheckBoxMenuGrouped.Checked := (Sender as TSettings).MenuGrouped;
end;

procedure TPageOptions.AutoMinimizeChanged(Sender: TObject);
begin
  CheckBoxAutoMinimize.Checked := (Sender as TSettings).AutoMinimize;
end;

procedure TPageOptions.UseRulesChanged(Sender: TObject);
var
  AValue: boolean;
begin
  AValue := (Sender as TSettings).ApplyRules;
  CheckBoxUseRules.Checked := AValue;
  CheckBoxRuleOnStartup.Enabled := AValue;
  ButtonEditRules.Enabled := AValue;
end;

procedure TPageOptions.ShowNotificationChanged(Sender: TObject);
begin
  CheckBoxNotification.Checked := (Sender as TSettings).ShowNotification;
end;

procedure TPageOptions.RuleOnStartupChanged(Sender: TObject);
begin
  CheckBoxRuleOnStartup.Checked := (Sender as TSettings).RuleOnStartup;
end;

procedure TPageOptions.HotkeyChanged(Sender: TObject);
var
  HotkeyID: THotkeyID;
  HotkeyInfo: THotkeyInfo;
  ListItem: TListItem;
begin
  if ListViewHotkeys.Items.Count = 0 then
  begin
    for HotkeyID := Low(THotkeyID) to High(THotkeyID) do
    begin
      ListItem := ListViewHotkeys.Items.Add;
      ListItem.SubItems.Add('');
      ListItem.SubItems.Add('');
    end;
  end;

  for HotkeyID := Low(THotkeyID) to High(THotkeyID) do
  begin
    HotkeyInfo := HotkeyManager.Hotkey[HotkeyID];
    ListItem := ListViewHotkeys.Items[Ord(HotkeyID)];
    ListItem.Caption := HOTKEY_DESCRIPTIONS[HotkeyID];
    ListItem.SubItems[0] := WinHotkeyToText(HotkeyInfo.Hotkey.Modifiers, HotkeyInfo.Hotkey.Key);
    ListItem.SubItems[1] := specialize IfThen<string>(HotkeyInfo.State = hsFailed, TEXT_YES, '');
  end;
end;

procedure TPageOptions.LanguageChanged(Sender: TObject);
var
  i: integer;
  HotkeyID: THotkeyID;
begin
  for i := 0 to High(OPTION_SYSTEM_MENUS) do
    CheckGroupSystemMenu.Items[i] := OPTION_SYSTEM_MENUS[i];

  for HotkeyID := Low(THotkeyID) to High(THotkeyID) do
    ListViewHotkeys.Items[Ord(HotkeyID)].Caption := HOTKEY_DESCRIPTIONS[HotkeyID];

  ComboBoxLanguages.Items[0] := TEXT_ANGLE_BRACKETED_AUTO;
end;

end.

