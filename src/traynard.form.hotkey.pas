unit Traynard.Form.Hotkey;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, ComCtrls, Traynard.Types;

type

  { TFormHotkey }

  TFormHotkey = class(TForm)
    ButtonCancel: TButton;
    ButtonOK: TButton;
    CheckBoxShift: TCheckBox;
    CheckBoxCtrl: TCheckBox;
    CheckBoxAlt: TCheckBox;
    CheckBoxWin: TCheckBox;
    ComboBoxKey: TComboBox;
    LabelHotkey: TLabel;
    PanelTabs: TPanel;
    PanelCapture: TPanel;
    PanelLabel: TPanel;
    PanelManual: TPanel;
    PanelButtons: TPanel;
    ToolBarSwitch: TToolBar;
    ToolButtonCapture: TToolButton;
    ToolButtonManual: TToolButton;
    procedure ButtonCancelClick(Sender: TObject);
    procedure ButtonOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure LabelHotkeyResize(Sender: TObject);
    procedure ToolButtonCaptureClick(Sender: TObject);
    procedure ToolButtonManualClick(Sender: TObject);
  type
    TEditMode = (emCapture, emManual);
  private
    FCaptureShortCut: TShortCut;
    FEditMode: TEditMode;
    procedure CenterLabelHotkey;
    procedure CenterPanelControls(const Panel: TPanel; const Spacing: integer);
    function GetHotkey: THotkey;
    function GetShortCut: TShortCut;
  public
    property ShortCut: TShortCut read GetShortCut;
    property Hotkey: THotkey read GetHotkey;
    function ShowModal(const Title: TCaption): integer; overload;
  end;

implementation

uses LCLType, LCLProc, Menus, Windows, Traynard.Helpers;

const
  MODIFIERS = [
    VK_SHIFT, VK_LSHIFT, VK_RSHIFT,
    VK_LWIN, VK_RWIN,
    VK_CONTROL, VK_LCONTROL, VK_RCONTROL,
    VK_MENU, VK_LMENU, VK_RMENU
  ];

  PANEL_TITLE_HEIGHT = 24;

  VK_CODES = [
    VK_A..VK_Z, VK_0..VK_9, VK_F1..VK_F12, VK_SPACE,
    VK_END..VK_DOWN, VK_INSERT..VK_DELETE,
    VK_BACK..VK_TAB, VK_ESCAPE, VK_OEM_1..VK_OEM_3
  ];

var
  VKeys: array of Word;

{$R *.lfm}

{ TFormHotkey }

procedure TFormHotkey.ButtonCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TFormHotkey.ButtonOKClick(Sender: TObject);
begin
  ModalResult := mrOK;
end;

procedure TFormHotkey.FormCreate(Sender: TObject);
var
  KeyCode: Word;
  Index: integer = 0;
begin
  FEditMode := emCapture;
  FCaptureShortCut := 0;

  if not Assigned(VKeys) then
  begin
    for KeyCode in VK_CODES do
    begin
      SetLength(VKeys, Index + 1);
      VKeys[Index] := KeyCode;
      Inc(Index);
    end;
  end;
  for Index := 0 to High(VKeys) do
    ComboBoxKey.Items.Add(KeyAndShiftStateToKeyString(VKeys[Index], []));
end;

procedure TFormHotkey.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (FEditMode <> emCapture) or (Key in MODIFIERS) or (key = VK_ESCAPE) and (Shift = []) then Exit;
  LabelHotkey.Caption := KeyAndShiftStateToKeyString(Key, Shift);
  FCaptureShortCut := KeyToShortCut(Key, Shift);
end;

procedure TFormHotkey.FormShow(Sender: TObject);
begin
  CenterLabelHotkey;
  CenterPanelControls(PanelButtons, 10);
  CenterPanelControls(PanelManual, 4);
end;

procedure TFormHotkey.LabelHotkeyResize(Sender: TObject);
begin
  CenterLabelHotkey;
end;

procedure TFormHotkey.ToolButtonCaptureClick(Sender: TObject);
begin
  PanelCapture.Visible := True;
  PanelManual.Visible := False;
  FEditMode := emCapture;
end;

procedure TFormHotkey.ToolButtonManualClick(Sender: TObject);
begin
  PanelCapture.Visible := False;
  PanelManual.Visible := True;
  FEditMode := emManual;
end;

procedure TFormHotkey.CenterLabelHotkey;
begin
  LabelHotkey.Left := (Width - LabelHotkey.Width) div 2;
end;

procedure TFormHotkey.CenterPanelControls(const Panel: TPanel; const Spacing: integer);
var
  Index, LastIndex: integer;
  Control: TControl;
  ChildrenWidth: integer;
begin
  LastIndex := Panel.ControlCount - 1;
  ChildrenWidth := LastIndex * Spacing;
  for Index := LastIndex downto 0 do
  begin
    Control := Panel.Controls[Index];
    Inc(ChildrenWidth, Control.Width);
  end;
  Control.Left := (Panel.Width - ChildrenWidth) div 2;
end;

function TFormHotkey.GetHotkey: THotkey;
var
  Key, TheModifiers: Word;
begin
  ShortCutToWinHotkey(ShortCut, Key, TheModifiers);
  Result.Key := Key;
  Result.Modifiers := TheModifiers;
end;

function TFormHotkey.GetShortCut: TShortCut;
var
  KeyItemIndex: integer;
  ShiftState: TShiftState = [];
begin
  case FEditMode of
    emCapture: Exit(FCaptureShortCut);
    emManual:
    begin
      KeyItemIndex := ComboBoxKey.ItemIndex;
      if (KeyItemIndex < Length(VKeys)) and (KeyItemIndex >= 0) then
      begin
        if CheckBoxShift.Checked then Include(ShiftState, ssShift);
        if CheckBoxCtrl.Checked then Include(ShiftState, ssCtrl);
        if CheckBoxAlt.Checked then Include(ShiftState, ssAlt);
        if CheckBoxWin.Checked then Include(ShiftState, ssMeta);
        Exit(Menus.ShortCut(VKeys[KeyItemIndex], ShiftState));
      end;
    end;
  end;
  Result := 0;
end;

function TFormHotkey.ShowModal(const Title: TCaption): integer;
begin
  if Title <> '' then
  begin
    PanelLabel.Visible := True;
    PanelLabel.Height := PANEL_TITLE_HEIGHT;
    PanelLabel.Caption := Title;
  end;

  Result := ShowModal();
end;

end.

