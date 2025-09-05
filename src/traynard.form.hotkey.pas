unit Traynard.Form.Hotkey;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Traynard.Types;

type

  { TFormHotkey }

  TFormHotkey = class(TForm)
    ButtonOK: TButton;
    ButtonCancel: TButton;
    LabelHotkey: TLabel;
    LabelTitle: TLabel;
    procedure ButtonCancelClick(Sender: TObject);
    procedure ButtonOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure LabelHotkeyResize(Sender: TObject);
  private
    FShortCut: TShortCut;
    procedure CenterLabelHotkey;
    function GetHotkey: THotkey;
  public
    property ShortCut: TShortCut read FShortCut;
    property Hotkey: THotkey read GetHotkey;
  end;

implementation

uses LCLType, Windows, Traynard.Helpers;

const MODIFIERS = [
  VK_SHIFT, VK_LSHIFT, VK_RSHIFT,
  VK_LWIN, VK_RWIN,
  VK_CONTROL, VK_LCONTROL, VK_RCONTROL,
  VK_MENU, VK_LMENU, VK_RMENU
];

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
begin
  FShortCut := 0;
end;

procedure TFormHotkey.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key in MODIFIERS) or (key = VK_ESCAPE) and (Shift = []) then Exit;
  LabelHotkey.Caption := KeyAndShiftStateToKeyString(Key, Shift);
  FShortCut := KeyToShortCut(Key, Shift);
end;

procedure TFormHotkey.FormShow(Sender: TObject);
begin
  CenterLabelHotkey;
end;

procedure TFormHotkey.LabelHotkeyResize(Sender: TObject);
begin
  CenterLabelHotkey;
end;

procedure TFormHotkey.CenterLabelHotkey;
begin
  LabelHotkey.Left := (Width - LabelHotkey.Width) div 2;
end;

function TFormHotkey.GetHotkey: THotkey;
var
  Key, TheModifiers: Word;
begin
  ShortCutToWinHotkey(FShortCut, Key, TheModifiers);
  Result.Key := Key;
  Result.Modifiers := TheModifiers;
end;

end.

