unit Traynard.Form.Popup;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, Traynard.Types;

type

  { TFormPopup }

  TFormPopup = class(TForm)
    ImageIcon: TImage;
    LabelMessage: TLabel;
    LabelTitle: TLabel;
    ShapeClose: TShape;
    TimerClose: TTimer;
    TimerFadeIn: TTimer;
    TimerFadeOut: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure ShapeCloseClick(Sender: TObject);
    procedure ShapeClosePaint(Sender: TObject);
    procedure TimerFadeInTimer(Sender: TObject);
    procedure TimerCloseTimer(Sender: TObject);
    procedure TimerFadeOutTimer(Sender: TObject);
  private
    procedure SetPopupType(AValue: TPopupType);
    procedure SetMessage(AValue: TCaption);
    procedure SetTimeout(AValue: Cardinal);
    procedure SetTitle(AValue: TCaption);
  public
    property Timeout: Cardinal write SetTimeout;
    property PopupType: TPopupType write SetPopupType;
    property Title: TCaption write SetTitle;
    property Message: TCaption write SetMessage;
  end;

const
  ALPHA_BLEND_VALUE_STEP = 15;

implementation

{$R *.lfm}

{ TFormPopup }

procedure TFormPopup.TimerCloseTimer(Sender: TObject);
begin
  TimerFadeOut.Enabled := True;
end;

procedure TFormPopup.TimerFadeOutTimer(Sender: TObject);
begin
  AlphaBlendValue := AlphaBlendValue - ALPHA_BLEND_VALUE_STEP;
  if AlphaBlendValue = 0 then
  begin
    TimerFadeOut.Enabled := False;
    Free;
  end;
end;

procedure TFormPopup.SetPopupType(AValue: TPopupType);
begin
  ImageIcon.Visible := True;
  case AValue of
    ptWarning:
      ImageIcon.Picture.LoadFromResourceName(HInstance, 'dialog_warning');
    ptError:
      ImageIcon.Picture.LoadFromResourceName(HInstance, 'dialog_error');
    ptInformation:
      ImageIcon.Picture.LoadFromResourceName(HInstance, 'dialog_information');
    ptConfirmation:
      ImageIcon.Picture.LoadFromResourceName(HInstance, 'dialog_confirmation');
    ptShield:
      ImageIcon.Picture.LoadFromResourceName(HInstance, 'dialog_shield');
    ptNone:
    begin
      ImageIcon.Picture.Clear;
      ImageIcon.Visible := False;
    end;
  end;
end;

procedure TFormPopup.SetMessage(AValue: TCaption);
begin
  LabelMessage.Caption := AValue;
end;

procedure TFormPopup.SetTimeout(AValue: Cardinal);
begin
  TimerClose.Interval := AValue;
end;

procedure TFormPopup.SetTitle(AValue: TCaption);
begin
  LabelTitle.Caption := AValue;
end;

procedure TFormPopup.TimerFadeInTimer(Sender: TObject);
begin
  AlphaBlendValue := AlphaBlendValue + ALPHA_BLEND_VALUE_STEP;
  if AlphaBlendValue = High(Byte) then
  begin
    TimerFadeIn.Enabled := False;
  end;
end;

procedure TFormPopup.FormCreate(Sender: TObject);
var
  ParentForm: TForm;
begin
  ParentForm := Owner as TForm;
  Parent := ParentForm;
  PopupParent := ParentForm;
end;

procedure TFormPopup.ShapeCloseClick(Sender: TObject);
begin
  TimerFadeIn.Enabled := False;
  AlphaBlendValue := ALPHA_BLEND_VALUE_STEP;
  TimerFadeOut.Enabled := True;
end;

procedure TFormPopup.ShapeClosePaint(Sender: TObject);
begin
  with Sender as TShape do
  begin
    Canvas.Line(1, 1, Width - 1, Height - 1);
    Canvas.Line(Width - 1, 1, 1, Height - 1);
  end;
end;

end.

