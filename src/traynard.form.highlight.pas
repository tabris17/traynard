unit Traynard.Form.Highlight;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Windows;

type

  { TFormHighlight }

  TFormHighlight = class(TForm)
    Timer: TTimer;
    procedure FormShow(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    FAttachedWindow: HWND;
  public
    constructor Create(AttachedWindow: HWND); overload;
    procedure UpdatePosition;
  end;

implementation

uses
  DwmAPI, Traynard.Settings;

{$R *.lfm}

{ TFormHighlight }

procedure TFormHighlight.UpdatePosition;
var
  Rect: TRect;
  Thickness, DblThickness: longint;
begin
  Color := Settings.HighlightTopmostColor;        
  Thickness := Settings.HighlightTopmostThickness;
  DblThickness := Thickness shl 1;
  if DwmGetWindowAttribute(FAttachedWindow,
                           DWMWA_EXTENDED_FRAME_BOUNDS,
                           @Rect,
                           SizeOf(Rect)) <> S_OK then Exit;
  SetWindowPos(Handle, FAttachedWindow,
               Rect.Left - Thickness,
               Rect.Top - Thickness,
               Rect.Width + DblThickness,
               Rect.Height + DblThickness,
               SWP_NOREDRAW or SWP_NOACTIVATE);
end;

procedure TFormHighlight.TimerTimer(Sender: TObject);
begin
  UpdatePosition;
end;

procedure TFormHighlight.FormShow(Sender: TObject);
begin
  UpdatePosition;
end;

constructor TFormHighlight.Create(AttachedWindow: HWND);
begin
  inherited Create(nil);
  FAttachedWindow := AttachedWindow;
  SetWindowLongPtr(Self.Handle, GWL_EXSTYLE,
    GetWindowLongPtr(Self.Handle, GWL_EXSTYLE) or WS_EX_TOOLWINDOW);
  Show;
end;

end.

