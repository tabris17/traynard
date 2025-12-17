unit Traynard.Form.Highlight;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Windows;

type

  { TFormHighlight }

  TFormHighlight = class(TForm)
    Timer: TTimer;
    procedure Track(Sender: TObject);
  private
    FAttachedWindow: HWND;
    FThickness: byte;
  public
    constructor Create(AttachedWindow: HWND); overload;
  end;

implementation

uses
  DwmAPI, Traynard.Settings;

{$R *.lfm}

{ TFormHighlight }

procedure TFormHighlight.Track(Sender: TObject);
var
  Rect: TRect;
  Thickness, DblThickness: longint;
begin
  Timer.Enabled := False;
  Color := Settings.HighlightTopmostColor;        
  Thickness := Settings.HighlightTopmostThickness;
  DblThickness := Thickness shl 1;
  try
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
  finally
    Timer.Enabled := True;
  end;
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

