unit Traynard.Highlight;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Windows;

type

  { TFrameDrawer }

  TFrameDrawer = class
  private
    FWindow: HWND;
  public
    constructor Create(Window: HWND);
    procedure Show;
    procedure Hide;
    procedure Draw(const Rect: TRect; const Thickness: byte; const Color: TColor);
  end;

  { THighlightFrame }

  THighlightFrame = class
  private
    FSize: TSize;
    FColor: TColor;
    FResized: boolean;
    FWindow: HWND;
    FTrackingWindow: HWND;
    FTimerId: PtrUInt;
    FDrawer: TFrameDrawer;
    function GetFrameRect(const Window: HWND; out Rect: TRect): boolean;
    procedure SetVisible(AValue: boolean);
  public
    constructor Create(const TrackingWindow: HWND);
    destructor Destroy; override;
    property Visible: boolean write SetVisible;
    procedure UpdatePosition;
    class function WindowProc(Window: HWND; Message: UINT; WParam: WPARAM; LParam: LPARAM): LRESULT; stdcall; static;
  end;

implementation

uses
  JwaWinUser, DwmApi, Traynard.Strings, Traynard.Types, Traynard.Helpers, Traynard.Settings;

const
  REFRESH_BORDER_TIMER_ID = 123;
  REFRESH_BORDER_INTERVAL = 500;

function ColorToARGB(Color: TColor; Alpha: Byte = $FF): Cardinal; inline;
var
  c: LongInt;
  r, g, b: Byte;
begin
  c := ColorToRGB(Color);  // 先转成标准RGB

  r := c and $FF;
  g := (c shr 8) and $FF;
  b := (c shr 16) and $FF;

  Result := (Cardinal(Alpha) shl 24) or
            (Cardinal(r) shl 16) or
            (Cardinal(g) shl 8)  or
            b;
end;

{ TFrameDrawer }

constructor TFrameDrawer.Create(Window: HWND);
begin
  FWindow := Window;
  Show;
end;

procedure TFrameDrawer.Show;
begin
  ShowWindow(FWindow, SW_SHOWNA);
  UpdateWindow(FWindow);
end;

procedure TFrameDrawer.Hide;
begin
  ShowWindow(FWindow, SW_HIDE);
end;

procedure TFrameDrawer.Draw(const Rect: TRect; const Thickness: byte; const Color: TColor);
var
  HdcScreen: HDC;
  HdcMem: HDC;
  Bmi: BITMAPINFO;
  Bits: Pointer;
  HBmp, HOldBmp: HBITMAP;
  PtSrc, PtDst: POINT;
  Size: TSIZE;
  Blend: BLENDFUNCTION;
  X, Y: Integer;
  Ptr: PCardinal;
begin
  Bmi := Default(BITMAPINFO);
  Bmi.bmiHeader.biSize := SizeOf(BITMAPINFOHEADER);
  Bmi.bmiHeader.biWidth := Rect.Width;
  Bmi.bmiHeader.biHeight := -Rect.Height;
  Bmi.bmiHeader.biPlanes := 1;
  Bmi.bmiHeader.biBitCount := 32;
  Bmi.bmiHeader.biCompression := BI_RGB;

  HdcScreen := GetDC(0);
  HdcMem := CreateCompatibleDC(HdcScreen);

  HBmp := CreateDIBSection(HdcScreen, Bmi, DIB_RGB_COLORS, Bits, 0, 0);
  HOldBmp := SelectObject(HdcMem, HBmp);

  FillChar(Bits^, Rect.Width * Rect.Height * 4, 0);
  Ptr := Bits;

  for Y := 0 to Rect.Height - 1 do
  begin
    for X := 0 to Rect.Width - 1 do
    begin
      if (X < Thickness) or (X >= Rect.Width - Thickness) or
         (Y < Thickness) or (Y >= Rect.Height - Thickness) then
        Ptr^ := ColorToARGB(Color);
      Inc(Ptr);
    end;
  end;

  PtSrc := Default(Point);
  PtDst := Default(Point);
  Size.cx := Rect.Width;
  Size.cy := Rect.Height;

  Blend.BlendOp := AC_SRC_OVER;
  Blend.BlendFlags := 0;
  Blend.SourceConstantAlpha := 255;
  Blend.AlphaFormat := AC_SRC_ALPHA;

  UpdateLayeredWindow(
    FWindow,
    HdcScreen,
    @PtDst,
    @Size,
    HdcMem,
    @PtSrc,
    0,
    @Blend,
    ULW_ALPHA
  );

  SelectObject(HdcMem, HOldBmp);
  DeleteObject(HBmp);
  DeleteDC(HdcMem);
  ReleaseDC(0, HdcScreen);
end;

{ THighlightFrame }

function THighlightFrame.GetFrameRect(const Window: HWND; out Rect: TRect): boolean;
var
  Thickness: byte;
begin
  if not SUCCEEDED(DwmGetWindowAttribute(Window, DWMWA_EXTENDED_FRAME_BOUNDS, @Rect, SizeOf(Rect))) then
    Exit(False);

  Thickness := Settings.HighlightTopmostThickness;
  Dec(Rect.Top, Thickness);
  Dec(Rect.Left, Thickness);
  Inc(Rect.Right, Thickness);
  Inc(Rect.Bottom, Thickness);

  FResized := (FSize.Width <> Rect.Width) or (FSize.Height <> Rect.Height);
  if FResized then
  begin
    FSize.Width := Rect.Width;
    FSize.Height := Rect.Height;
  end;

  Result := True;
end;

procedure THighlightFrame.SetVisible(AValue: boolean);
begin
  if AValue then
    FDrawer.Show
  else
    FDrawer.Hide;
end;

constructor THighlightFrame.Create(const TrackingWindow: HWND);
var
  WndClass: TWNDCLASSW;
  FrameRect: TRect;
  Rgn: HRGN;
  Pos: integer;
  Bh: DWM_BLURBEHIND;
begin
  FTrackingWindow := TrackingWindow;
  FResized := False;
  FSize := Default(TSize);
  FColor := Settings.HighlightTopmostColor;

  if not GetFrameRect(TrackingWindow, FrameRect) then
    raise ERuntimeException.Create(ERROR_CREATE_HIGHLIGHT_FRAME, GetLastErrorMsg);

  WndClass := Default(TWNDCLASSW);
  WndClass.lpfnWndProc := @WindowProc;
  WndClass.hInstance := HInstance;
  WndClass.lpszClassName := CLASS_NAME_HIGHLIGHT;
  WndClass.hCursor := LoadCursor(0, IDC_ARROW);
  Windows.RegisterClassW(@WndClass);

  FWindow := CreateWindowExW(
    WS_EX_LAYERED or WS_EX_TOPMOST or WS_EX_TOOLWINDOW,
    CLASS_NAME_HIGHLIGHT,
    '',
    WS_POPUP or WS_DISABLED,
    FrameRect.Left,
    FrameRect.Top,
    FrameRect.Width,
    FrameRect.Height,
    0, 0,
    HInstance,
    Self
  );

  if FWindow = 0 then
    raise ERuntimeException.Create(ERROR_CREATE_HIGHLIGHT_FRAME, GetLastErrorMsg);

  Pos := -GetSystemMetrics(SM_CXVIRTUALSCREEN) - 8;
  Rgn := CreateRectRgn(Pos, 0, (Pos + 1), 1);
  if Rgn <> 0 then
  begin
    Bh.dwFlags := DWM_BB_ENABLE or DWM_BB_BLURREGION;
    Bh.fEnable := True;
    Bh.hRgnBlur:= Rgn;
    Bh.fTransitionOnMaximized := False;
    DwmEnableBlurBehindWindow(FWindow, @Bh);
  end;

  SetWindowPos(
    TrackingWindow,
    FWindow,
    FrameRect.Left,
    FrameRect.Top,
    FrameRect.Right - FrameRect.Left,
    FrameRect.Bottom - FrameRect.Top,
    SWP_NOMOVE or SWP_NOSIZE
  );

  FDrawer := TFrameDrawer.Create(FWindow);
  FDrawer.Draw(FrameRect, Settings.HighlightTopmostThickness, Settings.HighlightTopmostColor);
  UpdatePosition;
end;

destructor THighlightFrame.Destroy;
begin                    
  FreeAndNil(FDrawer);
  DestroyWindow(FWindow);
  inherited Destroy;
end;

procedure THighlightFrame.UpdatePosition;
var
  FrameRect: TRect;
  ColorChanged: boolean = False;
begin
  if not GetFrameRect(FTrackingWindow, FrameRect) then
  begin
    FDrawer.Hide;
    Exit;
  end;

  if FColor <> Settings.HighlightTopmostColor then
  begin
    ColorChanged := True;
    FColor := Settings.HighlightTopmostColor;
  end;

  if FResized or ColorChanged then
    FDrawer.Draw(FrameRect, Settings.HighlightTopmostThickness, Settings.HighlightTopmostColor);

  SetWindowPos(
    FWindow,
    FTrackingWindow,
    FrameRect.Left,
    FrameRect.Top,
    FrameRect.Width,
    FrameRect.Height,
    SWP_NOREDRAW or SWP_NOACTIVATE
  );
end;

class function THighlightFrame.WindowProc(Window: HWND; Message: UINT; WParam: WPARAM; LParam: LPARAM): LRESULT; stdcall;
var
  This: THighlightFrame;
begin
  This := THighlightFrame(GetWindowLongPtr(Window, GWLP_USERDATA));

  case Message of
    WM_CREATE:
    begin
      This := THighlightFrame(LPCREATESTRUCT(LParam)^.lpCreateParams);
      SetWindowLongPtr(Window, GWLP_USERDATA, PtrInt(This));
      This.FTimerId := SetTimer(Window, REFRESH_BORDER_TIMER_ID, REFRESH_BORDER_INTERVAL, nil);
    end;
    WM_NCDESTROY:
    begin
      KillTimer(This.FWindow, This.FTimerId);
      Result := DefWindowProc(Window, Message, WParam, LParam);
    end;
    WM_TIMER:
    begin
      if WParam = REFRESH_BORDER_TIMER_ID then
      begin
        KillTimer(This.FWindow, This.FTimerId);
        This.FTimerId := SetTimer(Window, REFRESH_BORDER_TIMER_ID, REFRESH_BORDER_INTERVAL, nil);
        This.UpdatePosition;
      end;
    end;
    WM_SETCURSOR:
    begin
      SetCursor(LoadCursor(0, IDC_ARROW));
      Result := 1;
    end
    else
      Result := DefWindowProc(Window, Message, WParam, LParam);
  end;
end;

end.

