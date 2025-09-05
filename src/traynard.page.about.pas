unit Traynard.Page.About;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, IpHtml, IpHtmlNodes, IpFileBroker, FileInfo,
  Traynard.Page;

type

  { TPageAbout }

  TPageAbout = class(TFramePage)
    HtmlPanel: TIpHtmlPanel;
    HtmlDataProvider: TIpHtmlDataProvider;
    procedure FrameResize(Sender: TObject);
    procedure HtmlDataProviderGetImage(Sender: TIpHtmlNode; const URL: string; var Picture: TPicture);
    procedure HtmlPanelHotClick(Sender: TObject);
    procedure PageAboutActivate(Sender: TObject);
    procedure PageAboutDeactivate(Sender: TObject);
  private
    FVersionInfo: TFileVersionInfo;
  public
    procedure Initialize; override;
    procedure Finalize; override;
  end;


implementation

uses
  LCLVersion, LCLIntf, LCLType, LCLPlatformDef, fpTemplate,
  Traynard.Form.Main, Traynard.Strings, Traynard.Helpers, Traynard.Types;

{$R *.lfm}

{ TPageAbout }

procedure TPageAbout.HtmlPanelHotClick(Sender: TObject);
const
  ABOUT_PROTOCOL = 'about:';
var
  Node: TIpHtmlNode;
  URL: string;
begin
  Node := (Sender as TIpHtmlPanel).HotNode;
  if Node is TIpHtmlNodeA then
  begin
    URL := (Node as TIpHtmlNodeA).HRef;
    if StrStartsWith(URL, ABOUT_PROTOCOL, False) then
    begin
      case RightStr(URL, Length(URL) - Length(ABOUT_PROTOCOL)) of
        'license': FormMain.Navigate(piLicense);
      end;
    end
    else OpenURL(URL);
  end;
end;

procedure TPageAbout.PageAboutActivate(Sender: TObject);
var
  HtmlTmpl: TFPTemplate;
  ResStream: TResourceStream;
  TmplContent: string = '';
  TmplParams: TFPTemplateParams;
begin
  {$IFDEF DEBUG}
  HtmlPanel.FlagErrors:=True;
  {$ENDIF}
  ResStream := TResourceStream.Create(HInstance, ABOUT_HTML, RT_RCDATA);
  try
    SetLength(TmplContent, ResStream.Size);
    ResStream.ReadBuffer(TmplContent[1], ResStream.Size);
  finally
    ResStream.Free;
  end;

  HtmlTmpl:=TFPTemplate.Create;
  try
    HtmlTmpl.Template:=TmplContent;
    TmplParams:=TFPTemplateParams.Create;
    try
      TmplParams.AddPair('AppName', APP_NAME); 
      TmplParams.AddPair('Title', HTML_ABOUT_TITLE);
      TmplParams.AddPair('Description', FVersionInfo.VersionStrings.Values['FileDescription']);
      TmplParams.AddPair('LabelSourceCode', HTML_ABOUT_SOURCE_CODE);
      TmplParams.AddPair('LabelLicense', HTML_ABOUT_LICENSE);
      TmplParams.AddPair('LabelBuildDate', HTML_ABOUT_BUILD_DATE);
      TmplParams.AddPair('LabelMaintainer', HTML_ABOUT_MAINTAINER);
      TmplParams.AddPair('LabelVersion', HTML_ABOUT_VERSION);
      TmplParams.AddPair('LabelCompiler', HTML_ABOUT_COMPILER);
      TmplParams.AddPair('GitHub', URL_GITHUB);
      TmplParams.AddPair('License', LICENSE);
      TmplParams.AddPair('BuildDate', {$I %DATE%});
      TmplParams.AddPair('Version', FVersionInfo.VersionStrings.Values['FileVersion']);
      TmplParams.AddPair('Maintainer', MAINTAINER);
      TmplParams.AddPair('FPCVersion', {$I %FPCVERSION%});
      TmplParams.AddPair('LCLVersion', LCLVersion);
      TmplParams.AddPair('Platform', {$I %FPCTARGETCPU});
      TmplParams.AddPair('Intro', HTML_ABOUT_INTRO);
      TmplParams.AddPair('FeatureManagedWindow', HTML_ABOUT_FEATURE_1);
      TmplParams.AddPair('FeatureHotkeys', HTML_ABOUT_FEATURE_2);
      TmplParams.AddPair('FeatureSystemMenu', HTML_ABOUT_FEATURE_3);
      TmplParams.AddPair('FeatureRules', HTML_ABOUT_FEATURE_4);
      HtmlTmpl.OnGetParam:=@TmplParams.GetParam;
      HtmlPanel.SetHtmlFromStr(HtmlTmpl.GetContent);
    finally
      TmplParams.Free;
    end;
  finally
    HtmlTmpl.Free;
  end;

  HeaderTitle.Caption:=HtmlPanel.Title;
end;

procedure TPageAbout.PageAboutDeactivate(Sender: TObject);
begin
  HtmlPanel.SetHtml(nil);
end;

procedure TPageAbout.HtmlDataProviderGetImage(Sender: TIpHtmlNode; const URL: string; var Picture: TPicture);
begin
  { Picture object is managed by HtmlDataProvider and does not require manual free }
  Picture:=TPicture.Create;
  case URL of
    'logo.png': Picture.LoadFromResourceName(HInstance, LOGO_PNG);
  end;
end;

procedure TPageAbout.FrameResize(Sender: TObject);
begin
  HtmlPanel.Width:=Width;
end;

procedure TPageAbout.Initialize;
begin
  inherited;
  FVersionInfo:=TFileVersionInfo.Create(Self);
  FVersionInfo.ReadFileInfo;
  OnPageActivate:=@PageAboutActivate;
  OnPageDeactivate:=@PageAboutDeactivate;
end;

procedure TPageAbout.Finalize;
begin
  inherited Finalize;
end;

end.

