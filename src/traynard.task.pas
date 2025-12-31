unit Traynard.Task;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

procedure CreateRunAsAdministratorTask;
procedure RemoveRunAsAdministratorTask;
procedure CallRunAsAdministratorTask;

implementation

uses
  Windows, ShlObj, Dialogs, LazLogger, Process, fpTemplate,
  Traynard.Types, Traynard.Helpers, Traynard.Strings, Traynard.I18n;

var
  OriginalExceptProc: TExceptProc = nil;

procedure HandleException(Sender: TObject; Addr: Pointer; FrameCount: Longint; Frames: PPointer);
begin
  if Sender is ERuntimeException then
    MessageDlg(APP_NAME, (Sender as ERuntimeException).Message, mtError, [mbOK], 0)
  else if Assigned(OriginalExceptProc) then
    OriginalExceptProc(Sender, Addr, FrameCount, Frames);
end;

procedure Initialize; inline;
begin
  I18n.Translate;
  OriginalExceptProc := ExceptProc;
  ExceptProc := @HandleException;
end;

procedure Finalize; inline;
begin
  ExceptProc := OriginalExceptProc;
end;

procedure AssertRunAsAdministrator; inline;
begin
  if not IsUserAnAdmin then
    raise ERuntimeError.Create(MSG_MUST_RUN_AS_ADMINISTRATOR);
end;

procedure Exec(const ExeFile: string; const Arguments: TStringArray);
var
  Process: TProcess;
  Argument: string;
begin
  Process := TProcess.Create(nil);
  try
    Process.ShowWindow := swoHide;
    Process.Options := [poNoConsole];
    Process.Executable := ExeFile;
    for Argument in Arguments do
      Process.Parameters.Add(Argument);
    try
      Process.Execute;
    except on Exc: EProcess do
      raise ERuntimeError.Create(Exc.Message);
    end;
    Process.WaitOnExit;
    if Process.ExitCode <> 0 then
      raise ERuntimeError.Create('Failed to execute');
  finally
    Process.Free;
  end;
end;

procedure CreateRunAsAdministratorTask;
var
  XMLPath: string;
  XMLContent: unicodestring;
  TmplContent: string = '';
  ResStream: TResourceStream;
  HtmlTmpl: TFPTemplate;
  TmplParams: TFPTemplateParams;
  FileStream: TFileStream;
  BOM: Word = $FEFF;
begin
  Initialize;
  AssertRunAsAdministrator;
  ResStream := TResourceStream.Create(HInstance, TASK_XML, RT_RCDATA);
  try
    SetLength(TmplContent, ResStream.Size);
    ResStream.ReadBuffer(TmplContent[1], ResStream.Size);
  finally
    ResStream.Free;
  end;
  XMLPath := SysUtils.GetTempFileName;
  HtmlTmpl := TFPTemplate.Create;
  try
    HtmlTmpl.Template := TmplContent;
    TmplParams := TFPTemplateParams.Create;
    try
      TmplParams.AddPair('Command', Paramstr(0));
      TmplParams.AddPair('WorkingDirectory', GetCurrentDir);
      HtmlTmpl.OnGetParam := @TmplParams.GetParam;
      XMLContent := UTF8Decode(HtmlTmpl.GetContent);
    finally
      TmplParams.Free;
    end;
  finally
    HtmlTmpl.Free;
  end;
  FileStream := TFileStream.Create(XMLPath, fmCreate);
  try
    FileStream.Write(BOM, SizeOf(BOM));
    FileStream.Write(XMLContent[1], Length(XMLContent) * SizeOf(WideChar));
  finally
    FileStream.Free;
  end;
  Exec('schtasks.exe', ['/CREATE', '/TN', APP_NAME, '/XML', XMLPath, '/F']);
  Finalize;
end;

procedure RemoveRunAsAdministratorTask;
begin
  Initialize;
  AssertRunAsAdministrator;
  Exec('schtasks.exe', ['/DELETE', '/TN', APP_NAME, '/F']);
  Finalize;
end;

procedure CallRunAsAdministratorTask;
begin
  Initialize;
  Exec('schtasks.exe', ['/RUN', '/TN', APP_NAME]);
  Finalize;
end;

end.

