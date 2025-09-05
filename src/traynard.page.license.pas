unit Traynard.Page.License;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Traynard.Page;

type

  { TPageLicense }

  TPageLicense = class(TFramePage)
    Memo: TMemo;
    procedure PageLicenseActivate(Sender: TObject);
    procedure PageLicenseDeactivate(Sender: TObject);
  public
    procedure Initialize; override;
  end;

implementation

uses Windows, Traynard.Types;

{$R *.lfm}

{ TPageLicense }

procedure TPageLicense.PageLicenseActivate(Sender: TObject);
var
  ResourceStream: TResourceStream;
begin
  ResourceStream:=TResourceStream.Create(HInstance, LICENSE_TXT, RT_RCDATA);
  try
    Memo.Lines.LoadFromStream(ResourceStream);
  finally
    ResourceStream.Free;
  end;
end;

procedure TPageLicense.PageLicenseDeactivate(Sender: TObject);
begin
  Memo.Clear;
end;

procedure TPageLicense.Initialize;
begin
  inherited;
  OnPageActivate:=@PageLicenseActivate;
  OnPageDeactivate:=@PageLicenseDeactivate;
end;

end.

