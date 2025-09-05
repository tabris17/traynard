unit Traynard.Notification;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms;

type

  { TNotificationManager }

  TNotificationManager = class(TComponent)
  private
    FMainForm: TForm;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Notify(const Title, Message: string; Timeout: integer = 0);
  end;

var
  NotificationManager: TNotificationManager = nil;

implementation

uses
  Traynard.Form.Background;

{ TNotificationManager }

constructor TNotificationManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FMainForm := AOwner as TForm;
end;

procedure TNotificationManager.Notify(const Title, Message: string; Timeout: integer);
begin
  (FMainForm as TFormBackground).Notify(Title, Message, Timeout);
end;

end.

