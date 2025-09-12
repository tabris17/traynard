unit Traynard.Session;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Traynard.Types;

type

  { TSession }

  TSession = class
  private
    FStream: TFileStream;
    FHandles: THandleArray;
    procedure SetHandles(AValue: THandleArray);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Start;
    property Handles: THandleArray read FHandles write SetHandles;
  end;

var
  Session: TSession = nil;

const
  SESSION_FILENAME = 'session';

implementation

uses
  Windows, Traynard.Storage;

{ TSession }

procedure TSession.SetHandles(AValue: THandleArray);
var
  Size: SizeInt;
begin
  if FHandles = AValue then Exit;
  FHandles := AValue;
  Size := Length(AValue);
  FStream.Size := 0;
  if Size = 0 then Exit;
  FStream.WriteBuffer(FHandles[0], Size * SizeOf(HWND));
end;

constructor TSession.Create;
begin
  FStream := nil;
  FHandles := nil;
end;

destructor TSession.Destroy;
var
  Filename: string;
begin
  if Assigned(FStream) then
  begin
    Filename := FStream.FileName;
    FreeAndNil(FStream);
    SysUtils.DeleteFile(Filename);
  end;
  inherited Destroy;
end;

procedure TSession.Start;
var
  Size: SizeInt;
begin
  Storage.OpenOrCreate(SESSION_FILENAME, FStream);
  if FStream.Size = 0 then Exit;
  Size := Fstream.Size div SizeOf(HWND);
  if Size = 0 then Exit;
  SetLength(FHandles, Size);
  FStream.Seek(0, soBeginning);
  FStream.ReadBuffer(FHandles[0], Size * SizeOf(HWND));
end;

initialization

Session := TSession.Create;

finalization

FreeAndNil(Session);

end.

