unit unt_download;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, lNet, lnetbase, lNetComponents, lhttp, lHTTPUtil, Forms;

type

  TOnDownloadProgress = procedure(Sender: TObject; APercent: integer) of object;
  TOnDownloadError = procedure(Sender: TObject; AMsg: string) of object;
  TOnDownloadComplete = procedure(Sender: TObject) of object;

  { TDownloader }

  TDownloader = class
  private
    FDownloading: boolean;
    FHttp: TLHTTPClientComponent;
    FOnDownloadComplete: TOnDownloadComplete;
    FOnDownloadError: TOnDownloadError;
    FOnDownloadProgress: TOnDownloadProgress;
    FSSL: TLSSLSessionComponent;
    FDownloadFile: TFileStream;
    FTotalLength: longint;
    FCompleted: longint;
  protected
    function httpInput(ASocket: TLHTTPClientSocket; ABuffer: PChar;
      ASize: integer): integer;
    procedure httpDoneInput(ASocket: TLHTTPClientSocket);
    procedure httpError(const msg: string; aSocket: TLSocket);
    procedure httpHead(ASocket: TLHTTPClientSocket);
  public
    constructor Create;
    destructor Destroy; override;
    procedure StartDownload(AUrl: string; AFileName: string);
    procedure StopDownload;
  public
    property OnDownloadProgress: TOnDownloadProgress
      read FOnDownloadProgress write FOnDownloadProgress;
    property OnDownloadError: TOnDownloadError
      read FOnDownloadError write FOnDownloadError;
    property OnDownloadComplete: TOnDownloadComplete
      read FOnDownloadComplete write FOnDownloadComplete;
  end;

implementation

{ TDownloader }

function TDownloader.httpInput(ASocket: TLHTTPClientSocket; ABuffer: PChar;
  ASize: integer): integer;
begin
  FDownloadFile.Write(ABuffer^, ASize);
  FCompleted := FCompleted + ASize;
  if Assigned(FOnDownloadProgress) then
  begin
    FOnDownloadProgress(Self, Trunc(FCompleted * 100 / FTotalLength));
  end;
  Application.ProcessMessages;

  Result := ASize;
end;

procedure TDownloader.httpDoneInput(ASocket: TLHTTPClientSocket);
begin
  FDownloading := False;
  ASocket.Disconnect(True);
  if Assigned(FOnDownloadProgress) then
  begin
    FOnDownloadProgress(Self, 100);
  end;
  if Assigned(FOnDownloadComplete) then
  begin
    FOnDownloadComplete(Self);
  end;
end;

procedure TDownloader.httpError(const msg: string; aSocket: TLSocket);
begin
  aSocket.Disconnect(True);
  FDownloading := False;
  if Assigned(FOnDownloadError) then
  begin
    FOnDownloadError(Self, msg);
  end;
end;

procedure TDownloader.httpHead(ASocket: TLHTTPClientSocket);
begin
  FTotalLength := StrToIntDef(string(ASocket.Parameters[hpContentLength]), 0);
  if Assigned(FOnDownloadProgress) then
  begin
    FOnDownloadProgress(Self, 0);
  end;
end;

constructor TDownloader.Create;
begin
  FDownloading := False;
  FHttp := TLHTTPClientComponent.Create(nil);
  FSSL := TLSSLSessionComponent.Create(nil);
  FHttp.Session := FSSL;
  FHttp.OnInput := @httpInput;
  FHttp.OnDoneInput := @httpDoneInput;
  FHttp.OnError := @httpError;
  FHttp.OnProcessHeaders := @httpHead;
end;

destructor TDownloader.Destroy;
begin
  FDownloading := False;
  FHttp.Disconnect(True);
  inherited Destroy;
end;

procedure TDownloader.StartDownload(AUrl: string; AFileName: string);
var
  aHost: string;
  aUri: string;
  aPort: word;
begin
  if FDownloading then
  begin
    WriteLn('Downloading, cannot start a new task');
    Exit;
  end;
  FDownloading := True;
  FTotalLength := 0;
  FCompleted := 0;
  FSSL.SSLActive := DecomposeURL(AUrl, aHost, aUri, aPort);
  FHttp.Host := aHost;
  FHttp.URI := aUri;
  FHttp.Port := aPort;
  if FileExists(AFileName) then
  begin
    DeleteFile(AFileName);
  end;
  FDownloadFile := TFileStream.Create(AFileName, fmCreate or fmOpenWrite);
  FHttp.SendRequest;
end;

procedure TDownloader.StopDownload;
begin
  FHttp.Disconnect(True);
  FDownloading := False;
  FTotalLength := 0;
  FCompleted := 0;

end;

end.
