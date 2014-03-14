unit basenetwork;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, lNet, lnetbase, lNetComponents, lhttp, lHTTPUtil, intf_notify;

type

  { TNetworkBase }

  TNetworkBase = class
  private
    FHTTPBuffer: string;
    FHttp: TLHTTPClientComponent;
    FSSL: TLSSLSessionComponent;
  protected
    FNotifyId: integer;
    FNotify: INotifyable;
    FHttpResult: string;
    procedure httpDoneInput(ASocket: TLHTTPClientSocket);
    function httpInput(ASocket: TLHTTPClientSocket; ABuffer: PChar;
      ASize: integer): integer;
    procedure httpError(const msg: string; aSocket: TLSocket);
    function MakeNotifyMap: TStringList; virtual; abstract;
  public
    constructor Create(ANotifyId: integer; ANotify: INotifyable); virtual;
    destructor Destroy; override;
    procedure Start(AUrl: string);
  end;

implementation

{ TNetworkBase }

procedure TNetworkBase.httpDoneInput(ASocket: TLHTTPClientSocket);
begin
  ASocket.Disconnect(True);
  FNotify.ThreadNotify(FNotifyId, MakeNotifyMap);
end;

function TNetworkBase.httpInput(ASocket: TLHTTPClientSocket; ABuffer: PChar;
  ASize: integer): integer;
var
  oldLength: dword;
begin
  oldLength := Length(FHTTPBuffer);
  setlength(FHTTPBuffer, oldLength + ASize);
  move(ABuffer^, FHTTPBuffer[oldLength + 1], ASize);
  FHttpResult := FHTTPBuffer;
  Result := aSize;

end;

procedure TNetworkBase.httpError(const msg: string; aSocket: TLSocket);
begin
  aSocket.Disconnect(True);
end;

constructor TNetworkBase.Create(ANotifyId: integer; ANotify: INotifyable);
begin
  FNotifyId := ANotifyId;
  FNotify := ANotify;
  FHttp := TLHTTPClientComponent.Create(nil);
  FSSL := TLSSLSessionComponent.Create(nil);
  FHttp.Session := FSSL;
  FHttp.OnInput := @httpInput;
  FHttp.OnDoneInput := @httpDoneInput;
  FHttp.OnError := @httpError;
end;

destructor TNetworkBase.Destroy;
begin
  FHttp.Disconnect(True);
  FreeAndNil(FSSL);
  FreeAndNil(FHttp);
  inherited Destroy;
end;

procedure TNetworkBase.Start(AUrl: string);
var
  aHost: string;
  aURI: string;
  aPort: word;
begin
  FHTTPBuffer := '';
  FSSL.SSLActive := DecomposeURL(AUrl, aHost, aURI, aPort);
  FHttp.Host := aHost;
  FHttp.URI := aURI;
  FHttp.Port := aPort;
  FHttp.SendRequest;
end;

end.

