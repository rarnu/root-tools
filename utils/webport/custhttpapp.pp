{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2009 by the Free Pascal development team

    THTTPApplication class.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{ $define CGIDEBUG}
{$mode objfpc}
{$H+}

unit custhttpapp;

Interface

uses
  Classes, SysUtils, httpdefs, custweb, ssockets,  fphttpserver;

Type
  TCustomHTTPApplication = Class;
  TFPHTTPServerHandler = Class;

  { TEmbeddedHttpServer }

  TEmbeddedHttpServer = Class(TFPCustomHttpServer)
  Private
    FWebHandler: TFPHTTPServerHandler;
  protected
    Procedure InitRequest(ARequest : TFPHTTPConnectionRequest); override;
    Procedure InitResponse(AResponse : TFPHTTPConnectionResponse); override;
    Property WebHandler : TFPHTTPServerHandler Read FWebHandler;
    Property Active;
  end;

  { TFCgiHandler }

  { TFPHTTPServerHandler }

  TFPHTTPServerHandler = class(TWebHandler)
  Private
    FOnRequestError: TRequestErrorHandler;
    FServer: TEmbeddedHTTPServer;
    function GetAllowConnect: TConnectQuery;
    function GetAddress: string;
    function GetPort: Word;
    function GetQueueSize: Word;
    function GetThreaded: Boolean;
    procedure SetOnAllowConnect(const AValue: TConnectQuery);
    procedure SetAddress(const AValue: string);
    procedure SetPort(const AValue: Word);
    procedure SetQueueSize(const AValue: Word);
    procedure SetThreaded(const AValue: Boolean);
    function GetLookupHostNames : Boolean;
    Procedure SetLookupHostnames(Avalue : Boolean);
  protected
    procedure HTTPHandleRequest(Sender: TObject; var ARequest: TFPHTTPConnectionRequest; var AResponse: TFPHTTPConnectionResponse); virtual;
    procedure HandleRequestError(Sender: TObject; E: Exception); virtual;
    Procedure InitRequest(ARequest : TRequest); override;
    Procedure InitResponse(AResponse : TResponse); override;
    function WaitForRequest(out ARequest : TRequest; out AResponse : TResponse) : boolean; override;
    Function CreateServer : TEmbeddedHttpServer; virtual;
    Property HTTPServer : TEmbeddedHttpServer Read FServer;
  Public
    Procedure Run; override;
    Procedure Terminate; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // Address to listen on.
    Property Address : string Read GetAddress Write SetAddress;
    // Port to listen on.
    Property Port : Word Read GetPort Write SetPort Default 80;
    // Max connections on queue (for Listen call)
    Property QueueSize : Word Read GetQueueSize Write SetQueueSize Default 5;
    // Called when deciding whether to accept a connection.
    Property OnAllowConnect : TConnectQuery Read GetAllowConnect Write SetOnAllowConnect;
    // Use a thread to handle a connection ?
    property Threaded : Boolean read GetThreaded Write SetThreaded;
    // Handle On Request error. If not set, error is logged.
    Property OnRequestError : TRequestErrorHandler Read FOnRequestError Write FOnRequestError;
    // Should addresses be matched to hostnames ? (expensive)
    Property LookupHostNames : Boolean Read GetLookupHostNames Write SetLookupHostNames;
  end;

  { TCustomHTTPApplication }

  TCustomHTTPApplication = Class(TCustomWebApplication)
  private
    function GetLookupHostNames : Boolean;
    Procedure SetLookupHostnames(Avalue : Boolean);
    function GetAllowConnect: TConnectQuery;
    function GetAddress: String;
    function GetPort: Word;
    function GetQueueSize: Word;
    function GetThreaded: Boolean;
    procedure SetOnAllowConnect(const AValue: TConnectQuery);
    procedure SetAddress(const AValue: string);
    procedure SetPort(const AValue: Word);
    procedure SetQueueSize(const AValue: Word);
    procedure SetThreaded(const AValue: Boolean);
  protected
    function InitializeWebHandler: TWebHandler; override;
    Function HTTPHandler : TFPHTTPServerHandler;
  Public
    Property Address : string Read GetAddress Write SetAddress;
    Property Port : Word Read GetPort Write SetPort Default 80;
    // Max connections on queue (for Listen call)
    Property QueueSize : Word Read GetQueueSize Write SetQueueSize Default 5;
    // Called when deciding whether to accept a connection.
    Property OnAllowConnect : TConnectQuery Read GetAllowConnect Write SetOnAllowConnect;
    // Use a thread to handle a connection ?
    property Threaded : Boolean read GetThreaded Write SetThreaded;
    // Should addresses be matched to hostnames ? (expensive)
    Property LookupHostNames : Boolean Read GetLookupHostNames Write SetLookupHostNames;
  end;


Implementation

{ TEmbeddedHttpServer }

procedure TEmbeddedHttpServer.InitRequest(ARequest: TFPHTTPConnectionRequest);
begin
  WebHandler.InitRequest(ARequest);
end;

procedure TEmbeddedHttpServer.InitResponse(AResponse: TFPHTTPConnectionResponse
  );
begin
  WebHandler.InitResponse(AResponse);
end;

{$ifdef CGIDEBUG}
uses
  dbugintf;
{$endif}

{ TCustomHTTPApplication }

function TCustomHTTPApplication.GetLookupHostNames : Boolean;

begin
  Result:=HTTPHandler.LookupHostNames;
end;

Procedure TCustomHTTPApplication.SetLookupHostnames(Avalue : Boolean);

begin
  HTTPHandler.LookupHostNames:=AValue;
end;

function TCustomHTTPApplication.GetAllowConnect: TConnectQuery;
begin
  Result:=HTTPHandler.OnAllowConnect;
end;

function TCustomHTTPApplication.GetAddress: String;
begin
  Result:=HTTPHandler.Address;
end;

function TCustomHTTPApplication.GetPort: Word;
begin
  Result:=HTTPHandler.Port;
end;

function TCustomHTTPApplication.GetQueueSize: Word;
begin
  Result:=HTTPHandler.QueueSize;
end;

function TCustomHTTPApplication.GetThreaded: Boolean;
begin
  Result:=HTTPHandler.Threaded;
end;

procedure TCustomHTTPApplication.SetOnAllowConnect(const AValue: TConnectQuery);
begin
  HTTPHandler.OnAllowConnect:=AValue;
end;

procedure TCustomHTTPApplication.SetAddress(const AValue: string);
begin
  HTTPHandler.Address:=Avalue;
end;

procedure TCustomHTTPApplication.SetPort(const AValue: Word);
begin
  HTTPHandler.Port:=Avalue;
end;

procedure TCustomHTTPApplication.SetQueueSize(const AValue: Word);
begin
  HTTPHandler.QueueSize:=Avalue;
end;

procedure TCustomHTTPApplication.SetThreaded(const AValue: Boolean);
begin
  HTTPHandler.Threaded:=Avalue;
end;

function TCustomHTTPApplication.InitializeWebHandler: TWebHandler;
begin
  Result:=TFPHTTPServerHandler.Create(Self);
end;

function TCustomHTTPApplication.HTTPHandler: TFPHTTPServerHandler;
begin
  Result:=Webhandler as TFPHTTPServerHandler;
end;

{ TFPHTTPServerHandler }

procedure TFPHTTPServerHandler.HandleRequestError(Sender: TObject; E: Exception
  );
begin
  Try
    If Assigned(FOnRequestError) then
      FOnRequestError(Sender,E)
    else
      Log(etError,Format('Error (%s) handling request : %s',[E.ClassName,E.Message]));
  except
    // Do not let errors escape
  end;
end;

procedure TFPHTTPServerHandler.HTTPHandleRequest(Sender: TObject;
  var ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse);
begin
  // Exceptions are handled by (Do)HandleRequest. It also frees the response/request
  try
    DoHandleRequest(ARequest,AResponse);
  finally  
    ARequest:=Nil;
    AResponse:=Nil;
  end;    
  if Assigned(OnIdle) then
    OnIdle(Self);
end;

function TFPHTTPServerHandler.GetLookupHostNames : Boolean;

begin
  Result:=FServer.LookupHostNames;
end;

Procedure TFPHTTPServerHandler.SetLookupHostnames(Avalue : Boolean);

begin
  FServer.LookupHostNames:=AValue;
end;

function TFPHTTPServerHandler.GetAllowConnect: TConnectQuery;
begin
  Result:=FServer.OnAllowConnect;
end;

function TFPHTTPServerHandler.GetAddress: string;
begin
  Result:=FServer.Address;
end;

function TFPHTTPServerHandler.GetPort: Word;
begin
  Result:=FServer.Port;
end;

function TFPHTTPServerHandler.GetQueueSize: Word;
begin
  Result:=FServer.QueueSize;
end;

function TFPHTTPServerHandler.GetThreaded: Boolean;
begin
  Result:=FServer.Threaded;
end;

procedure TFPHTTPServerHandler.SetOnAllowConnect(const AValue: TConnectQuery);
begin
  FServer.OnAllowConnect:=Avalue
end;

procedure TFPHTTPServerHandler.SetAddress(const AValue: string);
begin
  FServer.Address:=AValue
end;

procedure TFPHTTPServerHandler.SetPort(const AValue: Word);
begin
  FServer.Port:=Avalue
end;

procedure TFPHTTPServerHandler.SetQueueSize(const AValue: Word);
begin
  FServer.QueueSize:=Avalue
end;

procedure TFPHTTPServerHandler.SetThreaded(const AValue: Boolean);
begin
  FServer.Threaded:=AValue;
end;

procedure TFPHTTPServerHandler.InitRequest(ARequest: TRequest);
begin
  inherited InitRequest(ARequest);
end;

procedure TFPHTTPServerHandler.InitResponse(AResponse: TResponse);
begin
  inherited InitResponse(AResponse);
end;

function TFPHTTPServerHandler.WaitForRequest(out ARequest: TRequest;
  out AResponse: TResponse): boolean;
begin
  Result:=False;
  ARequest:=Nil;
  AResponse:=Nil;
end;

function TFPHTTPServerHandler.CreateServer: TEmbeddedHttpServer;
begin
  Result:=TEmbeddedHttpServer.Create(Self);
end;

procedure TFPHTTPServerHandler.Run;
begin
  Fserver.Active:=True;
end;

procedure TFPHTTPServerHandler.Terminate;
begin
  Inherited;
  if Assigned(FServer) then
    Fserver.Active:=False;
end;

constructor TFPHTTPServerHandler.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FServer:=CreateServer;
  FServer.FWebHandler:=Self;
  FServer.OnRequest:=@HTTPHandleRequest;
  Fserver.OnRequestError:=@HandleRequestError;
end;

destructor TFPHTTPServerHandler.Destroy;
begin
  if Assigned(FServer) then
    begin
    FServer.Active:=False;
    FreeAndNil(FServer);
    end;
  inherited Destroy;

end;


end.
