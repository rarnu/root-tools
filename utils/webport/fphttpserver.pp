{
    $Id: header,v 1.1 2000/07/13 06:33:45 michael Exp $
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2011- by the Free Pascal development team
    
    Simple HTTP server component.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit fphttpserver;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sockets, ssockets, resolve, httpdefs;

Const
  ReadBufLen = 4096;

Type
  TFPHTTPConnection = Class;
  TFPHTTPConnectionThread = Class;
  TFPCustomHttpServer = Class;
  TRequestErrorHandler = Procedure (Sender : TObject; E : Exception) of object;

  { TFPHTTPConnectionRequest }

  TFPHTTPConnectionRequest = Class(TRequest)
  private
    FConnection: TFPHTTPConnection;
  protected
    Procedure InitRequestVars; override;
  published
    Property Connection : TFPHTTPConnection Read FConnection;
  end;

  { TFPHTTPConnectionResponse }

  TFPHTTPConnectionResponse = Class(TResponse)
  private
    FConnection: TFPHTTPConnection;
  Protected
    Procedure DoSendHeaders(Headers : TStrings); override;
    Procedure DoSendContent; override;
    Property Connection : TFPHTTPConnection Read FConnection;
  end;


  { TFPHTTPConnection }

  TFPHTTPConnection = Class(TObject)
  private
    FOnError: TRequestErrorHandler;
    FServer: TFPCustomHTTPServer;
    FSocket: TSocketStream;
    FBuffer : Ansistring;
    procedure InterPretHeader(ARequest: TFPHTTPConnectionRequest; const AHeader: String);
    function ReadString: String;
    Function GetLookupHostNames : Boolean;
  Protected
    procedure ReadRequestContent(ARequest: TFPHTTPConnectionRequest); virtual;
    procedure UnknownHeader(ARequest: TFPHTTPConnectionRequest; const AHeader: String); virtual;
    procedure HandleRequestError(E : Exception); virtual;
    Procedure SetupSocket; virtual;
    Function ReadRequestHeaders : TFPHTTPConnectionRequest;
  Public
    Constructor Create(AServer : TFPCustomHTTPServer; ASocket : TSocketStream);
    Destructor Destroy; override;
    Procedure HandleRequest; virtual;
    Property Socket : TSocketStream Read FSocket;
    Property Server : TFPCustomHTTPServer Read FServer;
    Property OnRequestError : TRequestErrorHandler Read FOnError Write FOnError;
    Property LookupHostNames : Boolean Read GetLookupHostNames;
  end;

  { TFPHTTPConnectionThread }

  TFPHTTPConnectionThread = Class(TThread)
  private
    FConnection: TFPHTTPConnection;
  Public
    Constructor CreateConnection(AConnection : TFPHTTPConnection); virtual;
    Procedure Execute; override;
    Property Connection : TFPHTTPConnection Read FConnection;
  end;

  { TFPHttpServer }
  THTTPServerRequestHandler = Procedure (Sender: TObject;
      Var ARequest: TFPHTTPConnectionRequest;
      Var AResponse : TFPHTTPConnectionResponse) of object;

  { TFPCustomHttpServer }

  TFPCustomHttpServer = Class(TComponent)
  Private
    FAdminMail: string;
    FAdminName: string;
    FOnAllowConnect: TConnectQuery;
    FOnRequest: THTTPServerRequestHandler;
    FOnRequestError: TRequestErrorHandler;
    FAddress: string;
    FPort: Word;
    FQueueSize: Word;
    FServer : TInetServer;
    FLoadActivate : Boolean;
    FServerBanner: string;
    FLookupHostNames,
    FThreaded: Boolean;
    FConnectionCount : Integer;
    function GetActive: Boolean;
    procedure SetActive(const AValue: Boolean);
    procedure SetOnAllowConnect(const AValue: TConnectQuery);
    procedure SetAddress(const AValue: string);
    procedure SetPort(const AValue: Word);
    procedure SetQueueSize(const AValue: Word);
    procedure SetThreaded(const AValue: Boolean);
    procedure SetupSocket;
    procedure WaitForRequests;
  Protected
    // Override these to create descendents of the request/response instead.
    Function CreateRequest : TFPHTTPConnectionRequest; virtual;
    Function CreateResponse(ARequest : TFPHTTPConnectionRequest) : TFPHTTPConnectionResponse; virtual;
    Procedure InitRequest(ARequest : TFPHTTPConnectionRequest); virtual;
    Procedure InitResponse(AResponse : TFPHTTPConnectionResponse); virtual;
    // Called on accept errors
    procedure DoAcceptError(Sender: TObject; ASocket: Longint; E: Exception;  var ErrorAction: TAcceptErrorAction);
    // Create a connection handling object.
    function CreateConnection(Data : TSocketStream) : TFPHTTPConnection; virtual;
    // Create a connection handling thread.
    Function CreateConnectionThread(Conn : TFPHTTPConnection) : TFPHTTPConnectionThread; virtual;
    // Check if server is inactive
    Procedure CheckInactive;
    // Called by TInetServer when a new connection is accepted.
    Procedure DoConnect(Sender : TObject; Data : TSocketStream); virtual;
    // Create and configure TInetServer
    Procedure CreateServerSocket; virtual;
    // Start server socket
    procedure StartServerSocket; virtual;
    // Stop server stocket
    procedure StopServerSocket; virtual;
    // free server socket instance
    Procedure FreeServerSocket; virtual;
    // Handle request. This calls OnRequest. It can be overridden by descendants to provide standard handling.
    procedure HandleRequest(Var ARequest: TFPHTTPConnectionRequest;
                            Var AResponse : TFPHTTPConnectionResponse); virtual;
    // Called when a connection encounters an unexpected error. Will call OnRequestError when set.
    procedure HandleRequestError(Sender: TObject; E: Exception); virtual;
    // Connection count
    Property ConnectionCount : Integer Read FConnectionCount;
  public
    Constructor Create(AOwner : TComponent); override;
    Destructor Destroy; override;
  protected
    // Set to true to start listening.
    Property Active : Boolean Read GetActive Write SetActive Default false;
    // Address to listen on.
    Property Address : string Read FAddress Write SetAddress;
    // Port to listen on.
    Property Port : Word Read FPort Write SetPort Default 80;
    // Max connections on queue (for Listen call)
    Property QueueSize : Word Read FQueueSize Write SetQueueSize Default 5;
    // Called when deciding whether to accept a connection.
    Property OnAllowConnect : TConnectQuery Read FOnAllowConnect Write SetOnAllowConnect;
    // Use a thread to handle a connection ?
    property Threaded : Boolean read FThreaded Write SetThreaded;
    // Called to handle the request. If Threaded=True, it is called in a the connection thread.
    Property OnRequest : THTTPServerRequestHandler Read FOnRequest Write FOnRequest;
    // Called when an unexpected error occurs during handling of the request. Sender is the TFPHTTPConnection.
    Property OnRequestError : TRequestErrorHandler Read FOnRequestError Write FOnRequestError;
  published
    //aditional server information
    property AdminMail: string read FAdminMail write FAdminMail;
    property AdminName: string read FAdminName write FAdminName;
    property ServerBanner: string read FServerBanner write FServerBanner;
    Property LookupHostNames : Boolean Read FLookupHostNames Write FLookupHostNames;
  end;

  TFPHttpServer = Class(TFPCustomHttpServer)
  Published
    Property Active;
    Property Port;
    Property QueueSize;
    Property OnAllowConnect;
    property Threaded;
    Property OnRequest;
    Property OnRequestError;
  end;

  EHTTPServer = Class(EHTTP);

  Function GetStatusCode (ACode: Integer) : String;

implementation


resourcestring
  SErrSocketActive    =  'Operation not allowed while server is active';
  SErrReadingSocket   = 'Error reading data from the socket';
  SErrMissingProtocol = 'Missing HTTP protocol version in request';

{ TFPHTTPConnectionRequest }
Function GetStatusCode (ACode: Integer) : String;

begin
  Case ACode of
    100 :  Result:='Continue';
    101 :  Result:='Switching Protocols';
    200 :  Result:='OK';
    201 :  Result:='Created';
    202 :  Result:='Accepted';
    203 :  Result:='Non-Authoritative Information';
    204 :  Result:='No Content';
    205 :  Result:='Reset Content';
    206 :  Result:='Partial Content';
    300 :  Result:='Multiple Choices';
    301 :  Result:='Moved Permanently';
    302 :  Result:='Found';
    303 :  Result:='See Other';
    304 :  Result:='Not Modified';
    305 :  Result:='Use Proxy';
    307 :  Result:='Temporary Redirect';
    400 :  Result:='Bad Request';
    401 :  Result:='Unauthorized';
    402 :  Result:='Payment Required';
    403 :  Result:='Forbidden';
    404 :  Result:='Not Found';
    405 :  Result:='Method Not Allowed';
    406 :  Result:='Not Acceptable';
    407 :  Result:='Proxy Authentication Required';
    408 :  Result:='Request Time-out';
    409 :  Result:='Conflict';
    410 :  Result:='Gone';
    411 :  Result:='Length Required';
    412 :  Result:='Precondition Failed';
    413 :  Result:='Request Entity Too Large';
    414 :  Result:='Request-URI Too Large';
    415 :  Result:='Unsupported Media Type';
    416 :  Result:='Requested range not satisfiable';
    417 :  Result:='Expectation Failed';
    500 :  Result:='Internal Server Error';
    501 :  Result:='Not Implemented';
    502 :  Result:='Bad Gateway';
    503 :  Result:='Service Unavailable';
    504 :  Result:='Gateway Time-out';
    505 :  Result:='HTTP Version not supported';
  else
    Result:='Unknown status';
  end;
end;

Function GetHostNameByAddress(const AnAddress: String): String;
var
  Resolver: THostResolver;
begin
  Result := '';
  if AnAddress = '' then exit;
  Resolver := THostResolver.Create(nil);
  try
    if Resolver.AddressLookup(AnAddress) then
      Result := Resolver.ResolvedName
  finally
    FreeAndNil(Resolver);
  end;
end;

procedure TFPHTTPConnectionRequest.InitRequestVars;
Var
  P : Integer;
  S : String;
begin
  S:=URL;
  P:=Pos('?',S);
  if (P<>0) then
    SetHTTPVariable(hvQuery,Copy(S,P+1,Length(S)-P));
  if Assigned(FConnection) and FConnection.LookupHostNames then
    SetHTTPVariable(hvRemoteHost,GetHostNameByAddress(RemoteAddress));
  inherited InitRequestVars;
end;

Function SocketAddrToString(ASocketAddr: TSockAddr): String;
begin
  if ASocketAddr.sa_family = AF_INET then
    Result := NetAddrToStr(ASocketAddr.sin_addr)
  else // no ipv6 support yet
    Result := '';
end;



procedure TFPHTTPConnectionResponse.DoSendHeaders(Headers: TStrings);

Var
  S : String;
  I : Integer;
begin
  S:=Format('HTTP/1.1 %3d %s'#13#10,[Code,GetStatusCode(Code)]);
  For I:=0 to Headers.Count-1 do
    S:=S+Headers[i]+#13#10;
  // Last line in headers is empty.
  Connection.Socket.WriteBuffer(S[1],Length(S));
end;

procedure TFPHTTPConnectionResponse.DoSendContent;
begin
  If Assigned(ContentStream) then
    Connection.Socket.CopyFrom(ContentStream,0)
  else
    Contents.SaveToStream(Connection.Socket);
end;

{ TFPHTTPConnection }

function TFPHTTPConnection.ReadString : String;

  Procedure FillBuffer;

  Var
    R : Integer;

  begin
    SetLength(FBuffer,ReadBufLen);
    r:=FSocket.Read(FBuffer[1],ReadBufLen);
    If r<0 then
      Raise EHTTPServer.Create(SErrReadingSocket);
    if (r<ReadBuflen) then
      SetLength(FBuffer,r);
  end;

Var
  CheckLF,Done : Boolean;
  P,L : integer;

begin
  Result:='';
  Done:=False;
  CheckLF:=False;
  Repeat
    if Length(FBuffer)=0 then
      FillBuffer;
    if Length(FBuffer)=0 then
      Done:=True
    else if CheckLF then
      begin
      If (FBuffer[1]<>#10) then
        Result:=Result+#13
      else
        begin
        Delete(FBuffer,1,1);
        Done:=True;
        end;
      CheckLF:=False;  
      end;
    if not Done then
      begin
      P:=Pos(#13#10,FBuffer);
      If P=0 then
        begin
        L:=Length(FBuffer);
        CheckLF:=FBuffer[L]=#13;
        if CheckLF then
          Result:=Result+Copy(FBuffer,1,L-1)
        else
          Result:=Result+FBuffer;
        FBuffer:='';
        end
      else
        begin
        Result:=Result+Copy(FBuffer,1,P-1);
        Delete(FBuffer,1,P+1);
        Done:=True;
        end;
      end;
  until Done;
end;

procedure TFPHTTPConnection.UnknownHeader(ARequest: TFPHTTPConnectionRequest;
  const AHeader: String);
begin
  // Do nothing
end;

procedure TFPHTTPConnection.HandleRequestError(E: Exception);
begin
  If Assigned(FOnError) then
    try
      FOnError(Self,E);
    except
      // We really cannot handle this...
    end;
end;

procedure TFPHTTPConnection.SetupSocket;
begin
{$if defined(FreeBSD) or defined(Linux)}
  FSocket.ReadFlags:=MSG_NOSIGNAL;
  FSocket.WriteFlags:=MSG_NOSIGNAL;
{$endif}
end;

Procedure TFPHTTPConnection.InterPretHeader(ARequest : TFPHTTPConnectionRequest; Const AHeader : String);

Var
  P : Integer;
  N,V : String;

begin
  V:=AHeader;
  P:=Pos(':',V);
  if (P=0) then
    begin
    UnknownHeader(ARequest,Aheader);
    Exit;
    end;
  N:=Copy(V,1,P-1);
  Delete(V,1,P+1);
  V:=Trim(V);
  ARequest.SetFieldByName(N,V);
end;

procedure ParseStartLine(Request : TFPHTTPConnectionRequest; AStartLine : String);

  Function GetNextWord(Var S : String) : string;

  Var
    P : Integer;

  begin
    P:=Pos(' ',S);
    If (P=0) then
      P:=Length(S)+1;
    Result:=Copy(S,1,P-1);
    Delete(S,1,P);
  end;

Var
  S : String;
  I : Integer;
  
begin
  Request.Method:=GetNextWord(AStartLine);
  Request.URL:=GetNextWord(AStartLine);
  S:=Request.URL;
  I:=Pos('?',S);
  if (I>0) then
    S:=Copy(S,1,I-1);
  If (Length(S)>1) and (S[1]<>'/') then
    S:='/'+S
  else if S='/' then 
    S:='';
  Request.PathInfo:=S;
  S:=GetNextWord(AStartLine);
  If (Pos('HTTP/',S)<>1) then
    Raise EHTTPServer.CreateHelp(SErrMissingProtocol,400);
  Delete(S,1,5);
  Request.ProtocolVersion:=trim(S);
end;

Procedure TFPHTTPConnection.ReadRequestContent(ARequest : TFPHTTPConnectionRequest);

Var
  P,L,R : integer;
  S : String;

begin
  L:=ARequest.ContentLength;
  If (L>0) then
    begin
    SetLength(S,L);
    P:=Length(FBuffer);
    if (P>0) then
      begin
      Move(FBuffer[1],S[1],P);
      L:=L-P;
      end;
    P:=P+1;
    R:=1;
    While (L<>0) and (R>0) do
      begin
      R:=FSocket.Read(S[p],L);
      If R<0 then
        Raise EHTTPServer.Create(SErrReadingSocket);
      if (R>0) then
        begin
        P:=P+R;
        L:=L-R;
        end;
      end;  
    end;
  ARequest.InitContent(S);
end;

function TFPHTTPConnection.ReadRequestHeaders: TFPHTTPConnectionRequest;

Var
  StartLine,S : String;
begin
  Result:=Server.CreateRequest;
  try
    Server.InitRequest(Result);
    Result.FConnection:=Self;
    StartLine:=ReadString;
    ParseStartLine(Result,StartLine);
    Repeat
      S:=ReadString;
      if (S<>'') then
        InterPretHeader(Result,S);
    Until (S='');
    Result.RemoteAddress := SocketAddrToString(FSocket.RemoteAddress);
    Result.ServerPort := FServer.Port;
  except
    FreeAndNil(Result);
    Raise;
  end;
end;

constructor TFPHTTPConnection.Create(AServer: TFPCustomHttpServer; ASocket: TSocketStream);
begin
  FSocket:=ASocket;
  FServer:=AServer;
  If Assigned(FServer) then
    InterLockedIncrement(FServer.FConnectionCount)
end;

destructor TFPHTTPConnection.Destroy;
begin
  If Assigned(FServer) then
    InterLockedDecrement(FServer.FConnectionCount);
  FreeAndNil(FSocket);
  Inherited;
end;

Function TFPHTTPConnection.GetLookupHostNames : Boolean;

begin
  if Assigned(FServer) then
    Result:=FServer.LookupHostNames
  else
    Result:=False;  
end;

procedure TFPHTTPConnection.HandleRequest;

Var
  Req : TFPHTTPConnectionRequest;
  Resp : TFPHTTPConnectionResponse;

begin
  Try
    SetupSocket;
    // Read headers.
    Req:=ReadRequestHeaders;
    try
      //set port
      Req.ServerPort := Server.Port;
      // Read content, if any
      If Req.ContentLength>0 then
        ReadRequestContent(Req);
      Req.InitRequestVars;
      // Create Response
      Resp:= Server.CreateResponse(Req);
      try
        Server.InitResponse(Resp);
        Resp.FConnection:=Self;
        // And dispatch
        if Server.Active then
          Server.HandleRequest(Req,Resp);
        if Assigned(Resp) and (not Resp.ContentSent) then
          Resp.SendContent;
      finally
        FreeAndNil(Resp);
      end;
    Finally
      FreeAndNil(Req);
    end;
  Except
    On E : Exception do
      HandleRequestError(E);
  end;
end;

{ TFPHTTPConnectionThread }

constructor TFPHTTPConnectionThread.CreateConnection(AConnection: TFPHTTPConnection
  );
begin
  FConnection:=AConnection;
  FreeOnTerminate:=True;
  Inherited Create(False);
end;

procedure TFPHTTPConnectionThread.Execute;
begin
  try
    try
      FConnection.HandleRequest;
    finally
      FreeAndNil(FConnection);
    end;
  except
    // Silently ignore errors.
  end;
end;

{ TFPCustomHttpServer }

procedure TFPCustomHttpServer.HandleRequestError(Sender: TObject; E: Exception);
begin
  If Assigned(FOnRequestError) then
    try
      FOnRequestError(Sender,E);
    except
      // Do not let errors in user code escape.
    end
end;

procedure TFPCustomHttpServer.DoAcceptError(Sender: TObject; ASocket: Longint;
  E: Exception; var ErrorAction: TAcceptErrorAction);
begin
  If Not Active then
    ErrorAction:=AEAStop
  else
    ErrorAction:=AEARaise
end;

function TFPCustomHttpServer.GetActive: Boolean;
begin
  if (csDesigning in ComponentState) then
    Result:=FLoadActivate
  else
    Result:=Assigned(FServer);
end;

procedure TFPCustomHttpServer.StopServerSocket;
begin
  FServer.StopAccepting(True);
end;

procedure TFPCustomHttpServer.SetActive(const AValue: Boolean);
begin
  If AValue=GetActive then exit;
  FLoadActivate:=AValue;
  if not (csDesigning in Componentstate) then
    if AValue then
      begin
      CreateServerSocket;
      SetupSocket;
      StartServerSocket;
      FreeServerSocket;
      end
    else
      StopServerSocket;
end;

procedure TFPCustomHttpServer.SetOnAllowConnect(const AValue: TConnectQuery);
begin
  if FOnAllowConnect=AValue then exit;
  CheckInactive;
  FOnAllowConnect:=AValue;
end;

procedure TFPCustomHttpServer.SetAddress(const AValue: string);
begin
  if FAddress=AValue then exit;
  CheckInactive;
  FAddress:=AValue;
end;

procedure TFPCustomHttpServer.SetPort(const AValue: Word);
begin
  if FPort=AValue then exit;
  CheckInactive;
  FPort:=AValue;
end;

procedure TFPCustomHttpServer.SetQueueSize(const AValue: Word);
begin
  if FQueueSize=AValue then exit;
  CheckInactive;
  FQueueSize:=AValue;
end;

procedure TFPCustomHttpServer.SetThreaded(const AValue: Boolean);
begin
  if FThreaded=AValue then exit;
  CheckInactive;
  FThreaded:=AValue;
end;

function TFPCustomHttpServer.CreateRequest: TFPHTTPConnectionRequest;
begin
  Result:=TFPHTTPConnectionRequest.Create;
end;

function TFPCustomHttpServer.CreateResponse(ARequest : TFPHTTPConnectionRequest): TFPHTTPConnectionResponse;
begin
  Result:=TFPHTTPConnectionResponse.Create(ARequest);
end;

procedure TFPCustomHttpServer.InitRequest(ARequest: TFPHTTPConnectionRequest);
begin

end;

procedure TFPCustomHttpServer.InitResponse(AResponse: TFPHTTPConnectionResponse
  );
begin

end;

function TFPCustomHttpServer.CreateConnection(Data: TSocketStream): TFPHTTPConnection;
begin
  Result:=TFPHTTPConnection.Create(Self,Data);
end;

function TFPCustomHttpServer.CreateConnectionThread(Conn: TFPHTTPConnection
  ): TFPHTTPConnectionThread;
begin
   Result:=TFPHTTPConnectionThread.CreateConnection(Conn);
end;

procedure TFPCustomHttpServer.CheckInactive;
begin
  If GetActive then
    Raise EHTTPServer.Create(SErrSocketActive);
end;

procedure TFPCustomHttpServer.DoConnect(Sender: TObject; Data: TSocketStream);

Var
  Con : TFPHTTPConnection;

begin
  Con:=CreateConnection(Data);
  try
    Con.FServer:=Self;
    Con.OnRequestError:=@HandleRequestError;
    if Threaded then
      CreateConnectionThread(Con)
    else
      begin
      Con.HandleRequest;
      end;
  finally
    if not Threaded then
      Con.Free;
  end;
end;

procedure TFPCustomHttpServer.SetupSocket;

begin
  FServer.QueueSize:=Self.QueueSize;
  FServer.ReuseAddress:=true;
end;

procedure TFPCustomHttpServer.CreateServerSocket;
begin
  if FAddress='' then
    FServer:=TInetServer.Create(FPort)
  else
    FServer:=TInetServer.Create(FAddress,FPort);
  FServer.MaxConnections:=-1;
  FServer.OnConnectQuery:=OnAllowConnect;
  FServer.OnConnect:=@DOConnect;
  FServer.OnAcceptError:=@DoAcceptError;
end;

procedure TFPCustomHttpServer.StartServerSocket;
begin
  FServer.Bind;
  FServer.Listen;
  FServer.StartAccepting;
end;

procedure TFPCustomHttpServer.FreeServerSocket;
begin
  FreeAndNil(FServer);
end;

procedure TFPCustomHttpServer.HandleRequest(var ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse);
begin
  If Assigned(FOnRequest) then
    FonRequest(Self,ARequest,AResponse);
end;

constructor TFPCustomHttpServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPort:=80;
  FQueueSize:=5;
  FServerBanner := 'Freepascal';
end;

Procedure TFPCustomHttpServer.WaitForRequests;

Var
  FLastCount,ACount : Integer;

begin
  ACount:=0;
  FLastCount:=FConnectionCount;
  While (FConnectionCount>0) and (ACount<10) do
    begin
    Sleep(100);
    if (FConnectionCount=FLastCount) then
      Dec(ACount)
    else
      FLastCount:=FConnectionCount;
    end;
end;

destructor TFPCustomHttpServer.Destroy;
begin
  Active:=False;
  if Threaded and (FConnectionCount>0) then
    WaitForRequests;
  inherited Destroy;
end;

end.

