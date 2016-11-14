{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1999-2000 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$MODE objfpc}{$H+}

unit ssockets;


interface

uses
 SysUtils, Classes, ctypes, sockets;

type

  TSocketErrorType = (
    seHostNotFound,
    seCreationFailed,
    seBindFailed,
    seListenFailed,
    seConnectFailed,
    seAcceptFailed,
    seAcceptWouldBlock);

  TSocketOption = (soDebug,soReuseAddr,soKeepAlive,soDontRoute,soBroadcast,
                   soOOBinline);
  TSocketOptions = Set of TSocketOption;

  ESocketError = class(Exception)
    Code: TSocketErrorType;
    constructor Create(ACode: TSocketErrorType; const MsgArgs: array of const);overload;
  end;

  TAcceptErrorAction = (aeaRaise,aeaIgnore,aeaStop);
  TSocketStream = Class;

  // Handles all OS calls

  { TSocketHandler }

  TSocketHandler = Class(TObject)
    FSocket: TSocketStream;
    FLastError : integer;
  Protected
    Procedure SetSocket(const AStream: TSocketStream); virtual;
    Procedure CheckSocket;
  Public
    constructor Create; virtual;
    // Called after the connect call succeded. Returns True to continue, false to close connection.
    function Connect: boolean; virtual;
    // Called after the accept call succeded.
    function Accept : Boolean; virtual;

    Function Close : Boolean; virtual;
    function Shutdown(BiDirectional : Boolean): boolean; virtual;
    function Recv(Const Buffer; Count: Integer): Integer; virtual;
    function Send(Const Buffer; Count: Integer): Integer; virtual;
    function BytesAvailable: Integer; virtual;
    Property Socket : TSocketStream Read FSocket;
    Property LastError : Integer Read FLastError;
  end;
  TSocketHandlerClass = Class of TSocketHandler;

  { TSocketStream }

  TSocketStream = class(THandleStream)
  Private
    FReadFlags: Integer;
    FSocketInitialized : Boolean;
    FSocketOptions : TSocketOptions;
    FWriteFlags: Integer;
    FHandler : TSocketHandler;
    function GetLastError: Integer;
    Procedure GetSockOptions;
    Procedure SetSocketOptions(Value : TSocketOptions);
    function GetLocalAddress: TSockAddr;
    function GetRemoteAddress: TSockAddr;
  Public
    Constructor Create (AHandle : Longint; AHandler : TSocketHandler = Nil);virtual;
    destructor Destroy; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    Function Read (Var Buffer; Count : Longint) : longint; Override;
    Function Write (Const Buffer; Count : Longint) :Longint; Override;
    Property SocketOptions : TSocketOptions Read FSocketOptions
                                            Write SetSocketOptions;
    property LocalAddress: TSockAddr read GetLocalAddress;
    property RemoteAddress: TSockAddr read GetRemoteAddress;
    Property LastError : Integer Read GetLastError;
    Property ReadFlags : Integer Read FReadFlags Write FReadFlags;
    Property WriteFlags : Integer Read FWriteFlags Write FWriteFlags;
  end;

  TConnectEvent = Procedure (Sender : TObject; Data : TSocketStream) Of Object;
  TConnectQuery = Procedure (Sender : TObject; ASocket : Longint; Var Allow : Boolean) of Object;
  TOnAcceptError = Procedure (Sender : TObject; ASocket : Longint; E : Exception; Var ErrorAction : TAcceptErrorAction) of Object;

  { TSocketServer }

  TSocketServer = Class(TObject)
  Private
    FOnAcceptError: TOnAcceptError;
    FOnIdle : TNotifyEvent;
    FNonBlocking : Boolean;
    FSocket : longint;
    FListened : Boolean;
    FAccepting : Boolean;
    FMaxConnections : Longint;
    FQueueSize : Longint;
    FOnConnect : TConnectEvent;
    FOnConnectQuery : TConnectQuery;
    FHandler : TSocketHandler;
    Procedure DoOnIdle;
    Function GetReuseAddress: Boolean;
    Function GetKeepAlive : Boolean;
    Function GetLinger : Integer;
    Procedure SetReuseAddress (AValue : Boolean);
    Procedure SetKeepAlive (AValue : Boolean);
    Procedure SetLinger(ALinger : Integer);
  Protected
    FSockType : Longint;
    FBound : Boolean;
    Procedure DoConnect(ASocket : TSocketStream); Virtual;
    Function  DoConnectQuery(ASocket : longint): Boolean ;Virtual;
    Procedure Bind; Virtual; Abstract;
    Function  Accept: Longint;Virtual;Abstract;
    Function  SockToStream (ASocket : Longint) : TSocketStream;Virtual;Abstract;
    Procedure Close; Virtual;
    Procedure Abort;
    function GetConnection: TSocketStream; virtual; abstract;
    Function HandleAcceptError(E : ESocketError) : TAcceptErrorAction;
    Property Handler : TSocketHandler Read FHandler;
  Public
    Constructor Create(ASocket : Longint; AHandler : TSocketHandler);
    Destructor Destroy; Override;
    Procedure Listen;
    function  GetSockopt(ALevel,AOptName : cint; var optval; Var optlen : tsocklen): Boolean;
    function  SetSockopt(ALevel,AOptName : cint; var optval; optlen : tsocklen): Boolean;
    Procedure StartAccepting;
    Procedure StopAccepting(DoAbort : Boolean = False);
    Procedure SetNonBlocking;
    Property Bound : Boolean Read FBound;
    // Maximium number of connections in total. *Not* the simultaneous connection count. -1 keeps accepting.
    Property MaxConnections : longint Read FMaxConnections Write FMaxConnections;
    Property QueueSize : Longint Read FQueueSize Write FQueueSize default 5;
    Property OnConnect : TConnectEvent Read FOnConnect Write FOnConnect;
    Property OnConnectQuery : TConnectQuery Read FOnConnectQuery Write FOnConnectQuery;
    Property OnAcceptError : TOnAcceptError Read FOnAcceptError Write FOnAcceptError;
    Property OnIdle : TNotifyEvent Read FOnIdle Write FOnIdle;
    Property NonBlocking : Boolean Read FNonBlocking;
    Property Socket : Longint Read FSocket;
    Property SockType : Longint Read FSockType;
    Property KeepAlive : Boolean Read GetKeepAlive Write SetKeepAlive;
    Property ReuseAddress : Boolean Read GetReuseAddress Write SetReuseAddress;
    // -1 means no linger. Any value >=0 sets linger on.
    Property Linger: Integer Read GetLinger Write Setlinger;
  end;

  { TInetServer }

  TInetServer = Class(TSocketServer)
  private
  Protected
    FAddr : TINetSockAddr;
    FPort : Word;
    FHost: string;
    Function GetConnection: TSocketStream; override;
    Function SockToStream (ASocket : Longint) : TSocketStream;Override;
    Function Accept : Longint;override;
  Public
    Procedure Bind; Override;
    Constructor Create(APort: Word);
    Constructor Create(const aHost: string; const APort: Word; AHAndler : TSocketHandler = Nil);
    Property Port : Word Read FPort;
    Property Host : string Read FHost;
  end;

{$ifdef Unix}

  { TUnixServer }

  TUnixServer = Class(TSocketServer)
  Private
    FUnixAddr : TUnixSockAddr;
    FFileName : String;
  Protected
    Procedure Bind; Override;
    Function Accept : Longint;override;
    function GetConnection: TSocketStream; override;
    Function SockToStream (ASocket : Longint) : TSocketStream;Override;
    Procedure Close; override;
  Public
    Constructor Create(AFileName : String; AHandler : TSocketHandler = Nil);
    Property FileName : String Read FFileName;
  end;
{$endif}

  { TInetSocket }

  TInetSocket = Class(TSocketStream)
  Private
    FHost : String;
    FPort : Word;
  Protected
  Public
    Constructor Create(const AHost: String; APort: Word; AHandler : TSocketHandler = Nil); Overload;
    Procedure Connect; Virtual;
    Property Host : String Read FHost;
    Property Port : Word Read FPort;
  end;

{$ifdef Unix}

  TUnixSocket = Class(TSocketStream)
  Private
    FFileName : String;
  Protected
    Procedure DoConnect(ASocket : longint); Virtual;
  Public
    Constructor Create(ASocket : Longint); Overload;
    Constructor Create(AFileName : String); Overload;
    Property FileName : String Read FFileName;
  end;
{$endif}

Implementation

uses
{$ifdef unix}
  BaseUnix, Unix,
{$endif}
  resolve;

Const
  SocketWouldBlock = -2;

{ ---------------------------------------------------------------------
  ESocketError
  ---------------------------------------------------------------------}

resourcestring
  strHostNotFound = 'Host name resolution for "%s" failed.';
  strSocketCreationFailed = 'Creation of socket failed: %s';
  strSocketBindFailed = 'Binding of socket failed: %s';
  strSocketListenFailed = 'Listening on port #%d failed, error: %d';
  strSocketConnectFailed = 'Connect to %s failed.';
  strSocketAcceptFailed = 'Could not accept a client connection on socket: %d, error %d';
  strSocketAcceptWouldBlock = 'Accept would block on socket: %d';
  strErrNoStream = 'Socket stream not assigned';
{ TSocketHandler }

Procedure TSocketHandler.SetSocket(const AStream: TSocketStream);
begin
  FSocket:=AStream;
end;

Procedure TSocketHandler.CheckSocket;
begin
  If not Assigned(FSocket) then
    Raise ESocketError.Create(StrErrNoStream);
end;

constructor TSocketHandler.Create;
begin
  FSocket:=Nil;
end;

function TSocketHandler.Connect: boolean;

begin
  // Only descendents can change this
  Result:=True;
end;

function TSocketHandler.Accept : Boolean;


begin
  // Only descendents can change this
  Result:=True;
end;

function TSocketHandler.Shutdown(BiDirectional: Boolean): boolean;
begin
  CheckSocket
end;

function TSocketHandler.Recv(Const Buffer; Count: Integer): Integer;

Var
  Flags : longint;
begin
  Flags:=Socket.FReadFlags;
{$ifdef unix}
  FLastError:=ESysEINTR;
  While (FlastError=ESysEINTR) do
{$endif}
    begin
    Result:=fprecv(Socket.Handle,@Buffer,count,flags);
    If (Result<0) then
      FLastError:=SocketError
    else
      FLastError:=0;
    end;
end;

function TSocketHandler.Send(Const Buffer; Count: Integer): Integer;

Var
  Flags : longint;

begin
  Flags:=FSocket.FWriteFlags;
{$ifdef unix}
  FLastError:=ESysEINTR;
  While (FlastError=ESysEINTR) do
{$endif}
    begin
    Result:=fpsend(Socket.Handle,@Buffer,count,flags);
    If Result<0 then
      FLastError:=SocketError
    else
      FlastError:=0;
    end;
end;

function TSocketHandler.BytesAvailable: Integer;
begin
  Result:=0;
  { we need ioctlsocket here }
end;


Function TSocketHandler.Close: Boolean;
begin
  Result:=True;
end;

constructor ESocketError.Create(ACode: TSocketErrorType; const MsgArgs: array of const);
var
  s: String;
begin
  Code := ACode;
  case ACode of
    seHostNotFound  : s := strHostNotFound;
    seCreationFailed: s := strSocketCreationFailed;
    seBindFailed    : s := strSocketBindFailed;
    seListenFailed  : s := strSocketListenFailed;
    seConnectFailed : s := strSocketConnectFailed;
    seAcceptFailed  : s := strSocketAcceptFailed;
    seAcceptWouldBLock : S:= strSocketAcceptWouldBlock;
  end;
  s := Format(s, MsgArgs);
  inherited Create(s);
end;

{ ---------------------------------------------------------------------
    TSocketStream
  ---------------------------------------------------------------------}
Constructor TSocketStream.Create (AHandle : Longint; AHandler : TSocketHandler = Nil);

begin
  Inherited Create(AHandle);
  FSocketInitialized := true;
  GetSockOptions;
  FHandler:=AHandler;
  If (FHandler=Nil) then
    FHandler:=TSocketHandler.Create;
  FHandler.SetSocket(Self);
end;

destructor TSocketStream.Destroy;
begin
  if FSocketInitialized then
    FHandler.Close; // Ignore the result
  FreeAndNil(FHandler);  
  CloseSocket(Handle);
  inherited Destroy;
end;

Procedure TSocketStream.GetSockOptions;

begin
end;

function TSocketStream.GetLastError: Integer;
begin
  Result:=FHandler.LastError;
end;

Procedure TSocketStream.SetSocketOptions(Value : TSocketOptions);

begin
end;

function TSocketStream.Seek(Offset: Longint; Origin: Word): Longint;

begin
  Result:=0;
end;

Function TSocketStream.Read (Var Buffer; Count : Longint) : longint;

begin
  Result:=FHandler.Recv(Buffer,Count);
end;

Function TSocketStream.Write (Const Buffer; Count : Longint) :Longint;

begin
  Result:=FHandler.Send(Buffer,Count);
end;

function TSocketStream.GetLocalAddress: TSockAddr;
var
  len: LongInt;
begin
  len := SizeOf(TSockAddr);
  if fpGetSockName(Handle, @Result, @len) <> 0 then
    FillChar(Result, SizeOf(Result), 0);
end;

function TSocketStream.GetRemoteAddress: TSockAddr;
var
  len: LongInt;
begin
  len := SizeOf(TSockAddr);
  if fpGetPeerName(Handle, @Result, @len) <> 0 then
    FillChar(Result, SizeOf(Result), 0);
end;


{ ---------------------------------------------------------------------
    TSocketServer
  ---------------------------------------------------------------------}

Constructor TSocketServer.Create(ASocket : Longint; AHandler : TSocketHandler);

begin
  FSocket:=ASocket;
  FQueueSize :=5;
  FMaxConnections:=-1;
  if (AHandler=Nil) then
    AHandler:=TSocketHandler.Create;
  FHandler:=AHandler;
end;

Destructor TSocketServer.Destroy;

begin
  Close;
  FreeAndNil(FHandler);
  Inherited;
end;

Procedure TSocketServer.Close;

begin
  If FSocket<>-1 Then
    CloseSocket(FSocket);
  FSocket:=-1;
end;

procedure TSocketServer.Abort;
var
  ASocket: longint;
begin
{$if defined(unix)}
  fpShutdown(FSocket,SHUT_RDWR);
{$elseif defined(mswindows) or defined(hasamiga)}
  CloseSocket(FSocket);
{$else}
  {$WARNING Method Abort is not tested on this platform!}
  ASocket:=FSocket;
  fpShutdown(ASocket,SHUT_RDWR);
  CloseSocket(ASocket);
{$endif}
end;

Procedure TSocketServer.Listen;

begin
  If Not FBound then
    Bind;
  If  Sockets.FpListen(FSocket,FQueueSize)<>0 then
    Raise ESocketError.Create(seListenFailed,[FSocket,SocketError]);
end;

function TSocketServer.GetSockopt(ALevel, AOptName: cint; Var optval;
  var optlen: tsocklen): Boolean;
begin
  Result:=fpGetSockOpt(FSocket,ALevel,AOptName,@optval,@optlen)<>-1;
end;

function TSocketServer.SetSockopt(ALevel, AOptName: cint; var optval;
  optlen: tsocklen): Boolean;
begin
  Result:=fpSetSockOpt(FSocket,ALevel,AOptName,@optval,optlen)<>-1;
end;

Function TInetServer.GetConnection : TSocketStream;

var
  NewSocket : longint;

begin
  Result:=Nil;
  NewSocket:=Accept;
  if (NewSocket<0) then
    Raise ESocketError.Create(seAcceptFailed,[Socket,SocketError]);
  If FAccepting and DoConnectQuery(NewSocket) Then
    Result:=SockToStream(NewSocket)
  else
    CloseSocket(NewSocket);
end;

function TSocketServer.HandleAcceptError(E: ESocketError): TAcceptErrorAction;
begin
  if FAccepting then
    Result:=aeaRaise
  else
    Result:=aeaStop;
  if Assigned(FOnAcceptError) then
    FOnAcceptError(Self,FSocket,E,Result);
end;

Procedure TSocketServer.StartAccepting;

Var
 NoConnections : Integer;
 Stream : TSocketStream;

begin
  FAccepting := True;
  NoConnections := 0;
  Listen;
  Repeat
    Repeat
      Try
        Stream:=GetConnection;
        if Assigned(Stream) then
          begin
          Inc (NoConnections);
          DoConnect(Stream);
          end;
      except
        On E : ESocketError do
          begin
          If E.Code=seAcceptWouldBlock then
            DoOnIdle
          else
            Case HandleAcceptError(E) of
              aeaIgnore : ;
              aeaStop : FAccepting:=False;
              aeaRaise : Raise;
            end;
          end;
       end;
    Until (Stream<>Nil) or (Not NonBlocking);
  Until Not (FAccepting) or ((FMaxConnections<>-1) and (NoConnections>=FMaxConnections));
end;

procedure TSocketServer.StopAccepting(DoAbort: Boolean = False);

begin
  FAccepting:=False;
  If DoAbort then
    Abort;
end;

Procedure TSocketServer.DoOnIdle;

begin
  If Assigned(FOnIdle) then
    FOnIdle(Self);
end;

function TSocketServer.GetReuseAddress: Boolean;
Var
  L : cint;
  ls : Tsocklen;
begin
  L:=0;
  ls:=0;
{$IFDEF UNIX}
  if not GetSockOpt(SOL_SOCKET, SO_REUSEADDR, L, LS) then
    Raise ESocketError.CreateFmt('Failed to get SO_REUSEADDR to %d: %d',[l,socketerror]);
  Result:=(L<>0);
{$ELSE}
  Result:=True;
{$ENDIF}

end;

function TSocketServer.GetKeepAlive: Boolean;
Var
  L : cint;
  ls : Tsocklen;
begin
  L:=0;
  ls:=0;
{$IFDEF UNIX}
  if Not GetSockOpt(SOL_SOCKET, SO_KEEPALIVE, L, LS) then
    Raise ESocketError.CreateFmt('Failed to get SO_KEEPALIVE: %d',[socketerror]);
  Result:=(L<>0);
{$ELSE}
  Result:=True;
{$ENDIF}
end;

function TSocketServer.GetLinger: Integer;
Var
  L : linger;
  ls : tsocklen;

begin
  L.l_onoff:=0;
  l.l_linger:=0;
  if Not GetSockOpt(SOL_SOCKET, SO_LINGER, l, ls) then
    Raise ESocketError.CreateFmt('Failed to set linger: %d',[socketerror]);
  if l.l_onoff=0 then
    Result:=-1
  else
    Result:=l.l_linger;
end;

Procedure TSocketServer.DoConnect(ASocket : TSocketStream);

begin
  If Assigned(FOnConnect) Then
    FOnConnect(Self,ASocket);
end;

Function TSocketServer.DoConnectQuery(ASocket : Longint) : Boolean;

begin
  Result:=True;
  If Assigned(FOnConnectQuery) then
    FOnConnectQuery(Self,ASocket,Result);
end;

Procedure TSocketServer.SetNonBlocking;

begin
{$ifdef Unix}
  fpfcntl(FSocket,F_SETFL,O_NONBLOCK);
{$endif}
  FNonBlocking:=True;
end;

procedure TSocketServer.SetLinger(ALinger: Integer);
Var
  L : linger;
begin
  L.l_onoff:=Ord(ALinger>0);
  if ALinger<0 then
    l.l_linger:=ALinger
  else
    l.l_linger:=0;
  if Not SetSockOpt(SOL_SOCKET, SO_LINGER, l, SizeOf(L)) then
    Raise ESocketError.CreateFmt('Failed to set linger: %d',[socketerror]);
end;

procedure TSocketServer.SetReuseAddress(AValue: Boolean);
Var
  L : cint;
begin
  L:=Ord(AValue);
{$IFDEF UNIX}
  if not SetSockOpt(SOL_SOCKET, SO_REUSEADDR , L, SizeOf(L)) then
    Raise ESocketError.CreateFmt('Failed to set SO_REUSEADDR to %d: %d',[l,socketerror]);
{$ENDIF}
end;

procedure TSocketServer.SetKeepAlive(AValue: Boolean);
Var
  L : cint;
begin
  L:=Ord(AValue);
{$IFDEF UNIX}
  if Not SetSockOpt(SOL_SOCKET, SO_KEEPALIVE, L, SizeOf(L)) then
    Raise ESocketError.CreateFmt('Failed to set SO_REUSEADDR to %d: %d',[l,socketerror]);
{$ENDIF}
end;

{ ---------------------------------------------------------------------
    TInetServer
  ---------------------------------------------------------------------}

Constructor TInetServer.Create(APort: Word);

begin
  Create('0.0.0.0', aPort);
end;

Constructor TInetServer.Create(const aHost: string; const APort: Word; AHAndler : TSocketHandler = Nil);

Var S : longint;

begin
  FHost:=aHost;
  FPort:=APort;
  S:=Sockets.FpSocket(AF_INET,SOCK_STREAM,0);
  If S=-1 Then
    Raise ESocketError.Create(seCreationFailed,[Format('%d',[APort])]);
  Inherited Create(S,AHandler);
end;

Procedure TInetServer.Bind;

begin
  Faddr.sin_family := AF_INET;
  Faddr.sin_port := ShortHostToNet(FPort);
  Faddr.sin_addr.s_addr := LongWord(StrToNetAddr(FHost));
  if  Sockets.fpBind(FSocket, @FAddr, Sizeof(FAddr))<>0 then
    raise ESocketError.Create(seBindFailed, [IntToStr(FPort)]);
  FBound:=True;
end;

Function  TInetServer.SockToStream (ASocket : Longint) : TSocketStream;

begin
  Result:=TInetSocket.Create(ASocket);
  (Result as TInetSocket).FHost:='';
  (Result as TInetSocket).FPort:=FPort;
end;

Function TInetServer.Accept : Longint;

Var
  L : longint;
  R : integer;
begin
  L:=SizeOf(FAddr);
{$IFDEF UNIX}
  R:=ESysEINTR;
  While (R=ESysEINTR) do
{$ENDIF UNIX}
   begin
   Result:=Sockets.fpAccept(Socket,@Faddr,@L);
   R:=SocketError;
   end;
{$ifdef Unix}
  If (Result<0) then
    If R=ESysEWOULDBLOCK then
      Raise ESocketError.Create(seAcceptWouldBlock,[socket]);
{$endif}
  if (Result<0) or Not (FAccepting and FHandler.Accept) then
    begin
    CloseSocket(Result);
    Raise ESocketError.Create(seAcceptFailed,[Socket,SocketError])
    end;
end;

{ ---------------------------------------------------------------------
    TUnixServer
  ---------------------------------------------------------------------}
{$ifdef Unix}
Constructor TUnixServer.Create(AFileName : String; AHandler : TSocketHandler = Nil);

Var S : Longint;

begin
  FFileName:=AFileName;
  S:=Sockets.fpSocket(AF_UNIX,SOCK_STREAM,0);
  If S=-1 then
    Raise ESocketError.Create(seCreationFailed,[AFileName])
  else
    Inherited Create(S,AHandler);
end;

Procedure TUnixServer.Close;
begin
  Inherited Close;
  DeleteFile(FFileName);
  FFileName:='';
end;

Procedure TUnixServer.Bind;

var
  AddrLen  : longint;
begin
  Str2UnixSockAddr(FFilename,FUnixAddr,AddrLen);
  If  Sockets.FpBind(Socket,@FUnixAddr,AddrLen)<>0 then
    Raise ESocketError.Create(seBindFailed,[FFileName]);
  FBound:=True;
end;

Function TUnixServer.Accept : Longint;

Var L : longint;

begin
  L:=Length(FFileName);
  Result:=Sockets.fpAccept(Socket,@FUnixAddr,@L);
  If Result<0 then
    If SocketError=ESysEWOULDBLOCK then
      Raise ESocketError.Create(seAcceptWouldBlock,[socket])
    else
      Raise ESocketError.Create(seAcceptFailed,[socket,SocketError]);
end;

Function  TUnixServer.SockToStream (ASocket : Longint) : TSocketStream;

begin
  Result:=TUnixSocket.Create(ASocket);
  (Result as TUnixSocket).FFileName:=FFileName;
end;

Function TUnixServer.GetConnection : TSocketStream;

var
  NewSocket : longint;

begin
  Result:=Nil;
  NewSocket:=Accept;
  if (NewSocket<0) then
    Raise ESocketError.Create(seAcceptFailed,[Socket,SocketError]);
  If FAccepting and DoConnectQuery(NewSocket) Then
    Result:=SockToStream(NewSocket)
  else
    CloseSocket(NewSocket);
end;

{$endif}

{ ---------------------------------------------------------------------
    TInetSocket
  ---------------------------------------------------------------------}

Constructor TInetSocket.Create(const AHost: String; APort: Word;AHandler : TSocketHandler = Nil);

Var
  S : Longint;

begin
  FHost:=AHost;
  FPort:=APort;
  S:=fpSocket(AF_INET,SOCK_STREAM,0);
  Inherited Create(S,AHandler);
  if (AHandler=Nil) then // Backwards compatible behaviour.
    Connect;
end;

Procedure TInetSocket.Connect;

Var
  A : THostAddr;
  addr: TInetSockAddr;
  Res : Integer;

begin
  A := StrToHostAddr(FHost);
  if A.s_bytes[1] = 0 then
    With THostResolver.Create(Nil) do
      try
        If Not NameLookup(FHost) then
          raise ESocketError.Create(seHostNotFound, [FHost]);
        A:=HostAddress;
      finally
        free;
      end;
  addr.sin_family := AF_INET;
  addr.sin_port := ShortHostToNet(FPort);
  addr.sin_addr.s_addr := HostToNet(a.s_addr);
  {$ifdef unix}
  Res:=ESysEINTR;
    While (Res=ESysEINTR) do
  {$endif}
      Res:=fpConnect(Handle, @addr, sizeof(addr));
  If Not (Res<0) then
    if not FHandler.Connect then
      begin
      Res:=-1;
      CloseSocket(Handle);
      end;
  If (Res<0) then
    Raise ESocketError.Create(seConnectFailed, [Format('%s:%d',[FHost, FPort])]);
end;

{ ---------------------------------------------------------------------
    TUnixSocket
  ---------------------------------------------------------------------}
{$ifdef Unix}
Constructor TUnixSocket.Create(ASocket : Longint);

begin
  Inherited Create(ASocket);
end;

Constructor TUnixSocket.Create(AFileName : String);

Var S : Longint;

begin
  FFileName:=AFileName;
  S:=FpSocket(AF_UNIX,SOCK_STREAM,0);
  DoConnect(S);
  Inherited Create(S);
end;

Procedure TUnixSocket.DoConnect(ASocket : longint);

Var
  UnixAddr : TUnixSockAddr;
  AddrLen  : longint;
begin
  Str2UnixSockAddr(FFilename,UnixAddr,AddrLen);
  If  FpConnect(ASocket,@UnixAddr,AddrLen)<>0 then
    Raise ESocketError.Create(seConnectFailed,[FFilename]);
end;
{$endif}
end.
