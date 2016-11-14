{
    $Id: header,v 1.1 2000/07/13 06:33:45 michael Exp $
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1999-2000 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{
  fcgigate: implements TFastCGIGatewayApplication, an application that
  acts as a CGI application and transforms CGI requests to FastCGI requests.

  Usage is as simple as

uses
  fcgigate;

begin
  With Application do
    begin
      HostName:='127.0.0.1'; // Where is FastCGI app running ?
      Port:=2015;  // What Port does it listen on ?
      Initialize;
      Run;
    end;

    Alternatively, an .ini file can be used:

uses
  fcgigate;

begin
  With Application do
    begin
      ConfigFileName:='/etc/mygate.ini';
      Initialize;
      Run;
    end;

}
unit fcgigate;

{ $define CGIGDEBUG}
{$mode objfpc}{$H+}

interface

uses
{$IFDEF CGIGDEBUG}
  dbugintf,
{$endif}
  Classes, SysUtils,httpDefs,custcgi,fastcgi,ssockets,inifiles,custweb;

Type

  { TCGIGateWayResponse }

  TCGIGateWayResponse = Class(TCGIResponse)
  Protected
    Procedure DoSendHeaders(Headers : TStrings); override;
  end;

  { TFastCGIGatewayApplication }

  { TFastCGIGatewayHandler }

  TFastCGIGatewayHandler = Class(TCgiHandler)
  private
    FConfigFile: String;
    FFastCGIBinary: String;
    FHostName: String;
    FPort: Integer;
    FSocket: TInetSocket;
    FInitDone : Boolean;
    FEnvironment : TStrings;
    procedure DisconnectfromFCGI;
    procedure SetConfigFile(const AValue: String);
    procedure SetEnvironment(const AValue: TStrings);
    procedure SetHostname(const AValue: String);
  protected
    // Auxiliary routines
    Function CreateResponse(AOutput : TStream) : TCGIResponse; override;
    Procedure CheckInitDone;
    procedure RaiseError(const Msg: String);
    // Configuration. Override to read additional values from .ini file.
    procedure ReadConfigFile(Ini: TIniFile); virtual;
    //  FASTCGI protocol
    // Allocate record for content length AContentLength.
    // Allocated size is AContentLength + SizeOf(FCGI_Header)
    function CreateFastCGIRecord(const AContentLength: Word): PFCGI_Header;
    // Read FastCGI record from socket. Result must be freed by called.
    Function ReadFastCGIRecord: PFCGI_Header;
    // Initialize memory containing fastcgi record header
    procedure InitFastCGIRecord(P: PFCGI_Header; Const AContentLength, APadLength : Word);
    // Send record over socket.
    procedure SendFastCGIRecord(P: PFCGI_Header);
    // Override this to handle FastCGI records that this class does not handle. Set EOR to 'True' if the request should be aborted.
    procedure ProcessUnknownRecord(const Rec: PFCGI_Header; const AResponse: TResponse; var EOR: Boolean); virtual;
    // Override this to send additional records when communication starts.
    procedure SendBeginRequest; virtual;
    // Transform CGI environment variables.
    Function TransformRequestVars: String;virtual;
    // Encode name=value pair for PARAMS fastcgi record.
    Function EncodeFastCGIParam(N, V: AnsiString): String;
    // High-level Communication
    // Send data from request
    procedure SendRequestData(const ARequest : TRequest); virtual;
    // Read FastCGI response.
    procedure ReadResponse(AResponse : TResponse); virtual;
    // Start CGIBinary if initial connect failed.
    procedure StartFCGIBinary;
    // Connect to FastCGI server. Will call StartFCGIBinary.
    procedure ConnectToFCGI;
    // Properties
    // Communication socket
    Property Socket : TInetSocket Read FSocket Write FSocket;
    // Initialize done ?
    Property InitDone : Boolean Read FInitDone;
  Public
    Constructor Create(AOwner : TComponent); override;
    Destructor Destroy; override;
    Procedure HandleRequest(ARequest : Trequest; AResponse : TResponse); override;
    Procedure Initialize;
    Property ConfigFileName : String Read FConfigFile Write SetConfigFile;
    Property FastCGIBinary : String Read FFastCGIBinary Write FFastCGIBinary;
    Property HostName : String Read FHostName Write SetHostname;
    Property Port : Integer Read FPort Write FPort;
    // Values in here override CGI environment variables.
    Property Environment : TStrings Read FEnvironment Write SetEnvironment;
  end;

  TFastCGIGatewayApplication = Class(TCustomCGIApplication)
  protected
    function InitializeWebHandler: TWebHandler; override;
  public
    Procedure Initialize; override;
  end;

Resourcestring
  SErrCouldNotConnectToFCGI = 'Could not connect to FastCGI server.';
  SErrNoConnectionData      = 'No FastCGI connection data available.';
  SErrInitDone              = 'Operation must be performed prior to calling Initialize';

Const
  SConfig = 'FCGI';
  KeyHost = 'Host';
  KeyPort = 'Port';
  KeyName = 'Name';
  SEnvironment = 'Environment';
  KeyPathInfo = 'Home';

Var
  Application : TFastCGIGatewayApplication;

Procedure InitCGIGateWay; // Initializes Application.
Procedure DoneCGIGateWay; // Frees Application.

implementation



{ TCGIGateWayResponse }

procedure TCGIGatewayResponse.DoSendHeaders(Headers: TStrings);

begin
  // Do nothing. Headers are in response from FastCGI and are sent as content;
end;

procedure TFastCGIGatewayHandler.SetConfigFile(const AValue: String);
begin
  if FConfigFile=AValue then exit;
  CheckInitDone;
  FConfigFile:=AValue;
end;

procedure TFastCGIGatewayHandler.SetEnvironment(const AValue: TStrings);
begin
  FEnvironment.Assign(AValue);
end;

procedure TFastCGIGatewayHandler.SetHostname(const AValue: String);
begin
  if FHostName=AValue then exit;
  CheckInitDone;
  FHostName:=AValue;
end;

procedure TFastCGIGatewayHandler.CheckInitDone;
begin
  If FInitDone then
    RaiseError(SErrInitDone);
end;

function TFastCGIGatewayHandler.CreateResponse(AOutput: TStream): TCGIResponse;
begin
{$IFDEF CGIGDEBUG}SendMethodEnter('CreateResponse');{$ENDIF}
  Result:=TCGIGatewayResponse.CreateCGI(Self,AOutput);
{$IFDEF CGIGDEBUG}SendMethodExit('CreateResponse');{$ENDIF}
end;

Procedure TFastCGIGatewayHandler.StartFCGIBinary;

begin
  ExecuteProcess(FastCGIBinary,'',[]);
end;

Procedure TFastCGIGatewayHandler.ConnectToFCGI;

begin
  try
    FSocket:=TInetSocket.Create(FHostName,FPort);
  except
    FSocket:=Nil;
  end;
  If (FSocket=Nil)
     and ((LowerCase(Hostname)='localhost') or (Hostname='127.0.0.1'))
     and (FastCGIBinary<>'') then
    begin
    StartFCGIBinary;
    try
      FSocket:=TInetSocket.Create(FHostName,FPort);
    except
      FSocket:=Nil;
    end;
    end;
  If (FSocket=Nil) Then
    RaiseError(SErrCouldNotConnectToFCGI);
end;

Function DebugString(Var S : String) : String;

Var
  I : Integer;

begin
  For I:=1 to length(S) do
    If (ord(S[i]) in [32..127]) then
      Result:=Result+S[i]
    else
      Result:=Result+Format('#%.3d',[Ord(S[i])]);
end;

Function TFastCGIGatewayHandler.EncodeFastCGIParam(N,V : AnsiString) : String;

  Function CalcJump(ALen : Integer) : Integer;
  begin
    If ALen<128 then
      Result:=1
    else
      Result:=4;
  end;

  Procedure AddLengthEncoding(Var S : String; ALen : Integer; Var Offset : Integer);

  Var
    J,L : integer;

  begin
    J:=CalcJump(ALen);
    If (J=1) then
      S[Offset] := AnsiChar(ALen)
    else
      begin
      ALen:=NtoBE(ALen);
      S[Offset]:=AnsiChar(((ALen shr 24) and $FF) + $80);
      S[Offset+1]:=AnsiChar((ALen shr 16) and $FF);
      S[Offset+2]:=AnsiChar((ALen shr  8) and $FF);
      S[Offset+3]:=AnsiChar(ALen and $FF);
      end;
    if (Byte(S[Offset]) and 128) = 0 then
      L:=Byte(S[Offset])
    else
      L:=BEtoN(PWord(@(S[Offset]))^);
    inc(Offset,J);
  end;


var
  J   : integer;
  NLen,VLen : integer;
  BlockSize : word;

begin
{$IFDEF CGIGDEBUG}SendMethodEnter('EncodeFastCGIParam');{$ENDIF}
  NLen:=Length(N);
  VLen:=Length(V);
  BlockSize := NLen+CalcJump(NLen)+VLen+CalcJump(VLen);
  SetLength(Result,BlockSize);
  J:=1;
  AddlengthEncoding(Result,NLen,J);
  AddlengthEncoding(Result,VLen,J);
  move(N[1],Result[J],NLen);
  move(V[1],Result[J+NLen],VLen);
{$IFDEF CGIGDEBUG}SendMethodExit('EncodeFastCGIParam');{$ENDIF}
end;

constructor TFastCGIGatewayHandler.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEnvironment:=TStringList.Create;
end;

destructor TFastCGIGatewayHandler.Destroy;
begin
  FreeAndNil(FEnvironment);
  inherited Destroy;
end;

Function TFastCGIGatewayHandler.TransformRequestVars : String;

Var
  L : TStringList;
  I,J : Integer;
  N,N2,V : String;

begin
  L:=TStringList.Create;
  try
    GetCGIVarList(L);
    I:=0;
    For I:=0 to L.Count-1 do
      begin
      L.GetNameValue(I,N,V);
      J:=FEnvironment.IndexOfName(N);
      If (J<>-1) then
        L.GetNameValue(J,N2,V); // Keep original name
      Result:=Result+EncodeFastCGIParam(N,V);
      end;
  finally
    L.Free;
  end;
end;

Procedure TFastCGIGatewayHandler.SendFastCGIRecord(P : PFCGI_Header);

Var
  Len : Integer;

begin
{$IFDEF CGIGDEBUG}SendMethodEnter('SendFastCGIRecord');{$ENDIF}
  Len := BEtoN(P^.ContentLength) + P^.PaddingLength+sizeof(FCGI_Header);
  FSocket.WriteBuffer(P^, Len);
{$IFDEF CGIGDEBUG}SendMethodExit('SendFastCGIRecord');{$ENDIF}
end;


Procedure TFastCGIGatewayHandler.InitFastCGIRecord(P : PFCGI_Header; Const AContentLength, APadLength : Word);

begin
{$IFDEF CGIGDEBUG}SendMethodEnter('InitFastCGIRecord');{$ENDIF}
  FillChar(P^,SizeOf(FCGI_Header),#0);
  P^.Version:=FCGI_VERSION_1;
  P^.RequestID:=0;
  P^.ContentLength:=NToBE(AContentLength);
  P^.PaddingLength:=APadLength;
{$IFDEF CGIGDEBUG}SendMethodExit('InitFastCGIRecord');{$ENDIF}
end;

function TFastCGIGatewayHandler.CreateFastCGIRecord(const AContentLength: Word) : PFCGI_Header;

Var
  L,PL : INteger;

begin
{$IFDEF CGIGDEBUG}SendMethodEnter('CreateFastCGIRecord');{$ENDIF}
  PL:=AContentLength Mod 8;
  If PL<>0 then
    PL:=8-PL;
  L:=SizeOf(FCGI_HEADER)+AContentLength+PL;
  Result:=GetMem(L);
  FillWord(Result^,L div 2,0);
  InitFastCGIRecord(Result,AContentLength,PL);
{$IFDEF CGIGDEBUG}SendMethodExit('CreateFastCGIRecord');{$ENDIF}
end;

Procedure TFastCGIGatewayHandler.SendBeginRequest;

Var
  Req : FCGI_BeginRequestRecord;

begin
{$IFDEF CGIGDEBUG}SendMethodEnter('SendBeginRequest');{$ENDIF}
  FillChar(Req,SizeOf(FCGI_BeginRequestRecord),0);
  InitFastCGIRecord(@Req,SizeOf(FCGI_BeginRequestBody),0);
  Req.Header.ReqType:=FCGI_BEGIN_REQUEST;
  Req.Body.Role:=NtoBE(FCGI_RESPONDER);
  SendFastCGIRecord(@Req);
{$IFDEF CGIGDEBUG}SendMethodExit('SendBeginRequest');{$ENDIF}
end;


Procedure TFastCGIGatewayHandler.SendRequestData(Const ARequest : Trequest);

  Procedure SendString(S : String; RecType : Byte);

  Var
    L : Integer;
    Cont : PFCGI_ContentRecord;

  begin
    {$IFDEF CGIGDEBUG}SendMethodEnter('SendString');{$ENDIF}
    L:=Length(S);
    Cont:=PFCGI_ContentRecord(CreateFastCGIrecord(L));
    try
      Cont^.Header.ReqType:=RecType;
      If (L>0) then
        Move(S[1],Cont^.ContentData[0],L);
      SendFastCGIRecord(PFCGI_Header(Cont));
    finally
      FreeMem(Cont);
    end;
    {$IFDEF CGIGDEBUG}SendMethodExit('SendString');{$ENDIF}
  end;

Var
  Vars : String;

begin
{$IFDEF CGIGDEBUG}SendMethodEnter('SendRequestData');{$ENDIF}
  // Start request
  SendBeginRequest;
  // CGI environment.
  Vars:=TransformRequestVars;
  If (Vars<>'') then
    begin
    SendString(Vars,FCGI_PARAMS);
    SendString('',FCGI_PARAMS);
    end;
  If (UpperCase(ARequest.Method)='POST') and (ARequest.ContentLength>0) then
    SendString(ARequest.Content,FCGI_STDIN);
  SendString('',FCGI_STDIN);
{$IFDEF CGIGDEBUG}SendMethodExit('SendRequestData');{$ENDIF}
end;

Function TFastCGIGatewayHandler.ReadFastCGIRecord : PFCGI_Header;

var
  Header : FCGI_Header;
  BytesRead : integer;
  ContentLength : word;
  PaddingLength : byte;
  ReadBuf : Pchar;

  function ReadBytes(ByteAmount : Word) : boolean;

  begin
   {$IFDEF CGIGDEBUG}SendMethodEnter('ReadBytes '+IntToStr(ByteAmount));{$ENDIF}
    result := False;
    if ByteAmount>0 then
      begin
      BytesRead := FSocket.Read(ReadBuf^, ByteAmount);
      Result:=BytesRead=ByteAmount;
      end;
   {$IFDEF CGIGDEBUG}SendMethodExit('ReadBytes '+IntToStr(BytesRead));{$ENDIF}
  end;

begin
{$IFDEF CGIGDEBUG}SendMethodEnter('ReadFastCGIRecord');{$ENDIF}
  Result := Nil;
  ReadBuf:=@Header;
  if not ReadBytes(Sizeof(Header)) then
    exit;
  ContentLength:=BetoN(Header.contentLength);
  PaddingLength:=Header.paddingLength;
  Result:=Getmem(BytesRead+ContentLength+PaddingLength);
  Result^:=Header;
  ReadBuf:=Pchar(Result)+SizeOf(Header);
  ReadBytes(ContentLength);
  ReadBuf:=ReadBuf+BytesRead;
  ReadBytes(PaddingLength);
{$IFDEF CGIGDEBUG}SendMethodExit('ReadFastCGIRecord');{$ENDIF}
end;

Procedure TFastCGIGatewayHandler.ProcessUnknownRecord(Const Rec : PFCGI_Header; Const AResponse : TResponse; Var EOR : Boolean);

begin
{$IFDEF CGIGDEBUG}SendMethodEnter('ProcessUnknownRecord');{$ENDIF}
{$IFDEF CGIGDEBUG}SendDebugFMT('Unknown record encountered : %d',[Rec^.ReqType]);{$ENDIF}
  EOR:=False;
  // Do nothing.
{$IFDEF CGIGDEBUG}SendMethodEnter('ProcessUnknownRecord');{$ENDIF}
end;

Procedure TFastCGIGatewayHandler.ReadResponse(AResponse : TResponse);

Var
  Rec : PFCGI_Header;
  CL : Integer;
  WBuf : PChar;
  EOR : Boolean;

begin
{$IFDEF CGIGDEBUG}SendMethodEnter('ReadResponse');{$ENDIF}
  EOR:=False;
  Rec:=ReadFastCGIRecord;
  While Assigned(Rec) do
    begin
    CL:=BeToN(Rec^.contentLength);
    If (Rec^.reqtype=FCGI_STDOUT) and (CL>0) then
      begin
      if (AResponse.ContentStream=Nil) then
         begin
         AResponse.ContentStream:=TMemoryStream.Create;
         end;
      WBuf:=Pchar(Rec)+SizeOf(FCGI_Header);
      AResponse.ContentStream.WriteBuffer(WBuf^,CL);
      end
    else If (Rec^.ReqType=FCGI_END_REQUEST) and (CL>0) then
      EOR:=True
    else
      ProcessUnknownRecord(Rec,AResponse,EOR);
   If Assigned(Rec) then
     begin
     FreeMem(Rec);
     Rec:=Nil;
     end;
   If Not EOR then
     Rec:=ReadFastCGIRecord
   else
     Rec:=Nil;
   end;
{$IFDEF CGIGDEBUG}SendMethodExit('ReadResponse');{$ENDIF}
end;

Procedure TFastCGIGatewayHandler.DisconnectfromFCGI;

begin
{$IFDEF CGIGDEBUG}SendMethodEnter('DisconnectfromFCGI');{$ENDIF}
  FreeAndNil(FSocket);
{$IFDEF CGIGDEBUG}SendMethodExit('DisconnectfromFCGI');{$ENDIF}
end;

Procedure TFastCGIGatewayHandler.HandleRequest(ARequest : Trequest; AResponse : TResponse);

begin
{$IFDEF CGIGDEBUG}SendMethodEnter('Handle request');{$ENDIF}
  ConnectToFCGI;
  try
    SendRequestData(ARequest);
    ReadResponse(AResponse);
    AResponse.SendResponse;
  finally
    DisconnectfromFCGI;
  end;
{$IFDEF CGIGDEBUG}SendMethodExit('Handle request');{$ENDIF}
end;

procedure TFastCGIGatewayHandler.Initialize;
Var
  Ini : TIniFile;
begin
  If (FConfigFile<>'') and FileExists(FConfigFile) then
    begin
    Ini:=TIniFile.Create(FConfigFile);
    try
      ReadConfigFile(Ini);
    finally
      Ini.Free;
    end;
    end;
  if (Hostname='') or (Port=0) then
    RaiseError(SErrNoConnectionData);
  FInitDone:=True;
end;

procedure TFastCGIGatewayHandler.RaiseError(Const Msg : String);

begin
  Raise HTTPError.Create(Msg);
end;


procedure TFastCGIGatewayHandler.ReadConfigFile(Ini : TIniFile);
begin
  With Ini do
    begin
    HostName:=ReadString(SConfig,KeyHost,'');
    Port:=ReadInteger(SConfig,KeyPort,0);
    FastCGIBinary:=ReadString(SConfig,KeyName,'');
    If SectionExists(SEnvironment) then
      ReadSectionValues(SEnvironment,FENvironment);
    If ValueExists(SConfig,KeyPathInfo) then
      FEnvironment.Values['PATH_INFO']:=ReadString(SConfig,KeyPathInfo,'');
    end;
end;

procedure TFastCGIGatewayApplication.Initialize;
begin
{$IFDEF CGIGDEBUG}SendMethodEnter('Initialize');{$ENDIF}
  inherited Initialize;
  TFastCGIGatewayHandler(WebHandler).Initialize;
{$IFDEF CGIGDEBUG}SendMethodExit('Initialize');{$ENDIF}
end;

function TFastCGIGatewayApplication.InitializeWebHandler: TWebHandler;
begin
  Result:=TFastCGIGatewayHandler.Create(self);
end;

Procedure InitCGIGateWay; // Initializes Application.

begin
  Application:=TFastCGIGatewayApplication.Create(Nil);
end;

Procedure DoneCGIGateWay; // Frees Application.

begin
  FreeAndNil(Application);
end;


initialization
  InitCGIGateWay;

finalization
  DoneCGIGateWay;

end.

