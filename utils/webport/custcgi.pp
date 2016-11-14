{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2014 by the Free Pascal development team

    TCGIApplication class.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{ $define CGIDEBUG}
{$mode objfpc}
{$H+}

unit custcgi;

Interface

uses
  CustWeb, Classes,SysUtils, httpdefs, cgiprotocol, httpprotocol;

Type
  { TCGIRequest }
  TCGIHandler = Class;
  // Content read handler. PByte points to read content, len is length.
  // Return False in ContinueReading to abort reading.
  TCGIContentReadEvent = Procedure (Sender : TRequest; Content : PByte; Len : Integer; Var ContinueReading : Boolean) of object;

  TCGIRequest = Class(TRequest)
  Private
    FCGI : TCGIHandler;
    FOnContentRead: TCGIContentReadEvent;
    function GetCGIVar(Index: integer): String;
  Protected
    Function DoMapCgiToHTTP(Const AVariableName : String; Out AHeaderType : THeader; Out AVariableType : THTTPVariableType) : Boolean;
    function DoGetCGIVar(AVarName: String): String; virtual;
    Procedure InitFromEnvironment; virtual;
    // Read content from stdin. Calls DoContentRead to see if reading must be aborted.
    procedure ReadContent; override;
    // Called whenever input is read from stdin. Calls OnContentRead.
    // Returns True to continue reading, false to abort reading.
    Function DoContentRead(B : PByte; Len : Integer) : Boolean; virtual;
  Public
    Constructor CreateCGI(ACGI : TCGIHandler);
    Function GetCustomHeader(const Name: String) : String; override;
    Property OnContentRead  : TCGIContentReadEvent Read FOnContentRead Write FOnContentRead;
    // Index is index in CGIVarnames array.
    Property GatewayInterface : String Index 4 Read GetCGIVar;
    Property RemoteIdent : String Index 10 read GetCGIVar;
    Property RemoteUser : String Index 11 read GetCGIVar;
    Property RequestMethod : String Index 12 read GetCGIVar;
    Property ServerName : String Index 14 read GetCGIVar;
    Property ServerProtocol : String Index 16 read GetCGIVar;
    Property ServerSoftware : String Index 17 read GetCGIVar;
    Property ServerSignature : String Index 28 Read GetCGIVar;
    Property ServerAddr : String Index 29 Read GetCGIVar;
    Property DocumentRoot : String Index 30 Read GetCGIVar;
    Property ServerAdmin : String Index 31 Read GetCGIVar;
    Property ScriptFileName : String Index 32 Read GetCGIVar;
    Property RemotePort : String Index 33 Read GetCGIVar;
    Property RequestURI : String Index 34 Read GetCGIVar;
    Property ScriptURI : String Index 38 Read GetCGIVar;
    Property ContextDocumentRoot : String Index 40 Read GetCGIVar;
    Property ContextPrefix : String Index 41 Read GetCGIVar;
    Property RequestScheme : String Index 44 Read GetCGIVar;
  end;
  TCGIRequestClass = Class of TCGIRequest;

  { TCGIResponse }

  TCGIResponse = Class(TResponse)
  private
    FCGI : TCGIHandler;
    FOutput : TStream;
  Protected
    Procedure DoSendHeaders(Headers : TStrings); override;
    Procedure DoSendContent; override;
  Public
    Constructor CreateCGI(ACGI : TCGIHandler; AStream : TStream);
  end;
  TCGIResponseClass = Class of TCGIResponse;

  { TCustomCgiApplication }

  { TCgiHandler }

  TCgiHandler = Class(TWebHandler)
  Private
    FResponse : TCGIResponse;
    FRequest : TCGIRequest;
    FOutput : TStream;
  protected
    Function GetEmail : String; override;
    Function GetAdministrator : String; override;
    Function CreateResponse(AOutput : TStream) : TCGIResponse; virtual;
    Function CreateRequest : TCGIRequest; virtual;
    Procedure InitRequest(ARequest : TRequest); override;
    function WaitForRequest(out ARequest : TRequest; out AResponse : TResponse) : boolean; override;
    procedure EndRequest(ARequest : TRequest;AResponse : TResponse); override;
  Public
    Procedure GetCGIVarList(List : TStrings);
    Property Request : TCGIRequest read FRequest;
    Property Response: TCGIResponse Read FResponse;
  end;
  TCgiHandlerClass = Class of TCgiHandler;

  { TCustomCgiApplication }

  TCustomCGIApplication = Class(TCustomWebApplication)
  private
    function GetRequest: TCGIRequest;
    function GetRequestVariable(VarName : String): String;
    function GetRequestVariableCount: Integer;
    function GetResponse: TCGIResponse;
  protected
    function InitializeWebHandler: TWebHandler; override;
  public
    Procedure ShowException(E: Exception);override;
    Property Request : TCGIRequest read GetRequest;
    Property Response: TCGIResponse Read GetResponse;
    Procedure AddResponse(Const S : String);
    Procedure AddResponse(Const Fmt : String; Args : Array of const);
    Procedure AddResponseLn(Const S : String);
    Procedure AddResponseLn(Const Fmt : String; Args : Array of const);
    Procedure GetCGIVarList(List : TStrings);
    Function VariableIsUploadedFile(Const VarName : String) : boolean;
    Function UploadedFileName(Const VarName : String) : String;
    Property RequestVariables[VarName : String] : String Read GetRequestVariable;
    Property RequestVariableCount : Integer Read GetRequestVariableCount;
  end;

  ECGI = Class(EFPWebError);

Var
  CGIRequestClass : TCGIRequestClass = TCGIRequest;
  CGIResponseClass : TCGIResponseClass = TCGIResponse;
  CGIWebHandlerClass : TCgiHandlerClass = TCgiHandler;
  ContentReadRetryInterval : Word = 100; // wait x milliseconds before retrying read
  ContentReadMaxRetryCount : Word = 150; // wait x times before aborting retry

ResourceString
  SWebMaster = 'webmaster';
  SErrNoContentLength = 'No content length passed from server!';

Implementation

uses
{$ifdef CGIDEBUG}
  dbugintf,
{$endif}
  iostream;

Type
  TMap = record
    h : THeader;
    v : THTTPVariableType;
  end;
  TCGIHeaderMap = Array[1..CGIVarCount] of TMap;

Const

  MapCgiToHTTP : TCGIHeaderMap =
   ({ 1: 'AUTH_TYPE'               } (h : hhWWWAuthenticate; v : hvUnknown), // ?
    { 2: 'CONTENT_LENGTH'          } (h : hhContentLength; v : hvUnknown),
    { 3: 'CONTENT_TYPE'            } (h : hhContentType; v : hvUnknown),
    { 4: 'GATEWAY_INTERFACE'       } (h:hhUnknown; v : hvUnknown),
    { 5: 'PATH_INFO'               } (h:hhUnknown; v : hvPathInfo),
    { 6: 'PATH_TRANSLATED'         } (h:hhUnknown; v : hvPathTranslated),
    { 7: 'QUERY_STRING'            } (h:hhUnknown; v : hvQuery),
    { 8: 'REMOTE_ADDR'             } (h:hhUnknown; v : hvRemoteAddress),
    { 9: 'REMOTE_HOST'             } (h:hhUnknown; v : hvRemoteHost),
    { 10: 'REMOTE_IDENT'           } (h:hhUnknown; v : hvUnknown),
    { 11: 'REMOTE_USER'            } (h:hhUnknown; v : hvUnknown),
    { 12: 'REQUEST_METHOD'         } (h:hhUnknown; v : hvMethod),
    { 13: 'SCRIPT_NAME'            } (h:hhUnknown; v : hvScriptName),
    { 14: 'SERVER_NAME'            } (h:hhServer; v : hvUnknown),
    { 15: 'SERVER_PORT'            } (h:hhUnknown; v : hvServerPort),
    { 16: 'SERVER_PROTOCOL'        } (h:hhUnknown; v : hvUnknown),
    { 17: 'SERVER_SOFTWARE'        } (h:hhUnknown; v : hvUnknown),
    { 18: 'HTTP_ACCEPT'            } (h:hhAccept; v : hvUnknown),
    { 19: 'HTTP_ACCEPT_CHARSET'    } (h:hhAcceptCharset; v : hvUnknown),
    { 20: 'HTTP_ACCEPT_ENCODING'   } (h:hhAcceptEncoding; v : hvUnknown),
    { 21: 'HTTP_IF_MODIFIED_SINCE' } (h:hhIfModifiedSince; v : hvUnknown),
    { 22: 'HTTP_REFERER'           } (h:hhReferer; v : hvUnknown),
    { 23: 'HTTP_USER_AGENT'        } (h:hhUserAgent; v : hvUnknown),
    { 24: 'HTTP_COOKIE'            } (h:hhUnknown; v : hvCookie),
    { 25: 'HTTP_IF_NONE_MATCH      } (h:hhIfNoneMatch; v : hvUnknown),
     // Additional Apache vars
    { 26: 'HTTP_CONNECTION'        } (h:hhConnection; v : hvUnknown),
    { 27: 'HTTP_ACCEPT_LANGUAGE'   } (h:hhAcceptLanguage; v : hvUnknown),
    { 28: 'HTTP_HOST'              } (h:hhHost; v : hvUnknown),
    { 29: 'SERVER_SIGNATURE'       } (h:hhUnknown; v : hvUnknown),
    { 30: 'SERVER_ADDR'            } (h:hhUnknown; v : hvUnknown),
    { 31: 'DOCUMENT_ROOT'          } (h:hhUnknown; v : hvUnknown),
    { 32: 'SERVER_ADMIN'           } (h:hhUnknown; v : hvUnknown),
    { 33: 'SCRIPT_FILENAME'        } (h:hhUnknown; v : hvUnknown),
    { 34: 'REMOTE_PORT'            } (h:hhUnknown; v : hvUnknown),
    { 35: 'REQUEST_URI'            } (h:hhUnknown; v : hvUnknown),
    { 36: 'CONTENT'                } (h:hhUnknown; v : hvContent),
    { 37: 'XHTTPREQUESTEDWITH'     } (h:hhUnknown; v : hvXRequestedWith),
    { 38: 'HTTP_AUTHORIZATION'     } (h:hhAuthorization; v : hvUnknown),
    { 39: 'SCRIPT_URI'             } (h:hhUnknown; v : hvUnknown),
    { 40: 'SCRIPT_URL'             } (h:hhUnknown; v : hvURL),
    { 41: 'CONTEXT_DOCUMENT_ROOT'  } (h:hhUnknown; v : hvUnknown),
    { 42: 'CONTEXT_PREFIX'         } (h:hhUnknown; v : hvUnknown),
    { 43: 'HTTP_CACHE_CONTROL'     } (h:hhCacheControl; v : hvUnknown),
    { 44: 'HTTP_PRAGMA'            } (h:hhPragma; v : hvUnknown),
    { 45: 'REQUEST_SCHEME'         } (h:hhUnknown; v : hvUnknown)
  );

procedure TCgiHandler.GetCGIVarList(List: TStrings);

Var
  I : Integer;

begin
  List.Clear;
  For I:=1 to cgiVarCount do
    List.Add(CGIVarNames[i]+'='+GetEnvironmentVariable(CGIVarNames[i]));
end;

function TCgiHandler.GetEmail: String;

Var
  H : String;

begin
  Result:=inherited GetEmail;
  If (Result='') then
    begin
    H:=Request.ServerName;
    If (H<>'') then
      Result:=Administrator+'@'+H
    end;
end;

function TCgiHandler.GetAdministrator: String;

begin
  Result:=Inherited GetAdministrator;
  If (result='') then
    Result:=SWebMaster;
end;

function TCgiHandler.CreateResponse(AOutput : TStream): TCGIResponse;

Var
  C : TCGIResponseClass;

begin
  C:=CGIResponseClass;
  if (C=Nil) then
    C:=TCGIResponse;
  Result:=TCGIResponse.CreateCGI(Self,AOutput);
end;

function TCgiHandler.CreateRequest: TCGIRequest;

Var
  C : TCGIRequestClass;

begin
  C:=CGIRequestClass;
  if (C=Nil) then
    C:=TCGIRequest;
  Result:=C.CreateCGI(Self);
end;

procedure TCgiHandler.InitRequest(ARequest: TRequest);
begin
  inherited InitRequest(ARequest);
  if (ARequest is TCGIRequest) then
    With (ARequest as TCGIRequest) do
      begin
      InitFromEnvironment;
      InitRequestVars;
      end;
end;

function TCgiHandler.WaitForRequest(out ARequest: TRequest; out AResponse: TResponse): boolean;
begin
  FOutput:=TIOStream.Create(iosOutput);
  FRequest:=CreateRequest;
  FResponse:=CreateResponse(FOutput);
  InitRequest(FRequest);
  InitResponse(FResponse);
  ARequest:=FRequest;
  AResponse:=FResponse;
  Result := True;
end;

procedure TCgiHandler.EndRequest(ARequest: TRequest; AResponse: TResponse);
begin
  inherited;
  FreeAndNil(FOutPut);
  Terminate;
end;

constructor TCGIRequest.CreateCGI(ACGI: TCGIHandler);
begin
  Inherited Create;
  FCGI:=ACGI;
end;

function TCGIRequest.GetCustomHeader(const Name: String): String;
begin
  Result:=inherited GetCustomHeader(Name);
  // Check environment
  if (Result='') then
    Result:=DoGetCGIVAr('HTTP_'+StringReplace(Uppercase(Name),'-','_',[rfReplaceAll]));
end;

{ TCGIHTTPRequest }
function TCGIRequest.DoGetCGIVar(AVarName : String) : String;

begin
  GetEnvironmentVariable(AVarName);
end;

function TCGIRequest.GetCGIVar(Index: integer): String;

Var
  R : String;

begin
  if Index in [1..CGIVarCount] then
    R:=DoGetCGIVar(CGIVarNames[Index])
  else
    R:='';
  Result:=HTTPDecode(R);
end;

function TCGIRequest.DoMapCgiToHTTP(const AVariableName: String; out
  AHeaderType: THeader; Out AVariableType: THTTPVariableType): Boolean;
Var
  I : Integer;
begin
  I:=IndexOfCGIVar(AVariableName);
  Result:=I<>-1;
  if Result then
    begin
    AHeaderType:=MapCgiToHTTP[i].H;
    AVariableType:=MapCgiToHTTP[i].V;
    end;
end;

procedure TCGIRequest.InitFromEnvironment;


Var
  I : Integer;
  R,V,OV : String;
  M : TMap;
  
begin
  For I:=1 to CGIVarCount do
    begin
    V:=GetEnvironmentVariable(CGIVarNames[I]);
    if (V<>'') then
      begin
      M:=MapCgiToHTTP[i];
      if M.H<>hhUnknown then
        SetHeader(M.H,HTTPDecode(V))
      else if M.V<>hvUnknown then
        begin
        if M.V<>hvQuery then
          V:=HTTPDecode(V);
        SetHTTPVariable(M.V,V)
        end;
      end;
    end;
  R:=UpCase(Method);
  if (R='POST') or (R='PUT') or (ContentLength>0) then
    ReadContent;
end;

procedure TCGIRequest.ReadContent;
var
  I : TIOStream;
  Cl : Integer;
  B : Byte;
  retrycount: integer;
  BytesRead, a: longint;
  AbortRead : Boolean;
  S : String;

begin
  S:='';
  Cl := ContentLength;
  I:=TIOStream.Create(iosInput);
  Try
    if (Cl<>0) then
      begin
      // It can be that the complete content is not yet sent by the server so repeat the read
      // until all data is really read
      SetLength(S,Cl);
      BytesRead:=0;
      RetryCount:=0;
      AbortRead:=False;
      repeat
        a := I.Read(S[BytesRead+1],Cl-BytesRead);
        BytesRead:=BytesRead+a;
        if (A=0) then // In fact this should not happen, but the content could be delayed...
          begin
          Inc(RetryCount);
          AbortRead:=RetryCount>ContentReadMaxRetryCount;
          if not AbortRead then
            Sleep(ContentReadRetryInterval);
          end
        else
          begin
          RetryCount:=0; // We got data, so let's reset this.
          AbortRead:=Not DoContentRead(PByte(@S[BytesRead+1]),A);
          end;
      until (BytesRead>=Cl) or (AbortRead);
      // In fact the request is incomplete, but this is not the place to throw an error for that
      if BytesRead<Cl then
        SetLength(S,BytesRead);
      end
    else
      begin
      B:=0;
      While (I.Read(B,1)>0) do
        S:=S + chr(B);
      end;
    InitContent(S);
  Finally
    I.Free;
  end;
end;

function TCGIRequest.DoContentRead(B: PByte; Len: Integer): Boolean;
begin
  Result:=True;
  if Assigned(FOnContentRead) then
    FOnContentRead(Self,B,Len,Result);
end;


{ TCGIResponse }

procedure TCGIResponse.DoSendHeaders(Headers : TStrings);
begin
{$ifdef CGIDEBUG}
  SendMethodEnter('TCGIResponse.DoSendHeaders');
  SendDebug('Headers: '+Headers.Text);
{$endif}
  if Assigned(FOutput) then
    Headers.SaveToStream(FOutput);
{$ifdef CGIDEBUG}
  SendMethodExit('TCGIResponse.DoSendHeaders');
{$endif}
end;

procedure TCGIResponse.DoSendContent;
begin
{$ifdef CGIDEBUG}
  SendMethodEnter('TCGIResponse.DoSendContent');
{$endif}
  If Assigned(ContentStream) then
    FOutput.CopyFrom(ContentStream,0)
  else
    Contents.SaveToStream(FOutput);
{$ifdef CGIDEBUG}
  SendMethodExit('TCGIResponse.DoSendContent');
{$endif}
end;

constructor TCGIResponse.CreateCGI(ACGI: TCgiHandler; AStream: TStream);
begin
  inherited Create(ACGI.Request);
  FCGI:=ACGI;
  FOutput:=AStream;
end;

{ TCustomCGIApplication }

function TCustomCGIApplication.GetRequest: TCGIRequest;
begin
  result := TCgiHandler(WebHandler).Request;
end;

function TCustomCGIApplication.GetRequestVariable(VarName : String): String;
begin
  If Assigned(Request) then
    Result:=Request.QueryFields.Values[VarName]
  else
    Result:='';
end;

function TCustomCGIApplication.GetRequestVariableCount: Integer;
begin
  If Assigned(Request) then
     Result:=Request.QueryFields.Count
   else
     Result:=0;
end;

function TCustomCGIApplication.GetResponse: TCGIResponse;
begin
  Result:=TCgiHandler(WebHandler).Response;
end;

function TCustomCGIApplication.InitializeWebHandler: TWebHandler;

Var
  C : TCGIHandlerClass;

begin
  C:=CGIWebHandlerClass;
  if C=Nil then
    C:=TCgiHandler;
  Result:=C.Create(self);
end;

Procedure TCustomCGIApplication.ShowException(E: Exception);
var
  CgiHandler: TCgiHandler;
begin
  CgiHandler := WebHandler as TCgiHandler;
  if assigned(CgiHandler.FResponse) then
    CgiHandler.ShowRequestException(CgiHandler.FResponse,E)
  else
    inherited ShowException(E);
end;

procedure TCustomCGIApplication.AddResponse(const S: String);
Var
  L : Integer;

begin
  L:=Length(S);
  If L>0 then
    Response.Content:=Response.Content+S;
end;

procedure TCustomCGIApplication.AddResponse(const Fmt: String; Args: array of const);
begin
  AddResponse(Format(Fmt,Args));
end;

procedure TCustomCGIApplication.AddResponseLn(const S: String);
begin
  AddResponse(S+LineEnding);
end;

procedure TCustomCGIApplication.AddResponseLn(const Fmt: String; Args: array of const);
begin
  AddResponseLN(Format(Fmt,Args));
end;

procedure TCustomCGIApplication.GetCGIVarList(List: TStrings);
begin
  TCgiHandler(WebHandler).GetCGIVarList(list);
end;

function TCustomCGIApplication.VariableIsUploadedFile(const VarName: String): boolean;
begin
  Result:=Request.Files.IndexOfFile(VarName)<>-1;
end;

function TCustomCGIApplication.UploadedFileName(const VarName: String): String;
begin
  If VariableIsUploadedFile(VarName) then
    Result:=Request.Files.FileByName(VarName).LocalFileName
  else
    Result:='';
end;

initialization

finalization
{$ifdef CGIDEBUG}
  if (SendError<>'') then
    Writeln('Debug failed: ',SendError);
{$endif}
end.
