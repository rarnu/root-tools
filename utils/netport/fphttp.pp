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
{$mode objfpc}
{$H+}
unit fphttp;

Interface

uses sysutils,classes,httpdefs;

Type
{ TODO : Implement wkSession }
  TWebModuleKind = (wkPooled, wkOneShot{, wkSession});

  { THTTPContentProducer }
  TWebActionEvent = Procedure (Sender : TObject;
                               ARequest : TRequest;
                               AResponse : TResponse;
                               Var Handled : Boolean) of object;

  THTTPContentProducer = Class(TComponent)
  private
    FAfterResponse: TResponseEvent;
    FBeforeRequest: TRequestEvent;
    FRequest      : TRequest;
    FResponse: TResponse;
  Protected
    Procedure DoHandleRequest(ARequest : TRequest; AResponse : TResponse; Var Handled : Boolean); virtual;
    Procedure DoGetContent(ARequest : TRequest; Content : TStream; Var Handled : Boolean); virtual;
    Function ProduceContent : String; virtual;
    Procedure SetRequest(ARequest: TRequest);
  Protected
    Property BeforeRequest : TRequestEvent Read FBeforeRequest Write FBeforeRequest;
    Property AfterResponse : TResponseEvent Read FAfterResponse Write FAfterResponse;
  Public
    Procedure HandleRequest(ARequest : TRequest; AResponse : TResponse; Var Handled : Boolean);
    Procedure GetContent(ARequest : TRequest; Content : TStream; Var Handled : Boolean);
    Function  HaveContent : Boolean; virtual;
    function ContentToStream(Stream : TStream) : boolean; virtual;
    Property Request : TRequest Read FRequest;
    Property Response : TResponse Read FResponse;
  end;
  
  { TCustomWebAction }
  TCustomWebAction = Class(TCollectionItem)
  private
    FAfterResponse: TResponseEvent;
    FBeforeRequest: TRequestEvent;
    FContentproducer: THTTPContentProducer;
    FDefault: Boolean;
    FName : String;
  Protected
    procedure SetContentProducer(const AValue: THTTPContentProducer);virtual;
    Function  GetDisplayName : String; override;
    Procedure SetDisplayName(const AValue : String); override;
    Procedure HandleRequest(ARequest : TRequest; AResponse : TResponse; Var Handled : Boolean);
    Procedure DoHandleRequest(ARequest : TRequest; AResponse : TResponse; Var Handled : Boolean); virtual;
  Public
    Function  GetNamePath : String; override;
  published
    Property Name : String Read GetDisplayName Write SetDisplayName;
    Property ContentProducer : THTTPContentProducer Read FContentproducer Write SetContentProducer;
    Property Default : Boolean Read FDefault Write FDefault;
    Property BeforeRequest : TRequestEvent Read FBeforeRequest Write FBeforeRequest;
    Property AfterResponse : TResponseEvent Read FAfterResponse Write FAfterResponse;
  end;

  { TCustomWebActions }
  TGetActionEvent = Procedure (Sender : TObject; ARequest : TRequest; Var ActionName : String) of object;

  TCustomWebActions = Class(TCollection)
  private
    FActionVar : String;
    FDefActionWhenUnknown: Boolean;
    FOnGetAction: TGetActionEvent;
    function GetActions(Index : Integer): TCustomWebAction;
    procedure SetActions(Index : Integer; const AValue: TCustomWebAction);
  Protected
    Function  GetRequestAction(ARequest: TRequest) : TCustomWebAction;
    Function  GetActionName(ARequest : TRequest) : String;
    Property  ActionVar : String Read FactionVar Write FActionVar;
  public
    constructor Create(AItemClass: TCollectionItemClass);
    Procedure Assign(Source : TPersistent); override;
    Function Add : TCustomWebAction;
    Function ActionByName(const AName : String) : TCustomWebAction;
    Function FindAction(const AName : String): TCustomWebAction;
    Function IndexOfAction(const AName : String) : Integer;
    Property OnGetAction : TGetActionEvent Read FOnGetAction Write FOnGetAction;
    Property Actions[Index : Integer] : TCustomWebAction Read GetActions Write SetActions; Default;
    Property DefActionWhenUnknown : Boolean read FDefActionWhenUnknown write FDefActionWhenUnknown;
  end;
  
  { TCustomHTTPModule }

  TInitModuleEvent = Procedure (Sender : TObject; ARequest : TRequest) of object;
  TCustomHTTPModule = Class(TDataModule)
  private
    FAfterInitModule : TInitModuleEvent;
    FBaseURL: String;
    FWebModuleKind: TWebModuleKind;
  Protected
    Class Function DefaultModuleName : String; virtual;
    Class Function DefaultSkipStreaming : Boolean; virtual;
  public
    Class Procedure RegisterModule(Const AModuleName : String = ''); overload;
    Class Procedure RegisterModule(Const AModuleName : String; ASkipStreaming : Boolean); overload;
    Procedure HandleRequest(ARequest : TRequest; AResponse : TResponse); virtual; abstract;
    Procedure DoAfterInitModule(ARequest : TRequest); virtual;
    property Kind: TWebModuleKind read FWebModuleKind write FWebModuleKind default wkPooled;
    Property BaseURL : String Read FBaseURL Write FBaseURL;
    Property AfterInitModule : TInitModuleEvent Read FAfterInitModule Write FAfterInitModule;
  end;
  TCustomHTTPModuleClass = Class of TCustomHTTPModule;

  { TSessionHTTPModule }

  TSessionHTTPModule = Class(TCustomHTTPModule)
  Private
    FCreateSession : Boolean;
    FOnNewSession: TNotifyEvent;
    FOnSessionExpired: TNotifyEvent;
    FSession: TCustomSession;
    FSessionRequest : TRequest;
    function GetSession: TCustomSession;
    procedure SetSession(const AValue: TCustomSession);
  Protected
    Procedure CheckSession(ARequest : TRequest);
    Procedure InitSession(AResponse : TResponse);
    Procedure UpdateSession(AResponse : TResponse);
    Procedure DoneSession; virtual;
  Public
    destructor destroy; override;
    Procedure Notification(AComponent : TComponent;Operation : TOperation); override;
    Procedure HandleRequest(ARequest : TRequest; AResponse : TResponse); override;
    Property CreateSession : Boolean Read FCreateSession Write FCreateSession;
    Property Session : TCustomSession Read GetSession Write SetSession;
    Property OnNewSession : TNotifyEvent Read FOnNewSession Write FOnNewSession;
    Property OnSessionExpired : TNotifyEvent Read FOnSessionExpired Write FOnSessionExpired;
  end;
  TSessionHTTPModuleClass = Class of TSessionHTTPModule;

  EWebSessionError = Class(HTTPError);

  { TSessionFactory }

  TSessionFactory = Class(TComponent)
  private
    FSessionCookie: String;
    FSessionCookiePath: String;
    FTimeOut: Integer;
    FCleanupInterval: Integer;
    FDoneCount: Integer;
  protected
    // Override in descendants
    Function DoCreateSession(ARequest : TRequest) : TCustomSession; virtual; abstract;
    Procedure DoDoneSession(Var ASession : TCustomSession); virtual; abstract;
    Procedure DoCleanupSessions; virtual; abstract;
    Property DoneCount : Integer Read FDoneCount;
  Public
    Function CreateSession(ARequest : TRequest) : TCustomSession;
    Procedure DoneSession(Var ASession : TCustomSession);
    Procedure CleanupSessions;
    // Number of requests before sweeping sessions for stale sessions.
    // Default 1000. Set to 0 to disable.
    // Note that for cgi programs, this will never happen, since the count is reset to 0
    // with each invocation. It takes a special factory to handle that, or a value of 1.
    Property CleanupInterval : Integer read FCleanupInterval Write FCleanUpInterval;
    // Default timeout for sessions, in minutes.
    Property DefaultTimeOutMinutes : Integer Read FTimeOut Write FTimeOut;
    // Default session cookie.
    property SessionCookie : String Read FSessionCookie Write FSessionCookie;
    // Default session cookie path
    Property SessionCookiePath : String Read FSessionCookiePath write FSessionCookiePath;
  end;
  TSessionFactoryClass = Class of TSessionFactory;

  { TModuleItem }

  TModuleItem = Class(TCollectionItem)
  private
    FModuleClass: TCustomHTTPModuleClass;
    FModuleName: String;
    FSkipStreaming: Boolean;
  Public
    Property ModuleClass : TCustomHTTPModuleClass Read FModuleClass Write FModuleClass;
    Property ModuleName : String Read FModuleName Write FModuleName;
    Property SkipStreaming : Boolean Read FSkipStreaming Write FSkipStreaming;
  end;

  { TModuleFactory }

  TModuleFactory = Class(TCollection)
  private
    function GetModule(Index : Integer): TModuleItem;
    procedure SetModule(Index : Integer; const AValue: TModuleItem);
  Public
    Function FindModule(const AModuleName : String) : TModuleItem;
    Function ModuleByName(const AModuleName : String) : TModuleItem;
    Function IndexOfModule(const AModuleName : String) : Integer;
    Property Modules [Index : Integer]: TModuleItem Read GetModule Write SetModule;default;
  end;

  { EFPHTTPError }

  EFPHTTPError = Class(EHTTP);

Procedure RegisterHTTPModule(ModuleClass : TCustomHTTPModuleClass; SkipStreaming : Boolean = False);
Procedure RegisterHTTPModule(Const ModuleName : String; ModuleClass : TCustomHTTPModuleClass; SkipStreaming : Boolean = False);

Var
  ModuleFactory : TModuleFactory;
  SessionFactoryClass : TSessionFactoryClass = nil;

Function SessionFactory : TSessionFactory;

Resourcestring
  SErrNosuchModule = 'No such module registered: "%s"';
  SErrNoSuchAction = 'No action found for action: "%s"';
  SErrUnknownAction = 'Unknown action: "%s"';
  SErrNoDefaultAction = 'No action name and no default action';
  SErrInvActNoDefaultAction = 'Invalid action name and no default action';
  SErrRequestNotHandled = 'Web request was not handled by actions.';
  SErrNoSessionFactoryClass = 'No session manager class available. Include iniwebsession unit and recompile.';
  SErrNoSessionOutsideRequest = 'Default session not available outside handlerequest';

Implementation

{$ifdef cgidebug}
uses dbugintf;
{$endif}

Var
  GSM : TSessionFactory;

Function SessionFactory : TSessionFactory;

begin
  if GSM=Nil then
    begin
    if (SessionFactoryClass=Nil) then
      Raise EFPHTTPError.Create(SErrNoSessionFactoryClass);
    GSM:=SessionFactoryClass.Create(Nil)
    end;
  Result:=GSM;
end;


{ TCustomHTTPModule }

Class Function TCustomHTTPModule.DefaultModuleName: String;
begin
  Result:=ClassName;
end;

Class Function TCustomHTTPModule.DefaultSkipStreaming: Boolean;
begin
  Result:=False;
end;

Class Procedure TCustomHTTPModule.RegisterModule(Const AModuleName: String);
begin
  RegisterModule(AModuleName,DefaultSkipStreaming);
end;

Class Procedure TCustomHTTPModule.RegisterModule(Const AModuleName: String;
  ASkipStreaming: Boolean);

Var
  MN : String;
begin
  MN:=AModuleName;
  if MN='' then
    MN:=DefaultModuleName;
  RegisterHTTPModule(MN,Self,ASkipStreaming);
end;

Procedure TCustomHTTPModule.DoAfterInitModule(ARequest: TRequest);
begin
  If Assigned(FAfterInitModule) then
    FAfterInitModule(Self, ARequest);
end;

{ TSessionFactory }

function TSessionFactory.CreateSession(ARequest: TRequest): TCustomSession;
begin
  Result:=DoCreateSession(ARequest);
  if Assigned(Result) then
    begin
    if (FTimeOut<>0) then
      Result.TimeoutMinutes:=FTimeOut;
    Result.SessionCookie:=Self.SessionCookie;
    Result.SessionCookiePath:=Self.SessionCookiePath;
    end;
end;

procedure TSessionFactory.DoneSession(var ASession: TCustomSession);
begin
  DoDoneSession(ASession);
  if (FCleanupInterval>0) then
    begin
    Inc(FDoneCount);
    If (FDoneCount>=FCleanupInterval) then
      CleanupSessions;
    end;
end;

procedure TSessionFactory.CleanupSessions;
begin
  FDoneCount:=0;
  DoCleanupSessions;
end;

{ TModuleFactory }

function TModuleFactory.GetModule(Index : Integer): TModuleItem;
begin
  Result:=TModuleItem(Items[Index]);
end;

procedure TModuleFactory.SetModule(Index : Integer; const AValue: TModuleItem);
begin
  Items[Index]:=AValue;
end;

function TModuleFactory.FindModule(const AModuleName: String): TModuleItem;

Var
  I : Integer;

begin
  I:=IndexOfModule(AModuleName);
  If (I=-1) then
    Result:=Nil
  else
    Result:=GetModule(I);
end;

function TModuleFactory.ModuleByName(const AModuleName: String): TModuleItem;
begin
  Result:=FindModule(AModuleName);
  If (Result=Nil) then
    Raise EFPHTTPError.CreateFmt(SErrNosuchModule,[AModuleName]);
end;

function TModuleFactory.IndexOfModule(const AModuleName: String): Integer;

begin
  Result:=Count-1;
  While (Result>=0) and (CompareText(Modules[Result].ModuleName,AModuleName)<>0) do
    Dec(Result);
end;


procedure RegisterHTTPModule(ModuleClass: TCustomHTTPModuleClass; SkipStreaming : Boolean = False);
begin
  RegisterHTTPModule(ModuleClass.ClassName,ModuleClass,SkipStreaming);
end;

procedure RegisterHTTPModule(const ModuleName: String;
  ModuleClass: TCustomHTTPModuleClass; SkipStreaming : Boolean = False);
  
Var
  I : Integer;
  MI : TModuleItem;
  
begin
  I:=ModuleFactory.IndexOfModule(ModuleName);
  If (I=-1) then
    begin
    MI:=ModuleFactory.Add as TModuleItem;
    MI.ModuleName:=ModuleName;
    end
  else
    MI:=ModuleFactory[I];
  MI.ModuleClass:=ModuleClass;
  MI.SkipStreaming:=SkipStreaming;
end;

{ THTTPContentProducer }


procedure THTTPContentProducer.HandleRequest(ARequest: TRequest;
  AResponse: TResponse; Var Handled : Boolean);
  
begin
  If Assigned(FBeforeRequest) then
    FBeforeRequest(Self,ARequest);
  DoHandleRequest(Arequest,AResponse,Handled);
  If Assigned(FAfterResponse) then
    FAfterResponse(Self,AResponse);
end;

procedure THTTPContentProducer.GetContent(ARequest: TRequest; Content: TStream; Var Handled : Boolean);
begin
  If Assigned(FBeforeRequest) then
    FBeforeRequest(Self,ARequest);
  DoGetContent(Arequest,Content,Handled);
end;
  
procedure THTTPContentProducer.DoHandleRequest(ARequest: TRequest;
  AResponse: TResponse; Var Handled : Boolean);

Var
  M : TMemoryStream;
  
begin
  FResponse:=AResponse;
  M:=TMemoryStream.Create;
  DoGetContent(ARequest,M,Handled);
  AResponse.ContentStream:=M;
  AResponse.ContentLength:=M.Size;
end;

procedure THTTPContentProducer.DoGetContent(ARequest: TRequest; Content: TStream; Var Handled : Boolean);
begin
  FRequest := ARequest;
  Handled:=ContentToStream(Content);
end;

function THTTPContentProducer.ProduceContent: String;
begin
  Result:='';
end;

procedure THTTPContentProducer.SetRequest(ARequest: TRequest);
begin
  FRequest := ARequest;
end;

function THTTPContentProducer.HaveContent: Boolean;
begin
  Result:=(ProduceContent<>'');
end;

function THTTPContentProducer.ContentToStream(Stream: TStream) : boolean;

Var
  S : String;

begin
  S:=ProduceContent;
  If length(S)>0 then
    begin
    Stream.WriteBuffer(S[1],Length(S));
    Result := True;
    end
  else
    Result := False;
end;

{ TCustomWebAction }

procedure TCustomWebAction.SetContentProducer(const AValue: THTTPContentProducer
  );
begin
  FContentProducer:=AValue;
end;

function TCustomWebAction.GetDisplayName: String;
begin
  Result:=FName;
  If (Result='') then
    begin
    Result:=ClassName+IntToStr(self.Index);
    if Result[1]='T' then
      Delete(Result,1,1)
    end;
end;

Function TCustomWebAction.GetNamePath : String;
begin
  Result:=FName;
  If (Result='') then
    FName:=ClassName+IntToStr(self.Index);
end;

procedure TCustomWebAction.SetDisplayName(const AValue: String);
begin
  Inherited;
  FName:=AValue;
end;

procedure TCustomWebAction.HandleRequest(ARequest: TRequest; AResponse: TResponse; Var Handled : Boolean);

begin
  If Assigned(FBeforeRequest) then
    FBeforeRequest(Self,ARequest);
  DoHandleRequest(Arequest,AResponse,Handled);
  If Assigned(FAfterResponse) then
    FAfterResponse(Self,AResponse);
end;

procedure TCustomWebAction.DoHandleRequest(ARequest: TRequest; AResponse: TResponse; Var Handled : Boolean);

begin
  If Assigned(FContentProducer) then
   FContentProducer.HandleRequest(ARequest,AResponse,Handled)
end;


{ TCustomWebActions }

function TCustomWebActions.GetActions(Index : Integer): TCustomWebAction;
begin
  Result:=TCustomWebAction(Items[Index]);
end;

procedure TCustomWebActions.SetActions(Index : Integer; const AValue: TCustomWebAction);
begin
  Items[Index]:=AValue;
end;

Function TCustomWebActions.GetRequestAction(ARequest: TRequest) : TCustomWebAction;

Var
  S : String;

  Function GetDefaultAction:TCustomWebAction;
  Var I : Integer;
  begin
    Result := nil;
    I:=0;
    While (Result=Nil) and (I<Count) do
    begin
      If Actions[I].Default then
        Result:=Actions[I];
      Inc(I);
    end;
  end;

begin
  Result:=Nil;
  S:=GetActionName(ARequest);
  If (S<>'') then
  begin
    Result:=FindAction(S);
    if Result = nil then
    begin//no action with that name found
      if not DefActionWhenUnknown then
        Raise EFPHTTPError.CreateFmt(SErrNoSuchAction,[s])
      else begin
        Result := GetDefaultAction;
        if Result = nil then
          Raise EFPHTTPError.Create(SErrInvActNoDefaultAction);
      end;
    end;
  end else begin //no action name was specified
    Result := GetDefaultAction;
    If (Result=Nil) then
      Raise EFPHTTPError.Create(SErrNoDefaultAction);
  end;
end;


function TCustomWebActions.GetActionName(ARequest: TRequest): String;

begin
  If (FActionVar<>'') then
    Result:=ARequest.QueryFields.Values[FActionVar]
  else
    Result := '';
  If Assigned(FOnGetAction) then
    FOnGetAction(Self,ARequest,Result);
  // GetNextPathInfo is only used after OnGetAction, so that the call to 
  // GetNextPathInfo can be avoided in the event.
  If (Result='') then
    Result:=ARequest.GetNextPathInfo;
end;

constructor TCustomWebActions.Create(AItemClass: TCollectionItemClass);
begin
  inherited Create(AItemClass);
  FDefActionWhenUnknown:=True;
end;

procedure TCustomWebActions.Assign(Source: TPersistent);
begin
  If (Source is TCustomWebActions) then
    ActionVar:=(Source as TCustomWebActions).ActionVar
  else
    inherited Assign(Source);
end;

function TCustomWebActions.Add: TCustomWebAction;
begin
  Result:=TCustomWebAction(Inherited Add);
end;

function TCustomWebActions.ActionByName(const AName: String): TCustomWebAction;
begin
  Result:=FindAction(AName);
  If (Result=Nil) then
    Raise HTTPError.CreateFmt(SErrUnknownAction,[AName]);
end;

function TCustomWebActions.FindAction(const AName: String): TCustomWebAction;

Var
  I : Integer;

begin
  I:=IndexOfAction(AName);
  If (I=-1) then
    Result:=Nil
  else
    Result:=Actions[I];
end;

function TCustomWebActions.IndexOfAction(const AName: String): Integer;

begin
  Result:=Count-1;
  While (Result>=0) and (CompareText(Actions[Result].Name,AName)<>0) do
    Dec(Result);
end;

function TSessionHTTPModule.GetSession: TCustomSession;
begin
{$ifdef cgidebug}SendMethodEnter('SessionHTTPModule.GetSession');{$endif}
  If (csDesigning in ComponentState) then
    begin
{$ifdef cgidebug}SendDebug('Sending session');{$endif}
    Result:=FSession
    end
  else
    begin
    If (FSession=Nil) then
      begin
{$ifdef cgidebug}SendDebug('Getting default session');{$endif}
      if (FSessionRequest=Nil) then
        Raise EFPHTTPError.Create(SErrNoSessionOutsideRequest);
      FSession:=SessionFactory.CreateSession(FSessionRequest);
      FSession.FreeNotification(Self);
      end;
    Result:=FSession
    end;
{$ifdef cgidebug}SendMethodExit('SessionHTTPModule.GetSession');{$endif}
end;

procedure TSessionHTTPModule.SetSession(const AValue: TCustomSession);

begin
  if FSession<>AValue then
    begin
    If Assigned(FSession) then
      FSession.RemoveFreeNotification(Self);
    FSession:=AValue;
    If Assigned(FSession) then
      FSession.FreeNotification(Self);
    end;
end;

procedure TSessionHTTPModule.CheckSession(ARequest : TRequest);

begin
{$ifdef cgidebug}SendMethodEnter('SessionHTTPModule('+Name+').CheckSession');{$endif}
  If CreateSession then
    begin
    If (FSession=Nil) then
      FSession:=SessionFactory.CreateSession(ARequest);
    if Assigned(FSession) then
      FSession.InitSession(ARequest,FOnNewSession,FOnSessionExpired);
    end;
{$ifdef cgidebug}SendMethodExit('SessionHTTPModule('+Name+').CheckSession');{$endif}
end;

procedure TSessionHTTPModule.InitSession(AResponse: TResponse);
begin
{$ifdef cgidebug}SendMethodEnter('SessionHTTPModule('+Name+').InitSession');{$endif}
  If CreateSession and Assigned(FSession) then
    FSession.InitResponse(AResponse);
{$ifdef cgidebug}SendMethodExit('SessionHTTPModule('+Name+').InitSession');{$endif}
end;

procedure TSessionHTTPModule.UpdateSession(AResponse: TResponse);
begin
  If CreateSession And Assigned(FSession) then
    FSession.UpdateResponse(AResponse);
end;

procedure TSessionHTTPModule.DoneSession;
begin
  // Session manager may or may not destroy the session.
  // Check if we actually have
  if Assigned(FSession) then
    SessionFactory.DoneSession(FSession);
  // In each case, our reference is no longer valid.
  FSession:=Nil;
end;

destructor TSessionHTTPModule.destroy;
begin
  // Prevent memory leaks.
  DoneSession;
  inherited destroy;
end;

procedure TSessionHTTPModule.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
{$ifdef cgidebug}SendMethodEnter('SessionHTTPModule('+Name+').Notification');{$endif}
  inherited Notification(AComponent, Operation);
  If (Operation=opRemove) then
    if (AComponent=FSession) Then
      FSession:=Nil;
{$ifdef cgidebug}SendMethodExit('SessionHTTPModule('+Name+').Notification');{$endif}
end;

procedure TSessionHTTPModule.HandleRequest(ARequest: TRequest;
  AResponse: TResponse);
begin
  FSessionRequest:=ARequest;
end;

Initialization
  ModuleFactory:=TModuleFactory.Create(TModuleItem);

Finalization
  FreeAndNil(ModuleFactory);
  FreeAndNil(GSM);
end.
