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
unit fpWeb;

interface

uses
  Classes, SysUtils, httpdefs, fphttp, fptemplate;

Type

  { TFPWebAction }

  TFPWebAction = Class(TCustomWebAction)
  Private
    FOnrequest: TWebActionEvent;
    FContents : TStrings;
    FTemplate : TFPTemplate;
    function  GetContents: TStrings;
    procedure SetContents(const AValue: TStrings);
    Procedure SetTemplate(const AValue : TFPTemplate);
  Protected  
    Procedure DoHandleRequest(ARequest : TRequest; AResponse : TResponse; Var Handled : Boolean); override;
    Procedure DoGetContent(ARequest : TRequest; Content : TStream; Var Handled : Boolean); virtual;
    Procedure GetContent(ARequest : TRequest; Content : TStream; Var Handled : Boolean);
  Public
    Constructor create(ACollection : TCollection); override;
    Destructor destroy; override;
    Procedure Assign(Source : TPersistent); override;
  published
    Property Contents : TStrings Read GetContents Write SetContents;
    Property OnRequest: TWebActionEvent Read FOnrequest Write FOnrequest;
    Property Template : TFPTemplate Read FTemplate Write SetTemplate;
  end;  

  { TFPWebActions }

  TFPWebActions = Class(TCustomWebActions)
  private
    FCurrentAction : TCustomWebAction;
  protected
    Procedure HandleRequest(ARequest : TRequest; AResponse : TResponse; Var Handled : Boolean); virtual;
    Procedure GetContent(ARequest : TRequest; Content : TStream; Var Handled : Boolean); virtual;
  Public
    Property ActionVar;
    Property CurrentAction: TCustomWebAction read FCurrentAction;
  end;

  { TTemplateVar }


  TTemplateVar = Class(TCollectionItem)
  Private
    FName: String;
    FValue: String;
  Public
    Procedure Assign(Source : TPersistent); override;
    Function GetDisplayName : String; override;
  Published
    Property Name : String Read FName Write FName;
    Property Value : String Read FValue Write FValue;
  end;

  { TTemplateVars }

  TTemplateVars = Class(TCollection)
  Private
    function GetVar(I : Integer): TTemplateVar;
    procedure Setvar(I : Integer; const AValue: TTemplateVar);
  Public
    Function IndexOfVar(AName : String) : Integer;
    Function VarByName(AName : String) : TTemplateVar;
    Function FindVar(AName : String) : TTemplateVar;
    Property Variables[I : Integer] : TTemplateVar Read GetVar Write Setvar; default;
  end;

  TContentEvent = Procedure (Sender : TObject; Content : TStream) of object;

  { TCustomFPWebModule }

  TCustomFPWebModule = Class(TSessionHTTPModule)
  private
    FActions: TFPWebActions;
    FAfterResponse: TResponseEvent;
    FBeforeRequest: TRequestEvent;
    FOnGetParam: TGetParamEvent;
    FOnRequest: TWebActionEvent;
    FRequest: TRequest;
    FResponse: TResponse;
    FTemplate: TFPTemplate;
    FTemplateVars : TTemplateVars;
    function GetActionVar: String;
    function GetDefActionWhenUnknown: Boolean;
    function GetOnGetAction: TGetActionEvent;
    procedure SetActions(const AValue: TFPWebActions);
    procedure SetActionVar(const AValue: String);
    procedure SetDefActionWhenUnknown(const AValue: Boolean);
    procedure SetOnGetAction(const AValue: TGetActionEvent);
    procedure SetTemplate(const AValue: TFPTemplate);
  Protected
    Function HandleActions(ARequest : TRequest): Boolean; virtual;
    procedure DoOnRequest(ARequest: TRequest; AResponse: TResponse; var AHandled: Boolean); virtual;
    Procedure DoBeforeRequest(ARequest : TRequest); virtual;
    Procedure DoAfterResponse(AResponse : TResponse); virtual;
    Procedure GetParam(Const ParamName : String; Out Value : String); virtual; // Called by template
    Procedure GetTemplateContent(ARequest : TRequest; AResponse : TResponse); virtual;
    function GetContent: String;virtual;
  Public
    Constructor CreateNew(AOwner : TComponent; CreateMode : Integer); override;
    Destructor Destroy; override;
    Procedure HandleRequest(ARequest : TRequest; AResponse : TResponse); override;
    Property Actions : TFPWebActions Read FActions Write SetActions;
    Property ActionVar : String Read GetActionVar Write SetActionVar;
    Property BeforeRequest : TRequestEvent Read FBeforeRequest Write FBeforeRequest;
    Property OnRequest : TWebActionEvent Read FOnRequest Write FOnRequest;
    Property AfterResponse : TResponseEvent Read FAfterResponse Write FAfterResponse;
    Property OnGetAction : TGetActionEvent Read GetOnGetAction Write SetOnGetAction;
    Property DefActionWhenUnknown : Boolean read GetDefActionWhenUnknown write SetDefActionWhenUnknown default true;
    Property ModuleTemplate : TFPTemplate Read FTemplate Write SetTemplate;
    Property OnGetParam : TGetParamEvent Read FOnGetParam Write FOnGetParam;
    Property OnTemplateContent : TGetParamEvent Read FOnGetParam Write FOnGetParam;
    Property Request: TRequest Read FRequest;
    Property Response: TResponse Read FResponse;
  end;
  
  { TFPWebModule }

  TFPWebModule = Class(TCustomFPWebModule)
  Published
    Property Actions;
    Property ActionVar;
    Property BeforeRequest;
    Property OnRequest;
    Property AfterResponse;
    Property OnGetAction;
    Property DefActionWhenUnknown;
    Property CreateSession;
    Property Session;
    property Kind;
    Property OnNewSession;
    Property OnSessionExpired;
    Property AfterInitModule;
  end;

  EFPWebError = Class(EHTTP);

resourcestring
  SErrInvalidVar        = 'Invalid template variable name : "%s"';
  SErrInvalidWebAction  = 'Invalid action for "%s".';
  SErrNoContentProduced = 'No template content was produced.';

implementation

{$ifdef cgidebug}
uses dbugintf;
{$endif cgidebug}


{ TFPWebAction }

procedure TFPWebAction.GetContent(ARequest: TRequest; Content: TStream; Var Handled : Boolean);

begin
  DoGetContent(ARequest, Content, Handled);
end;

procedure TFPWebAction.Assign(Source: TPersistent);

Var
  A : TFPWebAction;

begin
  If (Source is TFPWebAction) then
    begin
    A:=TFPWebAction(Source);
    Name:=A.Name;
    AfterResponse:=A.AfterResponse;
    BeforeRequest:=A.BeforeRequest;
    Default:=A.default;
    Contents:=A.FContents;
    ContentProducer:=A.ContentProducer;
    OnRequest:=A.OnRequest;
    FTemplate.Assign(A.Template);
    end
  else
    inherited Assign(Source);
end;

constructor TFPWebAction.create(ACollection: TCollection);
begin
  inherited create(ACollection);
  FTemplate:=TFPtemplate.Create;
end;

destructor TFPWebAction.destroy;
begin
  FreeandNil(FContents);
  FreeAndNil(FTemplate);
  inherited destroy;
end;

function TFPWebAction.GetContents: TStrings;
begin
  If Not Assigned(FContents) then
    FContents:=TStringList.Create;
  Result:=FContents;
end;

procedure TFPWebAction.SetContents(const AValue: TStrings);
begin
  if AValue = nil then
    FreeAndNil(FContents)
  else
    Contents.Assign(AValue);
end;

procedure TFPWebAction.SetTemplate(const AValue: TFPTemplate);
begin
  FTemplate.Assign(AValue);
end;


procedure TFPWebAction.DoHandleRequest(ARequest: TRequest; AResponse: TResponse; Var Handled : Boolean);

begin
{$ifdef cgidebug}
  SendMethodEnter('TFPWebAction('+Name+').Dohandlerequest');
  If Handled then
    SendDebug('Handled !!')
  else
    SendDebug('Not yet handled.');
{$endif cgidebug}
  If Assigned(FOnRequest) then
    begin
{$ifdef cgidebug}
    SendDebug('Executing user action');
{$endif cgidebug}
    FOnrequest(Self,Arequest,AResponse,Handled);
    end;
  If Not Handled then
    begin
{$ifdef cgidebug}
    SendDebug('Executing inherited');
{$endif cgidebug}
    Inherited DoHandleRequest(ARequest,AResponse,Handled);
    If not Handled then
      begin
      Handled := (FContents <> nil) and (FContents.Count > 0);
      if Handled then
        AResponse.Contents.AddStrings(FContents);
      end;
    end;
{$ifdef cgidebug}
  SendMethodExit('TFPWebAction('+Name+').Dohandlerequest');
{$endif cgidebug}
end;

procedure TFPWebAction.DoGetContent(ARequest: TRequest; Content: TStream; Var Handled : Boolean);

  //isolate string references in a subprocedure to avoid implicit exceptions in main procedure
  procedure CopyContent;
  var
    ContentStr: String;
  begin
    if FContents <> nil then
    begin
      ContentStr := FContents.Text;
      If ContentStr<>'' then
        Content.Write(ContentStr[1],Length(ContentStr));
    end;
  end;

begin
  If Assigned(ContentProducer) then
    ContentProducer.GetContent(ARequest,Content,Handled)
  else
    CopyContent;
end;


{ TFPWebTemplate }
Type
  TFPWebTemplate = Class(TFPTemplate)
  Private
    FOwner: TCustomFPWebModule;
    FRequest : TRequest;
  Public
    Constructor Create(AOwner :TCustomFPWebModule);
    Procedure GetParam(Sender : TObject; Const ParamName : String; Out AValue : String);override;
    Property Owner : TCustomFPWebModule Read FOwner;
    Property Request : TRequest Read FRequest Write FRequest;
  end;

constructor TFPWebTemplate.Create(AOwner: TCustomFPWebModule);
begin
  Inherited create;
  FOwner:=AOwner;
end;

procedure TFPWebTemplate.GetParam(Sender: TObject; const ParamName: String;
  out AValue: String);
begin
  FOwner.GetParam(ParamName, AValue);
end;

{ TFPWebModule }

function TCustomFPWebModule.GetActionVar: String;
begin
  Result:=FActions.ActionVar;
end;

function TCustomFPWebModule.GetDefActionWhenUnknown: Boolean;
begin
  Result:=FActions.DefActionWhenUnknown;
end;

function TCustomFPWebModule.GetOnGetAction: TGetActionEvent;
begin
  Result:=FActions.OnGetAction;
end;


procedure TCustomFPWebModule.SetActions(const AValue: TFPWebActions);
begin
  if (FActions<>AValue) then
    FActions.Assign(AValue);
end;

procedure TCustomFPWebModule.SetActionVar(const AValue: String);
begin
  FActions.ActionVar:=AValue;
end;

procedure TCustomFPWebModule.SetDefActionWhenUnknown(const AValue: Boolean);
begin
  FActions.DefActionWhenUnknown:=AValue;
end;

procedure TCustomFPWebModule.SetOnGetAction(const AValue: TGetActionEvent);
begin
  FActions.OnGetAction:=AValue;
end;


procedure TCustomFPWebModule.SetTemplate(const AValue: TFPTemplate);
begin
  if FTemplate<>AValue then
    FTemplate.Assign(AValue);
end;

function TCustomFPWebModule.HandleActions(ARequest: TRequest): Boolean;
begin
  Result:=True;
end;

procedure TCustomFPWebModule.DoBeforeRequest(ARequest : TRequest);
begin
  If Assigned(FBeforeRequest) then
    FBeforeRequest(Self,ARequest);
end;

procedure TCustomFPWebModule.DoAfterResponse(AResponse : TResponse);
begin
  If Assigned(FAfterResponse) then
    FAfterResponse(Self,AResponse);
end;

procedure TCustomFPWebModule.GetParam(const ParamName: String; out Value: String);
  
Var
  T : TTemplateVar;
  
begin
  If (0=CompareText(ParamName,'CONTENT')) then
    Value:=GetContent
  else
    begin
    T:=FTemplateVars.FindVar(ParamName);
    If (T<>Nil) then
      Value:=T.Value
    else
      If Assigned(FOnGetParam) then
        FOngetParam(Self,ParamName,Value);
    end;
end;

procedure TCustomFPWebModule.GetTemplateContent(ARequest: TRequest;
  AResponse: TResponse);
  
begin
  TFPWebTemplate(FTemplate).Request:=ARequest;
  AResponse.Content:=FTemplate.GetContent;
end;

function TCustomFPWebModule.GetContent: String;

Var
  S : TStringStream;
  B : Boolean;
  
begin
  S:=TStringStream.Create('');
  Try
    B:=False;
    FActions.GetContent(TFPWebTemplate(FTemplate).Request,S,B);
    If Not B then
      Raise EFPWebError.Create(SErrNoContentProduced);
    Result:=S.DataString;
  finally
    S.Free;
  end;
end;

constructor TCustomFPWebModule.CreateNew(AOwner: TComponent; CreateMode : Integer);
begin
  inherited;
  FActions:=TFPWebActions.Create(TFPWebAction);
  FTemplate:=TFPWebTemplate.Create(Self);
  FTemplateVars:=TTemplateVars.Create(TTemplateVar);
end;

destructor TCustomFPWebModule.Destroy;
begin
  FreeAndNil(FTemplateVars);
  FreeAndNil(FTemplate);
  FreeAndNil(FActions);
  inherited Destroy;
end;


procedure TCustomFPWebModule.DoOnRequest(ARequest: TRequest; AResponse: TResponse; Var AHandled : Boolean);

begin
  If Assigned(FOnRequest) then
    FOnRequest(Self,ARequest,AResponse,AHandled);
end;

procedure TCustomFPWebModule.HandleRequest(ARequest: TRequest; AResponse: TResponse);

Var
  B : Boolean;

begin
{$ifdef cgidebug}
  SendMethodEnter('WebModule('+Name+').handlerequest');
{$endif cgidebug}
  FRequest := ARequest; //So everything in the web module can access the current request variables
  FResponse := AResponse;//So everything in the web module can access the current response variables
  CheckSession(ARequest);
  DoBeforeRequest(ARequest);
  B:=False;
  InitSession(AResponse);
  DoOnRequest(ARequest,AResponse,B);
  If B then
    begin
    if not AResponse.ContentSent then
      AResponse.SendContent;
    end
  else
    if FTemplate.HasContent then
      GetTemplateContent(ARequest,AResponse)
    else if HandleActions(ARequest) then
      begin
      Actions.HandleRequest(ARequest,AResponse,B);
      FTemplate.Template := '';//if apache mod, then need to clear for next call because it is a webmodule global property,
      FTemplate.FileName := '';//so following calls are OK and the above FTemplate.HasContent is not becoming True
      If Not B then
        Raise EFPWebError.Create(SErrRequestNotHandled);
      end;
  DoAfterResponse(AResponse);
  UpdateSession(AResponse);
  FRequest := Nil;
  FResponse := Nil;
  // Clean up session for the case the webmodule is used again
  DoneSession;
{$ifdef cgidebug}
  SendMethodExit('WebModule('+Name+').handlerequest');
{$endif cgidebug}
end;

{ TTemplateVar }

procedure TTemplateVar.Assign(Source: TPersistent);
begin
  if Source is TTemplateVar then
    With Source as TTemplateVar do
      begin
      Self.Name:=Name;
      Self.Value:=Value;
      end
  else
    inherited Assign(Source);
end;

function TTemplateVar.GetDisplayName: String;
begin
  Result:=FName;
end;

{ TTemplateVars }

function TTemplateVars.GetVar(I : Integer): TTemplateVar;
begin
  Result:=TTemplateVar(Items[I])
end;

procedure TTemplateVars.Setvar(I : Integer; const AValue: TTemplateVar);
begin
  Items[i]:=AValue;
end;

function TTemplateVars.IndexOfVar(AName: String): Integer;

begin
  Result:=Count-1;
  While (Result>=0) and (CompareText(AName,GetVar(Result).Name)<>0) do
    Dec(Result);
end;

function TTemplateVars.VarByName(AName: String): TTemplateVar;
begin
  Result:=FindVar(AName);
  If (Result=Nil) then
    Raise EFPWebError.CreateFmt(SErrInvalidVar,[AName]);
end;

function TTemplateVars.FindVar(AName: String): TTemplateVar;

Var
  I : Integer;

begin
  I:=IndexOfVar(AName);
  If (I=-1) then
    Result:=Nil
  else
    Result:=GetVar(I);
end;

{ TFPWebActions }

procedure TFPWebActions.HandleRequest(ARequest: TRequest; AResponse: TResponse; Var Handled : Boolean);

Var
  A : TCustomWebAction;

begin
{$ifdef cgidebug}SendMethodEnter('FPWebActions.handlerequest');{$endif cgidebug}
  A:=GetRequestAction(ARequest);
  if Assigned(A) then
    begin
    FCurrentAction := A;
    (A as TFPWebAction).HandleRequest(ARequest,AResponse,Handled);
    end;
{$ifdef cgidebug}SendMethodExit('FPWebActions.handlerequest');{$endif cgidebug}
end;

procedure TFPWebActions.GetContent(ARequest: TRequest; Content: TStream;
  var Handled: Boolean);

Var
  A : TCustomWebAction;

begin
{$ifdef cgidebug}SendMethodEnter('WebActions.GetContent');{$endif cgidebug}
  A:=GetRequestAction(ARequest);
  If A is TFPWebAction then
   TFPWebAction(A).GetContent(ARequest,Content,Handled)
  else
    Raise EFPWebError.CreateFmt(SErrInvalidWebAction,[A.ClassName]);
{$ifdef cgidebug}SendMethodExit('WebActions.GetContent');{$endif cgidebug}
end;

end.

