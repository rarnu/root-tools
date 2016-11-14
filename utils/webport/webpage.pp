unit WebPage;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fphtml, htmlelements, htmlwriter, HTTPDefs, fpweb, contnrs, dom;

type
  TRequestResponseEvent = procedure(Sender: TObject; ARequest: TRequest; AResponse: TResponse) of object;
  TRequestEvent = procedure(Sender: TObject; ARequest: TRequest) of object;
  THandleAjaxRequest = procedure(Sender: TObject; ARequest: TRequest; AnAjaxResponse: TAjaxResponse; var handled: boolean) of object;
  TAjaxRequestResponseEvent = procedure(Sender: TObject; ARequest: TRequest; AResponse: TAjaxResponse) of object;

type

  { IWebPageDesigner }

  IWebPageDesigner = interface(IUnknown)
  ['{25629DEA-79D5-4165-A0A3-BE6E2BA74442}']
    procedure Invalidate;
  end;

  { IHTMLDesignable }

  IHTMLDesignable = interface(IUnknown)
  ['{C75546D6-9C93-49F0-809F-D29C52CD306D}']
    function GetDesigner: IWebPageDesigner;
    procedure SetDesigner(const AValue: IWebPageDesigner);
    property Designer: IWebPageDesigner read GetDesigner write SetDesigner;
  end;

  IHTMLIterationGroup = interface(IUnknown)
  ['{95575CB6-7D96-4F72-AF72-D2EAF0BECE71}']
    procedure SetIDSuffix(const AHTMLContentProducer: THTMLContentProducer);
    procedure SetAjaxIterationID(AValue: String);
  end;


  { TStandardWebController }

  TStandardWebController = class(TWebController)
  private
    FScriptFileReferences: TStringList;
    FScripts: TFPObjectList;
    FStyleSheetReferences: TContainerStylesheets;
  protected
    function GetScriptFileReferences: TStringList; override;
    function GetScripts: TFPObjectList; override;
    function GetStyleSheetReferences: TContainerStylesheets; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function CreateNewJavascriptStack(AJavaType: TJavaType): TJavaScriptStack; override;
    function GetUrl(ParamNames, ParamValues, KeepParams: array of string; Action: string = ''): string; override;
    procedure BindJavascriptCallstackToElement(AComponent: TComponent; AnElement: THtmlCustomElement; AnEvent: string); override;
    procedure AddScriptFileReference(AScriptFile: String); override;
    procedure AddStylesheetReference(Ahref, Amedia: String); override;
    function DefaultMessageBoxHandler(Sender: TObject; AText: String; Buttons: TWebButtons; ALoaded: string = ''): string; override;
    function CreateNewScript: TStringList; override;
    procedure ShowRegisteredScript(ScriptID: integer); override;
    procedure FreeScript(var AScript: TStringList); override;
  published
    property OnGetURL;
  end;

  { TWebPage }

  TWebPage = class(TDataModule, IHTMLContentProducerContainer, IHTMLDesignable)
  private
    FAfterAjaxRequest: TAjaxRequestResponseEvent;
    FBaseURL: string;
    FBeforeRequest: TRequestEvent;
    FBeforeShowPage: TRequestEvent;
    FDesigner: IWebPageDesigner;
    FOnAjaxRequest: THandleAjaxRequest;
    FRequest: TRequest;
    FWebController: TWebController;
    FWebModule: TFPWebModule;
    FContentProducers: TFPList; // list of THTMLContentProducer
    function GetContentProducer(Index: integer): THTMLContentProducer;
    function GetContentProducerList: TFPList;
    function GetContentProducers(Index: integer): THTMLContentProducer;
    function GetDesigner: IWebPageDesigner;
    function GetHasWebController: boolean;
    function GetWebController: TWebController;
    procedure SetDesigner(const AValue: IWebPageDesigner);
  protected
    procedure DoAfterAjaxRequest(ARequest: TRequest; AnAjaxResponse: TAjaxResponse); virtual;
    procedure DoHandleAjaxRequest(ARequest: TRequest; AnAjaxResponse: TAjaxResponse; var Handled: boolean); virtual;
    procedure DoBeforeRequest(ARequest: TRequest); virtual;
    procedure DoBeforeShowPage(ARequest: TRequest); virtual;
    procedure DoCleanupAfterRequest(const AContentProducer: THTMLContentProducer);
    procedure SetRequest(ARequest: TRequest); virtual;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    property ContentProducerList: TFPList read GetContentProducerList;
  public
    destructor Destroy; override;
    function ContentProducerCount: integer;

    function ProduceContent : string;
    procedure AddContentProducer(AContentProducer: THTMLContentProducer);
    procedure RemoveContentProducer(AContentProducer: THTMLContentProducer);
    function ExchangeContentProducers(Child1, Child2: THTMLContentProducer) : boolean;
    function MoveContentProducer(MoveElement, MoveBeforeElement: THTMLContentProducer) : boolean;
    procedure ForeachContentProducer(AForeachChildsProc: TForeachContentProducerProc; Recursive: boolean);
    function IsAjaxCall: boolean; virtual;

    procedure HandlePage(ARequest: TRequest; AResponse: TResponse; AWriter: THTMLwriter; AWebModule: TFPWebModule = nil); virtual;
    procedure DoBeforeGenerateXML; virtual;
    procedure CleanupAfterRequest; virtual;
    property Designer: IWebPageDesigner read GetDesigner write SetDesigner;
    property Request: TRequest read FRequest;
    property ContentProducers[Index: integer]: THTMLContentProducer read GetContentProducer;
    property HasWebController: boolean read GetHasWebController;
    property WebController: TWebController read GetWebController write FWebController;
    property WebModule: TFPWebModule read FWebModule;
  published
    property BeforeRequest: TRequestEvent read FBeforeRequest write FBeforeRequest;
    property BeforeShowPage: TRequestEvent read FBeforeShowPage write FBeforeShowPage;
    property AfterAjaxRequest: TAjaxRequestResponseEvent read FAfterAjaxRequest write FAfterAjaxRequest;
    property OnAjaxRequest: THandleAjaxRequest read FOnAjaxRequest write FOnAjaxRequest;
    property BaseURL: string read FBaseURL write FBaseURL;
  end;

  function RegisterScript(AScript: string) : integer;

implementation

uses typinfo, strutils;

var RegisteredScriptList : TStrings;

function RegisterScript(AScript: string) : integer;
begin
  if not Assigned(RegisteredScriptList) then
    begin
    RegisteredScriptList := TStringList.Create;
    end;
  result := RegisteredScriptList.Add(AScript);
end;

{ TWebPage }

function TWebPage.ProduceContent: string;
var i : integer;
begin
  result := '';
  for i := 0 to ContentProducerCount-1 do
    result := result + THTMLContentProducer(ContentProducers[i]).ProduceContent;
end;

procedure TWebPage.AddContentProducer(AContentProducer: THTMLContentProducer);
begin
  ContentProducerList.Add(AContentProducer);
end;

procedure TWebPage.RemoveContentProducer(AContentProducer: THTMLContentProducer);
begin
  ContentProducerList.Remove(AContentProducer);
end;

function TWebPage.ExchangeContentProducers(Child1, Child2: THTMLContentProducer): boolean;
var ChildIndex1, ChildIndex2: integer;
begin
  result := false;
  ChildIndex1:=GetContentProducerList.IndexOf(Child1);
  if (ChildIndex1=-1) then
    Exit;
  ChildIndex2:=GetContentProducerList.IndexOf(Child2);
  if (ChildIndex2=-1) then
    Exit;
  GetContentProducerList.Exchange(ChildIndex1,ChildIndex2);
  result := true;
end;

function TWebPage.MoveContentProducer(MoveElement, MoveBeforeElement: THTMLContentProducer): boolean;
var ChildIndex1, ChildIndex2: integer;
begin
  result := false;
  ChildIndex1:=GetContentProducerList.IndexOf(MoveElement);
  if (ChildIndex1=-1) then
    Exit;
  ChildIndex2:=GetContentProducerList.IndexOf(MoveBeforeElement);
  if (ChildIndex2=-1) then
    Exit;
  GetContentProducerList.Move(ChildIndex1,ChildIndex2);
  result := true;
end;

procedure TWebPage.ForeachContentProducer(AForeachChildsProc: TForeachContentProducerProc; Recursive: boolean);
var i : integer;
    tmpChild: THTMLContentProducer;
begin
  for i := 0 to ContentProducerCount -1 do
    begin
    tmpChild := ContentProducers[i];
    AForeachChildsProc(tmpChild);
    if recursive then
      tmpChild.ForeachContentProducer(AForeachChildsProc,Recursive);
    end;
end;

procedure TWebPage.HandlePage(ARequest: TRequest; AResponse: TResponse; AWriter: THTMLwriter; AWebModule: TFPWebModule=nil);
var Handled: boolean;
    CompName: string;
    AComponent: TComponent;
    AnAjaxResponse: TAjaxResponse;
    i: integer;
    ASuffixID: string;
    AIterationGroup: IHTMLIterationGroup;
    AIterComp: TComponent;
    wc: TWebController;
    Iterationlevel: integer;

  procedure SetIdSuffixes(AComp: THTMLContentProducer);
  var
    i: integer;
    s: string;
  begin
    if assigned(AComp.parent) and (acomp.parent is THTMLContentProducer) then
      SetIdSuffixes(THTMLContentProducer(AComp.parent));
    if supports(AComp,IHTMLIterationGroup,AIterationGroup) then
      begin
        if assigned(FWebController) then
          begin
          iterationlevel := FWebController.IncrementIterationLevel;
          assert(length(ASuffixID)>0);
          i := PosEx('_',ASuffixID,2);
          if i > 0 then
            s := copy(ASuffixID,2,i-2)
          else
            s := copy(ASuffixID,2,length(ASuffixID)-1);

          acomp.IDSuffix := s;
          AIterationGroup.SetAjaxIterationID(s);
          FWebController.SetIterationIDSuffix(iterationlevel,s);
          acomp.ForeachContentProducer(@AIterationGroup.SetIDSuffix,true);
          ASuffixID := copy(ASuffixID,i,length(ASuffixID)-i+1);
          end;
      end;
  end;
begin
  SetRequest(ARequest);
  FWebModule := AWebModule;
  try
    try
      DoBeforeRequest(ARequest);
      if IsAjaxCall then
        begin
        AnAjaxResponse := TAjaxResponse.Create(GetWebController, AResponse);
        try
          try
            if HasWebController then
              WebController.InitializeAjaxRequest;
            Handled := false;
            DoHandleAjaxRequest(ARequest, AnAjaxResponse, Handled);
            if not Handled then
              begin
              CompName := Request.QueryFields.Values['AjaxID'];
              if CompName='' then CompName := Request.GetNextPathInfo;

              i := pos('$',CompName);
              AComponent:=self;
              while (i > 0) and (assigned(AComponent)) do
                begin
                AComponent := AComponent.FindComponent(copy(CompName,1,i-1));
                CompName := copy(compname,i+1,length(compname)-i);
                i := pos('$',CompName);
                end;
              if assigned(AComponent) then
                AComponent := AComponent.FindComponent(CompName);

              if assigned(AComponent) and (AComponent is THTMLContentProducer) then
                begin
                // Handle the SuffixID, search for iteration-groups and set their iteration-id-values
                ASuffixID := ARequest.QueryFields.Values['IterationID'];
                if ASuffixID<>'' then
                  begin
                  SetIdSuffixes(THTMLContentProducer(AComponent));
                  webcontroller.ResetIterationLevel;
                  end;
                THTMLContentProducer(AComponent).HandleAjaxRequest(ARequest, AnAjaxResponse);
                end;
              end;
            DoAfterAjaxRequest(ARequest, AnAjaxResponse);
          except on E: Exception do
            AnAjaxResponse.SetError(e.HelpContext, e.Message);
          end;
          AnAjaxResponse.BindToResponse;
        finally
          AnAjaxResponse.Free;
        end;
        end
      else
        begin
        if HasWebController then
          WebController.InitializeShowRequest;
        DoBeforeShowPage(ARequest);
        AResponse.Content := ProduceContent;
        if HasWebController then
          WebController.CleanupShowRequest;
        end;
    finally
      CleanupAfterRequest;
    end;
  finally
    SetRequest(nil);
    AWebModule := nil;
  end;
end;

procedure TWebPage.DoBeforeGenerateXML;
begin
  // Do Nothing
end;

procedure TWebPage.CleanupAfterRequest;
begin
  ForeachContentProducer(@DoCleanupAfterRequest, True);
  if HasWebController then
    WebController.CleanupAfterRequest;
end;

procedure TWebPage.DoCleanupAfterRequest(const AContentProducer: THTMLContentProducer);
begin
  AContentProducer.CleanupAfterRequest;
end;

procedure TWebPage.SetRequest(ARequest: TRequest);
begin
  FRequest := ARequest;
end;

procedure TWebPage.GetChildren(Proc: TGetChildProc; Root: TComponent);
var i : integer;
begin
  inherited GetChildren(Proc, Root);
  if (Root=Self) then
    for I:=0 to ContentProducerCount-1 do
      Proc(ContentProducers[i]);
end;

destructor TWebPage.Destroy;
begin
  inherited Destroy;
  if assigned(FContentProducers) then
    FreeAndNil(FContentProducers);
end;

function TWebPage.ContentProducerCount: integer;
begin
  if assigned(FContentProducers) then
    result := FContentProducers.Count
  else
    result := 0;
end;

function TWebPage.GetContentProducers(Index: integer): THTMLContentProducer;
begin
  Result:=THTMLContentProducer(ContentProducerList[Index]);
end;

function TWebPage.GetDesigner: IWebPageDesigner;
begin
  result := FDesigner;
end;

function TWebPage.GetHasWebController: boolean;
begin
  result := assigned(FWebController);
end;

function TWebPage.GetWebController: TWebController;
begin
  if not assigned(FWebController) then
    raise EHTTP.create('No webcontroller available');
  result := FWebController;
end;

procedure TWebPage.SetDesigner(const AValue: IWebPageDesigner);
begin
  FDesigner := AValue;
end;

function TWebPage.GetContentProducerList: TFPList;
begin
  if not assigned(FContentProducers) then
    FContentProducers := tfplist.Create;
  Result := FContentProducers;
end;

function TWebPage.GetContentProducer(Index: integer): THTMLContentProducer;
begin
  Result := THTMLContentProducer(ContentProducerList[Index]);
end;

procedure TWebPage.DoAfterAjaxRequest(ARequest: TRequest; AnAjaxResponse: TAjaxResponse);
begin
  if assigned(AfterAjaxRequest) then
    AfterAjaxRequest(Self,ARequest,AnAjaxResponse);
end;

procedure TWebPage.DoHandleAjaxRequest(ARequest: TRequest; AnAjaxResponse: TAjaxResponse; var Handled: boolean);
begin
  if assigned(OnAjaxRequest) then
    OnAjaxRequest(Self,ARequest,AnAjaxResponse, Handled);
end;

procedure TWebPage.DoBeforeRequest(ARequest: TRequest);
begin
  if assigned(BeforeRequest) then
    BeforeRequest(Self,ARequest);
end;

procedure TWebPage.DoBeforeShowPage(ARequest: TRequest);
begin
  if assigned(BeforeShowPage) then
    BeforeShowPage(Self,ARequest);
end;

function TWebPage.IsAjaxCall: boolean;
var s : string;
begin
  if assigned(request) then
    begin
    s := Request.HTTPXRequestedWith;
    result := sametext(s,'XmlHttpRequest');
    end
  else
    result := false;
end;

{ TStandardWebController }

function TStandardWebController.GetScriptFileReferences: TStringList;
begin
  Result:=FScriptFileReferences;
end;

function TStandardWebController.GetScripts: TFPObjectList;
begin
  if not assigned(FScripts) then
    begin
    FScripts:=TFPObjectList.Create;
    FScripts.OwnsObjects:=true;
    end;
  Result:=FScripts;
end;

function TStandardWebController.GetStyleSheetReferences: TContainerStylesheets;
begin
  Result:=FStyleSheetReferences;
end;

function TStandardWebController.CreateNewScript: TStringList;
begin
  Result:=TStringList.Create;
  GetScripts.Add(result);
end;

procedure TStandardWebController.ShowRegisteredScript(ScriptID: integer);
var
  i: Integer;
  s: string;
begin
  s := '// ' + inttostr(ScriptID);
  for i := 0 to GetScripts.Count -1 do
    if tstrings(GetScripts.Items[i]).Strings[0]=s then
      Exit;
  with CreateNewScript do
    begin
    Append(s);
    Append(RegisteredScriptList.Strings[ScriptID]);
    end;
end;

procedure TStandardWebController.FreeScript(var AScript: TStringList);
begin
  with GetScripts do
    GetScripts.Delete(IndexOf(AScript));
  AScript := nil;
end;

function TStandardWebController.DefaultMessageBoxHandler(Sender: TObject;
  AText: String; Buttons: TWebButtons; ALoaded: string = ''): string;
var i : integer;
    HasCancel: boolean;
    OnOk: string;
    OnCancel: string;
begin
  HasCancel:=false;
  OnOk:='';
  OnCancel:='';
  for i := low(Buttons) to High(Buttons) do
    begin
    if Buttons[i].ButtonType=btOk then
      OnOk := Buttons[i].OnClick;
    if Buttons[i].ButtonType=btCancel then
      begin
      HasCancel := True;
      OnCancel := Buttons[i].OnClick;
      end;
    end;

  if HasCancel then
    result := 'if (confirm('''+AText+''')==true) {'+OnOk+'} else {'+OnCancel+'}'
  else
    result := 'alert('''+AText+''');'+OnOk;
end;

constructor TStandardWebController.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FStyleSheetReferences := TContainerStylesheets.Create(TContainerStylesheet);
  FScriptFileReferences := TStringList.Create;
  // For some reason the Duplicates property does not work when sorted is true,
  // But we don't want a sorted list so do a manual check in AddScriptFileReference
  //FScriptFileReferences.Sorted:=true;
  FScriptFileReferences.Duplicates:=dupIgnore;
end;

destructor TStandardWebController.Destroy;
begin
  FScriptFileReferences.Free;
  FScripts.Free;
  FStyleSheetReferences.Free;
  inherited Destroy;
end;

function TStandardWebController.CreateNewJavascriptStack(AJavaType: TJavaType): TJavaScriptStack;
begin
  Result:=TJavaScriptStack.Create(self, AJavaType);
end;

function TStandardWebController.GetUrl(ParamNames, ParamValues,
  KeepParams: array of string; Action: string): string;

var qs,p : String;
    i,j  : integer;
    found: boolean;
    FancyTitle: boolean;
    ConnectChar: char;
    CGIScriptName: string;
    ActionVar: string;
    ARequest: TRequest;
    WebMod: TFPWebModule;

begin
  FancyTitle:=false;
  qs := '';
  result := Action;
  ARequest := GetRequest;
  ActionVar := '';
  if assigned(owner) then
    begin
    if (owner is TWebPage) then
      WebMod := TWebPage(Owner).WebModule
    else if (owner is TFPWebModule) then
      WebMod := TFPWebModule(Owner);

    if assigned(WebMod) then
      begin
      ActionVar := WebMod.ActionVar;
      if (action = '') and assigned(WebMod.Actions) and assigned(WebMod.Actions.CurrentAction) then
        result := WebMod.Actions.CurrentAction.Name;
      end;
    end;
  if ActionVar='' then FancyTitle:=true;
  if Assigned(ARequest) then
    begin
    if  (high(KeepParams)>=0) and (KeepParams[0]='*') then
      begin
      for i := 0 to ARequest.QueryFields.Count-1 do
        begin
        p := ARequest.QueryFields.Names[i];
        found := False;
        for j := 0 to high(ParamNames) do if sametext(ParamNames[j],p) then
          begin
          found := True;
          break;
          end;
        if not FancyTitle and SameText(ActionVar,p) then
          found := true;
        if not found then
          qs := qs + p + '=' + ARequest.QueryFields.ValueFromIndex[i] + '&';
        end;
      end
    else for i := 0 to high(KeepParams) do
      begin
      p := ARequest.QueryFields.Values[KeepParams[i]];
      if p <> '' then
        qs := qs + KeepParams[i] + '=' + p + '&';
      end;
    end;
  for i := 0 to high(ParamNames) do
    qs := qs + ParamNames[i] + '=' + ParamValues[i] + '&';

  ConnectChar:='?';
  if ScriptName='' then CGIScriptName:='.'
  else
    begin
    CGIScriptName:=ScriptName;
    if pos('?',ScriptName)>0 then ConnectChar := '&';
    end;
  if FancyTitle then // use ? or /
    result := CGIScriptName + '/' + Result
  else
    begin
    result := CGIScriptName + ConnectChar +ActionVar+'=' + Result;
    ConnectChar:='&';
    end;

  p := copy(qs,1,length(qs)-1);
  if p <> '' then
    result := result + ConnectChar + p;
  if assigned(OnGetURL) then
    OnGetURL(ParamNames, ParamValues, KeepParams, Action, Result);
end;

procedure TStandardWebController.BindJavascriptCallstackToElement(AComponent: TComponent; AnElement: THtmlCustomElement; AnEvent: string);
begin
  if AnEvent='onclick' then
    (AnElement as THTMLAttrsElement).onclick:=CurrentJavaScriptStack.GetScript
  else if AnEvent='onchange' then
    if AnElement is THTML_input then (AnElement as THTML_input).onchange:=CurrentJavaScriptStack.GetScript;
end;

procedure TStandardWebController.AddScriptFileReference(AScriptFile: String);
begin
  if FScriptFileReferences.IndexOf(AScriptFile)=-1 then
    FScriptFileReferences.Add(AScriptFile);
end;

procedure TStandardWebController.AddStylesheetReference(Ahref, Amedia: String);
begin
  with FStyleSheetReferences.Add do
    begin
    href:=Ahref;
    media:=Amedia;
    end;
end;

initialization
  RegisteredScriptList := nil;
finalization
  if assigned(RegisteredScriptList) then
    RegisteredScriptList.Free;
end.

