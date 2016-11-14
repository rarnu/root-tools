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
unit fphtml; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, htmlelements, htmlwriter, httpdefs, fphttp, DB, DOM, contnrs;

type
  THtmlEntities = (heHtml,heBody,heHead,heDiv,heParagraph);

const
  THtmlEntitiesClasses : array[THtmlEntities] of THTMLElementClass =
    (THTML_html, THTML_body, THTML_head, THTML_div, THTML_p);

type

  { TJavaScriptStack }
  TWebButtonType = (btOk, btCancel, btCustom);
  TWebButton = record
    ButtonType: TWebButtonType;
    Caption: String;
    OnClick: String;
  end;
  TWebButtons = array of TWebButton;

  TMessageBoxHandler = function(Sender: TObject; AText: String; Buttons: TWebButtons; Loaded: string = ''): string of object;
  TOnGetUrlProc = procedure(ParamNames, ParamValues, KeepParams: array of string; Action: string; var URL: string) of object;
  TWebController = class;
  THTMLContentProducer = class;

  TJavaType = (jtOther, jtClientSideEvent);

  TJavaScriptStack = class(TObject)
  private
    FJavaType: TJavaType;
    FMessageBoxHandler: TMessageBoxHandler;
    FScript: TStrings;
    FWebController: TWebController;
  protected
    function GetWebController: TWebController;
  public
    constructor Create(const AWebController: TWebController; const AJavaType: TJavaType); virtual;
    destructor Destroy; override;
    procedure AddScriptLine(ALine: String); virtual;
    procedure MessageBox(AText: String; Buttons: TWebButtons; Loaded: string = ''); virtual;
    procedure RedrawContentProducer(AContentProducer: THTMLContentProducer); virtual;
    procedure CallServerEvent(AHTMLContentProducer: THTMLContentProducer; AEvent: Integer; APostVariable: string = ''); virtual;
    procedure Clear; virtual;
    procedure Redirect(AUrl: string); virtual;
    function ScriptIsEmpty: Boolean; virtual;
    function GetScript: String; virtual;
    property WebController: TWebController read GetWebController;
    property JavaType: TJavaType read FJavaType;
  end;

  { TContainerStylesheet }

  TContainerStylesheet = class(TCollectionItem)
  private
    Fhref: string;
    Fmedia: string;
  published
    property href: string read Fhref write Fhref;
    property media: string read Fmedia write Fmedia;
  end;

  { TContainerStylesheets }

  TContainerStylesheets = class(TCollection)
  private
    function GetItem(Index: integer): TContainerStylesheet;
    procedure SetItem(Index: integer; const AValue: TContainerStylesheet);
  public
    function Add: TContainerStylesheet;
    property Items[Index: integer]: TContainerStylesheet read GetItem write SetItem;
  end;

  { TJavaVariable }

  TJavaVariable = class(TCollectionItem)
  private
    FBelongsTo: string;
    FGetValueFunc: string;
    FID: string;
    FIDSuffix: string;
    FName: string;
  public
    property BelongsTo: string read FBelongsTo write FBelongsTo;
    property GetValueFunc: string read FGetValueFunc write FGetValueFunc;
    property Name: string read FName write FName;
    property ID: string read FID write FID;
    property IDSuffix: string read FIDSuffix write FIDSuffix;
  end;

  { TJavaVariables }

  TJavaVariables = class(TCollection)
  private
    function GetItem(Index: integer): TJavaVariable;
    procedure SetItem(Index: integer; const AValue: TJavaVariable);
  public
    function Add: TJavaVariable;
    property Items[Index: integer]: TJavaVariable read GetItem write SetItem;
  end;


  { TWebController }

  TWebController = class(TComponent)
  private
    FAddRelURLPrefix: boolean;
    FBaseURL: string;
    FMessageBoxHandler: TMessageBoxHandler;
    FOnGetURL: TOnGetUrlProc;
    FScriptName: string;
    FScriptStack: TFPObjectList;
    FIterationIDs: array of string;
    FJavaVariables: TJavaVariables;
    procedure SetBaseURL(const AValue: string);
    procedure SetScriptName(const AValue: string);
  protected
    function GetJavaVariables: TJavaVariables;
    function GetJavaVariablesCount: integer;
    function GetScriptFileReferences: TStringList; virtual; abstract;
    function GetCurrentJavaScriptStack: TJavaScriptStack; virtual;
    function GetStyleSheetReferences: TContainerStylesheets; virtual; abstract;
    function GetScripts: TFPObjectList; virtual; abstract;
    function GetRequest: TRequest;
    property OnGetURL: TOnGetUrlProc read FOnGetURL write FOnGetURL;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddScriptFileReference(AScriptFile: String); virtual; abstract;
    procedure AddStylesheetReference(Ahref, Amedia: String); virtual; abstract;
    function CreateNewJavascriptStack(AJavaType: TJavaType): TJavaScriptStack; virtual; abstract;
    function InitializeJavaScriptStack(AJavaType: TJavaType): TJavaScriptStack;
    procedure FreeJavascriptStack; virtual;
    function HasJavascriptStack: boolean; virtual; abstract;
    function GetUrl(ParamNames, ParamValues, KeepParams: array of string; Action: string = ''): string; virtual; abstract;
    procedure InitializeAjaxRequest; virtual;
    procedure InitializeShowRequest; virtual;
    procedure CleanupShowRequest; virtual;
    procedure CleanupAfterRequest; virtual;
    procedure BeforeGenerateHead; virtual;
    function AddJavaVariable(AName, ABelongsTo, AGetValueFunc, AID, AIDSuffix: string): TJavaVariable;
    procedure BindJavascriptCallstackToElement(AComponent: TComponent; AnElement: THtmlCustomElement; AnEvent: string); virtual; abstract;
    function MessageBox(AText: String; Buttons: TWebButtons; ALoaded: string = ''): string; virtual;
    function DefaultMessageBoxHandler(Sender: TObject; AText: String; Buttons: TWebButtons;  ALoaded: string = ''): string; virtual; abstract;
    function CreateNewScript: TStringList; virtual; abstract;
    function AddrelativeLinkPrefix(AnURL: string): string;
    procedure FreeScript(var AScript: TStringList); virtual; abstract;
    procedure ShowRegisteredScript(ScriptID: integer); virtual; abstract;

    function IncrementIterationLevel: integer; virtual;
    function ResetIterationLevel: integer; virtual;
    procedure SetIterationIDSuffix(AIterationLevel: integer; IDSuffix: string); virtual;
    function GetIterationIDSuffix: string; virtual;
    procedure DecrementIterationLevel; virtual;

    property ScriptFileReferences: TStringList read GetScriptFileReferences;
    property StyleSheetReferences: TContainerStylesheets read GetStyleSheetReferences;
    property Scripts: TFPObjectList read GetScripts;
    property CurrentJavaScriptStack: TJavaScriptStack read GetCurrentJavaScriptStack;
    property MessageBoxHandler: TMessageBoxHandler read FMessageBoxHandler write FMessageBoxHandler;
  published
    property BaseURL: string read FBaseURL write SetBaseURL;
    property ScriptName: string read FScriptName write SetScriptName;
    property AddRelURLPrefix: boolean read FAddRelURLPrefix write FAddRelURLPrefix;
  end;

  { TAjaxResponse }

  TAjaxResponse= class(TObject)
  private
    FJavascriptCallStack: TJavaScriptStack;
    FResponse: TResponse;
    FSendXMLAnswer: boolean;
    FXMLAnswer: TXMLDocument;
    FRootNode: TDOMNode;
    FWebController: TWebController;
    function GetXMLAnswer: TXMLDocument;
  public
    constructor Create(AWebController: TWebController; AResponse: TResponse); virtual;
    destructor Destroy; override;
    procedure BindToResponse; virtual;
    procedure SetError(HelpContext: longint; ErrorMessage: string);
    procedure CancelXMLAnswer;
    property Response: TResponse read FResponse;
    property XMLAnswer: TXMLDocument read GetXMLAnswer;
    property SendXMLAnswer: boolean read FSendXMLAnswer;
    property JavascriptCallStack: TJavaScriptStack read FJavascriptCallStack;
  end;

  TCSAjaxEvent=procedure(Sender: TComponent; AJavascriptClass: TJavaScriptStack; var Handled: boolean) of object;
  THandleAjaxEvent = procedure(Sender: TObject; ARequest: TRequest; AnAjaxResponse: TAjaxResponse) of object;

  TEventRecord = record
    csCallback: TCSAjaxEvent;
    ServerEvent: THandleAjaxEvent;
    ServerEventID: integer;
    JavaEventName: string;
  end;
  TEventRecords = array of TEventRecord;

  TForeachContentProducerProc = procedure(const AContentProducer: THTMLContentProducer) of object;

  { IHTMLContentProducerContainer }

  IHTMLContentProducerContainer = interface
   ['{8B4D8AE0-4873-49BF-B677-D03C8A02CDA5}']
    procedure AddContentProducer(AContentProducer: THTMLContentProducer);
    procedure RemoveContentProducer(AContentProducer: THTMLContentProducer);
    function ExchangeContentProducers(Child1, Child2: THTMLContentProducer) : boolean;
    function MoveContentProducer(MoveElement, MoveBeforeElement: THTMLContentProducer) : boolean;
    procedure ForeachContentProducer(AForeachChildsProc: TForeachContentProducerProc; Recursive: boolean);

    function ProduceContent : string;
  end;

  { THTMLContentProducer }

  THTMLContentProducer = Class(THTTPContentProducer, IHTMLContentProducerContainer)
  private
    FDocument: THTMLDocument;
    FElement: THTMLCustomElement;
    FWriter: THTMLWriter;
    FIDSuffix: string;
    procedure SetDocument(const AValue: THTMLDocument);
    procedure SetWriter(const AValue: THTMLWriter);
  private
    // for streaming
    FChilds: TFPList; // list of THTMLContentProducer
    FParent: TComponent;
    function GetContentProducerList: TFPList;
    function GetContentProducers(Index: integer): THTMLContentProducer;
    procedure SetParent(const AValue: TComponent);
  Protected
    function CreateWriter (Doc : THTMLDocument) : THTMLWriter; virtual;
    function GetIdentification: string; virtual;
    function GetIDSuffix: string; virtual;
    procedure SetIDSuffix(const AValue: string); virtual;
  protected
    // Methods for streaming
    FAcceptChildsAtDesignTime: boolean;
    procedure SetParentComponent(Value: TComponent); override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    procedure DoBeforeGenerateContent(const AContentProducer: THTMLContentProducer);
    function GetEvents: TEventRecords; virtual;
    procedure AddEvent(var Events: TEventRecords; AServerEventID: integer; AServerEvent: THandleAjaxEvent; AJavaEventName: string; AcsCallBack: TCSAjaxEvent); virtual;
    procedure DoOnEventCS(AnEvent: TEventRecord; AJavascriptStack: TJavaScriptStack; var Handled: boolean); virtual;
    procedure SetupEvents(AHtmlElement: THtmlCustomElement); virtual;
    function GetWebPage: TDataModule;
    function GetWebController(const ExceptIfNotAvailable: boolean = true): TWebController;
    property ContentProducerList: TFPList read GetContentProducerList;
  public
    procedure BeforeGenerateContent; virtual;
    function WriteContent (aWriter : THTMLWriter) : THTMLCustomElement; virtual;
    Function ProduceContent : String; override; // Here to test the output. Replace to protected after tests
    function GetParentComponent: TComponent; override;
    property ParentElement : THTMLCustomElement read FElement write FElement;
    property Writer : THTMLWriter read FWriter write SetWriter;
    Property HTMLDocument : THTMLDocument read FDocument write SetDocument;
    Property IDSuffix : string read GetIDSuffix write SetIDSuffix;
  public
    // for streaming
    constructor Create(AOwner: TComponent); override;
    destructor destroy; override;
    function HasParent: Boolean; override;
    function ChildCount: integer;
    procedure CleanupAfterRequest; virtual;
    procedure AddContentProducer(AContentProducer: THTMLContentProducer);
    procedure RemoveContentProducer(AContentProducer: THTMLContentProducer);
    function ExchangeContentProducers(Child1, Child2: THTMLContentProducer) : boolean;
    function MoveContentProducer(MoveElement, MoveBeforeElement: THTMLContentProducer) : boolean;
    procedure HandleAjaxRequest(ARequest: TRequest; AnAjaxResponse: TAjaxResponse); virtual;
    procedure ForeachContentProducer(AForeachChildsProc: TForeachContentProducerProc; Recursive: boolean);
    property Identification: string read GetIdentification;
    property Childs[Index: integer]: THTMLContentProducer read GetContentProducers;
    property AcceptChildsAtDesignTime: boolean read FAcceptChildsAtDesignTime;
    property parent: TComponent read FParent write SetParent;
  end;
  THTMLContentProducerClass = class of THTMLContentProducer;


  TWriterElementEvent = procedure (Sender:THTMLContentProducer; aWriter : THTMLWriter; var anElement : THTMLCustomElement) of object;
  TAfterElementEvent = procedure (Sender:THTMLContentProducer; anElement : THTMLCustomElement) of object;
  TWriterEvent = procedure (Sender:THTMLContentProducer; aWriter : THTMLWriter) of object;
  TBooleanEvent = procedure (Sender:THTMLContentProducer; var flag : boolean) of object;

  { THTMLCustomEntityProducer }

  THTMLCustomEntityProducer = class (THTMLContentProducer)
  private
    FOnWriteEntity: TWriterEvent;
    FEntity: THtmlEntities;
  protected
    procedure DoWriteEntity (aWriter : THTMLWriter); virtual;
    Property OnWriteEntity : TWriterEvent read FOnWriteEntity write FOnWriteEntity;
    Property Entity : THtmlEntities read FEntity write FEntity default heHtml;
  public
    constructor Create(AOwner: TComponent); override;
    function WriteContent (aWriter : THTMLWriter) : THTMLCustomElement; override;
  end;

  { THTMLEntityContentProducer }

  THTMLEntityProducer = class (THTMLCustomEntityProducer)
  published
    Property OnWriteEntity;
    Property Entity;
  end;

  { THTMLCustomPageProducer }

  THTMLCustomPageProducer = class (THTMLCustomEntityProducer)
  private
    FHeaderProducer : THTMLContentProducer;
    FOnWriteHeader: TWriterEvent;
    FOnWriteVisualBody: TWriterEvent;
    FOnWriteVisualFooter: TWriterEvent;
    FOnWriteVisualHeader: TWriterEvent;
    FVisualHeaderProducer : THTMLContentProducer;
    FVisualBodyProducer : THTMLContentProducer;
    FVisualFooterProducer : THTMLContentProducer;
  protected
    procedure DoWriteEntity (aWriter : THTMLWriter); override;
    procedure DoWriteHeader (aWriter : THTMLWriter); virtual;
    procedure DoWriteVisualHeader (aWriter : THTMLWriter); virtual;
    procedure DoWriteVisualBody (aWriter : THTMLWriter); virtual;
    procedure DoWriteVisualFooter (aWriter : THTMLWriter); virtual;
    procedure BeforeGenerateContent; override;
    Property HeaderProducer : THTMLContentProducer read FHeaderProducer write FHeaderProducer;
    Property VisualHeaderProducer : THTMLContentProducer read FVisualHeaderProducer write FVisualHeaderProducer;
    Property VisualBodyProducer : THTMLContentProducer read FVisualBodyProducer write FVisualBodyProducer;
    Property VisualFooterProducer : THTMLContentProducer read FVisualFooterProducer write FVisualFooterProducer;
    Property OnWriteHeader : TWriterEvent read FOnWriteHeader write FOnWriteHeader;
    Property OnWriteVisualHeader : TWriterEvent read FOnWriteVisualHeader write FOnWriteVisualHeader;
    Property OnWriteVisualBody : TWriterEvent read FOnWriteVisualBody write FOnWriteVisualBody;
    Property OnWriteVisualFooter : TWriterEvent read FOnWriteVisualFooter write FOnWriteVisualFooter;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { THTMLPageProducer }

  THTMLPageProducer = class (THTMLCustomPageProducer)
  published
    property OnWriteHeader;
    property OnWriteVisualHeader;
    property OnWriteVisualBody;
    property OnWriteVisualFooter;
    Property HeaderProducer;
    Property VisualHeaderProducer;
    Property VisualBodyProducer;
    Property VisualFooterProducer;
  end;

  { THTMLCustomDatasetContentProducer }

  THTMLCustomDatasetContentProducer = class (THTMLContentProducer)
  private
    FDatasource: TDatasource;
    FOnChange: THandleAjaxEvent;
    FOnChangeCS: TCSAjaxEvent;
    FOnWriteFooter: TWriterEvent;
    FOnWriteHeader: TWriterElementEvent;
    FOnWriteRecord: TWriterEvent;
    function WriteHeader (aWriter : THTMLWriter) : THTMLCustomElement;
    procedure WriteFooter (aWriter : THTMLWriter);
    procedure WriteRecord (aWriter : THTMLWriter);
  protected
    procedure DoWriteHeader (aWriter : THTMLWriter; var el : THTMLCustomElement); virtual;
    procedure DoWriteFooter (aWriter : THTMLWriter); virtual;
    procedure DoWriteRecord (aWriter : THTMLWriter); virtual;
    function GetEvents: TEventRecords; override;
    procedure HandleAjaxRequest(ARequest: TRequest; AnAjaxResponse: TAjaxResponse); override;
  public
    function WriteContent (aWriter : THTMLWriter) : THTMLCustomElement; override;
    Property OnWriteHeader : TWriterElementEvent read FOnWriteHeader write FOnWriteHeader;
    Property OnWriteFooter : TWriterEvent read FOnWriteFooter write FOnWriteFooter;
    Property OnWriteRecord : TWriterEvent read FOnWriteRecord write FOnWriteRecord;
  published
    Property DataSource : TDataSource read FDataSource write FDataSource;
    property OnChangeCS: TCSAjaxEvent read FOnChangeCS write FOnChangeCS;
    property OnChange: THandleAjaxEvent read FOnChange write FOnChange;
  end;

  { THTMLDatasetContentProducer }

  THTMLDatasetContentProducer = class (THTMLCustomDatasetContentProducer)
  published
    Property OnWriteHeader;
    Property OnWriteFooter;
    Property OnWriteRecord;
  end;
  
  { THTMLSelectProducer }

  THTMLSelectProducer = class (THTMLContentProducer)
  private
    FControlName: string;
    FItems: TStrings;
    FjsOnChange: string;
    FPreSelected: string;
    FSize: integer;
    FUseValues: boolean;
    procedure SetItems(const AValue: TStrings);
  public
    constructor create (aOwner : TComponent); override;
    destructor destroy; override;
    function WriteContent (aWriter : THTMLWriter) : THTMLCustomElement; override;
  published
    property Items : TStrings read FItems write SetItems;
    property UseValues : boolean read FUseValues write FUseValues default false;
    property PreSelected : string read FPreSelected write FPreSelected;
    property Size : integer read FSize write FSize default 1;
    property ControlName : string read FControlName write FControlName;
    property jsOnChange: string read FjsOnChange write FjsOnChange;
  end;

  { THTMLDatasetSelectProducer }

  THTMLDatasetSelectProducer = class (THTMLCustomDatasetContentProducer)
  private
    FControlName: string;
    FIsPreSelected: TBooleanEvent;
    FItemField: string;
    FSize: integer;
    FValueField: string;
    FValue, FItem : TField;
    FPreSelected: string;
    FUseValues: boolean;
  protected
    procedure DoWriteHeader (aWriter : THTMLWriter; var el : THTMLCustomElement); override;
    procedure DoWriteFooter (aWriter : THTMLWriter); override;
    procedure DoWriteRecord (aWriter : THTMLWriter); override;
  public
    constructor create (aOwner : TComponent); override;
  published
    property UseValues : boolean read FUseValues write FUseValues default false;
    property PreSelected : string read FPreSelected write FPreSelected;
    property ItemField : string read FItemField write FItemField;
    property ValueField : string read FValueField write FValueField;
    property OnIsPreSelected : TBooleanEvent read FIsPreSelected write FIsPreSelected;
    property Size : integer read FSize write FSize;
    property ControlName : string read FControlName write FControlName;
    property OnWriteHeader;
  end;
  
  { THTMLDataModule }
  THTMLGetContentEvent = Procedure (Sender : TObject; ARequest : TRequest; HTMLPage : THTMLWriter; Var Handled : Boolean) of object;
  TCreateDocumentEvent = Procedure(Sender : TObject; var ADocument : THTMLDocument) of object;
  TCreateWriterEvent = Procedure(Sender : TObject; ADocument : THTMLDocument; Var AWriter : THTMLWriter) of object;

  { THTMLContentAction }

  THTMLContentAction = Class(TCustomWebAction)
  private
    FOnGetContent: THTMLGetContentEvent;
  Public
    Procedure HandleRequest(ARequest : TRequest; HTMLPage : THTMLWriter; Var Handled : Boolean);
  Published
    Property OnGetContent : THTMLGetContentEvent Read FOnGetContent Write FOnGetContent;
  end;
  
  { THTMLContentActions }

  THTMLContentActions = Class(TCustomWebActions)
    Procedure HandleRequest(ARequest : TRequest; HTMLPage : THTMLWriter; Var Handled : Boolean);
  end;

  { TCustomHTMLDataModule }

  { TCustomHTMLModule }

  TCustomHTMLModule = Class(TCustomHTTPModule)
  private
    FDocument : THTMLDocument;
    FActions: THTMLContentActions;
    FOnCreateDocument: TCreateDocumentEvent;
    FOnCreateWriter: TCreateWriterEvent;
    FOnGetContent: THTMLGetContentEvent;
    procedure SetActions(const AValue: THTMLContentActions);
  Protected
    Function CreateWriter(ADocument : THTMLDocument) : THTMLWriter;
    Function CreateDocument : THTMLDocument;
    Property OnGetContent : THTMLGetContentEvent Read FOnGetContent Write FOnGetContent;
    Property Actions : THTMLContentActions Read FActions Write SetActions;
    Property OnCreateDocument : TCreateDocumentEvent Read FOnCreateDocument Write FOnCreateDocument;
    Property OnCreateWriter : TCreateWriterEvent Read FOnCreateWriter Write FOnCreateWriter;
  Public
    Constructor Create(AOwner : TComponent);override;
    Procedure HandleRequest(ARequest : TRequest; AResponse : TResponse); override;
  end;
  
  TFPHTMLModule=Class(TCustomHTMLModule)
  Published
    Property OnGetContent;
    Property Actions;
    Property OnCreateDocument;
    Property OnCreateWriter;
  end;
  
  EHTMLError = Class(EHTTP);

const SimpleOkButton: array[0..0] of TWebButton = ((buttontype: btok;caption: 'Ok';onclick: ''));

const jseButtonClick = 1000;
      jseInputChange = 1001;
      jseFormReset   = 1002;
      jseFormSubmit  = 1003;

implementation
Uses
{$ifdef cgidebug}
  dbugintf
{$endif cgidebug}
  webpage, XMLWrite;

resourcestring
  SErrRequestNotHandled = 'Web request was not handled by actions.';
  SErrNoContentProduced = 'The content producer "%s" didn''t produce any content.';

{ TJavaVariables }

function TJavaVariables.GetItem(Index: integer): TJavaVariable;
begin
  result := TJavaVariable(Inherited GetItem(Index));
end;

procedure TJavaVariables.SetItem(Index: integer; const AValue: TJavaVariable);
begin
  inherited SetItem(Index, AValue);
end;

function TJavaVariables.Add: TJavaVariable;
begin
  result := inherited Add as TJavaVariable;
end;

{ TcontainerStylesheets }

function TcontainerStylesheets.GetItem(Index: integer): TContainerStylesheet;
begin
  result := TContainerStylesheet(Inherited GetItem(Index));
end;

procedure TcontainerStylesheets.SetItem(Index: integer; const AValue: TContainerStylesheet);
begin
  inherited SetItem(Index, AValue);
end;

function TcontainerStylesheets.Add: TContainerStylesheet;
begin
  result := inherited Add as TContainerStylesheet;
end;


{ TJavaScriptStack }

function TJavaScriptStack.GetWebController: TWebController;
begin
  result := FWebController;
end;

constructor TJavaScriptStack.Create(const AWebController: TWebController; const AJavaType: TJavaType);
begin
  FWebController := AWebController;
  FScript := TStringList.Create;
  FJavaType := AJavaType;
end;

destructor TJavaScriptStack.Destroy;
begin
  FScript.Free;
  inherited Destroy;
end;

procedure TJavaScriptStack.AddScriptLine(ALine: String);
begin
  FScript.Add(ALine);
end;

procedure TJavaScriptStack.MessageBox(AText: String; Buttons: TWebButtons; Loaded: string = '');
begin
  AddScriptLine(WebController.MessageBox(AText,Buttons,Loaded));
end;

procedure TJavaScriptStack.RedrawContentProducer(AContentProducer: THTMLContentProducer);
begin
  raise EHTMLError.Create('RedrawContentProducer not supported by current WebController');
end;

procedure TJavaScriptStack.CallServerEvent(AHTMLContentProducer: THTMLContentProducer; AEvent: Integer; APostVariable: string = '');
begin
  raise EHTMLError.Create('SendServerEvent not supported by current WebController');
end;

procedure TJavaScriptStack.Clear;
begin
  FScript.Clear;
end;

procedure TJavaScriptStack.Redirect(AUrl: string);
begin
  AddScriptLine('window.location = "'+AUrl+'";');
end;

function TJavaScriptStack.ScriptIsEmpty: Boolean;
begin
  result := FScript.Count=0;
end;

function TJavaScriptStack.GetScript: String;
begin
  result := FScript.Text;
end;


{ THTMLContentProducer }

procedure THTMLContentProducer.SetWriter(const AValue: THTMLWriter);
begin
  FWriter := AValue;
  if not assigned (FDocument) then
    FDocument := AValue.Document
  else if FDocument <> AValue.Document then
    AValue.document := FDocument;
end;

procedure THTMLContentProducer.SetDocument(const AValue: THTMLDocument);
begin
  FDocument := AValue;
  if assigned (FWriter) and (AValue <> FWriter.Document) then
    FWriter.Document := AValue;
end;

procedure THTMLContentProducer.SetParent(const AValue: TComponent);
begin
  if FParent=AValue then exit;
  if FParent<>nil then
    (FParent as IHTMLContentProducerContainer).RemoveContentProducer(Self);
  FParent:=AValue;
  if FParent<>nil then
    (FParent as IHTMLContentProducerContainer).AddContentProducer(Self);
end;

function THTMLContentProducer.GetContentProducers(Index: integer): THTMLContentProducer;
begin
  Result:=THTMLContentProducer(ContentProducerList[Index]);
end;

function THTMLContentProducer.GetIDSuffix: string;
begin
  result := FIDSuffix;
end;

procedure THTMLContentProducer.SetIDSuffix(const AValue: string);
begin
  FIDSuffix := AValue;
end;

function THTMLContentProducer.GetContentProducerList: TFPList;
begin
  if not assigned(FChilds) then
    fchilds := tfplist.Create;
  Result := FChilds;
end;

function THTMLContentProducer.GetIdentification: string;
begin
  result := '';
end;

function THTMLContentProducer.ProduceContent: String;
var WCreated, created : boolean;
    el : THtmlCustomElement;
begin
  created := not assigned (FDocument);
  if created then
    FDocument := THTMLDocument.Create;
  try
    WCreated := not assigned(FWriter);
    if WCreated then
      FWriter := CreateWriter (FDocument);
    try
      FWriter.CurrentElement := ParentElement;
      el := WriteContent (FWriter);
      if not assigned(el) then
        Raise EHTMLError.CreateFmt(SErrNoContentProduced,[Self.Name]);
      BeforeGenerateContent;
      ForeachContentProducer(@DoBeforeGenerateContent,True);
      result := el.asstring;
    finally
      if WCreated then
        FreeAndNil(FWriter);
    end;
  finally
    if created then
      FreeAndNil(FDocument);
  end;
end;

constructor THTMLContentProducer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAcceptChildsAtDesignTime:=True;
end;

destructor THTMLContentProducer.destroy;
begin
  Parent:=nil;
  while ChildCount>0 do Childs[ChildCount-1].Free;
  FreeAndNil(FChilds);
  inherited destroy;
end;

function THTMLContentProducer.GetEvents: TEventRecords;
begin
  result := nil;
end;

procedure THTMLContentProducer.AddEvent(var Events: TEventRecords;
  AServerEventID: integer; AServerEvent: THandleAjaxEvent; AJavaEventName: string;
  AcsCallBack: TCSAjaxEvent);
begin
  SetLength(Events,length(Events)+1);
  with Events[high(Events)] do
    begin
    ServerEvent:=AServerEvent;
    ServerEventID:=AServerEventID;
    JavaEventName:=AJavaEventName;
    csCallback:=AcsCallBack;
    end;
end;

procedure THTMLContentProducer.DoOnEventCS(AnEvent: TEventRecord; AJavascriptStack: TJavaScriptStack; var Handled: boolean);
begin
  if assigned(AnEvent.csCallback) then
    AnEvent.csCallback(self, AJavascriptStack, Handled);
end;

procedure THTMLContentProducer.SetupEvents(AHtmlElement: THtmlCustomElement);
var AJSClass: TJavaScriptStack;
    wc: TWebController;
    Handled: boolean;
    Events: TEventRecords;
    i: integer;
begin
  Events := GetEvents;
  if length(Events)>0 then
    begin
    wc := GetWebController(false);
    if assigned(wc) then
      begin
      AJSClass := wc.InitializeJavaScriptStack(jtClientSideEvent);
      try
        for i := 0 to high(Events) do
          begin
          Handled:=false;
          DoOnEventCS(events[i],AJSClass, Handled);
          if not handled and assigned(events[i].ServerEvent) then
            AJSClass.CallServerEvent(self,events[i].ServerEventID);
          wc.BindJavascriptCallstackToElement(Self, AHtmlElement,events[i].JavaEventName);
          AJSClass.clear;
          end;
      finally
        wc.FreeJavascriptStack;
      end;
      end
    else
      begin
      for i := 0 to high(Events) do if assigned(events[i].csCallback) or assigned(events[i].ServerEvent) then
        raise EHTMLError.Create('There is no webcontroller available, which is necessary to use events.');
      end;
    end;
end;

function THTMLContentProducer.GetWebPage: TDataModule;
var
  aowner: TComponent;
begin
  result := nil;
  aowner := Owner;
  while assigned(aowner) do
    begin
    if aowner.InheritsFrom(TWebPage) then
      begin
      result := TWebPage(aowner);
      break;
      end;
    aowner:=aowner.Owner;
    end;
end;

function THTMLContentProducer.GetWebController(const ExceptIfNotAvailable: boolean): TWebController;
var
  i : integer;
  wp: TWebPage;
begin
  result := nil;
  wp := TWebPage(GetWebPage);
  if assigned(wp) then
    begin
    if wp.HasWebController then
      begin
      result := wp.WebController;
      exit;
      end;
    end
  else if assigned(Owner) then //if (owner is TDataModule) then
    begin
    for i := 0 to owner.ComponentCount-1 do if owner.Components[i] is TWebController then
      begin
      result := TWebController(Owner.Components[i]);
      Exit;
      end;
    end;
  if ExceptIfNotAvailable then
    raise EHTMLError.Create('No webcontroller available');
end;

procedure THTMLContentProducer.BeforeGenerateContent;
begin
  // do nothing
end;

function THTMLContentProducer.WriteContent(aWriter: THTMLWriter): THTMLCustomElement;
var i: integer;
begin
  for i := 0 to ChildCount-1 do
    if Childs[i] is THTMLContentProducer then
      result := THTMLContentProducer(Childs[i]).WriteContent(aWriter);
end;

function THTMLContentProducer.ChildCount: integer;
begin
  if assigned(FChilds) then
    result := FChilds.Count
  else
    result := 0;
end;

procedure THTMLContentProducer.CleanupAfterRequest;
begin
  // Do Nothing
end;

procedure THTMLContentProducer.AddContentProducer(AContentProducer: THTMLContentProducer);
begin
  ContentProducerList.Add(AContentProducer);
end;

procedure THTMLContentProducer.RemoveContentProducer(AContentProducer: THTMLContentProducer);
begin
  ContentProducerList.Remove(AContentProducer);
end;

function THTMLContentProducer.ExchangeContentProducers(Child1, Child2: THTMLContentProducer): boolean;
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

function THTMLContentProducer.MoveContentProducer(MoveElement, MoveBeforeElement: THTMLContentProducer): boolean;
var ChildIndex1, ChildIndex2: integer;
begin
  result := false;
  ChildIndex1:=GetContentProducerList.IndexOf(MoveElement);
  if (ChildIndex1=-1) then
    Exit;
  ChildIndex2:=GetContentProducerList.IndexOf(MoveBeforeElement);
  if (ChildIndex2=-1) then
    Exit;
  if ChildIndex2>ChildIndex1 then dec(ChildIndex2);
  GetContentProducerList.Move(ChildIndex1,ChildIndex2);
  result := true;
end;

procedure THTMLContentProducer.HandleAjaxRequest(ARequest: TRequest; AnAjaxResponse: TAjaxResponse);
begin
  // Do nothing
end;

procedure THTMLContentProducer.ForeachContentProducer(AForeachChildsProc: TForeachContentProducerProc; Recursive: boolean);
var i : integer;
    tmpChild: THTMLContentProducer;
begin
  for i := 0 to ChildCount -1 do
    begin
    tmpChild := Childs[i];
    AForeachChildsProc(tmpChild);
    if recursive then
      tmpChild.ForeachContentProducer(AForeachChildsProc,Recursive);
    end;
end;

function THTMLContentProducer.CreateWriter (Doc : THTMLDocument): THTMLWriter;
begin
  FDocument := Doc;
  result := THTMLWriter.Create (Doc);
end;

procedure THTMLContentProducer.SetParentComponent(Value: TComponent);
begin
  if Supports(Value,IHTMLContentProducerContainer) then
    Parent:=Value;
end;

function THTMLContentProducer.HasParent: Boolean;
begin
  Result:=FParent<>nil;
end;

function THTMLContentProducer.GetParentComponent: TComponent;
begin
  Result:=TComponent(Parent);
end;

procedure THTMLContentProducer.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  i: Integer;
begin
  for i:=0 to ChildCount-1 do
    if Childs[i].Owner=Root then
      Proc(Childs[i]);
end;

procedure THTMLContentProducer.DoBeforeGenerateContent(const AContentProducer: THTMLContentProducer);
begin
  AContentProducer.BeforeGenerateContent;
end;

{ THTMLCustomDatasetContentProducer }

function THTMLCustomDatasetContentProducer.WriteHeader(aWriter: THTMLWriter): THTMLCustomElement;
var el : THTmlCustomElement;
begin
  el := nil;
  DoWriteHeader (aWriter, el);
  result := el;
end;

procedure THTMLCustomDatasetContentProducer.WriteFooter(aWriter: THTMLWriter);
begin
  DoWriteFooter (aWriter);
end;

procedure THTMLCustomDatasetContentProducer.WriteRecord(aWriter: THTMLWriter);
begin
  DoWriteRecord (aWriter);
end;

function THTMLCustomDatasetContentProducer.WriteContent(aWriter: THTMLWriter): THTMLCustomElement;
var opened : boolean;
begin
  if assigned (FDataSource) and assigned(datasource.dataset) then
    begin
    result := WriteHeader (aWriter);
    try
        with FDataSource.dataset do
          try
            opened := Active;
            if not opened then
              Open;
            first;
            while not eof do
              begin
              WriteRecord(aWriter);
              next;
              end;
          finally
            if not opened then
              close;
          end;
      SetupEvents(Result);
    finally
      WriteFooter (aWriter);
    end;
    end;
end;

procedure THTMLCustomDatasetContentProducer.DoWriteHeader(aWriter: THTMLWriter; var el : THTMLCustomElement);
begin
  if assigned (FOnWriteHeader) then
    FOnWriteHeader (self, aWriter, el);
end;

procedure THTMLCustomDatasetContentProducer.DoWriteFooter(aWriter: THTMLWriter);
begin
  if assigned (FOnWriteFooter) then
    FOnWriteFooter (self, aWriter);
end;

procedure THTMLCustomDatasetContentProducer.DoWriteRecord(aWriter: THTMLWriter);
begin
  if assigned (FOnWriteRecord) then
    FOnWriteRecord (self, aWriter);
end;

function THTMLCustomDatasetContentProducer.GetEvents: TEventRecords;
begin
  AddEvent(result,jseInputChange,OnChange,'onchange',OnChangeCS);
end;

procedure THTMLCustomDatasetContentProducer.HandleAjaxRequest(ARequest: TRequest;
  AnAjaxResponse: TAjaxResponse);
begin
  inherited HandleAjaxRequest(ARequest, AnAjaxResponse);
  case StrToIntDef(ARequest.QueryFields.Values['event'],-1) of
    jseInputChange : if assigned(OnChange) then OnChange(Self, ARequest, AnAjaxResponse);
  end;
end;

{ THTMLSelectProducer }

procedure THTMLSelectProducer.SetItems(const AValue: TStrings);
begin
  if FItems<>AValue then
    FItems.assign(AValue);
end;

function THTMLSelectProducer.WriteContent(aWriter: THTMLWriter): THTMLCustomElement;
begin
  result := aWriter.FormSelect(FControlName, FPreselected, FSize, FItems, FUseValues);
  THTML_select(result).onchange:=FjsOnChange;
end;

constructor THTMLSelectProducer.create(aOwner: TComponent);
begin
  inherited create (aOwner);
  FUseValues := False;
  FItems := TStringlist.Create;
  size := 1;
end;

destructor THTMLSelectProducer.destroy;
begin
  FItems.Free;
  inherited;
end;

{ THTMLDatasetSelectProducer }

procedure THTMLDatasetSelectProducer.DoWriteHeader (aWriter : THTMLWriter; var el : THTMLCustomElement);
var s : THTML_Select;
begin
  s := aWriter.StartSelect;
  s.size := IntToStr(FSize);
  s.name := FControlName;
  el := s;
  if FValueField <> '' then
    FValue := datasource.dataset.findfield (FValueField);
  if FItemField <> '' then
    FItem := DataSource.dataset.findfield (FItemField);
  inherited DoWriteHeader(aWriter, el);
end;

procedure THTMLDatasetSelectProducer.DoWriteFooter(aWriter: THTMLWriter);
begin
  inherited DoWriteFooter(aWriter);
  aWriter.EndSelect;
end;

procedure THTMLDatasetSelectProducer.DoWriteRecord(aWriter: THTMLWriter);
var sel : boolean;
begin
  if assigned (FItem) then
    with aWriter.Option(FItem.asstring) do
      begin
      if FUseValues then
        begin
        if assigned(FValue) then
          sel := (FValue.AsString = FPreSelected)
        end
      else if assigned(FItem) then
        sel := (FItem.AsString = FPreSelected);
      if assigned (FIsPreSelected) then
        FIsPreSelected (self, sel);
      selected := sel;
      if assigned (FValue) then
        Value := FValue.Asstring;
      end;
end;

constructor THTMLDatasetSelectProducer.create(aOwner: TComponent);
begin
  inherited create(aOwner);
  Size := 1;
  FUseValues := False;
end;

{ TCustomHTMLDataModule }

Function TCustomHTMLModule.CreateDocument : THTMLDocument;

begin
  Result:=Nil;
  If Assigned(FOnCreateDocument) then
    FOnCreateDocument(Self,Result);
  If (Result=Nil) then
    Result:=THTMLDocument.Create;
end;

constructor TCustomHTMLModule.Create(AOwner: TComponent);
begin
  FActions:=THTMLContentActions.Create(THTMLContentAction);
  inherited Create(AOwner);
end;

procedure TCustomHTMLModule.SetActions(const AValue: THTMLContentActions);
begin
  FActions.Assign(AValue);
end;

Function TCustomHTMLModule.CreateWriter(ADocument : THTMLDocument) : THTMLWriter;

begin
  Result:=Nil;
  If Assigned(FOnCreateWriter) then
    FOnCreateWriter(Self,ADocument,Result);
  if (Result=Nil) then
    Result:=THTMLWriter.Create(ADocument);
end;


procedure TCustomHTMLModule.HandleRequest(ARequest: TRequest; AResponse: TResponse);

Var
  FWriter : THTMLWriter;
  B : Boolean;
  M : TMemoryStream;
  
begin
  FDocument := CreateDocument;
  Try
    FWriter:=CreateWriter(FDocument);
    Try
      B:=False;
      If Assigned(OnGetContent) then
        OnGetContent(Self,ARequest,FWriter,B);
      If Not B then
        Raise EHTMLError.Create(SErrRequestNotHandled);
      If (AResponse.ContentStream=Nil) then
        begin
        M:=TMemoryStream.Create;
        AResponse.ContentStream:=M;
        end;
      FDocument.SaveToStream(AResponse.ContentStream);
    Finally
      FreeAndNil(FWriter);
    end;
  Finally
    FreeAndNil(FDocument);
  end;
end;

{ THTMLContentActions }

procedure THTMLContentActions.HandleRequest(ARequest: TRequest;
  HTMLPage: THTMLWriter; var Handled: Boolean);
  
Var
  A : TCustomWebAction;

begin
{$ifdef cgidebug}SendMethodEnter('HTMLContentWebActions.handlerequest');{$endif cgidebug}
  A:=GetRequestAction(ARequest);
  if Assigned(A) then
    (A as THTMLContentAction).HandleRequest(ARequest,HTMLPage,Handled);
{$ifdef cgidebug}SendMethodEnter('HTMLContentWebActions.handlerequest');{$endif cgidebug}
end;


{ THTMLContentAction }

procedure THTMLContentAction.HandleRequest(ARequest: TRequest;
  HTMLPage: THTMLWriter; var Handled: Boolean);
begin
  If Assigned(FOngetContent) then
    FOnGetContent(Self,ARequest,HTMLPage,Handled);
end;

{ THTMLCustomEntityProducer }

function THTMLCustomEntityProducer.WriteContent(aWriter: THTMLWriter
  ): THTMLCustomElement;
begin
  result := aWriter.StartElement(THtmlEntitiesClasses[FEntity]);
  DoWriteEntity(aWriter);
  inherited WriteContent(aWriter);
  aWriter.EndElement(THtmlEntitiesClasses[FEntity]);
end;

procedure THTMLCustomEntityProducer.DoWriteEntity(aWriter: THTMLWriter);
begin
  if assigned (FOnWriteEntity) then
    FOnWriteEntity (self, aWriter);
end;

constructor THTMLCustomEntityProducer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEntity := heHtml;
end;

{ THTMLCustomPageProducer }

procedure THTMLCustomPageProducer.DoWriteEntity(aWriter: THTMLWriter);
begin
  inherited DoWriteEntity(aWriter);
  DoWriteHeader(aWriter);
  aWriter.Startbody;
  DoWriteVisualHeader(aWriter);
  DoWriteVisualBody(aWriter);
  DoWriteVisualFooter(aWriter);
  awriter.Endbody;
end;

procedure THTMLCustomPageProducer.DoWriteHeader(aWriter: THTMLWriter);
begin
  if assigned(FOnWriteHeader) then
    FOnWriteHeader(self,aWriter);
  if assigned(FHeaderProducer) then
    aWriter.AddElement(FHeaderProducer.WriteContent(aWriter));
end;

procedure THTMLCustomPageProducer.DoWriteVisualHeader(aWriter: THTMLWriter);
begin
  if assigned(FOnWriteVisualHeader) then
    FOnWriteVisualHeader(self,aWriter);
  if assigned(FVisualHeaderProducer) then
    aWriter.AddElement(FVisualHeaderProducer.WriteContent(aWriter));
end;

procedure THTMLCustomPageProducer.DoWriteVisualBody(aWriter: THTMLWriter);
begin
  if assigned(FOnWriteVisualBody) then
    FOnWriteVisualBody(self,aWriter);
  if assigned(FVisualBodyProducer) then
    aWriter.AddElement(FVisualBodyProducer.WriteContent(aWriter));
end;

procedure THTMLCustomPageProducer.DoWriteVisualFooter(aWriter: THTMLWriter);
begin
  if assigned(FOnWriteVisualFooter) then
    FOnWriteVisualFooter(self,aWriter);
  if assigned(FVisualFooterProducer) then
    aWriter.AddElement(FVisualFooterProducer.WriteContent(aWriter));
end;

procedure THTMLCustomPageProducer.BeforeGenerateContent;
begin
  inherited BeforeGenerateContent;
  if assigned(FHeaderProducer) then
    FHeaderProducer.BeforeGenerateContent;
  if assigned(FVisualHeaderProducer) then
    FVisualHeaderProducer.BeforeGenerateContent;
  if assigned(FVisualBodyProducer) then
    FVisualBodyProducer.BeforeGenerateContent;
  if assigned(FVisualFooterProducer) then
    FVisualFooterProducer.BeforeGenerateContent;
end;

constructor THTMLCustomPageProducer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Entity := heHtml;
end;

{ TAjaxResponse }

function TAjaxResponse.GetXMLAnswer: TXMLDocument;
begin
  if not assigned(FXMLAnswer) then
    begin
    FXMLAnswer := TXMLDocument.create;
    FRootNode := FXMLAnswer.CreateElement('CallResponse');
    FXMLAnswer.Appendchild(FRootNode);
    end;
  result := FXMLAnswer;
end;

constructor TAjaxResponse.Create(AWebController: TWebController;
  AResponse: TResponse);
begin
  FSendXMLAnswer:=true;
  FResponse:=AResponse;
  FWebController := AWebController;
  FJavascriptCallStack:=FWebController.InitializeJavaScriptStack(jtOther);
end;

destructor TAjaxResponse.Destroy;
begin
  FXMLAnswer.Free;
  assert(FWebController.CurrentJavaScriptStack=FJavascriptCallStack);
  FWebController.FreeJavascriptStack;
  FJavascriptCallStack:=nil;
  inherited Destroy;
end;

procedure TAjaxResponse.BindToResponse;
var SubNode: TDOMNode;
begin
  if SendXMLAnswer then
    begin
    SubNode := XMLAnswer.CreateElement('ExecScript');
    FRootNode.Appendchild(SubNode);
    SubNode.Appendchild(XMLAnswer.CreateTextNode(FJavascriptCallStack.GetScript));

    Response.ContentStream := TMemoryStream.Create;
    Response.ContentType:='text/xml';
    writeXMLFile(XMLAnswer,Response.ContentStream);
    Response.ContentLength := Response.ContentStream.Size;
    end
end;

procedure TAjaxResponse.SetError(HelpContext: longint; ErrorMessage: string);
var SubNode: TDOMNode;
    ErrNode: TDOMNode;
begin
  ErrNode := XMLAnswer.CreateElement('Error');
  FRootNode.AppendChild(ErrNode);
  SubNode := XMLAnswer.CreateElement('HelpContext');
  SubNode.AppendChild(XMLAnswer.CreateTextNode(IntToStr(HelpContext)));
  ErrNode.AppendChild(SubNode);
  SubNode := XMLAnswer.CreateElement('Message');
  SubNode.AppendChild(XMLAnswer.CreateTextNode(ErrorMessage));
  ErrNode.AppendChild(SubNode);
end;

procedure TAjaxResponse.CancelXMLAnswer;
begin
  FSendXMLAnswer:=false;
end;

{ TWebController }

function TWebController.GetJavaVariables: TJavaVariables;
begin
  if not assigned(FJavaVariables) then
    FJavaVariables := TJavaVariables.Create(TJavaVariable);
  Result := FJavaVariables;
end;

function TWebController.GetJavaVariablesCount: integer;
begin
  if assigned(FJavaVariables) then
    result := FJavaVariables.Count
  else
    result := 0;
end;

procedure TWebController.SetBaseURL(const AValue: string);
begin
  if FBaseURL=AValue then exit;
  FBaseURL:=AValue;
end;

procedure TWebController.SetScriptName(const AValue: string);
begin
  if FScriptName=AValue then exit;
  FScriptName:=AValue;
end;

function TWebController.GetCurrentJavaScriptStack: TJavaScriptStack;
begin
  if FScriptStack.Count>0 then
    result := TJavaScriptStack(FScriptStack.Items[FScriptStack.Count-1])
  else
    result := nil;
end;

procedure TWebController.InitializeAjaxRequest;
begin
  // do nothing
end;

procedure TWebController.InitializeShowRequest;
begin
  // do nothing
end;

procedure TWebController.CleanupShowRequest;
begin
  // Do Nothing
end;

procedure TWebController.CleanupAfterRequest;
begin
  // Do Nothing
end;

procedure TWebController.BeforeGenerateHead;
begin
  // do nothing
end;

function TWebController.AddJavaVariable(AName, ABelongsTo, AGetValueFunc, AID, AIDSuffix: string): TJavaVariable;
begin
  result := GetJavaVariables.Add;
  result.BelongsTo := ABelongsTo;
  result.GetValueFunc := AGetValueFunc;
  result.Name := AName;
  result.IDSuffix := AIDSuffix;
  result.ID := AID;
end;

function TWebController.MessageBox(AText: String; Buttons: TWebButtons; ALoaded: string = ''): string;
begin
  if assigned(MessageBoxHandler) then
    result := MessageBoxHandler(self,AText,Buttons,ALoaded)
  else
    result := DefaultMessageBoxHandler(self,AText,Buttons,ALoaded);
end;

function TWebController.AddrelativeLinkPrefix(AnURL: string): string;
var
  i: Integer;
begin
  if FAddRelURLPrefix and (AnURL<>'') and (copy(AnURL,1,1)<>'/') and assigned(Owner) and (owner is TWebPage) and assigned(TWebPage(Owner).Request) then
    result := TWebPage(Owner).Request.LocalPathPrefix + AnURL
  else
    result := AnURL;
end;

function TWebController.IncrementIterationLevel: integer;
begin
  result := Length(FIterationIDs)+1;
  SetLength(FIterationIDs,Result);
end;

function TWebController.ResetIterationLevel: integer;
begin
  SetLength(FIterationIDs,0);
end;

procedure TWebController.SetIterationIDSuffix(AIterationLevel: integer; IDSuffix: string);
begin
  FIterationIDs[AIterationLevel-1]:=IDSuffix;
end;

function TWebController.GetIterationIDSuffix: string;
var
  i: integer;
begin
  result := '';
  for i := 0 to length(FIterationIDs)-1 do
    result := result + '_' + FIterationIDs[i];
end;

procedure TWebController.DecrementIterationLevel;
var
  i: integer;
begin
  i := length(FIterationIDs);
  if i=0 then
    raise EHTMLError.Create('DecrementIterationLevel can not be called more times then IncrementIterationLevel');
  SetLength(FIterationIDs,i-1);
end;

function TWebController.GetRequest: TRequest;
begin
  if assigned(Owner) and (owner is TWebPage) then
    result := TWebPage(Owner).Request
  else
    result := nil;
end;

constructor TWebController.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  { TODO : Do this prperly using a notification. And make the WebController property readonly }
  if owner is TWebPage then TWebPage(Owner).WebController := self;
  FScriptStack := TFPObjectList.Create(true);
end;

destructor TWebController.Destroy;
begin
  if (Owner is TWebPage) and (TWebPage(Owner).WebController=self) then
    TWebPage(Owner).WebController := nil;
  FScriptStack.Free;
  if assigned(FJavaVariables) then FJavaVariables.Free;
  inherited Destroy;
end;

function TWebController.InitializeJavaScriptStack(AJavaType: TJavaType): TJavaScriptStack;
begin
  result := CreateNewJavascriptStack(AJavaType);
  FScriptStack.Add(result);
end;

procedure TWebController.FreeJavascriptStack;
begin
  FScriptStack.Delete(FScriptStack.Count-1);
end;


end.

