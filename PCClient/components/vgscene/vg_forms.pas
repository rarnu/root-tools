unit vg_forms;

{$I vg_define.inc}

{$MINENUMSIZE 4}
{$H+}
{.$DEFINE DARWINBUFFER}
{.$DEFINE UPDATERECT}
{.$DEFINE BOUNDS}
{.$DEFINE DRAWFOCUSED}

interface

uses
  {$IFNDEF FPC}
  Windows,
  {$ENDIF}
  TypInfo, Math, Classes, SysUtils, vg_scene;

type                                                              

  TvgCustomForm = class;

  TvgTimerProc = procedure of object;

  TvgTimer = class (TvgObject)
  private
    FInterval: Cardinal;
    FTimerHandle: cardinal;
    FOnTimer: TNotifyEvent;
    FEnabled: Boolean;
    procedure Timer;
  protected
    procedure SetEnabled(Value: Boolean); virtual;
    procedure SetInterval(Value: Cardinal); virtual;
    procedure SetOnTimer(Value: TNotifyEvent); virtual;
    procedure DoOnTimer; virtual;
    procedure UpdateTimer; virtual;
    procedure KillTimer; virtual;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property Interval: Cardinal read FInterval write SetInterval default 1000;
    property OnTimer: TNotifyEvent read FOnTimer write SetOnTimer;
  end;

  TTimer = class(TvgTimer)
  end;

{ Abstract platform class }

  { TvgPlatform }

  TvgPlatform = class(TComponent)
  private
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { Application }
    procedure Run; virtual; abstract;
    procedure Terminate; virtual; abstract;
    function HandleMessage: boolean; virtual; abstract;
    procedure WaitMessage; virtual; abstract;
    { Timer }
    function CreateTimer(Interval: integer; TimerFunc: TvgTimerProc): THandle; virtual; abstract;
    function DestroyTimer(Timer: THandle): boolean; virtual; abstract;
    function GetTick: single; virtual; abstract;
    { Window }
    function CreateWindow(AForm: TvgCustomForm): THandle; virtual; abstract;
    procedure DestroyWindow(AForm: TvgCustomForm); virtual; abstract;
    procedure ReleaseWindow(AForm: TvgCustomForm); virtual; abstract;
    procedure ShowWindow(AForm: TvgCustomForm); virtual; abstract;
    procedure HideWindow(AForm: TvgCustomForm); virtual; abstract;
    function ShowWindowModal(AForm: TvgCustomForm): TModalResult; virtual; abstract;
    procedure InvalidateWindowRect(AForm: TvgCustomForm; R: TvgRect); virtual; abstract;
    procedure SetWindowRect(AForm: TvgCustomForm; ARect: TvgRect); virtual; abstract;
    function GetWindowRect(AForm: TvgCustomForm): TvgRect; virtual; abstract;
    function GetClientSize(AForm: TvgCustomForm): TvgPoint; virtual; abstract;
    procedure SetWindowCaption(AForm: TvgCustomForm; ACaption: WideString); virtual; abstract;
    procedure SetCapture(AForm: TvgCustomForm); virtual; abstract;
    procedure ReleaseCapture(AForm: TvgCustomForm); virtual; abstract;
    function ClientToScreen(AForm: TvgCustomForm; const Point: TvgPoint): TvgPoint; virtual; abstract;
    function ScreenToClient(AForm: TvgCustomForm; const Point: TvgPoint): TvgPoint; virtual; abstract;
    { Drag and Drop }
    procedure BeginDragDrop(AForm: TvgCustomForm; const Data: TvgDragObject; ABitmap: TvgBitmap); virtual; abstract;
    { Clipboard }
    procedure SetClipboard(Value: Variant); virtual; abstract;
    function GetClipboard: Variant; virtual; abstract;
    { Mouse }
    function GetMousePos: TvgPoint; virtual; abstract;
    { Screen }
    function GetScreenSize: TvgPoint; virtual; abstract;
    { International }
    function GetCurrentLangID: string; virtual; abstract;
    { Dialogs }
    function DialogOpenFiles(var FileName: WideString; AInitDir: WideString; AllowMulti: boolean): boolean; virtual; abstract;
    { Keyboard }
    function ShowVirtualKeyboard(AControl: TvgObject): boolean; virtual;
    function HideVirtualKeyboard: boolean; virtual;
  end;
  TvgPlatformClass = class of TvgPlatform;

{ Forms }

  TCloseEvent = procedure(Sender: TObject; var Action: TCloseAction) of object;
  TCloseQueryEvent = procedure(Sender: TObject;
    var CanClose: Boolean) of object;

  TBorderIcon = (biSystemMenu, biMinimize, biMaximize, biHelp);
  TBorderIcons = set of TBorderIcon;
  TvgCustomFormBorderStyle = (bsNone, bsSingle, bsSizeable, bsToolWindow, bsSizeToolWin);

  TvgCustomFormState = (
    fsRecreating,
    fsModal
  );
  TvgCustomFormStates = set of TvgCustomFormState;

  TvgStartupLocation = (
    vgLocationManual,
    vgLocationCenter
  );

  { Only for correct loading }

  TFontPitch = (fpDefault, fpVariable, fpFixed);
  TFontStyle = (fsBold, fsItalic, fsUnderline, fsStrikeOut);
  TFontStyles = set of TFontStyle;
  TFont = class(TPersistent)
  private
    FSize: Integer;
    FName: string;
    FPitch: TFontPitch;
    FStyle: TFontStyles;
    FHeight: Integer;
    procedure ReadIdent(Reader: TReader);
  protected
    procedure DefineProperties(Filer: TFiler); override;
  public
  published
    property Height: Integer read FHeight write FHeight;
    property Name: string read FName write FName;
    property Pitch: TFontPitch read FPitch write FPitch;
    property Size: Integer read FSize write FSize;
    property Style: TFontStyles read FStyle write FStyle;
  end;

  { }

  TvgCustomForm = class(TvgObject)
  private
    FCaption: WideString;
    FLeft: integer;
    FTop: integer;
    FOnClose: TCloseEvent;
    FOnCloseQuery: TCloseQueryEvent;
    FTransparency: boolean;
    FHandle: cardinal;
    FContext: cardinal;
    FBorderStyle: TvgCustomFormBorderStyle;
    FBorderIcons: TBorderIcons;
    FVisible: boolean;
    FTopMost: boolean;
    FOnActivate: TNotifyEvent;
    FOnDeactivate: TNotifyEvent;
    FShowActivated: boolean;
    FModalResult: TModalResult;
    FFormState: TvgCustomFormStates;
    FStaysOpen: boolean;
    FIsActive: boolean;
    FOnCreate: TNotifyEvent;
    FOnDestroy: TNotifyEvent;
    FStartupLocation: TvgStartupLocation;
    FVCLFont: TFont;
    FOnResize: TNotifyEvent;
    procedure SetLeft(const Value: integer);
    procedure SetTop(const Value: integer);
    procedure SetHeight(const Value: integer);
    procedure SetWidth(const Value: integer);
    procedure SetCaption(const Value: WideString);
    function GetClientHeight: integer;
    function GetClientWidth: integer;
    procedure SetTransparency(const Value: boolean);
    procedure SetBorderStyle(const Value: TvgCustomFormBorderStyle);
    procedure SetBorderIcons(const Value: TBorderIcons);
    procedure SetVisible(const Value: boolean);
    procedure SetTopMost(const Value: boolean);
    procedure SetClientHeight(const Value: integer);
    procedure SetClientWidth(const Value: integer);
    procedure ReadColor(Reader: TReader);
    procedure ReadBool(Reader: TReader);
    procedure ReadInt(Reader: TReader);
  protected
    FCanvas: TvgCanvas;
    FHeight: integer;
    FWidth: integer;
    procedure InvalidateRect(R: TvgRect);
    procedure Recreate;
    procedure MouseCapture;
    procedure ReleaseCapture;
    procedure SetIsActive(const Value: boolean); virtual;
    procedure DefineProperties(Filer: TFiler); override;
  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateNew(AOwner: TComponent; Dummy: Integer = 0); virtual;
    destructor Destroy; override;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    { called from Platform }
    procedure PaintRects(UpdateRects: array of TvgRect); virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: single); virtual;
    procedure MouseMove(Shift: TShiftState; X, Y: single); virtual;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: single); virtual;
    procedure MouseWheel(Shift: TShiftState; WheelDelta: integer; var Handled: boolean); virtual;
    procedure MouseLeave; virtual;
    procedure KeyDown(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState); virtual;
    procedure KeyUp(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState); virtual;
    procedure Activate;
    procedure Deactivate;
    procedure DragEnter(const Data: TvgDragObject; const Point: TvgPoint); virtual;
    procedure DragOver(const Data: TvgDragObject; const Point: TvgPoint; var Accept: Boolean); virtual;
    procedure DragDrop(const Data: TvgDragObject; const Point: TvgPoint); virtual;
    procedure DragLeave; virtual;
    { settings }
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: integer); virtual;
    function ClientToScreen(const Point: TvgPoint): TvgPoint;
    function ScreenToClient(const Point: TvgPoint): TvgPoint;
    function CloseQuery: Boolean;
    procedure Release;
    procedure Close;
    procedure Show;
    procedure Hide;
    function ShowModal: TModalResult;
    procedure CloseModal;
    property Canvas: TvgCanvas read FCanvas;
    property Handle: cardinal read FHandle write FHandle;
    property Context: cardinal read FContext write FContext;
    property ModalResult: TModalResult read FModalResult write FModalResult;
    property FormState: TvgCustomFormStates read FFormState;
  published
    property IsActive: boolean read FIsActive write SetIsActive;
    property Caption: WideString read FCaption write SetCaption;
    property BorderStyle: TvgCustomFormBorderStyle read FBorderStyle write SetBorderStyle default bsSizeable;
    property BorderIcons: TBorderIcons read FBorderIcons write SetBorderIcons
      default [biSystemMenu, biMinimize, biMaximize];
    property ClientHeight: integer read GetClientHeight write SetClientHeight stored false;
    property ClientWidth: integer read GetClientWidth write SetClientWidth stored false;
    property Left: integer read FLeft write SetLeft;
    property Top: integer read FTop write SetTop;
    property Width: integer read FWidth write SetWidth;
    property Height: integer read FHeight write SetHeight;
    property ShowActivated: boolean read FShowActivated write FShowActivated default true;
    property StaysOpen: boolean read FStaysOpen write FStaysOpen default true;
    property StartupLocation: TvgStartupLocation read FStartupLocation write FStartupLocation default vgLocationManual;
    property Transparency: boolean read FTransparency write SetTransparency;
    property TopMost: boolean read FTopMost write SetTopMost default false;
    property Visible: boolean read FVisible write SetVisible;
    property OnCreate: TNotifyEvent read FOnCreate write FOnCreate;
    property OnDestroy: TNotifyEvent read FOnDestroy write FOnDestroy;
    property OnClose: TCloseEvent read FOnClose write FOnClose;
    property OnCloseQuery: TCloseQueryEvent read FOnCloseQuery write FOnCloseQuery;
    property OnActivate: TNotifyEvent read FOnActivate write FOnActivate;
    property OnDeactivate: TNotifyEvent read FOnDeactivate write FOnDeactivate;
    property OnResize: TNotifyEvent read FOnResize write FOnResize;
    { VCL Compatible }
    property Font: TFont read FVCLFont stored false;
  end;

  TCustomForm = TvgCustomForm;

  { TvgForm }

  TvgForm = class(TvgCustomForm, IvgScene)
  private
    FDisableUpdate: boolean;
    FDesignRoot, FSelected, FCaptured, FHovered, FFocused: TvgVisualObject;
    FSelection: array of TvgObject;
    FDesignPlaceObject: TvgVisualObject;
    FDesignGridLines: array of TvgVisualObject;
    FDesignChangeSelection: TNotifyEvent;
    FUnsnapMousePos, FMousePos, FDownPos: TvgPoint;
    FMoving, FLeftTop, FRightTop, FLeftBottom, FRightBottom, FTop, FBottom, FLeft, FRight, FRotate: boolean;
    FLeftTopHot, FRightTopHot, FLeftBottomHot, FRightBottomHot, FTopHot, FBottomHot, FLeftHot, FRightHot, FRotateHot: boolean;
    FResizeSize, FResizePos, FResizeStartPos, FDownSize: TvgPoint;
    FDragging, FResizing: boolean;
    FDesignTime: boolean;
    FFill: TvgBrush;
    FSnapToGrid: boolean;
    FSnapToLines: boolean;
    FSnapGridShow: boolean;
    FSnapGridSize: single;
    FInsertObject: string;
    FAlignRoot: boolean;
    FDesignPopupEnabled: boolean;
    FPopupPos: TvgPoint;
    FDrawing: boolean;
    FStyle: TvgResources;
    FShowTimer: TvgTimer;
    FLoadCursor: TCursor;
    FActiveControl: TvgControl;
    FTarget: TvgVisualObject;
    FShowUpdateRects: boolean;
    procedure SetActiveControl(AControl: TvgControl);
    procedure DoShowTimer(Sender: TObject);
    procedure SetFill(const Value: TvgBrush);
    procedure FillChanged(Sender: TObject);
    procedure SetSnapGridShow(const Value: boolean);
    procedure AddUpdateRectsFromGridLines;
    function SnapToGridValue(Value: single): single;
    procedure SetSnapGridSize(const Value: single);
    procedure SnapToGridLines(AllowChangePosition: boolean);
    function SnapPointToGridLines(const APoint: TvgPoint): TvgPoint;
    procedure ReadDesignSnapGridShow(Reader: TReader);
    procedure WriteDesignSnapGridShow(Writer: TWriter);
    procedure ReadDesignSnapToGrid(Reader: TReader);
    procedure WriteDesignSnapToGrid(Writer: TWriter);
    procedure ReadDesignSnapToLines(Reader: TReader);
    procedure WriteDesignSnapToLines(Writer: TWriter);
    { design }
    function GetRoot: TvgObject;
    procedure SetFocused(const Value: TvgVisualObject);
    procedure DoDesignSelect(AObject: TObject);
    procedure SetSelected(const Value: TvgVisualObject);
    { IvgScene }
    function GetDisableUpdate: boolean;
    function GetDesignTime: boolean;
    function GetCanvas: TvgCanvas;
    function GetOwner: TComponent;
    function GetComponent: TComponent;
    function GetSelected: TvgVisualObject;
    procedure SetDisableUpdate(Value: boolean);
    function GetUpdateRectsCount: integer;
    function GetUpdateRect(const Index: integer): TvgRect;
    procedure SetCaptured(const Value: TvgVisualObject);
    function GetCaptured: TvgVisualObject;
    function GetFocused: TvgVisualObject;
    procedure SetDesignRoot(const Value: TvgVisualObject);
    function GetMousePos: TvgPoint;
    procedure StartWindowDrag;
    procedure StartWindowResize;
    function GetTransparency: boolean;
    function GetDesignPlaceObject: TvgVisualObject;
    function GetStyle: TvgResources;
    function GetActiveControl: TvgControl;
    procedure SetStyle(const Value: TvgResources);
    procedure BeginDrag;
    procedure BeginResize;
    procedure BeginVCLDrag(Source: TObject; ABitmap: TvgBitmap);
    function GetAnimatedCaret: boolean;
    function LocalToScreen(const Point: TvgPoint): TvgPoint;
    function ShowKeyboardForControl(AObject: TvgObject): boolean;
    function HideKeyboardForControl(AObject: TvgObject): boolean;
  protected
    FUpdateRects: array of TvgRect;
    procedure Loaded; override;
    procedure Draw; virtual;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure DefineProperties(Filer: TFiler); override;
    function ObjectByPoint(X, Y: single): TvgVisualObject;
    { inherited }
    procedure PaintRects(UpdateRects: array of TvgRect); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: single); override;
    procedure MouseWheel(Shift: TShiftState; WheelDelta: integer; var Handled: boolean); override;
    procedure KeyDown(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState); override;
    procedure DragEnter(const Data: TvgDragObject; const Point: TvgPoint); override;
    procedure DragOver(const Data: TvgDragObject; const Point: TvgPoint; var Accept: Boolean); override;
    procedure DragDrop(const Data: TvgDragObject; const Point: TvgPoint); override;
    procedure DragLeave; override;
    procedure SetIsActive(const Value: boolean); override;
  public
    constructor CreateNew(AOwner: TComponent; Dummy: Integer = 0); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: integer); override;
    procedure UpdateResource;
    { children }
    procedure AddObject(AObject: TvgObject); override;
    procedure RemoveObject(AObject: TvgObject); override;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure EndUpdateWOInvalidate;
    procedure RealignRoot;
    { paint }
    procedure AddUpdateRect(R: TvgRect);
    { design }
    procedure InsertObject(const ClassName: string);
    property DesignTime: boolean read FDesignTime write FDesignTime stored false;
    { debug }
    property ShowUpdateRects: boolean read FShowUpdateRects write FShowUpdateRects stored false;
    { }
    property Root: TvgObject read GetRoot;
    property Selected: TvgVisualObject read FSelected write SetSelected;
    property Captured: TvgVisualObject read FCaptured;
    property Hovered: TvgVisualObject read FHovered;
    property Focused: TvgVisualObject read FFocused write SetFocused;
    property DisableUpdate: boolean read FDisableUpdate;
    { design }
    property DesignPopupEnabled: boolean read FDesignPopupEnabled write FDesignPopupEnabled;
    property DesignSnapGridShow: boolean read FSnapGridShow write SetSnapGridShow;
    property DesignSnapToGrid: boolean read FSnapToGrid write FSnapToGrid;
    property DesignSnapToLines: boolean read FSnapToLines write FSnapToLines;
    property DesignChangeSelection: TNotifyEvent read FDesignChangeSelection write FDesignChangeSelection;
    property DesignSnapGridSize: single read FSnapGridSize write SetSnapGridSize;
  published
    property Fill: TvgBrush read FFill write SetFill;
    property Style: TvgResources read FStyle write SetStyle;
    property ActiveControl: TvgControl read FActiveControl write SetActiveControl;
  end;

  { Compatible Issue }

  TForm = TvgForm;

  TVCLAlign = (alNone, alTop, alBottom, alLeft, alRight, alClient, alCustom);

  TvgScene = class(TvgVisualObject)
  private
    FForm: TvgForm;
    FTransparency: boolean;
    FFill: TvgBrush;
    FAlign: TVCLAlign;
    FActiveControl: TvgControl;
    FStyle: TvgResources;
    FShowUpdateRects: boolean;
    procedure ReadBool(Reader: TReader);
    procedure SetFill(const Value: TvgBrush);
    procedure SetStyle(const Value: TvgResources);
    procedure SetShowUpdateRects(const Value: boolean);
  protected
    procedure DefineProperties(Filer: TFiler); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddObject(AObject: TvgObject); override;
    property ShowUpdateRects: boolean read FShowUpdateRects write SetShowUpdateRects;
  published
    property Align: TVCLAlign read FAlign write FAlign;
    property Fill: TvgBrush read FFill write SetFill;
    property Style: TvgResources read FStyle write SetStyle;
    property ActiveControl: TvgControl read FActiveControl write FActiveControl;
    property Transparency: boolean read FTransparency write FTransparency;
  end;

{ Application }

  TIdleEvent = procedure (Sender: TObject; var Done: Boolean) of object;

  TCreateFormRec = record
    InstanceClass: TComponentClass;
    Reference: Pointer;
  end;

  { TvgApplication }

  TvgApplication = class(TComponent)
  private
    FRunning: boolean;
    FTerminate: Boolean;
    FOnIdle: TIdleEvent;
    FTitle: string;
    FMainForm: TvgCustomForm;
    FCreateForms: array of TCreateFormRec;
    procedure Idle;
  protected
  public
    MainFormOnTaskBar: Boolean;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure RealCreateForms;
    procedure CreateForm(InstanceClass: TComponentClass; var Reference);
    procedure ProcessMessages;
    function HandleMessage: boolean;
    procedure Run;
    procedure Terminate;
    procedure Initialize;
    procedure HandleException(Sender: TObject);
    procedure ShowException(E: Exception);
    property Terminated: Boolean read FTerminate write FTerminate;
    property OnIdle: TIdleEvent read FOnIdle write FOnIdle;
    property MainForm: TvgCustomForm read FMainForm;
    property Title: string read FTitle write FTitle; // deprecated
  end;

var
  Platform: TvgPlatform;
  Application: TvgApplication;
  DefaultPlatformClass: TvgPlatformClass;

implementation

{$IFDEF DARWIN}
{$IFDEF IOS}
uses
  vg_platform_ios, vg_canvas_ios;
{$ELSE}
uses
  vg_platform_macos, vg_canvas_macos;
{$ENDIF}
{$ENDIF}
{$IFDEF WINDOWS}
uses
  vg_platform_win, vg_canvas_gdip;
{$ENDIF}

{ TvgPlatform }

constructor TvgPlatform.Create;
begin
  inherited ;
end;

destructor TvgPlatform.Destroy;
begin
  inherited ;
end;

function TvgPlatform.ShowVirtualKeyboard(AControl: TvgObject): boolean;
begin
  Result := false;
end;

function TvgPlatform.HideVirtualKeyboard: boolean;
begin
  Result := false;
end;

{ TvgTimer ====================================================================}

const
  cIdNoTimer = Cardinal(-1);

constructor TvgTimer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FInterval := 1000;
  FTimerHandle := cIdNoTimer;
  FEnabled := true;
end;

destructor TvgTimer.Destroy;
begin
  FOnTimer := nil;
  FEnabled := false;
  KillTimer;
  inherited Destroy;
end;

procedure TvgTimer.KillTimer;
begin
  if FTimerHandle <> cIdNoTimer then
  begin
    Platform.DestroyTimer(FTimerHandle);
    FTimerHandle := cIdNoTimer;
  end;
end;

procedure TvgTimer.Loaded;
begin
  inherited Loaded;
  UpdateTimer;
end;

procedure TvgTimer.UpdateTimer;
begin
  KillTimer;
  if (FEnabled) and (FInterval > 0) and (([csDesigning,csLoading,csDestroying] * ComponentState = [])) and Assigned (FOnTimer) then
  begin
    FTimerHandle := Platform.CreateTimer(FInterval, Timer);
    if FTimerHandle = 0 then
    begin
      FTimerHandle := cIdNoTimer;
    end;
  end;
end;

procedure TvgTimer.Timer;
begin
  if (FEnabled) and (FInterval > 0) then
    DoOnTimer;
end;

procedure TvgTimer.SetOnTimer(Value: TNotifyEvent);
begin
  if @Value = @FOnTimer then Exit;
  FOnTimer := value;
  UpdateTimer;
end;

procedure TvgTimer.DoOnTimer;
begin
  if Assigned(FOnTimer) then
    FOnTimer(Self);
end;

procedure TvgTimer.SetEnabled (value : boolean);
begin
  if (Value <> FEnabled) then
  begin
    FEnabled := value;
    UpdateTimer;
  end;
end;

procedure TvgTimer.SetInterval (value : cardinal);
begin
  if (value <> FInterval) then
  begin
    FInterval := value;
    UpdateTimer;
  end;
end;

{ TFont }

procedure TFont.ReadIdent(Reader: TReader);
begin
  Reader.ReadIdent;
end;

procedure TFont.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('Charset', ReadIdent, nil, false);
  Filer.DefineProperty('Color', ReadIdent, nil, false);
end;

{ TvgCustomForm =======================================================================}

constructor TvgCustomForm.Create(AOwner: TComponent);
begin
  GlobalNameSpace.BeginWrite;
  try
    CreateNew(AOwner);
    if (ClassType <> TvgCustomForm) and not (csDesigning in ComponentState) then
    begin
      if not InitInheritedComponent(Self, TvgCustomForm) then
        raise EResNotFound.Create('Resource not found ' + ClassName);
    end;
  finally
    GlobalNameSpace.EndWrite;
  end;
end;

constructor TvgCustomForm.CreateNew(AOwner: TComponent; Dummy: Integer);
begin
  inherited Create(AOwner);
  FVCLFont := TFont.Create;
  FWidth := 600;
  FHeight := 400;
  FBorderIcons := [biSystemMenu, biMinimize, biMaximize];
  FBorderStyle := bsSizeable;
  FShowActivated := true;
  FStaysOpen := true;
  FHandle := Platform.CreateWindow(Self);
end;

destructor TvgCustomForm.Destroy;
begin
  Platform.DestroyWindow(Self);
  FVCLFont.Free;
  inherited;
end;

procedure TvgCustomForm.AfterConstruction;
begin
  inherited;
  if Assigned(FOnCreate) then FOnCreate(Self);
end;

procedure TvgCustomForm.BeforeDestruction;
begin
  if Assigned(FOnDestroy) then FOnDestroy(Self);
  inherited;
end;

procedure TvgCustomForm.Recreate;
begin
  FFormState := FFormState + [fsRecreating];
  Platform.DestroyWindow(Self);
  FHandle := Platform.CreateWindow(Self);
  if Visible then Platform.ShowWindow(Self);
  FFormState := FFormState - [fsRecreating];
end;

procedure TvgCustomForm.PaintRects(UpdateRects: array of TvgRect);
begin
end;

function TvgCustomForm.CloseQuery: Boolean;
var
  i: integer;
begin
  Result := True;
  if Assigned(FOnCloseQuery) then FOnCloseQuery(Self, Result);
end;

procedure TvgCustomForm.Close;
var
  CloseAction: TCloseAction;
begin
  if fsModal in FFormState then
    ModalResult := mrCancel
  else
    if CloseQuery then
    begin
      CloseAction := caHide;
      if Assigned(FOnClose) then FOnClose(Self, CloseAction);
      if CloseAction <> caNone then
        if Application.MainForm = Self then
          Application.Terminate
        else
          if CloseAction = caHide then
            Hide
          else
            if CloseAction = caMinimize then
//              WindowState := wsMinimized
            else
              Release;
    end;
end;

procedure TvgCustomForm.Show;
begin
  Platform.ShowWindow(Self);
  FVisible := true;
  case StartupLocation of
    vgLocationCenter:
      begin
        with Platform.GetScreenSize do
          SetBounds(round((X - Width) / 2), round((Y - Height) / 2), Width, Height);
      end;
  end;

end;

procedure TvgCustomForm.Hide;
begin
  Platform.HideWindow(Self);
  FVisible := false;
end;

function TvgCustomForm.ShowModal: TModalResult;
begin
  FFormState := FFormState + [fsModal];
  Result := Platform.ShowWindowModal(Self);
  FFormState := FFormState - [fsModal];
end;

procedure TvgCustomForm.CloseModal;
var
  CloseAction: TCloseAction;
begin
  try
    CloseAction := caNone;
    if CloseQuery then
    begin
      CloseAction := caHide;
      if Assigned(FOnClose) then FOnClose(Self, CloseAction);
    end;
    case CloseAction of
      caNone: ModalResult := mrNone;
      caFree: Release;
    end;
  except
    ModalResult := mrNone;
    Application.HandleException(Self);
  end;
end;

procedure TvgCustomForm.Release;
begin
  Platform.ReleaseWindow(Self);
end;

procedure TvgCustomForm.SetBounds(ALeft, ATop, AWidth, AHeight: integer);
begin
  if (ALeft <> FLeft) or (ATop <> FTop) or (AWidth <> FWidth) or (AHeight <> FHeight) then
  begin
    FTop := ATop;
    FLeft := ALeft;
    FWidth := AWidth;
    FHeight := AHeight;
    Platform.SetWindowRect(Self, vgRect(FLeft, FTop, FLeft + FWidth, FTop + FHeight));
    if Assigned(FOnResize) then
      FOnResize(Self);
  end;
end;

procedure TvgCustomForm.KeyDown(var Key: Word; var KeyChar: WideChar; Shift: TShiftState);
begin
end;

procedure TvgCustomForm.KeyUp(var Key: Word; var KeyChar: WideChar; Shift: TShiftState);
begin
end;

procedure TvgCustomForm.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: single);
begin
end;

procedure TvgCustomForm.MouseMove(Shift: TShiftState; X, Y: single);
begin
end;

procedure TvgCustomForm.MouseLeave;
begin
  MouseMove([], -1, -1);
end;

procedure TvgCustomForm.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: single);
begin
end;

procedure TvgCustomForm.MouseWheel(Shift: TShiftState; WheelDelta: integer; var Handled: boolean);
begin
end;

procedure TvgCustomForm.DragDrop(const Data: TvgDragObject; const Point: TvgPoint);
begin

end;

procedure TvgCustomForm.DragEnter(const Data: TvgDragObject; const Point: TvgPoint);
begin

end;

procedure TvgCustomForm.DragLeave;
begin

end;

procedure TvgCustomForm.DragOver(const Data: TvgDragObject; const Point: TvgPoint;
  var Accept: Boolean);
begin
  Accept := true;
end;

procedure TvgCustomForm.SetCaption(const Value: WideString);
begin
  if FCaption <> Value then
  begin
    FCaption := Value;
    Platform.SetWindowCaption(Self, FCaption);
  end;
end;

procedure TvgCustomForm.SetHeight(const Value: integer);
begin
  SetBounds(Left, Top, Width, Value);
end;

procedure TvgCustomForm.SetLeft(const Value: integer);
begin
  SetBounds(Value, Top, Width, Height);
end;

procedure TvgCustomForm.SetTop(const Value: integer);
begin
  SetBounds(Left, Value, Width, Height);
end;

procedure TvgCustomForm.SetWidth(const Value: integer);
begin
  SetBounds(Left, Top, Value, Height);
end;

procedure TvgCustomForm.MouseCapture;
begin
  Platform.SetCapture(Self);
end;

procedure TvgCustomForm.ReleaseCapture;
begin
  Platform.ReleaseCapture(Self);
end;

procedure TvgCustomForm.InvalidateRect(R: TvgRect);
begin
  if csLoading in ComponentState then Exit;
  if csDestroying in ComponentState then Exit;
  Platform.InvalidateWindowRect(Self, R);
end;

function TvgCustomForm.GetClientHeight: integer;
begin
  Result := round(Platform.GetClientSize(Self).Y);
end;

function TvgCustomForm.GetClientWidth: integer;
begin
  Result := round(Platform.GetClientSize(Self).X);
end;

procedure TvgCustomForm.SetClientHeight(const Value: integer);
begin
  Height := Value;
end;

procedure TvgCustomForm.SetClientWidth(const Value: integer);
begin
  Width := Value;
end;

procedure TvgCustomForm.SetTransparency(const Value: boolean);
begin
  if FTransparency <> Value then
  begin
    FTransparency := Value;
    Recreate;
  end;
end;

procedure TvgCustomForm.SetBorderStyle(const Value: TvgCustomFormBorderStyle);
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    Recreate;
  end;
end;

procedure TvgCustomForm.SetBorderIcons(const Value: TBorderIcons);
begin
  if FBorderIcons <> Value then
  begin
    FBorderIcons := Value;
    Recreate;
  end;
end;

procedure TvgCustomForm.SetVisible(const Value: boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    if FVisible then
      Show
    else
      Hide;
  end;
end;

function TvgCustomForm.ClientToScreen(const Point: TvgPoint): TvgPoint;
begin
  Result := Platform.ClientToScreen(Self, Point);
end;

function TvgCustomForm.ScreenToClient(const Point: TvgPoint): TvgPoint;
begin
  Result := Platform.ScreenToClient(Self, Point);
end;

procedure TvgCustomForm.SetTopMost(const Value: boolean);
begin
  if FTopMost <> Value then
  begin
    FTopMost := Value;
    Recreate;
  end;
end;

procedure TvgCustomForm.Activate;
begin
  if Assigned(FOnActivate) then FOnActivate(Self);
  IsActive := true;
end;

procedure TvgCustomForm.Deactivate;
begin
  if Assigned(FOnDeactivate) then FOnDeactivate(Self);
  IsActive := false;
end;

procedure TvgCustomForm.SetIsActive(const Value: boolean);
begin
  if FIsActive <> Value then
  begin
    FIsActive := Value;
  end;
end;

procedure TvgCustomForm.ReadColor(Reader: TReader);
begin
  Reader.ReadIdent;
end;

procedure TvgCustomForm.ReadBool(Reader: TReader);
begin
  Reader.ReadBoolean;
end;

procedure TvgCustomForm.ReadInt(Reader: TReader);
begin
  Reader.ReadInteger;
end;

procedure TvgCustomForm.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('Color', ReadColor, nil, false);
  Filer.DefineProperty('OldCreateOrder', ReadBool, nil, false);
  Filer.DefineProperty('PixelsPerInch', ReadInt, nil, false);
  Filer.DefineProperty('TextHeight', ReadInt, nil, false);
end;

type

  TvgHackObject = class(TvgObject);
  TvgHackVisualObject = class(TvgVisualObject);
  TvgHackControl = class(TvgControl);

{ TvgForm ==================================================================}

constructor TvgForm.CreateNew(AOwner: TComponent; Dummy: Integer = 0);
begin
  inherited;
  FDesignTime := csDesigning in ComponentState;
  FCanvas := DefaultCanvasClass.Create(ClientWidth, ClientHeight);
  RealignRoot;
  FSnapToLines := true;
  FAlignRoot := true;
  {$IFDEF DARWINBUFFER}
  FCanvas.FBuffered := true;
  {$ENDIF}
  FFill := TvgBrush.Create(vgBrushNone, $FF000000);
  FFill.OnChanged := FillChanged;
  DesignPopupEnabled := true;
  FSnapGridSize := 1;
  if vgDesigner <> nil then
    vgDesigner.AddScene(Self);
  vgSceneCount := vgSceneCount + 1;
end;

destructor TvgForm.Destroy;
begin
  if FHovered <> nil then
  begin
    TvgVisualObject(FHovered).RemoveFreeNotify(Self);
    FHovered := nil;
  end;
  if FFocused <> nil then
  begin
    TvgVisualObject(FFocused).RemoveFreeNotify(Self);
    FFocused := nil;
  end;
  if vgDesigner <> nil then
    vgDesigner.RemoveScene(Self);
  vgSceneCount := vgSceneCount - 1;
  if vgSceneCount = 0 then
  begin
    if aniThread <> nil then
    begin
      aniThread.Free;
    end;
    aniThread := nil;
  end;
  DeleteChildren;
  if FChildren <> nil then
    FreeAndNil(FChildren);
  FreeAndNil(FFill);
  FreeAndNil(FCanvas);
  inherited;
end;

procedure TvgForm.Loaded;
begin
  inherited;
  RealignRoot;
end;

procedure TvgForm.DoShowTimer(Sender: TObject);
begin
//  FShowTimer.Enabled := false;
  AddUpdateRect(vgRect(0, 0, $FFFF, $FFFF));
  FShowTimer.Free;
end;

procedure TvgForm.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  i: Integer;
begin
  inherited;
  if FChildren <> nil then
    for i := 0 to FChildren.Count - 1 do
      if (TObject(FChildren[i]) is TvgObject) and (TvgObject(FChildren[i]).Stored) then
        Proc(Children[i]);
end;

procedure TvgForm.AddUpdateRect(R: TvgRect);
begin
  if FDisableUpdate then Exit;
  if csLoading in ComponentState then Exit;
  if csDestroying in ComponentState then Exit;
  if not vgIntersectRect(R, vgRect(0, 0, ClientWidth, ClientHeight)) then Exit;
  InvalidateRect(R);
end;

procedure TvgForm.Draw;
var
  i, j: integer;
  R: TvgRect;
  Rgn, NRgn: Cardinal;
  ScaleMatrix: TvgMatrix;
begin
  if FDrawing then Exit;
  if Length(FUpdateRects) > 0 then
  begin
    FDrawing := true;
    try
      { Split rects if rects too more }
      if (Length(FUpdateRects) > 20) then
      begin
        for i := 1 to High(FUpdateRects) do
          FUpdateRects[0] := vgUnionRect(FUpdateRects[0], FUpdateRects[i]);
        SetLength(FUpdateRects, 1);
      end;
      { draw back }
      if Canvas.BeginScene then
      begin
        Canvas.ResetClipRect;
        ScaleMatrix := IdentityMatrix;
        Canvas.SetMatrix(ScaleMatrix);
        Canvas.SetClipRects(FUpdateRects);

        if (FFill.Style = vgBrushNone) or ((FFill.SolidColor and $FF000000 = 0) and (FFill.Style = vgBrushSolid)) then
        begin
          for i := 0 to High(FUpdateRects) do
          begin
            if Transparency then
              Canvas.ClearRect(FUpdateRects[i], 0)
            else
              {$ifdef FPC_BIG_ENDIAN}
              Canvas.ClearRect(FUpdateRects[i], BigEndianColor(FFill.SolidColor and $FFFFFF));
              {$ELSE}
              Canvas.ClearRect(FUpdateRects[i], FFill.SolidColor and $FFFFFF);
              {$ENDIF}
          end;
        end
        else
        begin
          Canvas.Fill.Assign(FFill);
          Canvas.FillRect(vgRect(-1, -1, Width + 1, Height + 1), 0, 0, AllCorners, 1);
        end;
        { reset }
        Canvas.StrokeThickness := 1;
        Canvas.StrokeCap := vgCapFlat;
        Canvas.StrokeJoin := vgJoinMiter;
        Canvas.StrokeDash := vgDashSolid;
        Canvas.Stroke.Style := vgBrushSolid;
        Canvas.Fill.Style := vgBrushSolid;
        if FChildren <> nil then
          for i := 0 to FChildren.Count - 1 do
          begin
            if not (TObject(FChildren[i]) is TvgVisualObject) then Continue;
            if not TvgVisualObject(FChildren[i]).Visible then Continue;

            ScaleMatrix := IdentityMatrix;
            for j := 0 to High(FUpdateRects) do
              if vgIntersectRect(FUpdateRects[j], TvgVisualObject(FChildren[i]).UpdateRect) then
              begin
                Canvas.SetMatrix(vgMatrixMultiply(ScaleMatrix, TvgVisualObject(FChildren[i]).AbsoluteMatrix));
                TvgHackVisualObject(FChildren[i]).BeforePaint;
                TvgHackVisualObject(FChildren[i]).Paint;
                TvgHackVisualObject(FChildren[i]).PaintChildren;
                if Assigned(TvgHackVisualObject(FChildren[i]).OnPaint) then
                begin
                  Canvas.SetMatrix(TvgHackVisualObject(FChildren[i]).AbsoluteMatrix);
                  TvgHackVisualObject(FChildren[i]).OnPaint(TvgHackVisualObject(FChildren[i]), Canvas, TvgHackVisualObject(FChildren[i]).LocalRect);
                end;
                Break;
              end;
          end;
        { grid }
        if FSnapGridShow and (FSnapGridSize <> 0) then
        begin
          ScaleMatrix := IdentityMatrix;
          Canvas.SetMatrix(ScaleMatrix);
          Canvas.Stroke.Style := vgBrushSolid;
          Canvas.StrokeThickness := 1;
  (*        for i := Trunc((-FDesignScroll.X) / (FSnapGridSize)) - 1 to Trunc((-FDesignScroll.X + Width) / (FSnapGridSize)) + 1 do
          begin
            if i mod 5 = 0 then
              Canvas.Stroke.SolidColor := $50505050
            else
              Canvas.Stroke.SolidColor := $50303030;
            Canvas.DrawLine(vgPoint(i * FSnapGridSize + 0.5, -FDesignScroll.Y + 0.5), vgPoint(i * FSnapGridSize + 0.5, -FDesignScroll.Y + Height + 0.5), 1);
          end;
          for j := Trunc((-FDesignScroll.Y) / (FSnapGridSize)) - 1 to Trunc((-FDesignScroll.Y + Height) / (FSnapGridSize)) + 1 do
          begin
            if j mod 5 = 0 then
              Canvas.Stroke.SolidColor := $50505050
            else
              Canvas.Stroke.SolidColor := $50303030;
            Canvas.DrawLine(vgPoint(-FDesignScroll.X + 0.5, j * FSnapGridSize + 0.5), vgPoint(-FDesignScroll.X + Width + 0.5, j * FSnapGridSize + 0.5), 1)
          end; *)
        end;
        { design }
        if (FSelected <> nil) and not FSelected.DisableDesignResize then
        begin
          Canvas.Fill.Style := vgBrushSolid;
          Canvas.Fill.SolidColor := $FFFFFFFF;
          Canvas.StrokeThickness := 1;
          Canvas.Stroke.Style := vgBrushSolid;
          Canvas.Stroke.SolidColor := $FF1072C5;
          ScaleMatrix := IdentityMatrix;
          Canvas.SetMatrix(vgMatrixMultiply(ScaleMatrix, FSelected.AbsoluteMatrix));
          R := FSelected.BoundsRect;
          vgInflateRect(R, -0.5, -0.5);
          Canvas.StrokeDash := vgDashDash;
          Canvas.DrawRect(R, 0, 0, AllCorners, 1);
          Canvas.StrokeDash := vgDashSolid;
          begin
          { rotate }
          if FRotateHot then
            Canvas.Fill.SolidColor := $FFFF0000
          else
            Canvas.Fill.SolidColor := $FFFFFFFF;
          R := FSelected.BoundsRect;
          vgInflateRect(R, -0.5, -0.5);
          Canvas.DrawLine(vgPoint((R.Left + R.Right) / 2, R.Top), vgPoint((R.Left + R.Right) / 2, R.Top - RotSize), 1);
          Canvas.Fillellipse(vgRect((R.Left + R.Right) / 2 - (GripSize), R.Top - RotSize - (GripSize),
            (R.Left + R.Right) / 2 +(GripSize), R.Top - RotSize + (GripSize)), 1);
          Canvas.DrawEllipse(vgRect((R.Left + R.Right) / 2 - (GripSize), R.Top - RotSize - (GripSize),
            (R.Left + R.Right) / 2 +(GripSize), R.Top - RotSize + (GripSize)), 1);
          { angles }
          if FLeftTopHot then
            Canvas.Fill.SolidColor := $FFFF0000
          else
            Canvas.Fill.SolidColor := $FFFFFFFF;
          R := FSelected.BoundsRect;
          vgInflateRect(R, -0.5, -0.5);
          Canvas.Fillellipse(vgRect(R.Left - (GripSize), R.Top - (GripSize), R.Left + (GripSize), R.Top + (GripSize)), 1);
          Canvas.DrawEllipse(vgRect(R.Left - (GripSize), R.Top - (GripSize), R.Left + (GripSize), R.Top + (GripSize)), 1);

          if FRightTopHot then
            Canvas.Fill.SolidColor := $FFFF0000
          else
            Canvas.Fill.SolidColor := $FFFFFFFF;
          R := FSelected.BoundsRect;
          vgInflateRect(R, -0.5, -0.5);
          Canvas.Fillellipse(vgRect(R.Right - (GripSize), R.Top - (GripSize), R.Right + (GripSize), R.Top + (GripSize)), 1);
          Canvas.DrawEllipse(vgRect(R.Right - (GripSize), R.Top - (GripSize), R.Right + (GripSize), R.Top + (GripSize)), 1);

          if FLeftBottomHot then
            Canvas.Fill.SolidColor := $FFFF0000
          else
            Canvas.Fill.SolidColor := $FFFFFFFF;
          R := FSelected.BoundsRect;
          vgInflateRect(R, -0.5, -0.5);
          Canvas.Fillellipse(vgRect(R.Left - (GripSize), R.Bottom - (GripSize), R.Left + (GripSize), R.Bottom + (GripSize)), 1);
          Canvas.DrawEllipse(vgRect(R.Left - (GripSize), R.Bottom - (GripSize), R.Left + (GripSize), R.Bottom + (GripSize)), 1);

          if FRightBottomHot then
            Canvas.Fill.SolidColor := $FFFF0000
          else
            Canvas.Fill.SolidColor := $FFFFFFFF;
          R := FSelected.BoundsRect;
          vgInflateRect(R, -0.5, -0.5);
          Canvas.FillEllipse(vgRect(R.Right - (GripSize), R.Bottom - (GripSize), R.Right + (GripSize), R.Bottom + (GripSize)), 1);
          Canvas.DrawEllipse(vgRect(R.Right - (GripSize), R.Bottom - (GripSize), R.Right + (GripSize), R.Bottom + (GripSize)), 1);
          { lines }
          if FSelected.Width > GripSize * 4 then
          begin
            if FTopHot then
              Canvas.Fill.SolidColor := $FFFF0000
            else
              Canvas.Fill.SolidColor := $FFFFFFFF;
            R := FSelected.BoundsRect;
            vgInflateRect(R, -0.5, -0.5);
            Canvas.FillRect(vgRect(R.Left + vgRectWidth(R)/2 - (GripSize), R.Top - (GripSize), R.Left + vgRectWidth(R)/2 + (GripSize), R.Top + (GripSize)), 0, 0, [], 1);
            Canvas.DrawRect(vgRect(R.Left + vgRectWidth(R)/2 - (GripSize), R.Top - (GripSize), R.Left + vgRectWidth(R)/2 + (GripSize), R.Top + (GripSize)), 0, 0, [], 1);
            if FBottomHot then
              Canvas.Fill.SolidColor := $FFFF0000
            else
              Canvas.Fill.SolidColor := $FFFFFFFF;
            R := FSelected.BoundsRect;
            vgInflateRect(R, -0.5, -0.5);
            Canvas.FillRect(vgRect(R.Left + vgRectWidth(R)/2 - (GripSize), R.Bottom - (GripSize), R.Left + vgRectWidth(R)/2 + (GripSize), R.Bottom + (GripSize)), 0, 0, [], 1);
            Canvas.DrawRect(vgRect(R.Left + vgRectWidth(R)/2 - (GripSize), R.Bottom - (GripSize), R.Left + vgRectWidth(R)/2 + (GripSize), R.Bottom + (GripSize)), 0, 0, [], 1);
          end;
          if FSelected.Height > GripSize * 4 then
          begin
            if FLeftHot then
              Canvas.Fill.SolidColor := $FFFF0000
            else
              Canvas.Fill.SolidColor := $FFFFFFFF;
            R := FSelected.BoundsRect;
            vgInflateRect(R, -0.5, -0.5);
            Canvas.FillRect(vgRect(R.Left - (GripSize), R.Top + vgRectHeight(R)/2 - (GripSize), R.Left + (GripSize), R.Top + vgRectHeight(R)/2 + (GripSize)), 0, 0, [], 1);
            Canvas.DrawRect(vgRect(R.Left - (GripSize), R.Top + vgRectHeight(R)/2 - (GripSize), R.Left + (GripSize), R.Top + vgRectHeight(R)/2 + (GripSize)), 0, 0, [], 1);
            if FRightHot then
              Canvas.Fill.SolidColor := $FFFF0000
            else
              Canvas.Fill.SolidColor := $FFFFFFFF;
            R := FSelected.BoundsRect;
            vgInflateRect(R, -0.5, -0.5);
            Canvas.FillRect(vgRect(R.Right - (GripSize), R.Top + vgRectHeight(R)/2 - (GripSize), R.Right + (GripSize), R.Top + vgRectHeight(R)/2 + (GripSize)), 0, 0, [], 1);
            Canvas.DrawRect(vgRect(R.Right - (GripSize), R.Top + vgRectHeight(R)/2 - (GripSize), R.Right + (GripSize), R.Top + vgRectHeight(R)/2 + (GripSize)), 0, 0, [], 1);
          end;
          { grid lines }
          if (FMoving or FLeftTop or FRightTop or FLeftBottom or FRightBottom or FTop or FBottom or FLeft or FRight) and
             (Length(FDesignGridLines) > 0) and (FSelected.Parent <> nil) and (FSelected.Parent.IsVisual) then
          begin
            ScaleMatrix := IdentityMatrix;
            Canvas.SetMatrix(vgMatrixMultiply(ScaleMatrix, TvgVisualObject(FSelected.Parent).AbsoluteMatrix));
            Canvas.StrokeDash := vgDashDash;
            for i := 0 to High(FDesignGridLines) do
            begin
              if (FDesignGridLines[i].Position.Y + round(FDesignGridLines[i].Height / 2)) = (FSelected.Position.Y + round(FSelected.Height / 2)) then
              begin
                Canvas.DrawLine(vgPoint(FSelected.Position.X + 0.5, Trunc(FSelected.Position.Y + (FSelected.Height / 2)) + 0.5),
                  vgPoint(FDesignGridLines[i].Position.X + 0.5, Trunc(FSelected.Position.Y + (FSelected.Height / 2)) + 0.5), 1);
              end;
              if (FDesignGridLines[i].Position.X + round(FDesignGridLines[i].Width / 2)) = (FSelected.Position.X + round(FSelected.Width / 2)) then
              begin
                Canvas.DrawLine(vgPoint(Trunc(FSelected.Position.X + (FSelected.Width / 2)) + 0.5, FSelected.Position.Y + 0.5),
                  vgPoint(Trunc(FDesignGridLines[i].Position.X + (FDesignGridLines[i].Width / 2)) + 0.5, FDesignGridLines[i].Position.Y + 0.5), 1);
              end;
              if (FSelected.Position.X = FDesignGridLines[i].Position.X) or (FSelected.Position.Y = FDesignGridLines[i].Position.Y) then
              begin
                Canvas.DrawLine(vgPoint(FSelected.Position.X + 0.5, FSelected.Position.Y + 0.5), vgPoint(FDesignGridLines[i].Position.X + 0.5, FDesignGridLines[i].Position.Y + 0.5), 1);
              end;
              if (FSelected.Position.X + FSelected.Width = FDesignGridLines[i].Position.X) then
              begin
                Canvas.DrawLine(vgPoint(FDesignGridLines[i].Position.X + 0.5, FSelected.Position.Y + 0.5), vgPoint(FDesignGridLines[i].Position.X + 0.5, FDesignGridLines[i].Position.Y + 0.5), 1);
              end;
              if (FSelected.Position.Y + FSelected.Height = FDesignGridLines[i].Position.Y) then
              begin
                Canvas.DrawLine(vgPoint(FSelected.Position.X + 0.5, FDesignGridLines[i].Position.Y + 0.5), vgPoint(FDesignGridLines[i].Position.X + 0.5, FDesignGridLines[i].Position.Y + 0.5), 1);
              end;
              if (FSelected.Position.X = FDesignGridLines[i].Position.X + FDesignGridLines[i].Width) then
              begin
                Canvas.DrawLine(vgPoint(FDesignGridLines[i].Position.X + FDesignGridLines[i].Width + 0.5, FSelected.Position.Y + 0.5),
                  vgPoint(FDesignGridLines[i].Position.X + FDesignGridLines[i].Width + 0.5, FDesignGridLines[i].Position.Y + 0.5), 1);
              end;
              if (FSelected.Position.Y = FDesignGridLines[i].Position.Y + FDesignGridLines[i].Height) then
              begin
                Canvas.DrawLine(vgPoint(FSelected.Position.X + 0.5, FDesignGridLines[i].Position.Y + FDesignGridLines[i].Height + 0.5),
                  vgPoint(FDesignGridLines[i].Position.X + 0.5, FDesignGridLines[i].Position.Y + FDesignGridLines[i].Height + 0.5), 1);
              end;
              if (FSelected.Position.X + FSelected.Width = FDesignGridLines[i].Position.X + FDesignGridLines[i].Width) then
              begin
                Canvas.DrawLine(vgPoint(FDesignGridLines[i].Position.X + FDesignGridLines[i].Width + 0.5, FSelected.Position.Y + 0.5),
                  vgPoint(FDesignGridLines[i].Position.X + FDesignGridLines[i].Width + 0.5, FDesignGridLines[i].Position.Y + 0.5), 1);
              end;
              if (FSelected.Position.Y + FSelected.Height = FDesignGridLines[i].Position.Y + FDesignGridLines[i].Height) then
              begin
                Canvas.DrawLine(vgPoint(FSelected.Position.X + 0.5, FDesignGridLines[i].Position.Y + FDesignGridLines[i].Height + 0.5),
                  vgPoint(FDesignGridLines[i].Position.X + 0.5, FDesignGridLines[i].Position.Y + FDesignGridLines[i].Height + 0.5), 1);
              end;
            end;
            Canvas.StrokeDash := vgDashSolid;
          end;
          end;
          { place message }
          if FDesignPlaceObject <> nil then
          begin
            ScaleMatrix := IdentityMatrix;
            Canvas.SetMatrix(ScaleMatrix);

            R := FDesignPlaceObject.AbsoluteRect;
            Canvas.Stroke.SolidColor := $FF5B91DE;
            Canvas.DrawRect(R, 0, 0, AllCorners, 1);
            Canvas.Font.Family := 'Tahoma';
            Canvas.Font.Style := vgFontRegular;
            Canvas.Font.Size := 9;
            R.Bottom := R.Top;
            R.Top := R.Bottom - 19;
            R.Right := R.Left + 160;
            Canvas.Fill.SolidColor := $FF5B91DE;
            Canvas.FillRect(R, 0, 0, AllCorners, 1);
            Canvas.Fill.SolidColor := $FFFFFFFF;
            vgInflateRect(R, -2, -2);
            if FDesignPlaceObject.Name <> '' then
              Canvas.FillText(R, R, 'ALT-drag to place into [' + FDesignPlaceObject.Name + ']', false, 1, vgTextAlignNear)
            else
              Canvas.FillText(R, R, 'ALT-drag to place into [' + FDesignPlaceObject.ClassName + ']', false, 1, vgTextAlignNear);
          end;
        end;
        { design modes }
        if FDesignTime then
        begin
          ScaleMatrix := IdentityMatrix;
          Canvas.SetMatrix(ScaleMatrix);

          Canvas.Stroke.SolidColor := $FF5B91DE;
          Canvas.Font.Family := 'Tahoma';
          Canvas.Font.Style := vgFontRegular;
          Canvas.Font.Size := 9;
          R := vgRect(0, 0, 200, 17);
        end;
        { debug }
        if ShowUpdateRects then
        with Canvas do
        begin
          ResetClipRect;
          ScaleMatrix := IdentityMatrix;
          Canvas.SetMatrix(ScaleMatrix);
          Stroke.Style := vgBrushSolid;
          Stroke.Color := '#9000FF00';
          StrokeThickness := 1;
          Fill.Style := vgBrushNone;
          for i := 0 to High(FUpdateRects) do
          begin
            R := FUpdateRects[i];
            DrawRect(FUpdateRects[i], 0, 0, AllCorners, 0.5);
          end;
        end;
        {$IFDEF UPDATERECT}
        with Canvas do
        begin
          ResetClipRect;
          ScaleMatrix := IdentityMatrix;
          Canvas.SetMatrix(ScaleMatrix);
          Stroke.Style := vgBrushSolid;
          Stroke.Color := '#90FF0000';
          StrokeThickness := 1;
          Fill.Style := vgBrushNone;
          for i := 0 to High(FUpdateRects) do
          begin
            R := FUpdateRects[i];
            DrawRect(FUpdateRects[i], 0, 0, AllCorners, 0.5);
          end;
        end;
        {$ENDIF}
        Canvas.EndScene;
      end;
      { flush buffer }
      if Canvas.Buffered then
        for i := 0 to High(FUpdateRects) do
        begin
          R := FUpdateRects[i];
          Canvas.FlushBufferRect(0, 0, Context, FUpdateRects[i]);
        end;
    finally
      setLength(FUpdateRects, 0);
      FDrawing := false;
    end;
  end;
end;

procedure TvgForm.PaintRects(UpdateRects: array of TvgRect);
begin
  SetLength(FUpdateRects, Length(UpdateRects));
  Move(UpdateRects[0], FUpdateRects[0], SizeOf(FUpdateRects[0]) * Length(FUpdateRects));
  Draw;
end;

function TvgForm.ObjectByPoint(X, Y: single): TvgVisualObject;
var
  i: integer;
  Obj, NewObj: TvgObject;
begin
  Result := nil;
  for i := ChildrenCount - 1 downto 0 do
  begin
    Obj := Children[i];
    if not (Obj is TvgVisualObject) then Continue;
    if not TvgVisualObject(Obj).Visible and not (FDesignTime) then Continue;

    NewObj := TvgVisualObject(Obj).ObjectByPoint(X, Y);
    if NewObj <> nil then
    begin
      Result := TvgVisualObject(NewObj);
      Exit;
    end;
  end;
end;

procedure TvgForm.AddObject(AObject: TvgObject);
begin
  if AObject = nil then Exit;
  if AObject.Parent <> nil then
    AObject.Parent := nil;
  if AObject is TvgScene then
  begin
    TvgScene(AObject).FForm := Self;
    Exit;
  end;
  inherited ;
  TvgHackObject(AObject).FScene := Self;
  if AObject.IsVisual and not (csDestroying in ComponentState) then
    TvgVisualObject(AObject).Repaint;
  RealignRoot;
end;

procedure TvgForm.RemoveObject(AObject: TvgObject);
begin
  inherited ;
end;

function TvgForm.SnapToGridValue(Value: single): single;
begin
  if (DesignSnapToGrid) and (DesignSnapGridSize <> 0) then
    Result := Trunc(Value / DesignSnapGridSize) * DesignSnapGridSize
  else
    Result := Value;
end;

procedure TvgForm.AddUpdateRectsFromGridLines;
  procedure IntAddUpdateRect(const R: TvgRect);
  var
    i: integer;
  begin
    for i := 0 to High(FUpdateRects) do
      with FUpdateRects[i] do
        if (R.Left = Left) and (R.Top = Top) and (R.Right = Right) and (R.Bottom = Bottom) then
        begin
          Exit;
        end;
    AddUpdateRect(R);
  end;
var
  i: integer;
begin
  { Add grip lines }
  if FDesignTime and (FSelected <> nil) and not FSelected.DisableDesignResize and
     (FMoving or FLeftTop or FRightTop or FLeftBottom or FRightBottom or FTop or FBottom or FLeft or FRight) and
     (Length(FDesignGridLines) > 0) and (FSelected.Parent <> nil) and (FSelected.Parent.IsVisual) then
  begin
    for i := 0 to High(FDesignGridLines) do
    begin
      IntAddUpdateRect(vgNormalizeRect2(vgRect(FSelected.Position.X - 1, FSelected.Position.Y + (FSelected.Height / 2) - 1,
        FDesignGridLines[i].Position.X + 1, FDesignGridLines[i].Position.Y + (FDesignGridLines[i].Height / 2) + 1)));
      IntAddUpdateRect(vgNormalizeRect2(vgRect(FSelected.Position.X + (FSelected.Width / 2) - 1, FSelected.Position.Y - 1,
        FDesignGridLines[i].Position.X + (FDesignGridLines[i].Width / 2) + 1, FDesignGridLines[i].Position.Y + 1)));

      IntAddUpdateRect(vgNormalizeRect2(vgRect(FSelected.Position.X - 1, FSelected.Position.Y - 1, FDesignGridLines[i].Position.X + 1, FDesignGridLines[i].Position.Y + 1)));
      IntAddUpdateRect(vgNormalizeRect2(vgRect(FDesignGridLines[i].Position.X - 1, FSelected.Position.Y - 1, FDesignGridLines[i].Position.X + 1, FDesignGridLines[i].Position.Y + 1)));
      IntAddUpdateRect(vgNormalizeRect2(vgRect(FSelected.Position.X - 1, FDesignGridLines[i].Position.Y - 1, FDesignGridLines[i].Position.X + 1, FDesignGridLines[i].Position.Y + 1)));
      IntAddUpdateRect(vgNormalizeRect2(vgRect(FDesignGridLines[i].Position.X + FDesignGridLines[i].Width - 1, FSelected.Position.Y - 1,
          FDesignGridLines[i].Position.X + FDesignGridLines[i].Width + 1, FDesignGridLines[i].Position.Y + 1)));
      IntAddUpdateRect(vgNormalizeRect2(vgRect(FSelected.Position.X - 1, FDesignGridLines[i].Position.Y + FDesignGridLines[i].Height - 1,
          FDesignGridLines[i].Position.X + 1, FDesignGridLines[i].Position.Y + FDesignGridLines[i].Height + 1)));
      IntAddUpdateRect(vgNormalizeRect2(vgRect(FDesignGridLines[i].Position.X + FDesignGridLines[i].Width - 1, FSelected.Position.Y - 1,
          FDesignGridLines[i].Position.X + FDesignGridLines[i].Width + 1, FDesignGridLines[i].Position.Y + 1)));
      IntAddUpdateRect(vgNormalizeRect2(vgRect(FSelected.Position.X - 1, FDesignGridLines[i].Position.Y + FDesignGridLines[i].Height - 1,
          FDesignGridLines[i].Position.X + 1, FDesignGridLines[i].Position.Y + FDesignGridLines[i].Height + 1)));
    end;
  end;
end;

procedure TvgForm.SnapToGridLines(AllowChangePosition: boolean);
  procedure AddGridLine(const Obj: TvgVisualObject);
  var
    i: integer;
  begin
    for i := 0 to High(FDesignGridLines) do
      if FDesignGridLines[i] = Obj then Exit;
    SetLength(FDesignGridLines, Length(FDesignGridLines) + 1);
    FDesignGridLines[High(FDesignGridLines)] := Obj;
  end;
const
  SnapLineSize = 2;
var
  i: integer;
begin
  if (DesignSnapToLines) and (FSelected.Parent <> nil) then
    for i := 0 to FSelected.Parent.ChildrenCount - 1 do
    begin
      if TvgObject(FSelected.Parent.Children[i]) = FSelected then Continue;
      if not TvgObject(FSelected.Parent.Children[i]).isVisual then Continue;
      with TvgVisualObject(FSelected.Parent.Children[i]) do
      begin
        if (Abs((Position.Y + round(Height / 2)) - (FSelected.Position.Y + round(FSelected.Height / 2))) < SnapLineSize) then
        begin
          if AllowChangePosition then
          begin
            FMousePos.Y := FMousePos.Y + ((Position.Y + round(Height / 2)) - (FSelected.Position.Y + round(FSelected.Height / 2)));
            FSelected.Position.Y := (Position.Y + round(Height / 2) - round(FSelected.Height / 2));
          end;
          AddGridLine(TvgVisualObject(FSelected.Parent.Children[i]));
          Continue;
        end;
        if (Abs((Position.X + round(Width / 2)) - (FSelected.Position.X + round(FSelected.Width / 2))) < SnapLineSize) then
        begin
          if AllowChangePosition then
          begin
            FMousePos.X := FMousePos.X + ((Position.X + round(Width / 2)) - (FSelected.Position.X + round(FSelected.Width / 2)));
            FSelected.Position.X := (Position.X + round(Width / 2) - round(FSelected.Width / 2));
          end;
          AddGridLine(TvgVisualObject(FSelected.Parent.Children[i]));
          Continue;
        end;
        if (Abs(Position.X - FSelected.Position.X) < SnapLineSize) then
        begin
          if AllowChangePosition then
          begin
            FMousePos.X := FMousePos.X + (Position.X - FSelected.Position.X);
            FSelected.Position.X := Position.X;
          end;
          AddGridLine(TvgVisualObject(FSelected.Parent.Children[i]));
          Continue;
        end;
        if (Abs(Position.Y - FSelected.Position.Y) < SnapLineSize) then
        begin
          if AllowChangePosition then
          begin
            FMousePos.Y := FMousePos.Y + (Position.Y - FSelected.Position.Y);
            FSelected.Position.Y := Position.Y;
          end;
          AddGridLine(TvgVisualObject(FSelected.Parent.Children[i]));
          Continue;
        end;
        if (Abs(Position.X - (FSelected.Position.X + FSelected.Width)) < SnapLineSize) then
        begin
          if AllowChangePosition then
          begin
            FMousePos.X := FMousePos.X + (Position.X - (FSelected.Position.X + FSelected.Width));
            FSelected.Position.X := Position.X - FSelected.Width;
          end;
          AddGridLine(TvgVisualObject(FSelected.Parent.Children[i]));
          Continue;
        end;
        if (Abs(Position.Y - (FSelected.Position.Y + FSelected.Height)) < SnapLineSize) then
        begin
          if AllowChangePosition then
          begin
            FMousePos.Y := FMousePos.Y + (Position.Y - (FSelected.Position.Y + FSelected.Height));
            FSelected.Position.Y := Position.Y - FSelected.Height;
          end;
          AddGridLine(TvgVisualObject(FSelected.Parent.Children[i]));
          Continue;
        end;
        if (Abs((Position.X + Width) - FSelected.Position.X) < SnapLineSize) then
        begin
          if AllowChangePosition then
          begin
            FMousePos.X := FMousePos.X + ((Position.X + Width) - FSelected.Position.X);
            FSelected.Position.X := Position.X + Width;
          end;
          AddGridLine(TvgVisualObject(FSelected.Parent.Children[i]));
          Continue;
        end;
        if (Abs((Position.Y + Height) - FSelected.Position.Y) < SnapLineSize) then
        begin
          if AllowChangePosition then
          begin
            FMousePos.Y := FMousePos.Y + ((Position.Y + Height) - FSelected.Position.Y);
            FSelected.Position.Y := Position.Y + Height;
          end;
          AddGridLine(TvgVisualObject(FSelected.Parent.Children[i]));
          Continue;
        end;
        if (Abs((Position.X + Width) - (FSelected.Position.X + FSelected.Width)) < SnapLineSize) then
        begin
          if AllowChangePosition then
          begin
            FMousePos.X := FMousePos.X + ((Position.X + Width) - (FSelected.Position.X + FSelected.Width));
            FSelected.Position.X := Position.X + Width - FSelected.Width;
          end;
          AddGridLine(TvgVisualObject(FSelected.Parent.Children[i]));
          Continue;
        end;
        if (Abs((Position.Y + Height) - (FSelected.Position.Y + FSelected.Height)) < SnapLineSize) then
        begin
          if AllowChangePosition then
          begin
            FMousePos.Y := FMousePos.Y + ((Position.Y + Height) - (FSelected.Position.Y + FSelected.Height));
            FSelected.Position.Y := Position.Y + Height - FSelected.Height;
          end;
          AddGridLine(TvgVisualObject(FSelected.Parent.Children[i]));
          Continue;
        end;
      end;
    end;
  AddUpdateRectsFromGridLines;
end;

function TvgForm.SnapPointToGridLines(const APoint: TvgPoint): TvgPoint;
var
  i: integer;
begin
  Result := APoint;
  if not DesignSnapToLines then Exit;
  if FSelected = nil then Exit;
  if FSelected.Parent = nil then Exit;
  SnapToGridLines(false);
  if Length(FDesignGridLines) > 0 then
  begin
    Result := FSelected.LocalToAbsolute(APoint);
    Result := TvgVisualObject(FSelected.Parent).AbsoluteToLocal(Result);
    for i := 0 to High(FDesignGridLines) do
    begin
      if Abs(Result.X - FDesignGridLines[i].Position.X) < (4) then
        Result.X := FDesignGridLines[i].Position.X;
      if Abs(Result.Y - FDesignGridLines[i].Position.Y) < (4) then
        Result.Y := FDesignGridLines[i].Position.Y;
      if Abs(Result.X - (FDesignGridLines[i].Position.X + FDesignGridLines[i].Width)) < (4) then
        Result.X := FDesignGridLines[i].Position.X + FDesignGridLines[i].Width;
      if Abs(Result.Y - (FDesignGridLines[i].Position.Y + FDesignGridLines[i].Height)) < (4) then
        Result.Y := FDesignGridLines[i].Position.Y + FDesignGridLines[i].Height;
    end;
    Result := TvgVisualObject(FSelected.Parent).LocalToAbsolute(Result);
    Result := FSelected.AbsolutetoLocal(Result);
  end;
end;

procedure TvgForm.RealignRoot;
begin
  if (FChildren <> nil) and (FChildren.Count > 0) and (TvgObject(FChildren[0]).isVisual) then
    with TvgVisualObject(FChildren[0]) do
    begin
      if (RotateAngle = 90) or (RotateAngle = -90) or (RotateAngle = -270) or (RotateAngle = 270) then
        SetBounds((Self.ClientWidth - Self.ClientHeight) / 2, -(Self.ClientWidth - Self.ClientHeight) / 2,
          Self.ClientHeight / Scale.Y, Self.ClientWidth / Scale.X)
      else
        SetBounds(0, 0, Self.ClientWidth / Scale.X, Self.ClientHeight / Scale.Y);
    end;
end;

procedure TvgForm.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FCaptured) then
    FCaptured := nil;
  if (Operation = opRemove) and (AComponent = FSelected) then
    FSelected := nil;
  if (Operation = opRemove) and (AComponent = FHovered) then
    FHovered := nil;
  if (Operation = opRemove) and (AComponent = FFocused) then
    FFocused := nil;
  if (Operation = opRemove) and (AComponent = FDesignPlaceObject) then
    FDesignPlaceObject := nil;
  if (Operation = opRemove) and (AComponent = FStyle) then
    Style := nil;
end;

procedure TvgForm.SetFill(const Value: TvgBrush);
begin
  FFill := Value;
end;

procedure TvgForm.FillChanged(Sender: TObject);
begin
  SetLength(FUpdateRects, 0);
  AddUpdateRect(vgRect(0, 0, Width, Height));
end;

procedure TvgForm.SetSnapGridShow(const Value: boolean);
begin
  if FSnapGridShow <> Value then
  begin
    FSnapGridShow := Value;
    SetLength(FUpdateRects, 0);
    AddUpdateRect(vgRect(0, 0, Width, Height));
  end;
end;

procedure TvgForm.SetSnapGridSize(const Value: single);
begin
  if FSnapGridSize <> Value then
  begin
    FSnapGridSize := Value;
    if FSnapGridSize < 0.01 then
      FSnapGridSize := 0.01;
    if FsnapGridShow then
//      Repaint;
  end;
end;

procedure TvgForm.InsertObject(const ClassName: string);
var
  Obj: TvgObject;
  InsertPos: TvgPoint;
begin
  if GetClass(ClassName) <> nil then
  begin
    if GetClass(ClassName).InheritsFrom(TvgObject) then
    begin
      if FSelected <> nil then
      begin
        try
          Obj := TvgObjectClass(GetClass(ClassName)).Create(Owner);
          if vgDesigner <> nil then
            Obj.Name := vgDesigner.UniqueName(Owner, Obj.ClassName);
          FSelected.AddObject(Obj);
          if vgDesigner <> nil then
          begin
            vgDesigner.SelectObject(Owner, Obj, []);
            if Assigned(FDesignChangeSelection) then
              FDesignChangeSelection(Self);
            vgDesigner.Modified(Owner);
          end;
          if Obj.IsVisual then
          begin
            if GetPropInfo(Obj.ClassInfo, 'Text', [tkString, tkLString, tkWString]) <> nil then
              SetStrProp(Obj, 'Text', Copy(Obj.ClassName, 4, Length(Obj.ClassName)));
            FPopupPos := vgPoint(-1, -1);
            FSelected := TvgVisualObject(Obj);
            TvgHackVisualObject(FSelected).DesignSelect;
            TvgHackVisualObject(FSelected).DesignInsert;
          end;
        except
        end;
      end
      else
        if (FChildren = nil) or (FChildren.Count = 0) then
        begin
          { insert root object }
          try
            Obj := TvgObjectClass(GetClass(ClassName)).Create(Owner);
            if vgDesigner <> nil then
              Obj.Name := vgDesigner.UniqueName(Owner, Obj.ClassName);
            AddObject(Obj);
            if vgDesigner <> nil then
            begin
              vgDesigner.SelectObject(Owner, Obj, []);
              if Assigned(FDesignChangeSelection) then
                FDesignChangeSelection(Self);
              vgDesigner.Modified(Owner);
            end;
            if Obj.IsVisual then
            begin
              InsertPos := FPopupPos;
              FPopupPos := vgPoint(-1, -1);
              FSelected := TvgVisualObject(Obj);
              TvgHackVisualObject(FSelected).DesignSelect;
            end;
          except
          end;
        end;
    end
    else
    begin
      FInsertObject := ClassName;
    end;
  end
  else
    FInsertObject := '';
end;

function StripHotkey(S: string): string;
begin
  Result := S;
end;

procedure TvgForm.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('DesignSnapGridShow', ReadDesignSnapGridShow, WriteDesignSnapGridShow, true);
  Filer.DefineProperty('DesignSnapToGrid', ReadDesignSnapToGrid, WriteDesignSnapToGrid, true);
  Filer.DefineProperty('DesignSnapToLines', ReadDesignSnapToLines, WriteDesignSnapToLines, true);
end;

procedure TvgForm.ReadDesignSnapGridShow(Reader: TReader);
begin
  FSnapGridShow := Reader.ReadBoolean;
  if not (FDesignTime) then
    FSnapGridShow := false;
end;

procedure TvgForm.ReadDesignSnapToGrid(Reader: TReader);
begin
  FSnapToGrid := Reader.ReadBoolean;
end;

procedure TvgForm.ReadDesignSnapToLines(Reader: TReader);
begin
  FSnapToLines := Reader.ReadBoolean;
end;

procedure TvgForm.WriteDesignSnapGridShow(Writer: TWriter);
begin
  Writer.WriteBoolean(FSnapGridShow);
end;

procedure TvgForm.WriteDesignSnapToGrid(Writer: TWriter);
begin
  Writer.WriteBoolean(FSnapToGrid);
end;

procedure TvgForm.WriteDesignSnapToLines(Writer: TWriter);
begin
  Writer.WriteBoolean(FSnapToLines);
end;

function TvgForm.GetRoot: TvgObject;
var
  i: integer;
begin
  if ChildrenCount > 0 then
  begin
    // Root must be Visual
    for i := 0 to ChildrenCount - 1 do
    begin
      if not Children[i].IsVisual then Continue;
      Result := Children[i];
      Exit;
    end;
  end;
  Result := nil;
end;

procedure TvgForm.SetFocused(const Value: TvgVisualObject);
begin
  if FFocused <> Value then
  begin
    if FFocused <> nil then
    begin
      TvgHackVisualObject(FFocused).KillFocus;
      TvgHackVisualObject(FFocused).RemoveFreeNotify(Self);
    end;
    FFocused := Value;
    if FFocused <> nil then
    begin
      TvgHackvisualObject(FFocused).EnterFocus;
      TvgHackVisualObject(FFocused).AddFreeNotify(Self);
    end;
  end;
end;

procedure TvgForm.DoDesignSelect(AObject: TObject);
begin
  if (AObject <> nil) and (AObject is TvgVisualObject) and (TvgVisualObject(AObject).Scene <> nil) and (TvgVisualObject(AObject).Scene.GetComponent = Self) then
  begin
    FSelected := TvgVisualObject(AObject);
    TvgHackvisualObject(FSelected).DesignSelect;
    FSelected.Repaint;
  end;
end;

procedure TvgForm.BeginUpdate;
begin
  FDisableUpdate := true;
end;

procedure TvgForm.EndUpdate;
begin
  FDisableUpdate := false;
  Draw;
end;

procedure TvgForm.EndUpdateWOInvalidate;
begin
  FDisableUpdate := false;
end;

procedure TvgForm.SetSelected(const Value: TvgVisualObject);
begin
  if FSelected <> Value then
  begin
    if FSelected <> nil then
      FSelected.Repaint;
    FSelected := Value;
    if FSelected <> nil then
      FSelected.Repaint;
    if Assigned(FDesignChangeSelection) then
      FDesignChangeSelection(Self);
    AddUpdateRect(vgRect(0, 0, 1000, 1000));
    Draw;
  end;
end;

function TvgForm.GetActiveControl: TvgControl;
begin
  Result := FActiveControl;
end;

function TvgForm.GetDisableUpdate: boolean;
begin
  Result := FDisableUpdate;
end;

function TvgForm.GetDesignTime: boolean;
begin
  Result := FDesignTime;
end;

function TvgForm.GetCanvas: TvgCanvas;
begin
  Result := FCanvas;
end;

function TvgForm.GetOwner: TComponent;
begin
  Result := Owner;
end;

function TvgForm.GetComponent: TComponent;
begin
  Result := Self;
end;

function TvgForm.GetSelected: TvgVisualObject;
begin
  Result := FSelected;
end;

procedure TvgForm.SetDisableUpdate(Value: boolean);
begin
  FDisableUpdate := Value;
end;

function TvgForm.GetUpdateRectsCount: integer;
begin
  Result := Length(FUpdateRects);
end;

function TvgForm.GetUpdateRect(const Index: integer): TvgRect;
begin
  Result := FUpdateRects[Index];
end;

function TvgForm.GetCaptured: TvgVisualObject;
begin
  Result := FCaptured;
end;

procedure TvgForm.SetCaptured(const Value: TvgVisualObject);
begin
  FCaptured := Value;
  if Value <> nil then
    MouseCapture
  else
    ReleaseCapture;
end;

function TvgForm.GetFocused: TvgVisualObject;
begin
  Result := FFocused;
end;

procedure TvgForm.SetDesignRoot(const Value: TvgVisualObject);
begin
  FDesignRoot := Value;
end;

function TvgForm.GetMousePos: TvgPoint;
begin
  Result := FMousePos;
end;

procedure TvgForm.StartWindowDrag;
begin
  FDragging := true;
  FDownPos := FMousePos;
  MouseCapture;
end;

procedure TvgForm.StartWindowResize;
begin
  FResizing := true;
  FDownPos := FMousePos;
//  FResizePos := vgPoint(0, 0);
//  FResizeStartPos := vgPoint(Round(Left - Width / 2), Round(top - Height / 2));
  FResizeSize := vgPoint(Width, Height);
  FDownSize := FResizeSize;
  MouseCapture;
end;

function TvgForm.GetStyle: TvgResources;
begin
  Result := FStyle;
end;

function TvgForm.GetTransparency: boolean;
begin
  Result := Transparency;
end;

function TvgForm.GetDesignPlaceObject: TvgVisualObject;
begin
  Result := FDesignPlaceObject;
end;

procedure TvgForm.UpdateResource;
begin
  if Root <> nil then
    Root.UpdateResource;
end;

procedure TvgForm.SetStyle(const Value: TvgResources);
begin
  if FStyle <> Value then
  begin
    if FStyle <> nil then
      FStyle.RemoveSceneUpdater(Self);
    FStyle := Value;
    if FStyle <> nil then
      FStyle.AddSceneUpdater(Self);

    UpdateResource;
  end;
end;

procedure TvgForm.BeginDrag;
begin

end;

procedure TvgForm.BeginResize;
begin

end;

procedure TvgForm.BeginVCLDrag(Source: TObject; ABitmap: TvgBitmap);
var
  D: TvgDragObject;
begin
  Fillchar(D, SizeOf(D), 0);
  D.Source := Source;
  if Source is TvgObject then
    D.Data := TvgObject(Source).Data;
  Platform.BeginDragDrop(Self, D, ABitmap);
end;

function TvgForm.GetAnimatedCaret: boolean;
begin
  Result := true;
end;

procedure TvgForm.SetActiveControl(AControl: TvgControl);
begin
  if AControl <> FActiveControl then
  begin
    FActiveControl := AControl;
    if (FActiveControl <> nil) and not (csLoading in ComponentState) then
      FActiveControl.SetFocus;
  end;
end;

procedure TvgForm.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;
  if (Canvas <> nil) then
  begin
    Canvas.ResizeBuffer(ClientWidth, ClientHeight);
    if FAlignRoot then
      RealignRoot;
  end;
end;

procedure TvgForm.KeyDown(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState);
var
  List: TList;
  i, CurIdx: integer;
  Found: boolean;
  K: Word;
  Ch: System.WideChar;
begin
  inherited;
  { dialog key }
  if (Key = VK_ESCAPE) or (Key = VK_RETURN) or (Key = VK_LEFT) or (Key = VK_RIGHT) or (Key = VK_UP) or (Key = VK_DOWN) or
     (Key = VK_HOME) or (Key = VK_END) or (Key = VK_ADD) or (Key = VK_SUBTRACT) then
  begin
    TvgHackVisualObject(Root).DialogKey(Key, Shift);
  end;
  { modal }
  if (Key = VK_ESCAPE) and (fsModal in FormState) then
  begin
    ModalResult := mrCancel;
    Key := 0;
    Exit;
  end;
  { change focus }
  if (Key = VK_TAB) and (Root <> nil) then
  begin
    Key := 0;
    List := TList.Create;
    TvgVisualObject(Root).GetTabOrderList(List, true);

    Found := false;
    if ssShift in Shift then
    begin
      { second search in first part of list }
      if FFocused <> nil then
        CurIdx := List.IndexOf(FFocused) - 1
      else
        CurIdx := List.Count - 1;
      if CurIdx > 0 then
          for i := CurIdx - 1 downto 0 do
            if TvgObject(List[i]).isVisual and (TvgVisualObject(List[i]).CheckParentVisible) and (TvgVisualObject(List[i]).CanFocused) and (TvgVisualObject(List[i]).AbsoluteEnabled) then
            begin
              TvgVisualObject(List[i]).SetFocus;
              Found := true;
              Break;
            end;
      { first search in last part of list }
      if not Found then
        if (List.Count > 2) and (CurIdx < List.Count) then
          for i := List.Count - 1 downto CurIdx do
            if TvgObject(List[i]).isVisual and (TvgVisualObject(List[i]).CheckParentVisible) and (TvgVisualObject(List[i]).CanFocused) and (TvgVisualObject(List[i]).AbsoluteEnabled) then
            begin
              TvgVisualObject(List[i]).SetFocus;
              Break;
            end;
    end
    else
    begin
      if FFocused <> nil then
        CurIdx := List.IndexOf(FFocused) + 1
      else
        CurIdx := 0;
      { first search in last part of list }
      if (List.Count > 2) and (CurIdx < List.Count) then
        for i := CurIdx to List.Count - 1 do
          if TvgObject(List[i]).isVisual and (TvgVisualObject(List[i]).CheckParentVisible) and (TvgVisualObject(List[i]).CanFocused) and (TvgVisualObject(List[i]).AbsoluteEnabled) then
          begin
            TvgVisualObject(List[i]).SetFocus;
            Found := true;
            Break;
          end;
      { second search in first part of list }
      if not Found then
        if CurIdx > 0 then
          for i := 0 to CurIdx - 1 do
            if TvgObject(List[i]).isVisual and (TvgVisualObject(List[i]).CheckParentVisible) and (TvgVisualObject(List[i]).CanFocused) and (TvgVisualObject(List[i]).AbsoluteEnabled) then
            begin
              TvgVisualObject(List[i]).SetFocus;
              Break;
            end;
    end;
    List.Free;
    Exit;
  end;
  { focused handler }
  if FFocused <> nil then
  begin
    TvgHackVisualObject(FFocused).KeyDown(Key, KeyChar, Shift);
  end;
end;

procedure TvgForm.KeyUp(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState);
begin
  inherited;
  if FDesignTime then
  begin
    if (Key = VK_DELETE) and (FSelected <> nil) then
    begin
      FSelected.Free;
      FSelected := nil;
    end;
  end;
  { focused handler }
  if FFocused <> nil then
  begin
    TvgHackVisualObject(FFocused).KeyUp(Key, KeyChar, Shift);
  end;
end;

procedure TvgForm.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: single);
var
  P: TvgPoint;
  R: TvgRect;
  Obj: TvgVisualObject;
  SG: IvgSizeGrip;
  i: integer;
begin
  inherited;
  { translate coord }
  FUnsnapMousePos := vgPoint(x, y);
  FMousePos := vgPoint(SnapToGridValue(x), SnapToGridValue(y));
  FDownPos := FMousePos;
  SetLength(FDesignGridLines, 0);
  { design }
  if FDesignTime then
  begin
    { Create root }
    if ((FChildren = nil) or (FChildren.Count = 0)) then
    begin
      Obj := TvgBackground.Create(Owner);
      if vgDesigner <> nil then
        Obj.Name := vgDesigner.UniqueName(Owner, 'Root');
      AddObject(Obj);
      RealignRoot;
    end;
    { Popup }
{    if Button = mbRight then
      OpenDesignPopup;}
    { Resize }
    if (FSelected <> nil) and (not FSelected.DisableDesignResize) then
    begin
      P := FSelected.AbsoluteToLocal(vgPoint(FUnsnapMousePos.X, FUnsnapMousePos.Y));
      R := vgRect(FSelected.Width / 2 - (GripSize), - RotSize - (GripSize),
          (FSelected.Width) / 2 +(GripSize), - RotSize + (GripSize));
      if vgPtInRect(P, R) then
      begin
        if vgDesigner <> nil then
          vgDesigner.SelectObject(Owner, FSelected, []);
        if Assigned(FDesignChangeSelection) then
          FDesignChangeSelection(Self);
        FRotate := true;
        FMoving := false;
        Exit;
      end;
      P := FSelected.AbsoluteToLocal(vgPoint(FUnsnapMousePos.X, FUnsnapMousePos.Y));
      R := vgRect(-GripSize, -GripSize, GripSize, GripSize);
      if vgPtInRect(P, R) then
      begin
        if vgDesigner <> nil then
          vgDesigner.SelectObject(Owner, FSelected, []);
        if Assigned(FDesignChangeSelection) then
          FDesignChangeSelection(Self);
        FLeftTop := true;
        FMoving := false;
        Exit;
      end;
      R := FSelected.BoundsRect;
      R := vgRect(R.Right -GripSize, -GripSize, R.Right + GripSize, GripSize);
      if vgPtInRect(P, R) then
      begin
        if vgDesigner <> nil then
          vgDesigner.SelectObject(Owner, FSelected, []);
        if Assigned(FDesignChangeSelection) then
          FDesignChangeSelection(Self);
        FRightTop := true;
        FMoving := false;
        Exit;
      end;
      R := FSelected.BoundsRect;
      R := vgRect(-GripSize, R.Bottom - GripSize, GripSize, R.Bottom + GripSize);
      if vgPtInRect(P, R) then
      begin
        if vgDesigner <> nil then
          vgDesigner.SelectObject(Owner, FSelected, []);
        if Assigned(FDesignChangeSelection) then
          FDesignChangeSelection(Self);
        FLeftBottom := true;
        FMoving := false;
        Exit;
      end;
      R := FSelected.BoundsRect;
      R := vgRect(R.Right - GripSize, R.Bottom - GripSize, R.Right + GripSize, R.Bottom + GripSize);
      if vgPtInRect(P, R) then
      begin
        if vgDesigner <> nil then
          vgDesigner.SelectObject(Owner, FSelected, []);
        if Assigned(FDesignChangeSelection) then
          FDesignChangeSelection(Self);
        FRightBottom := true;
        FMoving := false;
        Exit;
      end;

      R := FSelected.BoundsRect;
      R := vgRect(vgRectWidth(R)/2 - GripSize, -GripSize, vgRectWidth(R)/2 + GripSize, GripSize);
      if vgPtInRect(P, R) then
      begin
        if vgDesigner <> nil then
          vgDesigner.SelectObject(Owner, FSelected, []);
        if Assigned(FDesignChangeSelection) then
          FDesignChangeSelection(Self);
        FTop := true;
        FMoving := false;
        Exit;
      end;
      R := FSelected.BoundsRect;
      R := vgRect(vgRectWidth(R)/2 - GripSize, R.Bottom - GripSize, vgRectWidth(R)/2 + GripSize, R.Bottom + GripSize);
      if vgPtInRect(P, R) then
      begin
        if vgDesigner <> nil then
          vgDesigner.SelectObject(Owner, FSelected, []);
        if Assigned(FDesignChangeSelection) then
          FDesignChangeSelection(Self);
        FBottom := true;
        FMoving := false;
        Exit;
      end;
      R := FSelected.BoundsRect;
      R := vgRect(-GripSize, vgRectHeight(R)/2 - GripSize, GripSize, vgRectHeight(R)/2 + GripSize);
      if vgPtInRect(P, R) then
      begin
        if vgDesigner <> nil then
          vgDesigner.SelectObject(Owner, FSelected, []);
        if Assigned(FDesignChangeSelection) then
          FDesignChangeSelection(Self);
        FLeft := true;
        FMoving := false;
        Exit;
      end;
      R := FSelected.BoundsRect;
      R := vgRect(R.Right-GripSize, vgRectHeight(R)/2 - GripSize, R.Right + GripSize, vgRectHeight(R)/2 + GripSize);
      if vgPtInRect(P, R) then
      begin
        if vgDesigner <> nil then
          vgDesigner.SelectObject(Owner, FSelected, []);
        if Assigned(FDesignChangeSelection) then
          FDesignChangeSelection(Self);
        FRight := true;
        FMoving := false;
        Exit;
      end;
    end;
    { Change Selected }
    Obj := ObjectByPoint(FUnsnapMousePos.X, FUnsnapMousePos.Y);
    if (Obj <> nil) and (Obj = FSelected) and (ssDouble in Shift) then
    begin
      TvgHackVisualObject(Obj).DesignClick;
    end;
    if (Obj <> nil) then
    begin
      if (ssCtrl in Shift) and (Obj <> Selected) then
      begin
        { check is exists }
        for i := 0 to High(FSelection) do
          if FSelection[i] = Obj then
          begin
            FSelection[i] := Selected;
            Obj := nil;
          end;
        if Obj <> nil then
        begin
          SetLength(FSelection, Length(FSelection) + 1);
          FSelection[High(FSelection)] := Obj;
        end;
        if vgDesigner <> nil then
          vgDesigner.SelectObject(Owner, Selected, FSelection);
        Exit;
      end;
      SetLength(FSelection, 0);
      if FSelected <> nil then
      begin
        TvgHackVisualObject(FSelected).RecalcUpdateRect;
        TvgHackVisualObject(FSelected).Repaint;
      end;
      FSelected := Obj;
      TvgHackVisualObject(FSelected).DesignSelect;
      { Select in IDE }
      if vgDesigner <> nil then
        vgDesigner.SelectObject(Owner, Obj, []);
      if Assigned(FDesignChangeSelection) then
        FDesignChangeSelection(Self);
      { }
      TvgHackVisualObject(FSelected).RecalcUpdateRect;
      FSelected.Repaint;

      if (Obj = FSelected) then
        FMoving := true;
    end;
    Exit;
  end;
  { event }
  if not FDesignTime then
  begin
    Obj := TvgVisualObject(ObjectByPoint(FMousePos.X, FMousePos.Y));
    if (Obj <> nil) then
    begin
      if (TvgHackVisualObject(Obj).QueryInterface(IvgSizeGrip, SG) = 0) and (Assigned(SG)) then
      begin
        StartWindowResize;
      end
      else
      begin
        P := Obj.AbsoluteToLocal(vgPoint(FMousePos.X, FMousePos.Y));
        TvgHackVisualObject(Obj).MouseDown(Button, Shift, P.X, P.Y);
        if (TvgVisualObject(Obj).DragMode = vgDragAutomatic) then
          TvgHackVisualObject(Obj).BeginAutoDrag;
      end;
    end
    else
      StartWindowDrag;
  end;
end;

procedure TvgForm.MouseMove(Shift: TShiftState; X, Y: single);
var
  R: TvgRect;
  P, P1: TvgPoint;
  Obj: TvgVisualObject;
  SG: IvgSizeGrip;
  NewCursor: TCursor;
begin
  inherited ;
  NewCursor := FLoadCursor;
  { drag }
  if FDragging then
  begin
    SetBounds(Round(Left + (X - FDownPos.X)), Round(Top + (Y - FDownPos.Y)), Width, Height);
    Exit;
  end;
  if FResizing then
  begin
    FResizeSize.X := Round(FResizeSize.X + (X - FUnsnapMousePos.X));
    FResizeSize.Y := Round(FResizeSize.Y + (Y - FUnsnapMousePos.Y));
    SetBounds(Round(Left), Round(Top), Round(FResizeSize.X), Round(FResizeSize.Y));
//    Cursor := crSizeNWSE;
    FUnsnapMousePos := vgPoint(x, y);
    Exit;
  end;
  { translate coord }
  FMousePos := vgPoint(SnapToGridValue(x), SnapToGridValue(y));
  FUnsnapMousePos := vgPoint(x, y);
  { design }
  if FDesignTime then
  begin
    { change cursor }
    if (FSelected <> nil) then
    begin
      P := FSelected.AbsoluteToLocal(vgPoint(FUnsnapMousePos.X, FUnsnapMousePos.Y));
      R := vgRect(FSelected.Width / 2 - (GripSize), - RotSize - (GripSize),
        (FSelected.Width) / 2 +(GripSize), - RotSize + (GripSize));
      if FRotateHot <> vgPtInRect(P, R) then
      begin
        FRotateHot := vgPtInRect(P, R);
        R.TopLeft := FSelected.LocaltoAbsolute(R.TopLeft);
        R.BottomRight := FSelected.LocaltoAbsolute(R.BottomRight);
        vgInflateRect(R, 2, 2);
        AddUpdateRect(R);
      end;
      P := FSelected.AbsoluteToLocal(vgPoint(FUnsnapMousePos.X, FUnsnapMousePos.Y));
      R := vgRect(-GripSize, -GripSize, GripSize, GripSize);
      if FLeftTopHot <> vgPtInRect(P, R) then
      begin
        FLeftTopHot := vgPtInRect(P, R);
        R.TopLeft := FSelected.LocaltoAbsolute(R.TopLeft);
        R.BottomRight := FSelected.LocaltoAbsolute(R.BottomRight);
        vgInflateRect(R, 2, 2);
        AddUpdateRect(R);
      end;
      R := FSelected.BoundsRect;
      R := vgRect(R.Right -GripSize, -GripSize, R.Right + GripSize, GripSize);
      if FRightTopHot <> vgPtInRect(P, R) then
      begin
        FRightTopHot := vgPtInRect(P, R);
        R.TopLeft := FSelected.LocaltoAbsolute(R.TopLeft);
        R.BottomRight := FSelected.LocaltoAbsolute(R.BottomRight);
        vgInflateRect(R, 2, 2);
        AddUpdateRect(R);
      end;
      R := FSelected.BoundsRect;
      R := vgRect(-GripSize, R.Bottom - GripSize, GripSize, R.Bottom + GripSize);
      if FLeftBottomHot <> vgPtInRect(P, R) then
      begin
        FLeftBottomHot := vgPtInRect(P, R);
        R.TopLeft := FSelected.LocaltoAbsolute(R.TopLeft);
        R.BottomRight := FSelected.LocaltoAbsolute(R.BottomRight);
        vgInflateRect(R, 2, 2);
        AddUpdateRect(R);
      end;
      R := FSelected.BoundsRect;
      R := vgRect(R.Right - GripSize, R.Bottom - GripSize, R.Right + GripSize, R.Bottom + GripSize);
      if FRightBottomHot <> vgPtInRect(P, R) then
      begin
        FRightBottomHot := vgPtInRect(P, R);
        R.TopLeft := FSelected.LocaltoAbsolute(R.TopLeft);
        R.BottomRight := FSelected.LocaltoAbsolute(R.BottomRight);
        vgInflateRect(R, 2, 2);
        AddUpdateRect(R);
      end;

      R := FSelected.BoundsRect;
      R := vgRect(vgRectWidth(R)/2 - GripSize, -GripSize, vgRectWidth(R)/2 + GripSize, GripSize);
      if FTopHot <> vgPtInRect(P, R) then
      begin
        FTopHot := vgPtInRect(P, R);
        R.TopLeft := FSelected.LocaltoAbsolute(R.TopLeft);
        R.BottomRight := FSelected.LocaltoAbsolute(R.BottomRight);
        vgInflateRect(R, 2, 2);
        AddUpdateRect(R);
      end;
      R := FSelected.BoundsRect;
      R := vgRect(vgRectWidth(R)/2 - GripSize, R.Bottom - GripSize, vgRectWidth(R)/2 + GripSize, R.Bottom + GripSize);
      if FBottomHot <> vgPtInRect(P, R) then
      begin
        FBottomHot := vgPtInRect(P, R);
        R.TopLeft := FSelected.LocaltoAbsolute(R.TopLeft);
        R.BottomRight := FSelected.LocaltoAbsolute(R.BottomRight);
        vgInflateRect(R, 2, 2);
        AddUpdateRect(R);
      end;
      R := FSelected.BoundsRect;
      R := vgRect(-GripSize, vgRectHeight(R)/2 - GripSize, GripSize, vgRectHeight(R)/2 + GripSize);
      if FLeftHot <> vgPtInRect(P, R) then
      begin
        FLeftHot := vgPtInRect(P, R);
        R.TopLeft := FSelected.LocaltoAbsolute(R.TopLeft);
        R.BottomRight := FSelected.LocaltoAbsolute(R.BottomRight);
        vgInflateRect(R, 2, 2);
        AddUpdateRect(R);
      end;
      R := FSelected.BoundsRect;
      R := vgRect(R.Right-GripSize, vgRectHeight(R)/2 - GripSize, R.Right + GripSize, vgRectHeight(R)/2 + GripSize);
      if FRightHot <> vgPtInRect(P, R) then
      begin
        FRightHot := vgPtInRect(P, R);
        R.TopLeft := FSelected.LocaltoAbsolute(R.TopLeft);
        R.BottomRight := FSelected.LocaltoAbsolute(R.BottomRight);
        vgInflateRect(R, 2, 2);
        AddUpdateRect(R);
      end;
    end;
    { resize and move }
    if (ssLeft in Shift) and (FSelected <> nil) and (FMoving) then
    begin
      P := vgPoint(FUnsnapMousePos.X - FDownPos.X, FUnsnapMousePos.Y - FDownPos.Y);
      if (FSelected.Parent <> nil) and (FSelected.Parent.IsVisual) then
      begin
        with TvgVisualObject(FSelected.Parent).AbsoluteToLocalVector(vgVector(P.X, P.Y)) do
          P := vgPoint(X, Y);
      end
      else
      begin
        with FSelected.AbsoluteToLocalVector(vgVector(P.X, P.Y)) do
          P := vgPoint(X, Y);
      end;
      FSelected.Position.X := SnapToGridValue(FSelected.Position.X + P.X);
      FSelected.Position.Y := SnapToGridValue(FSelected.Position.Y + P.Y);
      { lines grid }
      SnapToGridLines(true);
      { check place }
      TvgHackVisualObject(FSelected).FLocked := true;
      Obj := TvgVisualObject(ObjectByPoint(FUnsnapMousePos.X, FUnsnapMousePos.Y));
      TvgHackVisualObject(FSelected).FLocked := false;
      { select }
      if (Obj <> nil) and (Obj <> FSelected.Parent) and (Obj <> FSelected) then
      begin
        if FDesignPlaceObject <> nil then
          FDesignPlaceObject.Repaint;
        FDesignPlaceObject := Obj;
        if FDesignPlaceObject <> nil then
        begin
          TvgHackVisualObject(FDesignPlaceObject).RecalcUpdateRect;
          FDesignPlaceObject.Repaint;
        end;
        if (ssAlt in Shift) then
        begin
          P := FSelected.LocalToAbsolute(vgPoint(0, 0));
          FSelected.Parent := FDesignPlaceObject;
          P := FDesignPlaceObject.AbsoluteToLocal(P);
          FSelected.Position.X := P.X;
          FSelected.Position.Y := P.Y;
        end;
      end
      else
      begin
        if FDesignPlaceObject <> nil then
          FDesignPlaceObject.Repaint;
        FDesignPlaceObject := nil;
      end;
    end;
    if (ssLeft in Shift) and (FSelected <> nil) then
    begin
      if (ssLeft in Shift) and (FSelected <> nil) and (FRotate) then
      begin
        R := FSelected.BoundsRect;
        P := FSelected.AbsoluteToLocal(FMousePos);
        P1 := FSelected.AbsoluteToLocal(FDownPos);
        if vgVectorCrossProductZ(vgVector(P.X - (FSelected.Width / 2), P.Y - (FSelected.Height / 2)),
          vgVector(P1.X - (FSelected.Width / 2), P1.Y - (FSelected.Height / 2))) < 0
        then
          FSelected.RotateAngle := FSelected.RotateAngle + vgRadToDeg(ArcCos(vgVectorAngleCosine(vgVector(P.X - (FSelected.Width / 2), P.Y - (FSelected.Height / 2)),
            vgVector(P1.X - (FSelected.Width / 2), P1.Y - (FSelected.Height / 2)))))
        else
          FSelected.RotateAngle := FSelected.RotateAngle - vgRadToDeg(ArcCos(vgVectorAngleCosine(vgVector(P.X - (FSelected.Width / 2), P.Y - (FSelected.Height / 2)),
            vgVector(P1.X - (FSelected.Width / 2), P1.Y - (FSelected.Height / 2)))));
      end;
      if (ssLeft in Shift) and (FSelected <> nil) and (FLeftTop) then
      begin
        R := FSelected.BoundsRect;
        P := FSelected.AbsoluteToLocal(FMousePos);
        P := SnapPointToGridLines(P);
        FSelected.BoundsRect := vgRect(P.X, P.Y,
          R.Right, R.Bottom);
      end;
      if (ssLeft in Shift) and (FSelected <> nil) and (FRightTop) then
      begin
        R := FSelected.BoundsRect;
        P := FSelected.AbsoluteToLocal(FMousePos);
        P := SnapPointToGridLines(P);
        FSelected.BoundsRect := vgRect(R.Left, P.Y,
          P.X, R.Bottom);
      end;
      if (ssLeft in Shift) and (FSelected <> nil) and (FLeftBottom) then
      begin
        R := FSelected.BoundsRect;
        P := FSelected.AbsoluteToLocal(FMousePos);
        P := SnapPointToGridLines(P);
        FSelected.BoundsRect := vgRect(P.X, R.Top,
          R.Right, P.Y);
      end;
      if (ssLeft in Shift) and (FSelected <> nil) and (FRightBottom) then
      begin
        R := FSelected.BoundsRect;
        P := FSelected.AbsoluteToLocal(FMousePos);
        P := SnapPointToGridLines(P);
        FSelected.BoundsRect := vgRect(R.Left, R.Top, P.X, P.Y);
      end;

      if (ssLeft in Shift) and (FSelected <> nil) and (FTop) then
      begin
        R := FSelected.BoundsRect;
        P := FSelected.AbsoluteToLocal(FMousePos);
        P := SnapPointToGridLines(P);
        FSelected.BoundsRect := vgRect(R.Left, P.Y, R.Right, R.Bottom);
      end;
      if (ssLeft in Shift) and (FSelected <> nil) and (FBottom) then
      begin
        R := FSelected.BoundsRect;
        P := FSelected.AbsoluteToLocal(FMousePos);
        P := SnapPointToGridLines(P);
        FSelected.BoundsRect := vgRect(R.Left, R.Top, R.Right, P.Y);
      end;
      if (ssLeft in Shift) and (FSelected <> nil) and (FLeft) then
      begin
        R := FSelected.BoundsRect;
        P := FSelected.AbsoluteToLocal(FMousePos);
        P := SnapPointToGridLines(P);
        FSelected.BoundsRect := vgRect(P.X, R.Top, R.Right, R.Bottom);
      end;
      if (ssLeft in Shift) and (FSelected <> nil) and (FRight) then
      begin
        R := FSelected.BoundsRect;
        P := FSelected.AbsoluteToLocal(FMousePos);
        P := SnapPointToGridLines(P);
        FSelected.BoundsRect := vgRect(R.Left, R.Top, P.X, R.Bottom);
      end;
      FDownPos := FMousePos;
    end;
    Exit;
  end;
  { event }
  if not FDesignTime then
  begin
    if (FCaptured <> nil) then
    begin
      P := FCaptured.AbsoluteToLocal(vgPoint(FMousePos.X, FMousePos.Y));
      TvgHackVisualObject(FCaptured).MouseMove(Shift, P.X, P.Y, 0, 0);
      Exit;
    end;
    Obj := TvgVisualObject(ObjectByPoint(FMousePos.X, FMousePos.Y));
    if (Obj <> nil) then
    begin
{      if ((Obj.QueryInterface(IvgSizeGrip, SG) = 0) and Assigned(SG)) then
        NewCursor := crSizeNWSE
      else}
        NewCursor := Obj.Cursor;

      if (Obj <> FHovered) then
      begin
        if FHovered <> nil then
        begin
          TvgHackVisualObject(FHovered).MouseInObject := false;
          TvgHackVisualObject(FHovered).MouseLeave;
          TvgHackVisualObject(FHovered).RemoveFreeNotify(Self);
        end;
        FHovered := Obj;
        TvgHackVisualObject(FHovered).MouseInObject := true;
        TvgHackVisualObject(FHovered).MouseEnter;
        TvgHackVisualObject(FHovered).AddFreeNotify(Self);
      end;

      P := Obj.AbsoluteToLocal(vgPoint(FMousePos.X, FMousePos.Y));
      TvgHackVisualObject(Obj).MouseMove(Shift, P.X, P.Y, 0, 0);
    end
    else
    begin
      if FHovered <> nil then
      begin
        TvgHackVisualObject(FHovered).MouseInObject := false;
        TvgHackVisualObject(FHovered).MouseLeave;
        TvgHackVisualObject(FHovered).RemoveFreeNotify(Self);
        FHovered := nil;
      end;
    end;
  end;
//  Cursor := NewCursor;
  FDownPos := FMousePos;
end;

procedure TvgForm.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: single);
var
  P: TvgPoint;
  Obj: TvgVisualObject;
begin
  inherited;
  { design }
  if FDesignPlaceObject <> nil then
    FDesignPlaceObject.Repaint;
  FDesignPlaceObject := nil;
  AddUpdateRectsFromGridLines;
  SetLength(FDesignGridLines, 0);
  if (FSelected <> nil) and FMoving then
  begin
    if (FSelected.Parent <> nil) and (TvgObject(FSelected.Parent).IsVisual) then
      TvgVisualObject(FSelected.Parent).Realign;
    if (vgDesigner <> nil) then
      vgDesigner.Modified(Owner);
    if FSelected.Parent = nil then
      RealignRoot;
  end;
  if (FSelected <> nil) and (FLeftTop or FLeftBottom or FLeftBottom or FRightBottom) then
  begin
    if (FSelected.Parent <> nil) and (TvgObject(FSelected.Parent).IsVisual) then
      TvgVisualObject(FSelected.Parent).Realign;
    if (vgDesigner <> nil) then
      vgDesigner.Modified(Owner);
    if FSelected.Parent = nil then
      RealignRoot;
  end;
  FMoving := false;
  FLeftTop := false;
  FLeftBottom := false;
  FRightTop := false;
  FRightBottom := false;
  FTop := false;
  FBottom := false;
  FLeft := false;
  FRight := false;
  FRotate := false;
  { drag }
  if FDragging then
  begin
    FDragging := false;
    ReleaseCapture;
  end;
  if FResizing then
  begin
    FResizing := false;
    ReleaseCapture;
  end;
  { event }
  if not FDesignTime then
  begin
    if (FCaptured <> nil) then
    begin
      P := FCaptured.AbsoluteToLocal(vgPoint(FMousePos.X, FMousePos.Y));
      TvgHackVisualObject(FCaptured).MouseUp(Button, Shift, P.X, P.Y);
      Exit;
    end;
    Obj := TvgVisualObject(ObjectByPoint(FMousePos.X, FMousePos.Y));
    if (Obj <> nil) then
    begin
      P := Obj.AbsoluteToLocal(vgPoint(FMousePos.X, FMousePos.Y));
      TvgHackVisualObject(Obj).MouseUp(Button, Shift, P.X, P.Y);
    end;
  end;
end;

procedure TvgForm.MouseWheel(Shift: TShiftState; WheelDelta: integer; var Handled: boolean);
var
  Obj: TvgVisualObject;
  MousePos: TvgPoint;
begin
  MousePos := ScreenToClient(Platform.GetMousePos);
  { event }
  if not FDesignTime then
  begin
    if (FCaptured <> nil) then
    begin
      TvgHackVisualObject(FCaptured).MouseWheel(Shift, WheelDelta, Handled);
      Exit;
    end;
    Obj := TvgHackVisualObject(ObjectByPoint(FMousePos.X, FMousePos.Y));
    while (Obj <> nil) do
    begin
      TvgHackVisualObject(Obj).MouseWheel(Shift, WheelDelta, Handled);
      if Handled then Break;
      if (Obj.Parent <> nil) and (Obj.Parent.IsVisual) then
        Obj := TvgHackVisualObject(Obj.Parent)
      else
        Obj := nil;
    end;
  end;
end;

function TvgForm.LocalToScreen(const Point: TvgPoint): TvgPoint;
begin
  Result := ClientToScreen(Point);
end;

function TvgForm.ShowKeyboardForControl(AObject: TvgObject): boolean;
begin
  Result := Platform.ShowVirtualKeyboard(AObject);
end;

function TvgForm.HideKeyboardForControl(AObject: TvgObject): boolean;
begin
  Result := Platform.HideVirtualKeyboard;
end;

procedure TvgForm.DragDrop(const Data: TvgDragObject;
  const Point: TvgPoint);
begin
  if FTarget <> nil then
  begin
    TvgHackVisualObject(FTarget).DragDrop(Data, Point);
  end;
end;

procedure TvgForm.DragEnter(const Data: TvgDragObject;
  const Point: TvgPoint);
var
  NewTarget: TvgVisualObject;
begin
  NewTarget := Root.Visual.FindTarget(Point, Data);
  if (NewTarget <> FTarget) then
  begin
    if FTarget <> nil then
      TvgHackVisualObject(FTarget).DragLeave;
    FTarget := NewTarget;
    if FTarget <> nil then
    begin
      TvgHackVisualObject(FTarget).DragEnter(Data, Point);
    end;
  end;
end;

procedure TvgForm.DragLeave;
begin
  if FTarget <> nil then
    TvgHackVisualObject(FTarget).DragLeave;
  FTarget := nil;
end;

procedure TvgForm.DragOver(const Data: TvgDragObject; const Point: TvgPoint; var Accept: Boolean);
var
  NewTarget: TvgVisualObject;
begin
  NewTarget := Root.Visual.FindTarget(Point, Data);
  if (NewTarget <> FTarget) then
  begin
    if FTarget <> nil then
      TvgHackVisualObject(FTarget).DragLeave;
    FTarget := NewTarget;
    if FTarget <> nil then
    begin
      TvgHackVisualObject(FTarget).DragEnter(Data, Point);
    end;
  end;
  if FTarget <> nil then
  begin
    TvgHackVisualObject(FTarget).DragOver(Data, Point, Accept);
  end;
end;

procedure TvgForm.SetIsActive(const Value: boolean);
begin
  inherited ;
  if Root <> nil then
  begin
    Root.ApplyTriggerEffect(Self, 'IsActive');
    Root.StartTriggerAnimation(Self, 'IsActive');
  end;
end;

{ TvgApplication }

constructor TvgApplication.Create(AOwner: TComponent);
begin
  inherited;
  if not Assigned(Classes.ApplicationHandleException) then
    Classes.ApplicationHandleException := HandleException;
  if not Assigned(Classes.ApplicationShowException) then
    Classes.ApplicationShowException := ShowException;
end;

procedure TvgApplication.CreateForm(InstanceClass: TComponentClass;
  var Reference);
begin
  SetLength(FCreateForms, Length(FCreateForms) + 1);
  FCreateForms[High(FCreateForms)].InstanceClass := InstanceClass;
  FCreateForms[High(FCreateForms)].Reference := @Reference;
end;

procedure TvgApplication.RealCreateForms;
var
  Instance: TComponent;
  i: integer;
begin
  // only one form are created
  if Length(FCreateForms) > 0 then
  begin
    for i := 0 to High(FCreateForms) do
    begin
      Instance := TComponent(FCreateForms[i].InstanceClass.NewInstance);
      TComponent(FCreateForms[i].Reference^) := Instance;
      try
        Instance.Create(Self);
      except
        TComponent(FCreateForms[i].Reference^) := nil;
        raise;
      end;
      if (FMainForm = nil) and (Instance is TForm) then
      begin
        FMainForm := TForm(Instance);
        FMainForm.Visible := true;
      end;
    end;
  end;
end;

destructor TvgApplication.Destroy;
type
  TExceptionEvent = procedure (E: Exception) of object;
var
  P: TNotifyEvent;
  E: TExceptionEvent;
begin
  Classes.WakeMainThread := nil;
  P := HandleException;
  if @P = @Classes.ApplicationHandleException then
    Classes.ApplicationHandleException := nil;
  E := ShowException;
  if @E = @Classes.ApplicationShowException then
    Classes.ApplicationShowException := nil;
  if FMainForm <> nil then
    FMainForm.Free;
  inherited;
end;

procedure TvgApplication.HandleException(Sender: TObject);
begin
end;

procedure TvgApplication.ShowException(E: Exception);
begin
end;

function TvgApplication.HandleMessage: boolean;
begin
  Result := Platform.HandleMessage;
  if not Result then Idle;
end;

procedure TvgApplication.Idle;
var
  Done: Boolean;
begin
  Done := true;
  try
    if Assigned(FOnIdle) then FOnIdle(Self, Done);
    if Done then
    begin
      // App idle
    end;
  except
    HandleException(Self);
  end;
  if (GetCurrentThreadID = MainThreadID) and CheckSynchronize then
    Done := false;
  if Done then
    Platform.WaitMessage;
end;

procedure TvgApplication.Run;
begin
  FRunning := True;
  try
    Platform.Run;
  finally
    FRunning := False;
  end;
end;

procedure TvgApplication.Terminate;
begin
  FTerminate := true;
  if CallTerminateProcs then
  begin
    Platform.Terminate;
  end;
end;

procedure TvgApplication.Initialize;
begin

end;

procedure TvgApplication.ProcessMessages;
begin
  while HandleMessage do {loop};
end;

{ TvgScene }

constructor TvgScene.Create(AOwner: TComponent);
begin
  inherited;
  FFill := TvgBrush.Create(vgBrushNone, $FF000000);
  HitTest := false;
end;

destructor TvgScene.Destroy;
begin
  FFill.Free;
  inherited;
end;

procedure TvgScene.AddObject(AObject: TvgObject);
begin
  if FForm <> nil then
  begin
    FForm.AddObject(AObject);
    FForm.Transparency := FTransparency;
    FForm.Fill.Assign(FFill);
    FForm.Style := FStyle;
    FForm.ActiveControl := FActiveControl;
    Exit;
  end;
  inherited;
  if AObject is TvgVisualObject then
  begin
    TvgVisualObject(AObject).Align := vaContents;
  end;
end;

procedure TvgScene.ReadBool(Reader: TReader);
begin
  Reader.ReadBoolean;
end;

procedure TvgScene.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('DesignSnapGridShow', ReadBool, nil, false);
  Filer.DefineProperty('DesignSnapToGrid', ReadBool, nil, false);
  Filer.DefineProperty('DesignSnapToLines', ReadBool, nil, false);
end;

procedure TvgScene.SetFill(const Value: TvgBrush);
begin

end;

procedure TvgScene.SetStyle(const Value: TvgResources);
begin
  FStyle := Value;
  if FForm <> nil then
    FForm.Style := FStyle;
end;

procedure TvgScene.SetShowUpdateRects(const Value: boolean);
begin
  FShowUpdateRects := Value;
  if FForm <> nil then
    FForm.ShowUpdateRects := ShowUpdateRects;
end;

initialization
  RegisterClasses([TvgApplication, TvgScene, TFont]);
  RegisterVGObjects('System', [TvgTimer]);
  Platform := DefaultPlatformClass.Create(nil);
finalization
  Platform.Free;
end.