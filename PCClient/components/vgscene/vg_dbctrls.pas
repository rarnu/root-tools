unit vg_dbctrls;

{$I vg_define.inc}

interface

uses
  {$IFDEF DARWIN}
  macosall,
  CarbonProc, CarbonDef, CarbonPrivate, carboncanvas,
  {$ENDIF}
  {$IFDEF FPC}
  LCLProc, LCLIntf, LCLType, LMessages, LResources,
  {$ENDIF}
  {$IFNDEF FPC}
  Windows, Messages,
  {$ENDIF}
  Classes, Variants, Forms, Controls, Graphics, SysUtils, ExtCtrls, Dialogs, DB, DBCtrls, vg_listbox,
  vg_scene, vg_objects, vg_ani, vg_layouts, vg_controls, vg_textbox, vg_grid, vg_memo;

const
  InitRepeatPause = 400;  { pause before repeat timer (ms) }
  RepeatPause     = 100;  { pause before hint window displays (ms)}
  SpaceSize       =  5;   { size of space between special buttons }

type
  TvgNavButton = class;
  TvgNavDataLink = class;

  TvgNavGlyph = (ngEnabled, ngDisabled);
  TvgNavigateBtn = (nbFirst, nbPrior, nbNext, nbLast,
                  nbInsert, nbDelete, nbEdit, nbPost, nbCancel, nbRefresh);
  TvgNavButtonSet = set of TvgNavigateBtn;
  TvgNavButtonStyle = set of (nsAllowTimer, nsFocusRect);

  EvgNavClick = procedure (Sender: TObject; Button: TvgNavigateBtn) of object;

{ TvgDBNavigator }

  TvgDBNavigator = class (TvgLayout)
  private
    FDataLink: TvgNavDataLink;
    FVisibleButtons: TvgNavButtonSet;
    FHints: TStrings;
    FDefHints: TStrings;
    ButtonWidth: Integer;
    MinBtnSize: TvgPoint;
    FOnNavClick: EvgNavClick;
    FBeforeAction: EvgNavClick;
    FocusedButton: TvgNavigateBtn;
    FConfirmDelete: Boolean;
    FFlat: Boolean;
    FyRadius: single;
    FxRadius: single;
    FCornerType: TvgCornerType;
    FCorners: TvgCorners;
    procedure BtnMouseDown (Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: single);
    procedure ClickHandler(Sender: TObject);
    function GetDataSource: TDataSource;
    function GetHints: TStrings;
    procedure HintsChanged(Sender: TObject);
    procedure InitButtons;
    procedure InitHints;
    procedure SetDataSource(Value: TDataSource);
    procedure SetHints(Value: TStrings);
    procedure SetSize(var W: single;var H: single);
    procedure SetVisible(Value: TvgNavButtonSet);
    procedure SetCornerType(const Value: TvgCornerType);
    procedure SetxRadius(const Value: single);
    procedure SetyRadius(const Value: single);
    function IsCornersStored: Boolean;
    procedure SetCorners(const Value: TvgCorners);
  protected
    Buttons: array[TvgNavigateBtn] of TvgNavButton;
    procedure DataChanged;
    procedure EditingChanged;
    procedure ActiveChanged;
    procedure Loaded; override;
    procedure KeyDown(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure CalcMinSize(var W, H: single);
    procedure Realign; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BtnClick(Index: TvgNavigateBtn); virtual;
  published
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property VisibleButtons: TvgNavButtonSet read FVisibleButtons write SetVisible
      default [nbFirst, nbPrior, nbNext, nbLast, nbInsert, nbDelete,
        nbEdit, nbPost, nbCancel, nbRefresh];
    property Align;
    property Enabled;
    property CornerType: TvgCornerType read FCornerType write SetCornerType default vgCornerRound;
    property Corners: TvgCorners read FCorners write SetCorners stored IsCornersStored;
    property xRadius: single read FxRadius write SetxRadius;
    property yRadius: single read FyRadius write SetyRadius;
    property Hints: TStrings read GetHints write SetHints;
    property PopupMenu;
    property ConfirmDelete: Boolean read FConfirmDelete write FConfirmDelete default True;
    property ShowHint;
    property Visible;
    property BeforeAction: EvgNavClick read FBeforeAction write FBeforeAction;
    property OnClick: EvgNavClick read FOnNavClick write FOnNavClick;
  end;

{ TvgNavButton }

  TvgNavButton = class(TvgCornerButton)
  private
    FIndex: TvgNavigateBtn;
    FNavStyle: TvgNavButtonStyle;
    FRepeatTimer: TTimer;
    FPath: TvgPath;
    procedure TimerExpired(Sender: TObject);
  protected
    procedure ApplyStyle; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: single); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property NavStyle: TvgNavButtonStyle read FNavStyle write FNavStyle;
    property Index : TvgNavigateBtn read FIndex write FIndex;
  end;

{ TvgNavDataLink }

  TvgNavDataLink = class(TDataLink)
  private
    FNavigator: TvgDBNavigator;
  protected
    procedure EditingChanged; override;
    procedure DataSetChanged; override;
    procedure ActiveChanged; override;
  public
    constructor Create(ANav: TvgDBNavigator);
    destructor Destroy; override;
  end;

{ TvgDBLabel }

  TvgDBLabel = class(TvgLabel)
  private
    FDataLink: TFieldDataLink;
    procedure DataChange(Sender: TObject);
    function GetDataField: string;
    function GetDataSource: TDataSource;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(const Value: TDataSource);
    function GetFieldText: string;
  protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property TextAlign default vgTextAlignNear;
  end;

  TvgDBImage = class(TvgImage)
  private
    FDataLink: TFieldDataLink;
    procedure DataChange(Sender: TObject);
    procedure UpdateData(Sender: TObject);
    function GetDataField: string;
    function GetDataSource: TDataSource;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(const Value: TDataSource);
    function GetFieldText: string;
  protected
    procedure DoBitmapChanged(Sender: TObject); override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
  end;

  TvgDBTextBox = class(TvgTextBox)
  private
    FDataLink: TFieldDataLink;
    procedure DataChange(Sender: TObject);
    function GetDataField: string;
    function GetDataSource: TDataSource;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(const Value: TDataSource);
    function GetFieldText: string;
    procedure UpdateData(Sender: TObject);
  protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure KeyDown(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState); override;
    procedure Change; override;
    procedure EnterFocus; override;
    procedure KillFocus; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
  end;

  TvgDBMemo = class(TvgMemo)
  private
    FDataLink: TFieldDataLink;
    procedure DataChange(Sender: TObject);
    function GetDataField: string;
    function GetDataSource: TDataSource;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(const Value: TDataSource);
    function GetFieldText: string;
    procedure UpdateData(Sender: TObject);
  protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure KeyDown(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState); override;
    procedure Change; override;
    procedure EnterFocus; override;
    procedure KillFocus; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
  end;

  TvgDBGrid = class;

  TvgGridDataLink = class(TDataLink)
  private
    FGrid: TvgDBGrid;
    FFieldCount: Integer;
    FFieldMap: array of Integer;
    FModified: Boolean;
    FInUpdateData: Boolean;
    FSparseMap: Boolean;
    function GetDefaultFields: Boolean;
    function GetFields(I: Integer): TField;
  protected
    procedure ActiveChanged; override;
    procedure BuildAggMap;
    procedure DataSetChanged; override;
    procedure DataSetScrolled(Distance: Integer); override;
    procedure FocusControl(Field: TFieldRef); override;
    procedure EditingChanged; override;
    function IsAggRow(Value: Integer): Boolean; virtual;
    procedure LayoutChanged; override;
    procedure RecordChanged(Field: TField); override;
    procedure UpdateData; override;
    function  GetMappedIndex(ColIndex: Integer): Integer;
  public
    constructor Create(AGrid: TvgDBGrid);
    destructor Destroy; override;
    procedure Modified;
    procedure Reset;
    property DefaultFields: Boolean read GetDefaultFields;
    property FieldCount: Integer read FFieldCount;
    property Fields[I: Integer]: TField read GetFields;
    property SparseMap: Boolean read FSparseMap write FSparseMap;
    property Grid: TvgDBGrid read FGrid;
  end;

  TvgDBColumn = class(TvgColumn)
  private
    FField: TField;
    FFieldName: String;
    procedure SetFieldName(const Value: String);
    function GetField: TField;
    procedure SetField(Value: TField);
  protected
    procedure SetData(Value: Variant); virtual;
    function GetData: Variant; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property  Field: TField read GetField write SetField;
  published
    property FieldName: String read FFieldName write SetFieldName;
  end;

  TvgDBCheckColumn = class(TvgDBColumn)
  private
  protected
    function CreateCellControl: TvgControl; override;
    procedure DoCheckChanged(Sender: TObject);
    function GetData: Variant; override;
  public
  published
  end;

  TvgDBPopupColumn = class(TvgDBColumn)
  private
    FItems: TvgWideStrings;
    procedure SetItems(const Value: TvgWideStrings);
  protected
    function CreateCellControl: TvgControl; override;
    procedure DoPopupChanged(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Items: TvgWideStrings read FItems write SetItems;
  end;

  TvgDBImageColumn = class(TvgDBColumn)
  private
    FCurrent: TvgBitmap;
  protected
    function CreateCellControl: TvgControl; override;
    procedure DoImageChanged(Sender: TObject);
    procedure SetData(Value: Variant); override;
    function GetData: Variant; override;
  public
    destructor Destroy; override;
  published
  end;

  TvgDBProgressColumn = class(TvgDBColumn)
  private
    FMin: single;
    FMax: single;
  protected
    function CreateCellControl: TvgControl; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Min: single read FMin write FMin;
    property Max: single read FMax write FMax;
  end;

  TvgDBGrid = class(TvgCustomGrid)
  private
    FDataLink: TvgGridDataLink;
    FDisableMove: boolean;
    FEditValue: Variant;
    FNeedUpdate: boolean;
    FFirstRecord: integer;
    function GetDataSource: TDataSource;
    procedure SetDataSource(const Value: TDataSource);
    function GetSelectedField: TField;
    procedure SetSelectedField(const Value: TField);
    procedure UpdateRowCount;
  protected
    function GetValue(Col, Row: integer): Variant; override;
    procedure SetValue(Col, Row: integer; const Value: Variant); override;
    function CanEditAcceptKey(Key: System.WideChar): Boolean; override;
    function CanEditModify: Boolean; override;
    procedure KeyDown(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState); override;
    procedure Reset; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure Loaded; override;
    procedure DataSetChanged;
    procedure DataChanged;
    procedure EditingChanged;
    procedure RecordChanged(Field: TField);
    procedure UpdateData;
    procedure LinkActive(Value: Boolean);
    { table }
    function GetContentBounds: TvgRect; override;
    procedure VScrollChange(Sender: TObject); override;
    procedure SetSelected(const Value: integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ItemClass: string; override;
    property DataLink: TvgGridDataLink read FDataLink;
    property SelectedField: TField read GetSelectedField write SetSelectedField;
  published
    property DataSource: TDataSource read GetDataSource write SetDataSource;
  end;

implementation {===============================================================}

const
  MaxMapSize = (MaxInt div 2) div SizeOf(Integer);  { 250 million }

{ TvgDBNavigator }

var
  BtnTypeName: array[TvgNavigateBtn] of PChar = ('FIRST', 'PRIOR', 'NEXT',
    'LAST', 'INSERT', 'DELETE', 'EDIT', 'POST', 'CANCEL', 'REFRESH');
  BtnTypePath: array[TvgNavigateBtn] of string = (
    'M 361.374,349.551 L 325.968,315.176 L 361.374,280.801 Z M 323.202,349.551 L 287.797,315.176 L 323.202,280.801 Z M 286.357,349.551 L 279.277,349.551 L 279.277,280.801 L 286.357,280.801 Z',
    'M 327.076,346.113 L 291.667,311.738 L 327.076,277.363 Z ',
    'M 341.236,311.738 L 305.830,346.113 L 305.830,277.363 Z ',
    'M 361.374,349.551 L 354.294,349.551 L 354.294,280.801 L 361.374,280.801 Z M 352.854,315.176 L 317.448,349.551 L 317.448,280.801 Z M 314.682,315.176 L 279.277,349.551 L 279.277,280.801 Z',
    // plus
    'M 315.303,336.714 L 315.303,315.122 L 293.228,315.122 L 293.228,306.099 L 315.303,306.099 L 315.303,284.668 L 324.706,284.668 L 324.706,306.099 L 346.781,306.099 L 346.781,315.122 L '+
    '324.706,315.122 L 324.706,336.714 Z ',
    // minus
    'M 286.766,375.304 L 286.766,364.321 L 352.763,364.321 L 352.763,375.304 Z ',
    // edit
    'M 350.074,271.455 L 350.074,350.947 L 289.995,350.947 L 289.995,271.455 Z M 347.362,274.087 L 292.704,274.087 L 292.704,348.315 L 347.362,348.315 Z M 300.892,337.681 L 300.892,335.049'+
    ' L 339.121,335.049 L 339.121,337.681 Z M 300.892,327.100 L 300.892,324.468 L 339.121,324.468 L 339.121,327.100 Z M 300.892,316.519 L 300.892,313.887 L 339.121,313.887 L 339.121,316.519 '+
    'Z M 300.892,305.884 L 300.892,303.252 L 339.121,303.252 L 339.121,305.884 Z M 300.892,295.249 L 300.892,292.617 L 339.121,292.617 L 339.121,295.249 Z M 300.892,284.668 L 300.892,282.036 L'+
    ' 339.121,282.036 L 339.121,284.668 Z ',
    // post
    'M 358.467,266.729 L '+
    '360.400,269.414 C 352.512,275.181 '+
    '343.733,284.064 334.069,296.058 L '+
    '334.069,296.058 C 324.407,308.056 '+
    '317.029,319.261 311.940,329.678 L '+
    '311.940,329.678 L 307.844,332.363 '+
    'C 304.454,334.659 302.148,336.358 '+
    '300.929,337.466 L 300.929,337.466 '+
    'C 300.452,335.787 299.402,333.028 '+
    '297.777,329.194 L 297.777,329.194 '+
    'L 296.229,325.703 C '+
    '294.017,320.695 291.959,316.989 '+
    '290.059,314.588 L 290.059,314.588 '+
    'C 288.159,312.191 286.031,310.597 '+
    '283.671,309.805 L 283.671,309.805 '+
    'C 287.656,305.726 291.308,303.685 '+
    '294.625,303.682 L 294.625,303.682 '+
    'C 297.465,303.685 300.620,307.428 '+
    '304.085,314.907 L 304.085,314.907 '+
    'L 305.800,318.667 C '+
    '312.034,308.465 320.037,298.549 '+
    '329.809,288.915 L 329.809,288.915 '+
    'C 339.584,279.283 349.135,271.888 '+
    '358.467,266.729 L 358.467,266.729 '+
    'Z ',
    // cancel
    'M 319.704,321.353 L 318.875,322.480 C 313.121,330.933 308.402,335.160 304.712,335.156 L 304.712,335.156 C 300.472,335.160 296.306,331.813 292.211,325.112 L 292.211,325.112 C 292.765,325.153 293.171,325.169 293.426,325.166 L 293.426,325.166 '+
    'C 298.260,325.169 '+
    '303.645,321.588 309.580,314.424 L 309.580,314.424 L 311.074,312.598 L 309.140,310.557 C 303.719,304.974 301.006,300.231 301.006,296.323 L 301.006,296.323 C 301.006,293.141 303.977,289.381 309.912,285.044 L 309.912,285.044 C 310.761,290.596 '+
    '313.289,296.004 '+
    '317.492,301.265 L 317.492,301.265 L 319.150,303.306 L 320.480,301.641 C 326.640,294.017 332.226,290.204 337.241,290.200 L 337.241,290.200 C 341.152,290.204 344.123,293.087 346.150,298.848 L 346.150,298.848 C 345.559,298.781 345.136,298.744 '+
    '344.878,298.740 '+
    'L 344.878,298.740 C 343.109,298.744 340.618,299.898 337.409,302.208 L 337.409,302.208 C 334.200,304.518 331.490,307.123 329.275,310.020 L 329.275,310.020 L 327.617,312.222 L 329.221,313.726 C 335.160,319.315 341.357,322.108 347.809,322.104 '+
    'L 347.809,322.104 '+
    'C 344.344,328.912 340.729,332.313 336.966,332.310 L 336.966,332.310 C 333.575,332.313 328.667,329.413 322.249,323.608 L 322.249,323.608 Z ',
    // refresh
    'M 354.848,307.012 C 354.848,312.779 353.633,318.224 351.196,323.340 L 351.196,323.340 C '+
    '348.614,328.677 344.999,332.994 340.353,336.284 L 340.353,336.284 L 346.493,340.957 L '+
    '326.744,346.113 L 328.570,327.046 L 334.102,331.289 C 339.819,326.388 342.676,319.567 '+
    '342.676,310.825 L 342.676,310.825 C 342.676,299.620 337.180,290.865 326.190,284.561 L '+
    '326.190,284.561 L 333.159,271.401 C 339.947,274.590 345.298,279.515 349.205,286.172 L '+
    '349.205,286.172 C 352.968,292.550 354.848,299.496 354.848,307.012 L 354.848,307.012 Z M '+
    '312.581,332.954 L 305.609,346.113 C 298.861,342.931 293.530,338.006 289.623,331.343 L '+
    '289.623,331.343 C 285.823,324.971 283.923,318.026 283.923,310.503 L 283.923,310.503 C '+
    '283.923,304.742 285.158,299.297 287.629,294.175 L 287.629,294.175 C 290.214,288.844 '+
    '293.809,284.527 298.418,281.230 L 298.418,281.230 L 292.278,276.504 L 312.027,271.401 L '+
    '310.201,290.469 L 304.669,286.226 C 298.955,291.133 296.095,297.955 296.095,306.689 L '+
    '296.095,306.689 C 296.095,317.902 301.590,326.656 312.581,332.954 L 312.581,332.954 Z '
  );

constructor TvgDBNavigator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCorners := AllCorners;
  FxRadius := 4;
  FyRadius := 4;
  FDataLink := TvgNavDataLink.Create(Self);
  FVisibleButtons := [nbFirst, nbPrior, nbNext, nbLast, nbInsert,
    nbDelete, nbEdit, nbPost, nbCancel, nbRefresh];
  FHints := TStringList.Create;
  TStringList(FHints).OnChange := HintsChanged;
  InitButtons;
  InitHints;
  Width := 241;
  Height := 25;
  ButtonWidth := 0;
  FocusedButton := nbFirst;
  FConfirmDelete := True;
end;

destructor TvgDBNavigator.Destroy;
begin
  FDefHints.Free;
  FDataLink.Free;
  FHints.Free;
  FDataLink := nil;
  inherited Destroy;
end;

procedure TvgDBNavigator.InitButtons;
var
  I: TvgNavigateBtn;
  Btn: TvgNavButton;
  X: single;
  ResName: string;
begin
  MinBtnSize := vgPoint(20, 18);
  X := 0;
  for I := Low(Buttons) to High(Buttons) do
  begin
    Btn := TvgNavButton.Create (Self);
    Btn.Index := I;
    Btn.Visible := I in FVisibleButtons;
    Btn.Enabled := True;
    Btn.SetBounds(X, 0, MinBtnSize.X, MinBtnSize.Y);
    FmtStr(ResName, 'dbn_%s', [BtnTypeName[I]]);
//    Btn.Glyph.LoadFromResourceName(HInstance, ResName);
//    Btn.NumGlyphs := 2;
    Btn.Enabled := False;
    Btn.Enabled := True;
    Btn.OnClick := ClickHandler;
    Btn.OnMouseDown := BtnMouseDown;
    Btn.Parent := Self;
    Buttons[I] := Btn;
    X := X + MinBtnSize.X;

    Btn.FPath := TvgPath.Create(Self);
    Btn.FPath.Parent := Btn;
    Btn.FPath.Width := 18;
    Btn.FPath.Height := 18;
    Btn.FPath.Align := vaCenter;
    Btn.FPath.HitTest := false;
    Btn.FPath.Locked := true;
    Btn.FPath.Stored := false;
    Btn.FPath.Data.Data := BtnTypePath[I];
    Btn.FPath.WrapMode := vgPathFit;
    Btn.FPath.Stroke.Style := vgBrushNone;
  end;
  Buttons[nbPrior].NavStyle := Buttons[nbPrior].NavStyle + [nsAllowTimer];
  Buttons[nbNext].NavStyle  := Buttons[nbNext].NavStyle + [nsAllowTimer];
end;

procedure TvgDBNavigator.InitHints;
var
  I: Integer;
  J: TvgNavigateBtn;
begin
(*  if not Assigned(FDefHints) then
  begin
    FDefHints := TStringList.Create;
    for J := Low(Buttons) to High(Buttons) do
      FDefHints.Add(LoadResString(BtnHintId[J]));
  end;
  for J := Low(Buttons) to High(Buttons) do
    Buttons[J].Hint := FDefHints[Ord(J)];
  J := Low(Buttons);
  for I := 0 to (FHints.Count - 1) do
  begin
    if FHints.Strings[I] <> '' then Buttons[J].Hint := FHints.Strings[I];
    if J = High(Buttons) then Exit;
    Inc(J);
  end; *)
end;

procedure TvgDBNavigator.HintsChanged(Sender: TObject);
begin
  InitHints;
end;

procedure TvgDBNavigator.SetHints(Value: TStrings);
begin
  if Value.Text = FDefHints.Text then
    FHints.Clear else
    FHints.Assign(Value);
end;

function TvgDBNavigator.GetHints: TStrings;
begin
  if (csDesigning in ComponentState) and not (csWriting in ComponentState) and
     not (csReading in ComponentState) and (FHints.Count = 0) then
    Result := FDefHints else
    Result := FHints;
end;

procedure TvgDBNavigator.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

procedure TvgDBNavigator.SetVisible(Value: TvgNavButtonSet);
var
  I: TvgNavigateBtn;
  W, H: single;
begin
  W := Width;
  H := Height;
  FVisibleButtons := Value;
  for I := Low(Buttons) to High(Buttons) do
    Buttons[I].Visible := I in FVisibleButtons;
  SetSize(W, H);
  if (W <> Width) or (H <> Height) then
    SetBounds(Position.X, Position.Y, W, H);
end;

procedure TvgDBNavigator.CalcMinSize(var W, H: single);
var
  Count: Integer;
  I: TvgNavigateBtn;
begin
  if (csLoading in ComponentState) then Exit;
  if Buttons[nbFirst] = nil then Exit;

  Count := 0;
  for I := Low(Buttons) to High(Buttons) do
    if Buttons[I].Visible then
      Inc(Count);
  if Count = 0 then Inc(Count);

  W := vgMaxFloat(W, Count * MinBtnSize.X);
  H := vgMaxFloat(H, MinBtnSize.Y);

  if Align = vaNone then
    W := Trunc(W / Count) * Count;
end;

procedure TvgDBNavigator.SetSize(var W: single; var H: single);
var
  Count: Integer;
  I: TvgNavigateBtn;
  Space, Temp, Remain: single;
  X: single;
begin
  if (csLoading in ComponentState) then Exit;
  if Buttons[nbFirst] = nil then Exit;

  CalcMinSize(W, H);

  Count := 0;
  for I := Low(Buttons) to High(Buttons) do
    if Buttons[I].Visible then
      Inc(Count);
  if Count = 0 then Inc(Count);

  ButtonWidth := trunc(W / Count);
  Temp := Count * ButtonWidth;
  if Align = vaNone then W := Temp;

  X := 0;
  Remain := W - Temp;
  Temp := Count div 2;
  for I := Low(Buttons) to High(Buttons) do
  begin
    if Buttons[I].Visible then
    begin
      if X = 0 then
        Buttons[I].Corners := [vgCornerTopLeft] * FCorners + [vgCornerBottomLeft] * FCorners
      else
      if X > Width - (ButtonWidth * 1.5) then
        Buttons[I].Corners := [vgCornerTopRight] * FCorners + [vgCornerBottomRight] * FCorners
      else
        Buttons[I].Corners := [];
      Buttons[I].xRadius := FxRadius;
      Buttons[I].yRadius := FyRadius;
      Buttons[I].CornerType := FCornerType;
      Buttons[I].ApplyStyle;

      Space := 0;
      if Remain <> 0 then
      begin
        Temp := Temp - Remain;
        if Temp < 0 then
        begin
          Temp := Temp + Count;
          Space := 1;
        end;
      end;
      Buttons[I].SetBounds(X, 0, ButtonWidth + Space, Height);
      X := X + ButtonWidth + Space;
    end
    else
      Buttons[I].SetBounds (Width + 1, 0, ButtonWidth, Height);
  end;
end;

procedure TvgDBNavigator.Realign;
var
  W, H: single;
begin
  inherited ;
  W := Width;
  H := Height;
  SetSize(W, H);
end;

procedure TvgDBNavigator.ClickHandler(Sender: TObject);
begin
  BtnClick (TvgNavButton (Sender).Index);
end;

procedure TvgDBNavigator.BtnMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: single);
var
  OldFocus: TvgNavigateBtn;
begin
  OldFocus := FocusedButton;
  FocusedButton := TvgNavButton(Sender).Index;
  if IsFocused then
  begin
    SetFocus;
  end
  else
  if CanFocused and (IsFocused) and (OldFocus <> FocusedButton) then
  begin
    Buttons[OldFocus].Repaint;
    Buttons[FocusedButton].Repaint;
  end;
end;

procedure TvgDBNavigator.BtnClick(Index: TvgNavigateBtn);
begin
  if (DataSource <> nil) and (DataSource.State <> dsInactive) then
  begin
    if not (csDesigning in ComponentState) and Assigned(FBeforeAction) then
      FBeforeAction(Self, Index);
    with DataSource.DataSet do
    begin
      case Index of
        nbPrior: Prior;
        nbNext: Next;
        nbFirst: First;
        nbLast: Last;
        nbInsert: Insert;
        nbEdit: Edit;
        nbCancel: Cancel;
        nbPost: Post;
        nbRefresh: Refresh;
        nbDelete:
          if not FConfirmDelete or (MessageDlg(TranslateText('Delete record?'), mtConfirmation, mbOKCancel, 0) <> idCancel) then Delete;
      end;
    end;
  end;
  if not (csDesigning in ComponentState) and Assigned(FOnNavClick) then
    FOnNavClick(Self, Index);
end;

procedure TvgDBNavigator.KeyDown(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState);
var
  NewFocus: TvgNavigateBtn;
  OldFocus: TvgNavigateBtn;
begin
(*  OldFocus := FocusedButton;
  case Key of
    VK_RIGHT:
      begin
        if OldFocus < High(Buttons) then
        begin
          NewFocus := OldFocus;
          repeat
            NewFocus := Succ(NewFocus);
          until (NewFocus = High(Buttons)) or (Buttons[NewFocus].Visible);
          if Buttons[NewFocus].Visible then
          begin
            FocusedButton := NewFocus;
            Buttons[OldFocus].Invalidate;
            Buttons[NewFocus].Invalidate;
          end;
        end;
      end;
    VK_LEFT:
      begin
        NewFocus := FocusedButton;
        repeat
          if NewFocus > Low(Buttons) then
            NewFocus := Pred(NewFocus);
        until (NewFocus = Low(Buttons)) or (Buttons[NewFocus].Visible);
        if NewFocus <> FocusedButton then
        begin
          FocusedButton := NewFocus;
          Buttons[OldFocus].Invalidate;
          Buttons[FocusedButton].Invalidate;
        end;
      end;
    VK_SPACE:
      begin
        if Buttons[FocusedButton].Enabled then
          Buttons[FocusedButton].Click;
      end;
  end; *)
end;

procedure TvgDBNavigator.DataChanged;
var
  UpEnable, DnEnable: Boolean;
begin
  UpEnable := Enabled and FDataLink.Active and not FDataLink.DataSet.BOF;
  DnEnable := Enabled and FDataLink.Active and not FDataLink.DataSet.EOF;
  Buttons[nbFirst].Enabled := UpEnable;
  Buttons[nbPrior].Enabled := UpEnable;
  Buttons[nbNext].Enabled := DnEnable;
  Buttons[nbLast].Enabled := DnEnable;
  Buttons[nbDelete].Enabled := Enabled and FDataLink.Active and
    FDataLink.DataSet.CanModify and
    not (FDataLink.DataSet.BOF and FDataLink.DataSet.EOF);
end;

procedure TvgDBNavigator.EditingChanged;
var
  CanModify: Boolean;
begin
  CanModify := Enabled and FDataLink.Active and FDataLink.DataSet.CanModify;
  Buttons[nbInsert].Enabled := CanModify;
  Buttons[nbEdit].Enabled := CanModify and not FDataLink.Editing;
  Buttons[nbPost].Enabled := CanModify and FDataLink.Editing;
  Buttons[nbCancel].Enabled := CanModify and FDataLink.Editing;
  Buttons[nbRefresh].Enabled := CanModify;
end;

procedure TvgDBNavigator.ActiveChanged;
var
  I: TvgNavigateBtn;
begin
  if not (Enabled and FDataLink.Active) then
    for I := Low(Buttons) to High(Buttons) do
      Buttons[I].Enabled := False
  else
  begin
    DataChanged;
    EditingChanged;
  end;
end;

procedure TvgDBNavigator.SetDataSource(Value: TDataSource);
begin
  FDataLink.DataSource := Value;
  if not (csLoading in ComponentState) then
    ActiveChanged;
  if Value <> nil then Value.FreeNotification(Self);
end;

function TvgDBNavigator.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TvgDBNavigator.Loaded;
var
  W, H: single;
begin
  inherited Loaded;
  W := Width;
  H := Height;
  SetSize(W, H);
  if (W <> Width) or (H <> Height) then
    SetBounds(Position.X, Position.Y, W, H);
  InitHints;
  ActiveChanged;
end;

procedure TvgDBNavigator.SetCornerType(const Value: TvgCornerType);
begin
  if FCornerType <> Value then
  begin
    FCornerType := Value;
    Realign;
  end;
end;

procedure TvgDBNavigator.SetxRadius(const Value: single);
begin
  if FxRadius <> Value then
  begin
    FxRadius := Value;
    Realign;
  end;
end;

procedure TvgDBNavigator.SetyRadius(const Value: single);
begin
  if FyRadius <> Value then
  begin
    FyRadius := Value;
    Realign;
  end;
end;

function TvgDBNavigator.IsCornersStored: Boolean;
begin
  Result := FCorners <> AllCorners;
end;

procedure TvgDBNavigator.SetCorners(const Value: TvgCorners);
begin
  if FCorners <> Value then
  begin
    FCorners := Value;
    Realign;
  end;
end;

{ TvgNavButton }

constructor TvgNavButton.Create(AOwner: TComponent);
begin
  inherited;
  CanFocused := false;
  FResource := 'CornerButtonStyle';
  Locked := true;
  Stored := false;
end;

destructor TvgNavButton.Destroy;
begin
  if FRepeatTimer <> nil then
    FRepeatTimer.Free;
  inherited Destroy;
end;

procedure TvgNavButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: single);
begin
  inherited MouseDown (Button, Shift, X, Y);
  if nsAllowTimer in FNavStyle then
  begin
    if FRepeatTimer = nil then
      FRepeatTimer := TTimer.Create(Self);

    FRepeatTimer.OnTimer := TimerExpired;
    FRepeatTimer.Interval := InitRepeatPause;
    FRepeatTimer.Enabled  := True;
  end;
end;

procedure TvgNavButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
                                  X, Y: single);
begin
  inherited MouseUp (Button, Shift, X, Y);
  if FRepeatTimer <> nil then
    FRepeatTimer.Enabled  := False;
end;

procedure TvgNavButton.TimerExpired(Sender: TObject);
begin
  FRepeatTimer.Interval := RepeatPause;
  if (IsPressed) then
  begin
    try
      Click;
    except
      FRepeatTimer.Enabled := False;
      raise;
    end;
  end;
end;

procedure TvgNavButton.ApplyStyle;
var
  S: TvgObject;
begin
  inherited;
  { from style }
  S := FindResource('text');
  if (S <> nil) and (S is TvgShape) and (FPath <> nil) then
    FPath.Fill.Assign(TvgShape(S).Fill);
end;

{ TvgNavDataLink }

constructor TvgNavDataLink.Create(ANav: TvgDBNavigator);
begin
  inherited Create;
  FNavigator := ANav;
  VisualControl := True;
end;

destructor TvgNavDataLink.Destroy;
begin
  FNavigator := nil;
  inherited Destroy;
end;

procedure TvgNavDataLink.EditingChanged;
begin
  if FNavigator <> nil then FNavigator.EditingChanged;
end;

procedure TvgNavDataLink.DataSetChanged;
begin
  if FNavigator <> nil then FNavigator.DataChanged;
end;

procedure TvgNavDataLink.ActiveChanged;
begin
  if FNavigator <> nil then FNavigator.ActiveChanged;
end;

{ TvgDBLabel ===================================================================}

constructor TvgDBLabel.Create(AOwner: TComponent);
begin
  inherited;
  TextAlign := vgTextAlignNear;
  FResource := 'labelstyle';
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
end;

procedure TvgDBLabel.DataChange(Sender: TObject);
begin
  Text := GetFieldText;
end;

destructor TvgDBLabel.Destroy;
begin
  FreeAndNil(FDataLink);
  inherited;
end;

procedure TvgDBLabel.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

function TvgDBLabel.GetFieldText: string;
begin
  if FDataLink.Field <> nil then
    Result := FDataLink.Field.DisplayText
  else
    if csDesigning in ComponentState then
      Result := Name
    else
      Result := '';
end;

function TvgDBLabel.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TvgDBLabel.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

function TvgDBLabel.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TvgDBLabel.SetDataSource(const Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

{ TvgDBImage ===================================================================}

constructor TvgDBImage.Create(AOwner: TComponent);
begin
  inherited;
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnUpdateData := UpdateData;
end;

procedure TvgDBImage.DoBitmapChanged(Sender: TObject);
begin
  if (not Assigned(FDataLink.Field) or FDataLink.Field.IsBlob) then
  begin
    FDataLink.OnDataChange := nil;
    try
      FDataLink.Edit;
      FDataLink.Modified;
      FDataLink.UpdateRecord;
    finally
      FDataLink.OnDataChange := DataChange;
    end;
    Repaint;
  end;
end;

procedure TvgDBImage.UpdateData(Sender: TObject);
var
  P: TPicture;
begin
  if (not Assigned(FDataLink.Field) or FDataLink.Field.IsBlob) then
  begin
    P := TPicture.Create;
    P.Assign(Bitmap);
    FDataLink.Field.Assign(P);
    P.Free;
  end;
end;

procedure TvgDBImage.DataChange(Sender: TObject);
begin
  if Assigned(FDataLink.Field) and FDataLink.Field.IsBlob then
  begin
    Bitmap.OnChange := nil;
    try
      Bitmap.Assign(FDataLink.Field);
    finally
      Bitmap.OnChange := DoBitmapChanged;
    end;
    Repaint;
  end
  else
    Bitmap.Clear();
end;

destructor TvgDBImage.Destroy;
begin
  FreeAndNil(FDataLink);
  inherited;
end;

procedure TvgDBImage.Paint;
begin
  inherited ;
  if Assigned(FScene) and FScene.GetDesignTime then
  begin
    Canvas.Fill.Style := vgBrushSolid;
    Canvas.Fill.Color := vcGray;
    if not Assigned(FDataLink.Field) then
      Canvas.FillText(LocalRect, LocalRect, '[TvgDBImage]', false, AbsoluteOpacity, vgTextAlignCenter)
    else
      Canvas.FillText(LocalRect, LocalRect, '[' + FDataLink.FieldName + ']', false, AbsoluteOpacity, vgTextAlignCenter);
  end;
end;

procedure TvgDBImage.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

function TvgDBImage.GetFieldText: string;
begin
  if FDataLink.Field <> nil then
    Result := FDataLink.Field.DisplayText
  else
    if csDesigning in ComponentState then
      Result := Name
    else
      Result := '';
end;

function TvgDBImage.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TvgDBImage.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

function TvgDBImage.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TvgDBImage.SetDataSource(const Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

{ TvgDBTextBox ===================================================================}

constructor TvgDBTextBox.Create(AOwner: TComponent);
begin
  inherited;
  FResource := 'textboxstyle';
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnUpdateData := UpdateData;
{  FDataLink.OnEditingChange := EditingChange;
  FDataLink.OnActiveChange := ActiveChange;}
end;

destructor TvgDBTextBox.Destroy;
begin
  FreeAndNil(FDataLink);
  inherited;
end;

procedure TvgDBTextBox.DataChange(Sender: TObject);
begin
  Text := GetFieldText;
end;

procedure TvgDBTextBox.UpdateData(Sender: TObject);
begin
  FDataLink.Field.Text := Text;
end;

procedure TvgDBTextBox.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

function TvgDBTextBox.GetFieldText: string;
begin
  if FDataLink.Field <> nil then
    Result := FDataLink.Field.DisplayText
  else
    if csDesigning in ComponentState then
      Result := Name
    else
      Result := '';
end;

function TvgDBTextBox.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TvgDBTextBox.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

function TvgDBTextBox.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TvgDBTextBox.SetDataSource(const Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

procedure TvgDBTextBox.KeyDown(var Key: Word; var KeyChar: System.WideChar;
  Shift: TShiftState);
begin
  if (Key = VK_DELETE) or ((Key = VK_INSERT) and (ssShift in Shift)) then
    FDataLink.Edit;
  if (KeyChar >= #32) and (FDataLink.Field <> nil) and
    not FDataLink.Field.IsValidChar(Char(KeyChar)) then
  begin
    KeyChar := #0;
  end;
  case KeyChar of
    ^H, ^V, ^X, #32..High(KeyChar):
      if not FDataLink.Edit then
        KeyChar := #0;
    #27:
      begin
        FDataLink.Reset;
        SelectAll;
        KeyChar := #0;
      end;
  end;
  inherited;
end;

procedure TvgDBTextBox.Change;
begin
  inherited;
  if FDataLink.Editing then
  begin
    FDataLink.Modified;
    FDataLink.UpdateRecord;
  end;
end;

procedure TvgDBTextBox.EnterFocus;
begin
  inherited;
  FDataLink.Reset;
end;

procedure TvgDBTextBox.KillFocus;
begin
  inherited;
  FDataLink.Reset;
end;

{ TvgDBMemo ===================================================================}

constructor TvgDBMemo.Create(AOwner: TComponent);
begin
  inherited;
  FResource := 'Memostyle';
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnUpdateData := UpdateData;
{  FDataLink.OnEditingChange := EditingChange;
  FDataLink.OnActiveChange := ActiveChange;}
end;

destructor TvgDBMemo.Destroy;
begin
  FreeAndNil(FDataLink);
  inherited;
end;

procedure TvgDBMemo.DataChange(Sender: TObject);
begin
  if Assigned(FDataLink.Field) and FDataLink.Field.IsBlob then
  begin
    Text := FDataLink.Field.AsString;
  end
  else
    Text := GetFieldText;
end;

procedure TvgDBMemo.UpdateData(Sender: TObject);
begin
  FDataLink.Field.AsString := Text;
end;

procedure TvgDBMemo.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

function TvgDBMemo.GetFieldText: string;
begin
  if FDataLink.Field <> nil then
    Result := FDataLink.Field.DisplayText
  else
    if csDesigning in ComponentState then
      Result := Name
    else
      Result := '';
end;

function TvgDBMemo.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TvgDBMemo.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

function TvgDBMemo.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TvgDBMemo.SetDataSource(const Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

procedure TvgDBMemo.KeyDown(var Key: Word; var KeyChar: System.WideChar;
  Shift: TShiftState);
begin
  if (Key = VK_DELETE) or ((Key = VK_INSERT) and (ssShift in Shift)) then
    FDataLink.Edit;
  if (KeyChar >= #32) and (FDataLink.Field <> nil) and
    not FDataLink.Field.IsValidChar(Char(KeyChar)) then
  begin
    KeyChar := #0;
  end;
  case KeyChar of
    ^H, ^V, ^X, #32..High(KeyChar):
      if not FDataLink.Edit then
        KeyChar := #0;
    #27:
      begin
        FDataLink.Reset;
        SelectAll;
        KeyChar := #0;
      end;
  end;
  inherited;
end;

procedure TvgDBMemo.Change;
begin
  inherited;
  if FDataLink.Editing then
  begin
    FDataLink.Modified;
    FDataLink.UpdateRecord;
  end;
end;

procedure TvgDBMemo.EnterFocus;
begin
  inherited;
  FDataLink.Reset;
end;

procedure TvgDBMemo.KillFocus;
begin
  inherited;
  FDataLink.Reset;
end;

{ TvgGridDataLink }

type
  EInvalidGridOperation = class(Exception);

procedure RaiseGridError(const S: string);
begin
  raise EInvalidGridOperation.Create(S);
end;

type
  TIntArray = array[0..MaxMapSize] of Integer;
  PIntArray = ^TIntArray;

constructor TvgGridDataLink.Create(AGrid: TvgDBGrid);
begin
  inherited Create;
  FGrid := AGrid;
  VisualControl := True;
end;

destructor TvgGridDataLink.Destroy;
begin
  inherited Destroy;
end;

function TvgGridDataLink.GetDefaultFields: Boolean;
var
  I: Integer;
begin
  Result := True;
  if DataSet <> nil then Result := DataSet.DefaultFields;
  if Result and SparseMap then
  for I := 0 to FFieldCount-1 do
    if FFieldMap[I] < 0 then
    begin
      Result := False;
      Exit;
    end;
end;

function TvgGridDataLink.GetFields(I: Integer): TField;
begin
  if (0 <= I) and (I < FFieldCount) and (FFieldMap[I] >= 0) then
    Result := DataSet.Fields[FFieldMap[I]]
  else
    Result := nil;
end;

procedure TvgGridDataLink.ActiveChanged;
begin
  if Active and Assigned(DataSource) then
    if Assigned(DataSource.DataSet) then
      if DataSource.DataSet.IsUnidirectional then
        DatabaseError('Operation not allowed on a unidirectional dataset');
  FGrid.LinkActive(Active);
  FModified := False;
end;

procedure TvgGridDataLink.Modified;
begin
  FModified := True;
end;

procedure TvgGridDataLink.DataSetChanged;
begin
  FGrid.DataSetChanged;
  FModified := False;
end;

procedure TvgGridDataLink.DataSetScrolled(Distance: Integer);
begin
  FGrid.DataChanged;
end;

procedure TvgGridDataLink.LayoutChanged;
begin
  inherited LayoutChanged;
end;

procedure TvgGridDataLink.FocusControl(Field: TFieldRef);
begin
  if Assigned(Field) and Assigned(Field^) then
  begin
    FGrid.SelectedField := Field^;
  end;
end;

procedure TvgGridDataLink.EditingChanged;
begin
  FGrid.EditingChanged;
end;

procedure TvgGridDataLink.RecordChanged(Field: TField);
begin
  FGrid.RecordChanged(Field);
  FModified := False;
end;

procedure TvgGridDataLink.UpdateData;
begin
  FInUpdateData := True;
  try
    if FModified then FGrid.UpdateData;
    FModified := False;
  finally
    FInUpdateData := False;
  end;
end;

function TvgGridDataLink.GetMappedIndex(ColIndex: Integer): Integer;
begin
  if (0 <= ColIndex) and (ColIndex < FFieldCount) then
    Result := FFieldMap[ColIndex]
  else
    Result := -1;
end;

procedure TvgGridDataLink.Reset;
begin
  if FModified then RecordChanged(nil) else Dataset.Cancel;
end;

function TvgGridDataLink.IsAggRow(Value: Integer): Boolean;
begin
  Result := False;
end;

procedure TvgGridDataLink.BuildAggMap;
begin
end;

{ TvgDBColumn }

constructor TvgDBColumn.Create(AOwner: TComponent);
begin
  inherited;
end;

destructor TvgDBColumn.Destroy;
begin
  inherited;
end;

function TvgDBColumn.GetField: TField;
var
  G: TvgDBGrid;
begin    { Returns Nil if FieldName can't be found in dataset }
  G := TvgDBGrid(Grid);
  if (FField = nil) and (Length(FFieldName) > 0) and Assigned(G) and Assigned(G.DataLink.DataSet) then
  with G.Datalink.Dataset do
    if Active or (not DefaultFields) then
      SetField(FindField(FieldName));
  Result := FField;
end;

procedure TvgDBColumn.SetField(Value: TField);
begin
  if FField = Value then Exit;
  if Assigned(FField) and (Grid <> nil) then
    FField.RemoveFreeNotification(Grid);
  if Assigned(Value) and (csDestroying in Value.ComponentState) then
    Value := nil;    // don't acquire references to fields being destroyed
  FField := Value;
  if Assigned(Value) then
  begin
    if Grid <> nil then
      FField.FreeNotification(Grid);
    {$IFNDEF FPC}
    FFieldName := Value.FullName
    {$ELSE}
    FFieldName := Value.FieldName;
    {$ENDIF}
  end;
end;

procedure TvgDBColumn.SetFieldName(const Value: String);
begin
  if FFieldName <> Value then
  begin
    FFieldName := Value;
    if Header = '' then
      Header := FieldName;
    UpdateColumn;
  end;
end;

function TvgDBColumn.GetData: Variant;
begin
  Result := Field.Value;
end;

procedure TvgDBColumn.SetData(Value: Variant);
begin
  Field.Value := Value
end;

{ TvgDBCheckColumn }

function TvgDBCheckColumn.CreateCellControl: TvgControl;
begin
  Result := TvgCheckCell.Create(Self);
  TvgCheckCell(Result).OnChange := DoCheckChanged;
end;

procedure TvgDBCheckColumn.DoCheckChanged(Sender: TObject);
begin
  if Grid = nil then Exit;
  if FUpdateColumn then Exit;
  with vgStringToPoint(TvgObject(Sender).TagString) do
    TvgDBGrid(Grid).SetValue(trunc(x), trunc(y), TvgControl(Sender).Data);
end;

function TvgDBCheckColumn.GetData: Variant;
begin
  Result := Field.AsBoolean
end;

{ TvgDBPopupColumn }

constructor TvgDBPopupColumn.Create(AOwner: TComponent);
begin
  inherited;
  FItems := TvgWideStringList.Create;
end;

destructor TvgDBPopupColumn.Destroy;
begin
  FItems.Free;
  inherited;
end;

function TvgDBPopupColumn.CreateCellControl: TvgControl;
begin
  Result := TvgPopupCell.Create(Self);
  TvgPopupCell(Result).Items.Assign(FItems);
  TvgPopupCell(Result).OnChange := DoPopupChanged;
end;

procedure TvgDBPopupColumn.SetItems(const Value: TvgWideStrings);
begin
  FItems.Assign(Value);
end;

procedure TvgDBPopupColumn.DoPopupChanged(Sender: TObject);
begin
  if Grid = nil then Exit;
  if FUpdateColumn then Exit;
  with vgStringToPoint(TvgObject(Sender).TagString) do
    TvgDBGrid(Grid).SetValue(trunc(x), trunc(y), TvgControl(Sender).Data);
end;

{ TvgDBImageColumn }

function TvgDBImageColumn.CreateCellControl: TvgControl;
begin
  Result := TvgImageCell.Create(Self);
  TvgImageCell(Result).OnChange := DoImageChanged;
  TvgImageCell(Result).EnableOpenDialog := true;
end;

destructor TvgDBImageColumn.Destroy;
begin
  if FCurrent <> nil then
    FCurrent.Free;
  inherited;
end;

procedure TvgDBImageColumn.DoImageChanged(Sender: TObject);
begin
  if Grid = nil then Exit;
  if FUpdateColumn then Exit;
  if not TvgDBGrid(Grid).CanEditModify then Exit;
  with vgStringToPoint(TvgObject(Sender).TagString) do
    TvgDBGrid(Grid).SetValue(trunc(x), trunc(y), TvgControl(Sender).Data);
end;

function TvgDBImageColumn.GetData: Variant;
begin
  if Assigned(Field) and Field.IsBlob and (TBlobField(Field).BlobSize > 0) then
  begin
    if FCurrent = nil then
      FCurrent := TvgBitmap.Create(1, 1);
    FCurrent.Assign(Field);
    Result := ObjectToVariant(FCurrent);
  end
  else
    Result := NULL;
end;

procedure TvgDBImageColumn.SetData(Value: Variant);
var
  P: TPicture;
begin
  if Assigned(Field) and Field.IsBlob and VarIsObject(Value) then
  begin
    P := TPicture.Create;
    P.Assign(TPersistent(VariantToObject(Value)));
    Field.Assign(P);
    P.Free;
  end
  else
    Field.Value := NULL
end;

{ TvgDBProgressColumn }

constructor TvgDBProgressColumn.Create(AOwner: TComponent);
begin
  inherited;
  FMax := 100;
end;

function TvgDBProgressColumn.CreateCellControl: TvgControl;
begin
  Result := TvgProgressCell.Create(Self);
  TvgProgressCell(Result).Min := FMin;
  TvgProgressCell(Result).Max := FMax;
end;

{ TvgDBGrid }

constructor TvgDBGrid.Create(AOwner: TComponent);
begin
  inherited;
  FResource := 'gridstyle';
  FDataLink := TvgGridDataLink.Create(Self);
  RowCount := 0;
end;

destructor TvgDBGrid.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited;
end;

procedure TvgDBGrid.Loaded;
begin
  inherited ;
  DataChanged;
end;

function TvgDBGrid.ItemClass: string;
begin
  Result := 'TvgDBColumn;TvgDBCheckColumn;TvgDBPopupColumn;TvgDBImageColumn;TvgDBProgressColumn';
end;

type
  THack = class(TDataSet);

procedure TvgDBGrid.UpdateRowCount;
begin
  if FDataLink.Active then
  begin
    FDataLink.BufferCount := VisibleRows * 2;
    if FDataLink.BufferCount < FDataLink.DataSet.RecordCount then
      RowCount := FDataLink.DataSet.RecordCount - FDataLink.BufferCount + FDataLink.RecordCount
    else
      RowCount := FDataLink.RecordCount;
  end
  else
    RowCount := 0;
end;

procedure TvgDBGrid.LinkActive(Value: Boolean);
begin
  UpdateRowCount;
  UpdateSelection;
end;

procedure TvgDBGrid.EditingChanged;
begin
  LinkActive(FDataLink.Active);
end;

procedure TvgDBGrid.RecordChanged(Field: TField);
begin
  if DataSource.State <> dsInactive then
    UpdateColumns;
end;

procedure TvgDBGrid.DataChanged;
begin
  if (DataSource <> nil) and (DataSource.State <> dsInactive) then
  begin
    FDisableMove := true;
    if FDataLink.DataSet.RecNo > 0 then
      Selected := (FDataLink.DataSet.RecNo - 1);
    FDisableMove := false;
    UpdateColumns;
  end;
end;

procedure TvgDBGrid.DataSetChanged;
begin
  if (FDataLink.DataSet <> nil) then
  begin
    UpdateRowCount;
    FDisableMove := true;
    if FDataLink.DataSet.RecNo > 0 then
      Selected := (FDataLink.DataSet.RecNo - 1);
    FDisableMove := false;
    UpdateColumns;
  end;
end;

function TvgDBGrid.GetValue(Col, Row: integer): Variant;
var
  C: TvgDBColumn;
  F, OldActive: Integer;
begin
  Result := NULL;
  if Assigned(DataLink) and DataLink.Active then
  begin
    C := TvgDBColumn(Columns[Col]);
    if (C <> nil) and (C.GetField <> nil) then
    begin
      OldActive := FDataLink.ActiveRecord;
      F := (Selected - OldActive);
      try
        if (Row - F >= 0) and (Row - F < FDataLink.BufferCount) then
        begin
          FDatalink.ActiveRecord := Row - F;
          Result := C.GetData;
        end;
 
      finally
        FDatalink.ActiveRecord := OldActive;
      end;
    end;
  end;
end;

procedure TvgDBGrid.SetValue(Col, Row: integer; const Value: Variant);
var
  C: TvgDBColumn;
begin
  if Assigned(DataLink) and DataLink.Active and not FDataLink.ReadOnly then
  begin
    C := TvgDBColumn(Columns[Col]);
    if (C <> nil) and (C.Field <> nil) and C.Field.CanModify then
    begin
      FDataLink.Modified;
      FEditValue := Value;
      FNeedUpdate := true;
      FDatalink.UpdateData;
    end;
  end;
end;

function TvgDBGrid.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TvgDBGrid.SetDataSource(const Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
  begin
    FDataLink.DataSource := Value;
  end;
  if Value <> nil then Value.FreeNotification(Self);
end;

procedure TvgDBGrid.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

procedure TvgDBGrid.UpdateData;
var
  C: TvgDBColumn;
begin
  C := TvgDBColumn(Columns[ColumnIndex]);
  if (C <> nil) and FNeedUpdate then
    TvgDBColumn(Columns[ColumnIndex]).SetData(FEditValue);
  FEditValue := NULL;
  FNeedUpdate := false;
end;

procedure TvgDBGrid.KeyDown(var Key: Word; var KeyChar: System.WideChar;
  Shift: TShiftState);

  procedure DoSelection(Select: Boolean; Direction: Integer);
  begin
    BeginUpdate;
    try
      FDatalink.MoveBy(Direction);
    finally
      EndUpdate;
    end;
  end;

  procedure NextRow(Select: Boolean);
  begin
    with FDatalink.Dataset do
    begin
      if (State = dsInsert) and not Modified and not FDatalink.FModified then
        if FDataLink.EOF then Exit else Cancel
      else
        DoSelection(Select, 1);
      if FDataLink.EOF and CanModify and (not ReadOnly) then
        Append;
    end;
  end;

  procedure PriorRow(Select: Boolean);
  begin
    with FDatalink.Dataset do
      if (State = dsInsert) and not Modified and FDataLink.EOF and
        not FDatalink.FModified then
        Cancel
      else
        DoSelection(Select, -1);
  end;

begin
  if (RowCount > 0) and (FDataLink.Active) then
  begin
    case Key of
      VK_UP: PriorRow(True);
      VK_DOWN: NextRow(True);
      VK_NEXT: FDataLink.MoveBy(VisibleRows);
      VK_PRIOR: FDataLink.MoveBy(-VisibleRows);
      VK_DELETE:
        if (not ReadOnly) and not FDataLink.DataSet.IsEmpty
          and FDataLink.DataSet.CanModify
        then
          FDataLink.DataSet.Delete;
      VK_HOME:
        if ssCtrl in Shift then
          FDataLink.DataSet.First
        else
          ColumnIndex := 0;
      VK_END:
        if ssCtrl in Shift then
          FDataLink.DataSet.Last
        else
          ColumnIndex := ColumnCount - 1;
      VK_INSERT:
        if FDataLink.DataSet.CanModify and (not ReadOnly) then
        begin
          FDataLink.DataSet.Insert;
        end;
      VK_ESCAPE:
        begin
          FDatalink.Reset;
          SetFocus;
        end;
    else
      inherited ;
      Exit;
    end;
    Key := 0;
  end;
  inherited ;
end;

procedure TvgDBGrid.SetSelected(const Value: integer);
var
  S: integer;
begin
  S := Selected;
  if Selected <> Value then
  begin
    inherited ;
    if not FDisableMove then
    begin
      if FDataLink.Active then
        FDataLink.MoveBy(Selected - S);
    end;
    UpdateSelection;
  end;
end;

function TvgDBGrid.GetSelectedField: TField;
begin
  Result := TvgDBColumn(Columns[ColumnIndex]).Field;
end;

procedure TvgDBGrid.SetSelectedField(const Value: TField);
var
  i: integer;
begin
  for i := 0 to ColumnCount - 1 do
  begin
    if TvgDBColumn(Columns[i]).Field = Value then
    begin
      ColumnIndex := i;
      Break;
    end;
  end;
end;

function TvgDBGrid.CanEditAcceptKey(Key: System.WideChar): Boolean;
begin
  with TvgDBColumn(Columns[ColumnIndex]) do
    Result := FDatalink.Active and Assigned(Field) and Field.IsValidChar(Char(Key));
end;

function TvgDBGrid.CanEditModify: Boolean;
begin
  Result := False;
  if not ReadOnly and FDatalink.Active and not FDatalink.Readonly then
  with TvgDBColumn(Columns[ColumnIndex]) do
    if (not ReadOnly) and Assigned(Field) and Field.CanModify then
    begin
      FDatalink.Edit;
      Result := FDatalink.Editing;
      if Result then FDatalink.Modified;
    end;
end;

procedure TvgDBGrid.Reset;
begin
  inherited;
  if FDatalink.Active then
    FDatalink.Reset;
end;

procedure TvgDBGrid.VScrollChange(Sender: TObject);
begin
  inherited;
  if FDataLink.Active then
  begin
    if TopRow < Selected - FDataLink.ActiveRecord then
      Selected := TopRow;
    if TopRow + VisibleRows - 1 > Selected - FDataLink.ActiveRecord + FDataLink.BufferCount then
    begin
      Selected := TopRow + VisibleRows - 1;
      Selected := Selected;
    end;
  end;
end;

function TvgDBGrid.GetContentBounds: TvgRect;
begin
  Result := inherited GetContentBounds;
  UpdateRowCount;
end;

initialization
  RegisterVGObjects('DB-Aware', [TvgDBNavigator, TvgDBGrid, TvgDBLabel, TvgDBImage, TvgDBTextBox, TvgDBMemo]);
  RegisterClasses([TvgNavButton, TvgDBColumn, TvgDBCheckColumn, TvgDBPopupColumn, TvgDBImageColumn, TvgDBProgressColumn]);
end.

