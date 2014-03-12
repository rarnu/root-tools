unit vg_grid;

{$I vg_define.inc}

interface

uses
  {$IFDEF WINDOWS} Windows, {$ENDIF}
  {$IFNDEF NOVCL} Controls, Forms, {$ENDIF}
  {$IFDEF LCL} LCLType, {$ENDIF}
  Math, Classes, SysUtils, Variants,
  vg_scene, vg_objects, vg_layouts, vg_textbox, vg_controls;

type

  TvgCustomGrid = class;
  TvgHeader = class;

  TvgHeaderItem = class(TvgCornerButton)
  private
    FSplitter: TvgVisualObject;
    FLeftSplitter: TvgVisualObject;
  protected
    procedure DragOver(const Data: TvgDragObject; const Point: TvgPoint; var Accept: Boolean); override;
    procedure DragDrop(const Data: TvgDragObject; const Point: TvgPoint); override;
    procedure DragEnd; override;
    procedure DoSplitterMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y, Dx, Dy: single);
    procedure DoLeftSplitterMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y, Dx, Dy: single);
    function Header: TvgHeader;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property CanFocused default false;
    property TextAlign default vgTextAlignNear;
    property DragMode default vgDragAutomatic;
  end;

  TvgOnRealignItemEvent = procedure (Sender: TObject; OldIndex, NewIndex: integer) of object;
  TvgOnResizeItemEvent = procedure (Sender: TObject; var NewSize: single) of object;

  TvgHeader = class(TvgControl)
  private
    FOnRealignItem: TvgOnRealignItemEvent;
    FOnResizeItem: TvgOnResizeItemEvent;
    FOffset: single; // hscroll offset used in grid
    FLastItem: TvgHeaderItem;
    FRadius: single;
    FSides: TvgSides;
    function GetItem(Index: integer): TvgHeaderItem;
    procedure SetRadius(const Value: single);
    procedure SetSides(const Value: TvgSides);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
    procedure Realign; override;
    function ItemClass: string; override;
    property Items[Index: integer]: TvgHeaderItem read GetItem;
  published
    property CanFocused default false;
    property ClipChildren default true;
    property OnRealignItem: TvgOnRealignItemEvent read FOnRealignItem write FOnRealignItem;
    property OnResizeItem: TvgOnResizeItemEvent read FOnResizeItem write FOnResizeItem;
    property Radius: single read FRadius write SetRadius;
    property Sides: TvgSides read FSides write SetSides;
  end;

  TvgTextCell = class(TvgTextBox)
  private
  end;

  TvgCheckCell = class(TvgCheckBox)
  private
  public
  end;

  TvgProgressCell = class(TvgProgressBar)
  private
  public
  end;

  TvgPopupCell = class(TvgPopupBox)
  private
  public
  end;

  TvgImageCell = class(TvgImageControl)
  private
  public
  end;

  TvgColumn = class(TvgControl)
  private
    FReadOnly: boolean;
    procedure SetHeader(const Value: WideString);
  protected
    FCellControls: array of TvgControl;
    FUpdateColumn: boolean;
    FHeader: WideString;
    FSaveData: Variant;
    FDisableChange: boolean;
    function Grid: TvgCustomGrid;
    procedure UpdateColumn; virtual;
    procedure UpdateSelected;
    procedure ClearColumn;
    function CreateCellControl: TvgControl; virtual;
    procedure DoTextChanged(Sender: TObject);
    procedure DoCanFocused(Sender: TObject; var ACanFocused: boolean);
    procedure DoEnterFocus(Sender: TObject);
    procedure DoKeyDown(Sender: TObject; var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState);
  public
    constructor Create(AOwner: TComponent); override;
    function CellControlByPoint(X, Y: single): TvgControl;
    function CellControlByRow(Row: integer): TvgControl;
  published
    property Resource;
    property Header: WideString read FHeader write SetHeader;
    property ReadOnly: boolean read FReadOnly write FReadOnly default false;
  end;

  TvgCheckColumn = class(TvgColumn)
  private
    function CreateCellControl: TvgControl; override;
    procedure DoCheckChanged(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TvgProgressColumn = class(TvgColumn)
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

  TvgPopupColumn = class(TvgColumn)
  private
    FItems: TvgWideStrings;
    procedure SetItems(const Value: TvgWideStrings);
  protected
    function CreateCellControl: TvgControl; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Items: TvgWideStrings read FItems write SetItems;
  end;

  TvgImageColumn = class(TvgColumn)
  private
    function CreateCellControl: TvgControl; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TOnGetValue = procedure (Sender: TObject; const Col, Row: integer; var Value: Variant) of object;
  TOnSetValue = procedure (Sender: TObject; const Col, Row: integer; const Value: Variant) of object;
  TOnEdititingDone = procedure (Sender: TObject; const Col, Row: integer) of object;

  TvgCustomGrid = class(TvgScrollBox)
  private
    FMouseSelecting: boolean;
    FItemHeight: single;
    FSelection: TvgVisualObject;
    FFocus: TvgVisualObject;
    FRowCount: integer;
    FOnGetValue: TOnGetValue;
    FOnSetValue: TOnSetValue;
    FSelections: TList;
    FAlternatingRowBackground: boolean;
    FOddFill: TvgBrush;
    FLineFill: TvgBrush;
    FShowHorzLines: boolean;
    FShowVertLines: boolean;
    FReadOnly: boolean;
    FColumnIndex: integer;
    FHeader: TvgHeader;
    FShowHeader: boolean;
    FShowSelectedCell: boolean;
    FOnEdititingDone: TOnEdititingDone;
    FMultiSelect: boolean;
    function GetColumnCount: integer;
    function GetColumn(Index: integer): TvgColumn;
    procedure SetRowCount(const Value: integer);
    procedure SetRowHeight(const Value: single);
    function GetVisibleRows: integer;
    procedure SetAlternatingRowBackground(const Value: boolean);
    procedure SetShowHorzLines(const Value: boolean);
    procedure SetShowVertLines(const Value: boolean);
    procedure SetColumnIndex(const Value: integer);
    procedure SetShowHeader(const Value: boolean);
    procedure SetShowSelectedCell(const Value: boolean);
  protected
    FSelected: integer;
    FRowHeight: single;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure ApplyStyle; override;
    procedure FreeStyle; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: single); override;
    procedure MouseMove(Shift: TShiftState; X, Y, Dx, Dy: single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: single); override;
    procedure KeyDown(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState); override;
    procedure HScrollChange(Sender: TObject); override;
    procedure VScrollChange(Sender: TObject); override;
    function GetContentBounds: TvgRect; override;
    procedure UpdateColumns; virtual;
    procedure UpdateHeader;
    procedure UpdateSelection;
    procedure Reset; virtual;
    procedure DoContentPaint(Sender: TObject; const Canvas: TvgCanvas; const ARect: TvgRect);
    procedure DoContentPaint2(Sender: TObject; const Canvas: TvgCanvas; const ARect: TvgRect);
    { data }
    function GetTopRow: integer; virtual;
    function GetValue(Col, Row: integer): Variant; virtual;
    procedure SetValue(Col, Row: integer; const Value: Variant); virtual;
    function IsSelected(Row: integer): boolean;
    procedure SetSelected(const Value: integer); virtual;
    function CanEditAcceptKey(Key: System.WideChar): Boolean; virtual;
    function CanEditModify: Boolean; virtual;
    { header }
    procedure DoRealignItem(Sender: TObject; OldIndex, NewIndex: integer);
    procedure DoResizeItem(Sender: TObject; var NewSize: single);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ItemClass: string; override;
    function ColumnByIndex(const Idx: integer): TvgColumn;
    function ColumnByPoint(const X, Y: single): TvgColumn;
    function RowByPoint(const X, Y: single): integer;
    procedure AddObject(AObject: TvgObject); override;
    procedure RemoveObject(AObject: TvgObject); override;
    property TopRow: integer read GetTopRow;
    property VisibleRows: integer read GetVisibleRows;
    property ColumnCount: integer read GetColumnCount;
    property ColumnIndex: integer read FColumnIndex write SetColumnIndex;
    property Columns[Index: integer]: TvgColumn read GetColumn;
    property RowCount: integer read FRowCount write SetRowCount;
    property Selected: integer read FSelected write SetSelected;
    property OnGetValue: TOnGetValue read FOnGetValue write FOnGetValue;
    property OnSetValue: TOnSetValue read FOnSetValue write FOnSetValue;
  published
    property Resource;
    property AlternatingRowBackground: boolean read FAlternatingRowBackground write SetAlternatingRowBackground default false;
    property CanFocused default true;
    property DisableFocusEffect;
    property RowHeight: single read FRowHeight write SetRowHeight;
    property ShowSelectedCell: boolean read FShowSelectedCell write SetShowSelectedCell default true;
    property ShowVertLines: boolean read FShowVertLines write SetShowVertLines default true;
    property ShowHorzLines: boolean read FShowHorzLines write SetShowHorzLines default true;
    property ShowHeader: boolean read FShowHeader write SetShowHeader default true;
    property ReadOnly: boolean read FReadOnly write FReadOnly default false;
    property TabOrder;
    property OnEdititingDone: TOnEdititingDone read FOnEdititingDone write FOnEdititingDone; 
  end;

  TvgGrid = class(TvgCustomGrid)
  private
  public
  published
    property RowCount;
    property OnGetValue;
    property OnSetValue;
  end;

  TvgStringColumn = class(TvgColumn)
  private
    FCells: array of WideString;
  published
    procedure UpdateColumn; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TvgStringGrid = class(TvgCustomGrid)
  private
    function GetCells(ACol, ARow: Integer): WideString;
    procedure SetCells(ACol, ARow: Integer; const Value: WideString);
  protected
    function GetValue(Col, Row: integer): Variant; override;
    procedure SetValue(Col, Row: integer; const Value: Variant); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ItemClass: string; override;
    property Cells[ACol, ARow: Integer]: WideString read GetCells write SetCells;
  published
    property RowCount;
  end;

implementation {===============================================================}

uses vg_ani;

type

  TvgHackVisualObject = class(TvgVisualObject);

{ TvgHeaderItem }

constructor TvgHeaderItem.Create(AOwner: TComponent);
begin
  inherited;
  CanFocused := false;
  TextAlign := vgTextAlignNear;
  DragMode := vgDragAutomatic;
  FSplitter := TvgVisualObject.Create(Self);
  FSplitter.Parent := Self;
  FSplitter.Width := 3;
  FSplitter.Align := vaRight;
  FSplitter.Locked := true;
  FSplitter.Stored := false;
  FSplitter.HitTest := true;
  FSplitter.AutoCapture := true;
  FSplitter.Cursor := crHSplit;
  FSplitter.OnMouseMove := DoSplitterMouseMove;
  FLeftSplitter := TvgVisualObject.Create(Self);
  FLeftSplitter.Parent := Self;
  FLeftSplitter.Width := 3;
  FLeftSplitter.Align := vaLeft;
  FLeftSplitter.Locked := true;
  FLeftSplitter.Stored := false;
  FLeftSplitter.HitTest := true;
  FLeftSplitter.AutoCapture := true;
  FLeftSplitter.Cursor := crHSplit;
  FLeftSplitter.OnMouseMove := DoLeftSplitterMouseMove;
end;

procedure TvgHeaderItem.DoSplitterMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y, Dx, Dy: single);
var
  NewSize: single;
begin
  if TvgHackVisualObject(FSplitter).FPressed then
  begin
    NewSize := AbsoluteToLocal(FSplitter.LocalToAbsolute(vgPoint(X, Y))).X;
    if NewSize < 0 then NewSize := 0;

    if (Parent <> nil) and (Parent is TvgHeader) then
    begin
      if Assigned(TvgHeader(Parent).OnResizeItem) then
        TvgHeader(Parent).OnResizeItem(Self, NewSize);
    end;
    Width := NewSize;
    if (Parent <> nil) and (Parent is TvgHeader) then
      TvgHeader(Parent).Realign;
  end;
end;

procedure TvgHeaderItem.DoLeftSplitterMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y, Dx, Dy: single);
var
  P: TvgPoint;
  LeftItem: TvgHeaderItem;
begin
  if (Index > 0) and TvgHackVisualObject(FLeftSplitter).FPressed then
  begin
    if (Parent <> nil) and (Parent is TvgHeader) then
      LeftItem := TvgHeaderItem(TvgHeader(Parent).Children[Index - 1]);
    if LeftItem = nil then Exit;

    P := FLeftSplitter.LocalToAbsolute(vgPoint(X, Y));

    P := LeftItem.FSplitter.AbsoluteToLocal(P);
    TvgHackVisualObject(LeftItem.FSplitter).FPressed := true;
    LeftItem.DoSplitterMouseMove(Sender, Shift, P.X, P.Y, 0, 0);
    TvgHackVisualObject(LeftItem.FSplitter).FPressed := false;
  end;
end;

procedure TvgHeaderItem.DragDrop(const Data: TvgDragObject;
  const Point: TvgPoint);
var
  NewIndex, OldIndex: integer;
begin
  inherited;
  NewIndex := Index;
  OldIndex := TvgObject(Data.Source).Index;
  TvgObject(Data.Source).Index := Index;
  if (Header <> nil) and Assigned(Header.OnRealignItem) then
    Header.OnRealignItem(TvgObject(Data.Source), OldIndex, NewIndex);
end;

procedure TvgHeaderItem.DragEnd;
begin
  inherited;
end;

procedure TvgHeaderItem.DragOver(const Data: TvgDragObject;
  const Point: TvgPoint; var Accept: Boolean);
begin
  Accept := (Data.Source is TvgHeaderItem) and (TvgHeaderItem(Data.Source).Header = Header) and (DragMode <> vgDragManual);
end;

function TvgHeaderItem.Header: TvgHeader;
begin
  if (Parent <> nil) and (Parent is TvgHeader) then
    Result := TvgHeader(Parent)
  else
    Result := nil;
end;

{ TvgHeader }

constructor TvgHeader.Create(AOwner: TComponent);
begin
  inherited;
  ClipChildren := true;
  FSides := AllSides;
  FLastItem := TvgHeaderItem.Create(Self);
  FLastItem.Parent := Self;
  FLastItem.Stored := false;
  FLastItem.Locked := true;
  FLastItem.Width := 50;
  FLastItem.DragMode := vgDragManual;
end;

function TvgHeader.GetItem(Index: integer): TvgHeaderItem;
begin
  Result := TvgHeaderItem(Children[Index]);
end;

function TvgHeader.ItemClass: string;
begin
  Result := 'TvgHeaderItem';
end;

procedure TvgHeader.Paint;
var
  R: TvgRect;
begin
  inherited Paint;
  if Assigned(Scene) and Scene.GetDesignTime and not Locked and not FInPaintTo then
  begin
    R := LocalRect;
    vgInflateRect(R, -0.5, -0.5);
    Canvas.StrokeThickness := 1;
    Canvas.StrokeDash := vgDashDash;
    Canvas.Stroke.Style := vgBrushSolid;
    Canvas.Stroke.SolidColor := $A0909090;
    Canvas.DrawRect(R, 0, 0, AllCorners, AbsoluteOpacity);
    Canvas.StrokeDash := vgDashSolid;
  end;
end;

procedure TvgHeader.Realign;
var
  i: integer;
  R: TvgRect;

  procedure DoPosition(Control: TvgVisualObject);
  begin
    if not Control.Visible then Exit;
    with R do
    begin
      Control.SetBounds(Left, Top, Control.Width, Height);
      if Control <> FLastItem then
        Left := Left + Control.Width;
    end;
  end;

  procedure DoAlign;
  var
    I, J: Integer;
    Control: TvgVisualObject;
  begin
    for I := 0 to ChildrenCount - 1 do
    begin
      if not (TvgVisualObject(Children[I]) is TvgHeaderItem) then Continue;
      DoPosition(TvgVisualObject(Children[I]));
      // sides
      TvgHeaderItem(Children[I]).Sides := FSides;
      // corners
      TvgHeaderItem(Children[I]).xRadius := Radius;
      TvgHeaderItem(Children[I]).yRadius := Radius;
      if I = 0 then
      begin
        TvgHeaderItem(Children[I]).Corners := [vgCornerTopLeft];
        TvgHeaderItem(Children[I]).Sides := TvgHeaderItem(Children[I]).Sides + (FSides * [vgSideLeft]) - [vgSideRight];
      end
      else
      if Children[I] = FLastItem then
      begin
        TvgHeaderItem(Children[I]).Corners := [vgCornerTopRight];
        TvgHeaderItem(Children[I]).Sides := TvgHeaderItem(Children[I]).Sides + [vgSideLeft];
      end
      else
      begin
        TvgHeaderItem(Children[I]).Corners := [];
        TvgHeaderItem(Children[I]).Sides := TvgHeaderItem(Children[I]).Sides + [vgSideLeft] - [vgSideRight];
      end;
    end;
  end;

begin
  inherited ;
  if csDestroying in ComponentState then Exit;
  if FDisableAlign then Exit;
  if ChildrenCount = 0 then Exit;
  FDisableAlign := true;
  try
    FChildren.Remove(FLastItem);
    FChildren.Add(FLastItem);

    R := vgRect(FOffset, 0, FWidth, FHeight);
    R := Margins.MarginRect(R);
    DoAlign;

    if R.Left > R.Right then
      FLastItem.Width := 0
    else
      FLastItem.Width := R.Right - R.Left;
  finally
    FDisableAlign := false;
  end;
end;

procedure TvgHeader.SetRadius(const Value: single);
begin
  if FRadius <> Value then
  begin
    FRadius := Value;
    Realign;
  end;
end;

procedure TvgHeader.SetSides(const Value: TvgSides);
begin
  if FSides <> Value then
  begin
    FSides := Value;
    Realign;
  end;
end;

{ Cells =======================================================================}

{ TvgTextCell }

{ TvgCheckCell }

{ TvgProgressCell }

{ Columns =====================================================================}

{ TvgColumn }

constructor TvgColumn.Create(AOwner: TComponent);
begin
  inherited;
  FAutoTranslate := true;
  Width := 100;
  HitTest := false;
  CanFocused := false;
end;

function TvgColumn.CreateCellControl: TvgControl;
begin
  Result := TvgTextCell.Create(Self);
  TvgTextCell(Result).OnChange := DoTextChanged;
end;

procedure TvgColumn.DoTextChanged(Sender: TObject);
begin
  if Grid = nil then Exit;
  if FUpdateColumn then Exit;
  if FDisableChange then Exit;
  with vgStringToPoint(TvgObject(Sender).TagString) do
  begin
    Grid.SetValue(trunc(x), trunc(y), TvgControl(Sender).Data);
    if Assigned(Grid.FOnEdititingDone) then
      Grid.FOnEdititingDone(Grid, trunc(x), trunc(y));
  end;
end;

procedure TvgColumn.DoCanFocused(Sender: TObject; var ACanFocused: boolean);
begin
  if Grid = nil then Exit;
  ACanFocused := Grid.CanEditModify;
  if ACanFocused and ReadOnly then
    ACanFocused := false;
end;

procedure TvgColumn.DoEnterFocus(Sender: TObject);
begin
  if Grid = nil then Exit;
  Grid.ColumnIndex := Index;
  FSaveData := TvgObject(Sender).Data;
end;

procedure TvgColumn.DoKeyDown(Sender: TObject; var Key: Word; var KeyChar: System.WideChar;
  Shift: TShiftState);
begin
  if (KeyChar <> #0) and (Grid <> nil) and not Grid.CanEditAcceptKey(KeyChar) then
    KeyChar := #0;
  if (Key = VK_RETURN) and (Grid <> nil) then
    Grid.SetFocus;
  if (Key = VK_ESCAPE) and (Grid <> nil) then
  begin
    FDisableChange := true;
    try
      TvgObject(Sender).Data := FSaveData;
    finally
      FDisableChange := false;
    end;
    Grid.Reset;
  end;
  if (Key = VK_UP) or (Key = VK_DOWN) and (Grid <> nil) then
  begin
    Grid.SetFocus;
    Grid.KeyDown(Key, Keychar, Shift);
  end;
end;

function TvgColumn.Grid: TvgCustomGrid;
var
  P: TvgObject;
begin
  P := Parent;
  while (P <> nil) do
  begin
    if P is TvgCustomGrid then
    begin
      Result := TvgCustomGrid(P);
      Exit;
    end;
    P := P.Parent;
  end;
  Result := nil;
end;

procedure TvgColumn.ClearColumn;
var
  i: integer;
begin
  for i := 0 to High(FCellControls) do
    FCellControls[i].Free;
  SetLength(FCellControls, 0);
end;

function TvgColumn.CellControlByPoint(X, Y: single): TvgControl;
var
  i: integer;
  P: TvgPoint;
begin
  Result := nil;
  if Grid = nil then Exit;

  P := Grid.LocalToAbsolute(vgPoint(X, Y));
  for i := 0 to High(FCellControls) do
    if FCellControls[i].Visible and (FCellControls[i].pointInObject(P.X, P.Y)) then
    begin
      Result := FCellControls[i];
      Exit;
    end;
end;

function TvgColumn.CellControlByRow(Row: integer): TvgControl;
var
  i: integer;
begin
  Result := nil;
  if Grid = nil then Exit;

  for i := 0 to High(FCellControls) do
    if (FCellControls[i].Visible) and (trunc(vgStringToPoint(FCellControls[i].TagString).Y) = Row) then
    begin
      Result := FCellControls[i];
      Exit;
    end;
end;

procedure TvgColumn.UpdateColumn;
var
  i, C: integer;
  V: Variant;
begin
  if Grid = nil then Exit;
  FUpdateColumn := true;
  try
    { Create controls }
    if Length(FCellControls) < Min(Grid.RowCount, Grid.VisibleRows) then
    begin
      C := High(FCellControls);
      SetLength(FCellControls, Min(Grid.RowCount, Grid.VisibleRows));
      for i := C + 1 to Min(Grid.RowCount, Grid.VisibleRows) - 1 do
      begin
        FCellControls[i] := CreateCellControl;
        FCellControls[i].Parent := Self;
        FCellControls[i].HitTest := false;
        FCellControls[i].Visible := false;
        FCellControls[i].Locked := true;
        FCellControls[i].Stored := false;
        FCellControls[i].OnCanFocused := DoCanFocused;
        FCellControls[i].OnEnterFocus := DoEnterFocus;
        FCellControls[i].OnKeyDown := DoKeyDown;
      end;
    end;
    { Hide if need }
    if Length(FCellControls) > Min(Grid.RowCount, Grid.VisibleRows) then
    begin
      for i := Min(Grid.RowCount, Grid.VisibleRows) to High(FCellControls) do
        FCellControls[i].Visible := false;
    end;
    { Update Data }
    for i := 0 to Min(Grid.RowCount, Grid.VisibleRows) - 1 do
    begin
      if Grid.TopRow + i >= Grid.RowCount then Break;

      V := Grid.GetValue(Index, Grid.TopRow + i);

      FCellControls[i].Visible := true;
      FCellControls[i].SetBounds(0, i * Grid.RowHeight, Width, Grid.RowHeight);
      FCellControls[i].TagString := vgPointToString(vgPoint(Index, Grid.TopRow + i));
      FCellControls[i].Data := V;
    end;
    UpdateSelected;
  finally
    FUpdateColumn := false;
  end;
end;

procedure TvgColumn.UpdateSelected;
var
  i: integer;
begin
  if Grid = nil then Exit;
  if Grid.ReadOnly then Exit;
  if ReadOnly then Exit;
  for i := 0 to Min(Grid.RowCount, Grid.VisibleRows) - 1 do
  begin
    if Grid.IsSelected(Grid.TopRow + i) and (Grid.ColumnIndex = Index) then
    begin
      FCellControls[i].CanFocused := true;
      FCellControls[i].HitTest := true
    end
    else
    begin
      FCellControls[i].CanFocused := false;
      FCellControls[i].HitTest := false;
      if FCellControls[i].IsFocused then
        Grid.SetFocus;
    end;
  end;
end;

procedure TvgColumn.SetHeader(const Value: WideString);
begin
  if FHeader <> Value then
  begin
    FHeader := Value;
    if (Grid <> nil) and (Grid.FHeader <> nil) then
      Grid.FHeader.Items[Index].Text := FHeader;
  end;
end;

{ TvgCheckColumn }

constructor TvgCheckColumn.Create(AOwner: TComponent);
begin
  inherited;
end;

function TvgCheckColumn.CreateCellControl: TvgControl;
begin
  Result := TvgCheckCell.Create(Self);
  TvgCheckCell(Result).OnChange := DoCheckChanged;
end;

procedure TvgCheckColumn.DoCheckChanged(Sender: TObject);
begin
  if Grid = nil then Exit;
  if FUpdateColumn then Exit;
  with vgStringToPoint(TvgObject(Sender).TagString) do
  begin
    Grid.SetValue(trunc(x), trunc(y), TvgControl(Sender).Data);
    if Assigned(Grid.FOnEdititingDone) then
      Grid.FOnEdititingDone(Grid, trunc(x), trunc(y));
  end;
end;

{ TvgProgressColumn }

constructor TvgProgressColumn.Create(AOwner: TComponent);
begin
  inherited;
  FMax := 100;
end;

function TvgProgressColumn.CreateCellControl: TvgControl;
begin
  Result := TvgProgressCell.Create(Self);
  TvgProgressCell(Result).Min := FMin;
  TvgProgressCell(Result).Max := FMax;
end;

{ TvgPopupColumn }

constructor TvgPopupColumn.Create(AOwner: TComponent);
begin
  inherited;
  FItems := TvgWideStringList.Create;
end;

destructor TvgPopupColumn.Destroy;
begin
  FItems.Free;
  inherited;
end;

function TvgPopupColumn.CreateCellControl: TvgControl;
begin
  Result := TvgPopupCell.Create(Self);
  TvgPopupCell(Result).Items.Assign(FItems);
end;

procedure TvgPopupColumn.SetItems(const Value: TvgWideStrings);
begin
  FItems.Assign(Value);
end;

{ TvgImageColumn }

constructor TvgImageColumn.Create(AOwner: TComponent);
begin
  inherited;
end;

function TvgImageColumn.CreateCellControl: TvgControl;
begin
  Result := TvgImageCell.Create(Self);
  TvgImageCell(Result).EnableOpenDialog := false;
end;

{ Grids ======================================================================}

{ TvgCustomGrid ==============================================================}

constructor TvgCustomGrid.Create(AOwner: TComponent);
begin
  inherited;
  FOddFill := TvgBrush.Create(vgBrushSolid, $20000000);
  FLineFill := TvgBrush.Create(vgBrushSolid, $FF202020);
  FShowSelectedCell := true;
  FShowVertLines := true;
  FShowHorzLines := true;
  FShowHeader := true;
  FRowHeight := 21;
  FRowCount := 100;
  CanFocused := true;
  AutoCapture := true;
  Width := 100;
  Height := 100;
  FItemHeight := 0;
end;

destructor TvgCustomGrid.Destroy;
begin
  if FSelections <> nil then
    FSelections.Free;
  FLineFill.Free;
  FOddFill.Free;
  inherited;
end;

function TvgCustomGrid.ItemClass: string;
begin
  Result := 'TvgColumn;TvgCheckColumn;TvgProgressColumn;TvgPopupColumn;TvgImageColumn';
end;

procedure TvgCustomGrid.FreeStyle;
begin
  inherited;
  FSelection := nil;
  FFocus := nil;
  FHeader := nil;
end;

procedure TvgCustomGrid.ApplyStyle;
var
  T: TvgObject;
begin
  inherited;
  T := FindResource('content');
  if (T <> nil) and (T.IsVisual) then
  begin
    TvgVisualObject(T).OnBeforePaint := DoContentPaint;
    TvgVisualObject(T).OnPaint := DoContentPaint2;
  end;
  T := FindResource('header');
  if (T <> nil) and (T is TvgHeader) then
  begin
    FHeader := TvgHeader(T);
    FHeader.OnRealignItem := DoRealignItem;
    FHeader.OnResizeItem := DoResizeItem;
    FHeader.Visible := FShowHeader;
  end;
  T := FindResource('selection');
  if (T <> nil) and (T is TvgVisualObject) then
  begin
    FSelection := TvgVisualObject(T);
    FSelection.Visible := false;
  end;
  T := FindResource('focus');
  if (T <> nil) and (T is TvgVisualObject) then
  begin
    FFocus := TvgVisualObject(T);
    FFocus.Visible := false;
  end;
  T := FindResource('AlternatingRowBackground');
  if (T <> nil) and (T is TvgBrushObject) then
  begin
    FOddFill.Assign(TvgBrushObject(T).Brush);
  end;
  T := FindResource('LineFill');
  if (T <> nil) and (T is TvgBrushObject) then
  begin
    FLineFill.Assign(TvgBrushObject(T).Brush);
  end;
  UpdateColumns;
  UpdateHeader;
  UpdateSelection;
end;

procedure TvgCustomGrid.UpdateSelection;
var
  i: integer;
  P: TvgPoint;
  R: TvgRect;
  Sel: boolean;
  SelRects: array of TvgRect;
  Clone: TvgVisualObject;
  Vis: boolean;
begin
  if FSelection = nil then Exit;
  if ColumnCount = 0 then Exit;
  if RowCount = 0 then
  begin
    if FSelections <> nil then
      for i := 0 to FSelections.Count - 1 do
        TvgVisualObject(FSelections[i]).Visible := false;
    if FFocus <> nil then
      FFocus.Visible := false;
    Exit;
  end;
  // calc rects
  Vis := true;
  Sel := false;
  SetLength(SelRects, 0);
  for i := 0 to RowCount - 1 do
  begin
    if IsSelected(i) then
    begin
      P := vgPoint(0, i * FRowHeight);
      P := FContent.LocalToAbsolute(P);
      if (FSelection.Parent <> nil) and FSelection.Parent.IsVisual then
        P := FSelection.Parent.Visual.AbsoluteToLocal(P);
      if FContent.Width < ClientWidth then
        R := vgRect(P.X, P.Y, P.X + ClientWidth, P.Y + FRowHeight)
      else
        R := vgRect(P.X, P.Y, P.X + FContent.Width, P.Y + FRowHeight);
      if (Length(SelRects) > 0) and (i > 0) and (IsSelected(i - 1)) then
        SelRects[High(SelRects)] := vgUnionRect(R, SelRects[High(SelRects)])
      else
      begin
        SetLength(SelRects, Length(SelRects) + 1);
        SelRects[High(SelRects)] := R;
      end;
      Sel := true;
    end;
  end;
  // Create selection list
  if FSelections = nil then
    FSelections := TList.Create;
  // create selections
  if FSelections.Count < Length(SelRects) then
    for i := FSelections.Count to Length(SelRects) - 1 do
    begin
      Clone := TvgVisualObject(FSelection.Clone(Self));
      Clone.ResourceName := '';
      FSelections.Add(Clone);
      Clone.Parent := FSelection.Parent;
      Clone.Stored := false;
    end;
  // hide if not need
  if Length(SelRects) < FSelections.Count then
    for i := Length(SelRects) to FSelections.Count - 1 do
    begin
      TvgVisualObject(FSelections[i]).Visible := false;
    end;
  // align selections
  for i := 0 to High(SelRects) do
  begin
    TvgVisualObject(FSelections[i]).Visible := Vis;
    if Vis then
    begin
      with SelRects[i] do
        TvgVisualObject(FSelections[i]).SetBounds(Left, Top, Right - Left, Bottom - Top);
      if (FFocus <> nil) and (FShowSelectedCell) then
      begin
        FFocus.Visible := true;
        FFocus.BringToFront;
        P := vgPoint(Columns[ColumnIndex].Position.X, 0);
        P := FContent.LocalToAbsolute(P);
        if (FSelection.Parent <> nil) and FSelection.Parent.IsVisual then
          P := FSelection.Parent.Visual.AbsoluteToLocal(P);
        with SelRects[i] do
          FFocus.SetBounds(P.X + FFocus.Padding.Left, SelRects[i].Top + FFocus.Padding.Top,
          Columns[FColumnIndex].Width - (FFocus.Padding.Left + FFocus.Padding.Right),
            SelRects[i].Bottom - SelRects[i].Top - (FFocus.Padding.Top + FFocus.Padding.Bottom));
      end;
    end;
  end;
  //
  for i := 0 to ColumnCount - 1 do
    Columns[i].UpdateSelected;
end;

procedure TvgCustomGrid.DoRealignItem(Sender: TObject; OldIndex, NewIndex: integer);
begin
  if ColumnIndex = Columns[OldIndex].Index then
    ColumnIndex := NewIndex;
  Columns[OldIndex].Index := NewIndex;
end;

procedure TvgCustomGrid.DoResizeItem(Sender: TObject;
  var NewSize: single);
begin
  if NewSize < 10 then NewSize := 10;
  Columns[TvgHeaderItem(Sender).Index].Width := NewSize;
end;

procedure TvgCustomGrid.UpdateHeader;
var
  i: integer;
  Item: TvgHeaderItem;
begin
  if FHeader = nil then Exit;

  FHeader.FOffset := -HScrollBar.Value;
  FHeader.FChildren.Remove(FHeader.FLastItem);
  if FHeader.ChildrenCount < ColumnCount then
    for i := FHeader.ChildrenCount to ColumnCount - 1 do
    begin
      Item := TvgHeaderItem.Create(Self);
      Item.Parent := FHeader;
      Item.Locked := true;
      Item.Stored := false;
    end;
  FHeader.FChildren.Add(FHeader.FLastItem);

  for i := 0 to ColumnCount - 1 do
  begin
    TvgHeaderItem(FHeader.Children[i]).Text := Columns[i].Header;
    TvgHeaderItem(FHeader.Children[i]).Width := Columns[i].Width;
  end;
  if FHeader <> nil then
    FHeader.Realign;
end;

procedure TvgCustomGrid.UpdateColumns;
var
  i: integer;
begin
  for i := 0 to ColumnCount - 1 do
  begin
    Columns[i].Position.Y := (TopRow * FRowHeight);
    Columns[i].UpdateColumn;
  end;
end;

procedure TvgCustomGrid.DoContentPaint(Sender: TObject;
  const Canvas: TvgCanvas; const ARect: TvgRect);
var
  i: integer;
  P: TvgPoint;
begin
  if (FContent <> nil) and (ContentLayout <> nil) then
  begin
    if FAlternatingRowBackground then
    begin
      Canvas.Fill.Assign(FOddFill);
      for i := 0 to Min(RowCount, VisibleRows) - 1 do
      begin
        if Odd(TopRow + i) then
        begin
          P := FContent.LocalToAbsolute(vgPoint(0, (TopRow + i) * FRowHeight));
          P := TvgVisualObject(Sender).AbsoluteToLocal(P);
          Canvas.FillRect(vgRect(P.X, P.Y, P.X + FContent.Width, P.Y + FRowHeight), 0, 0, [], AbsoluteOpacity);
        end;
      end;
    end;
  end;
end;

procedure TvgCustomGrid.DoContentPaint2(Sender: TObject;
  const Canvas: TvgCanvas; const ARect: TvgRect);
var
  i: integer;
  P: TvgPoint;
  State: integer;
begin
  if (FContent <> nil) and (ContentLayout <> nil) and (FShowVertLines or FShowHorzLines) then
  begin
    State := Canvas.SaveCanvas;
    Canvas.IntersectClipRect(ARect);

    Canvas.Stroke.Assign(FLineFill);
    if FShowHorzLines then
      for i := 0 to Min(RowCount, VisibleRows) do
      begin
        P := FContent.LocalToAbsolute(vgPoint(0, (TopRow + i) * FRowHeight));
        P := TvgVisualObject(Sender).AbsoluteToLocal(P);
        Canvas.DrawLine(vgPoint(P.X, P.Y - 0.5), vgPoint(P.X + FContent.Width, P.Y - 0.5), AbsoluteOpacity);
      end;
    if FShowVertLines then
      for i := 0 to ColumnCount - 1 do
      begin
        P := Columns[i].LocalToAbsolute(vgPoint(Columns[i].Width, 0));
        P := TvgVisualObject(Sender).AbsoluteToLocal(P);
        Canvas.DrawLine(vgPoint(P.X + 0.5, P.Y), vgPoint(P.X + 0.5, P.Y + (Min(RowCount, VisibleRows) * FRowHeight)), AbsoluteOpacity);
      end;

    Canvas.RestoreCanvas(State);
  end;
end;

function TvgCustomGrid.GetContentBounds: TvgRect;
var
  Col, R: TvgRect;
  i, Idx: integer;
begin
  Result := LocalRect;
  if FUpdating > 0 then Exit;
  if ContentLayout = nil then Exit;
  R := ContentLayout.LocalRect;
  if ColumnCount > 0 then
  begin
    R.Right := R.Left;
    R.Top := R.Top;
    for i := 0 to ColumnCount - 1 do
    begin
      Col := vgRect(R.Right, R.Top, R.Right + Columns[i].Width, R.Bottom);
      R.Right := R.Right + Columns[i].Width;
      Columns[i].SetBounds(Col.Left, (TopRow * FRowHeight), Columns[i].Width, ClientHeight);
      Columns[i].UpdateColumn;
    end;
    R.Bottom := R.Top + (FRowCount * FRowHeight);
  end;
  if vgRectWidth(R) < ContentLayout.Width then
    R.Right := R.Left + ContentLayout.Width;
  if vgRectHeight(R) < ContentLayout.Height then
    R.Bottom := R.Top + ContentLayout.Height;
  Result := R;
  UpdateColumns;
  UpdateHeader;
  UpdateSelection;
end;

procedure TvgCustomGrid.HScrollChange(Sender: TObject);
begin
  inherited;
  UpdateSelection;
  UpdateHeader;
end;

procedure TvgCustomGrid.VScrollChange(Sender: TObject);
begin
  inherited;
  UpdateColumns;
  UpdateSelection;
end;

function TvgCustomGrid.GetColumnCount: integer;
var
  i: integer;
begin
  Result := 0;
  if (FContent <> nil) and (FContent.ChildrenCount > 0) then
    for i := 0 to FContent.ChildrenCount - 1 do
      if FContent.Children[i] is TvgColumn then
      begin
        Inc(Result);
      end;
end;

function TvgCustomGrid.RowByPoint(const X, Y: single): integer;
var
  i: integer;
  P, P1: TvgPoint;
begin
  P := LocalToAbsolute(vgPoint(X, Y));
  if FContent <> nil then
  begin
    P := FContent.AbsoluteToLocal(P);
    Result := trunc(P.Y / FRowHeight);
    Exit;
  end;
  Result := -1;
end;

function TvgCustomGrid.ColumnByIndex(const Idx: integer): TvgColumn;
var
  c, i: integer;
begin
  c := 0;
  if (FContent <> nil) and (FContent.ChildrenCount > 0) then
    for i := 0 to FContent.ChildrenCount - 1 do
      if FContent.Children[i] is TvgColumn then
      begin
        if c = Idx then
        begin
          Result := TvgColumn(FContent.Children[i]);
          Exit;
        end;
        Inc(c);
      end;
  Result := nil;
end;

function TvgCustomGrid.ColumnByPoint(const X, Y: single): TvgColumn;
var
  i: integer;
  P, P1: TvgPoint;
begin
  P := LocalToAbsolute(vgPoint(X, Y));
  if (FContent <> nil) and (FContent.ChildrenCount > 0) then
    for i := 0 to FContent.ChildrenCount - 1 do
      if FContent.Children[i] is TvgColumn then
      begin
        if not TvgColumn(FContent.Children[i]).Visible then Continue;
        if not vgIntersectRect(TvgColumn(FContent.Children[i]).UpdateRect, UpdateRect) then Continue;
        if TvgColumn(FContent.Children[i]).pointInObject(P.X, P.Y) then
        begin
          Result := TvgColumn(FContent.Children[i]);
          Exit;
        end
      end;
  Result := nil;
end;

function TvgCustomGrid.CanEditModify: Boolean;
begin
  Result := not ReadOnly;
end;

function TvgCustomGrid.CanEditAcceptKey(Key: System.WideChar): Boolean;
begin
  Result := true;
end;

procedure TvgCustomGrid.KeyDown(var Key: Word; var KeyChar: System.WideChar;
  Shift: TShiftState);
begin
  inherited ;
  if RowCount > 0 then
  begin
    if (KeyChar <> #0) and not (ReadOnly) and not (Columns[ColumnIndex].ReadOnly) then
    begin
      if (Columns[ColumnIndex] <> nil) and (Columns[ColumnIndex].CellControlByRow(Selected) <> nil) then
      begin
        Columns[ColumnIndex].CellControlByRow(Selected).SetFocus;
        TvgHackVisualObject(Columns[ColumnIndex].CellControlByRow(Selected)).KeyDown(Key, KeyChar, Shift);
      end;
    end;
    case Key of
      VK_F2:
        begin
          if (Columns[ColumnIndex] <> nil) and (Columns[ColumnIndex].CellControlByRow(Selected) <> nil) then
            Columns[ColumnIndex].CellControlByRow(Selected).SetFocus;
        end;
      VK_UP: if FSelected > 0 then Selected := Selected - 1;
      VK_DOWN: if FSelected < FRowCount - 1 then Selected := Selected + 1;
      VK_HOME:
        if ssCtrl in Shift then
          Selected := 0
        else
          ColumnIndex := 0;
      VK_END:
        if ssCtrl in Shift then
          Selected := RowCount - 1
        else
          ColumnIndex := ColumnCount - 1;
      VK_PRIOR: if FSelected > 0 then Selected := Selected - Min(RowCount, VisibleRows);
      VK_NEXT: if FSelected < FRowCount - 1 then Selected := Selected + Min(RowCount, VisibleRows);
      VK_LEFT: if ColumnIndex > 0 then ColumnIndex := ColumnIndex - 1;
      VK_RIGHT: if ColumnIndex < ColumnCount - 1 then ColumnIndex := ColumnIndex + 1;
    else
      Exit;
    end;
    Key := 0;
  end;
end;

procedure TvgCustomGrid.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: single);
var
  Column: TvgColumn;
begin
  inherited;
  if Button = mbLeft then
  begin
    Selected := RowByPoint(X, Y);
    Column := ColumnByPoint(X, Y);
    if Column <> nil then
      ColumnIndex := Column.Index;
  end;
end;

procedure TvgCustomGrid.MouseMove(Shift: TShiftState; X, Y, Dx, Dy: single);
var
  Column: TvgColumn;
begin
  inherited;
  if FPressed then
  begin
    Selected := RowByPoint(X, Y);
    Column := ColumnByPoint(X, Y);
    if Column <> nil then
      ColumnIndex := Column.Index;
  end;
end;

procedure TvgCustomGrid.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: single);
begin
  inherited;
end;

procedure TvgCustomGrid.AddObject(AObject: TvgObject);
begin
  if (FContent <> nil) and ((AObject is TvgColumn) or (AObject is TvgSelectionItem)) then
  begin
    FContent.AddObject(AObject);
    if FUpdating = 0 then
      Realign;
  end
  else
    inherited;
end;

procedure TvgCustomGrid.RemoveObject(AObject: TvgObject);
begin
  inherited;
end;

procedure TvgCustomGrid.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
end;

function TvgCustomGrid.GetColumn(Index: integer): TvgColumn;
begin
  Result := ColumnByIndex(Index);
end;

procedure TvgCustomGrid.SetRowCount(const Value: integer);
begin
  if FRowCount <> Value then
  begin
    FRowCount := Value;
    Realign;
    if (FSelected >= FRowCount) and (FRowCount > 0) then
      Selected := FRowCount - 1;
  end;
end;

procedure TvgCustomGrid.SetRowHeight(const Value: single);
begin
  if FRowHeight <> Value then
  begin
    FRowHeight := Value;
    if FRowHeight < 5 then FRowHeight := 5;
    Realign;
  end;
end;

function TvgCustomGrid.GetTopRow: integer;
begin
  if VScrollBar <> nil then
    Result := trunc(VScrollBar.Value / FRowHeight)
  else
    Result := 0;
end;

function TvgCustomGrid.GetVisibleRows: integer;
begin
  Result := trunc(ClientHeight / FRowHeight) + 2;
end;

function TvgCustomGrid.GetValue(Col, Row: integer): Variant;
begin
  Result := NULL;
  if Assigned(FOnGetValue) then
    FOnGetValue(Self, Col, Row, Result);
end;

procedure TvgCustomGrid.SetValue(Col, Row: integer; const Value: Variant);
begin
  if Assigned(FOnSetValue) then
    FOnSetValue(Self, Col, Row, Value);
end;

procedure TvgCustomGrid.SetSelected(const Value: integer);
begin
  if FSelected <> Value then
  begin
    FSelected := Value;
    if FSelected < 0 then FSelected := 0;
    if FSelected > FRowCount - 1 then FSelected := FRowCount - 1;

    if FSelected <= TopRow then
      VScrollBar.Value := FSelected * FRowHeight;
    if FSelected * FRowHeight > VScrollBar.Value + ClientHeight - FRowHeight then
      VScrollBar.Value := FSelected * FRowHeight - ClientHeight + FRowHeight;
    UpdateSelection;
  end;
end;

function TvgCustomGrid.IsSelected(Row: integer): boolean;
begin
  Result := Row = FSelected;
end;

procedure TvgCustomGrid.SetAlternatingRowBackground(const Value: boolean);
begin
  if FAlternatingRowBackground <> Value then
  begin
    FAlternatingRowBackground := Value;
    Repaint;
  end;
end;

procedure TvgCustomGrid.SetShowHorzLines(const Value: boolean);
begin
  if FShowHorzLines <> Value then
  begin
    FShowHorzLines := Value;
    Repaint;
  end;
end;

procedure TvgCustomGrid.SetShowVertLines(const Value: boolean);
begin
  if FShowVertLines <> Value then
  begin
    FShowVertLines := Value;
    Repaint;
  end;
end;

procedure TvgCustomGrid.SetColumnIndex(const Value: integer);
begin
  if FColumnIndex <> Value then
  begin
    FColumnIndex := Value;
    UpdateSelection;
    if Columns[FColumnIndex].Position.X < HScrollBar.Value then
      HScrollBar.Value := Columns[FColumnIndex].Position.X;
    if Columns[FColumnIndex].Position.X + Columns[FColumnIndex].Width > HScrollBar.Value + ClientWidth then
      HScrollBar.Value := Columns[FColumnIndex].Position.X + Columns[FColumnIndex].Width - ClientWidth;
  end;
end;

procedure TvgCustomGrid.SetShowHeader(const Value: boolean);
begin
  if FShowHeader <> Value then
  begin
    FShowHeader := Value;
    if FHeader <> nil then
      FHeader.Visible := FShowHeader;
  end;
end;

procedure TvgCustomGrid.Reset;
begin
  SetFocus;
end;

{ TvgStringColumn }

constructor TvgStringColumn.Create(AOwner: TComponent);
begin
  inherited;
end;

destructor TvgStringColumn.Destroy;
begin
  inherited;
end;

procedure TvgStringColumn.UpdateColumn;
begin
  if Grid = nil then Exit;
  SetLength(FCells, Grid.RowCount);
  inherited;
end;

{ TvgStringGrid }

constructor TvgStringGrid.Create(AOwner: TComponent);
begin
  inherited;
  FResource := 'gridstyle';
end;

destructor TvgStringGrid.Destroy;
begin
  inherited;
end;

function TvgStringGrid.GetValue(Col, Row: integer): Variant;
var
  C: TvgColumn;
begin
  C := Columns[Col];
  if C <> nil then
  begin
    if Length(TvgStringColumn(C).FCells) <> RowCount then
      SetLength(TvgStringColumn(C).FCells, RowCount);
    Result := TvgStringColumn(C).FCells[Row]
  end
  else
    Result := NULL;
end;

procedure TvgStringGrid.SetValue(Col, Row: integer; const Value: Variant);
var
  C: TvgColumn;
begin
  C := Columns[Col];
  if (C <> nil) then
  begin
    if Length(TvgStringColumn(C).FCells) <> RowCount then
      SetLength(TvgStringColumn(C).FCells, RowCount);
    TvgStringColumn(C).FCells[Row] := VarToWideStr(Value);
  end;
end;

function TvgStringGrid.GetCells(ACol, ARow: Integer): WideString;
begin
  Result := GetValue(ACol, ARow);
end;

procedure TvgStringGrid.SetCells(ACol, ARow: Integer;
  const Value: WideString);
begin
  SetValue(ACol, ARow, Value)
end;

function TvgStringGrid.ItemClass: string;
begin
  Result := 'TvgStringColumn';
end;

procedure TvgCustomGrid.SetShowSelectedCell(const Value: boolean);
begin
  if FShowSelectedCell <> Value then
  begin
    FShowSelectedCell := Value;
    UpdateSelection;
  end;
end;

initialization
  RegisterClasses([TvgTextCell, TvgCheckCell, TvgProgressCell, TvgPopupCell, TvgStringColumn]);
  RegisterVGObjects('Grid', [TvgGrid, TvgStringGrid, TvgHeader]);
  RegisterVGObjects('Grid Columns', [TvgColumn, TvgCheckColumn, TvgProgressColumn, TvgPopupColumn, TvgImageColumn]);
  RegisterVGObjects('Items', [TvgHeaderItem]);
end.


