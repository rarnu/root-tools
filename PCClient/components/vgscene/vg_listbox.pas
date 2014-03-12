unit vg_listbox;

{$I vg_define.inc}

interface

uses
  {$IFDEF WINDOWS}
  Windows,
  {$ENDIF}
  {$IFNDEF NOVCL} Controls, Forms, {$ENDIF}
  {$IFDEF LCL} LCLType, {$ENDIF}
  Classes, Math, SysUtils, vg_scene, vg_objects, vg_controls,
  vg_layouts;

type

  TvgListBox = class;
  TvgComboBox = class;

  { TvgListBoxItem }

  TvgListBoxItem = class(TvgTextControl)
  private
    FIsChecked: boolean;
    FCheck: TvgCheckBox;
    FIsSelected: boolean;
    procedure SetIsChecked(const Value: boolean);
    procedure DoCheckClick(Sender: TObject);
    procedure UpdateCheck;
    procedure SetIsSelected(const Value: boolean);
  protected
    function ListBox: TvgListBox;
    function ComboBox: TvgComboBox;
    procedure ApplyStyle; override;
    procedure FreeStyle; override;
    function GetParentComponent: TComponent; override;
    procedure DesignInsert; override;
    procedure DesignSelect; override;
    function EnterFocusChildren(AObject: TvgVisualObject): boolean; override;
    procedure DragEnd; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
  published
    property IsChecked: boolean read FIsChecked write SetIsChecked;
    property IsSelected: boolean read FIsSelected write SetIsSelected;
    property AutoTranslate default true;
    property Font;
    property Resource;
    property Text;
    property TextAlign default vgTextAlignNear;
    property WordWrap;
  end;

  TvgListStyle = (
    vgListVertical,
    vgListHorizontal
  );

  TOnCompareListBoxItemEvent = function(Item1, Item2: TvgListBoxItem): integer of object;
  TOnListBoxDragChange = procedure (SourceItem, DestItem: TvgListBoxItem; Allow: boolean) of object;

  TvgListBox = class(TvgScrollBox)
  private
    FMouseSelecting: boolean;
    FOnChange: TNotifyEvent;
    FHideSelectionUnfocused: boolean;
    FShowCheckboxes: boolean;
    FOnChangeCheck: TNotifyEvent;
    FSorted: boolean;
    FOnCompare: TOnCompareListBoxItemEvent;
    FMultiSelect: boolean;
    FAlternatingRowBackground: boolean;
    FAllowDrag: boolean;
    FDragItem: TvgListBoxItem;
    FOnDragChange: TOnListBoxDragChange;
    function GetCount: integer;
    function GetSelected: TvgListBoxItem;
    procedure SetColumns(const Value: integer);
    procedure SetItemHeight(const Value: single);
    procedure SetItemWidth(const Value: single);
    procedure SetListStyle(const Value: TvgListStyle);
    procedure SetShowCheckboxes(const Value: boolean);
    function GetItem(Index: integer): TvgListBoxItem;
    procedure SetSorted(const Value: boolean);
    procedure SetAlternatingRowBackground(const Value: boolean);
    procedure SetMultiSelect(const Value: boolean);
    procedure SetAllowDrag(const Value: boolean);
  protected
    FColumns: integer;
    FItemWidth: single;
    FItemHeight: single;
    FListStyle: TvgListStyle;
    FFirstSelect: TvgListBoxItem;
    FSelection: TvgVisualObject;
    FSelections: TList;
    FOddFill: TvgBrush;
    FItemIndex: integer;
    procedure SortItems; virtual;
    procedure SetItemIndex(const Value: integer); virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: single); override;
    procedure MouseMove(Shift: TShiftState; X, Y, Dx, Dy: single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: single); override;
    procedure KeyDown(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState); override;
    procedure DragOver(const Data: TvgDragObject; const Point: TvgPoint; var Accept: Boolean); override;
    procedure DragDrop(const Data: TvgDragObject; const Point: TvgPoint); override;
    procedure ApplyStyle; override;
    procedure FreeStyle; override;
    procedure EnterFocus; override;
    procedure KillFocus; override;
    function GetData: Variant; override;
    procedure SetData(const Value: Variant); override;
    function GetContentBounds: TvgRect; override;
    procedure DoContentPaint(Sender: TObject; const Canvas: TvgCanvas; const ARect: TvgRect);
    procedure HScrollChange(Sender: TObject); override;
    procedure VScrollChange(Sender: TObject); override;
    procedure ContentAddObject(AObject: TvgObject); override;
    procedure ContentBeforeRemoveObject(AObject: TvgObject); override;
    procedure ContentRemoveObject(AObject: TvgObject); override;
    procedure UpdateSelection;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ItemClass: string; override;
    procedure Clear; virtual;
    procedure SelectAll;
    procedure ClearSelection;
    procedure SelectRange(Item1, Item2: TvgListBoxItem);
    function ItemByPoint(const X, Y: single): TvgListBoxItem;
    function ItemByIndex(const Idx: integer): TvgListBoxItem;
    procedure Exchange(Item1, Item2: TvgListBoxItem);
    procedure AddObject(AObject: TvgObject); override;
    property Count: integer read GetCount;
    property Selected: TvgListBoxItem read GetSelected;
    property Items[Index: integer]: TvgListBoxItem read GetItem;
  published
    property Resource;
    property AllowDrag: boolean read FAllowDrag write SetAllowDrag default false;
    property CanFocused default true;
    property DisableFocusEffect;
    property TabOrder;
    property AlternatingRowBackground: boolean read FAlternatingRowBackground write SetAlternatingRowBackground default false;
    property Columns: integer read FColumns write SetColumns default 1;
    property HideSelectionUnfocused: boolean read FHideSelectionUnfocused write FHideSelectionUnfocused default true;
    property ItemIndex: integer read FItemIndex write SetItemIndex default -1;
    property ItemWidth: single read FItemWidth write SetItemWidth;
    property ItemHeight: single read FItemHeight write SetItemHeight;
    property ListStyle: TvgListStyle read FListStyle write SetListStyle default vgListVertical;
    property MultiSelect: boolean read FMultiSelect write SetMultiSelect default false;
    property Sorted: boolean read FSorted write SetSorted default false;
    property ShowCheckboxes: boolean read FShowCheckboxes write SetShowCheckboxes default false;
    property BindingSource;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChangeCheck: TNotifyEvent read FOnChangeCheck write FOnChangeCheck;
    property OnCompare: TOnCompareListBoxItemEvent read FOnCompare write FOnCompare;
    property OnDragChange: TOnListBoxDragChange read FOnDragChange write FOnDragChange;
  end;

  TvgComboListBox = class(TvgListbox)
  private
  protected
    FComboBox: TvgControl;
    procedure ApplyResource; override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    function GetParentComponent: TComponent; override;
    procedure KillFocus; override;
    procedure KeyDown(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: single); override;
    procedure MouseMove(Shift: TShiftState; X, Y, Dx, Dy: single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: single); override;
    procedure MouseWheel(Shift: TShiftState; WheelDelta: integer; var Handled: boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
  end;

  TvgComboBox = class(TvgControl)
  private
    FDropDownCount: integer;
    FPopup: TvgPopup;
    FListBox: TvgComboListBox;
    FOnChange: TNotifyEvent;
    FPlacement: TvgPlacement;
    procedure SetItemIndex(const Value: integer);
    function GetItemIndex: integer;
    function GetCount: integer;
    procedure SetListBoxResource(const Value: string);
    function GetListBoxResource: string;
    function GetItemHeight: single;
    procedure SetItemHeight(const Value: single);
  protected
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    procedure ApplyStyle; override;
    procedure DoListBoxChange(Sender: TObject);
    procedure DoContentPaint(Sender: TObject; const Canvas: TvgCanvas; const ARect: TvgRect); virtual;
    procedure DesignClick; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: single); override;
    procedure MouseMove(Shift: TShiftState; X, Y, Dx, Dy: single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: single); override;
    procedure MouseWheel(Shift: TShiftState; WheelDelta: integer;
      var Handled: boolean); override;
    procedure KeyDown(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ItemClass: string; override;
    procedure Paint; override;
    procedure PaintChildren; override;
    procedure Realign; override;
    procedure Clear; virtual;
    procedure DropDown; virtual;
    procedure AddObject(AObject: TvgObject); override;
    property ListBox: TvgComboListBox read FListBox write FListBox;
    property Count: integer read GetCount;
  published
    property CanFocused default true;
    property DisableFocusEffect;
    property TabOrder;
    property Resource;
    property ItemIndex: integer read GetItemIndex write SetItemIndex;
    property ItemHeight: single read GetItemHeight write SetItemHeight;
    property DropDownCount: integer read FDropDownCount write FDropDownCount default 8;
    property Placement: TvgPlacement read FPlacement write FPlacement;
    property BindingSource;
    property ListBoxResource: string read GetListBoxResource write SetListBoxResource;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TvgStringListBox = class(TvgListBox)
  private
    FItems: TvgWideStrings;
    FTextAlign: TvgTextAlign;
    FItemStyle: string;
    FFont: TvgFont;
    procedure SetItems(const Value: TvgWideStrings);
    procedure SetTextAlign(const Value: TvgTextAlign);
    procedure SetFont(const Value: TvgFont);
  protected
    procedure SortItems; override;
    function GetData: Variant; override;
    procedure SetData(const Value: Variant); override;
    procedure ApplyStyle; override;
    procedure RebuildList;
    procedure DoItemsChanged(Sender: TObject);
    procedure FontChanged(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure EndUpdate; override;
  published
    property AutoTranslate default true;
    property TextAlign: TvgTextAlign read FTextAlign write SetTextAlign default vgTextAlignCenter;
    property Font: TvgFont read FFont write SetFont;
    property Items: TvgWideStrings read FItems write SetItems;
    property ItemIndex;
    property BindingSource;
  end;

  TvgStringComboBox = class(TvgComboBox)
  private
    FItemHeight: single;
    FItems: TvgWideStrings;
    FTextAlign: TvgTextAlign;
    FItemStyle: string;
    procedure SetItemHeight(const Value: single);
    procedure SetItems(const Value: TvgWideStrings);
    procedure SetTextAlign(const Value: TvgTextAlign);
  protected
    function GetData: Variant; override;
    procedure SetData(const Value: Variant); override;
    procedure ApplyStyle; override;
    procedure RebuildList;
    procedure DoItemsChanged(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear; override;
  published
    property AutoTranslate default true;
    property TextAlign: TvgTextAlign read FTextAlign write SetTextAlign default vgTextAlignCenter;
    property ItemHeight: single read FItemHeight write SetItemHeight;
    property Items: TvgWideStrings read FItems write SetItems;
    property ItemIndex;
  end;

  TvgHorzListBox = class(TvgListBox)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
  published
    property ListStyle default vgListHorizontal;
  end;

  TvgImageThread = class(TThread)
  private
    FImage: TvgImage;
    FTempBitmap: TvgBitmap;
    FFileName: string;
    FUseThumbnails: boolean;
  protected
    procedure Execute; override;
    procedure Finished;
  public
    constructor Create(const AImage: TvgImage; const AFileName: string; const AUseThumbnails: boolean);
    destructor Destroy; override;
  end;

  TvgImageListBoxItem = class(TvgListBoxItem)
  private
    function TextBorder: TvgVisualObject;
  protected
    procedure ApplyStyle; override;
  public
    function Text: TvgText;
  published
  end;

  TvgImageListBox = class(TvgListBox)
  private
    FFolder: string;
    FShowFileName: boolean;
    FItemHeight: single;
    FUseThumbnails: boolean;
    function GetSelectedFileName: string;
    procedure SetShowFileName(const Value: boolean);
    procedure SetItemHeight(const Value: single);
    function GetSelectedImage: TvgImage;
    function GetImage(Index: integer): TvgImage;
  protected
    procedure DoApplyResource(Sender: TObject);
    procedure BeginAutoDrag; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure AddFolder(const Folder: string);
    procedure AddFile(const AFile: string);
    procedure AddBitmap(const AFile: string; const ABitmap: TvgBitmap);
    procedure Clear; override;
    property Images[Index: integer]: TvgImage read GetImage;
    property SelectedFileName: string read GetSelectedFileName;
    property SelectedImage: TvgImage read GetSelectedImage;
  published
    property ShowFileName: boolean read FShowFileName write SetShowFileName;
    property UseThumbnails: boolean read FUseThumbnails write FUseThumbnails default true;
    property ItemHeight: single read FItemHeight write SetItemHeight;
  end;

  TvgHudImageListBox = class(TvgImageListBox)
  private
  protected
  public
  published
  end;

  TvgHorzImageListBox = class(TvgImageListBox)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
  published
    property ListStyle default vgListHorizontal;
  end;

  TvgHudHorzImageListBox = class(TvgHorzImageListBox)
  private
  protected
  public
  published
  end;

  TvgHudListBox = class(TvgListBox)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
  published
  end;

  TvgHudHorzListBox = class(TvgHorzListBox)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
  published
  end;

  TvgHudStringListBox = class(TvgStringListBox)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
  published
  end;

  TvgHudComboBox = class(TvgComboBox)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
  published
  end;

  TvgHudStringComboBox = class(TvgStringComboBox)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
  published
  end;

implementation {===============================================================}

uses vg_ani, vg_textbox;

type
  TvgHackObject = class(TvgVisualObject);

{ TvgListBoxItem }

constructor TvgListBoxItem.Create(AOwner: TComponent);
begin
  inherited;
  Position.Point := vgPoint(5000, 5000);
  TextAlign := vgTextAlignNear;
  FAutoTranslate := true;
  FText := '';
  Height := 19;
  Width := 19;
  HitTest := false;
end;

destructor TvgListBoxItem.Destroy;
begin
  inherited;
end;

procedure TvgListBoxItem.ApplyStyle;
var
  B: TvgObject;
begin
  inherited;
  B := FindResource('check');
  if (B <> nil) and (B is TvgCheckBox) then
  begin
    FCheck := TvgCheckBox(B);
    FCheck.IsChecked := IsChecked;
    FCheck.OnChange := DoCheckClick;
    if ListBox <> nil then
      FCheck.Visible := ListBox.ShowCheckboxes
    else
      FCheck.Visible := false;
  end;
  StartTriggerAnimation(Self, 'IsSelected');
end;

procedure TvgListBoxItem.FreeStyle;
begin
  inherited;
  FCheck := nil;
end;

procedure TvgListBoxItem.DoCheckClick(Sender: TObject);
begin
  if FCheck <> nil then
    FIsChecked := FCheck.IsChecked;
  if ListBox <> nil then
  begin
    ListBox.SetFocus;
    ListBox.ItemIndex := Index;
    if Assigned(ListBox.OnChangeCheck) then
      ListBox.OnChangeCheck(Self);
  end;
end;

function TvgListBoxItem.ComboBox: TvgComboBox;
var
  P: TvgObject;
begin
  P := Parent;
  while (P <> nil) do
  begin
    if P is TvgComboBox then
    begin
      Result := TvgComboBox(P);
      Exit;
    end;
    P := P.Parent;
  end;
  Result := nil;
end;

function TvgListBoxItem.ListBox: TvgListBox;
var
  P: TvgObject;
begin
  P := Parent;
  while (P <> nil) do
  begin
    if P is TvgListBox then
    begin
      Result := TvgListBox(P);
      Exit;
    end;
    if P is TvgComboBox then
    begin
      Result := TvgComboBox(P).FListBox;
      Exit;
    end;
    P := P.Parent;
  end;
  Result := nil;
end;

procedure TvgListBoxItem.DesignSelect;
begin
  inherited;
  if (ComboBox <> nil) then
  begin
    if ComboBox.FPopup.DesignHide then
      ComboBox.DesignClick;
  end;
end;

procedure TvgListBoxItem.DesignInsert;
begin
  inherited;
  if (ComboBox <> nil) then
  begin
    if ComboBox.FPopup.DesignHide then
      ComboBox.DesignClick;
  end;
end;

procedure TvgListBoxItem.Paint;
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

function TvgListBoxItem.GetParentComponent: TComponent;
begin
  if (ComboBox <> nil) then
    Result := ComboBox
  else
  if (ListBox <> nil) then
    Result := ListBox
  else
    Result := inherited GetParentComponent;
end;

function TvgListBoxItem.EnterFocusChildren(AObject: TvgVisualObject): boolean;
begin
  Result := inherited EnterFocusChildren(AObject);
  if (ListBox <> nil) then
  begin
    if ListBox.MultiSelect then
      ListBox.ClearSelection;
    ListBox.ItemIndex := Index;
    Result := true;
  end;
end;

procedure TvgListBoxItem.UpdateCheck;
var
  i: integer;
begin
  if (ListBox <> nil) and (FCheck <> nil) then
    FCheck.Visible := ListBox.ShowCheckboxes;
  if ChildrenCount > 0 then
    for i := 0 to ChildrenCount - 1 do
      if Children[i] is TvgListboxItem then
        TvgListboxItem(Children[i]).UpdateCheck;
end;

procedure TvgListBoxItem.SetIsChecked(const Value: boolean);
begin
  if FIsChecked <> Value then
  begin
    FIsChecked := Value;
    if FCheck <> nil then
      FCheck.IsChecked := FIsChecked;
  end;
end;

procedure TvgListBoxItem.SetIsSelected(const Value: boolean);
begin
  if FIsSelected <> Value then
  begin
    FIsSelected := Value;
    StartTriggerAnimation(Self, 'IsSelected');
    if FIsSelected and (ListBox <> nil) and not (Listbox.MultiSelect) then
      Listbox.ItemIndex := Index
    else
    if not FIsSelected and (ListBox <> nil) and not (Listbox.MultiSelect) and (Listbox.ItemIndex = Index) then
      Listbox.ItemIndex := -1
    else
      if Listbox <> nil then Listbox.UpdateSelection;
  end;
end;

procedure TvgListBoxItem.DragEnd;
begin
  inherited;
  DragLeave;
  if (ListBox <> nil) then
    ListBox.FDragItem := nil;
end;

{ TvgListBox ==================================================================}

constructor TvgListBox.Create(AOwner: TComponent);
begin
  inherited;
  FOddFill := TvgBrush.Create(vgBrushSolid, $20000000);
  FColumns := 1;
  FHideSelectionUnfocused := true;
  FContent.DisableDefaultAlign := true;
  FItemIndex := -1;
  CanFocused := true;
  AutoCapture := true;
  Width := 100;
  Height := 100;
end;

destructor TvgListBox.Destroy;
begin
  if FSelections <> nil then
    FSelections.Free;
  FOddFill.Free;
  inherited;
end;

function TvgListBox.ItemClass: string;
begin
  Result := 'TvgListBoxItem';
end;

procedure TvgListBox.HScrollChange(Sender: TObject);
begin
  inherited;
  UpdateSelection;
end;

procedure TvgListBox.VScrollChange(Sender: TObject);
begin
  inherited;
  UpdateSelection;
end;

function CompareListItem(item1, item2: TvgObject): integer;
begin
  if (item1 is TvgListBoxItem) and (item2 is TvgListBoxItem) then
  begin
    if (TvgListBoxItem(item1).ListBox <> nil) and Assigned(TvgListBoxItem(item1).ListBox.OnCompare) then
      Result := TvgListBoxItem(item1).ListBox.OnCompare(TvgListBoxItem(item1), TvgListBoxItem(item2))
    else
      {$IFDEF KS_COMPILER5}
      Result := CompareText(TvgListBoxItem(item1).Text, TvgListBoxItem(item2).Text);
      {$ELSE}
      Result := WideCompareText(TvgListBoxItem(item1).Text, TvgListBoxItem(item2).Text);
      {$ENDIF}
  end
  else
    Result := 0;
end;

procedure TvgListBox.SortItems;
begin
  if not FSorted then Exit;
  FContent.Sort(CompareListItem);
end;

procedure TvgListBox.DoContentPaint(Sender: TObject; const Canvas: TvgCanvas; const ARect: TvgRect);
var
  i: integer;
  Item: TvgListBoxItem;
  P: TvgPoint;
  R: TvgRect;
begin
  if (FContent <> nil) and (ContentLayout <> nil) then
  begin
    if FAlternatingRowBackground then
    begin
      Canvas.Fill.Assign(FOddFill);
      for i := 0 to Count div Columns do
      begin
        if Odd(i) then
        begin
          if i * Columns > Count - 1 then
            Item := ItemByIndex(Count - 1)
          else
            Item := ItemByIndex(i * Columns);
          P := Item.LocalToAbsolute(vgPoint(0, 0));
          P := TvgVisualObject(Sender).AbsoluteToLocal(P);
          R := vgRect(P.X, P.Y, P.X + ContentLayout.Width, P.Y + Item.Height);
          if not vgIntersectRect(R, ARect) then Continue;
          Canvas.FillRect(R, 0, 0, [], AbsoluteOpacity);
        end;
      end;
    end;
  end;
end;

procedure TvgListBox.ApplyStyle;
var
  T: TvgObject;
begin
  inherited;
  T := FindResource('content');
  if (T <> nil) and (T.IsVisual) then
  begin
    TvgVisualObject(T).OnBeforePaint := DoContentPaint;
  end;
  T := FindResource('selection');
  if (T <> nil) and (T is TvgVisualObject) then
  begin
    FSelection := TvgVisualObject(T);
    FSelection.Visible := false;
    UpdateSelection;
  end;
  T := FindResource('AlternatingRowBackground');
  if (T <> nil) and (T is TvgBrushObject) then
  begin
    FOddFill.Assign(TvgBrushObject(T).Brush);
  end;
  if (T <> nil) and (T is TvgVisualObject) then
  begin
    TvgVisualObject(T).Visible := false;
  end;
end;

procedure TvgListBox.FreeStyle;
begin
  inherited;
  FSelection := nil;
  if FSelections <> nil then
    FSelections.Clear;
end;

procedure TvgListBox.UpdateSelection;
var
  i: integer;
  P: TvgPoint;
  R: TvgRect;
  Sel: boolean;
  SelRects: array of TvgRect;
  Clone: TvgVisualObject;
  Vis: boolean;
  Item: TvgListboxItem;
begin
  if FSelection = nil then Exit;
  // calc rects
  Vis := true;
  Sel := false;
  SetLength(SelRects, 0);
  for i := 0 to Count - 1 do
  begin
    Item := Items[i];
    if (Item.IsSelected) and vgIntersectRect(Item.UpdateRect, UpdateRect) then
    begin
      P := Item.LocalToAbsolute(vgPoint(0, 0));
      if (FSelection.Parent <> nil) and FSelection.Parent.IsVisual then
        P := FSelection.Parent.Visual.AbsoluteToLocal(P);
      R := vgRect(P.X, P.Y, P.X + Item.Width, P.Y + Item.Height);
      if (Length(SelRects) > 0) and (i > 0) and (Items[i - 1].IsSelected) then
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
  // Check visible
  if HideSelectionUnfocused and not IsFocused then
    Vis := false;
  // align selections
  for i := 0 to High(SelRects) do
  begin
    TvgVisualObject(FSelections[i]).Visible := Vis;
    if Vis then
    begin
      with SelRects[i] do
        TvgVisualObject(FSelections[i]).SetBounds(Left, Top, Right - Left, Bottom - Top);
    end;
  end;
end;

procedure TvgListBox.ContentAddObject(AObject: TvgObject);
begin
  inherited;
  if AObject is TvgListBoxItem then
    if FUpdating = 0 then
      Realign;
end;

procedure TvgListBox.ContentBeforeRemoveObject(AObject: TvgObject);
begin
  inherited;
  if AObject is TvgListBoxItem then
  begin
    if FItemIndex > TvgListBoxItem(AObject).Index then
    begin
      Dec(FItemIndex);
      UpdateSelection;
    end;
    TvgListBoxItem(AObject).IsSelected := false;
  end;
end;

procedure TvgListBox.ContentRemoveObject(AObject: TvgObject);
begin
  inherited;
  if AObject is TvgListBoxItem then
  begin
    if FUpdating = 0 then
      Realign;
  end;
end;

function TvgListBox.GetContentBounds: TvgRect;
var
  R: TvgRect;
  i, j, Idx: integer;
  RowHeight, ColWidth, CurY: single;
begin
  Result := LocalRect;
  if FUpdating > 0 then Exit;
  if ContentLayout = nil then Exit;
  R := ContentLayout.LocalRect;
  { FContent }
  if FContent <> nil then
  begin
    { Sort if need }
    SortItems;
    { Set Selection }
    if not MultiSelect and (Selected <> nil) then
      Selected.IsSelected := true;
    { Align }
    case FListStyle of
      vgListVertical:
        begin
          { correct items size }
          if FItemWidth <> 0 then
          begin
            FColumns := trunc((R.Right - R.Left) / FItemWidth);
            if FColumns < 1 then FColumns := 1;
            if FContent.ChildrenCount > 0 then
              for i := 0 to (FContent.ChildrenCount - 1) do
                with TvgListBoxItem(FContent.Children[i]) do
                begin
                  if FItemHeight <> 0 then
                    SetBounds(Position.X, Position.Y, FItemWidth, FItemHeight)
                  else
                    SetBounds(Position.X, Position.Y, FItemWidth, Height);
                end;
          end;
          if (FItemWidth = 0) and (FItemHeight <> 0) then
          begin
            if FContent.ChildrenCount > 0 then
              for i := 0 to (FContent.ChildrenCount - 1) do
                if FContent.Children[i] is TvgListBoxItem then
                  with TvgListBoxItem(FContent.Children[i]) do
                  begin
                    SetBounds(Position.X, Position.Y, Width, FItemHeight)
                  end;
          end;
          { calc items size }
          CurY := 0;
          if FContent.ChildrenCount > 0 then
            for i := 0 to (FContent.ChildrenCount - 1) div FColumns do
            begin
              RowHeight := 0;
              for j := 0 to FColumns - 1 do
              begin
                if (i * FColumns) + j > FContent.ChildrenCount - 1 then Continue;
                if FContent.Children[(i * FColumns) + j] is TvgListBoxItem then
                  with TvgListBoxItem(FContent.Children[(i * FColumns) + j]) do
                  begin
                    if Height + Padding.Top + Padding.Bottom > RowHeight then
                      RowHeight := Height + Padding.Top + Padding.Bottom;
                  end;
              end;
              // set correct height
              for j := 0 to FColumns - 1 do
              begin
                if (i * FColumns) + j > FContent.ChildrenCount - 1 then Continue;
                if FContent.Children[(i * FColumns) + j] is TvgListBoxItem then
                  with TvgListBoxItem(FContent.Children[(i * FColumns) + j]) do
                  begin
                    Height := RowHeight - Padding.Top - Padding.Bottom;
                  end;
              end;
              CurY := CurY + RowHeight;
            end;
          FContent.Height := CurY;
          { align }
          CurY := 0;
          Idx := 0;
          if FContent.ChildrenCount > 0 then
            for i := 0 to (FContent.ChildrenCount - 1) div FColumns do
            begin
              RowHeight := 0;
              for j := 0 to FColumns - 1 do
              begin
                if (i * FColumns) + j > FContent.ChildrenCount - 1 then Continue;

                if FItemWidth <> 0 then
                  ColWidth := FItemWidth
                else
                  ColWidth := (R.Right - R.Left) / FColumns;

                if FContent.Children[(i * FColumns) + j] is TvgListBoxItem then
                  with TvgListBoxItem(FContent.Children[(i * FColumns) + j]) do
                  begin
                    SetBounds(Padding.Left + (j * ColWidth), CurY + Padding.Top,
                      ColWidth - Padding.Left - Padding.Right, Height);

                    if Height + Padding.Top + Padding.Bottom > RowHeight then
                      RowHeight := Height + Padding.Top + Padding.Bottom;
                    Inc(Idx);
                  end;
              end;
              CurY := CurY + RowHeight;
            end;
          if CurY > 0 then
            R.Bottom := R.Top + CurY;
          if FItemWidth <> 0 then
            R.Right := R.Left + (FItemWidth * FColumns);
      end;
      vgListHorizontal:
        begin
          { correct items size }
          if FItemHeight <> 0 then
          begin
            FColumns := trunc((R.Bottom - R.Top - Padding.Top - Padding.Bottom) / FItemHeight);
            if FColumns < 1 then FColumns := 1;
            if FContent.ChildrenCount > 0 then
              for i := 0 to (FContent.ChildrenCount - 1) do
                with TvgListBoxItem(FContent.Children[i]) do
                begin
                  if FItemWidth <> 0 then
                    SetBounds(Position.X, Position.Y, FItemWidth, FItemHeight)
                  else
                    SetBounds(Position.X, Position.Y, Width, FItemHeight);
                end;
          end;
          if (FItemHeight = 0) and (FItemWidth <> 0) then
          begin
            if FContent.ChildrenCount > 0 then
              for i := 0 to (FContent.ChildrenCount - 1) do
                with TvgListBoxItem(FContent.Children[i]) do
                begin
                  SetBounds(Position.X, Position.Y, FItemWidth, Height)
                end;
          end;
          { calc items size }
          CurY := 0;
          if FContent.ChildrenCount > 0 then
            for i := 0 to (FContent.ChildrenCount - 1) div FColumns do
            begin
              ColWidth := 0;
              if FItemHeight <> 0 then
                RowHeight := FItemHeight
              else
                RowHeight := (R.Bottom - R.Top) / FColumns;
              for j := 0 to FColumns - 1 do
                if FContent.Children[(i * FColumns) + j] is TvgListBoxItem then
                  with TvgListBoxItem(FContent.Children[(i * FColumns) + j]) do
                  begin
                    if ColWidth < Width + Padding.Left + Padding.Right then
                      ColWidth := Width + Padding.Left + Padding.Right;
                  end;
              // calc width
              for j := 0 to FColumns - 1 do
                if FContent.Children[(i * FColumns) + j] is TvgListBoxItem then
                  with TvgListBoxItem(FContent.Children[(i * FColumns) + j]) do
                  begin
                    Width := ColWidth - (Padding.Left + Padding.Right);
                  end;

              CurY := CurY + ColWidth;
            end;
          { selection }
          if FItemIndex > Count - 1 then
            FItemIndex := Count - 1;
          { align }
          CurY := 0;
          Idx := 0;
          if FContent.ChildrenCount > 0 then
            for i := 0 to (FContent.ChildrenCount - 1) div FColumns do
            begin
              ColWidth := 0;
              if FItemHeight <> 0 then
                RowHeight := FItemHeight
              else
                RowHeight := (R.Bottom - R.Top) / FColumns;
              for j := 0 to FColumns - 1 do
                if FContent.Children[(i * FColumns) + j] is TvgListBoxItem then
                  with TvgListBoxItem(FContent.Children[(i * FColumns) + j]) do
                  begin
                    if VScrollBar <> nil then
                      SetBounds(CurY + Padding.Left - VScrollBar.Value, Padding.Top + (j * RowHeight),
                        Width, RowHeight - Padding.Top - Padding.Bottom)
                    else
                      SetBounds(CurY + Padding.Left, Padding.Top + (j * RowHeight),
                        Width, RowHeight - Padding.Top - Padding.Bottom);
                    if ColWidth < Width + Padding.Left + Padding.Right then
                      ColWidth := Width + Padding.Left + Padding.Right;
                    Inc(Idx);
                  end;
              CurY := CurY + ColWidth;
            end;
          if CurY > 0 then
            R.Right := R.Left + CurY;
          if FItemHeight <> 0 then
            R.Bottom := R.Top + (FItemHeight * FColumns);
        end;
    end;
  end;
  UpdateSelection;
  Result := R;
end;

function TvgListBox.GetCount: integer;
begin
  Result := 0;
  if (FContent <> nil) then
    Result := FContent.ChildrenCount;
end;

function TvgListBox.ItemByIndex(const Idx: integer): TvgListBoxItem;
begin
  if (FContent <> nil) and (FContent.ChildrenCount > 0) and (Idx >= 0) and (Idx < FContent.ChildrenCount) then
    Result := TvgListBoxItem(FContent.Children[Idx])
  else
    Result := nil;
end;

function TvgListBox.ItemByPoint(const X, Y: single): TvgListBoxItem;
var
  i: integer;
  P: TvgPoint;
begin
  P := LocaltoAbsolute(vgPoint(X, Y));
  if (FContent <> nil) and (FContent.ChildrenCount > 0) then
    for i := 0 to FContent.ChildrenCount - 1 do
      if FContent.Children[i] is TvgListBoxItem then
        if TvgListBoxItem(FContent.Children[i]).pointInObject(P.X, P.Y) then
        begin
          Result := TvgListBoxItem(FContent.Children[i]);
          Exit;
        end;
  Result := nil;
end;

procedure TvgListBox.KeyDown(var Key: Word; var KeyChar: System.WideChar;
  Shift: TShiftState);
var
  i: integer;
begin
  inherited ;
  if Count > 0 then
  begin
    if KeyChar <> #0 then
    begin
      for i := 0 to Count - 1 do
        if (Items[i].Text <> '') and (WideLowerCase(Items[i].Text[1]) = WideLowerCase(KeyChar)) then
        begin
          ItemIndex := i;
          Break;
        end;
      KeyChar := #0;
    end;
    case Key of
      VK_HOME: ItemIndex := 0;
      VK_END: ItemIndex := Count - FColumns;
      VK_UP: If ItemIndex > 0 then
        begin
          ItemIndex := ItemIndex - FColumns;
          if ItemIndex < 0 then ItemIndex := 0;
        end;
      VK_DOWN:
        begin
          If ItemIndex < Count - 1 then ItemIndex := ItemIndex + FColumns;
          if ItemIndex > Count - 1 then ItemIndex := Count - 1;
        end;
      VK_LEFT: If ItemIndex > 0 then ItemIndex := ItemIndex - 1;
      VK_RIGHT: If ItemIndex < Count - 1 then ItemIndex := ItemIndex + 1;
    else
      Exit;
    end;
    Key := 0;
  end;
end;

procedure TvgListBox.KeyUp(var Key: Word; var KeyChar: System.WideChar;
  Shift: TShiftState);
begin
  inherited ;
end;

procedure TvgListBox.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: single);
var
  Item: TvgListBoxItem;
begin
  inherited;
  if (Button = mbLeft) and not MouseTracking then
  begin
    Item := ItemByPoint(X, Y);
    if Item <> nil then
    begin
      if Multiselect then
      begin
        if ssCtrl in Shift then
          Item.IsSelected := not Item.IsSelected
        else
        if ssShift in Shift then
        begin
          SelectRange(Selected, Item);
          ItemIndex := Item.Index;
        end
        else
        begin
          SelectRange(Item, Item);
          ItemIndex := Item.Index;
        end;
        FFirstSelect := Item;
      end
      else
      begin
        if ItemIndex <> Item.Index then
          ItemIndex := Item.Index
        else
          if AllowDrag then
            Scene.BeginVCLDrag(Selected, MakeScreenshot);
      end;
      if Assigned(Item.OnClick) then
        Item.OnClick(Item)
    end;
    FMouseSelecting := true;
  end;
end;

procedure TvgListBox.MouseMove(Shift: TShiftState; X, Y, Dx, Dy: single);
var
  Item: TvgListBoxItem;
begin
  inherited;
  if (ssLeft in Shift) and FMouseSelecting then
  begin
    Item := ItemByPoint(X, Y);
    if Item <> nil then
    begin
      if Selected = Item then Exit;
      if MultiSelect then
      begin
        if ssCtrl in Shift then
          Item.IsSelected := not Item.IsSelected
        else
          SelectRange(FFirstSelect, Item);
        ItemIndex := Item.Index;
      end
      else
        ItemIndex := Item.Index;
    end;
  end;
end;

procedure TvgListBox.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: single);
var
  Item: TvgListBoxItem;
begin
  inherited;
  FFirstSelect := nil;
  if MouseTracking and (FLastDelta.X = 0) and (FLastDelta.Y = 0) then
  begin
    Item := ItemByPoint(X, Y);
    if Item <> nil then
      ItemIndex := Item.Index;
  end;
  FMouseSelecting := false;
end;

function TvgListBox.GetSelected: TvgListBoxItem;
begin
  Result := ItemByIndex(FItemIndex);
end;

procedure TvgListBox.SetItemIndex(const Value: integer);
var
  Item: TvgListBoxItem;
begin
  if FItemIndex <> Value then
  begin
    Item := ItemByIndex(ItemIndex);
    if (Item <> nil) and (not MultiSelect) then
      Item.IsSelected := false;

    FItemIndex := Value;
    if (ItemByIndex(FItemIndex) <> nil) and (FContent <> nil) and (VScrollBar <> nil) and (ContentLayout <> nil) then
    begin
      Item := ItemByIndex(FItemIndex);
      if FContent.Position.Y + Item.Position.Y + Item.Padding.Top + Item.Padding.Bottom + Item.Height > ContentLayout.Position.Y + ContentLayout.Height then
       VScrollBar.Value := VScrollBar.Value + (FContent.Position.Y + Item.Position.Y + Item.Padding.Top + Item.Padding.Bottom + Item.Height - ContentLayout.Position.Y - ContentLayout.Height);
      if FContent.Position.Y + Item.Position.Y < ContentLayout.Position.Y then
        VScrollBar.Value := VScrollBar.Value + FContent.Position.Y + Item.Position.Y - ContentLayout.Position.Y;
    end;
    if (ItemByIndex(FItemIndex) <> nil) and (FContent <> nil) and (HScrollBar <> nil) and (ContentLayout <> nil) then
    begin
      Item := ItemByIndex(FItemIndex);
      if FContent.Position.X + Item.Position.X + Item.Padding.Left + Item.Padding.Right + Item.Width > ContentLayout.Position.X + ContentLayout.Width then
        HScrollBar.Value := HScrollBar.Value + (FContent.Position.X + Item.Position.X + Item.Padding.Left + Item.Padding.Right + Item.Width - ContentLayout.Position.X - ContentLayout.Width);
      if FContent.Position.X + Item.Position.X < 0 then
        HScrollBar.Value := HScrollBar.Value + FContent.Position.X + Item.Position.X - ContentLayout.Position.X;
    end;
    if (ItemIndex >= 0) then
    begin
      Item := ItemByIndex(ItemIndex);
      if Item <> nil then
        Item.IsSelected := true;
    end;
    if (FUpdating = 0) then
    begin
      if Assigned(FBindingObjects) then
        ToBindingObjects;
      if Assigned(FOnChange) then
        FOnChange(ItemByIndex(FItemIndex));
    end;
    UpdateSelection;
  end;
end;

procedure TvgListBox.Clear;
var
  i: integer;
begin
  BeginUpdate;
  if FContent <> nil then
    if FContent.ChildrenCount > 0 then
      for i := FContent.ChildrenCount - 1 downto 0 do
        if FContent.Children[i] is TvgListboxItem then
          TvgObject(FContent.Children[i]).Free;
  FScrollDesign.Y := 0;
  FScrollDesign.X := 0;
  EndUpdate;
end;

procedure TvgListBox.SelectRange(Item1, Item2: TvgListBoxItem);
var
  i: integer;
begin
  if Item1 = nil then Exit;
  if Item2 = nil then Exit;
  for i := 0 to Min(Item1.Index, Item2.Index) - 1 do
    ItemByIndex(i).IsSelected := false;
  for i := Max(Item1.Index, Item2.Index) + 1 to Count - 1 do
    ItemByIndex(i).IsSelected := false;
  for i := Min(Item1.Index, Item2.Index) to Max(Item1.Index, Item2.Index) do
    ItemByIndex(i).IsSelected := true;
end;

procedure TvgListBox.ClearSelection;
var
  i: integer;
begin
  for i := 0 to Count - 1 do
    ItemByIndex(i).IsSelected := false;
end;

procedure TvgListBox.SelectAll;
var
  i: integer;
begin
  for i := 0 to Count - 1 do
    ItemByIndex(i).IsSelected := true;
end;

function TvgListBox.GetData: Variant;
begin
  if Selected <> nil then
    Result := Selected.Text
  else
    Result := '';
end;

procedure TvgListBox.SetData(const Value: Variant);
begin
  if Selected <> nil then
    Selected.Text := Value;
end;

procedure TvgListBox.EnterFocus;
begin
  inherited ;
  if HideSelectionUnfocused and (Selected <> nil) then
    UpdateSelection;
end;

procedure TvgListBox.KillFocus;
begin
  inherited;
  if HideSelectionUnfocused and (Selected <> nil) then
    UpdateSelection;
end;

procedure TvgListBox.AddObject(AObject: TvgObject);
begin
  if (FContent <> nil) and ((AObject is TvgListBoxItem) or (AObject is TvgSelectionItem)) then
  begin
    FContent.AddObject(AObject);
  end
  else
    inherited;
end;

procedure TvgListBox.SetColumns(const Value: integer);
begin
  if FColumns <> Value then
  begin
    FColumns := Value;
    if FColumns < 1 then
      FColumns := 1;
    Realign;
  end;
end;

procedure TvgListBox.SetAlternatingRowBackground(const Value: boolean);
begin
  if FAlternatingRowBackground <> Value then
  begin
    FAlternatingRowBackground := Value;
    Repaint;
  end;
end;

procedure TvgListBox.SetMultiSelect(const Value: boolean);
begin
  if FMultiSelect <> Value then
  begin
    FMultiSelect := Value;
    if not FMultiSelect then
      ClearSelection;
  end;
end;

procedure TvgListBox.SetItemHeight(const Value: single);
begin
  if FItemHeight <> Value then
  begin
    FItemHeight := Value;
    Realign;
  end;
end;

procedure TvgListBox.SetItemWidth(const Value: single);
begin
  if FItemWidth <> Value then
  begin
    FItemWidth := Value;
    Realign;
  end;
end;

procedure TvgListBox.SetListStyle(const Value: TvgListStyle);
begin
  if FListStyle <> Value then
  begin
    FListStyle := Value;
    Realign;
  end;
end;

procedure TvgListBox.SetShowCheckboxes(const Value: boolean);
var
  i: integer;
begin
  if FShowCheckboxes <> Value then
  begin
    FShowCheckboxes := Value;
    for i := 0 to Count - 1 do
      if ItemByIndex(i) <> nil then
        ItemByIndex(i).UpdateCheck;
  end;
end;

function TvgListBox.GetItem(Index: integer): TvgListBoxItem;
begin
  Result := ItemByIndex(Index);
end;

procedure TvgListBox.SetSorted(const Value: boolean);
begin
  if FSorted <> Value then
  begin
    FSorted := Value;
    Realign;
  end;
end;

procedure TvgListBox.SetAllowDrag(const Value: boolean);
begin
  if FAllowDrag <> Value then
  begin
    FAllowDrag := Value;
    if FAllowDrag then
      DragDisableHighlight := true;
  end;
end;

{ TvgComboListBox }

constructor TvgComboListBox.Create(AOwner: TComponent);
begin
  inherited;
  HideSelectionUnfocused := false;
end;

destructor TvgComboListBox.Destroy;
begin
  inherited;
end;

procedure TvgComboListBox.ApplyResource;
begin
  inherited;
end;

procedure TvgComboListBox.GetChildren(Proc: TGetChildProc;
  Root: TComponent);
begin
  inherited ;
end;

function TvgComboListBox.GetParentComponent: TComponent;
begin
  Result := inherited GetParentComponent;
end;

procedure TvgComboListBox.KillFocus;
begin
  inherited;
end;

procedure TvgComboListBox.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: single);
begin
  inherited;
end;

procedure TvgComboListBox.MouseMove(Shift: TShiftState; X, Y, Dx,
  Dy: single);
begin
  inherited;
end;

procedure TvgComboListBox.KeyDown(var Key: Word; var KeyChar: System.WideChar;
  Shift: TShiftState);
begin
  inherited;
  if Key = VK_RETURN then
  begin
    if (Parent is TvgPopup) and (TvgPopup(Parent).IsOpen) and (FComboBox <> nil) and (FComboBox is TvgComboBox) then
    begin
      TvgComboBox(FComboBox).ItemIndex := ItemIndex;
      TvgPopup(Parent).IsOpen := false;
    end;
    if (Parent is TvgPopup) and (TvgPopup(Parent).IsOpen) and (FComboBox <> nil) and (FComboBox is TvgComboTextBox) then
    begin
      TvgComboTextBox(FComboBox).ItemIndex := ItemIndex;
      TvgPopup(Parent).IsOpen := false;
    end;
  end;
end;

procedure TvgComboListBox.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: single);
begin
  inherited;
  if (Parent is TvgPopup) and (TvgPopup(Parent).IsOpen) and (FComboBox <> nil) and (FComboBox is TvgComboBox) then
  begin
    if vgPtInRect(vgPoint(X, Y), LocalRect) then
      if ItemByPoint(X, Y) <> nil then
        TvgComboBox(FComboBox).ItemIndex := ItemByPoint(X, Y).Index;
    TvgPopup(Parent).IsOpen := false;
  end;
  if (Parent is TvgPopup) and (TvgPopup(Parent).IsOpen) and (FComboBox <> nil) and (FComboBox is TvgComboTextBox) then
  begin
    if vgPtInRect(vgPoint(X, Y), LocalRect) then
      if ItemByPoint(X, Y) <> nil then
        TvgComboTextBox(FComboBox).ItemIndex := ItemByPoint(X, Y).Index;
    TvgPopup(Parent).IsOpen := false;
  end;
end;

procedure TvgComboListBox.MouseWheel(Shift: TShiftState;
  WheelDelta: integer; var Handled: boolean);
begin
  inherited;
end;

{ TvgComboBox =================================================================}

constructor TvgComboBox.Create(AOwner: TComponent);
begin
  inherited;
  DropDownCount := 8;
  CanFocused := true;
  FPopup := TvgPopup.Create(Self);
  FPopup.PlacementTarget := Self;
  FPopup.StaysOpen := false;
  FPopup.Stored := false;
  FPopup.Parent := Self;
  FPopup.Locked := true;
  FPopup.DesignHide := true;
  FListBox := TvgComboListBox.Create(Self);
  FListBox.Parent := FPopup;
  FListBox.FComboBox := Self;
  FListBox.Stored := false;
  FListBox.Align := vaClient;
  FListBox.ShowCheckboxes := false;
  Width := 100;
  Height := 22;
end;

destructor TvgComboBox.Destroy;
begin
  inherited;
end;

function TvgComboBox.ItemClass: string;
begin
  Result := 'TvgListBoxItem';
end;

procedure TvgComboBox.ApplyStyle;
var
  T: TvgObject;
begin
  inherited;
  T := FindResource('Content');
  if (T <> nil) and (T is TvgContent) then
  begin
    TvgContent(T).OnPaint := DoContentPaint;
    if (FListBox <> nil) and (FListBox.ItemByIndex(ItemIndex) <> nil) then
    begin
      FListBox.ItemByIndex(ItemIndex).Width := TvgContent(T).Width;
    end;
  end;
end;

procedure TvgComboBox.Realign;
begin
  inherited;
  if FDisableAlign then Exit;
  FDisableAlign := true;
  { FContent }
  if FPopup <> nil then
    FPopup.Width := Width;
  FDisableAlign := false;
end;

procedure TvgComboBox.Paint;
begin
  inherited ;
end;

procedure TvgComboBox.PaintChildren;
begin
  inherited ;
end;

procedure TvgComboBox.DoContentPaint(Sender: TObject;
  const Canvas: TvgCanvas; const ARect: TvgRect);
var
  SOpacity: single;
  Item: TvgListBoxItem;
  SaveSize: TvgPoint;
  SaveScene: IvgScene;
begin
  if FListBox <> nil then
  begin
    Item := FListBox.ItemByIndex(FListBox.ItemIndex);
    if Item <> nil then
    begin
      Item.ApplyResource;
      SOpacity := Item.FAbsoluteOpacity;
      SaveSize := vgPoint(Item.Width, Item.Height);
      SaveScene := Item.Scene;
      Item.SetNewScene(Scene);
      Item.SetSizeWithoutChange(vgRectWidth(ARect), vgRectHeight(ARect));
      Item.FAbsoluteOpacity := Opacity;
      Item.RecalcOpacity;
      Item.FRecalcOpacity := false;
      Item.PaintTo(Canvas, ARect, TvgObject(Sender));
      Item.FAbsoluteOpacity := SOpacity;
      Item.RecalcOpacity;
      Item.SetSizeWithoutChange(SaveSize.X, SaveSize.Y);
      Item.SetNewScene(SaveScene);
    end;
  end;
end;

procedure TvgComboBox.DropDown;
var
  Count, i: integer;
begin
  if not FPopup.IsOpen then
  begin
    FPopup.Placement := FPlacement;
    FPopup.Width := Width;
    Count := DropDownCount;
    if FListBox.Count < Count then
    Count := FListBox.Count;
    if FListbox.ItemHeight > 0 then
      FPopup.Height := (Count * FListbox.ItemHeight) + 4
    else
      FPopup.Height := Count * (Height - 4);
    FPopup.IsOpen := true;
    if FPopup.IsOpen then
      FListBox.SetFocus;
  end
  else
  begin
    FPopup.IsOpen := false;
  end;
end;

procedure TvgComboBox.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: single);
begin
  inherited;
  if Button = mbLeft then
  begin
    DropDown;
  end;
end;

procedure TvgComboBox.MouseMove(Shift: TShiftState; X, Y, Dx, Dy: single);
begin
  inherited;
end;

procedure TvgComboBox.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: single);
begin
  inherited;
end;

procedure TvgComboBox.MouseWheel(Shift: TShiftState; WheelDelta: integer;
  var Handled: boolean);
begin
  inherited;
  if WheelDelta < 0 then
  begin
    if ItemIndex < Count - 1 then
      ItemIndex := ItemIndex + 1
  end
  else
  begin
    if ItemIndex > 0 then
      ItemIndex := ItemIndex - 1;
  end;
  Handled := true;             
end;

procedure TvgComboBox.KeyDown(var Key: Word; var KeyChar: System.WideChar;
  Shift: TShiftState);
var
  i: integer;
begin
  inherited ;
  if Count > 0 then
  begin
    if KeyChar <> #0 then
    begin
      for i := 0 to Count - 1 do
        if (FListBox.Items[i].Text <> '') and (WideLowerCase(FListBox.Items[i].Text[1]) = WideLowerCase(KeyChar)) then
        begin
          ItemIndex := i;
          Break;
        end;
      KeyChar := #0;
    end;
    case Key of
      VK_HOME: ItemIndex := 0;
      VK_END: ItemIndex := Count - 1;
      VK_UP: If ItemIndex > 0 then
        begin
          ItemIndex := ItemIndex - 1;
          if ItemIndex < 0 then ItemIndex := 0;
        end;
      VK_DOWN:
        begin
          if ssAlt in Shift then
          begin
            DropDown;
          end
          else
          begin
            If ItemIndex < Count - 1 then ItemIndex := ItemIndex + 1;
            if ItemIndex > Count - 1 then ItemIndex := Count - 1;
          end;
        end;
      VK_LEFT: If ItemIndex > 0 then ItemIndex := ItemIndex - 1;
      VK_RIGHT: If ItemIndex < Count - 1 then ItemIndex := ItemIndex + 1;
    else
      Exit;
    end;
    Key := 0;
  end;
end;

procedure TvgComboBox.Clear;
begin
  if FListBox <> nil then
    FListBox.Clear;
end;

procedure TvgComboBox.AddObject(AObject: TvgObject);
begin
  if (FListBox <> nil) and ((AObject is TvgListBoxItem) or (AObject is TvgSelectionItem)) then
  begin
    FListBox.AddObject(AObject);
  end
  else
    inherited;
end;

procedure TvgComboBox.DoListBoxChange(Sender: TObject);
begin
end;

function TvgComboBox.GetItemIndex: integer;
begin
  if FListBox <> nil then
    Result := FListBox.ItemIndex
  else
    Result := -1;
end;

function TvgComboBox.GetCount: integer;
begin
  if FListBox <> nil then
    Result := FListBox.Count
  else
    Result := 0;
end;

procedure TvgComboBox.SetItemIndex(const Value: integer);
begin
  if FListBox <> nil then
  begin
    FListBox.ItemIndex := Value;
    if Assigned(FBindingObjects) then
      ToBindingObjects;
    if Assigned(FOnChange) then
      FOnChange(Self);
    if (FResourceLink <> nil) then
      FResourceLink.Visual.UpdateEffects;
    Repaint;
  end;
end;

procedure TvgComboBox.DesignClick;
begin
  inherited ;
  FPopup.DesignHide := not FPopup.DesignHide;
  FPopup.Width := Width;
  if FListbox.ItemHeight > 0 then
    FPopup.Height := DropDownCount * FListbox.ItemHeight
  else
    FPopup.Height := DropDownCount * (Height - 4);
  FPopup.Position.X := 0;
  FPopup.Position.Y := Height;
  FPopup.Locked := FPopup.DesignHide;
end;

procedure TvgComboBox.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  j: integer;
begin
  inherited;
  if (FListBox <> nil) and (FListBox.FContent <> nil) then
    if (FListBox.FContent.ChildrenCount > 0) then
    begin
      for j := 0 to FListBox.FContent.ChildrenCount - 1 do
        if FListBox.FContent.Children[j].Stored then
          Proc(TComponent(FListBox.FContent.Children[j]));
    end;
end;

function TvgComboBox.GetListBoxResource: string;
begin
  Result := FListBox.Resource;
end;

procedure TvgComboBox.SetListBoxResource(const Value: string);
begin
  FListBox.Resource := Value;
end;

function TvgComboBox.GetItemHeight: single;
begin
  Result := FListBox.ItemHeight;
end;

procedure TvgComboBox.SetItemHeight(const Value: single);
begin
  FListBox.ItemHeight := Value;
end;

{ TvgStringListBox }

procedure TvgStringListBox.ApplyStyle;
begin
  inherited;
end;

constructor TvgStringListBox.Create(AOwner: TComponent);
begin
  inherited;
  FFont := TvgFont.Create;
  FFont.OnChanged := FontChanged;
  FAutoTranslate := true;
  FItems := TvgWideStringList.Create;
  TvgWideStringList(FItems).OnChange := DoItemsChanged;
  FItemHeight := 19;
  FNeedResource := true;
  FResource := 'listboxstyle';
  FItemStyle := 'listboxitemstyle';
end;

destructor TvgStringListBox.Destroy;
begin
  FreeAndNil(FItems);
  FreeAndNil(FFont);
  inherited;
end;

procedure TvgStringListBox.EndUpdate;
begin
  FUpdating := FUpdating + 1;
  inherited EndUpdate;
  FUpdating := FUpdating - 1;
  if FUpdating = 0 then
    RebuildList;
end;

procedure TvgStringListBox.DoItemsChanged(Sender: TObject);
begin
  RebuildList;
end;

function TvgStringListBox.GetData: Variant;
begin
  if (ItemIndex >= 0) and (ItemIndex < FItems.Count) then
    Result := FItems[ItemIndex]
  else
    Result := '';
end;

procedure TvgStringListBox.SetData(const Value: Variant);
begin
  if (ItemIndex >= 0) and (ItemIndex < FItems.Count) then
    FItems[ItemIndex] := Value;
end;

procedure TvgStringListBox.RebuildList;
var
  i, SaveIndex: integer;
  SaveScrollDesign: TvgPoint;
  Item: TvgListBoxItem;
begin
  if FUpdating > 0 then Exit;
  if csDestroying in ComponentState then Exit;
  if Items = nil then Exit;

  BeginUpdate;
  SaveScrollDesign := FScrollDesign;
  SaveIndex := ItemIndex;
  Clear;
  for i := 0 to FItems.Count - 1 do
  begin
    Item := TvgListBoxItem.Create(Self);
    Item.AutoTranslate := FAutoTranslate;
    Item.Parent := Self;
    Item.Height := FItemHeight;
    Item.Stored := false;
    Item.Locked := true;
    Item.Text := FItems[i];
    Item.Font := FFont;
    Item.FResource := FItemStyle;
    Item.TextAlign := TextAlign;
  end;
  FItemIndex := SaveIndex;
  FScrollDesign := SaveScrollDesign;
  inherited EndUpdate;
end;

procedure TvgStringListBox.SetItems(const Value: TvgWideStrings);
begin
  FItems.Assign(Value);
end;

procedure TvgStringListBox.SetTextAlign(const Value: TvgTextAlign);
begin
  if FTextAlign <> Value then
  begin
    FTextAlign := Value;
    if not (csLoading in ComponentState) then
      RebuildList;
  end;
end;

procedure TvgStringListBox.SetFont(const Value: TvgFont);
begin
  FFont.Assign(Value);
end;

procedure TvgStringListBox.FontChanged(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to Count - 1 do
    inherited Items[i].Font.Assign(FFont);
end;

procedure TvgStringListBox.SortItems;
begin
  TvgWideStringList(FItems).Sort;
end;

{ TvgStringComboBox }

constructor TvgStringComboBox.Create(AOwner: TComponent);
begin
  inherited;
  FAutoTranslate := true;
  FItems := TvgWideStringList.Create;
  TvgWideStringList(FItems).OnChange := DoItemsChanged;
  FItemHeight := 19;
  FNeedResource := true;
  FResource := 'comboboxstyle';
  FItemStyle := 'listboxitemstyle';
end;

destructor TvgStringComboBox.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;

procedure TvgStringComboBox.Clear;
begin
  inherited;
end;

procedure TvgStringComboBox.DoItemsChanged(Sender: TObject);
begin
  RebuildList;
end;

procedure TvgStringComboBox.RebuildList;
var
  SaveI, i: integer;
  Item: TvgListBoxItem;
begin
  if csDestroying in ComponentState then Exit;
  if Items = nil then Exit;
  BeginUpdate;
  SaveI := FListbox.ItemIndex;
  FListbox.FItemIndex := -1;
  Clear;
  for i := 0 to FItems.Count - 1 do
  begin
    Item := TvgListBoxItem.Create(Self);
    Item.AutoTranslate := FAutoTranslate;
    Item.Parent := Self;
    Item.Height := FItemHeight;
    Item.Stored := false;
    Item.Locked := true;
    Item.FResource := FItemStyle;
    Item.Text := FItems[i];
    Item.TextAlign := TextAlign;
  end;
  EndUpdate;
  FListbox.FItemIndex := SaveI;
  if FListbox.FItemIndex >= FListbox.Count then
    FListbox.FItemIndex := FListbox.Count - 1;
end;

procedure TvgStringComboBox.SetItemHeight(const Value: single);
begin
  if FItemHeight <> Value then
  begin
    FItemHeight := Value;
    RebuildList;
  end;
end;

procedure TvgStringComboBox.SetItems(const Value: TvgWideStrings);
begin
  FItems.Assign(Value);
end;

procedure TvgStringComboBox.SetTextAlign(const Value: TvgTextAlign);
begin
  if FTextAlign <> Value then
  begin
    FTextAlign := Value;
    if not (csLoading in ComponentState) then
      RebuildList;
  end;
end;

procedure TvgStringComboBox.ApplyStyle;
begin
  inherited;
end;

function TvgStringComboBox.GetData: Variant;
begin
  if (ItemIndex >= 0) and (ItemIndex < FItems.Count) then
    Result := FItems[ItemIndex]
  else
    Result := '';
end;

procedure TvgStringComboBox.SetData(const Value: Variant);
begin
  if (ItemIndex >= 0) and (ItemIndex < FItems.Count) then
    FItems[ItemIndex] := Value;
end;

{ TvgHorzListBox }

constructor TvgHorzListBox.Create(AOwner: TComponent);
begin
  inherited;
  FResource := 'listboxstyle';
  FNeedResource := true;
  FListStyle := vgListHorizontal;
end;

{ TvgHudStringListBox }

constructor TvgHudStringListBox.Create(AOwner: TComponent);
begin
  inherited;
  FNeedResource := true;
  FResource := 'hudlistboxstyle';
  FItemStyle := 'hudlistboxitemstyle';
end;

{ TvgHudStringComboBox }

constructor TvgHudStringComboBox.Create(AOwner: TComponent);
begin
  inherited;
  FNeedResource := true;
  FResource := 'hudcomboboxstyle';
  FListBox.FResource := 'hudcombolistboxstyle';
  FItemStyle := 'hudlistboxitemstyle';
end;

{ TvgHudListBox }

constructor TvgHudListBox.Create(AOwner: TComponent);
begin
  inherited;
end;

{ TvgHudHorzListBox }

constructor TvgHudHorzListBox.Create(AOwner: TComponent);
begin
  inherited;
  FNeedResource := true;
  FResource := 'hudlistboxstyle';
end;

{ TvgHudComboBox }

constructor TvgHudComboBox.Create(AOwner: TComponent);
begin
  inherited;
  FListBox.FResource := 'hudcombolistboxstyle';
end;

{ TvgImageThread }

constructor TvgImageThread.Create(const AImage: TvgImage; const AFileName: string; const AUseThumbnails: boolean);
begin
  inherited Create(true);
  FFileName := AFileName;
  FUseThumbnails := AUseThumbnails;
  FImage := AImage;
  Priority := tpIdle;
  FreeOnTerminate := true;
end;

destructor TvgImageThread.Destroy;
begin
  inherited;
end;

procedure TvgImageThread.Execute;
begin
  FTempBitmap := TvgBitmap.Create(0, 0);
  if FUseThumbnails then
    FTempBitmap.LoadThumbnailFromFile(FFileName, FImage.Width, FImage.Height, true)
  else
    FTempBitmap.LoadFromFile(FFileName);
  Synchronize(Finished);
end;

procedure TvgImageThread.Finished;
begin
  if FImage <> nil then
    FImage.Bitmap.Assign(FTempBitmap);
  FTempBitmap.Free;
end;

{ TvgImageListBoxItem }

procedure TvgImageListBoxItem.ApplyStyle;
begin
  inherited;
  if (ListBox is TvgImageListBox) then
  begin
    if (TextBorder <> nil) then
      TextBorder.Visible := TvgImageListBox(ListBox).ShowFileName;
    if (Text <> nil) then
      Text.Text := ExtractFileName(TagString);
  end;
  if (ListBox is TvgHorzImageListBox) then
  begin
    if (TextBorder <> nil) then
      TextBorder.Visible := TvgHorzImageListBox(ListBox).ShowFileName;
    if (Text <> nil) then
      Text.Text := ExtractFileName(TagString);
  end;
end;

function TvgImageListBoxItem.Text: TvgText;
begin
  Result := TvgText(FindResource('text'));
end;

function TvgImageListBoxItem.TextBorder: TvgVisualObject;
begin
  Result := TvgVisualObject(FindResource('textborder'));
end;

{ TvgImageListBox }

constructor TvgImageListBox.Create(AOwner: TComponent);
begin
  inherited;
  FUseThumbnails := true;
  FItemHeight := Trunc(Width);
  if Self is TvgHudImageListBox then
    FResource := 'HudListBoxStyle'
  else
    FResource := 'listboxstyle';
end;

procedure TvgImageListBox.AddBitmap(const AFile: string;
  const ABitmap: TvgBitmap);
var
  R: TvgRect;
  Item: TvgListBoxItem;
  Thumb: TvgImage;
begin
  // Create ListboxItem
  Item := TvgImageListBoxItem.Create(Self);
  Item.Parent := Self;
  Item.Height := FItemHeight;
  Item.Stored := false;
  Thumb := TvgImage.Create(Item);
  Thumb.Parent := Item;
  if ShowFileName then
    Thumb.Padding.Rect := vgRect(4, 4, 4, 20)
  else
    Thumb.Padding.Rect := vgRect(4, 4, 4, 4);
  Thumb.Align := vaClient;
  Thumb.Stored := false;
  Thumb.Locked := true;
  Thumb.HitTest := false;
  // create thumb
  if FUseThumbnails then
  begin
    R := vgRect(0, 0, ABitmap.Width, ABitmap.Height);
    vgFitRect(R, vgRect(0, 0, Height, Height));
    Thumb.Bitmap.SetSize(round(vgRectWidth(R)), round(vgRectHeight(R)));
    if Thumb.Bitmap.Canvas.BeginScene then
    begin
      Thumb.Bitmap.Canvas.DrawBitmap(ABitmap, vgRect(0, 0, ABitmap.Width, ABitmap.Height), vgRect(0, 0, Thumb.Bitmap.Width, Thumb.Bitmap.Height), 1);
      Thumb.Bitmap.Canvas.EndScene;
    end;
  end
  else
    Thumb.Bitmap.Assign(ABitmap);
  // Use Tag property as image loading state 1 - unload 0 - loaded
  Item.Tag := 0;
  // Set TagString property to image FileName
  Item.TagString := AFile;
  // Set the Item.TagObject as Thumb for later access
  Item.TagObject := Thumb;
  Realign;
  if Count = 1 then
    ItemIndex := 0;
end;

procedure TvgImageListBox.AddFile(const AFile: string);
var
  Dir, Ext: string;
  SR: TSearchRec;
  Item: TvgListBoxItem;
  Thumb: TvgImage;
begin
  Ext := LowerCase(ExtractFileExt(AFile));
  if Pos(Ext, DefaultFilterClass.GetFileTypes) > 0 then
  begin
    // Create ListboxItem
    Item := TvgImageListBoxItem.Create(Self);
    Item.Parent := Self;
    Item.Height := FItemHeight;
    Item.Stored := false;
    Item.OnApplyResource := DoApplyResource;
    Thumb := TvgImage.Create(Item);
    Thumb.Parent := Item;
    if ShowFileName then
      Thumb.Padding.Rect := vgRect(4, 4, 4, 20)
    else
      Thumb.Padding.Rect := vgRect(4, 4, 4, 4);
    Thumb.Align := vaClient;
    Thumb.Stored := false;
    Thumb.Locked := true;
    Thumb.HitTest := false;
    // Use Tag property as image loading state 1 - unload 0 - loaded
    Item.Tag := 1;
    // Set TagString property to image FileName
    Item.TagString := AFile;
    // Set the Item.TagObject as Thumb for later access
    Item.TagObject := Thumb;
  end;
  if Count = 1 then
    ItemIndex := 0;
end;

procedure TvgImageListBox.AddFolder(const Folder: string);
var
  Dir: string;
  SR: TSearchRec;
begin
  { add folder }
  { add SelectDialog }
  Dir := Folder;
  {$IFDEF UNIX}
  if (Length(Dir) > 0) and (Dir[Length(Dir)] <> '/') then Dir := Dir + '/';
  {$ELSE}
  if (Length(Dir) > 0) and (Dir[Length(Dir)] <> '\') then Dir := Dir + '\';
  {$ENDIF}
  BeginUpdate;
  if FindFirst(Dir + '*.*', $FFFF, SR) = 0 then
  begin
    repeat
      if SR.Name = '.' then Continue;
      if SR.Name = '..' then Continue;
      if SR.Attr and faDirectory = faDirectory then Continue;
      AddFile(Dir + SR.Name);
    until FindNext(SR) <> 0;
    FindClose(SR);
  end;
  EndUpdate;
  if Count > 0 then
    ItemIndex := 0;
end;

procedure TvgImageListBox.DoApplyResource(Sender: TObject);
begin
  if (TvgImageListboxItem(Sender).Tag = 1) and (TvgImageListboxItem(Sender).TagString <> '') then
  begin
    // Create ImageThread
    with TvgImageThread.Create(TvgImage(TvgImageListboxItem(Sender).TagObject), TvgImageListboxItem(Sender).TagString, FUseThumbnails) do
    begin
      // Clear Tag state
      TvgImageListboxItem(Sender).Tag := 0;
      // Resume thread
      Resume;
    end;
  end;
end;

function TvgImageListBox.GetImage(Index: integer): TvgImage;
var
  Item: TvgListBoxItem;
begin
  Item := ItemByIndex(Index);
  if (Item <> nil) and (Item.TagObject <> nil) and (Item.TagObject is TvgImage) then
    Result := TvgImage(Item.TagObject)
  else
    Result := nil;
end;

function TvgImageListBox.GetSelectedFileName: string;
begin
  if (Selected <> nil) then
    Result := Selected.TagString
  else
    Result := '';
end;

function TvgImageListBox.GetSelectedImage: TvgImage;
begin
  if (Selected <> nil) and (Selected.TagObject <> nil) and (Selected.TagObject is TvgImage) then
    Result := TvgImage(Selected.TagObject)
  else
    Result := nil;
end;

procedure TvgImageListBox.SetShowFileName(const Value: boolean);
begin
  if FShowFileName <> Value then
  begin
    FShowFileName := Value;
  end;
end;

procedure TvgImageListBox.SetItemHeight(const Value: single);
var
  i: integer;
begin
  if FItemHeight <> Value then
  begin
    FItemHeight := Value;
    FDisableAlign := true;
    try
      for i := 0 to Count - 1 do
        ItemByIndex(i).Height := ItemHeight;
    finally
      FDisableAlign := false;
      Realign;
    end;
  end;
end;

procedure TvgImageListBox.Clear;
var
  i: integer;
begin
  for i := 0 to Count - 1 do
    ItemByIndex(i).Tag := 0;
  inherited;
end;

procedure TvgImageListBox.BeginAutoDrag;
begin
  if SelectedImage <> nil then
    FScene.BeginVCLDrag(SelectedImage, SelectedImage.Bitmap);
end;

{ TvgHorzImageListBox }

constructor TvgHorzImageListBox.Create(AOwner: TComponent);
begin
  inherited;
  FItemHeight := 0;
  FItemWidth := Trunc(Height + 10);
  FListStyle := vgListHorizontal;
  if Self is TvgHudHorzImageListBox then
    FResource := 'HudListBoxStyle'
  else
    FResource := 'ListBoxStyle';
end;

procedure TvgListBox.DragDrop(const Data: TvgDragObject;
  const Point: TvgPoint);
var
  Obj: TvgListBoxItem;
  Allow: boolean;
begin
  inherited;
  if FDragItem <> nil then
  begin
    FDragItem.DragLeave;
    FDragItem := nil;
  end;
  with AbsoluteToLocal(Point) do
    Obj := ItemByPoint(X, Y);
  if Obj <> nil then
  begin
    Allow := true;
    if Assigned(OnDragChange) then
      OnDragChange(TvgListBoxItem(Data.Source), Obj, Allow);
    if Allow then
    begin
      Exchange(TvgListBoxItem(Data.Source), Obj);
    end;
  end;
end;

procedure TvgListBox.DragOver(const Data: TvgDragObject;
  const Point: TvgPoint; var Accept: Boolean);
var
  Obj: TvgListBoxItem;
begin
  inherited;
  with AbsoluteToLocal(Point) do
    Obj := ItemByPoint(X, Y);
  if (Obj <> FDragItem) then
  begin
    if FDragItem <> nil then
      FDragItem.DragLeave;
    FDragItem := Obj;
    if FDragItem <> nil then
    begin
      FDragItem.DragEnter(Data, Point);
      Accept := true;
    end
    else
      Accept := false;
  end
  else
    Accept := true;

  if FDragItem = Selected then
    Accept := false;
end;

procedure TvgListBox.Exchange(Item1, Item2: TvgListBoxItem);
begin
  if Item1.Index = FItemIndex then
    FItemIndex := Item2.Index
  else
    if Item2.Index = FItemIndex then
      FItemIndex := Item1.Index;
  FContent.Exchange(Item1, Item2);
end;

initialization
  RegisterVGObjects('Lists', [TvgListBox, TvgComboBox, TvgStringListBox, TvgStringComboBox, TvgHorzListBox]);
  RegisterVGObjects('Lists', [TvgImageListBox, TvgHorzImageListBox]);
  RegisterVGObjects('Items', [TvgListBoxItem]);
  RegisterVGObjects('Items', [TvgImageListBoxItem]);
  RegisterVGObjects('HUD', [TvgHudListBox, TvgHudHorzListBox, TvgHudComboBox, TvgHudStringListBox, TvgHudStringComboBox]);
  RegisterVGObjects('HUD', [TvgHudImageListBox, TvgHudHorzImageListBox]);
end.

