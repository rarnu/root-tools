unit vg_treeview;

{$I vg_define.inc}

interface

uses
  {$IFDEF WINDOWS} Windows, {$ENDIF}
  {$IFNDEF NOVCL} Controls, Menus, Clipbrd, {$ENDIF}
  {$IFDEF LCL} LCLType, {$ENDIF}
  Classes, SysUtils, Math,
  vg_scene, vg_objects, vg_layouts, vg_listbox, vg_controls;

type

  TvgTreeView = class;

  TvgTreeViewItem = class(TvgTextControl)
  private
    FIsExpanded: boolean;
    FButton: TvgCustomButton;
    FCheck: TvgCheckBox;
    FGlobalIndex: integer;
    FIsChecked: boolean;
    FIsSelected: boolean;
    procedure SetIsExpanded(const Value: boolean);
    procedure DoButtonClick(Sender: TObject);
    procedure DoCheckClick(Sender: TObject);
    function GetCount: integer;
    procedure SetIsChecked(const Value: boolean);
    procedure UpdateCheck;
    function GetItem(Index: integer): TvgTreeViewItem;
    procedure SetIsSelected(const Value: boolean);
  protected
    procedure DesignClick; override;
    procedure ApplyStyle; override;
    procedure FreeStyle; override;
    procedure Realign; override;
    procedure DragEnd; override;
    function EnterFocusChildren(AObject: TvgVisualObject): boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
    procedure AddObject(AObject: TvgObject); override;
    procedure RemoveObject(AObject: TvgObject); override;
    function ItemClass: string; override;
    function ItemByPoint(const X, Y: single): TvgTreeViewItem;
    function ItemByIndex(const Idx: integer): TvgTreeViewItem;
    property Count: integer read GetCount;
    property GlobalIndex: integer read FGlobalIndex write FGlobalIndex;
    function TreeView: TvgTreeView;
    function Level: integer;
    property Items[Index: integer]: TvgTreeViewItem read GetItem;
  published
    property IsChecked: boolean read FIsChecked write SetIsChecked;
    property IsExpanded: boolean read FIsExpanded write SetIsExpanded;
    property IsSelected: boolean read FIsSelected write SetIsSelected;
    property AutoTranslate default true;
    property Font;
    property Resource;
    property Text;
    property TextAlign default vgTextAlignNear;
  end;

  TOnCompareTreeViewItemEvent = function(Item1, Item2: TvgTreeViewItem): integer of object;
  TOnTreeViewDragChange = procedure (SourceItem, DestItem: TvgTreeViewItem; Allow: boolean) of object;

  TvgTreeView = class(TvgScrollBox)
  private
    FMouseSelecting: boolean;
    FOnChange: TNotifyEvent;
    FSelected: TvgTreeViewItem;
    FItemHeight: single;
    FCountExpanded: integer;
    FHideSelectionUnfocused: boolean;
    FGlobalCount: integer;
    FShowCheckboxes: boolean;
    FOnChangeCheck: TNotifyEvent;
    FSorted: boolean;
    FOnCompare: TOnCompareTreeViewItemEvent;
    FMultiSelect: boolean;
    FFirstSelect: TvgTreeViewItem;
    FSelection: TvgVisualObject;
    FSelections: TList;
    FAllowDrag: boolean;
    FDragItem: TvgTreeViewItem;
    FOnDragChange: TOnTreeViewDragChange;
    FGlobalList: TList;
    procedure SetItemHeight(const Value: single);
    procedure SetShowCheckboxes(const Value: boolean);
    function GetItem(Index: integer): TvgTreeViewItem;
    procedure SetSorted(const Value: boolean);
    procedure SortItems;
    function GetSelection: TvgSelectionItem;
    procedure ClearSelection;
    procedure SelectAll;
    procedure SelectRange(Item1, Item2: TvgTreeViewItem);
    procedure UpdateSelection;
    procedure SetAllowDrag(const Value: boolean);
    function GetCount: integer;
  protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure SetSelected(const Value: TvgTreeViewItem); virtual;
    procedure ApplyStyle; override;
    procedure FreeStyle; override;
    procedure EnterFocus; override;
    procedure KillFocus; override;
    procedure HScrollChange(Sender: TObject); override;
    procedure VScrollChange(Sender: TObject); override;
    function GetContentBounds: TvgRect; override;
    procedure UpdateGlobalIndexes;
    procedure ContentAddObject(AObject: TvgObject); override;
    procedure ContentRemoveObject(AObject: TvgObject); override;
    function GetItemRect(Item: TvgTreeViewItem): TvgRect;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ItemClass: string; override;
    procedure EndUpdate; override;
    procedure Clear;
    procedure ExpandAll;
    procedure CollapseAll;
    function ItemByPoint(const X, Y: single): TvgTreeViewItem;
    function ItemByIndex(const Idx: integer): TvgTreeViewItem;
    function ItemByGlobalIndex(const Idx: integer): TvgTreeViewItem;
    procedure AddObject(AObject: TvgObject); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: single); override;
    procedure MouseMove(Shift: TShiftState; X, Y, Dx, Dy: single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: single); override;
    procedure KeyDown(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState); override;
    procedure DragOver(const Data: TvgDragObject; const Point: TvgPoint; var Accept: Boolean); override;
    procedure DragDrop(const Data: TvgDragObject; const Point: TvgPoint); override;
    property Count: integer read GetCount;
    property GlobalCount: integer read FGlobalCount;
    property CountExpanded: integer read FCountExpanded;
    property Selected: TvgTreeViewItem read FSelected write SetSelected;
    property Items[Index: integer]: TvgTreeViewItem read GetItem;
  published
    property Resource;
    property CanFocused default true;
    property DisableFocusEffect;
    property TabOrder;
    property AllowDrag: boolean read FAllowDrag write SetAllowDrag default false;
    property ItemHeight: single read FItemHeight write SetItemHeight;
    property HideSelectionUnfocused: boolean read FHideSelectionUnfocused write FHideSelectionUnfocused default true;
    property MultiSelect: boolean read FMultiSelect write FMultiSelect default false;
    property ShowCheckboxes: boolean read FShowCheckboxes write SetShowCheckboxes default false;
    property Sorted: boolean read FSorted write SetSorted default false;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChangeCheck: TNotifyEvent read FOnChangeCheck write FOnChangeCheck;
    property OnCompare: TOnCompareTreeViewItemEvent read FOnCompare write FOnCompare;
    property OnDragChange: TOnTreeViewDragChange read FOnDragChange write FOnDragChange;
  end;

implementation {===============================================================}

uses vg_ani;

{ TvgTreeViewItem }

constructor TvgTreeViewItem.Create(AOwner: TComponent);
begin
  inherited;
  Position.Point := vgPoint(5000, 5000);
  FAutoTranslate := true;
  TextAlign := vgTextAlignNear;
  Height := 19;
  HitTest := false;
  CanFocused := false;
end;

procedure TvgTreeViewItem.Realign;
begin
  if (TreeView <> nil) and (TreeView.FUpdating > 0) then Exit;
  inherited;
end;

procedure TvgTreeViewItem.DragEnd;
begin
  inherited;
  DragLeave;
  if (TreeView <> nil) and (TreeView.FDragItem <> nil) then
  begin
    TreeView.FDragItem.RemoveFreeNotify(TreeView);
    TreeView.FDragItem := nil;
  end;
end;

function TvgTreeViewItem.GetCount: integer;
var
  i: integer;
begin
  Result := 0;
  if ChildrenCount > 0 then
    for i := 0 to ChildrenCount - 1 do
      if Children[i] is TvgTreeViewItem then
      begin
        Inc(Result);
      end;
end;

function TvgTreeViewItem.ItemByPoint(const X, Y: single): TvgTreeViewItem;
var
  i: integer;
  P, P1: TvgPoint;
begin
  P := LocaltoAbsolute(vgPoint(X, Y));
  for i := 0 to Count - 1 do
    with ItemByIndex(i) do
    begin
      if not Visible then Continue;
      if pointInObject(P.X, P.Y) then
      begin
        Result := Self.ItemByIndex(i);
        Exit;
      end
      else
        if (Count > 0) and (IsExpanded) then
        begin
          P1 := AbsoluteToLocal(P);
          Result := ItemByPoint(P1.X, P1.Y);
          if Result <> nil then
            Exit;
        end;
    end;
  Result := nil;
end;

function TvgTreeViewItem.ItemByIndex(const Idx: integer): TvgTreeViewItem;
var
  c, i: integer;
begin
  c := 0;
  if ChildrenCount > 0 then
    for i := 0 to ChildrenCount - 1 do
      if Children[i] is TvgTreeViewItem then
      begin
        if c = Idx then
        begin
          Result := TvgTreeViewItem(Children[i]);
          Exit;
        end;
        Inc(c);
      end;
  Result := nil;
end;

procedure TvgTreeViewItem.Paint;
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

function TvgTreeViewItem.ItemClass: string;
begin
  Result := ClassName;
end;

procedure TvgTreeViewItem.DesignClick;
begin
  inherited;
  IsExpanded := not IsExpanded;
end;

function TvgTreeViewItem.EnterFocusChildren(AObject: TvgVisualObject): boolean;
begin
  Result := inherited EnterFocusChildren(AObject);
  if (TreeView <> nil) then
  begin
    TreeView.Selected := Self;
    Result := true;
  end;
end;

function TvgTreeViewItem.Level: integer;
var
  P: TvgObject;
begin
  Result := 0;
  P := Parent;
  while (P <> nil) and not (P is TvgTreeView) and not (P is TvgContent) do
  begin
    Result := Result + 1;
    P := P.Parent;
  end;
end;

function TvgTreeViewItem.TreeView: TvgTreeView;
var
  P: TvgObject;
begin
  P := Parent;
  while (P <> nil) do
  begin
    if P is TvgTreeView then
    begin
      Result := TvgTreeView(P);
      Exit;
    end;
    P := P.Parent;
  end;
  Result := nil;
end;

procedure TvgTreeViewItem.FreeStyle;
begin
  inherited;
  FButton := nil;
  FCheck := nil;
end;

procedure TvgTreeViewItem.ApplyStyle;
var
  B: TvgObject;
begin
  inherited;
  B := FindResource('button');
  if (B <> nil) and (B is TvgCustomButton) then
  begin
    FButton := TvgCustomButton(B);
    FButton.OnClick := DoButtonClick;
    FButton.Visible := Count > 0;
    if FButton.Visible then
    begin
      FButton.ApplyResource;
      FButton.StartTriggerAnimation(Self, 'IsExpanded');
    end;
  end;
  B := FindResource('check');
  if (B <> nil) and (B is TvgCheckBox) then
  begin
    FCheck := TvgCheckBox(B);
    FCheck.IsChecked := IsChecked;
    FCheck.OnChange := DoCheckClick;
    if TreeView <> nil then
      FCheck.Visible := TreeView.ShowCheckboxes;
  end;
  StartTriggerAnimation(Self, 'IsSelected');
end;

procedure TvgTreeViewItem.DoCheckClick(Sender: TObject);
begin
  if FCheck <> nil then
    FIsChecked := FCheck.IsChecked;
  if TreeView <> nil then
  begin
    TreeView.SetFocus;
    TreeView.Selected := Self;
    if Assigned(TreeView.OnChangeCheck) then
      TreeView.OnChangeCheck(Self);
  end;
end;

procedure TvgTreeViewItem.UpdateCheck;
var
  i: integer;
begin
  if (TreeView <> nil) and (FCheck <> nil) then
    FCheck.Visible := TreeView.ShowCheckboxes;
  if ChildrenCount > 0 then
    for i := 0 to ChildrenCount - 1 do
      if Children[i] is TvgTreeViewItem then
        TvgTreeViewItem(Children[i]).UpdateCheck;
end;

procedure TvgTreeViewItem.SetIsChecked(const Value: boolean);
begin
  if FIsChecked <> Value then
  begin
    FIsChecked := Value;
    if FCheck <> nil then
      FCheck.IsChecked := FIsChecked;
  end;
end;

procedure TvgTreeViewItem.SetIsSelected(const Value: boolean);
begin
  if FIsSelected <> Value then
  begin
    FIsSelected := Value;
    StartTriggerAnimation(Self, 'IsSelected');
    if Treeview <> nil then Treeview.UpdateSelection;
  end;
end;

procedure TvgTreeViewItem.DoButtonClick(Sender: TObject);
begin
  IsExpanded := not IsExpanded;
end;

procedure TvgTreeViewItem.SetIsExpanded(const Value: boolean);
begin
  if FIsExpanded <> Value then
  begin
    FIsExpanded := Value;
    if (FButton <> nil) and not (csLoading in ComponentState) then
    begin
      FButton.Visible := Count > 0;
      if FButton.Visible then
        FButton.StartTriggerAnimation(Self, 'IsExpanded');
    end;
    if TreeView <> nil then
      TreeView.Realign;
  end;
end;

function TvgTreeViewItem.GetItem(Index: integer): TvgTreeViewItem;
begin
  Result := ItemByIndex(Index);
end;

{ TvgTreeView ==================================================================}

constructor TvgTreeView.Create(AOwner: TComponent);
begin
  inherited;
  FGlobalList := TList.Create;
  FGlobalList.Capacity := 100;
  CanFocused := true;
  AutoCapture := true;
  HideSelectionUnfocused := true;
  Width := 100;
  Height := 100;
  FItemHeight := 0;
end;

destructor TvgTreeView.Destroy;
begin
  if FSelections <> nil then
    FreeAndNil(FSelections);
  FreeAndNil(FGlobalList);
  inherited;
end;

procedure TvgTreeView.ApplyStyle;
var
  T: TvgObject;
begin
  inherited;
  T := FindResource('selection');
  if (T <> nil) and (T is TvgVisualObject) then
  begin
    FSelection := TvgVisualObject(T);
    FSelection.Visible := false;
  end;
  if (T <> nil) and (T is TvgVisualObject) then
  begin
    TvgVisualObject(T).Visible := false;
  end;
  UpdateSelection;
end;

procedure TvgTreeView.FreeStyle;
begin
  inherited;
  FSelection := nil;
  if FSelections <> nil then
    FSelections.Clear;
end;

procedure TvgTreeView.UpdateGlobalIndexes;
var
  GlobalIdx: integer;

  procedure AlignItem(AItem: TvgTreeViewItem);
  var
    i: integer;
    P: TvgPoint;
  begin
    AItem.GlobalIndex := GlobalIdx;
    GlobalIdx := GlobalIdx + 1;
    FGlobalList.Add(AItem);
    if AItem.Count > 0 then
    begin
      if AItem.IsExpanded then
        for i := 0 to AItem.Count - 1 do
          AlignItem(AItem.ItemByIndex(i));
    end;
  end;
var
  i: integer;
begin
  FGlobalList.Clear;
  GlobalIdx := 0;
  for i := 0 to Count - 1 do
    AlignItem(ItemByIndex(i));
  FGlobalCount := GlobalIdx;
end;

function CompareTreeItem(item1, item2: TvgObject): integer;
begin
  if (item1 is TvgTreeViewItem) and (item2 is TvgTreeViewItem) then
  begin
    if (TvgTreeViewItem(item1).TreeView <> nil) and Assigned(TvgTreeViewItem(item1).TreeView.OnCompare) then
      Result := TvgTreeViewItem(item1).TreeView.OnCompare(TvgTreeViewItem(item1), TvgTreeViewItem(item2))
    else
      {$IFDEF KS_COMPILER5}
      Result := CompareText(TvgTreeViewItem(item1).Text, TvgTreeViewItem(item2).Text);
      {$ELSE}
      Result := WideCompareText(TvgTreeViewItem(item1).Text, TvgTreeViewItem(item2).Text);
      {$ENDIF}
  end
  else
    Result := 0;
end;

procedure TvgTreeView.SortItems;
begin
  if not FSorted then Exit;
  FContent.Sort(CompareTreeItem);
end;

function TvgTreeView.GetItemRect(Item: TvgTreeViewItem): TvgRect;
var
  P: TvgPoint;
begin
  if Item <> nil then
  begin
    P := Item.LocalToAbsolute(vgPoint(0, 0));
    P := FContent.AbsoluteToLocal(P);
    Result := vgRect(0, 0, Item.Width, Item.Height);
    vgOffsetRect(Result, P.X, P.Y);
  end
  else
    Result := vgRect(0, 0, 0, 0);
end;

procedure TvgTreeView.UpdateSelection;
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
  // calc rects
  Vis := true;
  Sel := false;
  SetLength(SelRects, 0);
  for i := 0 to GlobalCount - 1 do
  begin
    if (ItemByGlobalIndex(i).IsSelected) then
    begin
      P := ItemByGlobalIndex(i).LocalToAbsolute(vgPoint(0, 0));
      if (FSelection.Parent <> nil) and (FSelection.Parent.IsVisual) then
        P := FSelection.Parent.Visual.AbsoluteToLocal(P);
      R := vgRect(P.X, P.Y, P.X + ItemByGlobalIndex(i).Width, P.Y + ItemByGlobalIndex(i).Height);
      if (Length(SelRects) > 0) and (i > 0) and (ItemByGlobalIndex(i - 1).IsSelected) then
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
      FSelections.Add(Clone);
      Clone.Parent := FSelection.Parent;
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

function TvgTreeView.GetContentBounds: TvgRect;
const
  StepX = 19;
var
  CurY, CurX: single;
  R: TvgRect;

  procedure HideItem(AItem: TvgTreeViewItem);
  var
    i: integer;
  begin
    AItem.Visible := false;
    AItem.Opacity := 0;
    if AItem.Count > 0 then
      for i := 0 to AItem.Count - 1 do
        HideItem(AItem.ItemByIndex(i));
  end;

  procedure AlignItem(AItem: TvgTreeViewItem);
  var
    i: integer;
    P: TvgPoint;
  begin
    P := vgPoint(CurX, CurY);
    P := FContent.LocalToAbsolute(P);
    P := AItem.Parent.Visual.AbsoluteToLocal(P);
    if FItemHeight <> 0 then
      AItem.SetBounds(P.X + AItem.Padding.Left, P.Y + AItem.Padding.Top,
        R.Right - R.Left - AItem.Padding.Left - AItem.Padding.Right - (AItem.Level * StepX), FItemHeight)
    else
      AItem.SetBounds(P.X + AItem.Padding.Left, P.Y + AItem.Padding.Top,
        R.Right - R.Left - AItem.Padding.Left - AItem.Padding.Right - (AItem.Level * StepX), AItem.Height);

    if AItem.FButton <> nil then
      AItem.FButton.Visible := AItem.Count > 0;

    CurY := CurY + AItem.Height + AItem.Padding.Top + AItem.Padding.Bottom;

    if AItem.Count > 0 then
    begin
      if AItem.IsExpanded then
      begin
        CurX := CurX + StepX;
        for i := 0 to AItem.Count - 1 do
        begin
          with AItem.ItemByIndex(i) do
          begin
            Visible := true;
            Opacity := 1;
          end;
          AlignItem(AItem.ItemByIndex(i));
        end;
        CurX := CurX - StepX;
      end
      else
      begin
        for i := 0 to AItem.Count - 1 do
          HideItem(AItem.ItemByIndex(i));
      end;
    end;
  end;
var
  i: integer;
  C: integer;
  P: TvgPoint;
  Sel: TvgTreeViewItem;
begin
  Result := LocalRect;
  UpdateGlobalIndexes;
  if FUpdating > 0 then Exit;
  if ContentLayout = nil then Exit;
  R := ContentLayout.LocalRect;
  { content }
  FCountExpanded := 0;
  if FContent <> nil then
  begin
    { Sort if need }
    SortItems;
    { align }
    CurY := 0;
    CurX := 0;
    for i := 0 to Count - 1 do
      AlignItem(ItemByIndex(i));
    R.Bottom := R.Top + CurY;
  end;
  if R.Bottom = R.Top then
    R.Bottom := R.Top + 1;
  Result := R;
  UpdateSelection;
end;

procedure TvgTreeView.HScrollChange(Sender: TObject);
begin
  inherited;
  UpdateSelection;
end;

procedure TvgTreeView.VScrollChange(Sender: TObject);
begin
  inherited;
  UpdateSelection;
end;

function TvgTreeView.ItemByIndex(const Idx: integer): TvgTreeViewItem;
begin
  if (FContent <> nil) and (FContent.ChildrenCount > 0) and (Idx >= 0) and (Idx < FContent.ChildrenCount) then
    Result := TvgTreeViewItem(FContent.Children[Idx])
  else
    Result := nil;
end;

function TvgTreeView.ItemByGlobalIndex(const Idx: integer): TvgTreeViewItem;
begin
  Result := TvgTreeViewItem(FGlobalList[Idx]);
end;

function TvgTreeView.ItemByPoint(const X, Y: single): TvgTreeViewItem;
var
  i: integer;
  P, P1: TvgPoint;
begin
  P := LocalToAbsolute(vgPoint(X, Y));
  if (FContent <> nil) and (FContent.ChildrenCount > 0) then
    for i := 0 to FContent.ChildrenCount - 1 do
      if FContent.Children[i] is TvgTreeViewItem then
      begin
        if not TvgTreeViewItem(FContent.Children[i]).Visible then Continue;
        if not vgIntersectRect(TvgTreeViewItem(FContent.Children[i]).UpdateRect, UpdateRect) then Continue;
        if TvgTreeViewItem(FContent.Children[i]).pointInObject(P.X, P.Y) then
        begin
          Result := TvgTreeViewItem(FContent.Children[i]);
          Exit;
        end
        else
          if (TvgTreeViewItem(FContent.Children[i]).IsExpanded) and (TvgTreeViewItem(FContent.Children[i]).Count > 0) then
          begin
            P1 := TvgTreeViewItem(FContent.Children[i]).AbsoluteToLocal(P);
            Result := TvgTreeViewItem(FContent.Children[i]).ItemByPoint(P1.X, P1.Y);
            if Result <> nil then
              Exit;
          end;
      end;
  Result := nil;
end;

procedure TvgTreeView.KeyDown(var Key: Word; var KeyChar: System.WideChar;
  Shift: TShiftState);
begin
  inherited ;
  if (Count > 0) and (Selected <> nil) then
  begin
    case Key of
      VK_ADD: Selected.IsExpanded := true;
      VK_SUBTRACT: Selected.IsExpanded := false;
      VK_HOME: Selected := ItemByGlobalIndex(0);
      VK_END: Selected := ItemByGlobalIndex(GlobalCount - 1);
      VK_UP:
        if Selected.GlobalIndex > 0 then
          Selected := ItemByGlobalIndex(Selected.GlobalIndex - 1);
      VK_DOWN:
        if Selected.GlobalIndex < GlobalCount - 1 then
          Selected := ItemByGlobalIndex(Selected.GlobalIndex + 1);
    else
      Exit;
    end;
    Key := 0;
  end;
end;

procedure TvgTreeView.KeyUp(var Key: Word; var KeyChar: System.WideChar;
  Shift: TShiftState);
begin
  inherited ;
end;

procedure TvgTreeView.DragOver(const Data: TvgDragObject;
  const Point: TvgPoint; var Accept: Boolean);
var
  Obj: TvgTreeViewItem;
begin
  inherited;
  with AbsoluteToLocal(Point) do
    Obj := ItemByPoint(X, Y);
  if (Obj <> FDragItem) then
  begin
    if FDragItem <> nil then
    begin
      FDragItem.DragLeave;
      FDragItem.RemoveFreeNotify(Self);
    end;
    FDragItem := Obj;
    if FDragItem <> nil then
    begin
      FDragItem.AddFreeNotify(Self);
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

procedure TvgTreeView.DragDrop(const Data: TvgDragObject;
  const Point: TvgPoint);
var
  Obj: TvgTreeViewItem;
  Allow: boolean;
begin
  inherited;
  if FDragItem <> nil then
  begin
    FDragItem.DragLeave;
    FDragItem.RemoveFreeNotify(Self);
    FDragItem := nil;
  end;
  with AbsoluteToLocal(Point) do
    Obj := ItemByPoint(X, Y);
  if Obj = nil then
  begin
    // to root
    Allow := true;
    if Assigned(OnDragChange) then
      OnDragChange(TvgTreeViewItem(Data.Source), nil, Allow);
    if Allow then
    begin
      TvgTreeViewItem(Data.Source).Parent := Self;
      Realign;
    end;
  end
  else
  begin
    Allow := true;
    if Assigned(OnDragChange) then
      OnDragChange(TvgTreeViewItem(Data.Source), Obj, Allow);
    if Allow then
    begin
      if not Obj.IsExpanded then
        Obj.IsExpanded := true;
      TvgTreeViewItem(Data.Source).Parent := Obj;
      Realign;
    end;
  end;
end;

procedure TvgTreeView.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: single);
var
  Item: TvgTreeViewItem;
begin
  inherited;
  if Button = mbLeft then
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
          Selected := Item;
        end
        else
        begin
          SelectRange(Item, Item);
          Selected := Item;
        end;
        FFirstSelect := Item;
      end
      else
      begin
        if Selected <> Item then
          Selected := Item
        else
          if AllowDrag then
            Scene.BeginVCLDrag(Selected, MakeScreenshot);
      end;
    end;
    FMouseSelecting := true;
  end;
end;

procedure TvgTreeView.MouseMove(Shift: TShiftState; X, Y, Dx, Dy: single);
begin
  inherited;
end;

procedure TvgTreeView.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: single);
begin
  inherited;
  FFirstSelect := nil;
  FMouseSelecting := false;
end;

procedure TvgTreeView.Clear;
var
  i: integer;
begin
  BeginUpdate;
  if FContent <> nil then
    if FContent.ChildrenCount > 0 then
      for i := FContent.ChildrenCount - 1 downto 0 do
        if FContent.Children[i] is TvgTreeViewItem then
          TvgObject(FContent.Children[i]).Free;
  FScrollDesign.Y := 0;
  FScrollDesign.X := 0;
  FSelected := nil;
  UpdateGlobalIndexes;
  UpdateSelection;
  EndUpdate;
end;

procedure TvgTreeView.SelectRange(Item1, Item2: TvgTreeViewItem);
var
  i: integer;
begin
  if Item1 = nil then Exit;
  if Item2 = nil then Exit;
  for i := 0 to Min(Item1.GlobalIndex, Item2.GlobalIndex) - 1 do
    ItemByGlobalIndex(i).IsSelected := false;
  for i := Max(Item1.GlobalIndex, Item2.GlobalIndex) + 1 to GlobalCount - 1 do
    ItemByGlobalIndex(i).IsSelected := false;
  for i := Min(Item1.GlobalIndex, Item2.GlobalIndex) to Max(Item1.GlobalIndex, Item2.GlobalIndex) do
    ItemByGlobalIndex(i).IsSelected := true;
end;

procedure TvgTreeView.ClearSelection;
var
  i: integer;
begin
  for i := 0 to GlobalCount - 1 do
    ItemByGlobalIndex(i).IsSelected := false;
end;

procedure TvgTreeView.SelectAll;
var
  i: integer;
begin
  for i := 0 to GlobalCount - 1 do
    ItemByGlobalIndex(i).IsSelected := true;
end;

procedure TvgTreeView.EnterFocus;
begin
  inherited;
  if HideSelectionUnfocused and (Selected <> nil) then
    UpdateSelection;
end;

procedure TvgTreeView.KillFocus;
begin
  inherited;
  if HideSelectionUnfocused and (Selected <> nil) then
    UpdateSelection;
end;

procedure TvgTreeView.AddObject(AObject: TvgObject);
begin
  if (FContent <> nil) and ((AObject is TvgTreeViewItem) or (AObject is TvgSelectionItem)) then
  begin
    FContent.AddObject(AObject);
  end
  else
    inherited;
end;

procedure TvgTreeViewItem.AddObject(AObject: TvgObject);
begin
  inherited;
  if AObject is TvgTreeViewItem then
    if FUpdating = 0 then
    begin
      TreeView.UpdateGlobalIndexes;
      TreeView.Realign;
    end;
end;

procedure TvgTreeViewItem.RemoveObject(AObject: TvgObject);
begin
  inherited;
  if AObject is TvgTreeViewItem then
  begin
    TvgTreeViewItem(AObject).IsSelected := false;
    if FUpdating = 0 then
    begin
      TreeView.UpdateGlobalIndexes;
      TreeView.Realign;
    end;
  end;
end;

procedure TvgTreeView.ContentAddObject(AObject: TvgObject);
begin
  inherited;
  if AObject is TvgTreeViewItem then
    if FUpdating = 0 then
    begin
      UpdateGlobalIndexes;
      Realign;
    end;
end;

procedure TvgTreeView.ContentRemoveObject(AObject: TvgObject);
begin
  inherited;
  if AObject is TvgTreeViewItem then
  begin
    TvgTreeViewItem(AObject).IsSelected := false;
    if FUpdating = 0 then
    begin
      UpdateGlobalIndexes;
      Realign;
    end;
  end;
end;

procedure TvgTreeView.SetSelected(const Value: TvgTreeViewItem);
var
  i: TvgObject;
  P: TvgPoint;
begin
  if FSelected <> Value then
  begin
    if (FSelected <> nil) and not MultiSelect then
      FSelected.IsSelected := false;

    FSelected := Value;
    if (FSelected <> nil) and (FContent <> nil) then
    begin
      i := FSelected.Parent;
      while ((i <> nil) and not (i is TvgTreeView)) do
      begin
        if (i is TvgTreeViewItem) then
          TvgTreeViewItem(i).IsExpanded := true;
        i := i.Parent;
      end;
      if (FContent <> nil) and (ContentLayout <> nil) and (VScrollBar <> nil) then
      begin
        P := ContentLayout.AbsoluteToLocal(FSelected.LocalToAbsolute(vgPoint(0, 0)));
        if P.Y < 0 then
          VScrollBar.Value := VScrollBar.Value + P.Y;
        if P.Y + FSelected.Padding.Top + FSelected.Padding.Bottom + FSelected.Height > ContentLayout.Height then
          VScrollBar.Value := VScrollBar.Value + (P.Y + FSelected.Padding.Top + FSelected.Padding.Bottom + FSelected.Height - ContentLayout.Height);
      end;
      FSelected.IsSelected := true;
    end;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

procedure TvgTreeView.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FSelected) then
    FSelected := nil;
  if (Operation = opRemove) and (AComponent = FDragItem) then
    FDragItem := nil;
end;

function TvgTreeView.ItemClass: string;
begin
  Result := 'TvgTreeViewItem';
end;

procedure TvgTreeView.SetItemHeight(const Value: single);
begin
  if FItemHeight <> Value then
  begin
    FItemHeight := Value;
    Realign;
  end;
end;

procedure TvgTreeView.CollapseAll;
var
  i: integer;
  item: TvgTreeViewItem;
begin
  BeginUpdate;
  for i := 0 to Count - 1 do
  begin
    item := ItemByIndex(i);
    if item <> nil then
      item.IsExpanded := false;
  end;
  EndUpdate;
end;

procedure TvgTreeView.ExpandAll;
var
  i: integer;
  item: TvgTreeViewItem;
begin
  BeginUpdate;
  for i := 0 to Count - 1 do
  begin
    item := ItemByIndex(i);
    if item <> nil then
      item.IsExpanded := true;
  end;
  EndUpdate;
end;

function TvgTreeView.GetSelection: TvgSelectionItem;
var
  B: TvgObject;
begin
  B := FindResource('selection');
  if (B <> nil) and (B is TvgSelectionItem) then
    Result := TvgSelectionItem(B)
  else
    Result := nil;
end;

procedure TvgTreeView.SetShowCheckboxes(const Value: boolean);
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

function TvgTreeView.GetItem(Index: integer): TvgTreeViewItem;
begin
  Result := ItemByIndex(Index);
end;

procedure TvgTreeView.SetSorted(const Value: boolean);
begin
  if FSorted <> Value then
  begin
    FSorted := Value;
    Realign;
  end;
end;

procedure TvgTreeView.SetAllowDrag(const Value: boolean);
begin
  if FAllowDrag <> Value then
  begin
    FAllowDrag := Value;
    if FAllowDrag then
      DragDisableHighlight := true;
  end;
end;

procedure TvgTreeView.EndUpdate;
begin
  inherited;
end;

function TvgTreeView.GetCount: integer;
begin
  Result := 0;
  if (FContent <> nil) then
    Result := FContent.ChildrenCount;
end;

initialization
  RegisterVGObjects('Trees', [TvgTreeView]);
  RegisterVGObjects('Items', [TvgTreeViewItem]);
end.

