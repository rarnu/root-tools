unit vg_tabcontrol;

{$I vg_define.inc}

interface

uses
  {$IFNDEF NOVCL}
  {$IFDEF UCL} UControls, {$ELSE} Controls, {$ENDIF}
  {$ENDIF}
  Classes, SysUtils, vg_scene, vg_objects, vg_controls;

type

  TvgTabItem = class(TvgTextControl)
  private
    FIndex: integer;
    FLayout: TvgVisualObject;
    FIsSelected: boolean;
    procedure SetIndex(const Value: integer);
  protected
    procedure ApplyStyle; override;
    procedure DesignSelect; override;
    procedure DesignInsert; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure SetVisible(const Value: boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: single); override;
    procedure Realign; override;
    procedure Select(ASelected: boolean);
  published
    { trigger }
    property IsSelected: boolean read FIsSelected;
    { props }
    property AutoTranslate default true;
    property Font;
    property Index: integer read FIndex write SetIndex;
    property TextAlign;
    property VertTextAlign;
    property Text;
    property Layout: TvgVisualObject read FLayout write FLayout;
    property Resource;
  end;

  TvgTabControl = class(TvgControl)
  private
    FItemIndex: integer;
    FOnChange: TNotifyEvent;
    FItemHeight: single;
    FFullSize: boolean;
    FBackground: TvgVisualObject;
    procedure SetItemIndex(const Value: integer);
    procedure SetItemHeight(const Value: single);
    procedure SetFullSize(const Value: boolean);
  protected
    function TabItem(AIndex: integer): TvgTabItem;
    function TabCount: integer;
    procedure ApplyStyle; override;
    procedure FreeStyle; override;
    procedure PaintChildren; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ItemClass: string; override;
    procedure Realign; override;
    procedure AddObject(AObject: TvgObject); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: single); override;
    procedure MouseMove(Shift: TShiftState; X, Y, Dx, Dy: single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: single); override;
    procedure MouseWheel(Shift: TShiftState; WheelDelta: integer; var Handled: boolean); override;
    procedure KeyDown(var Key: Word; var KeyChar: WideChar; Shift: TShiftState); //override;
    procedure KeyUp(var Key: Word; var KeyChar: WideChar; Shift: TShiftState); //override;
    procedure SetItem(AItem: TvgTabItem);
  published
    property Resource;
    property FullSize: boolean read FFullSize write SetFullSize default false;
    property ItemIndex: integer read FItemIndex write SetItemIndex default -1;
    property ItemHeight: single read FItemHeight write SetItemHeight;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

{ HUD }

  TvgHudTabItem = class(TvgTabItem)
  private
  public
  end;

  TvgHudTabControl = class(TvgTabControl)
  private
  protected
  public
    function ItemClass: string; override;
  published
  end;

implementation {===============================================================}

uses vg_ani, vg_layouts;

type
  TvgHackTabItem = class(TvgTabItem);

{ TvgTabItem }

constructor TvgTabItem.Create(AOwner: TComponent);
begin
  inherited;
  FAutoTranslate := true;
  Height := 20;
  Width := 80;
  HitTest := true;
end;

procedure TvgTabItem.ApplyStyle;
begin
  inherited;
  if (Parent <> nil) and (Parent is TvgTabControl) and (TvgTabControl(Parent).ItemIndex = Index) then
    Select(true)
  else
    Select(false)
end;

destructor TvgTabItem.Destroy;
begin
  inherited;
end;

procedure TvgTabItem.DesignSelect;
begin
  inherited;
  if (Parent <> nil) and (Parent is TvgTabControl) then
    TvgTabControl(Parent).ItemIndex := Index;
end;

procedure TvgTabControl.AddObject(AObject: TvgObject);
begin
  inherited;
  if AObject is TvgTabItem then
    Realign;
end;

procedure TvgTabItem.Realign;
begin
  inherited;
  if FDisableAlign then Exit;
  FDisableAlign := true;
  if (FLayout <> nil) and (Parent <> nil) and (Parent is TvgTabControl) then
  begin
    FLayout.Position.X := TvgTabControl(Parent).Margins.left + FLayout.Padding.left;
    FLayout.Position.Y := Self.Height + TvgTabControl(Parent).Margins.top + FLayout.Padding.bottom;
    FLayout.Width := TvgTabControl(Parent).Width - TvgTabControl(Parent).Margins.left - TvgTabControl(Parent).Margins.right -
      FLayout.Padding.left - FLayout.Padding.Right;
    FLayout.Height := TvgTabControl(Parent).Height - Self.Height - TvgTabControl(Parent).Margins.top - TvgTabControl(Parent).Margins.bottom -
      FLayout.Padding.top - FLayout.Padding.bottom;
  end;
  FDisableAlign := false;
end;

procedure TvgTabItem.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: single);
begin
  inherited;
  if Button = mbLeft then
  begin
    if (Parent <> nil) and (Parent is TvgTabControl) then
    begin
      TvgTabControl(Parent).ItemIndex := Index;
    end;
  end;
end;

procedure TvgTabItem.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FLayout) then
    FLayout := nil;
end;

procedure TvgTabItem.Select(ASelected: boolean);
begin
  FIsSelected := ASelected;
  StartTriggerAnimation(Self, 'IsSelected');
  ApplyTriggerEffect(Self, 'IsSelected');
end;

procedure TvgTabItem.SetIndex(const Value: integer);
begin
  if FIndex <> Value then
  begin
    inherited Index := Value;
    Realign;
  end;
end;

procedure TvgTabItem.SetVisible(const Value: boolean);
var
  S: TvgTabItem;
begin
  if Value <> Visible then
  begin
    S := TvgTabControl(Parent).TabItem(TvgTabControl(Parent).ItemIndex);
    inherited;
    if not Visible and IsSelected then
    begin
      Select(false);
      TvgTabControl(Parent).FItemIndex := -1;
      if Index >= TvgTabControl(Parent).TabCount then
        TvgTabControl(Parent).ItemIndex := TvgTabControl(Parent).TabCount - 1
      else
        TvgTabControl(Parent).ItemIndex := Index;
    end
    else
    begin
      TvgTabControl(Parent).Realign;
      if S <> nil then
      begin
        TvgTabControl(Parent).FItemIndex := -1;
        TvgTabControl(Parent).ItemIndex := S.Index;
      end;
    end;
  end;
end;

{ TvgTab ==================================================================}

constructor TvgTabControl.Create(AOwner: TComponent);
begin
  inherited;
  FItemIndex := -1;
  AutoCapture := true;
  Width := 200;
  Height := 200;
end;

destructor TvgTabControl.Destroy;
begin
  inherited;
end;

procedure TvgTabControl.FreeStyle;
begin
  inherited;
  FBackground := nil;
end;

procedure TvgTabControl.ApplyStyle;
var
  B: TvgObject;
begin
  inherited;
  B := FindResource('background');
  if (B <> nil) and (B.IsVisual) then
  begin
    FBackground := B.Visual;
  end;
  Realign;
end;

procedure TvgTabControl.PaintChildren;
var
  Sel: TvgTabItem;
  SaveOp: single;
begin
  Sel := TabItem(ItemIndex);
  if (Sel <> nil) and (Sel.Visible) then
  begin
    SaveOp := Sel.Opacity;
    Sel.Opacity := 0;
    inherited;
    Sel.Opacity := SaveOp;

    Canvas.SetMatrix(Sel.AbsoluteMatrix);
    Sel.BeforePaint;
    Sel.Paint;
    Sel.PaintChildren;
  end
  else
    inherited;
end;

procedure TvgTabControl.Realign;
var
  Idx, i: integer;
  CurX, CurY: single;
  AutoWidth, MaxHeight: single;
  B: TvgObject;
begin
  inherited;
  if FDisableAlign then Exit;
  FDisableAlign := true;
  { move all non TabItem to end of list }
  if FChildren <> nil then
    for i := 0 to FChildren.Count - 1 do
      if not (TvgObject(FChildren[i]) is TvgTabItem) then
        TvgObject(FChildren[i]).Index := FChildren.Count - 1;
  { calc max height }
  MaxHeight := 0;
  Idx := 0;
  if FChildren <> nil then
    for i := 0 to FChildren.Count - 1 do
      if TvgObject(FChildren[i]) is TvgTabItem then
        with TvgTabItem(FChildren[i]) do
        begin
          if not Visible then Continue;
          FIndex := Idx;
          if Height + Padding.Top + Padding.Bottom > MaxHeight then
            MaxHeight := Height + Padding.Top + Padding.Bottom;
          Idx := Idx + 1;
        end;
  if Idx = 0 then
    MaxHeight := 0
  else
    if FItemHeight > 0 then
      MaxHeight := FItemHeight;
  { background }
  if FResourceLink <> nil then
  begin
    B := FResourceLink;
    if (B <> nil) and (B.IsVisual) then
      with TvgVisualObject(B) do
      begin
        Align := vaNone;
        Position.X := Padding.Left;
        Position.Y := MaxHeight + Padding.Top;
        Width := Self.Width - Padding.Left - Padding.Top;
        Height := Self.Height - MaxHeight - Padding.Top - Padding.Bottom;
        BringToFront;
      end;
  end;
  { align }
  CurX := 0;
  CurY := 0;
  { fullsize }
  AutoWidth := Width;
  if FBackground <> nil then
    AutoWidth := Width - FBackground.Margins.Left - FBackground.Margins.Right;
  if FFullSize and (Idx > 0) then
    AutoWidth := AutoWidth / Idx
  else
    AutoWidth := AutoWidth;
  if FChildren <> nil then
    for i := 0 to FChildren.Count - 1 do
      if TvgObject(FChildren[i]) is TvgTabItem then
        with TvgTabItem(FChildren[i]) do
        begin
          if not Visible then Continue;
          Align := vaNone;
          if Layout <> nil then
          begin
            Layout.Align := vaNone;
            Layout.Visible := Index = ItemIndex;
            Layout.DesignHide := not (Index = ItemIndex);
            Layout.ClipChildren := true;
            if Layout.Visible then
              Layout.BringToFront;
          end;
          if FFullSize then
            SetBounds(CurX + Padding.Left, CurY + Padding.Top, AutoWidth, MaxHeight - Padding.Top - Padding.Bottom)
          else
            SetBounds(CurX + Padding.Left, CurY + Padding.Top, Width, MaxHeight - Padding.Top - Padding.Bottom);
          CurX := CurX + Padding.Left + Width + Padding.Right;
          Realign;
        end;
  FDisableAlign := false;
end;

function TvgTabControl.TabCount: integer;
var
  i: integer;
begin
  Result := 0;
  if FChildren <> nil then
    for i := 0 to FChildren.Count - 1 do
      if TvgObject(FChildren[i]) is TvgTabItem then
      begin
        if TvgTabItem(FChildren[i]).Visible then
        begin
          Inc(Result);
        end;
      end;
end;

function TvgTabControl.TabItem(AIndex: integer): TvgTabItem;
var
  i: integer;
begin
  { calc max height }
  if FChildren <> nil then
    for i := 0 to FChildren.Count - 1 do
      if TvgObject(FChildren[i]) is TvgTabItem then
      begin
        if TvgTabItem(FChildren[i]).Visible and (TvgTabItem(FChildren[i]).Index = AIndex) then
        begin
          Result := TvgTabItem(FChildren[i]);
          Exit;
        end;
      end;
  Result := nil;
end;

procedure TvgTabControl.KeyDown(var Key: Word; var KeyChar: WideChar;
  Shift: TShiftState);
begin
//  inherited ;
end;

procedure TvgTabControl.KeyUp(var Key: Word; var KeyChar: WideChar;
  Shift: TShiftState);
begin
//  inherited ;
end;

procedure TvgTabControl.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: single);
var
  Item: TvgTabItem;
begin
  inherited;
  Realign;
{  if Button = mbLeft then
  begin
    Item := ItemByPoint(X, Y);
    if Item <> nil then
      ItemIndex := Item.Index;
    FMouseSelecting := true;
  end;}
end;

procedure TvgTabControl.MouseMove(Shift: TShiftState; X, Y, Dx, Dy: single);
var
  Item: TvgTabItem;
begin
  inherited;
{  if (ssLeft in Shift) and FMouseSelecting then
  begin
    Item := ItemByPoint(X, Y);
    if Item <> nil then
      ItemIndex := Item.Index;
  end;}
end;

procedure TvgTabControl.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: single);
begin
  inherited;
//  FMouseSelecting := false;
end;

procedure TvgTabControl.MouseWheel(Shift: TShiftState; WheelDelta: integer;
  var Handled: boolean);
begin
  inherited;
end;

procedure TvgTabControl.SetItemIndex(const Value: integer);
begin
  if FItemIndex <> Value then
  begin
    if TabItem(FItemIndex) <> nil then
      TabItem(FItemIndex).Select(false);
    FItemIndex := Value;
    Realign;
    if TabItem(FItemIndex) <> nil then
      TabItem(FItemIndex).Select(true);
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

procedure TvgTabItem.DesignInsert;
var
  L: TvgLayout;
begin
  inherited;
  if FLayout = nil then
  begin
    L := TvgLayout.Create(Owner);
    L.Parent := Parent;
    if vgDesigner <> nil then
      L.Name := vgDesigner.UniqueName(Owner, L.ClassName);
    Layout := L;
  end;
end;

function TvgTabControl.ItemClass: string;
begin
  Result := 'TvgTabItem';
end;

procedure TvgTabControl.SetItem(AItem: TvgTabItem);
begin
  ItemIndex := AItem.Index;
end;

procedure TvgTabControl.SetItemHeight(const Value: single);
begin
  if FItemHeight <> Value then
  begin
    FItemHeight := Value;
    Realign;
  end;
end;

procedure TvgTabControl.SetFullSize(const Value: boolean);
begin
  if FFullSize <> Value then
  begin
    FFullSize := Value;
    Realign;
  end;
end;

{ TvgHudTabControl }

function TvgHudTabControl.ItemClass: string;
begin
  Result := 'TvgHudTabItem';
end;

initialization
  RegisterVGObjects('Controls', [TvgTabControl]);
  RegisterVGObjects('HUD', [TvgHudTabControl]);
  RegisterVGObjects('Items', [TvgTabItem, TvgHudTabItem]);
end.


