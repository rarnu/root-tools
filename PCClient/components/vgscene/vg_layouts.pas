unit vg_layouts;

{$I vg_define.inc}
{$H+}

interface

uses
  {$IFNDEF NOVCL}
  {$IFDEF UCL} UControls, {$ELSE} Controls, {$ENDIF}
  {$ENDIF}
  Classes, SysUtils, vg_scene, vg_ani, vg_controls;

type

  TvgLayout = class(TvgVisualObject)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
  published
    property HitTest default false;
  end;

  TvgScaledLayout = class(TvgVisualObject)
  private
    FOriginalWidth: single;
    FOriginalHeight: single;
    procedure SetOriginalWidth(const Value: single);
    procedure SetOriginalHeight(const Value: single);
  protected
    function GetChildrenMatrix: TvgMatrix; override;
    procedure SetHeight(const Value: single); override;
    procedure SetWidth(const Value: single); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    procedure Realign; override;
  published
    property OriginalWidth: single read FOriginalWidth write SetOriginalWidth;
    property OriginalHeight: single read FOriginalHeight write SetOriginalHeight;
  end;

  TvgScrollContent = class(TvgContent)
  private
  protected
    function GetClipRect: TvgRect; override;
    procedure PaintChildren; override;
    function ObjectByPoint(X, Y: single): TvgVisualObject; override;
    function GetUpdateRect: TvgRect; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
    procedure AddObject(AObject: TvgObject); override;
    procedure RemoveObject(AObject: TvgObject); override;
  published
  end;

  TvgScrollBox = class(TvgControl)
  private
    FAutoHide: boolean;
    FDisableMouseWheel: boolean;
    FDown: boolean;
    FHScrollAni: TvgFloatAnimation;
    FVScrollAni: TvgFloatAnimation;
    FAnimated: boolean;
    FShowScrollBars: boolean;
    FShowSizeGrip: boolean;
    FMouseTracking: boolean;
    FUseSmallScrollBars: boolean;
    procedure SetShowScrollBars(const Value: boolean);
    procedure SetShowSizeGrip(const Value: boolean);
    procedure SetUseSmallScrollBars(const Value: boolean);
  protected
    FScrollDesign: TvgPoint;
    FContent: TvgScrollContent;
    FHScrollBar: TvgScrollBar;
    FVScrollBar: TvgScrollBar;
    FContentLayout: TvgVisualObject;
    FDownPos, FLastDelta, FCurrentPos: TvgPoint;
    { VCL }
    procedure Loaded; override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadScrollDesign(Reader: TReader);
    procedure WriteScrollDesign(Writer: TWriter);
    { }
    procedure ContentAddObject(AObject: TvgObject); virtual;
    procedure ContentBeforeRemoveObject(AObject: TvgObject); virtual;
    procedure ContentRemoveObject(AObject: TvgObject); virtual;
    procedure HScrollChange(Sender: TObject); virtual;
    procedure VScrollChange(Sender: TObject); virtual;
    procedure ApplyStyle; override;
    procedure FreeStyle; override;
    procedure CreateVScrollAni;
    procedure CreateHScrollAni;
    function ContentRect: TvgRect;
    function VScrollBarValue: single;
    function HScrollBarValue: single;
    function GetContentBounds: TvgRect; virtual;
    procedure ContentPaint(const Canvas: TvgCanvas; const ARect: TvgRect); virtual;
    procedure RealignContent(R: TvgRect); virtual;
    property ContentLayout: TvgVisualObject read FContentLayout;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddObject(AObject: TvgObject); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: single); override;
    procedure MouseMove(Shift: TShiftState; X, Y, Dx, Dy: single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: single); override;
    procedure MouseWheel(Shift: TShiftState; WheelDelta: integer; var Handled: boolean); override;
    procedure Realign; override;
    procedure Centre;
    procedure ScrollTo(const Dx, Dy: single);
    procedure InViewRect(const Rect: TvgRect);
    function ClientWidth: single;
    function ClientHeight: single;
    property HScrollBar: TvgScrollBar read FHScrollBar;
    property VScrollBar: TvgScrollBar read FVScrollBar;
  published
    property AutoHide: boolean read FAutoHide write FAutoHide default true;
    property Animated: boolean read FAnimated write FAnimated default true;
    property DisableMouseWheel: boolean read FDisableMouseWheel write FDisableMouseWheel default false;
    property MouseTracking: boolean read FMouseTracking write FMouseTracking default false;
    property ShowScrollBars: boolean read FShowScrollBars write SetShowScrollBars default true;
    property ShowSizeGrip: boolean read FShowSizeGrip write SetShowSizeGrip default false;
    property UseSmallScrollBars: boolean read FUseSmallScrollBars write SetUseSmallScrollBars default false;
  end;

  TvgVertScrollBox = class(TvgScrollBox)
  private
  protected
    function GetContentBounds: TvgRect; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
  end;

  TvgFramedScrollBox = class(TvgScrollBox)
  private
  protected
  public
  published
  end;

  TvgFramedVertScrollBox = class(TvgVertScrollBox)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
  published
  end;

  TvgGridLayout = class(TvgVisualObject)
  private
    FItemWidth: single;
    FItemHeight: single;
    FOrientation: TvgOrientation;
    procedure SetItemHeight(const Value: single);
    procedure SetItemWidth(const Value: single);
    procedure SetOrientation(const Value: TvgOrientation);
  protected
    procedure Realign; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property ItemHeight: single read FItemHeight write SetItemHeight;
    property ItemWidth: single read FItemWidth write SetItemWidth;
    property Orientation: TvgOrientation read FOrientation write SetOrientation;
  end;

  TvgSplitLayout = class(TvgVisualObject)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property HitTest default false;
  end;

{$IFNDEF NOVCL}

  TvgNonVGLayout = class(TvgLayout)
  private
    FControl: TControl;
    FVisibleTimer: TvgTimer;
    procedure SetControl(const Value: TControl);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function CheckParentVisible: boolean; override;
    procedure DoVisibleTimer(Sender: TObject);
    procedure MatrixChanged(Sender: TObject); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Realign; override;
  published
    property NonVGControl: TControl read FControl write SetControl;
  end;

{$ENDIF}

implementation {===============================================================}

{ TvgLayout ===================================================================}

constructor TvgLayout.Create(AOwner: TComponent);
begin
  inherited;
  HitTest := false;
end;

destructor TvgLayout.Destroy;
begin
  inherited;
end;

procedure TvgLayout.Paint;
var
  R: TvgRect;
begin
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

{ TvgScrollContent }

constructor TvgScrollContent.Create(AOwner: TComponent);
begin
  inherited;
  ClipChildren := true;
end;

function TvgScrollContent.GetClipRect: TvgRect;
begin
  if (Parent <> nil) and (Parent is TvgScrollBox) and (TvgScrollBox(Parent).ContentLayout <> nil) then
  begin
    Result := TvgScrollBox(Parent).ContentLayout.LocalRect;
    if (TvgScrollBox(Parent).VScrollBar <> nil) and (TvgScrollBox(Parent).VScrollBar.Enabled) then
      vgOffsetRect(Result, 0, TvgScrollBox(Parent).VScrollBar.Value);
    if (TvgScrollBox(Parent).HScrollBar <> nil) and (TvgScrollBox(Parent).HScrollBar.Enabled) then
      vgOffsetRect(Result, TvgScrollBox(Parent).HScrollBar.Value, 0);
  end
  else
    Result := inherited GetClipRect;
end;

function TvgScrollContent.ObjectByPoint(X, Y: single): TvgVisualObject;
var
  P: TvgPoint;
begin
  Result := inherited ObjectByPoint(X, Y);
  if Result <> nil then
  begin
    P := AbsoluteToLocal(vgPoint(X, Y));
    if not vgPtInRect(P, ClipRect) then
      Result := nil;
  end;
end;

procedure TvgScrollContent.Paint;
var
  State: cardinal;
begin
  inherited;
  State := Canvas.SaveCanvas;
  Canvas.SetMatrix(AbsoluteMatrix);
  Canvas.IntersectClipRect(ClipRect);

  if (Parent <> nil) and (Parent is TvgScrollBox) and (TvgScrollBox(Parent).ContentLayout <> nil) then
  begin
    TvgScrollBox(Parent).ContentPaint(Canvas, LocalRect);
  end;

  Canvas.RestoreCanvas(State);
end;

procedure TvgScrollContent.PaintChildren;
begin
  inherited ;
end;

procedure TvgScrollContent.AddObject(AObject: TvgObject);
begin
  inherited;
  if (Parent <> nil) and (Parent is TvgScrollBox) then
    TvgScrollBox(Parent).ContentAddObject(AObject);
end;

procedure TvgScrollContent.RemoveObject(AObject: TvgObject);
begin
  if (Parent <> nil) and (Parent is TvgScrollBox) then
    TvgScrollBox(Parent).ContentBeforeRemoveObject(AObject);
  inherited;
  if (Parent <> nil) and (Parent is TvgScrollBox) then
    TvgScrollBox(Parent).ContentRemoveObject(AObject);
end;

function TvgScrollContent.GetUpdateRect: TvgRect;
begin
{  Result := inherited GetUpdateRect;
  exit;}
  if FRecalcUpdateRect then
  begin
    if (Parent <> nil) and (Parent is TvgScrollBox) then
    begin
      if (TvgScrollBox(Parent).ContentLayout <> nil) then
        FUpdateRect := TvgScrollBox(Parent).ContentLayout.UpdateRect
      else
        FUpdateRect := TvgScrollBox(Parent).UpdateRect;
    end;
  end;
  Result := FUpdateRect;
end;

{ TvgScrollBox }

constructor TvgScrollBox.Create(AOwner: TComponent);
begin
  inherited;
  AutoCapture := true;
  FAnimated := true;
  FAutoHide := true;
  FShowScrollBars := true;
  FContent := TvgScrollContent.Create(Self);
  FContent.Parent := Self;
  FContent.Stored := false;
  FContent.Locked := true;
  FContent.HitTest := false;
end;

destructor TvgScrollBox.Destroy;
begin
  FContent := nil;
  inherited;
end;

procedure TvgScrollBox.FreeStyle;
begin
  inherited;
  FContentLayout := nil;
  FHScrollBar := nil;
  FVScrollBar := nil;
end;

procedure TvgScrollBox.ApplyStyle;
var
  B: TvgObject;
begin
  inherited;
  B := FindResource('sizegrip');
  if (B <> nil) and (B.IsVisual) then
    B.Visual.Visible := FShowSizeGrip;

  if FUseSmallScrollBars then
  begin
    B := FindResource('vsmallscrollbar');
    if (B <> nil) and (B is TvgScrollBar) then
    begin
      FVScrollBar := TvgScrollBar(B);
      FVScrollBar.OnChange := VScrollChange;
      FVScrollBar.Locked := true;
      if FVScrollBar.Tag = 0 then
        FVScrollBar.Tag := Integer(FVScrollBar.Align);
    end;
    B := FindResource('hsmallscrollbar');
    if (B <> nil) and (B is TvgScrollBar) then
    begin
      FHScrollBar := TvgScrollBar(B);
      FHScrollBar.OnChange := HScrollChange;
      FHScrollBar.Locked := true;
      if FHScrollBar.Tag = 0 then
        FHScrollBar.Tag := Integer(FHScrollBar.Align);
    end;
    // hide standard
    B := FindResource('vscrollbar');
    if (B <> nil) and (B.IsVisual) then B.Visual.Visible := false;
    B := FindResource('hscrollbar');
    if (B <> nil) and (B.IsVisual) then B.Visual.Visible := false;
  end;
  if not FUseSmallScrollBars or ((FVScrollBar = nil) or (FHScrollBar = nil)) then
  begin
    B := FindResource('vscrollbar');
    if (B <> nil) and (B is TvgScrollBar) then
    begin
      FVScrollBar := TvgScrollBar(B);
      FVScrollBar.OnChange := VScrollChange;
      FVScrollBar.Locked := true;
      if FVScrollBar.Tag = 0 then
        FVScrollBar.Tag := Integer(FVScrollBar.Align);
    end;
    B := FindResource('hscrollbar');
    if (B <> nil) and (B is TvgScrollBar) then
    begin
      FHScrollBar := TvgScrollBar(B);
      FHScrollBar.OnChange := HScrollChange;
      FHScrollBar.Locked := true;
      if FHScrollBar.Tag = 0 then
        FHScrollBar.Tag := Integer(FHScrollBar.Align);
    end;
    // hide standard
    B := FindResource('vsmallscrollbar');
    if (B <> nil) and (B.IsVisual) then B.Visual.Visible := false;
    B := FindResource('hsmallscrollbar');
    if (B <> nil) and (B.IsVisual) then B.Visual.Visible := false;
  end;

  B := FindResource('content');
  if (B <> nil) and (B.IsVisual) then
    FContentLayout := B.Visual;

  Realign;
  FVScrollAni := nil;
  FHScrollAni := nil;
end;

function TvgScrollBox.GetContentBounds: TvgRect;
var
  i: integer;
  R, LocalR: TvgRect;
begin
  Result := vgRect(0, 0, Width, Height);
  if (FContent <> nil) and (ContentLayout <> nil) then
  begin
    R := ContentLayout.LocalRect;
    for i := 0 to FContent.ChildrenCount - 1 do
      if TvgObject(FContent.Children[i]).isVisual then
        if (TvgVisualObject(FContent.Children[i]).Visible) then
        begin
          LocalR := TvgVisualObject(FContent.Children[i]).ParentedRect;
          R := vgUnionRect(R, LocalR);
        end;
    Result := R;
  end;
end;

procedure TvgScrollBox.RealignContent(R: TvgRect);
begin
  if (FContent <> nil) and (ContentLayout <> nil) then
  begin
    FContent.SetBounds(R.Left, R.Top, vgRectWidth(R), vgRectHeight(R));
    FContent.FRecalcUpdateRect := true; // need to recalc
  end;
end;

procedure TvgScrollBox.Realign;
 procedure IntAlign;
 var
   R: TvgRect;
 begin
    R := GetContentBounds;
    if vgRectWidth(R) * vgRectHeight(R) = 0 then Exit;
    vgOffsetRect(R, ContentLayout.Position.X, ContentLayout.Position.Y);
    if (HScrollBar <> nil) and (HScrollBar.Enabled) then
      vgOffsetRect(R, -FScrollDesign.X, 0);
    if (VScrollBar <> nil) and (VScrollBar.Enabled) then
      vgOffsetRect(R, 0, -FScrollDesign.Y);
    RealignContent(R);
    // realign resource
    if ContentLayout.Parent <> nil then
      ContentLayout.Parent.Visual.BeginUpdate;
    if (VScrollBar <> nil) then
    begin
      VScrollBar.Enabled := vgRectHeight(R) > ContentLayout.Height;
      if FAutoHide then
        VScrollBar.Visible := VScrollBar.Enabled;
      if not FShowScrollBars then
      begin
        VScrollBar.Opacity := 0;
        VScrollBar.Align := vaNone;
      end
      else
      begin
        VScrollBar.Opacity := 1;
        VScrollBar.Align := TvgAlign(VScrollBar.Tag);
      end;
    end;
    if (HScrollBar <> nil) then
    begin
      HScrollBar.Enabled := vgRectWidth(R) > ContentLayout.Width;
      if FAutoHide then
        HScrollBar.Visible := HScrollBar.Enabled;
      if not FShowScrollBars then
      begin
        HScrollBar.Opacity := 0;
        HScrollBar.Align := vaNone;
      end
      else
      begin
        HScrollBar.Opacity := 1;
        HScrollBar.Align := TvgAlign(HScrollBar.Tag);
        if (VScrollBar <> nil) and (VScrollBar.Enabled) then
          HScrollBar.Padding.right := VScrollBar.Width;
      end;
    end;
    if ContentLayout.Parent <> nil then
    begin
      ContentLayout.Parent.Visual.EndUpdate;
      ContentLayout.Parent.Visual.Realign;
    end;
    // align scrollbars
    if (VScrollBar <> nil) then
    begin
      VScrollBar.Enabled := vgRectHeight(R) > ContentLayout.Height;
      if FAutoHide then
        VScrollBar.Visible := VScrollBar.Enabled;
      if not FShowScrollBars then
      begin
        VScrollBar.Opacity := 0;
        VScrollBar.Align := vaNone;
        VScrollBar.Position.Y := Width + 100;
      end
      else
      begin
        VScrollBar.Opacity := 1;
        VScrollBar.HitTest := true;
        VScrollBar.Align := TvgAlign(VScrollBar.Tag);
      end;
      VScrollBar.BringToFront;
      if VScrollBar.Visible and (ContentLayout <> nil) then
      begin
        VScrollBar.Max := vgRectHeight(R);
        VScrollBar.ViewportSize := ContentLayout.Height;
        VScrollBar.SmallChange := VScrollBar.ViewportSize / 5;
        VScrollBar.Value := FScrollDesign.Y;
      end
      else
      begin
        VScrollBar.Value := 0;
      end;
    end;
    if (HScrollBar <> nil) then
    begin
      HScrollBar.Enabled := vgRectWidth(R) > ContentLayout.Width;
      HScrollBar.Padding.right := 0;
      if FAutoHide then
        HScrollBar.Visible := HScrollBar.Enabled;
      if not FShowScrollBars then
      begin
        HScrollBar.Opacity := 0;
        HScrollBar.Align := vaNone;
        HScrollBar.Position.Y := Height + 100;
      end
      else
      begin
        HScrollBar.Opacity := 1;
        HScrollBar.Align := TvgAlign(HScrollBar.Tag);
        if (VScrollBar <> nil) and (VScrollBar.Enabled) then
          HScrollBar.Padding.right := VScrollBar.Width;
      end;
      HScrollBar.BringToFront;
      if HScrollBar.Visible and (ContentLayout <> nil)  then
      begin
        HScrollBar.Max := vgRectWidth(R);
        HScrollBar.ViewportSize := ContentLayout.Width;
        HScrollBar.SmallChange := HScrollBar.ViewportSize / 5;
        HScrollBar.Value := ContentLayout.Position.X - FContent.Position.X;
      end
      else
        HScrollBar.Value := 0;
    end;
 end;
var
  R, NewR: TvgRect;
begin
  if csDestroying in ComponentState then Exit;
  inherited;
  if csLoading in ComponentState then Exit;
  if ContentLayout = nil then Exit;
  if FDisableAlign then Exit;
  if FUpdating > 0 then Exit;
  FDisableAlign := true;
  try
    R := ContentLayout.LocalRect;
    IntAlign;
    NewR := ContentLayout.LocalRect;
    if (vgRectWidth(NewR) <> vgRectWidth(R)) or (vgRectHeight(NewR) <> vgRectHeight(R)) then
    begin
      IntAlign;
    end;
  finally
    FDisableAlign := false;
  end;
end;

function TvgScrollBox.ContentRect: TvgRect;
begin
  if ContentLayout <> nil then
    Result := ContentLayout.ParentedRect
  else
    Result := LocalRect;
end;

function TvgScrollBox.VScrollBarValue: single;
begin
  if (VScrollBar <> nil) and (VScrollBar.Visible) then
    Result := VScrollBar.Value
  else
    Result := 0;
end;

function TvgScrollBox.HScrollBarValue: single;
begin
  if (HScrollBar <> nil) and (HScrollBar.Visible) then
    Result := HScrollBar.Value
  else
    Result := 0;
end;

procedure TvgScrollBox.HScrollChange(Sender: TObject);
begin
  if ContentLayout = nil then Exit;
  if HScrollBar.Visible then
    FContent.Position.X := ContentLayout.Position.X - HScrollBar.Value
  else
    FContent.Position.X := ContentLayout.Position.X;
  FScrollDesign.X := HScrollBar.Value;
end;

procedure TvgScrollBox.VScrollChange(Sender: TObject);
begin
  if ContentLayout = nil then Exit;
  if VScrollBar.Visible then
    FContent.Position.Y := ContentLayout.Position.Y - VScrollBar.Value
  else
    FContent.Position.Y := ContentLayout.Position.Y;
  FScrollDesign.Y := VScrollBar.Value;
end;

procedure TvgScrollBox.CreateHScrollAni;
begin
  if FHScrollAni = nil then
  begin
    FHScrollAni := TvgFloatAnimation.Create(Self);
    FHScrollAni.Parent := HScrollBar;
    FHScrollAni.AnimationType := vgAnimationOut;
    FHScrollAni.Interpolation := vgInterpolationQuadratic;
    FHScrollAni.Duration := 0.7;
    FHScrollAni.PropertyName := 'Value';
    FHScrollAni.StartFromCurrent := true;
  end;
end;

procedure TvgScrollBox.CreateVScrollAni;
begin
  if FVScrollAni = nil then
  begin
    FVScrollAni := TvgFloatAnimation.Create(Self);
    FVScrollAni.Parent := VScrollBar;
    FVScrollAni.AnimationType := vgAnimationOut;
    FVScrollAni.Interpolation := vgInterpolationQuadratic;
    FVScrollAni.Duration := 0.7;
    FVScrollAni.PropertyName := 'Value';
    FVScrollAni.StartFromCurrent := true;
  end;
end;

procedure TvgScrollBox.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: single);
begin
  inherited;
  if (Button = mbLeft) and FMouseTracking then
  begin
    FLastDelta := vgPoint(0, 0);
    FDownPos := vgPoint(X, Y);
    FCurrentPos := vgPoint(X, Y);
    FDown := true;
    if (FVScrollAni <> nil) and FVScrollAni.Running then
      FVScrollAni.StopAtCurrent;
    if (FHScrollAni <> nil) and FHScrollAni.Running then
      FHScrollAni.StopAtCurrent;
  end;
end;

procedure TvgScrollBox.MouseMove(Shift: TShiftState; X, Y, Dx, Dy: single);
begin
  inherited;
  if FDown and FMouseTracking then
  begin
    if (VScrollBar <> nil) and (VScrollBar.Visible) then
    begin
      VScrollBar.Value := VScrollBar.Value - (Y - FCurrentPos.Y);
      FLastDelta.Y := (Y - FCurrentPos.Y);
    end;
    if (HScrollBar <> nil) and (HScrollBar.Visible) then
    begin
      HScrollBar.Value := HScrollBar.Value - (X - FCurrentPos.X);
      FLastDelta.X := (X - FCurrentPos.X);
    end;
    FCurrentPos := vgPoint(X, Y);
  end;
end;

procedure TvgScrollBox.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: single);
begin
  inherited;
  if FDown and FMouseTracking then
  begin
    FDown := false;
    // animation
    if FAnimated and ((FLastDelta.X <> 0) or (FLastDelta.Y <> 0)) then
    begin
      if (VScrollBar <> nil) and (VScrollBar.Visible) then
      begin
        CreateVScrollAni;
        if FVScrollAni.Running then
          FVScrollAni.StopAtCurrent;
        FVScrollAni.StopValue := VScrollBar.Value - (FLastDelta.Y * 7);
        FVScrollAni.Start;
      end;
      if (HScrollBar <> nil) and (HScrollBar.Visible) then
      begin
        CreateHScrollAni;
        if FHScrollAni.Running then
          FHScrollAni.StopAtCurrent;
        FHScrollAni.StopValue := HScrollBar.Value - (FLastDelta.X * 7);
        FHScrollAni.Start;
      end;
    end;
  end;
end;

procedure TvgScrollBox.MouseWheel(Shift: TShiftState; WheelDelta: integer;
  var Handled: boolean);
begin
  inherited;
  if not Handled and not (FDisableMouseWheel) and (VScrollBar <> nil) and (VScrollBar.Visible) then
  begin
    if FAnimated then
    begin
      CreateVScrollAni;
      if FVScrollAni.Running then
        FVScrollAni.StopAtCurrent;
      FVScrollAni.StopValue := VScrollBar.Value - (VScrollBar.SmallChange * 3 * (WheelDelta / 120));
      FVScrollAni.Start;
    end
    else
      VScrollBar.Value := VScrollBar.Value - (VScrollBar.SmallChange * 3 * (WheelDelta / 120));
    Handled := true;
  end;
  if not Handled and not (FDisableMouseWheel) and (HScrollBar <> nil) and (HScrollBar.Visible) then
  begin
    if FAnimated then
    begin
      CreateHScrollAni;
      if FHScrollAni.Running then
        FHScrollAni.StopAtCurrent;
      FHScrollAni.StopValue := HScrollBar.Value - (HScrollBar.SmallChange * 3 * (WheelDelta / 120));
      FHScrollAni.Start;
    end
    else
      HScrollBar.Value := HScrollBar.Value - (HScrollBar.SmallChange * 3 * (WheelDelta / 120));
    Handled := true;
  end;
end;

procedure TvgScrollBox.AddObject(AObject: TvgObject);
begin
  if (FContent <> nil) and (AObject.TagString <> 'self') and (AObject <> FContent) and (AObject <> FResourceLink) and not (AObject is TvgEffect) and not (AObject is TvgAnimation) then
  begin
    FContent.AddObject(AObject);
  end
  else
    inherited;
end;

procedure TvgScrollBox.Loaded;
begin
  inherited;
  // ScrollTo(-FScrollDesign.X, -FScrollDesign.Y);
end;

procedure TvgScrollBox.Centre;
begin
  if (VScrollBar <> nil) and (VScrollBar.Visible) then
  begin
    VScrollBar.Value := (VScrollBar.Max - VScrollBar.ViewportSize) / 2;
  end;
  if (HScrollBar <> nil) and (HScrollBar.Visible) then
  begin
    HScrollBar.Value := (HScrollBar.Max - HScrollBar.ViewportSize) / 2;
  end;
end;

procedure TvgScrollBox.ScrollTo(const Dx, Dy: single);
begin
  if (VScrollBar <> nil) and (VScrollBar.Visible) then
    VScrollBar.Value := VScrollBar.Value - Dy;
  if (HScrollBar <> nil) and (HScrollBar.Visible) then
    HScrollBar.Value := HScrollBar.Value - Dx;
end;

procedure TvgScrollBox.InViewRect(const Rect: TvgRect);
begin

end;

procedure TvgScrollBox.SetShowScrollBars(const Value: boolean);
begin
  if FShowScrollBars <> Value then
  begin
    FShowScrollBars := Value;
    Realign;
  end;
end;

procedure TvgScrollBox.SetShowSizeGrip(const Value: boolean);
begin
  if FShowSizeGrip <> Value then
  begin
    FShowSizeGrip := Value;
    ApplyStyle;
  end;
end;

procedure TvgScrollBox.DefineProperties(Filer: TFiler);
begin
  inherited;
  // Filer.DefineProperty('ScrollDesign', ReadScrollDesign, WriteScrollDesign, (FScrollDesign.X <> 0) and (FScrollDesign.Y <> 0));
end;

procedure TvgScrollBox.ReadScrollDesign(Reader: TReader);
begin
  FScrollDesign := vgStringToPoint(Reader.ReadString);;
end;

procedure TvgScrollBox.WriteScrollDesign(Writer: TWriter);
begin
  Writer.WriteString(vgPointToString(FScrollDesign));
end;

procedure TvgScrollBox.ContentPaint(const Canvas: TvgCanvas; const ARect: TvgRect);
begin

end;

procedure TvgScrollBox.SetUseSmallScrollBars(const Value: boolean);
begin
  if FUseSmallScrollBars <> Value then
  begin
    FUseSmallScrollBars := Value;
    ApplyStyle;
  end;
end;

function TvgScrollBox.ClientHeight: single;
begin
  if ContentLayout <> nil then
    Result := ContentLayout.Height
  else
    Result := Height;
end;

function TvgScrollBox.ClientWidth: single;
begin
  if ContentLayout <> nil then
    Result := ContentLayout.Width
  else
    Result := Width;
end;

procedure TvgScrollBox.ContentAddObject(AObject: TvgObject);
begin

end;

procedure TvgScrollBox.ContentRemoveObject(AObject: TvgObject);
begin

end;

procedure TvgScrollBox.ContentBeforeRemoveObject(AObject: TvgObject);
begin

end;

{ TvgGridLayout }

constructor TvgGridLayout.Create(AOwner: TComponent);
begin
  inherited;
  ItemHeight := 64;
  ItemWidth := 64;
end;

procedure TvgGridLayout.Realign;
var
  i: integer;
  CurPos: TvgPoint;
begin
  if FDisableAlign then Exit;
  FDisableAlign := true;
  { content }
  CurPos := vgPoint(Margins.Left, Margins.Top);
  for i := 0 to ChildrenCount - 1 do
    if Children[i].IsVisual then
      with Children[i].Visual do
      begin
        SetBounds(CurPos.X + Padding.Left, CurPos.Y + Padding.Top, FItemWidth - Padding.Left - Padding.right,
          FItemHeight - Padding.top - Padding.bottom);
        if Orientation = vgHorizontal then
        begin
          CurPos.X := CurPos.X + FItemWidth;
          if CurPos.X + FItemWidth > Self.Width - Self.Margins.Left - Self.Margins.Right then
          begin
            CurPos.X := Self.Margins.Left;
            CurPos.Y := CurPos.Y + FItemHeight;
          end;
        end
        else
        begin
          CurPos.Y := CurPos.Y + FItemHeight;
          if CurPos.Y + FItemHeight > Self.Height - Self.Margins.Top - Self.Margins.Bottom then
          begin
            CurPos.Y := Self.Margins.Top;
            CurPos.X := CurPos.X + FItemWidth;
          end;
        end;
      end;
  FDisableAlign := false;
end;

procedure TvgGridLayout.SetItemHeight(const Value: single);
begin
  if FItemHeight <> Value then
  begin
    FItemHeight := Value;
    Realign;
  end;
end;

procedure TvgGridLayout.SetItemWidth(const Value: single);
begin
  if FItemWidth <> Value then
  begin
    FItemWidth := Value;
    Realign;
  end;
end;

procedure TvgGridLayout.SetOrientation(const Value: TvgOrientation);
begin
  if FOrientation <> Value then
  begin
    FOrientation := Value;
    Realign;
  end;
end;

{ TvgScaledLayout }

constructor TvgScaledLayout.Create(AOwner: TComponent);
begin
  inherited;
  FOriginalWidth := Width;
  FOriginalHeight := Height;
end;

destructor TvgScaledLayout.Destroy;
begin
  inherited;
end;

procedure TvgScaledLayout.Realign;
begin
  if (Parent <> nil) and (Parent is TvgScrollBox) and (TvgScrollBox(Parent).FUpdating > 0) then Exit;
  inherited;
  if not (Assigned(FScene) and (FScene.GetDesignTime)) then
  begin
    RecalcAbsolute;
    FRecalcUpdateRect := true;
  end;
end;

function TvgScaledLayout.GetChildrenMatrix: TvgMatrix;
begin
  if (Assigned(FScene) and (FScene.GetDesignTime)) then
  begin
    OriginalHeight := Height;
    OriginalWidth := Width;
  end;
  Result := IdentityMatrix;
  Result.m11 := Width / FOriginalWidth;
  Result.m22 := Height / FOriginalHeight;
end;

procedure TvgScaledLayout.Paint;
var
  R: TvgRect;
begin
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
  inherited ;
end;

procedure TvgScaledLayout.SetOriginalHeight(const Value: single);
begin
  if FOriginalHeight <> Value then
  begin
    FOriginalHeight := Value;
    if FOriginalHeight < 1 then
      FOriginalHeight := 1;
    RecalcAbsolute;
  end;
end;

procedure TvgScaledLayout.SetOriginalWidth(const Value: single);
begin
  if FOriginalWidth <> Value then
  begin
    FOriginalWidth := Value;
    if FOriginalWidth < 1 then
      FOriginalWidth := 1;
    RecalcAbsolute;
  end;
end;

procedure TvgScaledLayout.SetHeight(const Value: single);
begin
  inherited;
  if (Assigned(FScene) and (FScene.GetDesignTime)) then
    OriginalHeight := Height
  else
    RecalcAbsolute;
end;

procedure TvgScaledLayout.SetWidth(const Value: single);
begin
  inherited;
  if (Assigned(FScene) and (FScene.GetDesignTime)) then
    OriginalWidth := Width
  else
    RecalcAbsolute;
end;

{ TvgVertScrollBox }

constructor TvgVertScrollBox.Create(AOwner: TComponent);
begin
  inherited;
  FResource := 'scrollboxstyle';
end;

function TvgVertScrollBox.GetContentBounds: TvgRect;
var
  i: integer;
begin
  if (FContent <> nil) and (ContentLayout <> nil) then
  begin
    FContent.Width := ContentLayout.Width;
  end;
  Result := inherited GetContentBounds;
end;

{ TvgFramedVertScrollBox }

constructor TvgFramedVertScrollBox.Create(AOwner: TComponent);
begin
  inherited;
  FResource := 'framedscrollboxstyle';
end;

{ TvgSplitLayout }

constructor TvgSplitLayout.Create(AOwner: TComponent);
begin
  inherited;
end;

destructor TvgSplitLayout.Destroy;
begin
  inherited;
end;

{$IFNDEF NOVCL}
{ TvgNonVGLayout }

constructor TvgNonVGLayout.Create(AOwner: TComponent);
begin
  inherited;
  FVisibleTimer := TvgTimer.Create(Self);
  FVisibleTimer.Enabled := false;
  FVisibleTimer.Interval := 100;
  FVisibleTimer.OnTimer := DoVisibleTimer;
end;

destructor TvgNonVGLayout.Destroy;
begin
  FVisibleTimer.Free;
  inherited;
end;

procedure TvgNonVGLayout.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FControl) then
    FControl := nil;
end;

function TvgNonVGLayout.CheckParentVisible: boolean;
begin
  Result := inherited CheckParentVisible;
  if (FControl = nil) then Exit;
  if (Result <> FControl.Visible) then
  begin
    FControl.Visible := Result;
    FVisibleTimer.Enabled := FControl.Visible;
    if (FControl.Visible) then Realign;
  end;
end;

procedure TvgNonVGLayout.Realign;
var
  R: TvgRect;
begin
  inherited;
  if (FControl <> nil) and (FControl.Visible) then
  begin
    R := AbsoluteRect;
    FControl.SetBounds(round(R.Left), round(R.Top), round(vgRectWidth(R)), round(vgRectHeight(R)));
  end;
end;

procedure TvgNonVGLayout.MatrixChanged(Sender: TObject);
var
  R: TvgRect;
begin
  inherited;
  if (FControl <> nil) and (FControl.Visible) then
  begin
    R := AbsoluteRect;
    FControl.SetBounds(round(R.Left), round(R.Top), round(vgRectWidth(R)), round(vgRectHeight(R)));
  end;
end;

procedure TvgNonVGLayout.SetControl(const Value: TControl);
begin
  if FControl <> Value then
  begin
    if Assigned(FScene) and (FControl = FScene.GetComponent) then Exit;
    FControl := Value;
    Realign;
  end;
end;

procedure TvgNonVGLayout.DoVisibleTimer(Sender: TObject);
begin
  CheckParentVisible;
end;

{$ENDIF}

initialization
  RegisterVGObjects('Layout', [TvgLayout, TvgScaledLayout, TvgGridLayout, TvgSplitLayout]);
  {$IFNDEF NOVCL}
  RegisterVGObjects('Layout', [TvgNonVGLayout]);
  {$ENDIF}
  RegisterVGObjects('Controls', [TvgScrollBox, TvgVertScrollBox, TvgFramedScrollBox, TvgFramedVertScrollBox]);
end.


