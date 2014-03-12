unit vg_extctrls;

{$I vg_define.inc}

interface

uses
  {$IFDEF WINDOWS}
  Windows,
  {$ENDIF}
  {$IFNDEF NOVCL}
  Controls,
  {$ENDIF}
  Variants, Classes, SysUtils, vg_listbox, vg_memo,
  vg_scene, vg_objects, vg_ani, vg_layouts, vg_controls, vg_textbox;

type

  TvgIPhoneButton = class(TvgBitmapButton)
  private
    FBackground: TvgBrush;
    procedure SetBackground(const Value: TvgBrush);
  protected
    procedure BackChanged(Sender: TObject);
    procedure ApplyStyle; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { props }
    property Background: TvgBrush read FBackground write SetBackground;
    { inherited }
    property Resource;
  end;

  TvgDockBar = class(TvgControl)
  private
    FMousePos: TvgPoint;
    FMaxSize: single;
    FMinSize: single;
    FAmplitude: single;
    procedure SetMaxSize(const Value: single);
    procedure SetMinSize(const Value: single);
  protected
    procedure Realign; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: single); override;
    procedure MouseMove(Shift: TShiftState; X, Y, Dx, Dy: single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: single); override;
    procedure MouseLeave; override;
    procedure Paint; override;
  published
    property MinSize: single read FMinSize write SetMinSize;
    property MaxSize: single read FMaxSize write SetMaxSize;
    property Resource;
  end;

  TvgDropTarget = class(TvgTextControl)
  private
    FOnDrop: TvgDragDropEvent;
    FFilter: string;
  protected
    procedure DragOver(const Data: TvgDragObject; const Point: TvgPoint; var Accept: Boolean); override;
    procedure DragDrop(const Data: TvgDragObject; const Point: TvgPoint); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Filter: string read FFilter write FFilter;
    property Font;
    property Text;
    property OnDroped: TvgDragDropEvent read FOnDrop write FOnDrop;
  end;

  TvgPlotGrid = class(TvgVisualObject)
  private
    FMarks: single;
    FFrequency: single;
    FLineFill: TvgBrush;
    procedure SetFrequency(const Value: single);
    procedure SetMarks(const Value: single);
    procedure SetLineFill(const Value: TvgBrush);
    procedure LineFillChanged(Sender: TObject);
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property LineFill: TvgBrush read FLineFill write SetLineFill;
    property Marks: single read FMarks write SetMarks;
    property Frequency: single read FFrequency write SetFrequency;
  end;

  TvgImageViewer = class(TvgScrollBox)
  private
    FBack: TvgRectangle;
    FImage: TvgImage;
    FScale: single;
    FMouseScaling: boolean;
    FShowBackground: boolean;
    function GetBitmap: TvgBitmap;
    procedure SetBitmap(const Value: TvgBitmap);
    procedure SetScale(const Value: single);
    function GetBackgroundFill: TvgBrush;
    procedure SetBackgroundFill(const Value: TvgBrush);
    procedure SetShowBackground(const Value: boolean);
  protected
    function GetContentBounds: TvgRect; override;
    procedure MouseWheel(Shift: TShiftState; WheelDelta: integer; var Handled: boolean); override;
    procedure DoBitmapChange(Sender: TObject);
    function GetData: Variant; override;
    procedure SetData(const Value: Variant); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BestFit;
  published
    property BackgroundFill: TvgBrush read GetBackgroundFill write SetBackgroundFill;
    property Bitmap: TvgBitmap read GetBitmap write SetBitmap;
    property BitmapScale: single read FScale write SetScale;
    property ShowBackground: boolean read FShowBackground write SetShowBackground default false;
    property MouseScaling: boolean read FMouseScaling write FMouseScaling default true;
  end;

{ Calendar control }

  TvgCalDayOfWeek = (vgMonday, vgTuesday, vgWednesday, vgThursday,
    vgFriday, vgSaturday, vgSunday, vgLocaleDefault);

  TvgCalendar = class(TvgControl)
  private
    FDateTime: TDateTime;
    FDays: TvgListBox;
    FToday, FPrev, FNext: TvgButton;
    FMonths: TvgPopupBox;
    FYears: TvgPopupBox;
    FWeeks: TvgGridLayout;
    FFirstDayOfWeek: TvgCalDayOfWeek;
    FFirstDayOfWeekNum: integer;
    FWeek: TvgGridLayout;
    FTodayDefault: Boolean;
    FOnChange: TNotifyEvent;
    FWeekNumbers: Boolean;
    FOnDayChange: TNotifyEvent;
    function GetDate: TDate;
    procedure SetDate(Value: TDate);
    procedure SetDateTime(const Value: TDateTime);
    procedure SetFirstDayOfWeek(const Value: TvgCalDayOfWeek);
    procedure SetTodayDefault(const Value: Boolean);
    procedure SetWeekNumbers(const Value: Boolean);
  protected
    FDisableDayChange: integer;
    procedure DoPrevClick(Sender: TObject);
    procedure DoNextClick(Sender: TObject);
    procedure DoTodayClick(Sender: TObject);
    procedure DoDayChange(Sender: TObject);
    procedure DoMonthChange(Sender: TObject);
    procedure DoYearChange(Sender: TObject);
    procedure FillList;
    function GetData: Variant; override;
    procedure SetData(const Value: Variant); override;
    procedure MouseWheel(Shift: TShiftState; WheelDelta: integer; var Handled: boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Realign; override;
    property DateTime: TDateTime read FDateTime write SetDateTime;
  published
    property BindingSource;
    property Date: TDate read GetDate write SetDate;
    property FirstDayOfWeek: TvgCalDayOfWeek read FFirstDayOfWeek write SetFirstDayOfWeek
      default vgLocaleDefault;
    property TodayDefault: Boolean read FTodayDefault write SetTodayDefault default false;
    property WeekNumbers: Boolean read FWeekNumbers write SetWeekNumbers default false;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnDayChange: TNotifyEvent read FOnDayChange write FOnDayChange;
  end;

  TvgCalendarBox = class(TvgTextControl)
  private
    FPopup: TvgPopup;
    FCalendar: TvgCalendar;
    FPlacement: TvgPlacement;
    function GetDate: TDate;
    procedure SetDate(const Value: TDate);
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: single); override;
    procedure DoClosePopup(Sender: TObject);
    procedure DoCalendarChanged(Sender: TObject);
    procedure DoDayChanged(Sender: TObject);
    procedure DoContentPaint(Sender: TObject; const Canvas: TvgCanvas; const ARect: TvgRect);
    procedure ApplyStyle; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DropDown;
    property Calendar: TvgCalendar read FCalendar;
  published
    property CanFocused default true;
    property DisableFocusEffect;
    property TabOrder;
    property Cursor default crDefault;
    property Date: TDate read GetDate write SetDate;
    property TextAlign default vgTextAlignNear;
  end;

  TvgCalendarTextBox = class(TvgCustomTextBox)
  private
    FPopup: TvgPopup;
    FCalendar: TvgCalendar;
    FPlacement: TvgPlacement;
    function GetDate: TDate;
    procedure SetDate(const Value: TDate);
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: single); override;
    procedure DoClosePopup(Sender: TObject);
    procedure DoCalendarChanged(Sender: TObject);
    procedure DoDayChanged(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DropDown;
    property Calendar: TvgCalendar read FCalendar;
  published
    property Cursor default crDefault;
    property Date: TDate read GetDate write SetDate;
  end;

{ Compound controls }

  TvgCompoundTrackBar = class(TvgControl)
  private
    FValueLabel: TvgLabel;
    FTextLabel: TvgLabel;
    FTrackBar: TvgTrackBar;
    FDecimalDigits: integer;
    FOnChange: TNotifyEvent;
    FSuffix: WideString;
    function GetValue: single;
    procedure SetValue(const Value: single);
    procedure SetDecimalDigits(const Value: integer);
    procedure SetSuffix(const Value: WideString);
  protected
    procedure DoTrack(Sender: TObject);
    procedure DoTracking(Sender: TObject);
    procedure UpdateLabel;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property DecimalDigits: integer read FDecimalDigits write SetDecimalDigits default 2;
    property TextLabel: TvgLabel read FTextLabel;
    property TrackBar: TvgTrackBar read FTrackBar;
    property ValueLabel: TvgLabel read FValueLabel;
    property Suffix: WideString read FSuffix write SetSuffix;
    property Value: single read GetValue write SetValue stored false;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TvgCompoundAngleBar = class(TvgControl)
  private
    FValueLabel: TvgLabel;
    FTextLabel: TvgLabel;
    FAngleBar: TvgAngleButton;
    FDecimalDigits: integer;
    FOnChange: TNotifyEvent;
    function GetValue: single;
    procedure SetValue(const Value: single);
  protected
    procedure DoChange(Sender: TObject);
    procedure UpdateLabel;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property TextLabel: TvgLabel read FTextLabel;
    property AngleButton: TvgAngleButton read FAngleBar;
    property ValueLabel: TvgLabel read FValueLabel;
    property Value: single read GetValue write SetValue stored false;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TvgCompoundTextBox = class(TvgControl)
  private
    FTextLabel: TvgLabel;
    FTextBox: TvgTextBox;
    FOnChange: TNotifyEvent;
    function GetText: WideString;
    procedure SetText(const Value: WideString);
  protected
    procedure DoChange(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
  published
    property TextLabel: TvgLabel read FTextLabel;
    property TextBox: TvgTextBox read FTextBox;
    property Value: WideString read GetText write SetText stored false;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TvgCompoundMemo = class(TvgControl)
  private
    FTextLabel: TvgLabel;
    FMemo: TvgMemo;
    FOnChange: TNotifyEvent;
    function GetText: WideString;
    procedure SetText(const Value: WideString);
  protected
    procedure DoChange(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
  published
    property TextLabel: TvgLabel read FTextLabel;
    property Memo: TvgMemo read FMemo;
    property Value: WideString read GetText write SetText stored false;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TvgCompoundNumberBox = class(TvgControl)
  private
    FTextLabel: TvgLabel;
    FNumberBox: TvgNumberBox;
    FOnChange: TNotifyEvent;
    function GetValue: single;
    procedure SetValue(const Value: single);
  protected
    procedure DoChange(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
  published
    property TextLabel: TvgLabel read FTextLabel;
    property NumberBox: TvgNumberBox read FNumberBox;
    property Value: single read GetValue write SetValue stored false;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TvgCompoundPopupBox = class(TvgControl)
  private
    FTextLabel: TvgLabel;
    FPopupBox: TvgPopupBox;
    FOnChange: TNotifyEvent;
    function GetItemIndex: integer;
    procedure SetItemIndex(const Value: integer);
  protected
    procedure DoChange(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
  published
    property TextLabel: TvgLabel read FTextLabel;
    property PopupBox: TvgPopupBox read FPopupBox;
    property Value: integer read GetItemIndex write SetItemIndex;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TvgCompoundColorButton = class(TvgControl)
  private
    FTextLabel: TvgLabel;
    FColorButton: TvgColorButton;
    FOnChange: TNotifyEvent;
    function GetValue: string;
    procedure SetValue(const Value: string);
  protected
    procedure DoChange(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
  published
    property TextLabel: TvgLabel read FTextLabel;
    property ColorButton: TvgColorButton read FColorButton;
    property Value: string read GetValue write SetValue stored false;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TvgCompoundImage = class(TvgControl)
  private
    FTextLabel: TvgLabel;
    FImage: TvgImageControl;
    FOnChange: TNotifyEvent;
  protected
    procedure DoChange(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
  published
    property TextLabel: TvgLabel read FTextLabel;
    property Image: TvgImageControl read FImage;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

implementation {===============================================================}

{ TvgIPhoneButton ===================================================================}

constructor TvgIPhoneButton.Create(AOwner: TComponent);
begin
  inherited;
  FBackground := TvgBrush.Create(vgBrushSolid, $FF808080);
  FBackground.OnChanged := BackChanged;
end;

destructor TvgIPhoneButton.Destroy;
begin
  FBackground.Free;
  inherited;
end;

procedure TvgIPhoneButton.BackChanged(Sender: TObject);
var
  T: TvgObject;
begin
  T := FindResource('background');
  if (T <> nil) and (T is TvgShape) then
    TvgShape(T).Fill.Assign(FBackground);
end;

procedure TvgIPhoneButton.ApplyStyle;
var
  T: TvgObject;
begin
  inherited ;
  T := FindResource('background');
  if (T <> nil) and (T is TvgShape) then
    TvgShape(T).Fill.Assign(FBackground);
end;

procedure TvgIPhoneButton.SetBackground(const Value: TvgBrush);
begin
  
end;

{ TvgDockBar }

constructor TvgDockBar.Create(AOwner: TComponent);
begin
  inherited;
  FMinSize := 32;
  FMaxSize := 64;
end;

destructor TvgDockBar.Destroy;
begin
  inherited;
end;

procedure TvgDockBar.MouseLeave;
begin
  inherited;
  FMousePos := vgPoint(0, 0);
  Realign;
end;

procedure TvgDockBar.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: single);
begin
  inherited;
end;

procedure TvgDockBar.MouseMove(Shift: TShiftState; X, Y, Dx, Dy: single);
begin
  inherited;
  FMousePos := vgPoint(X, Y);
  Realign;
end;

procedure TvgDockBar.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: single);
begin
  inherited;
end;

procedure TvgDockBar.Paint;
begin
  inherited;
end;

procedure TvgDockBar.Realign;
var
  i, j: integer;
  dist, Pos, MaxWidth, Amplitude: single;
  hot: boolean;
  List: TList;
begin
  inherited;
  if FChildren = nil then Exit;
  if FDisableAlign then Exit;
  FDisableAlign := true;
  try
    { make order left to right list }
    List := TList.Create;
    for i := 0 to FChildren.Count - 1 do
    begin
      if not TvgObject(FChildren[i]).isVisual then Continue;
      if not TvgVisualObject(FChildren[i]).Visible then Continue;

      if List.Count > 0 then
      begin
        for j := 0 to List.Count - 1 do
        begin
          if TvgVisualObject(FChildren[i]).Position.X < TvgVisualObject(List[j]).Position.X then
            Break;
        end;
        List.Insert(j, FChildren[i]);
      end
      else
        List.Add(FChildren[i]);
    end;
    { align }
    if FMousePos.X = 0 then
    begin
      { mouse leave }
      MaxWidth := List.Count * MinSize;
      for i := 0 to List.Count - 1 do
      begin
        TvgVisualObject(List[i]).Position.Y := Height - MinSize;
        TvgVisualObject(List[i]).Position.X := (Width / 2) - (MaxWidth / 2) + (i * MinSize);
        TvgVisualObject(List[i]).Height := MinSize;
        TvgVisualObject(List[i]).Width := MinSize;
      end;
    end
    else
    begin
      Amplitude := FMaxSize * 1.5;
      MaxWidth := (List.Count * MinSize);
      { check hot object }
      hot := false;
      for i := 0 to List.Count - 1 do
      begin
        if (FMousePos.X >= TvgVisualObject(List[i]).Position.X) and (FMousePos.X <= TvgVisualObject(List[i]).Position.X + TvgVisualObject(List[i]).Width) then
        begin
          hot := true;
          Break;
        end
      end;
      { set sizes }
      for i := 0 to List.Count - 1 do
      begin
        if (FMousePos.X >= TvgVisualObject(List[i]).Position.X) and (FMousePos.X <= TvgVisualObject(List[i]).Position.X + TvgVisualObject(List[i]).Width) then
        begin
          TvgVisualObject(List[i]).Width := MaxSize;
          TvgVisualObject(List[i]).Height := MaxSize;
          MaxWidth := MaxWidth + (MaxSize - MinSize);
        end
        else
        begin
          dist := (TvgVisualObject(List[i]).Position.X + (TvgVisualObject(List[i]).Width / 2)) - FMousePos.X;
          if (Abs(dist) < Amplitude) and (hot) then
          begin
            TvgVisualObject(List[i]).Width := MinSize + Sin((Pi / 2) + ((dist / Amplitude) * (Pi / 2))) * (MaxSize - MinSize);
            TvgVisualObject(List[i]).Height := TvgVisualObject(List[i]).Width;
            MaxWidth := MaxWidth + (TvgVisualObject(List[i]).Width - MinSize);
          end
          else
          begin
            TvgVisualObject(List[i]).Width := MinSize;
            TvgVisualObject(List[i]).Height := MinSize;
          end;
        end;
      end;
      { align }
      Pos := ((Width / 2) - (MaxWidth / 2));
      for i := 0 to List.Count - 1 do
      begin
        TvgVisualObject(List[i]).Position.Y := Height - TvgVisualObject(List[i]).Height - 1;
        TvgVisualObject(List[i]).Position.X := Pos;
        Pos := Pos + TvgVisualObject(List[i]).Height;
      end;
    end;
    { }
    List.Free;
  finally
    FDisableAlign := false;
  end;
end;

procedure TvgDockBar.SetMaxSize(const Value: single);
begin
  FMaxSize := Value;
end;

procedure TvgDockBar.SetMinSize(const Value: single);
begin
  FMinSize := Value;
end;

{ TvgDropTarget }

constructor TvgDropTarget.Create(AOwner: TComponent);
begin
  inherited;
  DragDisableHighlight := true;
  Width := 120;
  Height := 120;
end;

procedure TvgDropTarget.DragDrop(const Data: TvgDragObject;
  const Point: TvgPoint);
begin
  inherited;
  if Assigned(FOnDrop) then
    FOnDrop(Self, Data, Point);
end;

procedure TvgDropTarget.DragOver(const Data: TvgDragObject;
  const Point: TvgPoint; var Accept: Boolean);
begin
  inherited;
  Accept := true; //(Length(Data.Files) > 0) and FileExists(Data.Files[0]) and (Pos(ExtractFileExt(LowerCase(Data.Files[0])), Filter) > 0);
end;

{ Graph objects ===============================================================}

{ TvgPlotGrid }

constructor TvgPlotGrid.Create(AOwner: TComponent);
begin
  inherited;
  FLineFill := TvgBrush.Create(vgBrushSolid, $FF505050);
  FLineFill.OnChanged := LineFillChanged;
  FMarks := 25;
  FFrequency := 5;
end;

destructor TvgPlotGrid.Destroy;
begin
  FLineFill.Free;
  inherited;
end;

procedure TvgPlotGrid.LineFillChanged(Sender: TObject);
begin
  Repaint;
end;

procedure TvgPlotGrid.Paint;
var
  x, y: single;
begin
  x := 0;
  y := 0;
  Canvas.Stroke.Assign(FLineFill);
  while x < Width / 2 do
  begin
    if (x = 0) then
    begin
      Canvas.StrokeThickness := 2;
      Canvas.Stroke.SolidColor := FLineFill.SolidColor
    end
    else
    begin
      if (frac(x) = 0) and (frac(x / frequency / marks) = 0) then
        Canvas.Stroke.SolidColor := FLineFill.SolidColor
      else
        Canvas.Stroke.SolidColor := vgOpacity(FLineFill.SolidColor, 0.4);
      Canvas.StrokeThickness := 1;
    end;

    Canvas.DrawLine(vgPoint(round(Width / 2) + x + (Canvas.StrokeThickness / 2), 0), vgPoint(round(Width / 2) + x + (Canvas.StrokeThickness / 2), Height), AbsoluteOpacity);
    if x <> 0 then
      Canvas.DrawLine(vgPoint(round(Width / 2) - x + (Canvas.StrokeThickness / 2), 0), vgPoint(round(Width / 2) - x + (Canvas.StrokeThickness / 2), Height), AbsoluteOpacity);
    x := x + FFrequency;
  end;
  while y < Height / 2 do
  begin
    if (y = 0) then
    begin
      Canvas.StrokeThickness := 2;
      Canvas.Stroke.SolidColor := FLineFill.SolidColor
    end
    else
    begin
      if (frac(y) = 0) and (frac(y / frequency / marks) = 0) then
        Canvas.Stroke.SolidColor := FLineFill.SolidColor
      else
        Canvas.Stroke.SolidColor := vgOpacity(FLineFill.SolidColor, 0.4);
      Canvas.StrokeThickness := 1;
    end;

    Canvas.DrawLine(vgPoint(0, round(Height / 2) + y + (Canvas.StrokeThickness / 2)), vgPoint(Width, round(Height / 2) + y + (Canvas.StrokeThickness / 2)), AbsoluteOpacity);
    if y <> 0 then
      Canvas.DrawLine(vgPoint(0, round(Height / 2) - y + (Canvas.StrokeThickness / 2)), vgPoint(Width, round(Height / 2) - y + (Canvas.StrokeThickness / 2)), AbsoluteOpacity);
    y := y + FFrequency;
  end;
end;

procedure TvgPlotGrid.SetFrequency(const Value: single);
begin
  if FFrequency <> Value then
  begin
    FFrequency := Value;
    if FFrequency < 0.001 then
      FFrequency := 0.001;
    Repaint;
  end;
end;

procedure TvgPlotGrid.SetLineFill(const Value: TvgBrush);
begin
  FLineFill.Assign(Value);
end;

procedure TvgPlotGrid.SetMarks(const Value: single);
begin
  if FMarks <> Value then
  begin
    FMarks := Value;
    Repaint;
  end;
end;

{ TvgImageViewer }

constructor TvgImageViewer.Create(AOwner: TComponent);
begin
  inherited;
  FResource := 'framedscrollboxstyle';
  MouseTracking := true;
  MouseScaling := true;
  Cursor := crHandPoint;
  FScale := 1;
  FBack := TvgRectangle.Create(Self);
  FBack.HitTest := false;
  FBack.Parent := Self;
  FBack.Locked := true;
  FBack.Stroke.Style := vgBrushNone;
  FBack.Stored := false;
  FBack.Visible := false;
  FImage := TvgImage.Create(Self);
  FImage.HitTest := false;
  FImage.Parent := Self;
  FImage.Locked := true;
  FImage.Stored := false;
  FImage.WrapMode := vgImageStretch;
  FImage.Bitmap.OnChange := DoBitmapChange;
end;

destructor TvgImageViewer.Destroy;
begin
  inherited;
end;

function TvgImageViewer.GetBitmap: TvgBitmap;
begin
  Result := FImage.Bitmap;
end;

function TvgImageViewer.GetContentBounds: TvgRect;
begin
  FImage.SetBounds(0, 0, Bitmap.Width * BitmapScale, Bitmap.Height * BitmapScale);

  if (FContent <> nil) and (ContentLayout <> nil) then
  begin
    if FImage.Width < ContentLayout.Width then
      FImage.Position.X := round((ContentLayout.Width - FImage.Width) / 2);
    if FImage.Height < ContentLayout.Height then
      FImage.Position.Y := round((ContentLayout.Height - FImage.Height) / 2);
  end;
  FBack.SetBounds(FImage.Position.X, FImage.Position.Y, FImage.Width, FImage.Height);

  Result := vgUnionRect(vgRect(0, 0, 0, 0), FImage.ParentedRect);
end;

procedure TvgImageViewer.MouseWheel(Shift: TShiftState;
  WheelDelta: integer; var Handled: boolean);
begin
  if MouseScaling then
  begin
    BitmapScale := BitmapScale + (WheelDelta / 120) * 0.04;
    Handled := true;
  end;
  inherited;
end;

procedure TvgImageViewer.BestFit;
var
  R: TvgRect;
  s: single;
  NeedRealign: boolean;
begin
  if (FContent <> nil) and (ContentLayout <> nil) then
  begin
    NeedRealign := false;
    if (VScrollBar <> nil) and (VScrollBar.Enabled) then
      NeedRealign := true;
    if (VScrollBar <> nil) and (VScrollBar.Enabled) then
      NeedRealign := true;
    R := vgRect(0, 0, Bitmap.Width, Bitmap.Height);
    s := vgFitRect(R, ContentLayout.LocalRect);
    if s >= 1 then
      BitmapScale := 1 / s
    else
      BitmapScale := 1;
    if NeedRealign then
    begin
      R := vgRect(0, 0, Bitmap.Width, Bitmap.Height);
      s := vgFitRect(R, ContentLayout.LocalRect);
      if s >= 1 then
        BitmapScale := 1 / s
      else
        BitmapScale := 1;
    end;
  end
end;

procedure TvgImageViewer.SetBitmap(const Value: TvgBitmap);
begin
  FImage.Bitmap := Value;
end;

procedure TvgImageViewer.SetScale(const Value: single);
begin
  if FScale <> Value then
  begin
    FScale := Value;
    if FScale < 0.01 then
      FScale := 0.01;
    if FScale > 10 then
      FScale := 10;
    Realign;
    Centre;
  end;
end;

function TvgImageViewer.GetBackgroundFill: TvgBrush;
begin
  Result := FBack.Fill;
end;

procedure TvgImageViewer.SetBackgroundFill(const Value: TvgBrush);
begin
  FBack.Fill := Value;
end;

procedure TvgImageViewer.DoBitmapChange(Sender: TObject);
begin
  Realign;
end;

procedure TvgImageViewer.SetShowBackground(const Value: boolean);
begin
  if FShowBackground <> Value then
  begin
    FShowBackground := Value;
    FBack.Visible := FShowBackground;
  end;
end;

function TvgImageViewer.GetData: Variant;
begin
  Result := ObjectToVariant(Bitmap);
end;

procedure TvgImageViewer.SetData(const Value: Variant);
begin
  if VarIsNull(Value) then
    Bitmap.SetSize(1, 1)
  else
  if VarIsObject(Value) then
  begin
    if VariantToObject(Value) is TPersistent then
      Bitmap.Assign(TPersistent(VariantToObject(Value)));
  end
  else
    Bitmap.LoadFromFile(Value)
end;

{ TvgCalendar }

constructor TvgCalendar.Create(AOwner: TComponent);
var
  i: integer;
  L: TvgVisualObject;
  AYear, AMonth, ADay: Word;
begin
  inherited;
  FDateTime := Now;
  DecodeDate(FDateTime, AYear, AMonth, ADay);
  FFirstDayOfWeek := vgLocaleDefault;
  Width := 180;
  Height := 160;
  L := TvgLayout.Create(Self);
  with L do
  begin
    Parent := Self;
    Locked := true;
    Stored := false;
    Height := 19;
    Align := vaMostTop;
    Padding.Bottom := 2;
    FPrev := TvgButton.Create(Self);
    with FPrev do
    begin
      Parent := L;
      Width := 19;
      Locked := true;
      Stored := false;
      Align := vaLeft;
      Padding.Right := 2;
      Resource := 'transparentcirclebuttonstyle';
      OnClick := DoPrevClick;
      RepeatClick := true;
      with TvgScrollArrowLeft.Create(Self) do
      begin
        Parent := FPrev;
        Width := 5;
        Height := 5;
        Stroke.Style := vgBrushNone;
        Align := vaCenter;
        HitTest := false;
        Stored := false;
        Locked := true;
      end;
    end;
    FToday := TvgButton.Create(Self);
    with FToday do
    begin
      Parent := L;
      Width := 19;
      Locked := true;
      Stored := false;
      Align := vaLeft;
      Position.X := 30;
      Padding.Right := 2;
      Resource := 'transparentcirclebuttonstyle';
      OnClick := DoTodayClick;
      RepeatClick := true;
      with TvgEllipse.Create(Self) do
      begin
        Parent := FToday;
        Width := 5;
        Height := 5;
        Stroke.Style := vgBrushNone;
        Align := vaCenter;
        HitTest := false;
        Stored := false;
        Locked := true;
      end;
    end;
    FNext := TvgButton.Create(Self);
    with FNext do
    begin
      Parent := L;
      Width := 19;
      Locked := true;
      Stored := false;
      Position.X := 50;
      Align := vaLeft;
      Padding.Right := 2;
      Resource := 'transparentcirclebuttonstyle';
      RepeatClick := true;
      OnClick := DoNextClick;
      with TvgScrollArrowRight.Create(Self) do
      begin
        Parent := FNext;
        Width := 5;
        Height := 5;
        Stroke.Style := vgBrushNone;
        Align := vaCenter;
        HitTest := false;
        Stored := false;
        Locked := true;
      end;
    end;
    FMonths := TvgPopupBox.Create(Self);
    with FMonths do
    begin
      Parent := L;
      Align := vaClient;
      Locked := true;
      Stored := false;
      DisableFocusEffect := true;
      Padding.Left := 5;
      Padding.Right := 5;
      Resource := 'labelstyle';
      for i := 1 to 12 do
        Items.Add(LongMonthNames[i]);
      Font.Style := vgFontBold;
      TextAlign := vgTextAlignFar;
      ItemIndex := AMonth - 1;
      OnChange := DoMonthChange;
    end;
    FYears := TvgPopupBox.Create(Self);
    with FYears do
    begin
      Parent := L;
      Width := 40;
      Align := vaRight;
      Locked := true;
      Stored := false;
      DisableFocusEffect := true;
      Resource := 'labelstyle';
      for i := 1 to 10 do
        Items.Add(IntToStr(AYear - i));
      Items.Add(IntToStr(AYear));
      for i := 1 to 10 do
        Items.Add(IntToStr(AYear + i));
      Font.Style := vgFontBold;
      TextAlign := vgTextAlignNear;
      ItemIndex := 10;
      OnChange := DoYearChange;
    end;
  end;
  FWeek := TvgGridLayout.Create(Self);
  with FWeek do
  begin
    Parent := Self;
    Locked := true;
    Stored := false;
    Height := 19;
    Position.Y := 20;
    ItemHeight := 19;
    Align := vaTop;
    Padding.Bottom := 2;
    for i := 0 to 6 do
      with TvgLabel.Create(Self) do
      begin
        Parent := FWeek;
        Locked := true;
        Stored := false;
        TextAlign := vgTextAlignCenter;
        WordWrap := false;
      end;
    ItemWidth := Width / 7;
  end;
  FWeeks := TvgGridLayout.Create(Self);
  with FWeeks do
  begin
    Parent := Self;
    Locked := true;
    Stored := false;
    Width := 23;
    Align := vaMostLeft;
    Padding.Top := 19 + 2;
    ItemHeight := 19;
    ItemWidth := Width;
    Visible := false;
    for i := 0 to 5 do
      with TvgLabel.Create(Self) do
      begin
        Parent := FWeeks;
        Locked := true;
        Stored := false;
        TextAlign := vgTextAlignCenter;
        WordWrap := false;
      end;
  end;
  FDays := TvgListBox.Create(Self);
  with FDays do
  begin
    Parent := Self;
    Locked := true;
    Stored := false;
    Position.Y := 50;
    Height := 19 * 6;
    Align := vaTop;
    Columns := 7;
    ItemHeight := 19;
    AlternatingRowBackground := true;
    HideSelectionUnfocused := false;
    ShowScrollBars := false;
    OnChange := DoDayChange;
    Resource := 'transparentlistboxstyle';
    for i := 1 to 6 * 7 do
      with TvgListBoxItem.Create(Self) do
      begin
        Parent := FDays;
        Locked := true;
        Stored := false;
        TextAlign := vgTextAlignFar;
        WordWrap := false;
      end;
  end;
  FillList;
end;

function WeekOfYear(aDate : tDateTime) : byte;
var
  t,m,year : word;
  newyear : tDateTime;
  KW : word;
  wtag_ny : word;
begin
  DecodeDate (aDate, year,m,t); // calc year
  newyear := EncodeDate(year, 1, 1); // calc 1.1.year
  wtag_ny := ord(DayofWeek(newyear)); // DOW of 1.1.year
  KW := Trunc( ((aDate-newyear + ((wtag_ny + 1) Mod 7)-3) / 7)+1);
  if (KW = 0) then
  begin
    KW := 0;
  end;
  result := KW;
end;

procedure TvgCalendar.FillList;
var
  i: integer;
  AYear, PreMonth, AMonth, ADay: Word;
  Date: TDate;
  First, Last: integer;
  A: array[0..1] of char;
  Item: TvgListBoxItem;
begin
  FDisableDayChange := FDisableDayChange + 1;
  try
    { first day }
    if FFirstDayOfWeek = vgLocaleDefault then
    begin
      {$IFDEF WINDOWS}
      GetLocaleInfo(LOCALE_USER_DEFAULT, LOCALE_IFIRSTDAYOFWEEK, A, SizeOf(A));
      {$ENDIF}
      FFirstDayOfWeekNum := Ord(A[0]) - Ord('0');
    end
    else
      FFirstDayOfWeekNum := Ord(FFirstDayOfWeek);
    FFirstDayOfWeekNum := (8 + FFirstDayOfWeekNum) mod 7;
    { week days }
    for i := 0 to 6 do
      TvgLabel(FWeek.Children[i]).Text := Copy(ShortDayNames[1 + ((7 + i + FFirstDayOfWeekNum) mod 7)],5,MaxInt);//modify by eagle  ShortDayNames[1 + ((7 + i + FFirstDayOfWeekNum) mod 7)]
    { days }
    DecodeDate(FDateTime, AYear, AMonth, ADay);
    PreMonth := AMonth - 1;
    if PreMonth < 1 then
      PreMonth := 12;
    Date := EncodeDate(AYear, AMonth, 1);
    First := DayOfWeek(Date);
    if First - FFirstDayOfWeekNum < 3 then
      First := First + 7;
    if FDays.Count - (First + MonthDays[IsLeapYear(AYear), AMonth] - FFirstDayOfWeekNum) < 3 then
      First := First - 7;
    FDays.Tag := First; // store first
    Date := IncMonth(Date, 1);
    Last := DayOfWeek(Date);
    for i := 1 to First do
    begin
      Item := FDays.Items[i - 1];
      if Item = nil then Continue;
      Item.Opacity := 0.3;
      Item.Text := IntToStr(MonthDays[IsLeapYear(AYear), PreMonth] - First + i + 1 + FFirstDayOfWeekNum);
    end;
    for i := 1 to MonthDays[IsLeapYear(AYear), AMonth] do
    begin
      Item := FDays.Items[First + i - 2 - FFirstDayOfWeekNum];
      if Item = nil then Continue;
      Item.Opacity := 1;
      Item.Text := IntToStr(i);
    end;
    for i := First + MonthDays[IsLeapYear(AYear), AMonth] to FDays.Count + FFirstDayOfWeekNum do
    begin
      Item := FDays.Items[i - 1 - FFirstDayOfWeekNum];
      if Item = nil then Continue;
      Item.Opacity := 0.3;
      Item.Text := IntToStr(i - First - MonthDays[IsLeapYear(AYear), AMonth] + 1);
    end;
    { weeks number }
    if FWeekNumbers then
    begin
      FWeeks.Visible := true;
      DecodeDate(FDateTime, AYear, AMonth, ADay);
      Date := EncodeDate(AYear, AMonth, 1);
      for i := 0 to 5 do
        if WeekOfYear(Date) + i = 0 then
          TvgLabel(FWeeks.Children[i]).Text := IntToStr(52)
        else
        if WeekOfYear(Date) = 0 then
          TvgLabel(FWeeks.Children[i]).Text := IntToStr(i)
        else
        if WeekOfYear(Date) + i > 52 then
          TvgLabel(FWeeks.Children[i]).Text := IntToStr(WeekOfYear(Date) + i - 52)
        else
          TvgLabel(FWeeks.Children[i]).Text := IntToStr(WeekOfYear(Date) + i);
    end
    else
      FWeeks.Visible := false;
    { selection }
    FDays.ItemIndex := First + ADay - 2 - FFirstDayOfWeekNum;
    { month }
    FMonths.ItemIndex := AMonth - 1;
    { years }
    FYears.Items.Clear;
    for i := 10 downto 1 do
      FYears.Items.Add(IntToStr(AYear - i));
    FYears.Items.Add(IntToStr(AYear));
    for i := 1 to 10 do
      FYears.Items.Add(IntToStr(AYear + i));
    FYears.Text := IntToStr(AYear);
  finally
    FDisableDayChange := FDisableDayChange - 1;
  end;
end;

destructor TvgCalendar.Destroy;
begin
  inherited;
end;

function TvgCalendar.GetData: Variant;
begin
  Result := VarFromDateTime(FDateTime);
end;

procedure TvgCalendar.SetData(const Value: Variant);
var
  D: TDateTime;
begin
  if VarIsType(Value, varDate) then
    Date := VarToDateTime(Value)
  else
    if VarIsStr(Value) and TryStrToDateTime(AnsiString(Value), D) then
      Date := D;
end;

procedure TvgCalendar.DoDayChange(Sender: TObject);
var
  AYear, AMonth, ADay: Word;
begin
  DecodeDate(FDateTime, AYear, AMonth, ADay);
  if FDays.ItemIndex - FDays.Tag + 2 + FFirstDayOfWeekNum < 1 then
  begin
  end
  else
  if FDays.ItemIndex - FDays.Tag + 2 + FFirstDayOfWeekNum > MonthDays[IsLeapYear(AYear), AMonth] then
  begin
  end
  else
    DateTime := EncodeDate(AYear, AMonth, FDays.ItemIndex - FDays.Tag + 2 + FFirstDayOfWeekNum);
  if Assigned(FOnDayChange) and (FDisableDayChange = 0) then
    FOnDayChange(Self);
end;

procedure TvgCalendar.DoTodayClick(Sender: TObject);
begin
  Date := Now;
  if Assigned(FOnDayChange) and (FDisableDayChange = 0) then
    FOnDayChange(Self);
end;

procedure TvgCalendar.DoNextClick(Sender: TObject);
begin
  Date := IncMonth(Date, 1);
end;

procedure TvgCalendar.DoPrevClick(Sender: TObject);
begin
  Date := IncMonth(Date, -1);
end;

procedure TvgCalendar.DoMonthChange(Sender: TObject);
var
  AYear, AMonth, ADay: Word;
begin
  DecodeDate(FDateTime, AYear, AMonth, ADay);
  DateTime := EncodeDate(AYear, FMonths.ItemIndex + 1, ADay);
end;

procedure TvgCalendar.DoYearChange(Sender: TObject);
var
  AYear, AMonth, ADay: Word;
begin
  DecodeDate(FDateTime, AYear, AMonth, ADay);
  DateTime := EncodeDate(StrToInt(FYears.Text), AMonth, ADay);
end;

procedure TvgCalendar.Realign;
begin
  inherited;
  if FWeek <> nil then
    FWeek.ItemWidth := FWeek.Width / 7 - 0.1;
end;

function TvgCalendar.GetDate: TDate;
begin
  Result := TDate(FDateTime);
end;

procedure TvgCalendar.SetDate(Value: TDate);
begin
  FDisableDayChange := FDisableDayChange + 1;
  ReplaceTime(TDateTime(Value), FDateTime);
  try
    SetDateTime(Value);
  except
    SetDateTime(FDateTime);
    raise;
  end;
  FDisableDayChange := FDisableDayChange - 1;
end;

procedure TvgCalendar.SetDateTime(const Value: TDateTime);
begin
  if FDateTime <> Value then
  begin
    FDateTime := Value;
    FillList;
    if Assigned(FBindingObjects) then
      ToBindingObjects;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

procedure TvgCalendar.SetFirstDayOfWeek(const Value: TvgCalDayOfWeek);
begin
  if FFirstDayOfWeek <> Value then
  begin
    FFirstDayOfWeek := Value;
    FillList;
  end;
end;

procedure TvgCalendar.SetTodayDefault(const Value: Boolean);
begin
  FTodayDefault := Value;
  if FTodayDefault then Date := Now;
end;

procedure TvgCalendar.SetWeekNumbers(const Value: Boolean);
begin
  if FWeekNumbers <> Value then
  begin
    FWeekNumbers := Value;
    FillList;
  end;
end;

procedure TvgCalendar.MouseWheel(Shift: TShiftState; WheelDelta: integer;
  var Handled: boolean);
begin
  inherited;
  if not Handled then
  begin
    if WheelDelta > 0 then
      Date := IncMonth(Date, -1)
    else
      Date := IncMonth(Date, 1);
    Handled := true;
  end;
end;

{ TvgCalendarBox }

constructor TvgCalendarBox.Create(AOwner: TComponent);
begin
  inherited;
  CanFocused := true;
  Cursor := crDefault;
  TextAlign := vgTextAlignNear;
  FResource := 'combotextboxstyle';
  FPopup := TvgPopup.Create(Self);
  FPopup.FResource := 'combopopupstyle';
  FPopup.PlacementTarget := Self;
  FPopup.StaysOpen := false;
  FPopup.Stored := false;
  FPopup.Parent := Self;
  FPopup.Locked := true;
  FPopup.DesignHide := true;
  FPopup.OnClosePopup := DoClosePopup;
  FPopup.Width := 186;
  FPopup.Height := 166;
  FCalendar := TvgCalendar.Create(Self);
  FCalendar.Parent := FPopup;
  FCalendar.Stored := false;
  FCalendar.Padding.Rect := vgRect(3, 3, 3, 3);
  FCalendar.Align := vaClient;
  FCalendar.OnChange := DoCalendarChanged;
  FCalendar.OnDayChange := DoDayChanged;
  Text := DateTimeToStr(FCalendar.DateTime);
  Width := 100;
  Height := 22;
end;

destructor TvgCalendarBox.Destroy;
begin
  inherited;
end;

procedure TvgCalendarBox.DoCalendarChanged(Sender: TObject);
begin
  Text := DateTimeToStr(FCalendar.DateTime);
end;

procedure TvgCalendarBox.DoDayChanged(Sender: TObject);
begin
  if FPopup.IsOpen then
    DropDown;
end;

procedure TvgCalendarBox.DoClosePopup(Sender: TObject);
begin
end;

procedure TvgCalendarBox.DropDown;
begin
  if not FPopup.IsOpen then
  begin
    FPopup.Placement := FPlacement;
    FPopup.IsOpen := true;
  end
  else
  begin
    FPopup.IsOpen := false;
  end;
end;

function TvgCalendarBox.GetDate: TDate;
begin
  Result := FCalendar.Date;
end;

procedure TvgCalendarBox.SetDate(const Value: TDate);
begin
  FCalendar.Date := Value;
  Text := DateTimeToStr(FCalendar.DateTime);
end;

procedure TvgCalendarBox.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: single);
begin
  inherited;
  if (Button = mbLeft) then
  begin
    DropDown;
  end;
end;

procedure TvgCalendarBox.DoContentPaint(Sender: TObject;
  const Canvas: TvgCanvas; const ARect: TvgRect);
begin
  Canvas.Font.Assign(Font);
  Canvas.Fill.Assign(FontFill);
  Canvas.FillText(ARect, ARect, Text, false, AbsoluteOpacity, TextAlign, vgTextAlignCenter);
end;

procedure TvgCalendarBox.ApplyStyle;
var
  T: TvgObject;
begin
  inherited;
  T := FindResource('Content');
  if (T <> nil) and (T is TvgContent) then
  begin
    TvgContent(T).OnPaint := DoContentPaint;
  end;
end;

{ TvgCalendarTextBox }

constructor TvgCalendarTextBox.Create(AOwner: TComponent);
begin
  inherited;
  Cursor := crDefault;
  FFilterChar := '0123456789./';
  FResource := 'combotextboxstyle';
  FPopup := TvgPopup.Create(Self);
  FPopup.FResource := 'combopopupstyle';
  FPopup.PlacementTarget := Self;
  FPopup.StaysOpen := false;
  FPopup.Stored := false;
  FPopup.Parent := Self;
  FPopup.Locked := true;
  FPopup.DesignHide := true;
  FPopup.OnClosePopup := DoClosePopup;
  FPopup.Width := 186;
  FPopup.Height := 166;
  FCalendar := TvgCalendar.Create(Self);
  FCalendar.Parent := FPopup;
  FCalendar.Stored := false;
  FCalendar.Padding.Rect := vgRect(3, 3, 3, 3);
  FCalendar.Align := vaClient;
  FCalendar.OnChange := DoCalendarChanged;
  FCalendar.OnDayChange := DoDayChanged;
  Text := DateTimeToStr(FCalendar.DateTime);
end;

destructor TvgCalendarTextBox.Destroy;
begin
  inherited;
end;

procedure TvgCalendarTextBox.DoCalendarChanged(Sender: TObject);
begin
  if FPopup.IsOpen then
  begin
    Text := DateTimeToStr(FCalendar.DateTime);
    CaretPosition := Length(Text);
    Change;
    FNeedChange := false;
    DropDown;
  end;
end;

procedure TvgCalendarTextBox.DoDayChanged(Sender: TObject);
begin
end;

procedure TvgCalendarTextBox.DoClosePopup(Sender: TObject);
begin
  if ShowCaret and IsFocused then
    ShowCaretProc;
end;

procedure TvgCalendarTextBox.DropDown;
begin
  if not FPopup.IsOpen then
  begin
    if ShowCaret then
      HideCaret;
    FPopup.Placement := FPlacement;
    FPopup.IsOpen := true;
  end
  else
  begin
    FPopup.IsOpen := false;
  end;
end;

function TvgCalendarTextBox.GetDate: TDate;
begin
  Result := FCalendar.Date;
end;

procedure TvgCalendarTextBox.SetDate(const Value: TDate);
begin
  FCalendar.Date := Value;
  Text := DateTimeToStr(FCalendar.DateTime);
end;

procedure TvgCalendarTextBox.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: single);
begin
  inherited;
  if (Button = mbLeft) and not vgPtInRect(vgPoint(X, Y), ContentRect) then
  begin
    DropDown;
  end;
end;

{ TvgCompoundTrackBar =========================================================}

constructor TvgCompoundTrackBar.Create(AOwner: TComponent);
var
  C: TvgControl;
begin
  inherited;
  FDecimalDigits := 2;
  
  Width := 200;
  Height := 20;
  FTextLabel := TvgLabel.Create(Self);
  with FTextLabel do
  begin
    Parent := Self;
    Stored := false;
    Locked := true;
    Align := vaLeft;
    Name := 'TextLabel';
    Text := 'Caption';
    Width := 70;
    Padding.right := 5;
    WordWrap := false;
    TextAlign := vgTextAlignFar;
    {$ifdef ks_compiler7_up}
    SetSubComponent(True);
    {$endif}
    {$ifdef FPC}
    SetSubComponent(True);
    {$endif}
  end;
  C := TvgControl.Create(Self);
  with C do
  begin
    Parent := Self;
    Stored := false;
    Locked := true;
    Align := vaClient;
  end;
  FTrackBar := TvgTrackbar.Create(Self);
  with FTrackBar do
  begin
    Parent := C;
    Stored := false;
    Locked := true;
    Align := vaVertCenter;
    Name := 'TrackBar';
    {$ifdef ks_compiler7_up}
    SetSubComponent(True);
    {$endif}
    {$ifdef FPC}
    SetSubComponent(True);
    {$endif}
    OnChange := DoTrack;
    OnTracking := DoTracking;
  end;
  C := TvgControl.Create(Self);
  with C do
  begin
    Parent := Self;
    Stored := false;
    Locked := true;
    Align := vaRight;
  end;
  FValueLabel := TvgValueLabel.Create(Self);
  with FValueLabel do
  begin
    Parent := C;
    Stored := false;
    Locked := true;
    Align := vaVertCenter;
    Name := 'ValueLabel';
    Text := '0';
    Padding.left := 5;
    {$ifdef ks_compiler7_up}
    SetSubComponent(True);
    {$endif}
    {$ifdef FPC}
    SetSubComponent(True);
    {$endif}
  end;
end;

procedure TvgCompoundTrackBar.UpdateLabel;
begin
  if (FTrackBar.Frequency <> 0) and (frac(FTrackBar.Frequency) = 0) then
    FValueLabel.Text := IntToStr(Trunc(Value)) + Suffix
  else
    FValueLabel.Text := Format('%.' + IntToStr(FDecimalDigits) + 'f', [Self.Value]) + Suffix;
end;

procedure TvgCompoundTrackBar.DoTrack(Sender: TObject);
begin
  if Assigned(FOnChange) then FOnChange(Sender);
end;

procedure TvgCompoundTrackBar.DoTracking(Sender: TObject);
begin
  UpdateLabel;
end;

function TvgCompoundTrackBar.GetValue: single;
begin
  Result := FTrackBar.Value;
end;

procedure TvgCompoundTrackBar.SetDecimalDigits(const Value: integer);
begin
  if FDecimalDigits <> Value then
  begin
    FDecimalDigits := Value;
    UpdateLabel;
  end;
end;

procedure TvgCompoundTrackBar.SetValue(const Value: single);
begin
  FTrackBar.Value := Value;
  UpdateLabel;
end;

procedure TvgCompoundTrackBar.SetSuffix(const Value: WideString);
begin
  if FSuffix <> Value then
  begin
    FSuffix := Value;
    UpdateLabel;
  end;
end;

{ TvgCompoundAngleBar }

constructor TvgCompoundAngleBar.Create(AOwner: TComponent);
var
  C: TvgControl;
begin
  inherited;
  FDecimalDigits := 2;

  Width := 200;
  Height := 20;
  FTextLabel := TvgLabel.Create(Self);
  with FTextLabel do
  begin
    Parent := Self;
    Stored := false;
    Locked := true;
    Align := vaLeft;
    WordWrap := false;
    Name := 'TextLabel';
    Text := 'Caption';
    Width := 70;
    TextAlign := vgTextAlignFar;
    Padding.right := 5;
    {$ifdef ks_compiler7_up}
    SetSubComponent(True);
    {$endif}
    {$ifdef FPC}
    SetSubComponent(True);
    {$endif}
  end;
  C := TvgControl.Create(Self);
  with C do
  begin
    Width := 40;
    Parent := Self;
    Stored := false;
    Locked := true;
    Align := vaClient;
  end;
  FAngleBar := TvgAngleButton.Create(Self);
  with FAngleBar do
  begin
    Parent := C;
    Stored := false;
    Locked := true;
    Align := vaFit;
    Name := 'AngleBar';
    {$ifdef ks_compiler7_up}
    SetSubComponent(True);
    {$endif}
    {$ifdef FPC}
    SetSubComponent(True);
    {$endif}
    OnChange := DoChange;
  end;
  C := TvgControl.Create(Self);
  with C do
  begin
    Parent := Self;
    Stored := false;
    Locked := true;
    Align := vaRight;
  end;
  FValueLabel := TvgValueLabel.Create(Self);
  with FValueLabel do
  begin
    Parent := C;
    Stored := false;
    Locked := true;
    Align := vaVertCenter;
    Name := 'ValueLabel';
    Text := '0';
    Padding.left := 5;
    {$ifdef ks_compiler7_up}
    SetSubComponent(True);
    {$endif}
    {$ifdef FPC}
    SetSubComponent(True);
    {$endif}
  end;
end;

procedure TvgCompoundAngleBar.UpdateLabel;
begin
  FValueLabel.Text := IntToStr(Trunc(Value)) + '°';
end;

procedure TvgCompoundAngleBar.DoChange(Sender: TObject);
begin
  if Assigned(FOnChange) then FOnChange(Sender);
  UpdateLabel;
end;

function TvgCompoundAngleBar.GetValue: single;
begin
  Result := FAngleBar.Value;
end;

procedure TvgCompoundAngleBar.SetValue(const Value: single);
begin
  FAngleBar.Value := Value;
  UpdateLabel;
end;

{ TvgCompoundTextBox }

constructor TvgCompoundTextBox.Create(AOwner: TComponent);
var
  C: TvgControl;
begin
  inherited;
  Width := 200;
  Height := 21;
  FTextLabel := TvgLabel.Create(Self);
  with FTextLabel do
  begin
    Parent := Self;
    Stored := false;
    Locked := true;
    Align := vaLeft;
    Name := 'TextLabel';
    Text := 'Caption';
    Width := 70;
    TextAlign := vgTextAlignFar;
    WordWrap := false;
    Padding.right := 5;
    {$ifdef ks_compiler7_up}
    SetSubComponent(True);
    {$endif}
    {$ifdef FPC}
    SetSubComponent(True);
    {$endif}
  end;
  C := TvgControl.Create(Self);
  with C do
  begin
    Parent := Self;
    Stored := false;
    Locked := true;
    Align := vaClient;
  end;
  FTextBox := TvgTextBox.Create(Self);
  with FTextBox do
  begin
    Parent := C;
    Stored := false;
    Locked := true;
    Align := vaVertCenter;
    Name := 'TextBox';
    {$ifdef ks_compiler7_up}
    SetSubComponent(True);
    {$endif}
    {$ifdef FPC}
    SetSubComponent(True);
    {$endif}
    OnChange := DoChange;
  end;
end;

procedure TvgCompoundTextBox.DoChange(Sender: TObject);
begin
  if Assigned(FOnChange) then
    FOnChange(Sender);
end;

function TvgCompoundTextBox.GetText: WideString;
begin
  Result := FTextBox.Text;
end;

procedure TvgCompoundTextBox.SetText(const Value: WideString);
begin
  FTextBox.Text := Value;
end;

{ TvgCompoundMemo }

constructor TvgCompoundMemo.Create(AOwner: TComponent);
begin
  inherited;
  Width := 200;
  Height := 60;
  FTextLabel := TvgLabel.Create(Self);
  with FTextLabel do
  begin
    Parent := Self;
    Stored := false;
    Locked := true;
    Align := vaLeft;
    Name := 'TextLabel';
    Text := 'Caption';
    Width := 70;
    TextAlign := vgTextAlignFar;
    WordWrap := false;
    Padding.right := 5;
    {$ifdef ks_compiler7_up}
    SetSubComponent(True);
    {$endif}
    {$ifdef FPC}
    SetSubComponent(True);
    {$endif}
  end;
  FMemo := TvgMemo.Create(Self);
  with FMemo do
  begin
    Parent := Self;
    Stored := false;
    Locked := true;
    Align := vaClient;
    Name := 'Memo';
    {$ifdef ks_compiler7_up}
    SetSubComponent(True);
    {$endif}
    {$ifdef FPC}
    SetSubComponent(True);
    {$endif}
    OnChange := DoChange;
  end;
end;

procedure TvgCompoundMemo.DoChange(Sender: TObject);
begin
  if Assigned(FOnChange) then
    FOnChange(Sender);
end;

function TvgCompoundMemo.GetText: WideString;
begin
  Result := FMemo.Text;
end;

procedure TvgCompoundMemo.SetText(const Value: WideString);
begin
  FMemo.Text := Value;
end;

{ TvgCompoundNumberBox }

constructor TvgCompoundNumberBox.Create(AOwner: TComponent);
var
  C: TvgControl;
begin
  inherited;
  Width := 200;
  Height := 21;
  FTextLabel := TvgLabel.Create(Self);
  with FTextLabel do
  begin
    Parent := Self;
    Stored := false;
    Locked := true;
    Align := vaLeft;
    Name := 'TextLabel';
    Text := 'Caption';
    WordWrap := false;
    Width := 70;
    TextAlign := vgTextAlignFar;
    Padding.right := 5;
    {$ifdef ks_compiler7_up}
    SetSubComponent(True);
    {$endif}
    {$ifdef FPC}
    SetSubComponent(True);
    {$endif}
  end;
  C := TvgControl.Create(Self);
  with C do
  begin
    Parent := Self;
    Stored := false;
    Locked := true;
    Align := vaClient;
  end;
  FNumberBox := TvgNumberBox.Create(Self);
  with FNumberBox do
  begin
    Parent := C;
    Stored := false;
    Locked := true;
    Align := vaVertCenter;
    Name := 'NumberBox';
    {$ifdef ks_compiler7_up}
    SetSubComponent(True);
    {$endif}
    {$ifdef FPC}
    SetSubComponent(True);
    {$endif}
    OnChange := DoChange;
  end;
end;

procedure TvgCompoundNumberBox.DoChange(Sender: TObject);
begin
  if Assigned(FOnChange) then
    FOnChange(Sender);
end;

function TvgCompoundNumberBox.GetValue: single;
begin
  Result := FNumberBox.Value;
end;

procedure TvgCompoundNumberBox.SetValue(const Value: single);
begin
  FNumberBox.Value := Value;
end;

{ TvgCompoundPopupBox }

constructor TvgCompoundPopupBox.Create(AOwner: TComponent);
var
  C: TvgControl;
begin
  inherited;
  Width := 200;
  Height := 21;
  FTextLabel := TvgLabel.Create(Self);
  with FTextLabel do
  begin
    Parent := Self;
    Stored := false;
    Locked := true;
    Align := vaLeft;
    Name := 'TextLabel';
    Text := 'Caption';
    Width := 70;
    TextAlign := vgTextAlignFar;
    WordWrap := false;
    Padding.right := 5;
    {$ifdef ks_compiler7_up}
    SetSubComponent(True);
    {$endif}
    {$ifdef FPC}
    SetSubComponent(True);
    {$endif}
  end;
  C := TvgControl.Create(Self);
  with C do
  begin
    Parent := Self;
    Stored := false;
    Locked := true;
    Align := vaClient;
  end;
  FPopupBox := TvgPopupBox.Create(Self);
  with FPopupBox do
  begin
    Parent := C;
    Stored := false;
    Locked := true;
    Align := vaVertCenter;
    Name := 'PopupBox';
    {$ifdef ks_compiler7_up}
    SetSubComponent(True);
    {$endif}
    {$ifdef FPC}
    SetSubComponent(True);
    {$endif}
    OnChange := DoChange;
  end;
end;

procedure TvgCompoundPopupBox.DoChange(Sender: TObject);
begin
  if Assigned(FOnChange) then
    FOnChange(Sender);
end;

function TvgCompoundPopupBox.GetItemIndex: integer;
begin
  Result := FPopupBox.ItemIndex;
end;

procedure TvgCompoundPopupBox.SetItemIndex(const Value: integer);
begin
  FPopupBox.ItemIndex := Value;
end;

{ TvgCompoundColorButton }

constructor TvgCompoundColorButton.Create(AOwner: TComponent);
var
  C: TvgControl;
begin
  inherited;
  Width := 200;
  Height := 21;
  FTextLabel := TvgLabel.Create(Self);
  with FTextLabel do
  begin
    Parent := Self;
    Stored := false;
    Locked := true;
    Align := vaLeft;
    Name := 'TextLabel';
    Text := 'Caption';
    Width := 70;
    TextAlign := vgTextAlignFar;
    WordWrap := false;
    Padding.right := 5;
    {$ifdef ks_compiler7_up}
    SetSubComponent(True);
    {$endif}
    {$ifdef FPC}
    SetSubComponent(True);
    {$endif}
  end;
  C := TvgControl.Create(Self);
  with C do
  begin
    Parent := Self;
    Stored := false;
    Locked := true;
    Align := vaLeft;
    Width := 60;
  end;
  FColorButton := TvgColorButton.Create(Self);
  with FColorButton do
  begin
    Parent := C;
    Stored := false;
    Locked := true;
    Align := vaVertCenter;
    Name := 'ColorButton';
    {$ifdef ks_compiler7_up}
    SetSubComponent(True);
    {$endif}
    {$ifdef FPC}
    SetSubComponent(True);
    {$endif}
    OnChange := DoChange;
  end;
end;

procedure TvgCompoundColorButton.DoChange(Sender: TObject);
begin
  if Assigned(FOnChange) then
    FOnChange(Sender);
end;

function TvgCompoundColorButton.GetValue: string;
begin
  Result := FColorButton.Color;
end;

procedure TvgCompoundColorButton.SetValue(const Value: string);
begin
  FColorButton.Color := Value;
end;

{ TvgCompoundImage }

constructor TvgCompoundImage.Create(AOwner: TComponent);
begin
  inherited;
  Width := 200;
  Height := 60;
  FTextLabel := TvgLabel.Create(Self);
  with FTextLabel do
  begin
    Parent := Self;
    Stored := false;
    Locked := true;
    Align := vaLeft;
    Name := 'TextLabel';
    Text := 'Caption';
    Width := 70;
    TextAlign := vgTextAlignFar;
    WordWrap := false;
    Padding.right := 5;
    {$ifdef ks_compiler7_up}
    SetSubComponent(True);
    {$endif}
    {$ifdef FPC}
    SetSubComponent(True);
    {$endif}
  end;
  FImage := TvgImageControl.Create(Self);
  with FImage do
  begin
    Parent := Self;
    Stored := false;
    Locked := true;
    Align := vaClient;
    Name := 'Image';
    {$ifdef ks_compiler7_up}
    SetSubComponent(True);
    {$endif}
    {$ifdef FPC}
    SetSubComponent(True);
    {$endif}
    OnChange := DoChange;
  end;
end;

procedure TvgCompoundImage.DoChange(Sender: TObject);
begin
  if Assigned(FOnChange) then
    FOnChange(Sender);
end;

initialization
  RegisterVGObjects('Ext. Controls', [TvgIPhoneButton, {TvgDockBar, }TvgDropTarget, TvgImageViewer]);
  RegisterVGObjects('Math', [TvgPlotGrid]);
  RegisterVGObjects('Compound', [TvgCompoundTrackBar, TvgCompoundAngleBar, TvgCompoundTextBox,
    TvgCompoundMemo, TvgCompoundNumberBox, TvgCompoundPopupBox, TvgCompoundColorButton,
    TvgCompoundImage, TvgCalendar]);
  RegisterVGObjects('Boxes', [TvgCalendarBox]);
  RegisterVGObjects('Text Edits', [TvgCalendarTextBox]);
end.


