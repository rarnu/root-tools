unit vg_colors;

{$I vg_define.inc}

interface

uses
  {$IFNDEF NOVCL}
  {$IFDEF UCL} UControls, {$ELSE} Controls, {$ENDIF}
  {$ENDIF}
  Classes, Variants, SysUtils, vg_objects, vg_scene, vg_controls, vg_textbox;

const
  colorPickSize = 10;

type

  TvgBitmapTrackBar = class(TvgTrackBar)
  private
    FBitmap: TvgBitmap;
    FBackground: TvgShape;
  protected
    procedure ApplyStyle; override;
    procedure FreeStyle; override;
    procedure Realign; override;
    procedure UpdateBitmap;
    procedure FillBitmap; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
  end;

  TvgHueTrackBar = class(TvgBitmapTrackBar)
  private
  protected
    procedure FillBitmap; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
  end;

  TvgAlphaTrackBar = class(TvgBitmapTrackBar)
  private
  protected
    procedure FillBitmap; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
  end;

  TvgBWTrackBar = class(TvgBitmapTrackBar)
  private
  protected
    procedure FillBitmap; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
  end;

  TvgColorBox = class(TvgControl)
  private
    FColor: TvgColor;
    procedure SetColor(const Value: TvgColor);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
    property Color: TvgColor read FColor write SetColor;
  published
  end;

  TvgColorQuad = class(TvgControl)
  private
    FColorBox: TvgColorBox;
    FColorBitmap: TvgBitmap;
    FHue: single;
    FSat: single;
    FLum: single;
    FOnChange: TNotifyEvent;
    FAlpha: single;
    procedure SetHue(const Value: single);
    procedure SetLum(const Value: single);
    procedure SetSat(const Value: single);
    procedure SetAlpha(const Value: single);
    procedure SetColorBox(const Value: TvgColorBox);
  protected
    procedure MouseMove(Shift: TShiftState; X, Y, Dx, Dy: single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: single); override;
    function GetAbsoluteRect: TvgRect; override;
    function pointInObject(X, Y: single): boolean; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
  published
    property Hue: single read FHue write SetHue;
    property Lum: single read FLum write SetLum;
    property Sat: single read FSat write SetSat;
    property Alpha: single read FAlpha write SetAlpha;
    property ColorBox: TvgColorBox read FColorBox write SetColorBox;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TvgColorPicker = class(TvgControl)
  private
    FHueBitmap: TvgBitmap;
    FHue: single;
    FColorQuad: TvgColorQuad;
    procedure SetHue(const Value: single);
    function GetColor: TvgColor;
    procedure SetColor(const Value: TvgColor);
  protected
    procedure MouseMove(Shift: TShiftState; X, Y, Dx, Dy: single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: single); override;
    function GetAbsoluteRect: TvgRect; override;
    function pointInObject(X, Y: single): boolean; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    property Color: TvgColor read GetColor write SetColor;
  published
    property Hue: single read FHue write SetHue;
    property ColorQuad: TvgColorQuad read FColorQuad write FColorQuad;
  end;

  TvgGradientEdit = class(TvgControl)
  private
    FBitmap: TvgBitmap;
    FGradient: TvgGradient;
    FCurrentPoint: integer;
    FCurrentPointInvisible: boolean;
    FMoving: boolean;
    FOnChange: TNotifyEvent;
    FOnSelectPoint: TNotifyEvent;
    FColorPicker: TvgColorPicker;
    procedure SetGradient(const Value: TvgGradient);
    function GetPointRect(const Point: integer): TvgRect;
    procedure DoChanged(Sender: TObject);
    procedure SetCurrentPoint(const Value: integer);
    procedure SetColorPicker(const Value: TvgColorPicker);
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: single); override;
    procedure MouseMove(Shift: TShiftState; X, Y, Dx, Dy: single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: single); override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    procedure UpdateGradient;
    property Gradient: TvgGradient read FGradient write SetGradient;
    property CurrentPoint: integer read FCurrentPoint write SetCurrentPoint;
  published
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnSelectPoint: TNotifyEvent read FOnSelectPoint write FOnSelectPoint;
    property ColorPicker: TvgColorPicker read FColorPicker write SetColorPicker;
  end;

  TvgColorPanel = class(TvgControl)
  private
    FOnChange: TNotifyEvent;
    FColorQuad: TvgColorQuad;
    FAlphaTrack: TvgAlphaTrackBar;
    FHueTrack: TvgHueTrackBar;
    FColorBox: TvgColorBox;
    FUseAlpha: boolean;
    function GetColor: string;
    procedure SetColor(const Value: string);
    procedure SetColorBox(const Value: TvgColorBox);
    procedure SetUseAlpha(const Value: boolean);
  protected
    procedure DoAlphaChange(Sender: TObject);
    procedure DoHueChange(Sender: TObject);
    procedure DoQuadChange(Sender: TObject);
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Color: string read GetColor write SetColor;
    property ColorBox: TvgColorBox read FColorBox write SetColorBox;
    property UseAlpha: boolean read FUseAlpha write SetUseAlpha default true;
  end;

  TvgComboColorBox = class(TvgControl)
  private
    FPopup: TvgPopup;
    FColorPanel: TvgColorPanel;
    FColorBox: TvgColorBox;
    FColorText: TvgTextBox;
    FPlacement: TvgPlacement;
    FOnChange: TNotifyEvent;
    function GetValue: string;
    procedure SetValue(const Value: string);
    function GetUseAlpha: boolean;
    procedure SetUseAlpha(const Value: boolean);
  protected
    procedure ApplyStyle; override;
    procedure DoContentPaint(Sender: TObject; const Canvas: TvgCanvas; const ARect: TvgRect);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: single); override;
    procedure ChangeParent; override;
    procedure DoColorChange(Sender: TObject); virtual;
    procedure DoTextChange(Sender: TObject); virtual;
    function GetData: Variant; override;
    procedure SetData(const Value: Variant); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DropDown;
  published
    property CanFocused default true;
    property DisableFocusEffect;
    property TabOrder;
    property Color: string read GetValue write SetValue;
    property UseAlpha: boolean read GetUseAlpha write SetUseAlpha default true;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TvgHudHueTrackBar = class(TvgHueTrackBar)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
  published
  end;

  TvgHudAlphaTrackBar = class(TvgAlphaTrackBar)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
  published
  end;

  TvgHudBWTrackBar = class(TvgBitmapTrackBar)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
  published
  end;

  TvgHudComboColorBox = class(TvgComboColorBox)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
  published
  end;

implementation {===============================================================}

{ TvgBitmapTrackBar }

constructor TvgBitmapTrackBar.Create(AOwner: TComponent);
begin
  inherited;
  FResource := 'trackbarstyle';
end;

destructor TvgBitmapTrackBar.Destroy;
begin
  if FBitmap <> nil then
    FreeAndNil(FBitmap);
  inherited;
end;

procedure TvgBitmapTrackBar.ApplyStyle;
var
  T: TvgObject;
begin
  inherited;
  T := FindResource('background');
  if (T <> nil) and (T is TvgShape) then
  begin
    FBackground := TvgShape(T);
    UpdateBitmap;
  end;
end;

procedure TvgBitmapTrackBar.FreeStyle;
begin
  FBackground := nil;
  inherited;
end;

procedure TvgBitmapTrackBar.Realign;
begin
  inherited;
  UpdateBitmap;
end;

procedure TvgBitmapTrackBar.UpdateBitmap;
begin
  if FBackground = nil then Exit;

  if FBitmap <> nil then
    if (FBitmap.Width <> Trunc(FBackground.Width)) or (FBitmap.Height <> Trunc(FBackground.Height)) then
    begin
      FreeAndNil(FBitmap);
    end;

  if FBitmap = nil then
  begin
    FBitmap := TvgBitmap.Create(Trunc(FBackground.Width), Trunc(FBackground.Height));
    FillBitmap;
  end;
  FBackground.Fill.Style := vgBrushBitmap;
  FBackground.Fill.Bitmap.Bitmap := FBitmap;

  Repaint;
end;

procedure TvgBitmapTrackBar.FillBitmap;
begin
end;

{ TvgHueTrackBar }

constructor TvgHueTrackBar.Create(AOwner: TComponent);
begin
  inherited;
  Max := 1;
  Value := 0.5;
end;

procedure TvgHueTrackBar.FillBitmap;
var
  i, j: integer;
begin
  for j := 0 to FBitmap.Height - 1 do
  begin
    for i := 0 to FBitmap.Width - 1 do
    begin
      if Orientation = vgHorizontal then
        FBitmap.Scanline[j][i] := vgCorrectColor(vgHSLtoRGB(i / FBitmap.Width, 0.9, 0.5))
      else
        FBitmap.Scanline[j][i] := vgCorrectColor(vgHSLtoRGB(j / FBitmap.Height, 0.9, 0.5));
    end;
  end;
end;

{ TvgAlphaTrackBar }

constructor TvgAlphaTrackBar.Create(AOwner: TComponent);
begin
  inherited;
  Max := 1;
  Value := 1;
end;

procedure TvgAlphaTrackBar.FillBitmap;
var
  i, j: integer;
begin
  for j := 0 to FBitmap.Height - 1 do
  begin
    for i := 0 to FBitmap.Width - 1 do
    begin
      if odd(i div 3) and not odd(j div 3) then
        FBitmap.Scanline[j][i] := vgCorrectColor($FFA0A0A0)
      else
      if not odd(i div 3) and odd(j div 3) then
        FBitmap.Scanline[j][i] := vgCorrectColor($FFA0A0A0)
      else
        FBitmap.Scanline[j][i] := vgCorrectColor($FFFFFFFF)
    end;
  end;
  if FBitmap.Canvas.BeginScene then
  begin
    FBitmap.Canvas.Fill.Style := vgBrushGradient;
    FBitmap.Canvas.Fill.Gradient.Points[0].Color := '$00FFFFFF';
    FBitmap.Canvas.Fill.Gradient.Points[1].Color := '$FFFFFFFF';
    FBitmap.Canvas.Fill.Gradient.StopPosition.Point := vgPoint(1, 0);
    FBitmap.Canvas.FillRect(vgRect(0, 0, FBitmap.Width, FBitmap.Height), 0, 0, [], 1);
    FBitmap.Canvas.EndScene;
  end;
end;

{ TvgBWTrackBar }

constructor TvgBWTrackBar.Create(AOwner: TComponent);
begin
  inherited;
  Max := 1;
  Value := 0.5;
end;

procedure TvgBWTrackBar.FillBitmap;
var
  i, j: integer;
  a: byte;
begin
  for j := 0 to FBitmap.Height - 1 do
  begin
    for i := 0 to FBitmap.Width - 1 do
    begin
      if Orientation = vgHorizontal then
        a := round((i / FBitmap.Width) * $FF)
      else
        a := round((j / FBitmap.Height) * $FF);
      FBitmap.Scanline[j][i] := vgCorrectColor(vgColor(a, a, a));
    end;
  end;
end;

{ TvgColorBox }

constructor TvgColorBox.Create(AOwner: TComponent);
begin
  inherited;
end;

procedure TvgColorBox.Paint;
var
  i, j: integer;
  SaveIndex: integer;
begin
  SaveIndex := Canvas.SaveCanvas;
  Canvas.IntersectClipRect(LocalRect);
  Canvas.Stroke.Style := vgBrushNone;
  Canvas.Fill.Style := vgBrushSolid;
  Canvas.Fill.SolidColor := $FFFFFFFF;
  Canvas.FillRect(LocalRect, 0, 0, AllCorners, AbsoluteOpacity);
  Canvas.Fill.SolidColor := $FFD3D3D3;
  for i := 0 to Trunc(Width / 5) + 1 do
    for j := 0 to Trunc(Height / 5) + 1 do
    begin
      if Odd(i + j) then
      begin
        Canvas.FillRect(vgRect(i * 5, j * 5, (i + 1) * 5, (j + 1) * 5), 0, 0, AllCorners, AbsoluteOpacity);
      end;
    end;
  Canvas.RestoreCanvas(SaveIndex);

  Canvas.Fill.Style := vgBrushSolid;
  Canvas.Fill.SolidColor := FColor;
  Canvas.FillRect(LocalRect, 0, 0, AllCorners, AbsoluteOpacity);
end;

procedure TvgColorBox.SetColor(const Value: TvgColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    Repaint;
  end;
end;

{ TvgColorQuad }

constructor TvgColorQuad.Create(AOwner: TComponent);
begin
  inherited;
  FAlpha := 1; 
  AutoCapture := true;
end;

destructor TvgColorQuad.Destroy;
begin
  if (FColorBitmap <> nil) then
    FColorBitmap.Free;
  inherited;
end;

function TvgColorQuad.GetAbsoluteRect: TvgRect;
begin
  Result := inherited GetAbsoluteRect;
  vgInflateRect(Result, colorPickSize + 1, colorPickSize + 1);
end;

function TvgColorQuad.pointInObject(X, Y: single): boolean;
var
  P: TvgPoint;
begin
  Result := false;
  P := AbsoluteToLocal(vgPoint(X, Y));
  if (P.X > -colorPickSize / 2) and (P.X < Width + colorPickSize / 2) and
     (P.Y > -colorPickSize / 2) and (P.Y < Height + colorPickSize / 2) then
  begin
    Result := true;
  end;
end;

procedure TvgColorQuad.MouseMove(Shift: TShiftState; X, Y, Dx, Dy: single);
begin
  inherited;
  if FPressed then
  begin
    if Height <> 0 then
      Lum := 1 - ((Y) / (Height));
    if Width <> 0 then
      Sat := ((X) / (Width));
  end;
end;

procedure TvgColorQuad.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: single);
begin
  if FPressed then
    MouseMove([ssLeft], X, Y, 0, 0);
  inherited;
end;

procedure TvgColorQuad.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FColorBox) then
    ColorBox := nil;
end;

procedure TvgColorQuad.Paint;
var
  i, j: integer;
  R: TvgRect;
begin
  if FColorBitmap = nil then
  begin
    FColorBitmap := TvgBitmap.Create(Trunc(Width), Trunc(Height));
    if FColorBitmap <> nil then
    begin
      for i := 0 to FColorBitmap.Width - 1 do
      begin
        for j := 0 to FColorBitmap.Height - 1 do
        begin
          FColorBitmap.Scanline[j][i] := vgCorrectColor(vgHSLtoRGB(FHue, i / FColorBitmap.Width, (1 - (j / FColorBitmap.Height))));
          {$ifdef FPC_BIG_ENDIAN}
          ReverseBytes(@FColorBitmap.Scanline[j][i], 4);
          {$endif}
        end;
      end;
    end;
  end;
  if FColorBitmap <> nil then
    Canvas.DrawBitmap(FColorBitmap, vgRect(0, 0, FColorBitmap.Width, FColorBitmap.Height),
      vgRect(0, 0, Width, Height), AbsoluteOpacity);
  { current }
  R := vgRect(FSat * (Width), (1 - FLum) * (Height),
    FSat * (Width), (1 - FLum) * (Height));
  vgInflateRect(R, colorPickSize / 2, colorPickSize / 2);
  Canvas.Stroke.Style := vgBrushSolid;
  Canvas.StrokeThickness := 1;
  Canvas.Stroke.SolidColor := $FF000000;
  Canvas.DrawEllipse(R, AbsoluteOpacity);
  vgInflateRect(R, -1, -1);
  Canvas.Stroke.SolidColor := $FFFFFFFF;
  Canvas.DrawEllipse(R, AbsoluteOpacity);
  vgInflateRect(R, -1, -1);
  Canvas.Fill.Style := vgBrushSolid;
  Canvas.Fill.SolidColor := vgHSLtoRGB(Hue, sat, Lum);
  Canvas.FillEllipse(R, AbsoluteOpacity);
end;

procedure TvgColorQuad.SetAlpha(const Value: single);
begin
  if FAlpha <> Value then
  begin
    FAlpha := Value;
    if FAlpha < 0 then FAlpha := 0;
    if FAlpha > 1 then FAlpha := 1;
    if FColorBox <> nil then
      FColorBox.Color := vgHSLtoRGB(Hue, Sat, Lum) and $FFFFFF or (Round(Alpha * $FF) shl 24);
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

procedure TvgColorQuad.SetHue(const Value: single);
begin
  if FHue <> Value then
  begin
    FHue := Value;
    if FHue < 0 then FHue := 0;
    if FHue > 1 then FHue := 1;
    if FColorBitmap <> nil then
      FreeAndNil(FColorBitmap);
    if FColorBox <> nil then
      FColorBox.Color := vgHSLtoRGB(Hue, Sat, Lum) and $FFFFFF or (Round(Alpha * $FF) shl 24);
    if Assigned(FOnChange) then
      FOnChange(Self);
    Repaint;
  end;
end;

procedure TvgColorQuad.SetLum(const Value: single);
begin
  if FLum <> Value then
  begin
    FLum := Value;
    if FLum < 0 then FLum := 0;
    if FLum > 1 then FLum := 1;
    if FColorBox <> nil then
      FColorBox.Color := vgHSLtoRGB(Hue, Sat, Lum) and $FFFFFF or (Round(Alpha * $FF) shl 24);
    if Assigned(FOnChange) then
      FOnChange(Self);
    Repaint;
  end;
end;

procedure TvgColorQuad.SetSat(const Value: single);
begin
  if FSat <> Value then
  begin
    FSat := Value;
    if FSat < 0 then FSat := 0;
    if FSat > 1 then FSat := 1;
    if FColorBox <> nil then
      FColorBox.Color := vgHSLtoRGB(Hue, Sat, Lum) and $FFFFFF or (Round(Alpha * $FF) shl 24);
    if Assigned(FOnChange) then
      FOnChange(Self);
    Repaint;
  end;
end;

procedure TvgColorQuad.SetColorBox(const Value: TvgColorBox);
begin
  if FColorBox <> Value then
  begin
    FColorBox := Value;
    if FColorBox <> nil then
      FColorBox.Color := vgHSLtoRGB(Hue, Sat, Lum) and $FFFFFF or (Round(Alpha * $FF) shl 24);
  end;
end;

{ TvgColorPicker ==============================================================}

constructor TvgColorPicker.Create(AOwner: TComponent);
begin
  inherited;
  AutoCapture := true;
end;

destructor TvgColorPicker.Destroy;
begin
  if (FHueBitmap <> nil) then
    FreeAndNil(FHueBitmap);
  inherited;
end;

procedure TvgColorPicker.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FColorQuad) then
    ColorQuad := nil;
end;

function TvgColorPicker.GetAbsoluteRect: TvgRect;
begin
  Result := inherited GetAbsoluteRect;
  vgInflateRect(Result, 0, colorPickSize / 2);
end;

function TvgColorPicker.pointInObject(X, Y: single): boolean;
var
  P: TvgPoint;
begin
  Result := false;
  P := AbsoluteToLocal(vgPoint(X, Y));
  if (P.X > 0) and (P.X < Width) and
     (P.Y > -colorPickSize / 2) and (P.Y < Height + colorPickSize / 2) then
  begin
    Result := true;
  end;
end;

procedure TvgColorPicker.MouseMove(Shift: TShiftState; X, Y, Dx,
  Dy: single);
begin
  inherited;
  if FPressed then
  begin
    if Height <> 0 then
      Hue := ((Y) / (Height));
  end;
end;

procedure TvgColorPicker.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: single);
begin
  if FPressed then
    MouseMove([ssLeft], X, Y, 0, 0);
  inherited;
end;

procedure TvgColorPicker.Paint;
var
  i, j: integer;
  R: TvgRect;
begin
  if FHueBitmap = nil then
  begin
    FHueBitmap := TvgBitmap.Create(Trunc(Width), Trunc(Height));
    if FHueBitmap <> nil then
    begin
      for j := 0 to FHueBitmap.Height - 1 do
      begin
        for i := 0 to FHueBitmap.Width - 1 do
        begin
          FHueBitmap.Scanline[j][i] := vgCorrectColor(vgHSLtoRGB(j / FHueBitmap.Height, 0.9, 0.5));
          {$ifdef FPC_BIG_ENDIAN}
          ReverseBytes(@FHueBitmap.Scanline[j][i], 4);
          {$endif}
        end;
      end;
    end;
  end;

  if FHueBitmap <> nil then
    Canvas.DrawBitmap(FHueBitmap, vgRect(0, 0, FHueBitmap.Width, FHueBitmap.Height),
      vgRect(0, 0, Width, Height), AbsoluteOpacity);

  { hue pos }
  R := vgRect(Width / 2, FHue * (Height),
    Width / 2, FHue * (Height));
  vgInflateRect(R, colorPickSize / 2, colorPickSize / 2);
//  vgOffsetRect(R, 01, StrokeThickness);
  Canvas.Stroke.Style := vgBrushSolid;
  Canvas.StrokeThickness := 1;
  Canvas.Stroke.SolidColor := $FF000000;
  Canvas.DrawEllipse(R, AbsoluteOpacity);
  vgInflateRect(R, -1, -1);
  Canvas.Stroke.SolidColor := $FFFFFFFF;
  Canvas.DrawEllipse(R, AbsoluteOpacity);
  vgInflateRect(R, -1, -1);
  Canvas.Fill.Style := vgBrushSolid;
  Canvas.Fill.SolidColor := vgHSLtoRGB(Hue, 0.9, 0.5);
  Canvas.FillEllipse(R, AbsoluteOpacity);
end;

function TvgColorPicker.GetColor: TvgColor;
begin
  Result := vgHSLToRGB(Hue,1,0.5)
end;

procedure TvgColorPicker.SetColor(const Value: TvgColor);
var
  H, S, L: single;
  SaveChange: TNotifyEvent;
begin
  vgRGBtoHSL(Value, H, S, L);
  Hue := H;
  if FColorQuad <> nil then
  begin
    FColorQuad.Alpha := TvgColorRec(Value).A / $FF;
    FColorQuad.Hue := H;
    FColorQuad.Sat := S;
    FColorQuad.Lum := L;
  end;
end;

procedure TvgColorPicker.SetHue(const Value: single);
begin
  if FHue <> Value then
  begin
    FHue := Value;
    if FHue < 0 then FHue := 0;
    if FHue > 1 then FHue := 1;
    if FColorQuad <> nil then
      FColorQuad.Hue := FHue;
    Repaint;
  end;
end;

{ TvgGradientEdit ==============================================================}

constructor TvgGradientEdit.Create(AOwner: TComponent);
begin
  inherited;
  FGradient := TvgGradient.Create;
  FGradient.OnChanged := DoChanged;
  Width := 200;
  Height := 20;
  AutoCapture := true;
end;

destructor TvgGradientEdit.Destroy;
begin
  if FBitmap <> nil then
    FreeAndNil(FBitmap);
  FGradient.Free;
  inherited;
end;

function TvgGradientEdit.GetPointRect(const Point: integer): TvgRect;
begin
  if (Point >= 0) and (Point < FGradient.Points.Count) then
  with FGradient do
  begin
    Result := vgRect(0 + colorPickSize + (Points[Point].Offset * (Width - ((0 + colorPickSize) * 2))), Height - 0 - colorPickSize,
      0 + colorPickSize + (Points[Point].Offset * (Width - ((0 + colorPickSize) * 2))), Height - 0);
    vgInflateRect(Result, colorPickSize / 2, 0);
  end
  else
    Result := vgRect(0, 0, 0, 0);
end;

procedure TvgGradientEdit.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: single);
var
  NewOffset: single;
  NewColor: TvgColor;
  i: integer;
begin
  inherited;
  FMoving := false;
  if Button = mbLeft then
  begin
    { select point }
    for i := 0 to FGradient.Points.Count - 1 do
      if vgPtInRect(vgPoint(X, Y), GetPointRect(i)) then
      begin
        CurrentPoint := i;
        if Assigned(OnSelectPoint) then
          OnSelectPoint(Self);
        FMoving := true;
        Repaint;
        Exit;
      end;
    { add new point }
    if (Y > 0) and (Y < Height - 0 - colorPickSize) then
    begin
      NewOffset := ((X - 0 - colorPickSize) / (Width - ((0 + colorPickSize)* 2)));
      if NewOffset < 0 then NewOffset := 0;
      if NewOffset > 1 then NewOffset := 1;
      NewColor := FGradient.InterpolateColor(NewOffset);
      for i := 1 to FGradient.Points.Count - 1 do
        if NewOffset < FGradient.Points[i].Offset then
          with TvgGradientPoint(FGradient.Points.Add) do
          begin
            Index := i;
            CurrentPoint := Index;
            IntColor := NewColor;
            Offset := NewOffset;
            Repaint;
            DoChanged(Self);
            Break;
          end;
    end;
  end;
end;

procedure TvgGradientEdit.MouseMove(Shift: TShiftState; X, Y, Dx,
  Dy: single);
begin
  inherited;
  if ssLeft in Shift then
  begin
    if FMoving then
    begin
      FCurrentPointInvisible := ((Y < -10) or (Y > Height + 10)) and (FGradient.Points.Count > 1) and
        (CurrentPoint <> 0) and (CurrentPoint <> FGradient.Points.Count - 1);
      { move }
      FGradient.Points[CurrentPoint].Offset := ((X - 0 - colorPickSize) / (Width - ((0 + colorPickSize) * 2)));
      if FGradient.Points[CurrentPoint].Offset < 0 then
        FGradient.Points[CurrentPoint].Offset := 0;
      if FGradient.Points[CurrentPoint].Offset > 1 then
        FGradient.Points[CurrentPoint].Offset := 1;
      { move right }
      if CurrentPoint < FGradient.Points.Count - 1 then
        if FGradient.Points[CurrentPoint].Offset > FGradient.Points[CurrentPoint + 1].Offset then
        begin
          FGradient.Points[CurrentPoint].Index := FGradient.Points[CurrentPoint].Index + 1;
          CurrentPoint := CurrentPoint + 1;
        end;
      { move left }
      if CurrentPoint > 0 then
        if FGradient.Points[CurrentPoint].Offset < FGradient.Points[CurrentPoint - 1].Offset then
        begin
          FGradient.Points[CurrentPoint].Index := FGradient.Points[CurrentPoint].Index - 1;
          CurrentPoint := CurrentPoint - 1;
        end;
      Repaint;
      DoChanged(Self);
    end;
  end;
end;

procedure TvgGradientEdit.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: single);
begin
  inherited;
  FCurrentPointInvisible := false;
  if FMoving then
  begin
    { delete }
    if (Y > Height + 10) and (FGradient.Points.Count > 1) then
    begin
      FGradient.Points.Delete(CurrentPoint);
      CurrentPoint := CurrentPoint - 1;
      if CurrentPoint < 0 then CurrentPoint := 0;
      Repaint;
      DoChanged(Self);
      FMoving := false;
      Exit;
    end;
  end;
  FMoving := false;
end;

procedure TvgGradientEdit.Paint;
var
  i, j: integer;
  R: TvgRect;
  SaveIndex: integer;
begin
//  Canvas.DrawRect(vgRect(0, 0, Width, Height));
  if FBitmap = nil then
  begin
    FBitmap := TvgBitmap.Create(Trunc(Width - (0 * 2)), Trunc(Height - (0 * 2) - colorPickSize));
  end;
  if FBitmap <> nil then
  begin
    for j := 0 to FBitmap.Height - 1 do
    begin
      for i := 0 to FBitmap.Width - 1 do
      begin
        FBitmap.Scanline[j][i] := vgCorrectColor(FGradient.InterpolateColor(i / FBitmap.Width));
        {$ifdef FPC_BIG_ENDIAN}
        ReverseBytes(@FBitmap.Scanline[j][i], 4);
        {$endif}
      end;
    end;
  end;

  { draw back }
  R := vgRect(0 + colorPickSize, 0, Width - 0 - colorPickSize, Height - 0 - colorPickSize);
  SaveIndex := Canvas.SaveCanvas;
  Canvas.IntersectClipRect(R);
  Canvas.Stroke.Style := vgBrushNone;
  Canvas.Fill.Style := vgBrushSolid;
  Canvas.Fill.SolidColor := $FFFFFFFF;
  Canvas.FillRect(R, 0, 0, AllCorners, AbsoluteOpacity);
  Canvas.Fill.SolidColor := $FFD3D3D3;
  for i := 0 to Trunc(Width / 10) + 1 do
    for j := 0 to Trunc(Height / 10) + 1 do
    begin
      if Odd(i + j) then
      begin
        Canvas.FillRect(vgRect(i * 10, j * 10, (i + 1) * 10, (j + 1) * 10), 0, 0, AllCorners, AbsoluteOpacity);
      end;
    end;
  Canvas.RestoreCanvas(SaveIndex);
  { draw gradient }
  Canvas.Stroke.Style := vgBrushSolid;
  Canvas.StrokeThickness := 0;
  if FBitmap <> nil then
  begin
    Canvas.DrawBitmap(FBitmap, vgRect(0, 0, FBitmap.Width, FBitmap.Height),
      vgRect(0 + colorPickSize, 0, Width - 0 - colorPickSize, Height - 0 - colorPickSize), AbsoluteOpacity);
  end;
  { points }
  for i := 0 to FGradient.Points.Count - 1 do
  begin
    if FCurrentPointInvisible and (i = CurrentPoint) then Continue;
    R := GetPointRect(i);
    vgInflateRect(R, -1, -1);
    Canvas.Stroke.SolidColor := $FF757575;
    Canvas.Fill.SolidColor := FGradient.Points[i].IntColor;
    Canvas.FillEllipse(R, AbsoluteOpacity);
    Canvas.DrawEllipse(R, AbsoluteOpacity);
    { color }
    if CurrentPoint = i then
    begin
      vgInflateRect(R, 1, 1);
      Canvas.Stroke.SolidColor := $FF000000;
      Canvas.Stroke.SolidColor := $FFFFFFFF;
      Canvas.DrawEllipse(R, AbsoluteOpacity);
    end;
  end;
end;

procedure TvgGradientEdit.SetGradient(const Value: TvgGradient);
begin
  FGradient.Assign(Value);
end;

procedure TvgGradientEdit.DoChanged(Sender: TObject);
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
  UpdateGradient;
end;

procedure TvgGradientEdit.SetCurrentPoint(const Value: integer);
begin
  if FCurrentPoint <> Value then
  begin
    FCurrentPoint := Value;
    if Assigned(OnSelectPoint) then
      OnSelectPoint(Self);
    if (FColorPicker <> nil) and (CurrentPoint >= 0) then
      FColorPicker.Color := Gradient.Points[CurrentPoint].IntColor;
  end;
end;

procedure TvgGradientEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FColorPicker) then
    ColorPicker := nil;
end;

procedure TvgGradientEdit.SetColorPicker(const Value: TvgColorPicker);
begin
  FColorPicker := Value;
  if (FColorPicker <> nil) and (CurrentPoint >= 0) then
    FColorPicker.Color := Gradient.Points[CurrentPoint].IntColor;
end;

procedure TvgGradientEdit.UpdateGradient;
begin
  if (FColorPicker <> nil) and (CurrentPoint >= 0) then
    FColorPicker.Color := Gradient.Points[CurrentPoint].IntColor;
end;

{ TvgColorPanel }

constructor TvgColorPanel.Create(AOwner: TComponent);
begin
  inherited;
  FUseAlpha := true;
  Width := 150;
  Height := 150;
  FAlphaTrack := TvgAlphaTrackBar.Create(Self);
  FAlphaTrack.Parent := Self;
  FAlphaTrack.Align := vaBottom;
  FAlphaTrack.Stored := false;
  FAlphaTrack.Locked := true;
  FAlphaTrack.Padding.Rect := vgRect(0, 0, 15, 0);
  FAlphaTrack.Height := 15;
  FAlphaTrack.DisableFocusEffect := true;
  FAlphaTrack.OnChange := DoAlphaChange;

  FHueTrack := TvgHueTrackBar.Create(Self);
  FHueTrack.Parent := Self;
  FHueTrack.Align := vaRight;
  FHueTrack.Stored := false;
  FHueTrack.Locked := true;
  FHueTrack.Padding.Rect := vgRect(0, 0, 0, 0);
  FHueTrack.Orientation := vgVertical;
  FHueTrack.Width := 15;
  FHueTrack.DisableFocusEffect := true;
  FHueTrack.OnChange := DoHueChange;

  FColorQuad := TvgColorQuad.Create(Self);
  FColorQuad.Parent := Self;
  FColorQuad.Align := vaClient;
  FColorQuad.Stored := false;
  FColorQuad.Locked := true;
  FColorQuad.Padding.Rect := vgRect(5, 5, 3, 3);
  FColorQuad.OnChange := DoQuadChange;

  Color := vcWhite;
end;

destructor TvgColorPanel.Destroy;
begin
  inherited;
end;

procedure TvgColorPanel.DoAlphaChange(Sender: TObject);
begin
  FColorQuad.Alpha := FAlphaTrack.Value;
end;

procedure TvgColorPanel.DoHueChange(Sender: TObject);
begin
  FColorQuad.Hue := FHueTrack.Value;
end;

procedure TvgColorPanel.DoQuadChange(Sender: TObject);
begin
  if FColorBox <> nil then
    FColorBox.Color := vgStrToColor(Color);
  if Assigned(OnChange) then
    OnChange(Self);
end;

function TvgColorPanel.GetColor: string;
begin
  Result := vgColorToStr(vgOpacity(vgHSLtoRGB(FColorQuad.Hue, FColorQuad.Sat, FColorQuad.Lum), FColorQuad.Alpha));
end;

procedure TvgColorPanel.Loaded;
begin
  inherited;
  Color := Color;
end;

procedure TvgColorPanel.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FColorBox) then
    ColorBox := nil;
end;

procedure TvgColorPanel.SetColor(const Value: string);
var
  H, S, L: single;
  C: TvgColor;
  SaveOnChange: TNotifyEvent;
begin
  SaveOnChange := FOnChange;
  FOnChange := nil;

  C := vgStrToColor(Value);
  vgRGBtoHSL(C, H, S, L);
  FColorQuad.Lum := L;
  FColorQuad.Sat := S;
  FHueTrack.Value := H;
  FAlphaTrack.Value := TvgColorRec(C).A / $FF;

  FOnChange := SaveOnChange;

  if not (csLoading in ComponentState) then
    DoQuadChange(Self);
end;

procedure TvgColorPanel.SetColorBox(const Value: TvgColorBox);
begin
  if FColorBox <> Value then
  begin
    FColorBox := Value;
    if FColorBox <> nil then
      FColorBox.Color := vgStrToColor(Color);
  end;
end;

procedure TvgColorPanel.SetUseAlpha(const Value: boolean);
begin
  if FUseAlpha <> Value then
  begin
    FUseAlpha := Value;
    FAlphaTrack.Visible := FUseAlpha;
  end;
end;

{ TvgComboColorBox }

constructor TvgComboColorBox.Create(AOwner: TComponent);
begin
  inherited;
  Width := 60;
  Height := 22;
  CanFocused := true;
  AutoCapture := true;
  FResource := 'comboboxstyle';
  FPopup := TvgPopup.Create(Self);
  FPopup.FResource := 'combopopupstyle';
  FPopup.PlacementTarget := Self;
  FPopup.StaysOpen := false;
  FPopup.Stored := false;
  FPopup.Parent := Self;
  FPopup.Locked := true;
  FPopup.DesignHide := true;
  FPopup.Width := 240;
  FPopup.Height := 160;
  FPopup.Margins.Rect := vgRect(5, 5, 5, 5);
  FColorBox := TvgColorBox.Create(Self);
  FColorBox.Width := 50;
  FColorBox.Parent := FPopup;
  FColorBox.Stored := false;
  FColorBox.Align := vaRight;
  FColorBox.Padding.Rect := vgRect(15, 70, 15, 30);
  FColorText := TvgTextBox.Create(Self);
  FColorText.Parent := FPopup;
  FColorText.Stored := false;
  FColorText.Locked := true;
  FColorText.FilterChar := '#0123456789abcdefABCDEF';
  FColorText.SetBounds(160, 20, 70, 22);
  FColorText.Align := vaTopRight;
  FColorText.DisableFocusEffect := true;
  FColorText.OnChange := DoTextChange;
  FColorPanel := TvgColorPanel.Create(Self);
  FColorPanel.Parent := FPopup;
  FColorPanel.Stored := false;
  FColorPanel.DisableFocusEffect := true;
  FColorPanel.Align := vaClient;
  FColorPanel.OnChange := DoColorChange;
  FColorPanel.ColorBox := FColorBox;
end;

destructor TvgComboColorBox.Destroy;
begin
  inherited;
end;

procedure TvgComboColorBox.DoTextChange(Sender: TObject);
var
  S: string;
begin
  try
    S := Color;
    Color := FColorText.Text;
  except
    Color := S;
  end;
end;

procedure TvgComboColorBox.DoColorChange(Sender: TObject);
begin
  FColorText.Text := Color;
  Repaint;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TvgComboColorBox.DropDown;
var
  i: integer;
begin
  if not FPopup.IsOpen then
  begin
    FPopup.Placement := FPlacement;
    FColorPanel.ApplyResource;
    FPopup.IsOpen := true;
  end
  else
  begin
    FPopup.IsOpen := false;
  end;
end;

procedure TvgComboColorBox.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: single);
begin
  inherited;
  if (Button = mbLeft) then
  begin
    DropDown;
  end;
end;

procedure TvgComboColorBox.ChangeParent;
begin
  inherited;
end;

function TvgComboColorBox.GetValue: string;
begin
  Result := FColorPanel.Color
end;

procedure TvgComboColorBox.SetValue(const Value: string);
begin
  FColorPanel.Color := Value;
end;

procedure TvgComboColorBox.ApplyStyle;
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

procedure TvgComboColorBox.DoContentPaint(Sender: TObject;
  const Canvas: TvgCanvas; const ARect: TvgRect);
var
  R: TvgRect;
  i, j, SaveIndex: integer;
begin
  R := ARect;
  vgInflateRect(R, -0.5 - 2, -0.5 - 2);
  { draw back }
  SaveIndex := Canvas.SaveCanvas;
  Canvas.IntersectClipRect(R);
  Canvas.Stroke.Style := vgBrushNone;
  Canvas.Fill.Style := vgBrushSolid;
  Canvas.Fill.SolidColor := $FFFFFFFF;
  Canvas.FillRect(R, 0, 0, AllCorners, AbsoluteOpacity);
  Canvas.Fill.SolidColor := $FFD3D3D3;
  for i := 0 to Trunc(vgRectWidth(R) / 5) + 1 do
    for j := 0 to Trunc(vgRectHeight(R) / 5) + 1 do
    begin
      if Odd(i + j) then
      begin
        Canvas.FillRect(vgRect(i * 5, j * 5, (i + 1) * 5, (j + 1) * 5), 0, 0, AllCorners, AbsoluteOpacity);
      end;
    end;
  { color }
  Canvas.RestoreCanvas(SaveIndex);
  Canvas.Fill.Style := vgBrushSolid;
  Canvas.Fill.Color := Color;
  Canvas.FillRect(R, 0, 0, AllCorners, AbsoluteOpacity);
  Canvas.Stroke.Color := vcBlack;
  Canvas.Stroke.Style := vgBrushSolid;
  Canvas.DrawRect(R, 0, 0, AllCorners, AbsoluteOpacity);
end;

function TvgComboColorBox.GetUseAlpha: boolean;
begin
  Result := FColorPanel.UseAlpha;
end;

procedure TvgComboColorBox.SetUseAlpha(const Value: boolean);
begin
  FColorPanel.UseAlpha := Value;
end;

function TvgComboColorBox.GetData: Variant;
begin
  Result := Color;
end;

procedure TvgComboColorBox.SetData(const Value: Variant);
begin
  if VarIsEvent(Value) then
    Onchange := VariantToEvent(Value)
  else
  if VarIsStr(Value) then
    Color := Value
  else
  if VarIsOrdinal(Value) then
    Color := vgColorToStr(Value);
end;

{ TvgHudHueTrackBar }

constructor TvgHudHueTrackBar.Create(AOwner: TComponent);
begin
  inherited;
  FResource := 'hudtrackbarstyle';
end;

{ TvgHudAlphaTrackBar }

constructor TvgHudAlphaTrackBar.Create(AOwner: TComponent);
begin
  inherited;
  FResource := 'hudtrackbarstyle';
end;

{ TvgHudBWTrackBar }

constructor TvgHudBWTrackBar.Create(AOwner: TComponent);
begin
  inherited;
  FResource := 'hudtrackbarstyle';
end;

{ TvgHudComboColorBox }

constructor TvgHudComboColorBox.Create(AOwner: TComponent);
begin
  inherited;
  FResource := 'hudcomboboxstyle';
  FPopup.FResource := 'hudcombopopupstyle';
end;

initialization
  RegisterVGObjects('Colors', [TvgHueTrackBar, TvgAlphaTrackBar, TvgBWTrackBar, TvgColorQuad, TvgColorPicker, TvgGradientEdit, TvgColorBox, TvgColorPanel, TvgComboColorBox]);
  RegisterVGObjects('HUD', [TvgHudHueTrackBar, TvgHudAlphaTrackBar, TvgHudBWTrackBar, TvgHudComboColorBox]);
end.


