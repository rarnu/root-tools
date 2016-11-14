{%MainUnit ../graphics.pp}
{******************************************************************************
                                     TCANVAS
 ******************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}

const
  csAllValid = [csHandleValid..csBrushValid];

{-----------------------------------------------}
{--  TCanvas.Draw --}
{-----------------------------------------------}
procedure TCanvas.Draw(X, Y: Integer; SrcGraphic: TGraphic);
var
  ARect: TRect;
begin
  if not Assigned(SrcGraphic) then exit;
  ARect:=Bounds(X,Y,SrcGraphic.Width,SrcGraphic.Height);
  StretchDraw(ARect,SrcGraphic);
end;

{-----------------------------------------------}
{--  TCanvas.DrawFocusRect --}
{-----------------------------------------------}
procedure TCanvas.DrawFocusRect(const ARect: TRect);
begin
  Changing;
  RequiredState([csHandleValid]);
  LCLIntf.DrawFocusRect(FHandle, ARect);
  Changed;
end;

{-----------------------------------------------}
{--  TCanvas.StretchDraw --}
{-----------------------------------------------}
procedure TCanvas.StretchDraw(const DestRect: TRect; SrcGraphic: TGraphic);
begin
  if not Assigned(SrcGraphic) then exit;
  Changing;
  RequiredState([csHandleValid]);
  SrcGraphic.Draw(Self, DestRect);
  Changed;
end;

{-----------------------------------------------}
{--  TCanvas.GetClipRect --}
{-----------------------------------------------}
function TCanvas.GetClipRect: TRect;
begin
  // return actual clipping rectangle
  if GetClipBox(FHandle, @Result) = ERROR then
    Result := Rect(0, 0, 2000, 2000);{Just in Case}
end;

procedure TCanvas.SetClipRect(const ARect: TRect);
var
  RGN: HRGN;
  LogicalRect: TRect;
begin
  inherited SetClipRect(ARect);
  if inherited GetClipping then
  begin
    // ARect is in logical coords. CreateRectRGN accepts device coords.
    // So we need to translate them
    LogicalRect := ARect;
    LPtoDP(Handle, LogicalRect, 2);
    with LogicalRect do
      RGN := CreateRectRGN(Left, Top, Right, Bottom);
    SelectClipRGN(Handle, RGN);
    DeleteObject(RGN);
  end;
end;

function TCanvas.GetClipping: Boolean;
var
  R: TRect;
begin
  Result := GetClipBox(FHandle, @R) > NullRegion;
end;

procedure TCanvas.SetClipping(const AValue: boolean);
begin
  inherited SetClipping(AValue);
  if AValue then
    SetClipRect(inherited GetClipRect)
  else
    SelectClipRGN(Handle, 0);
end;


{-----------------------------------------------}
{--  TCanvas.CopyRect --}
{-----------------------------------------------}
procedure TCanvas.CopyRect(const Dest: TRect; SrcCanvas: TCanvas;
  const Source: TRect);
var
  SH, SW, DH, DW: Integer;
Begin
  if SrcCanvas= nil then exit;

  SH := Source.Bottom - Source.Top;
  SW := Source.Right - Source.Left;
  if (SH=0) or (SW=0) then exit;
  DH := Dest.Bottom - Dest.Top;
  DW := Dest.Right - Dest.Left;
  if (Dh=0) or (DW=0) then exit;

  SrcCanvas.RequiredState([csHandleValid]);
  Changing;
  RequiredState([csHandleValid]);

  //DebugLn('TCanvas.CopyRect ',ClassName,' SrcCanvas=',SrcCanvas.ClassName,' ',
  //  ' Src=',Source.Left,',',Source.Top,',',SW,',',SH,
  //  ' Dest=',Dest.Left,',',Dest.Top,',',DW,',',DH);
  StretchBlt(FHandle, Dest.Left, Dest.Top, DW, DH,
    SrcCanvas.FHandle, Source.Left, Source.Top, SW, SH, CopyMode);
  Changed;
end;
{-----------------------------------------------}
{--  TCanvas.GetPixel --}
{-----------------------------------------------}
function TCanvas.GetPixel(X, Y: Integer): TColor;
begin
  RequiredState([csHandleValid]);
  Result := WidgetSet.DCGetPixel(FHandle, X, Y);
end;

{-----------------------------------------------}
{--  TCanvas.SetPixel --}
{-----------------------------------------------}
procedure TCanvas.SetPixel(X, Y: Integer; Value: TColor);
begin
  Changing;
  RequiredState([csHandleValid, csPenvalid]);
  WidgetSet.DCSetPixel(FHandle, X, Y, Value);
  Changed;
end;

{------------------------------------------------------------------------------
  procedure TCanvas.RealizeAutoRedraw;
 ------------------------------------------------------------------------------}
procedure TCanvas.RealizeAutoRedraw;
begin
  if FAutoRedraw and HandleAllocated then
    WidgetSet.DCRedraw(Handle);
end;

procedure TCanvas.RealizeAntialiasing;
begin
  if HandleAllocated then
  begin
    // do not call Changed, the content has not changed
    case FAntialiasingMode of
      amOn: WidgetSet.DCSetAntialiasing(FHandle, True);
      amOff: WidgetSet.DCSetAntialiasing(FHandle, False);
    else
      WidgetSet.DCSetAntialiasing(FHandle, Boolean(WidgetSet.GetLCLCapability(lcAntialiasingEnabledByDefault)) )
    end;
  end;
end;

{------------------------------------------------------------------------------
  Method:   TCanvas.CreateBrush
  Params:   None
  Returns:  Nothing

 ------------------------------------------------------------------------------}
procedure TCanvas.CreateBrush;
const
  HatchBrushes = [bsHorizontal, bsVertical, bsFDiagonal, bsBDiagonal, bsCross, bsDiagCross];
var
  OldHandle: HBRUSH;
begin
  OldHandle := SelectObject(FHandle, HGDIOBJ(Brush.Reference.Handle));
  if (OldHandle <> HBRUSH(Brush.Reference.Handle)) and (FSavedBrushHandle=0) then
    FSavedBrushHandle := OldHandle;
  Include(FState, csBrushValid);
  // do not use color for hatched brushes. windows cannot draw hatches when SetBkColor is called
  if ([Brush.Style] * HatchBrushes) = [] then
    SetBkColor(FHandle, TColorRef(Brush.GetColor));
  if Brush.Style = bsSolid then
    SetBkMode(FHandle, OPAQUE)
  else
    SetBkMode(FHandle, TRANSPARENT);
end;

{------------------------------------------------------------------------------
  Method:   TCanvas.CreatePen
  Params:   None
  Returns:  Nothing

 ------------------------------------------------------------------------------}
procedure TCanvas.CreatePen;
var
  OldHandle: HPEN;
  
const
  PenModes: array[TPenMode] of Integer =
  (
{pmBlack      } R2_BLACK,
{pmWhite      } R2_WHITE,
{pmNop        } R2_NOP,
{pmNot        } R2_NOT,
{pmCopy       } R2_COPYPEN,
{pmNotCopy    } R2_NOTCOPYPEN,
{pmMergePenNot} R2_MERGEPENNOT,
{pmMaskPenNot } R2_MASKPENNOT,
{pmMergeNotPen} R2_MERGENOTPEN,
{pmMaskNotPen } R2_MASKNOTPEN,
{pmMerge      } R2_MERGEPEN,
{pmNotMerge   } R2_NOTMERGEPEN,
{pmMask       } R2_MASKPEN,
{pmNotMask    } R2_NOTMASKPEN,
{pmXor        } R2_XORPEN,
{pmNotXor     } R2_NOTXORPEN
  );
begin
//DebugLn('[TCanvas.CreatePen] ',Classname,'  Self=',DbgS(Self)
// ,'  Pen=',DbgS(Pen));
  OldHandle := SelectObject(FHandle, HGDIOBJ(Pen.Reference.Handle));
  if (OldHandle <> HPEN(Pen.Reference.Handle)) and (FSavedPenHandle=0) then
    FSavedPenHandle := OldHandle;
  MoveTo(PenPos.X, PenPos.Y);
  Include(FState, csPenValid);
  SetROP2(FHandle, PenModes[Pen.Mode]);
end;

{------------------------------------------------------------------------------
  Method:   TCanvas.CreateFont
  Params:   None
  Returns:  Nothing

 ------------------------------------------------------------------------------}
procedure TCanvas.CreateFont;
var
  OldHandle: HFONT;
begin
  // The first time the font handle is selected, the default font handle
  // is returned. Save this font handle to restore it later in DeselectHandles.
  // The TFont will call DeleteObject itself, so we never need to call it.
  OldHandle := SelectObject(FHandle, HGDIOBJ(Font.Reference.Handle));
  //DebugLn(['TCanvas.CreateFont OldHandle=',dbghex(OldHandle),' Font.Handle=',dbghex(Font.Handle)]);
  if (OldHandle <> HFONT(Font.Reference.Handle)) and (FSavedFontHandle = 0) then
    FSavedFontHandle := OldHandle;
  Include(FState, csFontValid);
  SetTextColor(FHandle, TColorRef(Font.GetColor));
end;

{------------------------------------------------------------------------------
  procedure TCanvas.CreateRegion;
 ------------------------------------------------------------------------------}
procedure TCanvas.CreateRegion;
var
  OldHandle: HRGN;
begin
  OldHandle := SelectObject(FHandle, HGDIOBJ(Region.Reference.Handle));
  if (OldHandle <> HRGN(Region.Reference.Handle)) and (FSavedRegionHandle=0) then
    FSavedRegionHandle := OldHandle;
  Include(FState, csRegionValid);
end;

{------------------------------------------------------------------------------
  Method:   TCanvas.SetAutoReDraw
  Params:   Value
  Returns:  Nothing

 ------------------------------------------------------------------------------}
procedure TCanvas.SetAutoRedraw(Value : Boolean);
begin
  if FAutoRedraw=Value then exit;
  FAutoRedraw := Value;
  RealizeAutoRedraw;
end;

{------------------------------------------------------------------------------
  procedure TCanvas.SetInternalPenPos(const Value: TPoint);
 ------------------------------------------------------------------------------}
procedure TCanvas.SetInternalPenPos(const Value: TPoint);
begin
  inherited SetPenPos(Value);
end;

{------------------------------------------------------------------------------
  Method:   TCanvas.SetLazBrush
  Params:   Value
  Returns:  Nothing

 ------------------------------------------------------------------------------}
procedure TCanvas.SetLazBrush(Value : TBrush);
begin
  FLazBrush.Assign(Value);
end;

procedure TCanvas.SetPenPos(const AValue: TPoint);
begin
  MoveTo(AValue.X,AValue.Y);
end;

{------------------------------------------------------------------------------
  Method:   TCanvas.SetLazFont
  Params:   Value
  Returns:  Nothing

 ------------------------------------------------------------------------------}
procedure TCanvas.SetLazFont(Value : TFont);
begin
  FLazFont.Assign(Value);
end;

{------------------------------------------------------------------------------
  Method:   TCanvas.SetLazPen
  Params:   Value
  Returns:  Nothing

 ------------------------------------------------------------------------------}
procedure TCanvas.SetLazPen(Value : TPen);
begin
  FLazPen.Assign(Value);
end;

{------------------------------------------------------------------------------
  Method:   TCanvas.SetRegion
  Params:   Value
  Returns:  Nothing

 ------------------------------------------------------------------------------}
procedure TCanvas.SetRegion(Value: TRegion);
begin
  FRegion.Assign(Value);
end;

function TCanvas.DoCreateDefaultFont: TFPCustomFont;
begin
  Result:=TFont.Create;
end;

function TCanvas.DoCreateDefaultPen: TFPCustomPen;
begin
  Result:=TPen.Create;
end;

function TCanvas.DoCreateDefaultBrush: TFPCustomBrush;
begin
  Result:=TBrush.Create;
end;

procedure TCanvas.SetColor(x, y: integer; const Value: TFPColor);
begin
  Pixels[x,y]:=FPColorToTColor(Value);
end;

function TCanvas.GetColor(x, y: integer): TFPColor;
begin
  Result:=TColorToFPColor(Pixels[x,y]);
end;

procedure TCanvas.SetHeight(AValue: integer);
begin

end;

function TCanvas.GetHeight: integer;
var
  p: TPoint;
begin
  if HandleAllocated then begin
    GetDeviceSize(Handle,p);
    Result:=p.y;
  end else
    Result:=0;
end;

procedure TCanvas.SetWidth(AValue: integer);
begin

end;

function TCanvas.GetWidth: integer;
var
  p: TPoint;
begin
  if HandleAllocated then begin
    GetDeviceSize(Handle,p);
    Result:=p.x;
  end else
    Result:=0;
end;

procedure TCanvas.GradientFill(ARect: TRect; AStart, AStop: TColor;
  ADirection: TGradientDirection);
var
  RStart, RStop: Byte;
  GStart, GStop: Byte;
  BStart, BStop: Byte;
  RDiff, GDiff, BDiff: Integer;
  Count, I: Integer;
begin
  if IsRectEmpty(ARect) then
    Exit;

  RedGreenBlue(ColorToRGB(AStart), RStart, GStart, BStart);
  RedGreenBlue(ColorToRGB(AStop),  RStop,  GStop,  BStop);

  RDiff := RStop - RStart;
  GDiff := GStop - GStart;
  BDiff := BStop - BStart;

  if ADirection = gdVertical then
    Count := ARect.Bottom - ARect.Top
  else
    Count := ARect.Right - ARect.Left;

  Changing;
  for I := 0 to Count-1 do
  begin
    Pen.Color := RGBToColor(RStart + (i * RDiff) div Count,
                            GStart + (i * GDiff) div Count,
                            BStart + (i * BDiff) div Count);

    RequiredState([csHandleValid, csPenValid]);
    if ADirection = gdHorizontal
    then begin
      // draw top to bottom, because LineTo does not draw last pixel
      LCLIntf.MoveToEx(FHandle, ARect.Left+I, ARect.Top, nil);
      LCLIntf.LineTo(FHandle, ARect.Left+I, ARect.Bottom);
    end
    else begin
      // draw left to right, because LineTo does not draw last pixel
      LCLIntf.MoveToEx(FHandle, ARect.Left, ARect.Top+I, nil);
      LCLIntf.LineTo(FHandle, ARect.Right, ARect.Top+I);
    end;
  end;
  Changed;
end;

procedure TCanvas.DoLockCanvas;
begin
  if FLock=0 then InitializeCriticalSection(FLock);
  EnterCriticalSection(FLock);
  inherited DoLockCanvas;
end;

procedure TCanvas.DoUnlockCanvas;
begin
  LeaveCriticalSection(FLock);
  inherited DoUnlockCanvas;
end;

procedure TCanvas.DoTextOut(x, y: integer; Text: string);
begin
  TextOut(X,Y,Text);
end;

procedure TCanvas.DoGetTextSize(Text: string; var w, h: integer);
var
  TxtSize: TSize;
begin
  TxtSize:=TextExtent(Text);
  w:=TxtSize.cx;
  h:=TxtSize.cy;
end;

function TCanvas.DoGetTextHeight(Text: string): integer;
begin
  Result:=TextHeight(Text);
end;

function TCanvas.DoGetTextWidth(Text: string): integer;
begin
  Result:=TextWidth(Text);
end;

procedure TCanvas.DoRectangle(const Bounds: TRect);
begin
  Frame(Bounds);
end;

procedure TCanvas.DoRectangleFill(const Bounds: TRect);
begin
  FillRect(Bounds);
end;

procedure TCanvas.DoRectangleAndFill(const Bounds: TRect);
begin
  Rectangle(Bounds);
end;

procedure TCanvas.DoEllipse(const Bounds: TRect);
var
  x1: Integer;
  y1: Integer;
  x2: Integer;
  y2: Integer;
begin
  if Bounds.Left < Bounds.Right then
  begin
    x1 := Bounds.Left;
    x2 := Bounds.Right;
  end else
  begin
    x1 := Bounds.Right;
    x2 := Bounds.Left;
  end;
  if Bounds.Top < Bounds.Bottom then
  begin
    y1 := Bounds.Top;
    y2 := Bounds.Bottom;
  end else
  begin
    y1 := Bounds.Bottom;
    y2 := Bounds.Top;
  end;
  Arc(x1, y1, x2, y2, 0, 360*16);
end;

procedure TCanvas.DoEllipseFill(const Bounds: TRect);
begin
  Ellipse(Bounds);
end;

procedure TCanvas.DoEllipseAndFill(const Bounds: TRect);
begin
  inherited DoEllipseAndFill(Bounds);
end;

procedure TCanvas.DoPolygon(const Points: array of TPoint);
begin
  Polyline(Points);
end;

procedure TCanvas.DoPolygonFill(const Points: array of TPoint);
begin
  Polygon(Points);
end;

procedure TCanvas.DoPolygonAndFill(const Points: array of TPoint);
begin
  inherited DoPolygonAndFill(Points);
end;

procedure TCanvas.DoPolyline(const Points: array of TPoint);
begin
  Polyline(Points);
end;

procedure TCanvas.DoPolyBezier(Points: PPoint; NumPts: Integer;
  Filled: boolean; Continuous: boolean);
begin
  PolyBezier(Points,NumPts,Filled,Continuous);
end;

procedure TCanvas.DoFloodFill(x, y: integer);
begin
  FloodFill(x, y, Brush.Color, fsSurface);
end;

procedure TCanvas.DoMoveTo(x, y: integer);
begin
  RequiredState([csHandleValid]);
  if LCLIntf.MoveToEx(FHandle, X, Y, nil) then
    SetInternalPenPos(Point(X, Y));
end;

procedure TCanvas.DoLineTo(x, y: integer);
begin
  Changing;
  RequiredState([csHandleValid, csPenValid]);
  if LCLIntf.LineTo(FHandle, X, Y) then
    SetInternalPenPos(Point(X, Y));
  Changed;
end;

procedure TCanvas.DoLine(x1, y1, x2, y2: integer);
begin
  MoveTo(x1,y1);
  LineTo(x2,y2);
end;

procedure TCanvas.DoCopyRect(x, y: integer; SrcCanvas: TFPCustomCanvas;
  const SourceRect: TRect);
  
  procedure WarnNotSupported;
  begin

  end;
  
var
  SH: Integer;
  SW: Integer;
Begin
  if SrcCanvas=nil then exit;
  if SrcCanvas is TCanvas then begin
    SW := SourceRect.Right - SourceRect.Left;
    SH := SourceRect.Bottom - SourceRect.Top;
    if (SH=0) or (SW=0) then exit;
    CopyRect(Rect(x,y,x+SW,y+SH),TCanvas(SrcCanvas),SourceRect);
  end else begin
    WarnNotSupported;
  end;
end;

procedure TCanvas.DoDraw(x, y: integer; const Image: TFPCustomImage);
var
  LazImg: TLazIntfImage;
  BitmapHnd, DummyHnd: HBitmap;
begin
  if Image=nil then exit;

  BitmapHnd:=0;
  try
    if Image is TLazIntfImage
    then begin
      LazImg := TLazIntfImage(Image);
    end
    else begin
      LazImg := TLazIntfImage.Create(0,0,[]);
      RequiredState([csHandleValid]);
      LazImg.DataDescription := GetDescriptionFromDevice(Handle, 0, 0);
      LazImg.Assign(Image);
    end;
    LazImg.CreateBitmaps(BitmapHnd, DummyHnd, True);
    if BitmapHnd=0 then exit;

    Changing;
    RequiredState([csHandleValid]);
    StretchBlt(FHandle,x,y,LazImg.Width,LazImg.Height,
      BitmapHnd, 0,0,LazImg.Width,LazImg.Height, CopyMode);
    Changed;
  finally
    if Image <> LazImg then LazImg.Free;
    if BitmapHnd <> 0 then DeleteObject(BitmapHnd);
  end;
end;

procedure TCanvas.CheckHelper(AHelper: TFPCanvasHelper);
begin

end;

function TCanvas.GetDefaultColor(const ADefaultColorType: TDefaultColorType): TColor;
begin
  Result := clDefault;
end;

{------------------------------------------------------------------------------
  Method:   TCanvas.Arc
  Params:   ALeft, ATop, ARight, ABottom, angle1, angle2
  Returns:  Nothing

  Use Arc to draw an elliptically curved line with the current Pen.
  The angles angle1 and angle2 are 1/16th of a degree. For example, a full
  circle equals 5760 (16*360). Positive values of Angle and AngleLength mean
  counter-clockwise while negative values mean clockwise direction.
  Zero degrees is at the 3'o clock position.

 ------------------------------------------------------------------------------}
procedure TCanvas.Arc(ALeft, ATop, ARight, ABottom,
  Angle16Deg, Angle16DegLength: Integer);
begin
  Changing;
  RequiredState([csHandleValid, csPenValid]);
  LCLIntf.Arc(FHandle, ALeft, ATop, ARight, ABottom, Angle16Deg, Angle16DegLength);
  Changed;
end;

procedure TCanvas.ArcTo(ALeft, ATop, ARight, ABottom, SX, SY, EX, EY: Integer);
var
  r: TRect;
begin
  r:=Rect(ALeft, ATop, ARight, ABottom);
  LineTo(RadialPoint(EccentricAngle(Point(SX, SY), r), r));
  Arc(ALeft, ATop, ARight, ABottom, SX, SY, EX, EY);
  MoveTo(RadialPoint(EccentricAngle(Point(EX, EY), r), r));
end;

procedure TCanvas.AngleArc(X, Y: Integer; Radius: Longword; StartAngle, SweepAngle: Single);
var x1, y1, x2, y2: integer;
begin
  x1:=trunc(x+cos(pi*StartAngle/180)*Radius);
  y1:=trunc(y-sin(pi*StartAngle/180)*Radius);
  x2:=trunc(x+cos(pi*(StartAngle+SweepAngle)/180)*Radius);
  y2:=trunc(y-sin(pi*(StartAngle+SweepAngle)/180)*Radius);
  LineTo(x1,y1);
  if SweepAngle>0 then
    Arc(x-Radius, y-Radius, x+Radius, y+Radius, x1, y1, x2, y2)
  else
    Arc(x-Radius, y-Radius, x+Radius, y+Radius, x2, y2, x1, y1);
  MoveTo(x2,y2);
end;

{------------------------------------------------------------------------------
  Method:   TCanvas.Arc
  Params:   ALeft, ATop, ARight, ABottom, sx, sy, ex, ey
  Returns:  Nothing

  Use Arc to draw an elliptically curved line with the current Pen. The
  values sx,sy, and ex,ey represent the starting and ending radial-points
  between which the Arc is drawn.

------------------------------------------------------------------------------}
procedure TCanvas.Arc(ALeft, ATop, ARight, ABottom, SX, SY, EX, EY: Integer);
begin
  Changing;
  RequiredState([csHandleValid, csBrushValid, csPenValid]);
  LCLIntf.RadialArc(FHandle, ALeft, ATop, ARight, ABottom, sx, sy, ex, ey);
  Changed;
end;

{------------------------------------------------------------------------------
  Method:   TCanvas.BrushCopy
  Params:   ADestRect, ABitmap, ASourceRect, ATransparentColor
  Returns:  Nothing

  Makes a stretch draw operation while substituting a color of the source bitmap
  with the color of the brush of the canvas
 ------------------------------------------------------------------------------}
procedure TCanvas.BrushCopy(ADestRect: TRect; ABitmap: TBitmap; ASourceRect: TRect;
  ATransparentColor: TColor);
var
  lIntfImage: TLazIntfImage;
  lTransparentColor, lBrushColor, lPixelColor: TFPColor;
  lPaintedBitmap: TBitmap;
  x, y: Integer;
  lSrcWidth, lSrcHeight: Integer;
begin
  // Preparation of data
  //lDestWidth := ADestRect.Right - ADestRect.Left;
  //lDestHeight := ADestRect.Bottom - ADestRect.Top;
  lSrcWidth := ASourceRect.Right - ASourceRect.Left;
  lSrcHeight := ASourceRect.Bottom - ASourceRect.Top;
  lTransparentColor := TColorToFPColor(ColorToRGB(ATransparentColor));
  lBrushColor := TColorToFPColor(ColorToRGB(Brush.Color));

  lPaintedBitmap := TBitmap.Create;
  lIntfImage := TLazIntfImage.Create(0, 0);
  try
    // First copy the source rectangle to another bitmap
    // So that we don't have to iterate in pixels which wont be used changing the color
    lPaintedBitmap.Width := lSrcWidth;
    lPaintedBitmap.Height := lSrcHeight;
    lPaintedBitmap.Canvas.Draw(-ASourceRect.Left, -ASourceRect.Top, ABitmap);

    // Next copy the bitmap to a intfimage to be able to make the color change
    lIntfImage.LoadFromBitmap(lPaintedBitmap.Handle, 0);
    for x := 0 to lSrcWidth-1 do
      for y := 0 to lSrcHeight-1 do
      begin
        lPixelColor := lIntfImage.Colors[x, y];
        if (lPixelColor.red = lTransparentColor.red) and
           (lPixelColor.green = lTransparentColor.green) and
           (lPixelColor.blue = lTransparentColor.blue) then
           lIntfImage.Colors[x, y] := lBrushColor;
      end;

    // Now obtain a bitmap with the new image
    lPaintedBitmap.LoadFromIntfImage(lIntfImage);

    // And stretch draw it
    Self.StretchDraw(ADestRect, lPaintedBitmap);
  finally
    lIntfImage.Free;
    lPaintedBitmap.Free;
  end;
end;

{------------------------------------------------------------------------------
  Method:   TCanvas.RadialPie
  Params:   x1, y1, x2, y2, StartAngle16Deg, EndAngle16Deg: Integer
  Returns:  Nothing

  Use RadialPie to draw a filled pie-shaped wedge on the canvas.
  The angles StartAngle16Deg and EndAngle16Deg are 1/16th of a degree.
  For example, a full circle equals 5760 (16*360).
  Positive values of Angle and AngleLength mean
  counter-clockwise while negative values mean clockwise direction.
  Zero degrees is at the 3'o clock position.

 ------------------------------------------------------------------------------}
procedure TCanvas.RadialPie(x1, y1, x2, y2,
  StartAngle16Deg, Angle16DegLength: Integer);
begin
  Changing;
  RequiredState([csHandleValid, csBrushValid, csPenValid]);
  LCLIntf.RadialPie(FHandle, x1, y1, x2, y2, StartAngle16Deg,Angle16DegLength);
  Changed;
end;

{------------------------------------------------------------------------------
  Method:   TCanvas.Pie
  Params:   EllipseX1, EllipseY1, EllipseX2, EllipseY2,
            StartX, StartY, EndX, EndY
  Returns:  Nothing

  Use Pie to draw a filled Pie-shaped wedge on the canvas. The pie is part of
  an ellipse between the points EllipseX1, EllipseY1, EllipseX2, EllipseY2.
  The values StartX, StartY and EndX, EndY represent the starting and ending
  radial-points between which the Bounding-Arc is drawn.

------------------------------------------------------------------------------}
procedure TCanvas.Pie(EllipseX1, EllipseY1, EllipseX2, EllipseY2,
  StartX, StartY, EndX, EndY: Integer);
begin
  Changing;
  RequiredState([csHandleValid, csBrushValid, csPenValid]);
  LCLIntf.Pie(FHandle,EllipseX1,EllipseY1,EllipseX2,EllipseY2,
              StartX,StartY,EndX,EndY);
  Changed;
end;

{------------------------------------------------------------------------------
  Method:  TCanvas.PolyBezier
  Params:  Points, Filled, Continous
  Returns: Boolean

  Use Polybezier to draw cubic Bézier curves. The first curve is drawn from the
  first point to the fourth point with the second and third points being the
  control points. If the Continuous flag is TRUE then each subsequent curve
  requires three more points, using the end-point of the previous Curve as its
  starting point, the first and second points being used as its control points,
  and the third point its end-point. If the continous flag is set to FALSE,
  then each subsequent Curve requires 4 additional points, which are used
  exactly as in the first curve. Any additonal points which do not add up to
  a full bezier(4 for Continuous, 3 otherwise) are ignored. There must be at
  least 4 points for an drawing to occur. If the Filled Flag is set to TRUE
  then the resulting Poly-Bézier will be drawn as a Polygon.

 ------------------------------------------------------------------------------}
procedure TCanvas.PolyBezier(const Points: array of TPoint;
  Filled: boolean = False;
  Continuous: boolean = False);
var NPoints, i: integer;
  PointArray: ^TPoint;
begin
  NPoints:=High(Points)-Low(Points)+1;
  if NPoints<=0 then exit;
  GetMem(PointArray,SizeOf(TPoint)*NPoints);
  for i:=0 to NPoints-1 do
    PointArray[i]:=Points[i+Low(Points)];
  PolyBezier(PointArray, NPoints, Filled, Continuous);
  FreeMem(PointArray);
end;

procedure TCanvas.PolyBezier(Points: PPoint; NumPts: Integer;
  Filled: boolean = False;
  Continuous: boolean = False);
begin
  Changing;
  RequiredState([csHandleValid, csBrushValid, csPenValid]);
  LCLIntf.PolyBezier(FHandle,Points,NumPts,Filled, Continuous);
  Changed;
end;


{------------------------------------------------------------------------------
  Method:   TCanvas.Polygon
  Params:   Points: array of TPoint; Winding: Boolean = False;
            StartIndex: Integer = 0; NumPts: Integer = -1
  Returns:  Nothing

  Use Polygon to draw a closed, many-sided shape on the canvas, using the value
  of Pen. After drawing the complete shape, Polygon fills the shape using the
  value of Brush.
  The Points parameter is an array of points that give the vertices of the
  polygon.
  Winding determines how the polygon is filled. When Winding is True, Polygon
  fills the shape using the Winding fill algorithm. When Winding is False,
  Polygon uses the even-odd (alternative) fill algorithm.
  StartIndex gives the index of the first point in the array to use. All points
  before this are ignored.
  NumPts indicates the number of points to use, starting at StartIndex.
  If NumPts is -1 (the default), Polygon uses all points from StartIndex to the
  end of the array.
  The first point is always connected to the last point.
  To draw a polygon on the canvas, without filling it, use the Polyline method,
  specifying the first point a second time at the end.
}
procedure TCanvas.Polygon(const Points: array of TPoint; Winding: Boolean;
  StartIndex: Integer; NumPts: Integer);
var
  NPoints: integer;
begin
  if NumPts < 0 then
    NPoints := High(Points) - StartIndex + 1
  else
    NPoints := NumPts;
  if NPoints <= 0 then Exit;
  Polygon(@Points[StartIndex], NPoints, Winding);
end;

procedure TCanvas.Polygon(Points: PPoint; NumPts: Integer;
  Winding: boolean = False);
begin
  if NumPts <= 0 then Exit;
  Changing;
  RequiredState([csHandleValid, csBrushValid, csPenValid]);
  LCLIntf.Polygon(FHandle, Points, NumPts, Winding);
  Changed;
end;

{------------------------------------------------------------------------------
  Method:   TCanvas.Polygon
  Params:   Points
  Returns:  Nothing

 ------------------------------------------------------------------------------}
procedure TCanvas.Polygon(const Points: array of TPoint);
begin
  Polygon(Points, True, Low(Points), High(Points) - Low(Points) + 1);
end;

{------------------------------------------------------------------------------
  Method:   TCanvas.Polyline
  Params:   Points: array of TPoint;
            StartIndex: Integer = 0; NumPts: Integer = -1
  Returns:  Nothing

  Use Polyline to connect a set of points on the canvas. If you specify only two
  points, Polyline draws a single line.
  The Points parameter is an array of points to be connected.
  StartIndex identifies the first point in the array to use.
  NumPts indicates the number of points to use. If NumPts is -1 (the default),
  PolyLine uses all the points from StartIndex to the end of the array.
  Calling the MoveTo function with the value of the first point, and then
  repeatedly calling LineTo with all subsequent points will draw the same image
  on the canvas. However, unlike LineTo, Polyline does not change the value of
  PenPos.
}
procedure TCanvas.Polyline(const Points: array of TPoint; StartIndex: Integer;
  NumPts: Integer);
var
  NPoints : integer;
begin
  if NumPts<0 then
    NPoints:=High(Points)-StartIndex+1
  else
    NPoints:=NumPts;
  if NPoints<=0 then exit;
  Polyline(@Points[StartIndex], NPoints);
end;

procedure TCanvas.Polyline(Points: PPoint; NumPts: Integer);
begin
  Changing;
  RequiredState([csHandleValid, csPenValid]);
  LCLIntf.Polyline(FHandle,Points,NumPts);
  Changed;
end;

{------------------------------------------------------------------------------
  Method:   TCanvas.Polyline
  Params:   Points
  Returns:  Nothing

 ------------------------------------------------------------------------------}
procedure TCanvas.Polyline(const Points: array of TPoint);
begin
  Polyline(Points, Low(Points), High(Points) - Low(Points) + 1);
end;

{------------------------------------------------------------------------------
  Method:   TCanvas.Ellipse
  Params:   X1, Y1, X2, Y2
  Returns:  Nothing

  Use Ellipse to draw a filled circle or ellipse on the canvas.

 ------------------------------------------------------------------------------}
procedure TCanvas.Ellipse(x1, y1, x2, y2: Integer);
begin
  Changing;
  RequiredState([csHandleValid, csBrushValid, csPenValid]);
  LCLIntf.Ellipse(FHandle,x1,y1,x2,y2);
  Changed;
end;

{------------------------------------------------------------------------------
  Method:   TCanvas.Ellipse
  Params:   ARect: TRect
  Returns:  Nothing

  Use Ellipse to draw a filled circle or ellipse on the canvas.

 ------------------------------------------------------------------------------}
procedure TCanvas.Ellipse(const ARect: TRect);
begin
  Ellipse(ARect.Left,ARect.Top,ARect.Right,ARect.Bottom);
end;

{------------------------------------------------------------------------------
  Method:   TCanvas.FillRect
  Params:   ARect
  Returns:  Nothing

 ------------------------------------------------------------------------------}
procedure TCanvas.FillRect(const ARect : TRect);
begin
  Changing;
  RequiredState([csHandleValid, csBrushValid]);
  LCLIntf.FillRect(FHandle, ARect, HBRUSH(Brush.Reference.Handle));
  Changed;
end;

{------------------------------------------------------------------------------
  procedure TCanvas.FillRect(X1,Y1,X2,Y2 : Integer);
 ------------------------------------------------------------------------------}
procedure TCanvas.FillRect(X1,Y1,X2,Y2 : Integer);
begin
  FillRect(Rect(X1,Y1,X2,Y2));
end;

{------------------------------------------------------------------------------
  Method:   TCanvas.FillRect
  Params:   X, Y: Integer; Color: TColor; FillStyle: TFillStyle
  Returns:  Nothing
  

 ------------------------------------------------------------------------------}
procedure TCanvas.FloodFill(X, Y: Integer; FillColor: TColor;
  FillStyle: TFillStyle);
begin
  Changing;
  RequiredState([csHandleValid, csBrushValid]);
  LCLIntf.FloodFill(FHandle, X, Y, FillColor, FillStyle, HBRUSH(Brush.Reference.Handle));
  Changed;
end;

{------------------------------------------------------------------------------
  Method:   TCanvas.Frame3d
  Params:   Rect
  Returns:  the inflated rectangle (the inner rectangle without the frame)

 ------------------------------------------------------------------------------}
procedure TCanvas.Frame3d(var ARect: TRect; const FrameWidth : integer;
  const Style : TGraphicsBevelCut);
begin
  Changing;
  RequiredState([csHandleValid,csBrushValid,csPenValid]);
  LCLIntf.Frame3d(FHandle, ARect, FrameWidth, Style);
  Changed;
end;

{------------------------------------------------------------------------------
  Method:   TCanvas.Frame3D
  Params:   Rect
  Returns:  the inflated rectangle (the inner rectangle without the frame)

 ------------------------------------------------------------------------------}
procedure TCanvas.Frame3D(var ARect: TRect; TopColor, BottomColor: TColor;
  const FrameWidth: integer);
var
  W, ii : Integer;
begin
  if ARect.Bottom-ARect.Top > ARect.Right-ARect.Left
  then
    W := ARect.Right-ARect.Left+1
  else
    W := ARect.Bottom-ARect.Top+1;

  if FrameWidth > W then
    W := W-1
  else
    W := FrameWidth;

  for ii := 1 to W do
  begin
    Pen.Color := TopColor;
    MoveTo(ARect.Left,    ARect.Bottom-1);
    LineTo(ARect.Left,    ARect.Top);
    LineTo(ARect.Right-1, ARect.Top);
    Pen.Color := BottomColor;
    LineTo(ARect.Right-1, ARect.Bottom-1);
    LineTo(ARect.Left,    ARect.Bottom-1);

    Inc(ARect.Left);
    Inc(ARect.Top);
    Dec(ARect.Right);
    Dec(ARect.Bottom);
  end;
end;

{------------------------------------------------------------------------------
  procedure TCanvas.Frame(const ARect: TRect);

  Drawing the border of a rectangle with the current pen
 ------------------------------------------------------------------------------}
procedure TCanvas.Frame(const ARect: TRect);
var
  OldBrushStyle: TFPBrushStyle;
begin
  Changing;
  RequiredState([csHandleValid, csPenValid]);
  OldBrushStyle := Brush.Style;
  Brush.Style := bsClear;
  Rectangle(ARect);
  Brush.Style := OldBrushStyle;
  Changed;
end;

{------------------------------------------------------------------------------
  procedure TCanvas.Frame(const ARect: TRect);

  Drawing the border of a rectangle with the current pen
 ------------------------------------------------------------------------------}
procedure TCanvas.Frame(X1, Y1, X2, Y2: Integer);
begin
  Frame(Rect(X1, Y1, X2, Y2));
end;

{------------------------------------------------------------------------------
  procedure TCanvas.FrameRect(const ARect: TRect);

  Drawing the border of a rectangle with the current brush
 ------------------------------------------------------------------------------}
procedure TCanvas.FrameRect(const ARect: TRect);
begin
  Changing;
  RequiredState([csHandleValid, csBrushValid]);
  LCLIntf.FrameRect(FHandle, ARect, Brush.GetHandle);
  Changed;
end;

{------------------------------------------------------------------------------
  procedure TCanvas.FrameRect(const ARect: TRect);

  Drawing the border of a rectangle with the current brush
 ------------------------------------------------------------------------------}
procedure TCanvas.FrameRect(X1, Y1, X2, Y2: Integer);
begin
  FrameRect(Rect(X1, Y1, X2, Y2));
end;

function TCanvas.GetTextMetrics(out TM: TLCLTextMetric): boolean;
var
  TTM: TTextMetric;
begin
  RequiredState([csHandleValid, csFontValid]); // csFontValid added in patch from bug 17555
  Fillchar(TM, SizeOf(TM), 0);
  Result := LCLIntf.GetTextMetrics(FHandle, TTM);
  if Result then begin
    TM.Ascender := TTM.tmAscent;
    TM.Descender := TTM.tmDescent;
    TM.Height := TTM.tmHeight;
  end;
end;

{------------------------------------------------------------------------------
  Method:   TCanvas.Rectangle
  Params:   X1,Y1,X2,Y2
  Returns:  Nothing

 ------------------------------------------------------------------------------}
procedure TCanvas.Rectangle(X1,Y1,X2,Y2 : Integer);
begin
  Changing;
  RequiredState([csHandleValid, csBrushValid, csPenValid]);
  LCLIntf.Rectangle(FHandle, X1, Y1, X2, Y2);
  Changed;
end;

{------------------------------------------------------------------------------
  Method:   TCanvas.Rectangle
  Params:   Rect
  Returns:  Nothing

 ------------------------------------------------------------------------------}
procedure TCanvas.Rectangle(const ARect: TRect);
begin
  Rectangle(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);
end;

{------------------------------------------------------------------------------
  Method:   TCanvas.RoundRect
  Params:   X1, Y1, X2, Y2, RX, RY
  Returns:  Nothing

 ------------------------------------------------------------------------------}
procedure TCanvas.RoundRect(X1, Y1, X2, Y2: Integer; RX,RY : Integer);
begin
  Changing;
  RequiredState([csHandleValid, csBrushValid, csPenValid]);
  LCLIntf.RoundRect(FHandle, X1, Y1, X2, Y2, RX, RY);
  Changed;
end;

{------------------------------------------------------------------------------
  Method:   TCanvas.RoundRect
  Params:   Rect, RX, RY
  Returns:  Nothing

 ------------------------------------------------------------------------------}
procedure TCanvas.RoundRect(const Rect : TRect; RX,RY : Integer);
begin
  RoundRect(Rect.Left, Rect.Top, Rect.Right, Rect.Bottom, RX, RY);
end;

{------------------------------------------------------------------------------
  Method:   TCanvas.TextRect
  Params:   ARect, X, Y, Text
  Returns:  Nothing

 ------------------------------------------------------------------------------}
procedure TCanvas.TextRect(const ARect: TRect; X, Y: integer; const Text: string
  );
begin
  TextRect(ARect,X,Y,Text,TextStyle);
end;

{------------------------------------------------------------------------------
  Method:   TCanvas.TextRect
  Params:   ARect, X, Y, Text, Style
  Returns:  Nothing

 ------------------------------------------------------------------------------}
procedure TCanvas.TextRect(ARect: TRect; X, Y: integer; const Text: string;
  const Style: TTextStyle);
var
  Options : Longint;
  fRect : TRect;
  DCIndex: Integer;
  DC: HDC;
  ReqState: TCanvasState;

  procedure SaveState;
  begin
    if DCIndex<>0 then exit;
    DCIndex:=SaveDC(DC);
  end;
  
  procedure RestoreState;
  begin
    if DCIndex=0 then exit;
    RestoreDC(DC,DCIndex);
  end;
  
begin
  //debugln(['TCanvas.TextRect ',DbgSName(Self),' Text="',Text,'" ',dbgs(ARect),' X=',X,',Y=',Y]);
  Changing;
  
  Options := 0;
  case Style.Alignment of
    taRightJustify : Options := DT_RIGHT;
    taCenter : Options := DT_CENTER;
  end;
  case Style.Layout of
    tlCenter : Options := Options or DT_VCENTER;
    tlBottom : Options := Options or DT_BOTTOM;
  end;
  if Style.EndEllipsis then
    Options := Options or DT_END_ELLIPSIS;
  if Style.WordBreak then begin
    Options := Options or DT_WORDBREAK;
    if Style.EndEllipsis then
      Options := Options and not DT_END_ELLIPSIS;
  end;

  if Style.SingleLine then
    Options := Options or DT_SINGLELINE;

  if not Style.Clipping then
    Options := Options or DT_NOCLIP;

  if Style.ExpandTabs then
    Options := Options or DT_EXPANDTABS;

  if not Style.ShowPrefix then
    Options := Options or DT_NOPREFIX;

  if Style.RightToLeft then
    Options := Options or DT_RTLREADING;

  ReqState:=[csHandleValid];
  if not Style.SystemFont then
    Include(ReqState,csFontValid);
  if Style.Opaque then
    Include(ReqState,csBrushValid);
  DC:=GetUpdatedHandle(ReqState);

  DCIndex:=0;
  if Style.SystemFont or Style.Clipping or (not Style.Opaque) then
    SaveState;

  if Style.SystemFont then
    SelectObject(DC, OnGetSystemFont());

  // calculate text rectangle
  fRect := ARect;
  if Style.Alignment = taLeftJustify then
    fRect.Left := X;
  if Style.Layout = tlTop then
    fRect.Top := Y;

  if (Style.Alignment in [taRightJustify,taCenter]) or
    (Style.Layout in [tlCenter,tlBottom]) then
  begin
    DrawText(DC, pChar(Text), Length(Text), fRect, DT_CALCRECT or Options);
    case Style.Alignment of
      taRightJustify : OffsetRect(fRect, ARect.Right - fRect.Right, 0);
      taCenter : OffsetRect(fRect, (ARect.Right - fRect.Right) div 2, 0);
    end;
    case Style.Layout of
      tlCenter : OffsetRect(fRect, 0,
               ((ARect.Bottom - ARect.Top) - (fRect.Bottom - fRect.Top)) div 2);
      tlBottom : OffsetRect(fRect, 0, ARect.Bottom - fRect.Bottom);
    end;
  end;

  if Style.Clipping then
  begin
    IntersectRect(ARect, ARect, fRect);
    with ARect do
      InterSectClipRect(DC, Left, Top, Right, Bottom);
    Options := Options or DT_NOCLIP; // no clipping as we are handling it here
  end;

  if Style.Opaque then
    FillRect(fRect)
  else
    SetBkMode(DC, TRANSPARENT);

  if Style.SystemFont then
    SetTextColor(DC, TColorRef(Font.GetColor));

  //debugln('TCanvas.TextRect DRAW Text="',Text,'" ',dbgs(fRect));
  DrawText(DC, pChar(Text), Length(Text), fRect, Options);
  
  if Style.Opaque and (csBrushValid in FState) then
  begin
    if Brush.Style=bsSolid then // restore BKMode
      SetBkMode(DC, OPAQUE)
  end;

  RestoreState;

  Changed;
end;


{------------------------------------------------------------------------------
  Method:   TCanvas.TextOut
  Params:   X,Y,Text
  Returns:  Nothing

 ------------------------------------------------------------------------------}
procedure TCanvas.TextOut(X,Y: Integer; const Text: String);
var
  Flags : Cardinal;
begin
  Changing;
  RequiredState([csHandleValid, csFontValid, csBrushValid]);
  Flags := 0;
  if TextStyle.Opaque then
    Flags := ETO_Opaque;
  ExtUTF8Out(FHandle, X, Y, Flags, nil, PChar(Text), Length(Text), nil);
  MoveTo(X + TextWidth(Text), Y);
  Changed;
end;

{------------------------------------------------------------------------------
  function TCanvas.HandleAllocated: boolean;
 ------------------------------------------------------------------------------}
function TCanvas.HandleAllocated: boolean;
begin
  Result:=(FHandle<>0);
end;

{------------------------------------------------------------------------------
  function TCanvas.GetUpdatedHandle(ReqState: TCanvasState): HDC;
 ------------------------------------------------------------------------------}
function TCanvas.GetUpdatedHandle(ReqState: TCanvasState): HDC;
begin
  RequiredState(ReqState+[csHandleValid]);
  Result:=FHandle;
end;

{------------------------------------------------------------------------------
  Method:   TCanvas.BrushChanged
  Params:   ABrush: The changed brush
  Returns:  Nothing

  Notify proc for a brush change
 ------------------------------------------------------------------------------}
procedure TCanvas.BrushChanged(ABrush: TObject);
begin
  if csBrushValid in FState then
    Exclude(FState, csBrushValid);
end;

{------------------------------------------------------------------------------
  Method:   TCanvas.FontChanged
  Params:   AFont: the changed font
  Returns:  Nothing

  Notify proc for a font change
 ------------------------------------------------------------------------------}
procedure TCanvas.FontChanged(AFont: TObject);
begin
  if csFontValid in FState then
    Exclude(FState, csFontValid);
end;

{------------------------------------------------------------------------------
  Method:   TCanvas.PenChanging
  Params:   APen: The changing pen
  Returns:  Nothing

  Notify proc for a pen change
 ------------------------------------------------------------------------------}
procedure TCanvas.PenChanging(APen: TObject);
begin
  if [csPenValid, csHandleValid] * FState = [csPenValid, csHandleValid] then
  begin
    Exclude(FState, csPenValid);
    SelectObject(FHandle, FSavedPenHandle);
    FSavedPenHandle := 0;
  end;
end;

procedure TCanvas.FontChanging(AFont: TObject);
begin
  if [csFontValid, csHandleValid] * FState = [csFontValid, csHandleValid] then
  begin
    Exclude(FState, csFontValid);
    SelectObject(FHandle, FSavedFontHandle);
    FSavedFontHandle := 0;
  end;
end;

procedure TCanvas.BrushChanging(ABrush: TObject);
begin
  if [csBrushValid, csHandleValid] * FState = [csBrushValid, csHandleValid] then
  begin
    Exclude(FState, csBrushValid);
    SelectObject(FHandle, FSavedBrushHandle);
    FSavedBrushHandle := 0;
  end;
end;

procedure TCanvas.RegionChanging(ARegion: TObject);
begin
  if [csRegionValid, csHandleValid] * FState = [csRegionValid, csHandleValid] then
  begin
    Exclude(FState, csRegionValid);
    SelectObject(FHandle, FSavedRegionHandle);
    FSavedRegionHandle := 0;
  end;
end;

{------------------------------------------------------------------------------
  Method:   TCanvas.PenChanged
  Params:   APen: The changed pen
  Returns:  Nothing

  Notify proc for a pen change
 ------------------------------------------------------------------------------}
procedure TCanvas.PenChanged(APen: TObject);
begin
  if csPenValid in FState then
    Exclude(FState, csPenValid);
end;

{------------------------------------------------------------------------------
  Method:   TCanvas.RegionChanged
  Params:   ARegion: The changed Region
  Returns:  Nothing

  Notify proc for a region change
 ------------------------------------------------------------------------------}
procedure TCanvas.RegionChanged(ARegion: TObject);
begin
  if csRegionValid in FState then
    Exclude(FState, csRegionValid);
end;

{------------------------------------------------------------------------------
  Method:  TCanvas.Create
  Params:  none
  Returns: Nothing

  Constructor for the class.
 ------------------------------------------------------------------------------}
constructor TCanvas.Create;
begin
  FHandle := 0;
  ManageResources := true;
  inherited Create;
  FLazFont := TFont(inherited Font);
  FLazPen := TPen(inherited Pen);
  FLazBrush := TBrush(inherited Brush);
  FLazFont.OnChanging := @FontChanging;
  FLazFont.OnChange := @FontChanged;
  FSavedFontHandle := 0;
  FLazPen.OnChanging := @PenChanging;
  FLazPen.OnChange := @PenChanged;
  FSavedPenHandle := 0;
  FLazBrush.OnChanging := @BrushChanging;
  FLazBrush.OnChange := @BrushChanged;
  FSavedBrushHandle := 0;
  FRegion := TRegion.Create;
  FRegion.OnChanging := @RegionChanging;
  FRegion.OnChange := @RegionChanged;
  FSavedRegionHandle := 0;
  FCopyMode := cmSrcCopy;
  FAntialiasingMode := amDontCare;
  // FLock will be initialized on demand, because most canvas don't use it
  with FTextStyle do
  begin
    Alignment := taLeftJustify;
    Layout := tlTop;
    WordBreak := True;
    SingleLine := True;
    Clipping := True;
    ShowPrefix := False;
    Opaque := False;
  end;
end;

{------------------------------------------------------------------------------
  Method:   TCanvas.Chord
  Params:   x1, y1, x2, y2, StartAngle16Deg, EndAngle16Deg
  Returns:  Nothing

  Use Chord to draw a filled Chord-shape on the canvas. The angles angle1 and
  angle2 are 1/16th of a degree. For example, a full circle equals 5760(16*360).
  Positive values of Angle and AngleLength mean counter-clockwise while negative
  values mean clockwise direction. Zero degrees is at the 3'o clock position.

------------------------------------------------------------------------------}
procedure TCanvas.Chord(x1, y1, x2, y2,
  Angle16Deg, Angle16DegLength: Integer);
begin
  Changing;
  RequiredState([csHandleValid, csBrushValid, csPenValid]);
  LCLIntf.AngleChord(FHandle, x1, y1, x2, y2, Angle16Deg, Angle16DegLength);
  Changed;
end;

{------------------------------------------------------------------------------
  Method:   TCanvas.Chord
  Params:   x1, y1, x2, y2, sx, sy, ex, ey
  Returns:  Nothing

  Use Chord to draw a filled Chord-shape on the canvas. The values sx,sy,
  and ex,ey represent a starting and ending radial-points between which
  the Arc is draw.

------------------------------------------------------------------------------}
procedure TCanvas.Chord(x1, y1, x2, y2, SX, SY, EX, EY: Integer);
begin
  Changing;
  RequiredState([csHandleValid, csBrushValid, csPenValid]);
  LCLIntf.RadialChord(FHandle, x1, y1, x2, y2, sx, sy, ex, ey);
  Changed;
end;

{------------------------------------------------------------------------------
  Method: TCanvas.Destroy
  Params:  None
  Returns: Nothing

  Destructor for the class.
 ------------------------------------------------------------------------------}
destructor TCanvas.Destroy;
begin
//DebugLn('[TCanvas.Destroy] ',ClassName,'  Self=',DbgS(Self));
  Handle := 0;
  {$IF FPC_FULLVERSION>=20602}
  FreeThenNil(FClipRegion); {issue #24980 looks like TFPCustomCanvas bug}
  {$ENDIF}
  FreeThenNil(FRegion);
  FreeThenNil(FSavedHandleStates);
  if FLock <> 0 then
    DeleteCriticalSection(FLock);
  inherited Destroy;
  // set resources to nil, so that dangling pointers are spotted early
  FLazFont:=nil;
  FLazPen:=nil;
  FLazBrush:=nil;
end;

{------------------------------------------------------------------------------
  Function: TCanvas.GetHandle
  Params:   None
  Returns:  A handle to the GUI object

  Checks if a handle is allocated, otherwise create it
 ------------------------------------------------------------------------------}
function TCanvas.GetHandle : HDC;
begin
  //DebugLn('[TCanvas.GetHandle] ',ClassName);
  RequiredState(csAllValid);
  Result := FHandle;
end;

procedure TCanvas.SetAntialiasingMode(const AValue: TAntialiasingMode);
begin
  if FAntialiasingMode <> AValue then
  begin
    FAntialiasingMode := AValue;
    RealizeAntialiasing;
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCanvas.SetHandle
  Params:  NewHandle - the new device context
  Returns: nothing

  Deselect sub handles and sets the Handle
 ------------------------------------------------------------------------------}
procedure TCanvas.SetHandle(NewHandle: HDC);
begin
  if FHandle = NewHandle then Exit;

    //DebugLn('[TCanvas.SetHandle] Self=',DbgS(Self),' Old=',DbgS(FHandle,8),' New=',DbgS(NewHandle,8));
  if FHandle <> 0 then
  begin
    DeselectHandles;
    Exclude(FState, csHandleValid);
  end;
  
  FHandle := NewHandle;
  if FHandle <> 0 then
  begin
    RealizeAntialiasing;
    Include(FState, csHandleValid);
  end;
  //DebugLn('[TCanvas.SetHandle] END Self=',DbgS(Self),' Handle=',DbgS(FHandle,8));
end;

{------------------------------------------------------------------------------
  Method:  TCanvas.DeselectHandles
  Params:  none
  Returns: nothing

  Deselect all subhandles in the current device context
 ------------------------------------------------------------------------------}
procedure TCanvas.DeselectHandles;
begin
  //debugln('TCanvas.DeselectHandles ',ClassName,' Self=',DbgS(Self),' Handle=',DbgS(FHandle),' FSavedBrushHandle=',DbgS(Cardinal(FSavedBrushHandle)));
  if (FHandle <> 0) then
  begin
    // select default sub handles in the device context without deleting owns
    if FSavedBrushHandle <> 0 then
      SelectObject(FHandle, FSavedBrushHandle);
    if FSavedPenHandle <> 0 then
      SelectObject(FHandle, FSavedPenHandle);
    if FSavedFontHandle <> 0 then
      SelectObject(FHandle, FSavedFontHandle);
    FState := FState - [csPenValid, csBrushValid, csFontValid];
  end;
  FSavedBrushHandle:=0;
  FSavedPenHandle:=0;
  FSavedFontHandle:=0;
end;

{------------------------------------------------------------------------------
  Method:  TCanvas.CreateHandle
  Params:  None
  Returns: Nothing

  Creates the handle ( = object).
 ------------------------------------------------------------------------------}
procedure TCanvas.CreateHandle;
begin
  // Plain canvas does nothing
end;

procedure TCanvas.FreeHandle;
begin
  Handle:=0;
end;

{------------------------------------------------------------------------------
  Method:   TCanvas.RequiredState
  Params:   ReqState: The required state
  Returns:  Nothing

  Ensures that all handles needed are valid;
 ------------------------------------------------------------------------------}
procedure TCanvas.RequiredState(ReqState: TCanvasState);
var
  Needed: TCanvasState;
begin
  Needed := ReqState - FState;
  //DebugLn('[TCanvas.RequiredState] ',ClassName,' ',csHandleValid in ReqState,' ',csHandleValid in FState,' Needed=',Needed<>[]);
  if Needed <> [] then
  begin
    //DebugLn('[TCanvas.RequiredState] B ',ClassName,' ',csHandleValid in Needed,',',csFontValid in Needed,',',csPenValid in Needed,',',csBrushValid in Needed);
    if csHandleValid in Needed then
    begin
      CreateHandle;
      if FHandle = 0 then
        raise EInvalidOperation.Create(rsCanvasDoesNotAllowDrawing);
      RealizeAntialiasing;
      Include(FState, csHandleValid);
    end;
    if csFontValid in Needed then
    begin
      CreateFont;
      Include(FState, csFontValid);
    end;
    if csPenValid in Needed then
    begin
      CreatePen;
      if Pen.Style in [psDash, psDot, psDashDot, psDashDotDot] then
        Include(Needed, csBrushValid);
      Include(FState, csPenValid);
    end;
    if csBrushValid in Needed then
    begin
      CreateBrush;
      Include(FState, csBrushValid);
    end;
  end;
end;

procedure TCanvas.Changed;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TCanvas.SaveHandleState;
var
  DCIndex: LongInt;
begin
  if FSavedHandleStates = nil then
    FSavedHandleStates := TFPList.Create;
  DeselectHandles;
  RequiredState([csHandleValid]);
  DCIndex := SaveDC(Handle);
  FSavedHandleStates.Add(Pointer(PtrInt(DCIndex)));
end;

procedure TCanvas.RestoreHandleState;
var
  DCIndex: LongInt;
begin
  DCIndex := LongInt(PtrUInt(FSavedHandleStates[FSavedHandleStates.Count-1]));
  FSavedHandleStates.Delete(FSavedHandleStates.Count-1);
  DeselectHandles;
  RestoreDC(Handle, DCIndex);
end;

procedure TCanvas.Changing;
begin
  if Assigned(FOnChanging) then FOnChanging(Self);
end;

{------------------------------------------------------------------------------
  Function: TCanvas.TextExtent
  Params:   Text: The text to measure
  Returns:  The size

  Gets the width and height of a text
 ------------------------------------------------------------------------------}
function TCanvas.TextExtent(const Text: string): TSize;
begin
  Result.cX := 0;
  Result.cY := 0;
  if Text='' then exit;
  RequiredState([csHandleValid, csFontValid]);
  GetTextExtentPoint(FHandle, PChar(Text), Length(Text), Result);
end;

{------------------------------------------------------------------------------
  Function: TCanvas.TextWidth
  Params:   Text: The text to measure
  Returns:  The width

  Gets the width of a text
 ------------------------------------------------------------------------------}
function TCanvas.TextWidth(const Text: string): Integer;
begin
  Result := TextExtent(Text).cX;
end;

{------------------------------------------------------------------------------
  Function: TCanvas.TextFitInfo
  Params:   Text: The text in consideration
            MaxWidth: The size, the major input
  Returns:  The number of characters which will fit into MaxWidth

  Returns how many characters will fit in a specified width
 ------------------------------------------------------------------------------}
function TCanvas.TextFitInfo(const Text: string; MaxWidth: Integer): Integer;
var
  lSize: TSize;
begin
  LCLIntf.GetTextExtentExPoint(Self.Handle, PChar(Text), Length(Text),
    MaxWidth, @Result, nil, lSize);
end;

{------------------------------------------------------------------------------
  Function: TCanvas.TextHeight
  Params:   Text: The text to measure
  Returns:  A handle to the GUI object

  Gets the height of a text
 ------------------------------------------------------------------------------}
function TCanvas.TextHeight(const Text: string): Integer;
begin
  Result := TextExtent(Text).cY;
end;

{------------------------------------------------------------------------------
  Function: TCanvas.Lock
  Params:   none
  Returns:  nothing
 ------------------------------------------------------------------------------}
procedure TCanvas.Lock;
begin
  LockCanvas;
end;

function TCanvas.TryLock: Boolean;
begin
  Result := not Locked;
  if Result then
    Lock;
end;

{------------------------------------------------------------------------------
  Function: TCanvas.Unlock
  Params:   none
  Returns:  nothing
 ------------------------------------------------------------------------------}
procedure TCanvas.Unlock;
begin
  UnlockCanvas;
end;

{------------------------------------------------------------------------------
  procedure TCanvas.Refresh;
 ------------------------------------------------------------------------------}
procedure TCanvas.Refresh;
begin
  DeselectHandles;
end;
