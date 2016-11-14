{%MainUnit customdrawnint.pas}
{******************************************************************************
  All CustomDrawn Winapi implementations specific to the Cocoa backend

 ******************************************************************************
 Implementation
 ******************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}

function TCDWidgetSet.ClientToScreen(Handle: HWND; var P: TPoint) : Boolean;
begin
  Result := True;
end;

function TCDWidgetSet.ClipboardGetData(ClipboardType: TClipboardType;
  FormatID: TClipboardFormat; Stream: TStream): boolean;
begin
  Result := False;
end;

function TCDWidgetSet.ClipboardGetOwnerShip(ClipboardType: TClipboardType;
  OnRequestProc: TClipboardRequestEvent;  FormatCount: integer;
  Formats: PClipboardFormat): boolean;
begin
  Result := False;
end;

//##apiwiz##sps##   // Do not remove, no wizard declaration before this line
(*
procedure ColorToRGBFloat(cl: TColorRef; var r,g,b: Single); inline;
begin
  R:=(cl and $FF) / $FF;
  G:=((cl shr 8) and $FF) / $FF;
  B:=((cl shr 16) and $FF) / $FF;
end;

function RGBToColorFloat(r,g,b: Single): TColorRef; inline;
begin
  Result:=(Round(b*$FF) shl 16) or (Round(g*$FF) shl 8) or Round(r*$FF);
end;

function CocoaCombineMode(fnCombineMode: Integer): TCocoaCombine;
begin
  case fnCombineMode of
    RGN_AND:  Result:=cc_And;
    RGN_OR:   Result:=cc_Or;
    RGN_XOR:  Result:=cc_Xor;
    RGN_DIFF: Result:=cc_Diff;
  else
    Result:=cc_Copy;
  end;
end;

function TCocoaWidgetSet.CombineRgn(Dest, Src1, Src2: HRGN;
  fnCombineMode: Longint): Longint;
begin
  Result := LCLType.Error;
  if (Dest = 0) or (Src1 = 0) or (fnCombineMode<RGN_AND) or (fnCombineMode>RGN_COPY) then Exit;
  if (fnCombineMode <> RGN_COPY) and (Src2 = 0) then Exit;

  TCocoaRegion(Dest).CombineWith(TCocoaRegion(Src1), cc_Copy);

  if fnCombineMode <> RGN_COPY then
    TCocoaRegion(Dest). CombineWith(TCocoaRegion(Src2), CocoaCombineMode(fnCombineMode));
end;

{------------------------------------------------------------------------------
  Method:  CreateBitmap
  Params:  Width      - Bitmap width, in pixels
           Height     - Bitmap height, in pixels
           Planes     - Number of color planes
           BitCount   - Number of bits required to identify a color (TODO)
           BitmapBits - Pointer to array containing color data (TODO)
  Returns: A handle to a bitmap

  Creates a bitmap with the specified width, height and color format
 ------------------------------------------------------------------------------}
function TCocoaWidgetSet.CreateBitmap(Width, Height: Integer;
  Planes, BitCount: Longint; BitmapBits: Pointer): HBITMAP;
var
  bmpType: TCocoaBitmapType;
begin
  {$IFDEF VerboseCDWinAPI}
    DebugLn('TCocoaWidgetSet.CreateBitmap');
  {$ENDIF}

  // WORKAROUND: force context supported depths
  if BitmapBits = nil then
  begin
    if BitCount = 24 then BitCount := 32;
    // if BitCount = 1 then BitCount := 8;
  end;

  case BitCount of
    1:  bmpType := cbtMono;
    8:  bmpType := cbtGray;
    32: bmpType := cbtARGB;
  else
    bmpType := cbtRGB;
  end;

  // winapi Bitmaps are on a word boundary
  Result := HBITMAP(TCocoaBitmap.Create(Width, Height, BitCount, BitCount, cbaWord, bmpType, BitmapBits));
end;

function TCocoaWidgetSet.CreateBrushIndirect(const LogBrush: TLogBrush): HBRUSH;
var
  b     : TCocoaBrush;
begin
  b:=TCocoaBrush.Create;
  with b do ColorToRGBFloat(LogBrush.lbColor, R, G, B);
  Result:=HBRUSH(b);
end;

function TCocoaWidgetSet.CreateCompatibleBitmap(DC: HDC; Width, Height: Integer): HBITMAP;
begin
  {$IFDEF VerboseWinAPI}
    DebugLn('TCocoaWidgetSet.CreateCompatibleBitmap');
  {$ENDIF}

  Result := HBITMAP(TCocoaBitmap.Create(Width, Height, 32, 32, cbaDQWord, cbtARGB, nil));
end;

{------------------------------------------------------------------------------
  Method:  CreateCompatibleDC
  Params:  DC - Handle to memory device context
  Returns: Handle to a memory device context

  Creates a memory device context (DC) compatible with the specified device
 ------------------------------------------------------------------------------}
function TCocoaWidgetSet.CreateCompatibleDC(DC: HDC): HDC;
begin
  {$IFDEF VerboseWinAPI}
    DebugLn('TCocoaWidgetSet.CreateCompatibleDC');
  {$ENDIF}

  Result := HDC(TCocoaContext.Create);
end;

//todo:
//function TCocoaWidgetSet.CreateEllipticRgn(p1, p2, p3, p4: Integer): HRGN;
//begin
//end;

function TCocoaWidgetSet.CreateFontIndirect(const LogFont: TLogFont): HFONT;
begin
  Result:=CreateFontIndirectEx(LogFont, LogFont.lfFaceName);
end;

function TCocoaWidgetSet.CreateFontIndirectEx(const LogFont: TLogFont;
  const LongFontName: string): HFONT;
var
  cf  : TCocoaFont;
begin
  cf:=TCocoaFont.Create;
  cf.Size:=LogFont.lfHeight;
  cf.Name:=LongFontName;
  if LogFont.lfWeight>FW_NORMAL then Include(cf.Style, cfs_Bold);
  if LogFont.lfItalic>0 then Include(cf.Style, cfs_Italic);
  if LogFont.lfUnderline>0 then Include(cf.Style, cfs_Underline);
  if LogFont.lfStrikeOut>0 then Include(cf.Style, cfs_Strikeout);
  cf.Antialiased:=logFont.lfQuality>=ANTIALIASED_QUALITY;
  Result:=HFONT(cf);
end;*)

(*function Create32BitAlphaBitmap(ABitmap, AMask: TCocoaBitmap): TCocoaBitmap;
var
  ARawImage: TRawImage;
  Desc: TRawImageDescription absolute ARawimage.Description;

  ImgHandle, ImgMaskHandle: HBitmap;
  ImagePtr: PRawImage;
  DevImage: TRawImage;
  DevDesc: TRawImageDescription;
  SrcImage, DstImage: TLazIntfImage;
  W, H: Integer;
begin
  Result := nil;

  if not RawImage_FromBitmap(ARawImage, HBITMAP(ABitmap), HBITMAP(AMask)) then
    Exit;

  ImgMaskHandle := 0;

  W := Desc.Width;
  if W < 1 then W := 1;
  H := Desc.Height;
  if H < 1 then H := 1;

  QueryDescription(DevDesc, [riqfRGB, riqfAlpha], W, H);

  if DevDesc.IsEqual(Desc)
  then begin
    // image is compatible, so use it
    DstImage := nil;
    ImagePtr := @ARawImage;
  end
  else begin
    // create compatible copy
    SrcImage := TLazIntfImage.Create(ARawImage, False);
    DstImage := TLazIntfImage.Create(0,0,[]);
    DstImage.DataDescription := DevDesc;
    DstImage.CopyPixels(SrcImage);
    SrcImage.Free;
    DstImage.GetRawImage(DevImage);
    ImagePtr := @DevImage;
  end;

  try
    if not RawImage_CreateBitmaps(ImagePtr^, ImgHandle, ImgMaskHandle, True) then Exit;

    Result := TCocoaBitmap(ImgHandle);
  finally
    ARawImage.FreeData;
    DstImage.Free;
  end;
end;

function TCocoaWidgetSet.CreateIconIndirect(IconInfo: PIconInfo): HICON;
var
  ABitmap: TCocoaBitmap;
begin
  Result := 0;
  if IconInfo^.hbmColor = 0 then Exit;

  ABitmap := Create32BitAlphaBitmap(TCocoaBitmap(IconInfo^.hbmColor), TCocoaBitmap(IconInfo^.hbmMask));

  if IconInfo^.fIcon then
    Result := HICON(ABitmap)
  else
    Result := HICON(TCocoaCursor.CreateFromBitmap(ABitmap, GetNSPoint(IconInfo^.xHotSpot, IconInfo^.yHotSpot)));
end;

function TCocoaWidgetSet.CreatePenIndirect(const LogPen: TLogPen): HPEN;
var
  p   : TCocoaPen;
  cl  : DWORD;
begin
  {$IFDEF VerboseWinAPI}
    DebugLn('TCocoaWidgetSet.CreatePenIndirect');
  {$ENDIF}
  p:=TCocoaPen.Create;
  if LogPen.lopnWidth.x>0 then p.Width:=LogPen.lopnWidth.x;
  p.Style:=LogPen.lopnStyle;
  if LogPen.lopnColor and $8000000 > 0 then cl:=GetSysColor(LogPen.lopnColor)
  else cl:=LogPen.lopnColor;
  //todo:!
  ColorToRGBFloat(cl, p.R, p.G, p.B);
  Result := HPEN(p);//TCocoaPen.Create(LogPen));
end;

{------------------------------------------------------------------------------
  Method:  CreatePolygonRgn
  Params:  Points   - Pointer to array of polygon points
           NumPts   - Number of points passed
           FillMode - Filling mode
  Returns: The new polygonal region

  Creates a new polygonal region from the specified points
 ------------------------------------------------------------------------------}
function TCocoaWidgetSet.CreatePolygonRgn(Points: PPoint; NumPts: Integer;
  FillMode: integer): HRGN;
begin
  {$IFDEF VerboseWinAPI}
    DebugLn('TCocoaWidgetSet.CreatePolygonRgn NumPts: ' + DbgS(NumPts) +
      ' FillMode: ' + DbgS(FillMode));
  {$ENDIF}

  Result := HRGN(TCocoaRegion.Create(Points, NumPts, FillMode=ALTERNATE));
end;

function TCocoaWidgetSet.CreateRectRgn(X1, Y1, X2, Y2: Integer): HRGN;
begin
  {$IFDEF VerboseWinAPI}
    DebugLn('TCocoaWidgetSet.CreateRectRgn R: ' + DbgS(Classes.Rect(X1, Y1, X2, Y2)));
  {$ENDIF}

  Result := HRGN(TCocoaRegion.Create(X1, Y1, X2, Y2));
end;

function TCocoaWidgetSet.DeleteObject(GDIObject: HGDIOBJ): Boolean;
var
  gdi: TCocoaGDIObject;
begin
  Result:=True;
  gdi:=CheckGDIOBJ(GdiObject);
  if Assigned(gdi) then gdi.Release;
end;

function TCocoaWidgetSet.DestroyIcon(Handle: HICON): Boolean;
var
  Ico: TObject;
begin
  Result := Handle <> 0;
  if not Result then
    Exit;
  Ico := TObject(Handle);
  Result := (Ico is TCocoaBitmap) or (Ico is TCocoaCursor);
  if Result then
    Ico.Destroy;
end;

function TCocoaWidgetSet.Ellipse(DC: HDC; x1, y1, x2, y2: Integer): Boolean;
var
  ctx : TCocoaContext;
begin
  ctx:=CheckDC(DC);
  Result:=Assigned(ctx);
  if not Result then Exit;

  ctx.Ellipse(x1, y1, x2, y2);
end;

function TCocoaWidgetSet.EnableWindow(hWnd: HWND; bEnable: Boolean): Boolean;
begin
  if hWnd<>0
    then NSObject(hWnd).lclSetEnabled(bEnable)
    else Result:=False;
end;

function TCocoaWidgetSet.EnumDisplayMonitors(hdc: HDC; lprcClip: PRect;
  lpfnEnum: MonitorEnumProc; dwData: LPARAM): LongBool;
var
  i: integer;
begin
  Result := True;
  for i := 0 to NSScreen.screens.count - 1 do
  begin
    Result := Result and lpfnEnum(HMONITOR(NSScreen.screens.objectAtIndex(i)), 0, nil, dwData);
    if not Result then break;
  end;
end;*)

{$ifdef CD_UseNativeText}
function TCDWidgetSet.ExtTextOut(DC: HDC; X, Y: Integer; Options: Longint;
  Rect: PRect; Str: PChar; Count: Longint; Dx: PInteger): Boolean;
var
  ctx: TCocoaContext;
  lazdc: TLazCanvas;
begin
  {$ifdef VerboseCDText}
    DebugLn(Format('[WinAPI ExtTextOut] DC=%x X=%d Y=%d Str=%s Count=%d', [DC, X, Y, StrPas(Str), Count]));
  {$endif}

  if not IsValidDC(DC) then Exit;

  lazdc := TLazCanvas(ScreenDC);
  if lazdc.NativeDC = 0 then Exit;
  ctx := TCocoaContext(lazdc.NativeDC);

  // Native TextOut
  ctx.TextOut(0, 0, Str, Count, Dx, 0);

  // Now blend it into our DC
  lazdc := TLazCanvas(DC);
  lazdc.AlphaBlend(ScreenDC, X, Y, 0, 0, ScreenBitmapWidth, ScreenBitmapHeight);
end;
{$endif}

(*{------------------------------------------------------------------------------
  Method:  GetWindowRect
  Params:  Handle - Handle of window
           Rect   - Record for window coordinates
  Returns: if the function succeeds, the return value is nonzero; if the
           function fails, the return value is zero

  Retrieves the screen bounding rectangle of the specified window
 ------------------------------------------------------------------------------}
function TCocoaWidgetSet.GetWindowRect(Handle: hwnd; var ARect: TRect): Integer;
var
  dx, dy: Integer;
begin
  if Handle<>0 then begin
    ARect:=NSObject(Handle).lclFrame;
    if not NSObject(Handle).isKindOfClass_(NSWindow) then begin
      dx:=0; dy:=0;
      NSObject(Handle).lclLocalToScreen(dx, dx);
      MoveRect(ARect, dx, dy);
    end;
    Result:=1;
  end else
    Result:=0;
end;

function TCocoaWidgetSet.IsWindowEnabled(Handle: HWND): boolean;
begin
  if Handle<>0
    then Result:=NSObject(Handle).lclIsEnabled
    else Result:=False;
end;

function TCocoaWidgetSet.IsWindowVisible(Handle: HWND): boolean;
begin
  if Handle<>0
    then Result:=NSObject(Handle).lclIsVisible
    else Result:=False;
end;*)

(*function TCocoaWidgetSet.GetClientRect(handle : HWND; var ARect : TRect) : Boolean;
var
  dx, dy: Integer;
begin
  if Handle<>0 then begin
    Result:=True;
    ARect:=NSObject(handle).lclClientFrame;
    dx:=0; dy:=0;
    NSObject(Handle).lclLocalToScreen(dx, dy);
    MoveRect(ARect, dx, dy);
  end else
    Result:=False;
end;*)

{------------------------------------------------------------------------------
  Function: GetDeviceCaps
  Params: DC: HDC; Index: Integer
  Returns: Integer
 ------------------------------------------------------------------------------}

function TCDWidgetSet.GetKeyState(nVirtKey: Integer): Smallint;
begin
  Result := inherited GetKeyState(nVirtKey);
end;

(*function TCocoaWidgetSet.GetMonitorInfo(hMonitor: HMONITOR; lpmi: PMonitorInfo): Boolean;
var
  ScreenID: NSScreen absolute hMonitor;
begin
  Result := (lpmi <> nil) and (lpmi^.cbSize >= SizeOf(TMonitorInfo));
  if not Result then Exit;
  NSToLCLRect(ScreenID.frame, lpmi^.rcMonitor);
  NSToLCLRect(ScreenID.visibleFrame, lpmi^.rcWork);
  if ScreenID = NSScreen.mainScreen then
    lpmi^.dwFlags := MONITORINFOF_PRIMARY
  else
    lpmi^.dwFlags := 0;
end;

function TCocoaWidgetSet.GetParent(Handle : HWND): HWND;
begin
  if Handle<>0 then
    Result:=HWND(NSObject(Handle).lclParent)
  else
    Result:=0;
end;*)

{------------------------------------------------------------------------------
  Method:  GetSystemMetrics
  Params:  NIndex - System metric to retrieve
  Returns: The requested system metric value

  Retrieves various system metrics.
 ------------------------------------------------------------------------------}
function TCDWidgetSet.GetSystemMetrics(nIndex: Integer): Integer;
begin
  Result := 0;
end;

{$ifdef CD_UseNativeText}
{------------------------------------------------------------------------------
  Method:  GetTextExtentPoint
  Params:  DC    - Handle of device context
           Str   - Text string
           Count - Number of characters in string
           Size  - The record for the dimensions of the string
  Returns: If the function succeeds

  Computes the width and height of the specified string of text
 ------------------------------------------------------------------------------}
function TCDWidgetSet.GetTextExtentPoint(DC: HDC; Str: PChar; Count: Integer; var Size: TSize): Boolean;
var
  ctx: TCocoaContext;
  lazdc: TLazCanvas;
begin
  {$IFDEF VerboseCDText}
    DebugLn('[TCDWidgetSet.GetTextExtentPoint] DC: %x Str: %s Count: %d', [DC, Str, Count]);
  {$ENDIF}

  if not IsValidDC(DC) then Exit;

  lazdc := TLazCanvas(DC);
  if lazdc.NativeDC = 0 then Exit;
  ctx := TCocoaContext(lazdc.NativeDC);

  Result := ctx.GetTextExtentPoint(Str, Count, Size);

  {$IFDEF VerboseCDText}
    DebugLn('[TCDWidgetSet.GetTextExtentPoint] Size: %d,%d', [Size.cx, Size.cy]);
  {$ENDIF}
end;

{------------------------------------------------------------------------------
  Method:  GetTextMetrics
  Params:  DC - Handle of device context
           TM - The Record for the text metrics
  Returns: If the function succeeds

  Fills the specified buffer with the metrics for the currently selected font
  TODO: get exact max. and av. char width, pitch and charset
 ------------------------------------------------------------------------------}
function TCDWidgetSet.GetTextMetrics(DC: HDC; var TM: TTextMetric): Boolean;
var
  ctx: TCocoaContext;
  lazdc: TLazCanvas;
begin
  Result := False;

  {$IFDEF VerboseCDText}
    DebugLn('TCDWidgetSet.GetTextMetrics DC: ' + DbgS(DC));
  {$ENDIF}

  if not IsValidDC(DC) then Exit;

  lazdc := TLazCanvas(DC);
  if lazdc.NativeDC = 0 then Exit;
  ctx := TCocoaContext(lazdc.NativeDC);

  Result := ctx.GetTextMetrics(TM);

  {$IFDEF VerboseCDText}
    DebugLn('TCDWidgetSet.GetTextMetrics Result: ' + DbgS(Result) +
      ' TextMetric: ' + DbgS(TM));
  {$ENDIF}
end;
{$endif}

function TCDWidgetSet.BackendGetWindowRelativePosition(Handle: hwnd; var Left, Top: Integer): boolean;
begin
  if Handle<>0 then
  begin
    Result:=True;
    //TCocoaWindow(handle).lclRelativePos(Left, Top);
  end
  else
    Result:=False;
end;

(*function TCocoaWidgetSet.UpdateWindow(Handle: HWND): Boolean;
begin
  Result:=InvalidateRect(Handle, nil, false);
end;

{----------------------------- WINDOWS SCROLLING ------------------------------}

function TCocoaWidgetSet.GetScrollBarSize(Handle: HWND; BarKind: Integer): integer;
begin
  Result:=0;
end;

function TCocoaWidgetSet.GetScrollbarVisible(Handle: HWND; SBStyle: Integer): boolean;
begin
  Result:=False;
end;

function TCocoaWidgetSet.GetScrollInfo(Handle: HWND; BarFlag: Integer; Var ScrollInfo: TScrollInfo): Boolean;
begin
  Result:=False;
end;

function TCocoaWidgetSet.SetScrollInfo(Handle : HWND; SBStyle : Integer; ScrollInfo: TScrollInfo; bRedraw : Boolean): Integer;
begin
  Result:=0;
end;

function TCocoaWidgetSet.ShowScrollBar(Handle: HWND; wBar: Integer; bShow: Boolean): Boolean;
begin
  Result:=False;
end;

function TCocoaWidgetSet.SelectObject(ADC: HDC; GDIObj: HGDIOBJ): HGDIOBJ;
var
  dc: TCocoaContext;
  gdi: TCocoaGDIObject;
const
  SName = 'TCarbonWidgetSet.SelectObject';
begin
  {$IFDEF VerboseWinAPI}
    DebugLn(Format('TCocoaWidgetSet.SelectObject DC: %x GDIObj: %x', [ADC, GDIObj]));
  {$ENDIF}
  Result := 0;

  dc:=CheckDC(ADC);
  gdi:=CheckGDIOBJ(GDIObj);
  if not Assigned(dc) then Exit;

  if gdi is TCocoaBrush then begin // select brush
    Result := HBRUSH(dc.Brush);
    dc.Brush := TCocoaBrush(gdi);
  end else if gdi is TCocoaPen then begin // select pen
    Result := HPEN(dc.Pen);
    dc.Pen := TCocoaPen(gdi);
  end else if gdi is TCocoaFont then begin // select font
    Result := HFONT(dc.Font);
    dc.Font := TCocoaFont(gdi);
  end else if gdi is TCocoaRegion then begin // select region
    Result := HBRUSH(dc.Region);
    dc.Region := TCocoaRegion(gdi);
  end else if gdi is TCocoaBitmap then begin // select bitmap
    {if not (ADC is TCarbonBitmapContext) then
    begin
      DebugLn(SName + ' Error - The specified device context is not bitmap context!');
      Exit;
    end;}
    Result := HBITMAP(dc.Bitmap);
    dc.Bitmap:=TCocoaBitmap(gdi);
    //TCarbonBitmapContext(ADC).Bitmap := TCarbonBitmap(GDIObj);
  end;

  if Result<>0 then TCocoaGDIObject(Result).Release;
  if Assigned(gdi) then gdi.AddRef;

  {$IFDEF VerboseWinAPI}
    DebugLn(Format('TCocoaWidgetSet.SelectObject Result: %x', [Result]));
  {$ENDIF}
end;*)

{------------------------------------------------------------------------------
  Function: SetFocus
  Params: hWnd   - Window handle to be focused
  Returns:

 ------------------------------------------------------------------------------}

(*{------------------------------------------------------------------------------
  function ShowWindow(hWnd: HWND; nCmdShow: Integer): Boolean;

  nCmdShow:
    SW_SHOWNORMAL, SW_MINIMIZE, SW_SHOWMAXIMIZED
------------------------------------------------------------------------------}
function TCocoaWidgetSet.ShowWindow(hWnd: HWND; nCmdShow: Integer): Boolean;
begin
  {$ifdef VerboseCocoaWinAPI}
    DebugLn('TCocoaWidgetSet.ShowWindow');
  {$endif}

  case nCmdShow of
    SW_SHOW, SW_SHOWNORMAL:
      NSWindow(hwnd).orderFront(nil);
    SW_HIDE:
      NSWindow(hwnd).orderOut(nil);
    SW_MINIMIZE:
      NSWindow(hwnd).miniaturize(nil);
  end;
  Result:=true;
end;

function TCocoaWidgetSet.RectVisible(DC: HDC; const ARect: TRect): Boolean;
var
  ClipBox: CGRect;
  ctx : TCocoaContext;
  R: TRect;
begin
  Result := False;

  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.RectVisible DC: ' + DbgS(DC) + ' R: ' + DbgS(ARect));
  {$ENDIF}

  ctx:=CheckDC(DC);
  if not Assigned(ctx) or (ARect.Right <= ARect.Left) or (ARect.Bottom <= ARect.Top) then Exit;

  // In Quartz 2D there is no direct access to clipping path of CGContext,
  // therefore we can only test bounding box of the clipping path.

  ClipBox := CGContextGetClipBoundingBox(ctx.CGContext);
  Result := IntersectRect(R, ARect, CGRectToRect(ClipBox));

  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.RectVisible Result: ' + DbgS(Result) + ' Clip: ' + DbgS(CGRectToRect(ClipBox)));
  {$ENDIF}
end;

function TCocoaWidgetSet.MoveWindowOrgEx(DC: HDC; dX, dY: Integer): Boolean;
var
  ctx : TCocoaContext;
begin
  Result := False;
  ctx:=CheckDC(DC);
  if not Assigned(ctx) then Exit;

  {$IFDEF VerboseWinAPI}
  DebugLn('TCarbonWidgetSet.MoveWindowOrgEx DC: ' + DbgS(DC) + ' ' + DbgS(DX) + ', ' + DbgS(DY));
  {$ENDIF}
  ctx.SetOrigin(dX, dY);
  Result := True;
end;

function TCocoaWidgetSet.GetWindowOrgEx(dc : hdc; P : PPoint): Integer;
var
  ctx : TCocoaContext;
begin
  ctx:=CheckDC(dc);
  if not Assigned(ctx) or not Assigned(P) then
    Result:=0
  else begin
    ctx.GetOrigin(p^.X, p^.Y);
    Result:=1;
  end;
end;

function TCocoaWidgetSet.SetCursor(ACursor: HCURSOR): HCURSOR;
begin
  Result := HCURSOR(TCocoaCursor(ACursor).Install);
end;

function TCocoaWidgetSet.SetCursorPos(X, Y: Integer): Boolean;
var
  CursorPos: CGPoint;
begin
  Result := False;

  CursorPos.X := X;
  CursorPos.Y := Y;
  if CGWarpMouseCursorPosition(CursorPos) <> noErr then Exit;
  Result := True;
end;
*)

(*function TCocoaWidgetSet.SaveDC(DC: HDC): Integer;
var
  ctx : TCocoaContext;
  cg  : CGContextRef;
begin
  ctx := CheckDC(DC);
  if not Assigned(ctx) then begin
    Result:=0;
    Exit;
  end;
  cg:=ctx.CGContext;
  if Assigned(cg) then begin
    CGContextSaveGState(cg);
    inc(ctx.Stack);
    Result:=ctx.Stack;
  end else
    Result:=0;
end;

function TCocoaWidgetSet.RestoreDC(DC: HDC; SavedDC: Integer): Boolean;
var
  ctx : TCocoaContext;
  cg  : CGContextRef;
  cnt : Integer;
  i   : Integer;
begin
  Result:=False;
  ctx := CheckDC(DC);
  cg:=ctx.CGContext;
  if not Assigned(ctx) or not Assigned(cg) then Exit;

  if SavedDC<0 then cnt:=1
  else cnt:=ctx.Stack-SavedDC+1;
  Result:=cnt>0;

  if Result then begin
    for i:=1 to cnt do CGContextRestoreGState(cg);
    dec(ctx.Stack, cnt);
  end;
end;*)

{------------------------------------------------------------------------------
  Method:  ScreenToClient
  Params: Handle - window handle for source coordinates
          P      - record containing coordinates
  Returns: if the function succeeds, the return value is nonzero; if the
           function fails, the return value is zero

  Converts the screen coordinates of a specified point on the screen to client
  coordinates.
 ------------------------------------------------------------------------------}
function TCDWidgetSet.ScreenToClient(Handle: HWND; Var P: TPoint): Integer;
begin
  Result := 0;
  //Result := Integer(Windows.ScreenToClient(Handle, @P));
end;

//##apiwiz##eps##   // Do not remove, no wizard declaration after this line
