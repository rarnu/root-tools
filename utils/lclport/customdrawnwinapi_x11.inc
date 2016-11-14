{%MainUnit customdrawnint.pp}

function TCDWidgetSet.ClientToScreen(Handle: HWND; var P: TPoint) : Boolean;
begin
  Result := False;
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


{$ifdef CD_UseNativeText}
{------------------------------------------------------------------------------
  Function: ExtTextOut
  Params:  none
  Returns: Nothing
 ------------------------------------------------------------------------------}
function TCDWidgetSet.ExtTextOut(DC: HDC; X, Y: Integer; Options: Longint;
  Rect: PRect; Str: PChar; Count: Longint; Dx: PInteger): Boolean;
var
  FontData: PXftFont;
  Descriptor, lText: string;
  extents: TXGlyphInfo;
  XftDraw: PXftDraw;
  LazDC: TLazCanvas absolute DC;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('[WinAPI ExtTextOut]');
  {$endif}

  Result := False;
{  lText := StrPas(Str);
  if UTF8Length(lText) < 1 then Exit(True);

  // Create the handle
  // 'Arial-10');
  Descriptor := 'Sans-10';
  FontData := XftFontOpenName(FDisplay, XDefaultScreen(FDisplay), PChar(Descriptor));
  if not Assigned(FontData) then raise Exception.Create('[TCDWidgetSet.ExtTextOut] X11: Error creating font');

  XftDraw := XftDrawCreate(FDisplay, Handle,
    XDefaultVisual(GFApplication.Handle, XDefaultScreen(GFApplication.Handle)),
    XDefaultColormap(GFApplication.Handle, XDefaultScreen(GFApplication.Handle)));

  // Draw
  XftDrawSetClip(FXftDraw, FRegion);
  XftDrawStringUtf8(FXftDraw, FXftColor, TX11Font(FFont).FontData, APosition.x,
    Aposition.y + TX11Font(FFont).GetAscent, PChar(AText), Length(AText));

  // Cleanup
  XftFontClose(FDisplay, FontData);    }
  Result := True;

{  if ((Options and (ETO_OPAQUE + ETO_CLIPPED)) <> 0) and (Rect = nil) then
    exit;

  if not IsValidDC(DC) then Exit;

   if ((Options and ETO_OPAQUE) <> 0) then
     QtDC.fillRect(Rect^.Left, Rect^.Top, Rect^.Right - Rect^.Left, Rect^.Bottom - Rect^.Top);

  if Str <> nil then
  begin
    if Count >= 0 then
      WideStr := GetUtf8String(Copy(Str, 1, Count))
    else
      WideStr := GetUtf8String(Str);

    if (Options and ETO_CLIPPED <> 0) then
    begin
      B := QtDC.getClipping;
      if not B then
      begin
        QtDC.save;
        QtDC.setClipRect(Rect^);
      end;
      QtDC.drawText(X, Y, Rect^.Right - Rect^.Left, Rect^.Bottom - Rect^.Top, 0, @WideStr);
      if not B then
        QtDC.restore;
    end else
      QtDC.drawText(X, Y, @WideStr);
  end;

  Result := True;   }
end;
{$endif}

(*{------------------------------------------------------------------------------
  Function: FillRect
  Params:  none
  Returns: Nothing


 ------------------------------------------------------------------------------}
function TQtWidgetSet.FillRect(DC: HDC; const Rect: TRect; Brush: HBRUSH): Boolean;
begin
  Result := False;

  {$ifdef VerboseQtWinAPI}
    DebugLn('[WinAPI FillRect Rect=', dbgs(Rect),' Brush=', dbghex(Brush));
  {$endif}

  if not IsValidDC(DC) then
    exit;
  if not IsValidGdiObject(Brush) then
    exit;

  TQtDeviceContext(DC).fillRect(@Rect, TQtBrush(Brush).FHandle);
  Result := True;
end;

{------------------------------------------------------------------------------
  Function: FillRgn
  Params:  DC: HDC; RegionHnd: HRGN; hbr: HBRUSH
  Returns: Boolean

 ------------------------------------------------------------------------------}
function TQtWidgetSet.FillRgn(DC: HDC; RegionHnd: HRGN; hbr: HBRUSH): Bool;
var
  OldRgn: TQtRegion;
  R: TRect;
  hasClipping: Boolean;
  QtDC: TQtDeviceContext;
begin
  {$ifdef VerboseQtWinAPI}
    DebugLn('[WinAPI FillRgn Rgn=', dbgs(RegionHnd),' Brush=', dbghex(hbr));
  {$endif}

  Result := False;

  if not IsValidDC(DC) then exit;

  QtDC := TQtDeviceContext(DC);

  HasClipping := QtDC.getClipping;
  QtDC.save;
  if HasClipping then
    OldRgn := TQtRegion.Create(True);
  try
    if HasClipping then
      QPainter_clipRegion(QtDC.Widget, OldRgn.FHandle);
    if SelectClipRgn(DC, RegionHnd) <> ERROR then
    begin
      R := TQtRegion(RegionHnd).getBoundingRect;
      QtDC.fillRect(@R, TQtBrush(hbr).FHandle);
      if HasClipping then
        SelectClipRgn(DC, HRGN(OldRgn));
      Result := True;
    end;
  finally
    if HasClipping then
      OldRgn.Free;
    QtDC.restore;
  end;

end;

{------------------------------------------------------------------------------
  Function: Frame
  Params:  none
  Returns: Nothing

  Draws the border of a rectangle.
 ------------------------------------------------------------------------------}
function TQtWidgetSet.Frame(DC: HDC; const ARect: TRect): Integer;
begin
  Result := 0;

  if not IsValidDC(DC) then Exit;

  TQtDeviceContext(DC).drawRect(ARect.Left, ARect.Top,
   ARect.Right - ARect.Left, ARect.Bottom - ARect.Top);

  Result := 1;
end;

{------------------------------------------------------------------------------
  Function: Frame3D
  Params:  none
  Returns: Nothing

  Draws a 3d border in Qt native style.
 ------------------------------------------------------------------------------}
function TQtWidgetSet.Frame3d(DC : HDC; var ARect : TRect;
  const FrameWidth : integer; const Style : TBevelCut) : boolean;
var
  QtDC: TQtDeviceContext;
begin
  {$ifdef VerboseQtWinAPI}
    DebugLn('[TQtWidgetSet.Frame3d Rect=', dbgs(ARect));
  {$endif}

  Result := False;

  if not IsValidDC(DC) then exit;

  QtDC := TQtDeviceContext(DC);

  case Style of
    bvNone: ;
    bvLowered: QtDC.qDrawWinPanel(ARect.Left, ARect.Top,
      ARect.Right - ARect.Left, ARect.Bottom - ARect.Top, nil, True, FrameWidth);
    bvRaised: QtDC.qDrawWinPanel(ARect.Left, ARect.Top,
      ARect.Right - ARect.Left, ARect.Bottom - ARect.Top, nil, False, FrameWidth);
    bvSpace: QtDC.qDrawPlainRect(ARect.Left, ARect.Top,
      ARect.Right - ARect.Left, ARect.Bottom - ARect.Top, nil, FrameWidth);
  end;

  InflateRect(ARect, -FrameWidth, -FrameWidth);
  Result := True;
end;

{------------------------------------------------------------------------------
  Function: FrameRect
  Params:  none
  Returns: Nothing
 ------------------------------------------------------------------------------}
function TQtWidgetSet.FrameRect(DC: HDC; const ARect: TRect;
  hBr: HBRUSH): Integer;
begin
  Result := 0;

  if not IsValidDC(DC) then Exit;

  TQtDeviceContext(DC).qDrawPLainRect(ARect.Left, ARect.Top,
   ARect.Right - ARect.Left, ARect.Bottom - ARect.Top);

  Result := 1;
end;

function TQtWidgetSet.GetActiveWindow: HWND;
var
  Widget: QWidgetH;
  W: TQtWidget;
  SubW: TQtWidget;
  Area: QMdiAreaH;
begin
  Widget := QApplication_activeWindow;
  if Widget <> nil then
  begin
    W := QtObjectFromWidgetH(Widget);
    if W <> nil then
    begin
      if TQtMainWindow(W).MDIAreaHandle <> nil then
      begin
        Area := QMdiAreaH(TQtMainWindow(W).MDIAreaHandle.Widget);
        SubW := QtObjectFromWidgetH(QMdiArea_activeSubWindow(Area));
        if SubW <> nil then
          Result := HWND(SubW)
        else
          Result := HWND(W);
      end else
        Result := HWND(W);
    end;
  end else
    Result := 0;
end;


{------------------------------------------------------------------------------
  Method:  TQtWidgetSet.GetBitmapBits
  Params:  none
  Returns:

 ------------------------------------------------------------------------------}
function TQtWidgetSet.GetBitmapBits(Bitmap: HBITMAP; Count: Longint;  Bits: Pointer): Longint;
var
  Image: QImageH;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('[WinAPI GetBitmapBits]',' Bitmap=', dbghex(Bitmap),' Count=',Count);
  {$endif}

  Result := 0;

  if (Bitmap = 0) or (Count <= 0) then
    Exit;

  Image := QImage_create(TQtImage(Bitmap).FHandle);
  try
    Result := (QImage_width(Image) * QImage_height(Image) * QImage_depth(Image) + 7) div 8;
    if Count < Result then
      Result := Count;
    if Result > 0 then
      Move(QImage_bits(Image)^, Bits^, Result);
  finally
    QImage_destroy(Image);
  end;
end;

function TQtWidgetSet.GetBkColor(DC: HDC): TColorRef;
var
  QtDC: TQtDeviceContext;
begin
  Result := CLR_INVALID;
  if not IsValidDC(DC) then Exit;
  QtDC := TQtDeviceContext(DC);
  Result := QtDC.GetBkColor;
end;

function TQtWidgetSet.GetCapture: HWND;
var
  w: QWidgetH;
  Widget: TQtWidget;
  {$IFDEF MSWINDOWS}
  AWin: HWND;
  {$ENDIF}
begin
  {$IFDEF MSWINDOWS}
  AWin := Windows.GetCapture;
  if AWin <> 0 then
    w := QWidget_find(AWin)
  else
    w := nil;

  if (w = nil) and (QApplication_mouseButtons() > 0) then
    w := QApplication_focusWidget()
  else
    if w <> QWidget_mouseGrabber then
      w := QWidget_mouseGrabber;

  {$ELSE}
  w := QWidget_mouseGrabber();
  {$ENDIF}

  if w <> nil then
  begin
    // Capture widget can be child of complex control. In any case we should return TQtWidget as result.
    // So we will look for parent while not found apropriate LCL handle.
    Widget := GetFirstQtObjectFromWidgetH(w);
    Result := HWND(Widget);
  end
  else
    Result := 0;
  {$ifdef VerboseQtWinAPI}
  WriteLn('[WinAPI GetCapture] Capture = ', Result);
  {$endif}
end;

function TQtWidgetSet.GetCaretPos(var lpPoint: TPoint): Boolean;
begin
  Result := QtCaret.GetCaretPos(lpPoint);
end;

function TQtWidgetSet.GetCaretRespondToFocus(handle: HWND; var ShowHideOnFocus: boolean): Boolean;
begin
  ShowHideOnFocus := QtCaret.GetQtCaretRespondToFocus;
  Result := True;
end;*)

{------------------------------------------------------------------------------
  Function: GetClientBounds
  Params: handle:
          Result:
  Returns: true on success

  Returns the client bounds of a control. The client bounds is the rectangle of
  the inner area of a control, where the child controls are visible. The
  coordinates are relative to the control's left and top.
 ------------------------------------------------------------------------------}

(*{------------------------------------------------------------------------------
  Function: GetClientRect
  Params: handle:
          Result:
  Returns: true on success

  Returns the client bounds of a control. The client bounds is the rectangle of
  the inner area of a control, where the child controls are visible. The
  coordinates are relative to the control's left and top.
 ------------------------------------------------------------------------------}
function TQtWidgetSet.GetClientRect(handle : HWND; var ARect : TRect) : Boolean;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('[WinAPI GetClientRect]');
  {$endif}

  GetClientBounds(Handle, ARect);
  OffsetRect(ARect, -ARect.Left, -ARect.Top);

  Result := True;
end;

{------------------------------------------------------------------------------
  Function: GetClipBox
  Params: dc, lprect
  Returns: Integer

  Returns the smallest rectangle which includes the entire current
  Clipping Region, or if no Clipping Region is set, the current
  dimensions of the Drawable.

  The result can be one of the following constants
      Error
      NullRegion
      SimpleRegion
      ComplexRegion
 ------------------------------------------------------------------------------}
function TQtWidgetSet.GetClipBox(DC: hDC; lpRect: PRect): Longint;
var
  ARegion: QRegionH;
  Pt: TPoint;
begin
  Result := NULLREGION;
  if lpRect <> nil then
    lpRect^ := Rect(0,0,0,0);

  if not IsValidDC(DC) then
    Result := ERROR;

  if Result <> ERROR then
  with TQtDeviceContext(DC) do
  begin
    {$ifdef VerboseQtWinAPI}
      Writeln('TQtWidgetSet.GetClipBox FastClip=',
       ((vClipRect <> nil) and not vClipRectDirty) );
    {$endif}

    // the most correct way to get a clipbox if through
    // region.boundingrect, but it's slower.

    // TODO: remove "and false" below when vClipRectDirty is implemented
    //       it should be "true" when user set a custom clip rect
    //       and "false" on beginpaint
    if (vClipRect<>nil) and not vClipRectDirty and false then
      lpRect^ := vClipRect^
    else
    if getClipping then
    begin
      ARegion := QRegion_Create;
      try
        QPainter_clipRegion(Widget, ARegion);
        GetWindowOrgEx(DC, @Pt);
        if (Pt.X <> 0) or (Pt.Y <> 0) then
          SetWindowOrgEx(DC, Pt.X, Pt.Y, @Pt);
        QRegion_boundingRect(ARegion, lpRect);
      finally
        QRegion_destroy(ARegion);
      end;
      Result := SIMPLEREGION;
    end
    else
      if vImage <> nil then
      begin
        lpRect^ := Rect(0, 0, vImage.width, vImage.height);
        Result := SIMPLEREGION;
      end;
    {$ifdef VerboseQtWinAPI}
    WriteLn('TQtWidgetSet.GetClipBox Rect=', dbgs(lprect^));
    {$endif}
  end;
end;

{------------------------------------------------------------------------------
  Function: GetClipRGN
  Params: dc, rgn
  Returns: Integer

  Returns a copy of the current Clipping Region.

  The result can be one of the following constants
     0 = no clipping set
     1 = ok
    -1 = error
 ------------------------------------------------------------------------------}
function TQtWidgetSet.GetClipRGN(DC : hDC; RGN : hRGN): Longint;
begin
  {$ifdef VerboseQtWinAPI}
    Writeln('Trace: [WinAPI GetClipRgn]',
     ' DC: ', dbghex(DC),
     ' RGN: ', dbghex(Rgn));
    if RGN<>0 then
      WriteLn(' QRegionH=', PtrInt(TQtRegion(Rgn).Widget))
    else
      WriteLn(' Rgn=0');
  {$endif}
  // it assumes that clipregion object has been created some other place
  Result := -1;
  if not IsValidDC(DC) then
    exit;
  if Rgn = 0 then
    exit;
  if not TQtDeviceContext(DC).getClipping then
    Result := 0
  else
  begin
    // if our TQtRegion contains widget then
    // first destroy it because QPainter creates
    // new reference.
    if TQtRegion(Rgn).FHandle <> nil then
    begin
      QRegion_destroy(TQtRegion(Rgn).FHandle);
      TQtRegion(Rgn).FHandle := QRegion_create;
    end;
    QPainter_clipRegion(TQtDeviceContext(DC).Widget, TQtRegion(Rgn).FHandle);
    Result := 1;
  end;
end;

function TQtWidgetSet.GetCmdLineParamDescForInterface: string;
  function b(const s: string): string;
  begin
    Result:=BreakString(s,75,22)+LineEnding+LineEnding;
  end;
begin
  Result:=
     b(rsqtOptionNoGrab)
    +b(rsqtOptionDoGrab)
    +b(rsqtOptionSync)
    +b(rsqtOptionStyle)
    +b(rsqtOptionStyleSheet)
    +b(rsqtOptionGraphicsStyle)
    +b(rsqtOptionSession)
    +b(rsqtOptionWidgetCount)
    +b(rsqtOptionReverse)
    {$IFDEF HASX11}
    +b(rsqtOptionX11Display)
    +b(rsqtOptionX11Geometry)
    +b(rsqtOptionX11Font)
    +b(rsqtOptionX11BgColor)
    +b(rsqtOptionX11FgColor)
    +b(rsqtOptionX11BtnColor)
    +b(rsqtOptionX11Name)
    +b(rsqtOptionX11Title)
    +b(rsqtOptionX11Visual)
    +b(rsqtOptionX11NCols)
    +b(rsqtOptionX11CMap)
    +b(rsqtOptionX11IM)
    +b(rsqtOptionX11InputStyle)
    {$ENDIF}
    ;
end;

{------------------------------------------------------------------------------
  Method: GetCurrentObject
  Params:
    DC - A handle to the DC
    uObjectType - The object type to be queried
  Returns: If the function succeeds, the return value is a handle to the specified object.
    If the function fails, the return value is NULL.
 ------------------------------------------------------------------------------}

function TQtWidgetSet.GetCurrentObject(DC: HDC; uObjectType: UINT): HGDIOBJ;
var
  QtDC: TQtDeviceContext absolute DC;
begin
  Result := 0;
  if not QtWidgetSet.IsValidDC(DC) then
    Exit;
  case uObjectType of
    OBJ_BITMAP: Result := HGDIOBJ(QtDC.vImage);
    OBJ_BRUSH: Result := HGDIOBJ(QtDC.vBrush);
    OBJ_FONT: Result := HGDIOBJ(QtDC.vFont);
    OBJ_PEN: Result := HGDIOBJ(QtDC.vPen);
  end;
end;*)

{------------------------------------------------------------------------------
  Function: GetCursorPos
  Params:  lpPoint: The cursorposition
  Returns: True if succesful
 ------------------------------------------------------------------------------}

(*{------------------------------------------------------------------------------
  Function: GetDC
  Params:  hWnd is any widget.
  Returns: Nothing

  This function is Called:
  - Once on app startup with hWnd = 0
  - Twice for every TLabel on the TCustomLabel.CalcSize function
 ------------------------------------------------------------------------------}
function TCDWidgetSet.BackendGetDC(hWnd: HWND): HDC;
var
  lFormHandle: TX11WindowInfo;
begin
  lFormHandle := TX11WindowInfo(hWnd);

  Result := 0;

  // Screen DC
  if hWnd = 0 then Exit;

  // Form DC
  if lFormHandle.Canvas = nil then lFormHandle.Canvas := TLazCanvas.create(nil);
  Result := HDC(lFormHandle.Canvas);
end;

function TQtWidgetSet.GetDCOriginRelativeToWindow(PaintDC: HDC;
  WindowHandle: HWND; var OriginDiff: TPoint): boolean;
var
  QtDC: TQtDeviceContext absolute PaintDC;
  Matrix: QTransformH;
  P: TPoint;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('[WinAPI GetDCOriginRelativeToWindow] PaintDC ' + dbghex(PaintDC));
  {$endif}
  Result := IsValidDC(PaintDC);
  if not Result then
    exit;
  Matrix := QPainter_transform(QtDC.Widget);
  OriginDiff := Point(0, 0);
  P := Point(0, 0);
  if WindowHandle <> 0 then
    P := TQtWidget(WindowHandle).getClientOffset;
  if Matrix <> nil then
  begin
    OriginDiff.X := Round(QTransform_Dx(Matrix)) - P.X;
    OriginDiff.Y := Round(QTransform_Dy(Matrix)) - P.Y;
  end;
end;*)

{------------------------------------------------------------------------------
  Function: GetDeviceCaps
  Params: DC: HDC; Index: Integer
  Returns: Integer

 ------------------------------------------------------------------------------}

(*function TQtWidgetSet.GetDIBits(DC: HDC; Bitmap: HBitmap; StartScan, NumScans: UINT; Bits: Pointer; var BitInfo: BitmapInfo; Usage: UINT): Integer;
begin
  Result := 0;
  {$ifdef VerboseQtWinAPI_MISSING_IMPLEMENTATION}
    WriteLn('***** [WinAPI TQtWidgetSet.GetDIBits] missing implementation ');
  {$endif}
end;

{------------------------------------------------------------------------------
  Function: GetDoubleClickTime
  Params: none
  Returns:

 ------------------------------------------------------------------------------}
function TQtWidgetSet.GetDoubleClickTime: UINT;
begin
  Result := QApplication_doubleClickInterval;
end;

{------------------------------------------------------------------------------
  Function: GetFocus
  Params:  None
  Returns: Nothing

 ------------------------------------------------------------------------------}
function TQtWidgetSet.GetFocus: HWND;
var
  W: QWidgetH;
 {$ifdef VerboseFocus}
 Obj: TQtWidget;
 {$endif}
begin
  Result := 0;
  W := QApplication_FocusWidget();
  if W <> nil then
  begin
    Result := HwndFromWidgetH(W);
    {$ifdef VerboseFocus}
    Obj := TQtWidget(Result);
    Write('TQtWidgetSet.GetFocus: WidgetH=',dbghex(ptruint(W)), ' QtWidget=', dbgsname(Obj));
    if Obj<>nil then
      WriteLn(' LclObject=', dbgsname(Obj.LCLObject))
    else
      WriteLn;
    {$endif}
  end;
end;
*)

function TCDWidgetSet.GetKeyState(nVirtKey: Integer): Smallint;
const
  StateDown    = SmallInt($FF80);
  {StateToggled = SmallInt($0001);}
begin
  Result := 0;

  case nVirtKey of
    VK_LSHIFT:   nVirtKey := VK_SHIFT;
    VK_RSHIFT:   nVirtKey := VK_SHIFT;
    VK_LCONTROL: nVirtKey := VK_CONTROL;
    VK_RCONTROL: nVirtKey := VK_CONTROL;
    VK_LMENU:    nVirtKey := VK_MENU;
  end;

  // where to track toggle state?

  case nVirtKey of
(*    VK_LBUTTON:
      if (QApplication_mouseButtons and QtLeftButton) > 0 then
        Result := Result or StateDown;
    VK_RBUTTON:
      if (QApplication_mouseButtons and QtRightButton) > 0 then
        Result := Result or StateDown;
    VK_MBUTTON:
      if (QApplication_mouseButtons and QtMidButton) > 0 then
        Result := Result or StateDown;
    VK_XBUTTON1:
      if (QApplication_mouseButtons and QtXButton1) > 0 then
        Result := Result or StateDown;
    VK_XBUTTON2:
      if (QApplication_mouseButtons and QtXButton2) > 0 then
        Result := Result or StateDown;
    VK_MENU:
      if (QApplication_keyboardModifiers and QtAltModifier) > 0 then
        Result := Result or StateDown;
*)
    VK_SHIFT:
      if ssShift in ShiftState then
        Result := Result or StateDown;
    VK_CONTROL:
      if ssCtrl in ShiftState then
        Result := Result or StateDown;
(*    VK_LWIN, VK_RWIN:
      if (QApplication_keyboardModifiers and QtMetaModifier) > 0 then
        Result := Result or StateDown;
   {$ifdef VerboseQtWinAPI}
    else
      DebugLn('TQtWidgetSet.GetKeyState TODO ', DbgSVKCode(Word(nVirtkey)));
   {$endif}  *)
  end;
end;

(*
function TQtWidgetSet.GetMapMode(DC: HDC): Integer;
begin
  if IsValidDC(DC) then
    Result := TQtDeviceContext(DC).vMapMode
  else
    Result := 0;
end;

function TQtWidgetSet.GetMonitorInfo(Monitor: HMONITOR; lpmi: PMonitorInfo): Boolean;
var
  Desktop: QDesktopWidgetH;
begin
  Result := (lpmi <> nil) and (lpmi^.cbSize >= SizeOf(TMonitorInfo)) or (Monitor = 0);
  if not Result then Exit;
  Desktop := QApplication_desktop();
  Dec(Monitor);
  Result := (Monitor >= 0) and (Monitor < PtrUInt(QDesktopWidget_numScreens(Desktop)));
  if not Result then Exit;
  QDesktopWidget_screenGeometry(Desktop, @lpmi^.rcMonitor, Monitor);
  QDesktopWidget_availableGeometry(Desktop, @lpmi^.rcWork, Monitor);
  if PtrUInt(QDesktopWidget_primaryScreen(Desktop)) = Monitor then
    lpmi^.dwFlags := MONITORINFOF_PRIMARY
  else
    lpmi^.dwFlags := 0;
end;

{------------------------------------------------------------------------------
  Method:  TQtWidgetSet.GetDeviceSize
  Params:  none
  Returns: True if successful

  Return the size of a device
 ------------------------------------------------------------------------------}
function TQtWidgetSet.GetDeviceSize(DC: HDC; var P: TPoint): Boolean;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('[WinAPI GetDeviceSize]');
  {$endif}

  Result := False;

  P.X := 0;
  P.Y := 0;

  if not IsValidDC(DC) then Exit;

  if (TObject(DC) is TQtDeviceContext) then
    P := TQtDeviceContext(DC).getDeviceSize;

  Result := True;
end;

{------------------------------------------------------------------------------
  Method:  TQtWidgetSet.GetObject
  Params:  none
  Returns: The size written to the buffer

  Necessary for TBitmap support
 ------------------------------------------------------------------------------}
function TQtWidgetSet.GetObject(GDIObj: HGDIOBJ; BufSize: Integer; Buf: Pointer): Integer;
const
  QtPenStyleToWinStyleMap: array[QtPenStyle] of UINT =
  (
 { QtNoPen          } PS_NULL,
 { QtSolidLine      } PS_SOLID,
 { QtDashLine       } PS_DASH,
 { QtDotLine        } PS_DOT,
 { QtDashDotLine    } PS_DASHDOT,
 { QtDashDotDotLine } PS_DASHDOTDOT,
 { QtCustomDashLine } PS_USERSTYLE
  );
var
  aObject: TObject;
  AFont: TQtFont absolute aObject;
  APen: TQtPen absolute aObject;
  ABrush: TQtBrush absolute aObject;
  BitmapSection : TDIBSECTION;
  ALogFont: PLogFont absolute Buf;
  ALogPen: PLogPen absolute Buf;
  AExtLogPen: PExtLogPen absolute Buf;
  ALogBrush: PLogBrush absolute Buf;
  Dashes: TQRealArray;
  i: integer;
  {$ifdef VerboseQtWinAPI}
    ObjType: string;
  {$endif}
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('Trace:> [WinAPI GetObject] GDIObj: ' + dbghex(GDIObj));
    ObjType := '';
  {$endif}

  Result := 0;

  if not IsValidGDIObject(GDIObj) then
  begin
    {$ifdef VerboseQtWinAPI}
      WriteLn('Trace:< [WinAPI GetObject] Invalid GDI Object');
    {$endif}

    Exit;
  end;

  aObject := TObject(GDIObj);

  {------------------------------------------------------------------------------
    Font
   ------------------------------------------------------------------------------}
  if aObject is TQtFont then
  begin
    if Buf = nil then
      Result := SizeOf(TLogFont)
    else
    if BufSize >= SizeOf(TLogFont) then
    begin
      Result := SizeOf(TLogFont);

      FillChar(ALogFont^, SizeOf(ALogFont^), 0);
      ALogFont^.lfHeight := AFont.getPixelSize;
      ALogFont^.lfEscapement := AFont.Angle;
      case AFont.getWeight of
        10: ALogFont^.lfWeight := FW_THIN;
        15: ALogFont^.lfWeight := FW_EXTRALIGHT;
        25: ALogFont^.lfWeight := FW_LIGHT;
        50: ALogFont^.lfWeight := FW_NORMAL;
        55: ALogFont^.lfWeight := FW_MEDIUM;
        63: ALogFont^.lfWeight := FW_SEMIBOLD;
        75: ALogFont^.lfWeight := FW_BOLD;
        80: ALogFont^.lfWeight := FW_EXTRABOLD;
        87: ALogFont^.lfWeight := FW_HEAVY;
      end;

      ALogFont^.lfItalic := Ord(AFont.getItalic) * High(Byte);
      ALogFont^.lfUnderline := Ord(AFont.getUnderline) * High(Byte);
      ALogFont^.lfStrikeOut := Ord(AFont.getStrikeOut) * High(Byte);
      ALogFont^.lfCharSet := DEFAULT_CHARSET;
      case AFont.getStyleStategy of
        QFontPreferMatch: ALogFont^.lfQuality := DRAFT_QUALITY;
        QFontPreferQuality: ALogFont^.lfQuality := PROOF_QUALITY;
        QFontNoAntialias: ALogFont^.lfQuality := NONANTIALIASED_QUALITY;
        QFontPreferAntialias: ALogFont^.lfQuality := ANTIALIASED_QUALITY;
      else
        ALogFont^.lfQuality := DEFAULT_QUALITY;
      end;
      ALogFont^.lfFaceName := UTF16ToUTF8(AFont.getFamily);
    end;
  end
  {------------------------------------------------------------------------------
    Pen
   ------------------------------------------------------------------------------}
  else
  if aObject is TQtPen then
  begin
    if not APen.IsExtPen then
    begin
      if Buf = nil then
        Result := SizeOf(TLogPen)
      else
      if BufSize >= SizeOf(TLogPen) then
      begin
        Result := SizeOf(TLogPen);
        TQColorToColorRef(APen.getColor, ALogPen^.lopnColor);
        if APen.getCosmetic then
          ALogPen^.lopnWidth := Point(1, 0)
        else
          ALogPen^.lopnWidth := Point(APen.getWidth, 0);
        ALogPen^.lopnStyle := QtPenStyleToWinStyleMap[APen.getStyle];
      end;
    end
    else
    begin
      i := SizeOf(TExtLogPen);
      if APen.getStyle = QtCustomDashLine then
      begin
        Dashes := APen.getDashPattern;
        inc(i, (Length(Dashes) - 1) * SizeOf(DWord));
      end
      else
        Dashes := nil;
      if Buf = nil then
        Result := i
      else
      if BufSize >= i then
      begin
        Result := i;
        AExtLogPen^.elpPenStyle := QtPenStyleToWinStyleMap[APen.getStyle];

        if not APen.getCosmetic then
        begin
          AExtLogPen^.elpPenStyle := AExtLogPen^.elpPenStyle or PS_GEOMETRIC;

          case APen.getJoinStyle of
            QtMiterJoin: AExtLogPen^.elpPenStyle := AExtLogPen^.elpPenStyle or PS_JOIN_MITER;
            QtBevelJoin: AExtLogPen^.elpPenStyle := AExtLogPen^.elpPenStyle or PS_JOIN_BEVEL;
            QtRoundJoin: AExtLogPen^.elpPenStyle := AExtLogPen^.elpPenStyle or PS_JOIN_ROUND;
          end;

          case APen.getCapStyle of
            QtFlatCap: AExtLogPen^.elpPenStyle := AExtLogPen^.elpPenStyle or PS_ENDCAP_FLAT;
            QtSquareCap: AExtLogPen^.elpPenStyle := AExtLogPen^.elpPenStyle or PS_ENDCAP_SQUARE;
            QtRoundCap: AExtLogPen^.elpPenStyle := AExtLogPen^.elpPenStyle or PS_ENDCAP_ROUND;
          end;

          AExtLogPen^.elpWidth := APen.getWidth;
        end
        else
          AExtLogPen^.elpWidth := 1;

        AExtLogPen^.elpBrushStyle := BS_SOLID;
        TQColorToColorRef(APen.getColor, AExtLogPen^.elpColor);
        AExtLogPen^.elpHatch := 0;

        AExtLogPen^.elpNumEntries := Length(Dashes);
        if AExtLogPen^.elpNumEntries > 0 then
        begin
          for i := 0 to AExtLogPen^.elpNumEntries - 1 do
            PDword(@AExtLogPen^.elpStyleEntry)[i] := Trunc(Dashes[i]);
        end
        else
          AExtLogPen^.elpStyleEntry[0] := 0;
      end;
    end;
  end
  {------------------------------------------------------------------------------
    Region
   ------------------------------------------------------------------------------}
  else
  if aObject is TQtRegion then
  begin
    {TODO: implement Region}
    {$ifdef VerboseQtWinAPI}
      ObjType := 'Region';
    {$endif}
  end else
  {------------------------------------------------------------------------------
    Brush
   ------------------------------------------------------------------------------}
  if aObject is TQtBrush then
  begin
    if Buf = nil then
      Result := SizeOf(TLogBrush)
    else
    if BufSize >= SizeOf(TLogBrush) then
    begin
      Result := SizeOf(TLogBrush);
      TQColorToColorRef(ABrush.getColor^, ALogBrush^.lbColor);
      ABrush.GetLbStyle(ALogBrush^.lbStyle, ALogBrush^.lbHatch);
    end;
  end
  {------------------------------------------------------------------------------
    Image
   ------------------------------------------------------------------------------}
  else
  if aObject is TQtImage then
  begin
    {$ifdef VerboseQtWinAPI}
      ObjType := 'Image';
    {$endif}

    if Buf = nil then
      Result := SizeOf(TDIBSECTION)
    else
    begin
      BitmapSection.dsOffset := 0;
      FillChar(BitmapSection, SizeOf(TDIBSECTION), 0);

      with TQtImage(aObject) do
      begin
        {dsBM - BITMAP}
        BitmapSection.dsBm.bmType := $4D42;
        BitmapSection.dsBm.bmWidth := width;
        BitmapSection.dsBm.bmHeight := height;
        BitmapSection.dsBm.bmWidthBytes := bytesPerLine;
        BitmapSection.dsBm.bmPlanes := 1;//Does Bitmap Format support more?
        BitmapSection.dsBm.bmBitsPixel := depth;
        BitmapSection.dsBm.bmBits := bits;

        {dsBmih - BITMAPINFOHEADER}
        BitmapSection.dsBmih.biSize := 40;
        BitmapSection.dsBmih.biWidth := BitmapSection.dsBm.bmWidth;
        BitmapSection.dsBmih.biHeight := BitmapSection.dsBm.bmHeight;
        BitmapSection.dsBmih.biPlanes := BitmapSection.dsBm.bmPlanes;
        BitmapSection.dsBmih.biBitCount := BitmapSection.dsBm.bmBitsPixel;

        BitmapSection.dsBmih.biCompression := 0;

        BitmapSection.dsBmih.biSizeImage := numBytes;
        BitmapSection.dsBmih.biXPelsPerMeter := dotsPerMeterX;
        BitmapSection.dsBmih.biYPelsPerMeter := dotsPerMeterY;

        BitmapSection.dsBmih.biClrUsed := 0;
        BitmapSection.dsBmih.biClrImportant := 0;
      end;

      if BufSize >= SizeOf(BitmapSection) then
      begin
        PDIBSECTION(Buf)^ := BitmapSection;
        Result := SizeOf(TDIBSECTION);
      end
      else if BufSize > 0 then
      begin
        Move(BitmapSection, Buf^, BufSize);
        Result := BufSize;
      end;
    end;
  end;

  {$ifdef VerboseQtWinAPI}
    WriteLn('Trace:< [WinAPI GetObject] Result=', dbgs(Result), ' ObjectType=', ObjType);
  {$endif}
end;

function TQtWidgetSet.GetParent(Handle : HWND): HWND;
var
  QtWidget: TQtWidget;
begin
  {$ifdef VerboseQtWinAPI}
  writeln('Trace:> [WinAPI GetParent] Handle: ' + dbghex(Handle));
  {$endif}
  Result := 0;
  if Handle = 0 then
    exit;

  QtWidget := TQtWidget(Handle);

  Result := HwndFromWidgetH(QtWidget.GetParent);

  {$ifdef VerboseQtWinAPI}
  writeln('Trace:< [WinAPI GetParent] : ' + dbghex(Result));
  {$endif}
end;

function TQtWidgetSet.GetProp(Handle: hwnd; Str: PChar): Pointer;
begin
  if Handle<>0 then
    result := TQtWidget(Handle).Props[str]
  else
    result := nil;
end;

function TQtWidgetSet.GetRgnBox(RGN : HRGN; lpRect : PRect) : Longint;
var
  R: TRect;
begin
  {$ifdef VerboseQtWinAPI}
  writeln('Trace:> [WinAPI GetRgnBox] Handle: ' + dbghex(RGN));
  {$endif}
  Result := SIMPLEREGION;
  if lpRect <> nil then
    lpRect^ := Rect(0,0,0,0);
  if not IsValidGDIObject(RGN) then
    Result := ERROR
  else
  begin
    Result := TQtRegion(RGN).GetRegionType;
    if not (Result in [ERROR, NULLREGION]) and (lpRect <> nil) then
    begin
      R := TQtRegion(RGN).getBoundingRect;
      with lpRect^ do
      begin
        Left   := R.Left;
        Top    := R.Top;
        Right  := R.Left + R.Right;
        Bottom := R.Top + R.Bottom;
      end;
    end;
  end;
end;

function TQtWidgetSet.GetROP2(DC: HDC): Integer;
var
  QtDC: TQtDeviceContext absolute DC;
begin
  {$ifdef VerboseQtWinAPI}
  writeln('> TQtWidgetSet.GetROP2() DC ',dbghex(DC));
  {$endif}
  Result := R2_COPYPEN;
  if not IsValidDC(DC) then
    exit;
  Result := QtDC.Rop2;
  {$ifdef VerboseQtWinAPI}
  writeln('< TQtWidgetSet.GetROP2() DC ',dbghex(DC),' Result ',Result);
  {$endif}
end;

function TQtWidgetSet.GetScrollBarSize(Handle: HWND; BarKind: Integer): integer;
var
  w: TQtWidget;
  ScrollBar: TQtScrollBar;
begin
	{$ifdef VerboseQtWinAPI}
  writeln('Trace:> [WinAPI GetScrollBarSize] Handle: ' + dbghex(Handle),' BarKind: ',BarKind);
  {$endif}
  Result := 0;
  if Handle = 0 then exit;

  w := TQtWidget(Handle);

  {TODO: find out what to do with TCustomForm descendants }
  if w is TQtAbstractScrollArea then
  begin
    if BarKind in [SM_CXVSCROLL, SM_CYVSCROLL] then
      ScrollBar := TQtAbstractScrollArea(w).verticalScrollBar
    else
      ScrollBar := TQtAbstractScrollArea(w).horizontalScrollBar;
  end else
  if w is TQtScrollBar then
    ScrollBar := TQtScrollBar(w)
  else
    ScrollBar := nil;
  if ScrollBar <> nil then
  begin
    if BarKind in [SM_CXHSCROLL, SM_CYVSCROLL] then
      Result := ScrollBar.getWidth
    else
      Result := ScrollBar.getHeight;
  end;
end;

function TQtWidgetSet.GetScrollbarVisible(Handle: HWND; SBStyle: Integer): boolean;
var
  w: TQtWidget;
  ScrollBar: TQtScrollBar;
begin
	{$ifdef VerboseQtWinAPI}
  writeln('Trace:> [WinAPI GetScrollBarVisible] Handle: ' + dbghex(Handle),' SBStyle: ',SBStyle);
  {$endif}
  Result := False;
  if Handle = 0 then exit;

  w := TQtWidget(Handle);

  {TODO: find out what to do with TCustomForm descendants }
  if w is TQtAbstractScrollArea then
  begin
    if SBStyle = SB_VERT then
      ScrollBar := TQtAbstractScrollArea(w).verticalScrollBar
    else
      ScrollBar := TQtAbstractScrollArea(w).horizontalScrollBar;
  end else
  if w is TQtScrollBar then
    ScrollBar := TQtScrollBar(w)
  else
    ScrollBar := nil;

  if ScrollBar <> nil then
    Result := ScrollBar.getVisible;
end;

{------------------------------------------------------------------------------
  Function: GetScrollInfo
  Params: BarFlag
           SB_CTL Retrieves the parameters for a scroll bar control. The hwnd
           parameter must be the handle to the scroll bar control.
           SB_HORZ Retrieves the parameters for the window's standard horizontal
           scroll bar.
           SB_VERT Retrieves the parameters for the window's standard vertical
           scroll bar.

          ScrollInfo returns TScrollInfo structure.

  Returns: boolean

 ------------------------------------------------------------------------------}
function TQtWidgetSet.GetScrollInfo(Handle: HWND; BarFlag: Integer; Var ScrollInfo: TScrollInfo): Boolean;
var
  QtScrollBar: TQtScrollBar;
begin
  Result := False;

  if Handle = 0 then exit;

  if (csDestroying in TQtWidget(Handle).LCLObject.ComponentState) or
   (csFreeNotification in TQtWidget(Handle).LCLObject.ComponentState) then
    exit;

  QtScrollBar := nil;

  if not TQtWidget(Handle).LCLObject.InheritsFrom(TCustomScrollBar) then
  begin
    if (TQtWidget(Handle) is TQtAbstractScrollArea) then
    begin
      case BarFlag of
        SB_HORZ: QtScrollBar := TQtAbstractScrollArea(Handle).horizontalScrollBar;
        SB_VERT: QtScrollBar := TQtAbstractScrollArea(Handle).verticalScrollBar;
      end;
    end else
      Result := False;
  end
  else
    QtScrollBar := TQtScrollBar(TScrollBar(TQtWidget(Handle).LCLObject).Handle);

  if Assigned(QtScrollBar) then
  begin
    // POS
    if (ScrollInfo.fMask and SIF_POS) <> 0 then
    begin
      if QtScrollBar.ChildOfComplexWidget = ccwAbstractScrollArea then
        ScrollInfo.nPos := QtScrollBar.getSliderPosition
      else
        ScrollInfo.nPos := QtScrollBar.getValue;
    end;

    // RANGE
    if (ScrollInfo.fMask and SIF_RANGE) <> 0 then
    begin
      ScrollInfo.nMin:= QtScrollBar.getMin;
      ScrollInfo.nMax:= QtScrollBar.getMax + QtScrollBar.getPageStep;
    end;
    // PAGE
    if (ScrollInfo.fMask and SIF_PAGE) <> 0 then
      ScrollInfo.nPage := QtScrollBar.getPageStep;

    // TRACKPOS
    if (ScrollInfo.fMask and SIF_TRACKPOS) <> 0 then
      ScrollInfo.nTrackPos := QtScrollBar.getSliderPosition;

    Result := True;
  end;
end;

function TQtWidgetSet.GetStockObject(Value: Integer): THandle;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('Trace:> [WinAPI GetStockObject] Value: ', Value);
  {$endif}

  Result := 0;

  case Value of
    BLACK_BRUSH:         // Black brush.
      Result := FStockBlackBrush;
    DKGRAY_BRUSH:        // Dark gray brush.
      Result := FStockDKGrayBrush;
    GRAY_BRUSH:          // Gray brush.
      Result := FStockGrayBrush;
    LTGRAY_BRUSH:        // Light gray brush.
      Result := FStockLtGrayBrush;
    NULL_BRUSH:          // Null brush (equivalent to HOLLOW_BRUSH).
      Result := FStockNullBrush;
    WHITE_BRUSH:         // White brush.
      Result := FStockWhiteBrush;

    BLACK_PEN:           // Black pen.
      Result := FStockBlackPen;
    NULL_PEN:            // Null pen.
      Result := FStockNullPen;
    WHITE_PEN:           // White pen.
      Result := FStockWhitePen;

    {System font. By default, Windows uses the system font to draw menus,
     dialog box controls, and text. In Windows versions 3.0 and later,
     the system font is a proportionally spaced font; earlier versions of
     Windows used a monospace system font.}
    DEFAULT_GUI_FONT, SYSTEM_FONT:
      begin

        If FStockSystemFont <> 0 then
        begin
          DeleteObject(FStockSystemFont);
          FStockSystemFont := 0;
        end;

        If FStockSystemFont = 0 then
          FStockSystemFont := CreateDefaultFont;
        Result := FStockSystemFont;
      end;

  {$ifdef VerboseQtWinAPI}
    else
    WriteLn('[WinAPI GetStockObject] UNHANDLED Value: ', Value);
  {$endif}
  end;
  {$ifdef VerboseQtWinAPI}
    WriteLn('Trace:< [WinAPI GetStockObject] Value: ', Value);
  {$endif}
end;

{------------------------------------------------------------------------------
  Function: TQtWidgetSet.GetSysColor
  Params:   index to the syscolors array
  Returns:  RGB value

 ------------------------------------------------------------------------------}
function TQtWidgetSet.GetSysColor(nIndex: Integer): DWORD;

  function GetColor(Group: QPaletteColorGroup; Role: QPaletteColorRole; ClassName: PAnsiChar = nil): TColor;
  var
    Handle: QPaletteH;
    QColor: PQColor;
    QC: QColorH;
  begin
    Handle := QPalette_create;
    if ClassName = nil then
      QApplication_palette(Handle)
    else
      QApplication_palette(Handle, ClassName);

    QColor := QPalette_color(Handle, Group, Role);
    QC := QColor_create(QColor);
    try
      Result := (QColor_red(QC) and $00FF) or ((QColor_green(QC) and $00FF) shl 8) or ((QColor_blue(QC) and $00FF) shl 16);
    finally
      QColor_destroy(QC);
    end;

    QPalette_destroy(Handle);
  end;

begin
  if (nIndex < 0) or (nIndex > MAX_SYS_COLORS) then
  begin
    {$ifdef VerboseQtWinAPI}
      WriteLn('Trace:Unknown lcl system color: [TQtWidgetSet.GetSysColor]');
    {$endif}
    Result := 0;
    Exit;
  end;

  if FCachedColors[nIndex] = nil then
  begin
    case nIndex of
      COLOR_SCROLLBAR               : Result:=GetColor(QPaletteActive,   QPaletteButton);
      COLOR_BACKGROUND              : Result:=GetColor(QPaletteActive,   QPaletteWindow);
      COLOR_WINDOW                  : Result:=GetColor(QPaletteInActive, QPaletteBase);
      COLOR_WINDOWFRAME             : Result:=GetColor(QPaletteActive,   QPaletteShadow);
      COLOR_WINDOWTEXT              : Result:=GetColor(QPaletteActive,   QPaletteWindowText);
      COLOR_ACTIVEBORDER            : Result:=GetColor(QPaletteActive,   QPaletteWindow);
      COLOR_INACTIVEBORDER          : Result:=GetColor(QPaletteInactive, QPaletteWindow);
      COLOR_APPWORKSPACE            : Result:=GetColor(QPaletteActive,   QPaletteWindow);
      COLOR_HIGHLIGHT               : Result:=GetColor(QPaletteActive,   QPaletteHighlight);
      COLOR_HIGHLIGHTTEXT           : Result:=GetColor(QPaletteActive,   QPaletteHighlightedText);
      COLOR_BTNFACE                 : Result:=GetColor(QPaletteActive,   QPaletteButton);
      COLOR_BTNSHADOW               : Result:=GetColor(QPaletteActive,   QPaletteDark);
      COLOR_GRAYTEXT                : Result:=GetColor(QPaletteDisabled,   QPaletteText);
      COLOR_BTNTEXT                 : Result:=GetColor(QPaletteActive,   QPaletteButtonText);
      COLOR_BTNHIGHLIGHT            : Result:=GetColor(QPaletteActive,   QPaletteLight);
      COLOR_3DDKSHADOW              : Result:=GetColor(QPaletteActive,   QPaletteShadow);
      COLOR_3DLIGHT                 : Result:=GetColor(QPaletteActive,   QPaletteMidlight);
      COLOR_INFOTEXT                : Result:=GetColor(QPaletteInActive, QPaletteToolTipText);
      COLOR_INFOBK                  : Result:=GetColor(QPaletteInActive, QPaletteToolTipBase);
      COLOR_HOTLIGHT                : Result:=GetColor(QPaletteActive,   QPaletteLight);

      // qt does not provide any methods to retrieve titlebar colors
    {$IFNDEF MSWINDOWS}
      COLOR_ACTIVECAPTION           : Result:=GetColor(QPaletteActive,   QPaletteHighlight);
      COLOR_INACTIVECAPTION         : Result:=GetColor(QPaletteInActive, QPaletteHighlight);
      COLOR_CAPTIONTEXT             : Result:=GetColor(QPaletteActive,   QPaletteHighlightedText);
      COLOR_INACTIVECAPTIONTEXT     : Result:=GetColor(QPaletteInactive, QPaletteHighlightedText);
      COLOR_GRADIENTACTIVECAPTION   : Result:=GetColor(QPaletteActive,   QPaletteBase);
      COLOR_GRADIENTINACTIVECAPTION : Result:=GetColor(QPaletteInactive, QPaletteBase);
    {$ELSE}
      COLOR_ACTIVECAPTION           : Result:=Windows.GetSysColor(COLOR_ACTIVECAPTION);
      COLOR_INACTIVECAPTION         : Result:=Windows.GetSysColor(COLOR_INACTIVECAPTION);
      COLOR_CAPTIONTEXT             : Result:=Windows.GetSysColor(COLOR_CAPTIONTEXT);
      COLOR_INACTIVECAPTIONTEXT     : Result:=Windows.GetSysColor(COLOR_INACTIVECAPTIONTEXT);
      COLOR_GRADIENTACTIVECAPTION   : Result:=Windows.GetSysColor(COLOR_GRADIENTACTIVECAPTION);
      COLOR_GRADIENTINACTIVECAPTION : Result:=Windows.GetSysColor(COLOR_GRADIENTINACTIVECAPTION);
    {$ENDIF}
      COLOR_MENU                    : Result:=GetColor(QPaletteActive,   QPaletteButton, 'QMenu');
      COLOR_MENUTEXT                : Result:=GetColor(QPaletteActive,   QPaletteButtonText, 'QMenu');
      COLOR_MENUHILIGHT             : Result:=GetColor(QPaletteDisabled, QPaletteHighlight, 'QMenu');
      COLOR_MENUBAR                 : Result:=GetColor(QPaletteActive,   QPaletteButton, 'QMenu');
      COLOR_FORM                    : Result:=GetColor(QPaletteActive,   QPaletteWindow);
    else
      Result:=0;
    end;
    FCachedColors[nIndex] := getMem(SizeOf(LongWord));
    FCachedColors[nIndex]^ := Result;
  end
  else
    Result := FCachedColors[nIndex]^;
end;

function TQtWidgetSet.GetSysColorBrush(nIndex: Integer): HBrush;

  function GetBrush(Group: QPaletteColorGroup; Role: QPaletteColorRole; ClassName: PAnsiChar = nil): HBrush;
  var
    Handle: QPaletteH;
  begin
    Handle := QPalette_create;
    if ClassName = nil then
      QApplication_palette(Handle)
    else
      QApplication_palette(Handle, ClassName);
    if FSysColorBrushes[nIndex] = 0 then
      Result := HBrush(TQtBrush.Create(False))
    else
      Result := FSysColorBrushes[nIndex];
    TQtBrush(Result).FHandle := QBrush_create(QPalette_brush(Handle, Group, Role));
    TQtBrush(Result).FShared := True;

    QPalette_destroy(Handle);
  end;

  function GetSolidBrush(AColor: TColor): HBrush;
  var
    Color: TQColor;
  begin
    if FSysColorBrushes[nIndex] = 0 then
      Result := HBrush(TQtBrush.Create(True))
    else
      Result := FSysColorBrushes[nIndex];
    Color := QBrush_Color(TQtBrush(Result).FHandle)^;
    ColorRefToTQColor(ColorToRGB(AColor), Color);
    QBrush_setColor(TQtBrush(Result).FHandle, @Color);
    TQtBrush(Result).FShared := True;
  end;

begin
  if (nIndex < 0) or (nIndex > MAX_SYS_COLORS) then
  begin
    Result := 0;
    Exit;
  end;

  if (FSysColorBrushes[nIndex] = 0) or
    (
    (FSysColorBrushes[nIndex] <> 0) and
    (TQtBrush(FSysColorBrushes[nIndex]).FHandle = nil)
    ) then
  begin
    case nIndex of
      COLOR_SCROLLBAR               : Result:=GetBrush(QPaletteActive,   QPaletteButton);
      COLOR_BACKGROUND              : Result:=GetBrush(QPaletteActive,   QPaletteWindow);
      COLOR_WINDOW                  : Result:=GetBrush(QPaletteInActive, QPaletteBase);
      COLOR_WINDOWFRAME             : Result:=GetBrush(QPaletteActive,   QPaletteShadow);
      COLOR_WINDOWTEXT              : Result:=GetBrush(QPaletteActive,   QPaletteWindowText);
      COLOR_ACTIVEBORDER            : Result:=GetBrush(QPaletteActive,   QPaletteWindow);
      COLOR_INACTIVEBORDER          : Result:=GetBrush(QPaletteInactive, QPaletteWindow);
      COLOR_APPWORKSPACE            : Result:=GetBrush(QPaletteActive,   QPaletteWindow);
      COLOR_HIGHLIGHT               : Result:=GetBrush(QPaletteActive,   QPaletteHighlight);
      COLOR_HIGHLIGHTTEXT           : Result:=GetBrush(QPaletteActive,   QPaletteHighlightedText);
      COLOR_BTNFACE                 : Result:=GetBrush(QPaletteActive,   QPaletteButton);
      COLOR_BTNSHADOW               : Result:=GetBrush(QPaletteActive,   QPaletteDark);
      COLOR_GRAYTEXT                : Result:=GetBrush(QPaletteActive,   QPaletteText);
      COLOR_BTNTEXT                 : Result:=GetBrush(QPaletteActive,   QPaletteButtonText);
      COLOR_BTNHIGHLIGHT            : Result:=GetBrush(QPaletteActive,   QPaletteLight);
      COLOR_3DDKSHADOW              : Result:=GetBrush(QPaletteActive,   QPaletteShadow);
      COLOR_3DLIGHT                 : Result:=GetBrush(QPaletteActive,   QPaletteMidlight);
      COLOR_INFOTEXT                : Result:=GetBrush(QPaletteInActive, QPaletteToolTipText);
      COLOR_INFOBK                  : Result:=GetBrush(QPaletteInActive, QPaletteToolTipBase);
      COLOR_HOTLIGHT                : Result:=GetBrush(QPaletteActive,   QPaletteLight);

      // qt does not provide any methods to retrieve titlebar colors
    {$IFNDEF MSWINDOWS}
      COLOR_ACTIVECAPTION           : Result:=GetBrush(QPaletteActive,   QPaletteHighlight);
      COLOR_INACTIVECAPTION         : Result:=GetBrush(QPaletteInActive, QPaletteHighlight);
      COLOR_CAPTIONTEXT             : Result:=GetBrush(QPaletteActive,   QPaletteHighlightedText);
      COLOR_INACTIVECAPTIONTEXT     : Result:=GetBrush(QPaletteInactive, QPaletteHighlightedText);
      COLOR_GRADIENTACTIVECAPTION   : Result:=GetBrush(QPaletteActive,   QPaletteBase);
      COLOR_GRADIENTINACTIVECAPTION : Result:=GetBrush(QPaletteInactive, QPaletteBase);
    {$ELSE}
      COLOR_ACTIVECAPTION           : Result:=GetSolidBrush(Windows.GetSysColor(COLOR_ACTIVECAPTION));
      COLOR_INACTIVECAPTION         : Result:=GetSolidBrush(Windows.GetSysColor(COLOR_INACTIVECAPTION));
      COLOR_CAPTIONTEXT             : Result:=GetSolidBrush(Windows.GetSysColor(COLOR_CAPTIONTEXT));
      COLOR_INACTIVECAPTIONTEXT     : Result:=GetSolidBrush(Windows.GetSysColor(COLOR_INACTIVECAPTIONTEXT));
      COLOR_GRADIENTACTIVECAPTION   : Result:=GetSolidBrush(Windows.GetSysColor(COLOR_GRADIENTACTIVECAPTION));
      COLOR_GRADIENTINACTIVECAPTION : Result:=GetSolidBrush(Windows.GetSysColor(COLOR_GRADIENTINACTIVECAPTION));
    {$ENDIF}
      COLOR_MENU                    : Result:=GetBrush(QPaletteActive,   QPaletteButton, 'QMenu');
      COLOR_MENUTEXT                : Result:=GetBrush(QPaletteActive,   QPaletteButtonText, 'QMenu');
      COLOR_MENUHILIGHT             : Result:=GetBrush(QPaletteDisabled, QPaletteHighlight, 'QMenu');
      COLOR_MENUBAR                 : Result:=GetBrush(QPaletteActive,   QPaletteButton, 'QMenu');
      COLOR_FORM                    : Result:=GetBrush(QPaletteActive,   QPaletteWindow);
    else
      Result:=0;
    end;
    FSysColorBrushes[nIndex] := Result;
  end
  else
    Result := FSysColorBrushes[nIndex];
end;*)

{------------------------------------------------------------------------------
  Function: GetSystemMetrics
  Params: nIndex Integer
  Returns: Integer

  It returns the value for the System Metric requested in nIndex.
  SM_.. constants are declared in LCLTypes
 ------------------------------------------------------------------------------}
function TCDWidgetSet.GetSystemMetrics(nIndex: Integer): Integer;
var
  R: TRect;
begin
  {$ifdef VerboseCDX11WinAPI}
    WriteLn(Format('Trace:> [TCDWidgetSet.GetSystemMetrics] %d', [nIndex]));
  {$endif}
  Result := 0;
  case nIndex of
    SM_ARRANGE:
      begin
        {$ifdef VerboseQtWinAPI}
          WriteLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_ARRANGE          ');
        {$endif}
      end;
    SM_CLEANBOOT:
      begin
        {$ifdef VerboseQtWinAPI}
          WriteLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CLEANBOOT          ');
        {$endif}
      end;
    SM_CMONITORS:
      Result := 1; //QDesktopWidget_numScreens(QApplication_desktop());
    SM_CMOUSEBUTTONS:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CMOUSEBUTTONS    ');
      end;
    SM_CXBORDER, SM_CYBORDER:
      begin
        // size of frame around controls
        Result:= 1;
        {Result := QStyle_pixelMetric(QApplication_style(),
                    QStylePM_DefaultFrameWidth, nil, nil);}
      end;
    SM_CXCURSOR:
      begin
        Result := 32; // recomended in docs
      end;
    SM_CYCURSOR:
      begin
        Result := 32; // recomended in docs
      end;
    SM_CXDOUBLECLK:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CXDOUBLECLK      ');
      end;
    SM_CYDOUBLECLK:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CYDOUBLECLK      ');
      end;
    SM_CXDRAG:
      begin
        Result := 2;
      end;
    SM_CYDRAG:
      begin
        Result := 2;
      end;
    SM_CXEDGE:
      begin
        Result := 2;
      end;
    SM_CYEDGE:
      begin
        Result := 2;
      end;
    SM_CXFIXEDFRAME:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CXFIXEDFRAME     ');
      end;
    SM_CYFIXEDFRAME:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CYFIXEDFRAME     ');
      end;
    SM_CXFULLSCREEN:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CXFULLSCREEN     ');
      end;
    SM_CYFULLSCREEN:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CYFULLSCREEN     ');
      end;
    SM_CXHTHUMB:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CXHTHUMB         ');
      end;
    SM_CXICON,
    SM_CYICON:
      begin
        Result := 32;
      end;
    SM_CXICONSPACING:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CXICONSPACING    ');
      end;
    SM_CYICONSPACING:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CYICONSPACING    ');
      end;
    SM_CXMAXIMIZED:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CXMAXIMIZED      ');
      end;
    SM_CYMAXIMIZED:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CYMAXIMIZED      ');
      end;
    SM_CXMAXTRACK:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CXMAXTRACK       ');
      end;
    SM_CYMAXTRACK:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CYMAXTRACK       ');
      end;
    SM_CXMENUCHECK:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CXMENUCHECK      ');
      end;
    SM_CYMENUCHECK:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CYMENUCHECK      ');
      end;
    SM_CXMENUSIZE:
      begin
        //Result := QStyle_pixelMetric(QApplication_style(), QStylePM_IndicatorWidth, nil, nil);
      end;
    SM_CYMENUSIZE:
      begin
        //Result := QStyle_pixelMetric(QApplication_style(), QStylePM_IndicatorHeight, nil, nil);
      end;
    SM_CXMIN:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CXMIN            ');
      end;
    SM_CYMIN:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CYMIN            ');
      end;
    SM_CXMINIMIZED:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CXMINIMIZED      ');
      end;
    SM_CYMINIMIZED:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CYMINIMIZED      ');
      end;
    SM_CXMINSPACING:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CXMINSPACING     ');
      end;
    SM_CYMINSPACING:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CYMINSPACING     ');
      end;
    SM_CXMINTRACK:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CXMINTRACK       ');
      end;
    SM_CYMINTRACK:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CYMINTRACK       ');
      end;
    SM_CXSCREEN:
      begin
        //QDesktopWidget_screenGeometry(QApplication_desktop(), @R, QDesktopWidget_primaryScreen(QApplication_desktop()));
        //Result := R.Right - R.Left;
        Result := XDisplayWidth(FDisplay,0);
      end;
    SM_CYSCREEN:
      begin
        //QDesktopWidget_screenGeometry(QApplication_desktop(), @R, QDesktopWidget_primaryScreen(QApplication_desktop()));
        //Result := R.Bottom - R.Top;
      Result := XDisplayHeight(FDisplay,0);
      end;
    SM_CXSIZE:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CXSIZE           ');
      end;
    SM_CYSIZE:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CYSIZE           ');
      end;
    SM_CXSIZEFRAME,
    SM_CYSIZEFRAME:
      begin
        //Result := QStyle_pixelMetric(QApplication_style(), QStylePM_MDIFrameWidth, nil, nil);
      end;
    SM_CXSMICON,
    SM_CYSMICON:
      begin
        Result := 16
      end;
    SM_CXSMSIZE:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CXSMSIZE         ');
      end;
    SM_CYSMSIZE:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CYSMSIZE         ');
      end;
    SM_CXVIRTUALSCREEN:
      begin
        //Result := QWidget_width(QApplication_desktop);
        Result := XDisplayWidth(FDisplay,0);
      end;
    SM_CYVIRTUALSCREEN:
      begin
        //Result := QWidget_height(QApplication_desktop);
        Result := XDisplayHeight(FDisplay,0);
      end;
    SM_CXVSCROLL,
    SM_CYVSCROLL,
    SM_CXHSCROLL,
    SM_CYHSCROLL:
      begin
        //Result := QStyle_pixelMetric(QApplication_Style, QStylePM_ScrollBarExtent, nil, nil);
      end;
    SM_CYCAPTION:
      begin
        //Result := QStyle_pixelMetric(QApplication_Style, QStylePM_TitleBarHeight, nil, nil);
      end;
    SM_CYKANJIWINDOW:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CYKANJIWINDOW    ');
      end;
    SM_CYMENU:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CYMENU           ');
      end;
    SM_CYSMCAPTION:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CYSMCAPTION      ');
      end;
    SM_CYVTHUMB:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CYVTHUMB         ');
      end;
    SM_DBCSENABLED:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_DBCSENABLED      ');
      end;
    SM_DEBUG:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_DEBUG            ');
      end;
    SM_MENUDROPALIGNMENT:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_MENUDROPALIGNMENT');
      end;
    SM_MIDEASTENABLED:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_MIDEASTENABLED   ');
      end;
    SM_MOUSEPRESENT:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_MOUSEPRESENT     ');
      end;
    SM_MOUSEWHEELPRESENT:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_MOUSEWHEELPRESENT');
      end;
    SM_NETWORK:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_NETWORK          ');
      end;
    SM_PENWINDOWS:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_PENWINDOWS       ');
      end;
    SM_SECURE:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_SECURE           ');
      end;
    SM_SHOWSOUNDS:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_SHOWSOUNDS       ');
      end;
    SM_SLOWMACHINE:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_SLOWMACHINE      ');
      end;
    SM_SWAPBUTTON:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_SWAPBUTTON       ');
      end;
  end;
end;

(*{------------------------------------------------------------------------------
  Function: GetTextColor
  Params:  DC     - A device context
  Returns: TColorRef

  Gets the Font Color currently assigned to the Device Context
 ------------------------------------------------------------------------------}
function TQtWidgetSet.GetTextColor(DC: HDC) : TColorRef;
var
  Color: TQColor;
  QtDC: TQtDeviceContext;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('[WinAPI GetTextColor]');
  {$endif}

  Result := 0;

  if IsValidDC(DC) then
  begin
    QtDC := TQtDeviceContext(DC);
    ColorRefToTQColor(TColorRef(QtDC.vTextColor), Color);
    TQColorToColorRef(Color, Result);
  end;
end;

{------------------------------------------------------------------------------
  Function: GetTextExtentExPoint
  Params: http://msdn.microsoft.com/en-us/library/dd144935%28VS.85%29.aspx
  Returns: True on success
 ------------------------------------------------------------------------------}
function TQtWidgetSet.GetTextExtentExPoint(DC: HDC; Str: PChar; Count,
  MaxWidth: Integer; MaxCount, PartialWidths: PInteger; var Size: TSize
  ): Boolean;
var
  i: Integer;
  w: Integer;
  AStr: WideString;
  Accu: Integer;
begin
  Result := False;
  if not IsValidDC(DC) then Exit;
  with TQtDeviceContext(DC) do
  begin
    AStr := GetUtf8String(Str);
    Size.cx := 0;
    Size.cY := Font.Metrics.Height;
    if PartialWidths = nil then
    begin
      if MaxCount <> nil then
      begin
        Size.cx := Font.Metrics.width(@AStr);
        Accu := 0;
        if MaxWidth <= 0 then
          MaxCount^ := 0
        else
          for i := 0 to Count - 1 do
          begin
            W := QFontMetrics_charWidth(Font.Metrics.FHandle, @AStr, i);
            Accu := Accu + W;
            if Accu <= MaxWidth then
              MaxCount^ := i + 1
            else
              break;
          end;
      end;
    end else
    begin
      if MaxCount <> nil then
        MaxCount^ := 0;
      for i := 0 to Count - 1 do
      begin
        w := QFontMetrics_charWidth(Font.Metrics.FHandle, @AStr, i);
        Inc(Size.cx, w);
        if MaxCount <> nil then
        begin
          if Size.cx <= MaxWidth then
          begin
            inc(MaxCount^);
            PartialWidths[i] := Size.cx;
          end else
          begin
            Dec(Size.cx, w);
            break;
          end;
        end else
          PartialWidths[i] := Size.cx;
      end;
    end;
  end;
  Result := True;
end;*)

{$ifdef CD_UseNativeText}
{------------------------------------------------------------------------------
  Function: GetTextExtentPoint
  Params:  none
  Returns: Nothing
 ------------------------------------------------------------------------------}
function TCDWidgetSet.GetTextExtentPoint(DC: HDC; Str: PChar; Count: Integer; var Size: TSize): Boolean;
var
  FontData: PXftFont;
  Descriptor: string;
  extents: TXGlyphInfo;
  LazDC: TLazCanvas absolute DC;
begin
  {$ifdef VerboseCDWinAPI}
    DebugLn('[WinAPI GetTextExtentPoint]');
  {$endif}

  Result := False;

  // Create the handle
  // 'Arial-10');
  Descriptor := 'Sans-10';
  FontData := XftFontOpenName(FDisplay, XDefaultScreen(FDisplay), PChar(Descriptor));
  if not Assigned(FontData) then raise Exception.Create('[TCDWidgetSet.GetTextExtentPoint] X11: Error creating font');

  // Get the size
  if Length(AText) = 0 then
  begin
    Size.cx := 0;
    Size.cy := 0;
  end
  else
  begin
    XftTextExtentsUtf8(FDisplay, FontData, Str, Count, extents);
    Size.cx := extents.xOff;
    Size.cy := extents.yOff;
  end;

  // Cleanup
  XftFontClose(FDisplay, FontData);

  Result := True;
end;

{------------------------------------------------------------------------------
  Function: GetTextMetrics
  Params:  DC     - A device context with a font selected
           TM     - The structure to receive the font information
  Returns: If successfull
 ------------------------------------------------------------------------------}
function TCDWidgetSet.GetTextMetrics(DC: HDC; var TM: TTextMetric): Boolean;
var
  FontData: PXftFont;
  Descriptor: string;
  LazDC: TLazCanvas absolute DC;
begin
  {$ifdef VerboseCDWinAPI}
    WriteLn('[WinAPI GetTextMetrics]');
  {$endif}

  Result := IsValidDC(DC);
  if not Result then Exit;

  TM.tmHeight := 10;//QtFontMetrics.height;
  TM.tmAscent := FontData^.Ascent;
  TM.tmDescent := FontData^.Descent;
  TM.tmInternalLeading := 0;
//  TM.tmExternalLeading := QtFontMetrics.leading;
//  TM.tmAveCharWidth := QtFontMetrics.charWidth('x',0);
//  TM.tmMaxCharWidth := QtFontMetrics.maxWidth;
{  case FontWeight of
    25: TM.tmWeight := FW_LIGHT;
    50: TM.tmWeight := FW_NORMAL;
    63: TM.tmWeight := FW_SEMIBOLD;
    75: TM.tmWeight := FW_BOLD;
    87: TM.tmWeight := FW_HEAVY;
    else
      TM.tmWeight := Round(FontWeight * 9.5);
  end;}
  TM.tmOverhang := 0;
  TM.tmDigitizedAspectX := 0;
  TM.tmDigitizedAspectY := 0;
  TM.tmFirstChar := 'a';
  TM.tmLastChar := 'z';
  TM.tmDefaultChar := 'x';
  TM.tmBreakChar := '?';
  TM.tmItalic := 0;//Ord(QtDC.Font.getItalic);
  TM.tmUnderlined := 0;//Ord(QtDC.Font.getUnderline);
  TM.tmStruckOut := 0;//Ord(QtDC.Font.getStrikeOut);

//  QtDC.font.family(@FontFamily);

  { Defaults to a TrueType font.
    Note that the meaning of the FIXED_PITCH constant is the opposite of
    the name implies, according to MSDN docs. Just a small inconsistency
    on Windows API that we have to mimic. }
//  if QtDC.font.fixedPitch then
//    TM.tmPitchAndFamily := TRUETYPE_FONTTYPE
//  else
    TM.tmPitchAndFamily := FIXED_PITCH or TRUETYPE_FONTTYPE;

  TM.tmCharSet := DEFAULT_CHARSET;
end;
{$endif}

(*function TQtWidgetSet.GetViewPortExtEx(DC: HDC; Size: PSize): Integer;
var
  R: TRect;
begin
  if IsValidDC(DC) and (Size <> nil) then
  begin
    QPainter_ViewPort(TQtDeviceContext(DC).Widget, @R);
    Size^.cx := R.Right - R.Left;
    Size^.cy := R.Bottom - R.Top;
    Result := Integer(True);
  end else
    Result := Integer(False);
end;

function TQtWidgetSet.GetViewPortOrgEx(DC: HDC; P: PPoint): Integer;
var
  R: TRect;
begin
  if IsValidDC(DC) and (P <> nil) then
  begin
    QPainter_ViewPort(TQtDeviceContext(DC).Widget, @R);
    P^ := R.TopLeft;
    Result := Integer(True);
  end else
    Result := Integer(False);
end;

function TQtWidgetSet.GetWindowExtEx(DC: HDC; Size: PSize): Integer;
var
  R: TRect;
begin
  if IsValidDC(DC) and (Size <> nil) then
  begin
    QPainter_Window(TQtDeviceContext(DC).Widget, @R);
    Size^.cx := R.Right - R.Left;
    Size^.cy := R.Bottom - R.Top;
    Result := Integer(True);
  end else
    Result := Integer(False);
end;

function TQtWidgetSet.GetWindowLong(Handle : hwnd; int: Integer): PtrInt;
begin
  Result := 0;
  {$ifdef VerboseQtWinAPI_MISSING_IMPLEMENTATION}
    WriteLn('***** [WinAPI TQtWidgetSet.GetWindowLong] missing implementation ');
  {$endif}
end;

{------------------------------------------------------------------------------
  Method:  GetWindowOrgEx
  Params:  DC    -
  Returns:
 ------------------------------------------------------------------------------}
function TQtWidgetSet.GetWindowOrgEx(dc: hdc; P: PPoint): Integer;
var
  Matrix: QTransformH;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('Trace: > [WinAPI GetWindowOrgEx]');
  {$endif}
  Result := 0;
  if not IsValidDC(DC) and (P<>nil) then
  begin
    {$ifdef VerboseQtWinAPI}
      WriteLn('Trace: < [WinAPI GetWindowOrgEx] No valid DC or P is nil');
    {$endif}
    exit;
  end;

  Matrix := QPainter_transform(TQtDeviceContext(DC).Widget);
  if Matrix <> nil then
  begin
    P^.X := -Trunc(QTransform_Dx(Matrix));
    P^.Y := -Trunc(QTransform_Dy(Matrix));
    Result := 1;
  end;
  {$ifdef VerboseQtWinAPI}
    WriteLn('Trace: < [WinAPI GetWindowOrgEx] Result=', dbgs(p^));
  {$endif}
end;


{------------------------------------------------------------------------------
  Method:  GetWindowRect
  Params:  Handle - handle of window
           Rect   - record for window coordinates
  Returns: if the function succeeds, the return value is nonzero; if the
           function fails, the return value is zero

  Retrieves the dimensions of the bounding rectangle of the specified window.
 ------------------------------------------------------------------------------}
function TQtWidgetSet.GetWindowRect(Handle: HWND; var ARect: TRect): Integer;
var
  APos: TQtPoint;
  R: TRect;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('[WinAPI GetWindowRect]');
  {$endif}

  Result := 0;
  if not IsValidHandle(Handle) then
    exit;
  APos := QtPoint(0,0);
  QWidget_mapToGlobal(TQtWidget(Handle).Widget, @APos, @APos);

  R := TQtWidget(Handle).getFrameGeometry;
  ARect := Bounds(APos.X,APos.Y,R.Right-R.Left,R.Bottom-R.Top);

  Result := -1;
end;*)

{------------------------------------------------------------------------------
  Function: GetWindowRelativePosition
  Params:  Handle : HWND;
  Returns: true on success

  returns the current widget Left, Top, relative to the client origin of its
  parent
 ------------------------------------------------------------------------------}
function TCDWidgetSet.BackendGetWindowRelativePosition(Handle: HWND; var Left, Top: integer): boolean;
var
  R: TRect;
begin
  {$ifdef VerboseCDWinAPI}
    WriteLn('[WinAPI BackendGetWindowRelativePosition]');
  {$endif}
{  if Handle = 0 then}
    Exit(False);
{  R := TQtWidget(Handle).getFrameGeometry;
  Left := R.Left;
  Top := R.Top;
  Result := True;}
end;

{------------------------------------------------------------------------------
  Function: GetWindowSize
  Params:  Handle : hwnd;
  Returns: true on success

  Returns the current widget Width and Height
 ------------------------------------------------------------------------------}
(*
{------------------------------------------------------------------------------
  Function: GradientFill
  Params: DC - DeviceContext to perform on
          Vertices - array of Points W/Color & Alpha
          NumVertices - Number of Vertices
          Meshes - array of Triangle or Rectangle Meshes,
                   each mesh representing one Gradient Fill
          NumMeshes - Number of Meshes
          Mode - Gradient Type, either Triangle,
                 Vertical Rect, Horizontal Rect

  Returns: true on success

  Performs multiple Gradient Fills, either a Three way Triangle Gradient,
  or a two way Rectangle Gradient, each Vertex point also supports optional
  Alpha/Transparency for more advanced Gradients.
 ------------------------------------------------------------------------------}
function TQtWidgetSet.GradientFill(DC: HDC; Vertices: PTriVertex;
  NumVertices : Longint;
  Meshes: Pointer; NumMeshes : Longint; Mode : Longint): boolean;

  function DoFillTriangle: Boolean; inline;
  begin
    Result := (Mode and GRADIENT_FILL_TRIANGLE) = GRADIENT_FILL_TRIANGLE;
  end;

  function DoFillVRect: Boolean; inline;
  begin
    Result := (Mode and GRADIENT_FILL_RECT_V) = GRADIENT_FILL_RECT_V;
  end;

  function VertexToColor(AVertex: tagTRIVERTEX): TQColor;
  var
    TheAlpha: Byte;
  begin
    TheAlpha := AVertex.Alpha shr 8;
    if TheAlpha = 0 then
      TheAlpha := 255;
    with AVertex do
      QColor_fromRgb(@Result, Red shr 8, Green shr 8, Blue shr 8, TheAlpha);
  end;

  function FillTriMesh(Mesh: tagGradientTriangle) : Boolean;
  var
    V1, V2, V3: tagTRIVERTEX;
    C1, C2, C3: TQColor;
    Grad: QConicalGradientH;
    Brush: QBrushH;
    Triangle: QPolygonH;
    R: TRect;
    Painter: QPainterH;
    Rgn: QRegionH;
  begin
    with Mesh do
    begin
      Result :=
        (Vertex1 < Cardinal(NumVertices)) and (Vertex2 >= 0) and
        (Vertex2 < Cardinal(NumVertices)) and (Vertex2 >= 0) and
        (Vertex3 < Cardinal(NumVertices)) and (Vertex3 >= 0);

      if (Vertex1 = Vertex2) or
        (Vertex1 = Vertex3) or
        (Vertex2 = Vertex3) or not Result then
        Exit;

      V1 := Vertices[Vertex1];
      V2 := Vertices[Vertex2];
      V3 := Vertices[Vertex3];

      Painter := TQtDeviceContext(DC).Widget;
      QPainter_save(Painter);
      Triangle := QPolygon_create(3);
      QPolygon_setPoint(Triangle, 0, V1.X, V1.Y);
      QPolygon_setPoint(Triangle, 1, V2.X, V2.Y);
      QPolygon_setPoint(Triangle, 2, V3.X, V3.Y);
      QPolygon_boundingRect(Triangle, @R);

      Dec(R.Bottom);
      Dec(R.Right);

      Rgn := QRegion_create(@R);

      // make our poly clip region , so gradient center is at real center
      QPainter_setClipRegion(Painter, Rgn, QtIntersectClip);

      Grad := QConicalGradient_create(R.Right div 2, R.Bottom div 2, 90);
      C1 := VertexToColor(V1);
      C2 := VertexToColor(V2);
      C3 := VertexToColor(V3);

      QGradient_setColorAt(Grad, 0.0, @C1); // open
      QGradient_setColorAt(Grad, 0.33, @C2); // left corner
      QGradient_setColorAt(Grad, 0.66, @C3); // right corner
      QGradient_setColorAt(Grad, 1.0, @C1); // close


      Brush := QBrush_create(Grad);
      QPainter_setPen(Painter, QtNoPen);
      QPainter_setBrush(Painter, Brush);

      // move center point down, so we remove reflections of C2 and C3
      // TODO: C1 reflection is still visible
      QPainter_setBrushOrigin(Painter, 0, R.Bottom div 5);
      QPainter_drawPolygon(Painter, Triangle);

      //TODO: now me must make it look "softer" because reflection look of
      // first color is ugly.

      QBrush_destroy(Brush);
      QPolygon_destroy(Triangle);
      QGradient_destroy(Grad);
      QRegion_destroy(Rgn);
      QPainter_restore(Painter);

    end;
  end;

  function FillRectMesh(Mesh: tagGradientRect) : boolean;
  var
    TL,BR: tagTRIVERTEX;
    StartColor, EndColor, SwapColor: TQColor;
    Swap: Longint;
    SwapColors: Boolean;
    Grad: QGradientH;
    Brush: QBrushH;
  begin
    with Mesh do
    begin
      Result :=
        (UpperLeft < Cardinal(NumVertices)) and (UpperLeft >= 0) and
        (LowerRight < Cardinal(NumVertices)) and (LowerRight >= 0);
      if (LowerRight = UpperLeft) or not Result then
        Exit;

      TL := Vertices[UpperLeft];
      BR := Vertices[LowerRight];
      SwapColors := (BR.Y < TL.Y) and (BR.X < TL.X);
      if BR.X < TL.X then
      begin
        Swap := BR.X;
        BR.X := TL.X;
        TL.X := Swap;
      end;
      if BR.Y < TL.Y then
      begin
        Swap := BR.Y;
        BR.Y := TL.Y;
        TL.Y := Swap;
      end;
      StartColor := VertexToColor(TL);
      EndColor := VertexToColor(BR);
      if SwapColors then
      begin
        SwapColor := StartColor;
        StartColor := EndColor;
        EndColor := SwapColor;
      end;
      if DoFillVRect then
        Grad := QLinearGradient_create(TL.X, TL.Y, TL.X, BR.Y)
      else
        Grad := QLinearGradient_create(TL.X, TL.Y, BR.X, TL.Y);
      QGradient_setColorAt(Grad, 0, @StartColor);
      QGradient_setColorAt(Grad, 1, @EndColor);
      Brush := QBrush_create(Grad);
      TQtDeviceContext(DC).fillRect(TL.X, TL.Y, BR.X - TL.X, BR.Y - TL.Y, Brush);
      QGradient_destroy(Grad);
      QBrush_destroy(Brush);
    end;
  end;

const
  MeshSize: Array[Boolean] of Integer = (
    SizeOf(tagGradientRect), SizeOf(tagGradientTriangle));
var
  i : Integer;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('***** [WinAPI TQtWidgetSet.GradientFill] ');
  {$endif}

  //Currently Alpha blending is ignored... Ideas anyone?
  Result := (Meshes <> nil) and (NumMeshes >= 1) and (NumVertices >= 2)
            and (Vertices <> nil);
  if Result and DoFillTriangle then
    Result := NumVertices >= 3;
  if Result then
  begin
    Result := False;

    //Sanity Checks For Vertices Size vs. Count
    if MemSize(Vertices) < PtrUInt(SizeOf(tagTRIVERTEX)*NumVertices) then
      exit;

    //Sanity Checks For Meshes Size vs. Count
    if MemSize(Meshes) < PtrUInt(MeshSize[DoFillTriangle]*NumMeshes) then
      exit;

    for I := 0 to NumMeshes - 1 do
    begin
      if DoFillTriangle then
      begin
        if not FillTriMesh(PGradientTriangle(Meshes)[I]) then
          exit;
      end
      else
      begin
        if not FillRectMesh(PGradientRect(Meshes)[I]) then
          exit;
      end;
    end;
    Result := True;
  end;
end;

function TQtWidgetSet.HideCaret(hWnd: HWND): Boolean;
begin
  Result := (hWnd <> 0) and QtCaret.HideCaret(TQtWidget(hWnd));
end;

{------------------------------------------------------------------------------
  Procedure: InitializeCriticalSection
  Params: var CritSection: TCriticalSection
  Returns:
 ------------------------------------------------------------------------------}
procedure TQtWidgetSet.InitializeCriticalSection(var CritSection: TCriticalSection);
var
  ACritSec: System.PRTLCriticalSection;
begin
  New(ACritSec);
  System.InitCriticalSection(ACritSec^);
  CritSection:=TCriticalSection(ACritSec);
end;

function TQtWidgetSet.IntersectClipRect(dc: hdc; Left, Top, Right, Bottom: Integer): Integer;
var
  QtDC: TQtDeviceContext absolute dc;
  IntersectRgn, Rgn: QRegionH;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('[WinAPI TQtWidgetSet.IntersectClipRect] L ',Left,' T ',Top,' R ',Right,' B ',Bottom);
  {$endif}
  Result := ERROR;
  if not IsValidDC(DC) then exit;

  IntersectRgn := QRegion_create(Left, Top, Right - Left, Bottom - Top);
  try
    if QtDC.getClipping then
    begin
      Rgn := QRegion_create;
      try
        QPainter_clipRegion(QtDC.Widget, Rgn);
        if QRegion_isEmpty(Rgn) then
          QtDC.setClipRegion(IntersectRgn)
        else
          QtDC.setClipRegion(IntersectRgn, QtIntersectClip);
        QtDC.setClipping(True);
        // recreate Rgn
        QRegion_destroy(Rgn);
        Rgn := QRegion_create;
        QPainter_clipRegion(QtDC.Widget, Rgn);
        Result := QtDC.GetRegionType(Rgn);
      finally
        QRegion_destroy(Rgn);
      end;
    end else
    begin
      QtDC.setClipRegion(InterSectRgn);
      QtDC.setClipping(True);
      Result := QtDC.GetRegionType(InterSectRgn);
    end;
  finally
    QRegion_destroy(IntersectRgn);
  end;
end;

function TQtWidgetSet.IsIconic(Handle: HWND): boolean;
begin
  Result := TQtWidget(Handle).isMinimized;
end;

function TQtWidgetSet.IsWindow(handle: HWND): boolean;
begin
  Result := IsValidHandle(Handle);
end;

function TQtWidgetSet.IsWindowEnabled(Handle: HWND): boolean;
begin
  Result := TQtWidget(Handle).getEnabled;
end;

function TQtWidgetSet.IsWindowVisible(Handle: HWND): boolean;
begin
  Result := TQtWidget(Handle).getVisible;
end;

function TQtWidgetSet.IsZoomed(Handle: HWND): boolean;
begin
  Result := TQtWidget(Handle).isMaximized;
end;*)

{------------------------------------------------------------------------------
  Function: InvalidateRect
  Params: aHandle:
          Rect:
          bErase:
  Returns:

 ------------------------------------------------------------------------------}

(*{------------------------------------------------------------------------------
  Function: InvalidateRgn
  Params: aHandle:
          Rect:
          bErase:
  Returns: True if invalidate is successfull.
  Invalidates region of widget.
 ------------------------------------------------------------------------------}
function TQtWidgetSet.InvalidateRgn(aHandle: HWND; Rgn: HRGN; Erase: Boolean
  ): Boolean;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('[WinAPI InvalidateRgn]');
  {$endif}
  if aHandle = 0 then
    exit(False);
  if IsValidGDIObject(Rgn) and (TQtRegion(Rgn).FHandle <> nil) then
    TQtWidget(aHandle).UpdateRegion(TQtRegion(Rgn).FHandle)
  else
    TQtWidget(aHandle).Update;
end;

{------------------------------------------------------------------------------
  Procedure: LeaveCriticalSection
  Params:  var CritSection: TCriticalSection
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtWidgetSet.LeaveCriticalSection(var CritSection: TCriticalSection);
var
  ACritSec: System.PRTLCriticalSection;
begin
  ACritSec:=System.PRTLCriticalSection(CritSection);
  System.LeaveCriticalsection(ACritSec^);
end;

{------------------------------------------------------------------------------
  Function: LineTo
  Params:  none
  Returns: Nothing


 ------------------------------------------------------------------------------}
function TQtWidgetSet.LineTo(DC: HDC; X, Y: Integer): Boolean;
var
  PenPos, LastPos: TPoint;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('[WinAPI LineTo]');
  {$endif}

  Result := False;

  if not IsValidDC(DC) then Exit;

  TQtDeviceContext(DC).getPenPos(@PenPos);
  LastPos := Point(X, Y);
  if TQtDeviceContext(DC).pen.getCosmetic then
    LastPos := TQtDeviceContext(DC).GetLineLastPixelPos(PenPos, LastPos);
  TQtDeviceContext(DC).drawLine(PenPos.X, PenPos.Y, LastPos.X, LastPos.Y);
  MoveToEx(DC, X, Y, nil);

  Result := True;
end;

function TQtWidgetSet.LPtoDP(DC: HDC; var Points; Count: Integer): BOOL;
var
  P: PPoint;
  QtPoint: TQtPoint;
  Matrix: QTransformH;
  QtDC: TQtDeviceContext;
begin
  Result := False;

  if not IsValidDC(DC) then
    Exit;

  QtDC := TQtDeviceContext(DC);

  Matrix := QPainter_transform(QtDC.Widget);
  P := @Points;
  while Count > 0 do
  begin
    Dec(Count);
    QtPoint.X := P^.X;
    QtPoint.Y := P^.Y;
    QTransform_map(Matrix, PQtPoint(@QtPoint), PQtPoint(@QtPoint));
    P^.X := QtPoint.X;
    P^.Y := QtPoint.Y;
    Inc(P);
  end;

  Result := True;
end;*)


(*{------------------------------------------------------------------------------
  Function: MoveToEx
  Params:  none
  Returns: Nothing
 ------------------------------------------------------------------------------}
function TQtWidgetSet.MoveToEx(DC: HDC; X, Y: Integer; OldPoint: PPoint): Boolean;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('[WinAPI MoveToEx]',
     ' DC:', dbghex(DC),
     ' X:', dbgs(X),
     ' Y:', dbgs(Y));
  {$endif}

  Result := False;

  if not IsValidDC(DC) then Exit;

  if (OldPoint <> nil) then TQtDeviceContext(DC).getPenPos(OldPoint);

  TQtDeviceContext(DC).setPenPos(X, Y);

  Result := True;
end;

function TQtWidgetSet.OffsetRgn(RGN: HRGN; nXOffset, nYOffset: Integer): Integer;
var
  QtRgn: QRegionH;
begin
  Result := ERROR;

  if not IsValidGDIObject(RGN) then
    Exit
  else
    QtRgn := TQtRegion(RGN).FHandle;

  QRegion_translate(QtRgn, nXOffset, nYOffset);

  if QRegion_isEmpty(QtRgn) then
    Result := NULLREGION
  else
  begin
    if TQtRegion(RGN).IsPolyRegion or (TQtRegion(RGN).numRects > 0) then
      Result := COMPLEXREGION
    else
      Result := SIMPLEREGION;
  end;
end;

function TQtWidgetSet.PeekMessage(var lpMsg : TMsg; Handle : HWND; wMsgFilterMin, wMsgFilterMax,wRemoveMsg : UINT): Boolean;
begin
  Result := False;
  {$ifdef VerboseQtWinAPI_MISSING_IMPLEMENTATION}
    WriteLn('***** [WinAPI TQtWidgetSet.PeekMessage] missing implementation ');
  {$endif}
end;

{------------------------------------------------------------------------------
  Function: PolyBezier
  Params:  DC: HDC; Points: PPoint; NumPts: Integer; Filled: Boolean;
           Continuous: Boolean
  Returns: Nothing
 ------------------------------------------------------------------------------}
function TQtWidgetSet.PolyBezier(DC: HDC; Points: PPoint; NumPts: Integer;
  Filled, Continuous: Boolean): Boolean;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('[WinAPI PolyBezier] DC: ', dbghex(DC));
  {$endif}
  Result := inherited PolyBezier(DC, Points, NumPts, Filled, Continuous);
end;

{------------------------------------------------------------------------------
  Function: Polygon
  Params:  DC: HDC; Points: PPoint; NumPts: Integer; Winding: Boolean
  Returns: Nothing
 ------------------------------------------------------------------------------}
function TQtWidgetSet.Polygon(DC: HDC; Points: PPoint; NumPts: Integer;
  Winding: Boolean): boolean;
var
  QtPoints: PQtPoint;
  i: integer;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('[WinAPI Polygon] DC: ', dbghex(DC));
  {$endif}
  Result := IsValidDC(DC);
  if Result then
  begin
    GetMem(QtPoints, NumPts * SizeOf(TQtPoint));
    for i := 0 to NumPts - 1 do
      QtPoints[i] := QtPoint(Points[i].x, Points[i].y);
    if Winding then
      QPainter_drawPolygon(TQtDeviceContext(DC).Widget, QtPoints, NumPts, QtWindingFill)
    else
      QPainter_drawPolygon(TQtDeviceContext(DC).Widget, QtPoints, NumPts, QtOddEvenFill);
    FreeMem(QtPoints);
  end;
end;

{------------------------------------------------------------------------------
  Function: Polyline
  Params:  DC: HDC; Points: PPoint; NumPts: Integer
  Returns: Nothing
 ------------------------------------------------------------------------------}
function TQtWidgetSet.Polyline(DC: HDC; Points: PPoint; NumPts: Integer): boolean;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('[WinAPI Polyline] DC: ', dbghex(DC));
  {$endif}
  Result := IsValidDC(DC) and (NumPts > 0);
  if Result then
    TQtDeviceContext(DC).DrawPolyLine(Points, NumPts);
end;

function TQtWidgetSet.PostMessage(Handle: HWND; Msg: Cardinal; wParam: WParam; lParam: LParam): Boolean;
var
  Widget: TQtWidget absolute Handle;
  Event: QLCLMessageEventH;
begin
  Result := False;
  if Handle <> 0 then
  begin
    Event := QLCLMessageEvent_create(QEventLCLMessage, Msg, wParam, lParam, 0);
    QCoreApplication_postEvent(Widget.Widget, Event, 1 {high priority});
    Result := True;
  end;
end;

function TQtWidgetSet.PtInRegion(RGN: HRGN; X, Y: Integer): Boolean;
begin
  Result := False;

  if not IsValidGDIObject(RGN) then
    exit;

  Result := TQtRegion(RGN).containsPoint(X, Y);
end;

{------------------------------------------------------------------------------
  Function: Rectangle
  Params:  DC: HDC; X1, Y1, X2, Y2: Integer
  Returns: Nothing

  The Rectangle function draws a rectangle. The rectangle is outlined by using
  the current pen and filled by using the current brush.
 ------------------------------------------------------------------------------}
function TCDWidgetSet.Rectangle(DC: HDC; X1, Y1, X2, Y2: Integer): Boolean;
var
  LazDC: TLazCanvas absolute DC;
begin
  if DC = 0 then Exit;

  {$ifdef VerboseWinAPI}
//  DebugLn(Format('[WinAPI Rectangle] DC=%s DC.Width=%d DC.Height=%d', [dbghex(DC), LazDC.Width, LazDC.Height]));
  DebugLn(Format('[WinAPI Rectangle] DC=%s', [dbghex(DC)]));
  DebugLn(Format('[WinAPI Rectangle] DC.Width=%d DC.Height=%d', [LazDC.Width, LazDC.Height]));
  {$endif}

  //if not IsValidDC(DC) then Exit(False);
  LazDC.Brush.FPColor := colWhite;
  LazDC.Rectangle(X1, Y1, X2, Y2);
{  R := NormalizeRect(Rect(X1, Y1, X2, Y2));
  if IsRectEmpty(R) then Exit(True);

  TQtDeviceContext(DC).drawRect(R.Left, R.Top, R.Right - R.Left - 1, R.Bottom - R.Top - 1);}
  Result := True;
end;

function TQtWidgetSet.RectVisible(dc : hdc; const ARect: TRect) : Boolean;
var
  QtDC: TQtDeviceContext;
begin
  {$ifdef VerboseQtWinAPI}
  writeln('[WinAPI RectVisible] ');
  {$endif}
  Result := False;
  if not IsValidDC(DC) then Exit;
  QtDC := TQtDeviceContext(DC);
  // as MSDN says only clipping region can play here
  if QtDC.getClipping then
    Result := QtDC.getClipRegion.containsRect(ARect);
end;

{------------------------------------------------------------------------------
  Function: RedrawWindow
  Params: Wnd:
          lprcUpdate:
          hrgnUpdate:
          flags:
  Returns:

 ------------------------------------------------------------------------------}
function TQtWidgetSet.RedrawWindow(Wnd: HWND; lprcUpdate: PRECT; hrgnUpdate: HRGN; flags: UINT): Boolean;
var
  QtWidget: TQtWidget;
  Region: TQtRegion;
begin
  if not IsValidHandle(Wnd) then
    Exit(False);

  QtWidget := TQtWidget(Wnd);
  if IsValidGDIObject(hrgnUpdate) then
    Region := TQtRegion(hrgnUpdate)
  else
    Region := nil;
  if (lprcUpdate = nil) and (hrgnUpdate = 0) then
  begin
    QtWidget.Update(nil);
    Exit(True);
  end;

  if Region = nil then
    Result := InvalidateRect(Wnd, lprcUpdate, False)
  else
    QtWidget.UpdateRegion(Region.FHandle);

  Result := True;
end;

function TQtWidgetSet.ReleaseCapture: Boolean;
var
  w: TQtWidget;
begin
  w := TQtWidget(GetCapture);
  Result := w <> nil;
  if Result then
  begin
    {$IFDEF MSWINDOWS}
    if w is TQtMainWindow then
      w.releaseMouse()
    else
      windows.ReleaseCapture;
    {$ELSE}
    w.releaseMouse();
    {$ENDIF}
  end;
  {$ifdef VerboseQtWinAPI}
  WriteLn('[WinAPI ReleaseCapture] Capture = ', THandle(w));
  {$endif}
end;

{------------------------------------------------------------------------------
  Function: ReleaseDC
  Params:     hWnd:       Handle to the window whose DC is to be released.
              hDC:        Handle to the DC to be released.
  Returns:
 ------------------------------------------------------------------------------}
function TQtWidgetSet.ReleaseDC(hWnd: HWND; DC: HDC): Integer;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('[WinAPI ReleaseDC]',
     ' hWnd: ', dbghex(hWnd),
     ' DC: ', dbghex(DC));
  {$endif}

  Result := 0;

  if IsValidDC(DC) then Exit;

  Result := 1;
end;


{------------------------------------------------------------------------------
  Function: RestoreDC: Restore a previously saved DC state
  Params:
    DC: Handle to a DeviceContext
    SavedDC: Index of saved state that needs to be restored
  Returns: True if state was successfuly restored.
-------------------------------------------------------------------------------}
function TQtWidgetSet.RestoreDC(DC: HDC; SavedDC: Integer): Boolean;
var
  DCData: PQtDCData;
begin
  {$ifdef VerboseQTWinAPI}
  WriteLn('Trace:> [WinAPI RestoreDC] DC=', dbghex(DC),' SavedDC=',SavedDC);
  {$Endif}
  // if SavedDC is positive, it represents the wished saved dc instance
  // if SavedDC is negative, it's a relative number from last pushed state
  Result := False;
  if SavedDCList=nil then
  begin
    {$ifdef VerboseQTWinAPI}
    WriteLn('Trace:< [WinAPI RestoreDC] there is no List yet, result=', result);
    {$Endif}
    exit;
  end;

  if SavedDC < 0 then
    SavedDC := SavedDC + SavedDCList.Count;

  // check index
  Result := (SavedDC > 0) and (SavedDC < SavedDCList.Count);
  if Result then
  begin
    Result := true;
    while SavedDC > 0 do
    begin
      DCData := PQtDcData(SavedDCList[SavedDC]);
      SavedDCList.Delete(SavedDC);
      Result := TQtDeviceContext(DC).RestoreDCData(DCData);
      Dec(SavedDC);
    end;
  end;
  {$ifdef VerboseQTWinAPI}
  WriteLn('Trace:< [WinAPI RestoreDC]');
  {$Endif}
end;

function TQtWidgetSet.RoundRect(DC: hDC; X1, Y1, X2, Y2: Integer; RX,RY : Integer): Boolean;
begin
  Result := False;
  if not IsValidDC(DC) then
  begin
    {$ifdef VerboseQTWinAPI}
    WriteLn('Trace:< [WinAPI RoundRect] DC Invalid, result=', result);
    {$Endif}
    Exit;
  end;
  Result := inherited RoundRect(DC, X1, Y1, X2, Y2, RX, RY);
end;
{------------------------------------------------------------------------------
  Function: SaveDC: save DC state information to a stack
  Params:  DC
  Returns: The index assigned to the or 0 if DC is not valid
-------------------------------------------------------------------------------}
function TQtWidgetSet.SaveDC(DC: HDC): Integer;
var
  DCData: PQtDCData;
begin
  {$ifdef VerboseQTWinAPI}
  WriteLn('Trace:> [WinAPI SaveDC] DC=', dbghex(DC));
  {$Endif}

  result:=0;

  if not IsValidDC(DC) then
  begin
    {$ifdef VerboseQTWinAPI}
    WriteLn('Trace:< [WinAPI SaveDC] DC Invalid, result=', result);
    {$Endif}
    exit;
  end;

  if SavedDCList=nil then
  begin
    SavedDCList := TFPList.Create;
    SavedDCList.Add(nil); // start at index 1, 0 is an invalid saved state
  end;

  DCData := TQtDeviceContext(DC).CreateDCData;
  Result := 1;
  SavedDCList.Insert(Result, DCData);

  {$ifdef VerboseQTWinAPI}
  WriteLn('Trace:< [WinAPI SaveDC] result=', Result);
  {$Endif}
end;*)

{------------------------------------------------------------------------------
  Function: ScreenToClient
  Params:  Handle: HWND; var P: TPoint
  Returns:
-------------------------------------------------------------------------------}
function TCDWidgetSet.ScreenToClient(Handle : HWND; var P : TPoint) : Integer;
begin
  Result := 0;
end;

(*{------------------------------------------------------------------------------
  Method:  ScrollWindowEx
  Params:  HWnd       - handle of window to scroll
           DX         - horizontal amount to scroll
           DY         - vertical amount to scroll
           PRcScroll  - pointer to scroll rectangle
           PRcClip    - pointer to clip rectangle
           HRgnUpdate - handle of update region
           PRcUpdate  - pointer to update rectangle
           Flags      - scrolling flags

  Returns: True if succesfull

  The ScrollWindowEx function scrolls the content of the specified window's
  client area
 ------------------------------------------------------------------------------}
function TQtWidgetSet.ScrollWindowEx(HWnd: HWND; DX, DY: Integer; PRcScroll,
  PRcClip: PRect; HRgnUpdate: HRGN; PRcUpdate: PRect; Flags: UINT): Boolean;
var
  R: TRect;
  W: TQtWidget;
begin
  Result := False;
  if (HWND = 0) then exit;

  W := TQtWidget(HWND);
  if ((Flags and SW_SCROLLCHILDREN) <> 0) then
    W.scroll(dx, dy, nil)
  else
  if (PrcScroll = nil) then
  begin
    R := W.getClientBounds;
    W.scroll(dx, dy, @R);
  end
  else
    W.scroll(dx, dy, PRcScroll);

  if ((Flags and SW_INVALIDATE) <> 0) then
  begin
    if IsValidGDIObject(HRgnUpdate) then
    begin
      R := TQtRegion(HRgnUpdate).getBoundingRect;
      PRcUpdate := @R;
      W.Update(@R);
    end else
    if PRcClip <> nil then
    begin
      PRcUpdate := PRcClip;
      W.Update(PrcClip);
    end;
  end;

  Result := True;
end;

{------------------------------------------------------------------------------
  Function: SelectClipRGN
  Params:  DC, RGN
  Returns: longint

  Sets the DeviceContext's ClipRegion. The Return value
  is the new clip regions type, or ERROR.

  The result can be one of the following constants
      Error
      NullRegion
      SimpleRegion
      ComplexRegion

 ------------------------------------------------------------------------------}
function TQtWidgetSet.SelectClipRGN(DC: hDC; RGN: HRGN): Longint;
var
  QtDC: TQtDeviceContext;
  EmptyRegion: QRegionH;
  P: TPoint;
begin
  Result := ERROR;
  if IsValidDC(DC) then
  begin
    QtDC := TQtDeviceContext(DC);
    if IsValidGDIObject(RGN) then
    begin
      Result := TQtRegion(Rgn).GetRegionType;
      // RGN is in Device coordinates. Qt expects logical coordinates
      // so we need to convert RGN coords.
      GetWindowOrgEx(DC, @P);
      TQtRegion(Rgn).translate(P.X, P.Y);
      QtDC.setClipRegion(TQtRegion(Rgn).FHandle);
    end else
    begin
      EmptyRegion := QRegion_create;
      try
        QtDC.setClipRegion(EmptyRegion, QtNoClip);
      finally
        QRegion_destroy(EmptyRegion);
      end;
      Result := NULLREGION;
    end;
  end;
end;

{------------------------------------------------------------------------------
  Function: SelectObject
  Params:  none
  Returns: The GDI object of the same type previously associated with the DC

  Changes one of the GDI objects (Font, Brush, etc) of a Device Context;
 ------------------------------------------------------------------------------}
function TQtWidgetSet.SelectObject(DC: HDC; GDIObj: HGDIOBJ): HGDIOBJ;
var
  aObject: TObject;
  {$ifdef VerboseQtWinAPI}
    ObjType: string;
  {$endif}
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('Trace:> [WinAPI SelectObject]',
    ' DC=', dbghex(DC),
    ' GDIObj=', dbghex(GDIObj));
  {$endif}

  Result := 0;

  if not IsValidDC(DC) then
  begin
    {$ifdef VerboseQtWinAPI}
      WriteLn('Trace:< [WinAPI SelectObject] Invalid DC');
    {$endif}

    Exit;
  end;

  if not IsValidGDIObject(GDIObj) then
  begin
    {$ifdef VerboseQtWinAPI}
      WriteLn('Trace:< [WinAPI SelectObject] Invalid GDI Object');
    {$endif}

    Exit;
  end;

  aObject := TObject(GDIObj);

  if aObject is TQtFont then
  begin
    {$ifdef VerboseQtWinAPI}
      ObjType := 'Font';
    {$endif}

    Result := HGDIOBJ(TQtDeviceContext(DC).font);

    TQtDeviceContext(DC).setFont(TQtFont(aObject));
  end
  else if aObject is TQtPen then
  begin
    {$ifdef VerboseQtWinAPI}
      ObjType := 'Pen'      ;
    {$endif}
    result := HGDIOBJ(TQtDeviceContext(DC).pen);

    TQtDeviceContext(DC).setPen(TQtPen(aObject));
  end
  else if aObject is TQtBrush then
  begin
    {$ifdef VerboseQtWinAPI}
      ObjType := 'Brush';
    {$endif}

    Result := HGDIOBJ(TQtDeviceContext(DC).brush);

    TQtDeviceContext(DC).setBrush(TQtBrush(aObject));
  end
  else if aObject is TQtImage then
  begin
    {$ifdef VerboseQtWinAPI}
      ObjType := 'Image';
    {$endif}

    Result := HGDIOBJ(TQtDeviceContext(DC).vImage);

    // TODO: is this also saved in qpainter_save?
    TQtDeviceContext(DC).setImage(TQtImage(aObject));
  end else
  if AObject is TQtRegion then
  begin
    Result := HGDIOBJ(TQtDeviceContext(DC).getClipRegion);
    SelectClipRGN(DC, HRGN(GDIObj));
  end;

  {$ifdef VerboseQtWinAPI}
    WriteLn('Trace:< [WinAPI SelectObject] Result=', dbghex(Result), ' ObjectType=', ObjType);
  {$endif}
end;

function TQtWidgetSet.SendMessage(HandleWnd: HWND; Msg: Cardinal;
  WParam: WParam; LParam: LParam): LResult;
var
  Widget: TQtWidget absolute HandleWnd;
  Event: QLCLMessageEventH;
begin
  Result := 0;
  if (HandleWnd <> 0) and (Widget.Widget <> nil) then
  begin
    Event := QLCLMessageEvent_create(QEventLCLMessage, Msg, wParam, lParam, 0);
    try
      QCoreApplication_sendEvent(Widget.Widget, Event);
      Result := QLCLMessageEvent_getMsgResult(Event);
    finally
      QLCLMessageEvent_destroy(Event);
    end;
  end;
end;

function TQtWidgetSet.SetActiveWindow(Handle: HWND): HWND;
begin
  Result := GetActiveWindow;

  if Handle <> 0 then
    TQtWidget(Handle).Activate
  else
    Result := 0; // error
end;

{------------------------------------------------------------------------------
  Function: SetBKColor
  Params: X:
          Y:
  Returns:

 ------------------------------------------------------------------------------}
function TQtWidgetSet.SetBKColor(DC: HDC; Color: TColorRef): TColorRef;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('Trace:> [WinAPI SetBkColor]',
     ' DC: ', dbghex(DC),
     ' Color: ', dbgs(Color));
  {$endif}

  Result := 0;

  if not IsValidDC(DC) then
  begin
    {$ifdef VerboseQtWinAPI}
      WriteLn('Trace:< [WinAPI SetBkColor] Invalid DC');
    {$endif}

    Exit;
  end;

  Result := TQtDeviceContext(DC).SetBkColor(TColorRef(Color));
end;

{------------------------------------------------------------------------------
  Method:  SetBkMode
  Params:  DC    -
  Returns:
 ------------------------------------------------------------------------------}
function TQtWidgetSet.SetBkMode(DC: HDC; bkMode: Integer): Integer;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('Trace:> [WinAPI SetBkMode] DC=', dbghex(DC), ' BkMode=', dbgs(bkMode));
  {$endif}

  Result := 0;

  if not IsValidDC(DC) then
  begin
    {$ifdef VerboseQtWinAPI}
      WriteLn('Trace:< [WinAPI SetBkMode] Invalid DC');
    {$endif}

    Exit;
  end;

  Result := TQtDeviceContext(DC).SetBkMode(bkMode);
end;

function TQtWidgetSet.SetCapture(AHandle: HWND): HWND;
var
  Message: TLMessage;
begin
  Result := GetCapture;
  if Result <> AHandle then
  begin
    if Result <> 0 then
      ReleaseCapture;
    if AHandle <> 0 then
     {$IFDEF MSWINDOWS}
      Windows.SetCapture(AHandle);
     {$ELSE}
      TQtWidget(AHandle).grabMouse();
     {$ENDIF}
    {$ifdef VerboseQtWinAPI}
      WriteLn('[WinAPI SetCapture] Capture = ', Result, ' New capture = ', AHandle);
    {$endif}
    if Result <> 0 then
    begin
      Message.Msg := 0;
      FillChar(Message, SizeOf(Message), 0);
      Message.msg := LM_CAPTURECHANGED;
      Message.wParam := 0;
      Message.lParam := Result;
      LCLMessageGlue.DeliverMessage(TQtWidget(AHandle).LCLObject, Message);
    end;
  end;
end;

function TQtWidgetSet.SetCaretPos(X, Y: Integer): Boolean;
begin
  Result := QtCaret.SetCaretPos(X, Y);
end;

function TQtWidgetSet.SetCaretPosEx(Handle: HWnd; X, Y: Integer): Boolean;
begin
  Result := QtCaret.SetCaretPos(X, Y);
end;

function TQtWidgetSet.SetCaretRespondToFocus(handle: HWND;
  ShowHideOnFocus: boolean): Boolean;
begin
  Result := True;
  QtCaret.SetQtCaretRespondToFocus(ShowHideOnFocus);
end;

{------------------------------------------------------------------------------
  Function: SetCursor
  Params: ACursor - HCursor (TQtCursor)
  Returns:
       previous global cursor
 ------------------------------------------------------------------------------}
function TQtWidgetSet.SetCursor(ACursor: HCURSOR): HCURSOR;
begin
  Result := HCURSOR(OverrideCursor);

  if Result = ACursor then
    Exit;

  if Screen.Cursors[crDefault] = ACursor then
    OverrideCursor := nil
  else
    OverrideCursor := TQtCursor(ACursor);
end;

{------------------------------------------------------------------------------
  Function: SetCursorPos
  Params: X:
          Y:
  Returns:

 ------------------------------------------------------------------------------}
function TQtWidgetSet.SetCursorPos(X, Y: Integer): Boolean;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('[WinAPI SetCursorPos]');
  {$endif}

  QCursor_setPos(X, Y);

  Result := True;
end;*)

{------------------------------------------------------------------------------
  Function: SetFocus
  Params: hWnd   - Window handle to be focused
  Returns:

 ------------------------------------------------------------------------------}

(*function TQtWidgetSet.GetForegroundWindow: HWND;
var
  W: QWidgetH;
begin
  {$IFDEF HASX11}
  if WindowManagerName = 'metacity' then
    W := X11GetActivewindow
  else
    W := QApplication_activeWindow();
  {$ELSE}
  W := QApplication_activeWindow();
  {$ENDIF}
  Result := HwndFromWidgetH(W);
end;

function TQtWidgetSet.SetForegroundWindow(HWnd: HWND): boolean;
begin
  Result := False;
  if HWND <> 0 then
  begin
    Result := TQtWidget(HWND).IsActiveWindow;
    TQtWidget(HWnd).Activate;
  end;
end;

function TQtWidgetSet.SetMenu(AWindowHandle: HWND; AMenuHandle: HMENU): Boolean;
var
  AWidget, AMenuWidget: TQtWidget;
  QtMainWindow: TQtMainWindow absolute AWidget;
  QtMenuBar: TQtMenuBar absolute AMenuWidget;
  R, R1: TRect;
begin
  AWidget := TQtWidget(AWindowHandle);
  Result := AWidget is TQtMainWindow;
  if Result then
  begin
    AMenuWidget := TQtWidget(AMenuHandle);
    if AMenuWidget is TQtMenuBar then
    begin
      R := AWidget.LCLObject.ClientRect;
      R1 := QtMainWindow.MenuBar.getGeometry;
      R1.Right := R.Right;
      QtMenuBar.setGeometry(R1);
      QtMainWindow.setMenuBar(QMenuBarH(QtMenuBar.Widget));
    end
    else
      QtMainWindow.setMenuBar(QMenuBarH(QtMainWindow.MenuBar.Widget));
  end;
end;

function TQtWidgetSet.SetParent(hWndChild: HWND; hWndParent: HWND): HWND;
var
  OldVisible: Boolean;
  Flags: QtWindowFlags;
  W: TQtWidget;
begin
  {$ifdef VerboseQtWinAPI}
  writeln('[WinApi SetParent] child: ',dbgHex(PtrUInt(hwndChild)),
    ' parent: ',dbgHex(PtrUInt(hWndParent)));
  {$endif}
  Result := 0;
  if not IsValidHandle(hwndChild) then
    exit;
  Result := GetParent(hWndChild);
  if (Result = hwndParent) then
    exit;
  W := TQtWidget(hWndChild);
  OldVisible := W.getVisible;
  Flags := W.windowFlags;
  if IsValidHandle(hWndParent) then
    W.setParent(TQtWidget(hWndParent).GetContainerWidget)
  else
  begin
    W.setParent(nil);
    W.setWindowFlags(Flags);
  end;
  W.setVisible(OldVisible);
end;

function TQtWidgetSet.SetMapMode(DC: HDC; fnMapMode : Integer): Integer;
var
  AWindowExt: TPoint;
  R: TRect;
begin
  if IsValidDC(DC) then
  begin
    if fnMapMode <> TQtDeviceContext(DC).vMapMode then
    begin
      case fnMapMode of
        MM_ANISOTROPIC:; // user's choice
        MM_ISOTROPIC:; // adjusted after each SetViewPortExtEx call (see MSDN for details)
        MM_HIENGLISH: AWindowExt := Point(1000, -1000);
        MM_HIMETRIC: AWindowExt := Point(2540, -2540);
        MM_LOENGLISH: AWindowExt := Point(100, -100);
        MM_LOMETRIC: AWindowExt := Point(254, -254);
        MM_TWIPS: AWindowExt := Point(1440, -1440);
      else
        fnMapMode := MM_TEXT;
      end;
      TQtDeviceContext(DC).vMapMode := fnMapMode;
      QPainter_setViewTransformEnabled(TQtDeviceContext(DC).Widget, fnMapMode <> MM_TEXT);
      if not (fnMapMode in [MM_TEXT, MM_ANISOTROPIC, MM_ISOTROPIC]) then
      begin
        QPainter_Window(TQtDeviceContext(DC).Widget, @R);
        R.BottomRight := AWindowExt;
        QPainter_setWindow(TQtDeviceContext(DC).Widget, @R);
        QPainter_ViewPort(TQtDeviceContext(DC).Widget, @R);
        R.Right := QtWidgetSet.GetDeviceCaps(DC, LOGPIXELSX);
        R.Bottom := QtWidgetSet.GetDeviceCaps(DC, LOGPIXELSX);
        QPainter_setViewPort(TQtDeviceContext(DC).Widget, @R);
      end;
    end;
    Result := Integer(True);
  end else
    Result := Integer(False);
end;

function TQtWidgetSet.SetViewPortExtEx(DC: HDC; XExtent, YExtent : Integer; OldSize: PSize): Boolean;
var
  R, RW: TRect;
  Ratio: Single;
begin
  Result := False;
  if IsValidDC(DC) then
  begin
    QPainter_ViewPort(TQtDeviceContext(DC).Widget, @R);
    if OldSize <> nil then
    begin
      OldSize^.cx := R.Right - R.Left;
      OldSize^.cy := R.Bottom - R.Top;
    end;
    if (XExtent <> R.Right) or (YExtent <> R.Bottom) then
    begin
      case TQtDeviceContext(DC).vMapMode of
        MM_ANISOTROPIC, MM_ISOTROPIC:
        begin
          if TQtDeviceContext(DC).vMapMode = MM_ISOTROPIC then
          begin
            // TK: Is here also an adjustment on Windows if DPIX and DPIY are different?
            QPainter_Window(TQtDeviceContext(DC).Widget, @RW);
            Ratio := RW.Right / RW.Bottom; // no check, programmer cannot put nonsense
            if YExtent * Ratio > XExtent then
              YExtent := RoundToInt(XExtent / Ratio)
            else if YExtent * Ratio < XExtent then
              XExtent := RoundToInt(YExtent * Ratio)
          end;
          QPainter_setViewPort(TQtDeviceContext(DC).Widget, R.Left, R.Top, XExtent, YExtent);
          Result := True;
        end;
      end;
    end;
  end;
end;

function TQtWidgetSet.SetViewPortOrgEx(DC: HDC; NewX, NewY: Integer; OldPoint: PPoint): Boolean;
var
  R: TRect;
begin
  Result := False;
  if IsValidDC(DC) then
  begin
    QPainter_ViewPort(TQtDeviceContext(DC).Widget, @R);
    if OldPoint <> nil then
      OldPoint^ := R.TopLeft;
    if (TQtDeviceContext(DC).vMapMode <> MM_TEXT) and (NewX <> R.Left) or (NewY <> R.Top) then
    begin
      QPainter_setViewPort(TQtDeviceContext(DC).Widget, NewX, NewY, R.Right - R.Left, R.Bottom - R.Top);
      Result := True;
    end;
  end;
end;

function TQtWidgetSet.SetWindowExtEx(DC: HDC; XExtent, YExtent: Integer; OldSize: PSize): Boolean;
var
  R: TRect;
begin
  Result := False;
  if IsValidDC(DC) then
  begin
    QPainter_Window(TQtDeviceContext(DC).Widget, @R);
    if OldSize <> nil then
    begin
      OldSize^.cx := R.Right - R.Left;
      OldSize^.cy := R.Bottom - R.Top;
    end;
    if (XExtent <> R.Right) or (YExtent <> R.Bottom) then
    begin
      case TQtDeviceContext(DC).vMapMode of
        MM_ANISOTROPIC, MM_ISOTROPIC:
        begin
          QPainter_setWindow(TQtDeviceContext(DC).Widget, R.Left, R.Top, XExtent, YExtent);
          Result := True;
        end;
      end;
    end;
  end;
end;

{------------------------------------------------------------------------------
  Method:  SetWindowOrgEx
  Params:  DC    - handle of device context
           NewX  - new x-coordinate of window origin
           NewY  - new y-coordinate of window origin
           Point - record receiving original origin
  Returns: Whether the call was successful

  Sets the window origin of the device context by using the specified coordinates.
 ------------------------------------------------------------------------------}
function TQtWidgetSet.SetWindowOrgEx(DC : HDC; NewX, NewY : Integer; OldPoint: PPoint) : Boolean;
var
  P: TPoint;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('[WinAPI SetWindowOrgEx] DC: ', dbghex(DC), ' NewX: ', dbgs(NewX), ' NewY: ', dbgs(NewY));
  {$endif}

  Result := False;

  if IsValidDC(DC) then
  begin
    GetWindowOrgEx(DC, @P);
    // restore 0, 0
    if (P.X <> 0) or (P.Y <> 0) then
      TQtDeviceContext(DC).translate(P.X, P.Y);
    if OldPoint <> nil then
      OldPoint^ := P;
    TQtDeviceContext(DC).translate(-NewX, -NewY);
    Result := True;
  end;
end;

{------------------------------------------------------------------------------
  Method:  SetWindowPos
  Params: HWnd            - handle of window
          HWndInsertAfter - placement-order handle
          X               - horizontal position
          Y               - vertical position
          CX              - width
          CY              - height
          UFlags          - window-positioning flags
  Returns: If the function succeeds

  Changes the size, position, and Z order of a child, pop-up, or top-level
  window.
 ------------------------------------------------------------------------------}
function TQtWidgetSet.SetWindowPos(hWnd: HWND; hWndInsertAfter: HWND; X, Y, cx,
  cy: Integer; uFlags: UINT): Boolean;
var
  DisableUpdates: boolean;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('[WinAPI SetWindowPos] Handle: ', dbghex(hWnd),
      ' hWndInsertAfter: ',dbghex(hWnd));
  {$endif}
  Result := hWnd <> 0;
  if not Result then
    exit;

  DisableUpdates := (SWP_NOREDRAW and uFlags) <> 0;
  if DisableUpdates then
    TQtWidget(Hwnd).setUpdatesEnabled(False);
  try
    if (SWP_NOMOVE and uFlags) = 0 then
      TQtWidget(Hwnd).move(X, Y);

    if (SWP_NOSIZE and uFlags) = 0 then
      TQtWidget(Hwnd).resize(CX, CY);

    if (SWP_NOZORDER and uFlags) = 0 then
    begin
      case hWndInsertAfter of
        HWND_TOP:
          begin
            TQtWidget(hWnd).raiseWidget;
            if (SWP_NOACTIVATE and uFlags) = 0 then
              TQtWidget(hWnd).Activate;
          end;
        HWND_BOTTOM: TQtWidget(hWnd).lowerWidget;
        {TODO: HWND_TOPMOST ,HWND_NOTOPMOST}
      end;
    end;
  finally
    if DisableUpdates then
      TQtWidget(Hwnd).setUpdatesEnabled(True);
  end;
end;

{------------------------------------------------------------------------------
  Method:  SetWindowRgn
  Params:  hWnd    - handle of the widget
           hRgn    - handle of the region
           bRedraw - ?
  Returns: 0 if the call failed, any other value if it was successful

  Makes the region specifyed in hRgn be the only part of the window which is
  visible.
 ------------------------------------------------------------------------------}
function TQtWidgetSet.SetWindowRgn(hWnd: HWND;
 hRgn: HRGN; bRedraw: Boolean):longint;
var
  w: TQtWidget;
  r: TQtRegion;
begin
  Result := 0;

  {$ifdef VerboseQtWinAPI}
    WriteLn('[WinAPI SetWindowRgn] Handle: ', dbghex(hWnd));
  {$endif}

  // Basic checks
  if (hWnd = 0) or (hRgn = 0) then Exit;

  w := TQtWidget(hWnd);
  r := TQtRegion(hRgn);

  // Now set the mask in the widget
  w.setMask(r.FHandle);

  Result := 1;
end;

function TQtWidgetSet.ShowCaret(hWnd: HWND): Boolean;
begin
  Result := (hWnd <> 0) and (QtCaret.ShowCaret(TQtWidget(hWnd)));
end;

{------------------------------------------------------------------------------
  Method:  SetProp
  Params:  Handle -
  Returns:
 ------------------------------------------------------------------------------}
function TQtWidgetSet.SetProp(Handle: hwnd; Str: PChar; Data: Pointer): Boolean;
begin
  if Handle<>0 then
  begin
    TQtWidget(Handle).Props[str] := Data;
    Result := (TQtWidget(Handle).Props[str]=Data);
    {$ifdef VerboseQtWinApi}
    DebugLn('[WinAPI SetProp win=%s str=%s data=%x',[dbgsname(TQtWidget(Handle)), str, ptrint(data)]);
    {$endif}
  end else
    Result := False;
end;

{------------------------------------------------------------------------------
  Function: SetROP2
  Params:  HDC, Raster OP mode
  Returns: Old Raster OP mode

  Please note that the bitwise raster operation modes, denoted with a
  RasterOp prefix, are only natively supported in the X11 and
  raster paint engines.
  This means that the only way to utilize these modes on the Mac is
  via a QImage.
  The RasterOp denoted blend modes are not supported for pens and brushes
  with alpha components. Also, turning on the QPainter::Antialiasing render
  hint will effectively disable the RasterOp modes.
 ------------------------------------------------------------------------------}
function TQtWidgetSet.SetROP2(DC: HDC; Mode: Integer): Integer;
var
  QtDC: TQtDeviceContext absolute DC;
begin
  {$ifdef VerboseQtWinAPI}
  writeln('TQtWidgetSet.SetROP2() DC ',dbghex(DC),' Mode ',Mode);
  {$endif}
  Result := R2_COPYPEN;
  if not IsValidDC(DC) then
    exit;
  Result := QtDC.Rop2;
  QtDC.Rop2 := Mode;
end;

{------------------------------------------------------------------------------
  Function: SetScrollInfo
  Params:  none
  Returns: The new position value

 ------------------------------------------------------------------------------}
function TQtWidgetSet.SetScrollInfo(Handle : HWND; SBStyle : Integer;
  ScrollInfo: TScrollInfo; bRedraw : Boolean): Integer;
var
  Control: TWinControl;
  ScrollBar: TQtScrollBar;

  function UpdateScrollInfo: Integer;
  var
    iReCountMax: Integer;
    SBUpdatesCount: Integer;
    i: Integer;
    WheelLines: Integer;
  begin
    Result := 0;
    SBUpdatesCount := 0;

    if (ScrollInfo.FMask and SIF_RANGE) <> 0 then
    begin
      inc(SBUpdatesCount);
      ScrollBar.setMinimum(ScrollInfo.nMin);

      // we must recount ScrollBar.Max since invalid value raises AV
      iRecountMax := ScrollInfo.nMax - ScrollInfo.nPage;
      if iRecountMax < ScrollInfo.nMin then
        iRecountMax := ScrollInfo.nMin;

      ScrollBar.setMaximum(iRecountMax);
    end;

    if (ScrollInfo.FMask and SIF_PAGE) <> 0 then
    begin
      // segfaults if we don't check Enabled property
      if ScrollBar.getEnabled then
      begin
        inc(SBUpdatesCount);
        ScrollBar.setPageStep(ScrollInfo.nPage);
        WheelLines := QApplication_wheelScrollLines();
        with Scrollbar do
        begin
          i := Max(1, floor((GetPageStep / WheelLines) / 6));
          setSingleStep(i);
        end;
      end;
    end;

    if (ScrollInfo.FMask and SIF_UPDATEPOLICY) <> 0 then
      ScrollBar.setTracking(ScrollInfo.nTrackPos <> SB_POLICY_DISCONTINUOUS);

    if (ScrollInfo.FMask and SIF_POS) <> 0 then
    begin
      inc(SBUpdatesCount);

      if SBUpdatesCount = 1 then
        ScrollBar.BeginUpdate;
      try
        if not (ScrollBar.getTracking and ScrollBar.getSliderDown) then
        begin
          {do not setValue() if values are equal, since it calls
           signalValueChanged() which sends unneeded LM_SCROLL msgs }
          if (ScrollBar.getValue = ScrollInfo.nPos) then
            SBUpdatesCount := 0;

          if (ScrollInfo.nPos < ScrollBar.getMin) then
            ScrollInfo.nPos := ScrollBar.getMin
          else
          if (ScrollInfo.nPos > ScrollBar.getMax) then
            ScrollInfo.nPos := ScrollBar.getMax;

          if (SBUpdatesCount > 0) then
            ScrollBar.setValue(ScrollInfo.nPos);
        end;
      finally
        if ScrollBar.InUpdate then
          ScrollBar.EndUpdate;
      end;
    end;

    if (ScrollInfo.FMask and SIF_TRACKPOS) <> 0 then
    begin
      ScrollBar.TrackPos := ScrollInfo.nTrackPos;
      // from MSDN: the SetScrollInfo function ignores this member
      // ScrollBar.setSliderPosition(ScrollInfo.nTrackPos);
    end;

    Result := ScrollBar.getValue;
  end;

begin
  // bRedraw is useles with qt

  Result := 0;

  if (Handle = 0) then exit;

  ScrollBar := nil;
  case SBStyle of
    SB_BOTH:
    begin
      {TODO: SB_BOTH fixme }
      //writeln('TODO: ############## SB_BOTH CALLED HERE .... #################');
    end; {SB_BOTH}

    SB_CTL:
    begin
      {HWND is always TScrollBar, but seem that Create ScrollBar should be called here }
      if (csReading in TQtWidget(Handle).LCLObject.ComponentState) or
      (csDestroying in TQtWidget(Handle).LCLObject.ComponentState) then exit;

      ScrollBar := TQtScrollBar(Handle);

      if not Assigned(ScrollBar) then exit;
    end; {SB_CTL}

    SB_HORZ:
    begin
      if (csReading in TQtWidget(Handle).LCLObject.ComponentState) or
         (csDestroying in TQtWidget(Handle).LCLObject.ComponentState) then
        exit;

      if TQtWidget(Handle) is TQtAbstractScrollArea then
      begin
        ScrollBar := TQtAbstractScrollArea(Handle).horizontalScrollBar;
      end else
      begin
        {do not localize !}
        Control := TWinControl(TQtWidget(Handle).LCLObject.FindChildControl(TQtWidget(Handle).LCLObject.Name+'_HSCROLLBAR'));
        if (Control <> nil) and (Control.HandleAllocated) then
          ScrollBar := TQtScrollBar(Control.Handle)
      end;
    end; {SB_HORZ}

    SB_VERT:
    begin
      if (csReading in TQtWidget(Handle).LCLObject.ComponentState) or
        (csDestroying in TQtWidget(Handle).LCLObject.ComponentState) then
        exit;

      if TQtWidget(Handle) is TQtAbstractScrollArea then
      begin
        ScrollBar := TQtAbstractScrollArea(Handle).verticalScrollBar;
      end else
      begin
        {do not localize !}
        Control := TWinControl(TQtWidget(Handle).LCLObject.FindChildControl(TQtWidget(Handle).LCLObject.Name+'_VSCROLLBAR'));
        if (Control <> nil) and (Control.HandleAllocated) then
          ScrollBar := TQtScrollBar(Control.Handle)
      end;
    end; {SB_VERT}

  end;

  if Assigned(ScrollBar) then
    Result := UpdateScrollInfo;
end;

{------------------------------------------------------------------------------
  Method:  SetTextColor
  Params:  Handle -
  Returns:
 ------------------------------------------------------------------------------}
function TQtWidgetSet.SetTextColor(DC: HDC; Color: TColorRef): TColorRef;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('[WinAPI SetTextColor] DC: ', dbghex(DC));
  {$endif}
  Result := CLR_INVALID;
  if not IsValidDC(DC) then begin
    {$ifdef VerboseQtWinAPI}
    WriteLn('[WinAPI SetTextColor] Invalid DC');
    {$endif}
    exit;
  end;
  Result := TQtDeviceContext(DC).vTextColor;
  TQtDeviceContext(DC).vTextColor := ColorToRGB(TColor(Color)); // be sure we get TColorRef
end;

{------------------------------------------------------------------------------
  function ShowScrollBar(Handle: HWND; wBar: Integer; bShow: Boolean): Boolean;
  Params  Handle: HWND; wBar: Integer; bShow: Boolean
  Result
------------------------------------------------------------------------------}
function TQtWidgetSet.ShowScrollBar(Handle: HWND; wBar: Integer; bShow: Boolean): Boolean;
var
  w: TQtWidget;
  ScrollArea: TQtAbstractScrollArea;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('[WinAPI ShowScrollBar] Handle: ', dbghex(Handle),' wBar: ',wBar);
  {$endif}

  Result := (Handle <> 0);

  if not Result then exit;

  w := TQtWidget(Handle);

  if w is TQtAbstractScrollArea then
  begin
    ScrollArea := TQtAbstractScrollArea(w);
    case wBar of
      SB_BOTH:
      begin
        if bShow then
          ScrollArea.setScrollStyle(ssBoth)
        else
          ScrollArea.setScrollStyle(ssNone);
      end;

      SB_HORZ:
      begin
        if bShow then
          ScrollArea.setScrollStyle(ssHorizontal)
        else
          ScrollArea.ScrollBarPolicy[False] := QtScrollBarAlwaysOff;
      end;

      SB_VERT:
      begin
        if bShow then
          ScrollArea.setScrollStyle(ssVertical)
        else
          ScrollArea.ScrollBarPolicy[True] := QtScrollBarAlwaysOff;
      end;

      SB_CTL:
      begin
        if bShow then
          ScrollArea.Show
        else
          ScrollArea.Hide;
      end;
    end;

  end else
    Result := False;
end;

{------------------------------------------------------------------------------
  function ShowWindow(hWnd: HWND; nCmdShow: Integer): Boolean;

  nCmdShow:
    SW_SHOWNORMAL, SW_MINIMIZE, SW_SHOWMAXIMIZED
------------------------------------------------------------------------------}
function TQtWidgetSet.ShowWindow(hWnd: HWND; nCmdShow: Integer): Boolean;
var
  Widget: TQtWidget;
begin
  {$ifdef VerboseQtWinAPI}
  WriteLn('[WinAPI ShowWindow] hwnd ',dbgHex(PtrUInt(hWnd)),' nCmdShow ',nCmdShow);
  {$endif}

  Result := False;

  Widget := TQtWidget(hWnd);

  if Widget <> nil then
  begin
    case nCmdShow of
      SW_SHOW: Widget.setVisible(True);
      SW_SHOWNORMAL: Widget.ShowNormal;
      SW_MINIMIZE: Widget.setWindowState(QtWindowMinimized);
      SW_SHOWMINIMIZED: Widget.ShowMinimized;
      SW_SHOWMAXIMIZED: Widget.ShowMaximized;
      SW_SHOWFULLSCREEN: Widget.ShowFullScreen;
      SW_HIDE: Widget.setVisible(False);
    end;
    Result := True;
  end;
end;

{------------------------------------------------------------------------------
  Function: StretchBlt
  Params:  DestDC:                The destination devicecontext
           X, Y:                  The left/top corner of the destination rectangle
           Width, Height:         The size of the destination rectangle
           SrcDC:                 The source devicecontext
           XSrc, YSrc:            The left/top corner of the source rectangle
           SrcWidth, SrcHeight:   The size of the source rectangle
           ROp:                   The raster operation to be performed
  Returns: True if succesful

  The StretchBlt function copies a bitmap from a source rectangle into a
  destination rectangle using the specified raster operation. If needed it
  resizes the bitmap to fit the dimensions of the destination rectangle.
  Sizing is done according to the stretching mode currently set in the
  destination device context.
  If SrcDC contains a mask the pixmap will be copied with this transparency.
 ------------------------------------------------------------------------------}
function TQtWidgetSet.StretchBlt(DestDC: HDC; X, Y, Width, Height: Integer;
  SrcDC: HDC; XSrc, YSrc, SrcWidth, SrcHeight: Integer; ROp: Cardinal): Boolean;
begin
  Result := StretchMaskBlt(DestDC,X,Y,Width,Height,
                          SrcDC,XSrc,YSrc,SrcWidth,SrcHeight,
                          0,0,0,
                          ROp);
end;

{------------------------------------------------------------------------------
  Function: StretchMaskBlt
  Params:  DestDC:                The destination devicecontext
           X, Y:                  The left/top corner of the destination rectangle
           Width, Height:         The size of the destination rectangle
           SrcDC:                 The source devicecontext
           XSrc, YSrc:            The left/top corner of the source rectangle
           SrcWidth, SrcHeight:   The size of the source rectangle
           Mask:                  The handle of a monochrome bitmap
           XMask, YMask:          The left/top corner of the mask rectangle
           ROp:                   The raster operation to be performed
  Returns: True if succesful

  The StretchMaskBlt function copies a bitmap from a source rectangle into a
  destination rectangle using the specified mask and raster operation. If needed
  it resizes the bitmap to fit the dimensions of the destination rectangle.
  Sizing is done according to the stretching mode currently set in the
  destination device context.
 ------------------------------------------------------------------------------}
function TQtWidgetSet.StretchMaskBlt(DestDC: HDC; X, Y, Width, Height: Integer;
  SrcDC: HDC; XSrc, YSrc, SrcWidth, SrcHeight: Integer; Mask: HBITMAP;
  XMask, YMask: Integer; Rop: DWORD): Boolean;
var
  SrcQDC: TQtDeviceContext absolute SrcDC;
  DstQDC: TQtDeviceContext absolute DestDC;
  SrcRect, DstRect, MaskRect: TRect;
  Image, TmpImage, QMask, TmpMask: QImageH;
  TmpPixmap: QPixmapH;
  SrcMatrix: QTransformH;
  dx, dy: integer;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('[WinAPI StretchMaskBlt]',
     ' DestDC:', dbghex(DestDC),
     ' SrcDC:', dbghex(SrcDC),
     ' Image:', dbghex(PtrInt(Image)),
     ' X:', dbgs(X), ' Y:', dbgs(Y),
     ' W:', dbgs(Width), ' H:', dbgs(Height),
     ' XSrc:', dbgs(XSrc), ' YSrc:', dbgs(YSrc),
     ' WSrc:', dbgs(SrcWidth), ' HSrc:', dbgs(SrcHeight));
  {$endif}

  Result := False;

  SrcMatrix := QPainter_transform(SrcQDC.Widget);
  if SrcQDC.vImage = nil then
  begin
    if SrcQDC.Parent <> nil then
    begin
      with SrcQDC.getDeviceSize do
        TmpPixmap := QPixmap_create(x, y);
      QPixmap_grabWindow(TmpPixmap, QWidget_winId(SrcQDC.Parent), 0, 0);
      Image := QImage_create();
      QPixmap_toImage(TmpPixmap, Image);
      QPixmap_destroy(TmpPixmap);
    end
    else
      Exit;
  end
  else
    Image := SrcQDC.vImage.FHandle;

  QTransform_map(SrcMatrix, XSrc, YSrc, @XSrc, @YSrc);
  // our map can have some transformations
  if XSrc < 0 then // we cannot draw from negative coord, so we will draw from zero with shift
  begin
    dx := -XSrc;
    XSrc := 0;
  end
  else
    dx := 0;

  if YSrc < 0 then
  begin
    dy := -YSrc;
    YSrc := 0;
  end
  else
    dy := 0;

  if dx <> 0 then // apply shifts
  begin
    inc(X, dx);        // shift destination
    dec(Width, dx);    // substract width
    dec(SrcWidth, dx); // and do not forget about SrcWidth or we will get unneeded stretching
  end;

  if dy <> 0 then
  begin
    inc(Y, dy);
    dec(Height, dy);
    dec(SrcHeight, dy);
  end;

  DstRect := Bounds(X, Y, Width, Height);
  SrcRect := Bounds(XSrc, YSrc, SrcWidth, SrcHeight);
  MaskRect := Bounds(XMask, YMask, SrcWidth, SrcHeight);
  // #0011187 - makes painting wrong
  //DstQDC.CorrectCoordinates(DstRect);
  //DstQDC.CorrectCoordinates(SrcRect);
  //DstQDC.CorrectCoordinates(MaskRect);
  if Mask <> 0 then
    QMask := TQtImage(Mask).FHandle
  else
    QMask := nil;

  if (DstRect.Right < DstRect.Left) or (DstRect.Bottom < DstRect.Top) then
  begin
    // Right < Left mean horizontal flip, Bottom < Top - vertical
    TmpImage := QImage_create();
    QImage_mirrored(Image, TmpImage, DstRect.Right < DstRect.Left, DstRect.Bottom < DstRect.Top);
    if QMask <> nil then
    begin
      TmpMask := QImage_create();
      QImage_mirrored(QMask, TmpMask, DstRect.Right < DstRect.Left, DstRect.Bottom < DstRect.Top);
    end
    else
      TmpMask := QMask;
    DstRect := NormalizeRect(DstRect);
    MaskRect := NormalizeRect(MaskRect);
    DstQDC.drawImage(@DstRect, TmpImage, @SrcRect, TmpMask, @MaskRect);
    QImage_destroy(TmpImage);
    if TmpMask <> nil then
      QImage_destroy(TmpMask);
  end
  else
    DstQDC.drawImage(@DstRect, Image, @SrcRect, QMask, @MaskRect);

  if SrcQDC.vImage = nil then
    QImage_destroy(Image);

  Result := True;
end;

{------------------------------------------------------------------------------
  Function: SystemParametersInfo
  Params: uiAction: System-wide parameter to be retrieved or set
          uiParam: Depends on the system parameter being queried or set
          pvParam: Depends on the system parameter being queried or set
          fWinIni:
  Returns: True if the function succeeds
  retrieves or sets the value of one of the system-wide parameters
 ------------------------------------------------------------------------------}
function TQtWidgetSet.SystemParametersInfo(uiAction: DWord; uiParam: DWord; pvParam: Pointer; fWinIni: DWord): LongBool;
begin
  case uiAction of
    SPI_GETWHEELSCROLLLINES: PDword(pvPAram)^ := QApplication_wheelScrollLines;
    SPI_GETWORKAREA: begin
      TRect(pvParam^):=Bounds(GetSystemMetrics(SM_XVIRTUALSCREEN),
                              GetSystemMetrics(SM_YVIRTUALSCREEN),
                              GetSystemMetrics(SM_CXVIRTUALSCREEN),
                              GetSystemMetrics(SM_CYVIRTUALSCREEN));
      Result:=True;
    end;
  else
    Result := False;
  end
end;*)

(*{------------------------------------------------------------------------------
  Method:  UpdateWindow
  Params:  Handle
  Returns:
 ------------------------------------------------------------------------------}
function TQtWidgetSet.UpdateWindow(Handle: HWND): Boolean;
begin
 {$ifdef VerboseQtWinAPI}
   WriteLn('[WinAPI UpdateWindow]');
 {$endif}
  Result := False;
  if Handle <> 0 then
  begin
    TQtWidget(Handle).Update;
    Result := True;
  end;
end;

{------------------------------------------------------------------------------
  Method:  WindowFromPoint
  Params:  TPoint
  Returns: The return value is a handle to the window that contains the param
  point.
  If no window exists at the given point, the return value is 0.
  If the point is over a static text control,
  the return value is a handle to the window under the static text control.
 ------------------------------------------------------------------------------}
function TQtWidgetSet.WindowFromPoint(APoint: TPoint): HWND;
var
  Widget: QWidgetH;
begin
  // we use cachedresults instead of calling very expensive widgetAt
  if (FLastWFPResult <> 0) then
  begin
    if not IsValidWidgetAtCachePointer then
      FLastWFPResult := 0
    else
    if (APoint.X = FLastWFPMousePos.X) and (APoint.Y = FLastWFPMousePos.Y) and
      TQtWidget(FLastWFPResult).getVisible and
      TQtWidget(FLastWFPResult).getEnabled then
    begin
      // return from cache
      exit(FLastWFPResult);
    end;
  end;

  Result := 0;
  Widget := QApplication_widgetAt(APoint.x, APoint.y);

  if (Widget = nil) then
  begin
    if (APoint.X = FLastWFPMousePos.X) and (APoint.Y = FLastWFPMousePos.Y) then
    begin
      FLastWFPMousePos := Point(MaxInt, MaxInt);
      FLastWFPResult := 0;
    end;
    exit;
  end;

  // according to MSDN disabled widget shouldn't be in result
  // but win32 returns first enabled and visible parent !
  if not QWidget_isEnabled(Widget) or not QWidget_isVisible(Widget) then
  begin
    while Widget <> nil do
    begin
      Widget := QWidget_parentWidget(Widget);
      if (Widget <> nil) and QWidget_IsVisible(Widget) and
        QWidget_isEnabled(Widget) then
          break;
    end;
    if Widget = nil then
      exit;
  end;

  Result := HwndFromWidgetH(Widget);

  // return from cache if we are same TQtWidget, just update point
  if IsValidWidgetAtCachePointer and (Result = FLastWFPResult) then
  begin
    FLastWFPMousePos := APoint;
    exit(FLastWFPResult);
  end;

  // maybe we are viewport of native QAbstractScrollArea (eg. QTextEdit).
  if (Result = 0) then
  begin
    if QWidget_parentWidget(Widget) <> nil then
    begin
      while (Widget <> nil) do
      begin
        Widget := QWidget_parentWidget(Widget);
        if Widget <> nil then
          Result := HwndFromWidgetH(Widget);
        if Result <> 0 then
          break;
      end;
    end;
  end;

  if (Result <> 0) and
    not (TQtWidget(Result) is TQtMainWindow) then
  begin
    if TQtWidget(Result).getOwner <> nil then
      Result := HWND(TQtWidget(Result).getOwner);
  end else
  begin
    Widget := QApplication_topLevelAt(APoint.x, APoint.y);
    if (Widget <> nil) and QWidget_isEnabled(Widget) then
      Result := HwndFromWidgetH(Widget)
    else
      Result := 0;
  end;

  // add to cache
  FLastWFPResult := Result;
  FLastWFPMousePos := APoint;
end;*)

//##apiwiz##eps##   // Do not remove, no wizard declaration after this line
