{%MainUnit customdrawnint.pp}
{******************************************************************************
                         All CustomDrawn API implementations.
                   Initial Revision : Sat Nov 13 12:53:53 1999


  !! Keep alphabetical !!

 ******************************************************************************
 Implementation
 ******************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}

(*//##apiwiz##sps##   // Do not remove

{------------------------------------------------------------------------------
  Method:  Arc
  Params:  DC - handle to device context
           Left - x-coordinate of bounding rectangle's upper-left corner
           Top - y-coordinate of bounding rectangle's upper-left corner
           Right - x-coordinate of bounding rectangle's lower-right corner
           Bottom - y-coordinate of bounding rectangle's lower-right corner
           StartAngle - base angle
           AngleLength - angle length
  Returns: Whether the call was successful

  Use Arc to draw an elliptically curved line with the current Pen.
  The angles angle1 and angle2 are 1/16th of a degree. For example, a full
  circle equals 5760 (16*360). Positive values of Angle and AngleLength mean
  counter-clockwise while negative values mean clockwise direction.
  Zero degrees is at the 3'o clock position.
 ------------------------------------------------------------------------------}
function TWin32WidgetSet.Arc(DC: HDC; Left, Top, Right, Bottom, Angle16Deg, Angle16DegLength: Integer): Boolean;
var
  SX, SY, EX, EY, OldArcDirection: Longint;
begin
  if Angle16DegLength < 0 then OldArcDirection := Windows.SetArcDirection(DC, AD_CLOCKWISE)
  else OldArcDirection := Windows.SetArcDirection(DC, AD_COUNTERCLOCKWISE);

  Angles2Coords(Left, Top, Right - Left, Bottom - Top, Angle16Deg, Angle16DegLength, SX, SY, EX, EY);
  Result := Boolean(Windows.Arc(DC, Left, Top, Right, Bottom, SX, SY, EX, EY));
  // Revert the arc direction to the previous value
  Windows.SetArcDirection(DC, OldArcDirection);
end;

{------------------------------------------------------------------------------
  Method:   AngleChord
  Params:   DC, x1, y1, x2, y2, angle1, angle2
  Returns:  Nothing

  Use AngleChord to draw a filled Chord-shape on the canvas. The angles angle1
  and angle2 are 1/16th of a degree. For example, a full circle equals 5760
  16*360). Positive values of Angle and AngleLength mean counter-clockwise while
  negative values mean clockwise direction. Zero degrees is at the 3'o clock
  position.

------------------------------------------------------------------------------}
function TWin32WidgetSet.AngleChord(DC: HDC; x1, y1, x2, y2, Angle1,
  Angle2: Integer): Boolean;
var
  SX, SY, EX, EY : Longint;
begin
  Angles2Coords(x1, y1, x2-x1, y2-y1, Angle1, Angle2, SX, SY, EX, EY);
  Result := Boolean(Windows.Chord(DC, x1, y1, x2, y2, SX, SY, EX, EY));
end;

{------------------------------------------------------------------------------
  Method:  BitBlt
  Params:  DestDC        - The destination device context
           X, Y          - The left/top corner of the destination rectangle
           Width, Height - The size of the destination rectangle
           SrcDC         - The source devicecontext
           XSrc, YSrc    - The left/top corner of the source rectangle
           Rop           - The raster operation to be performed
  Returns: True if succesful

  The BitBlt function copies a bitmap from a source context into a destination
  context using the specified raster operation.
 ------------------------------------------------------------------------------}
function TWin32WidgetSet.BitBlt(DestDC: HDC; X, Y, Width, Height: Integer; SrcDC: HDC; XSrc, YSrc: Integer; Rop: DWORD): Boolean;
begin
  // use stretchmaskblt for alpha images, since that one is customized for alpha
  if IsAlphaDC(DestDC) or IsAlphaDC(SrcDC)
  then Result := StretchMaskBlt(DestDC, X, Y, Width, Height, SrcDC, XSrc, YSrc, Width, Height, 0, 0, 0, Rop)
  else Result := Windows.BitBlt(DestDC, X, Y, Width, Height, SrcDC, XSrc, YSrc, Rop);
end;

{------------------------------------------------------------------------------
  Method:  BeginPaint
  Params:  Handle        - Handle to window to begin painting in
           PS            - PAINTSTRUCT variable that will receive painting information.
  Returns: A device context for the specified window if succesful otherwise nil

  The BeginPaint function prepares the specified window for painting and fills
  a PAINTSTRUCT structure with information about the painting.
 ------------------------------------------------------------------------------}
function TWin32WidgetSet.BeginPaint(Handle: HWND; var PS: TPaintStruct): HDC;
begin
  Result := Windows.BeginPaint(Handle, @PS);
end;

{------------------------------------------------------------------------------
  Method:  CallDefaultWndHandler
  Params:  Sender - object, that sent the message
           Message - a TLMessage
  Returns: -

  Called by TWinControl.DefaultHandler to let the interface call some default
  functions for the message.
 ------------------------------------------------------------------------------}
procedure TWin32WidgetSet.CallDefaultWndHandler(Sender: TObject; var Message);
var
  Handle: HWND;

  procedure CallWin32PaintHandler;
  var
    ClientBoundRect: TRect;
    Moved: Boolean;
    PaintMsg: TLMPaint absolute Message;
  begin
    // the LCL creates paint messages, with a DC origin set to the client
    // origin of the emitting control. The paint handler of win32 expects the
    // DC origin at the origin of the control.
    // -> move the windoworigin
    if PaintMsg.DC <> 0 then
    begin
      ClientBoundRect := Rect(0, 0, 0, 0);
      if Sender is TWinControl then
        if not GetClientBounds(Handle, ClientBoundRect) then Exit;
      Moved := MoveWindowOrgEx(PaintMsg.DC, -ClientBoundRect.Left, -ClientBoundRect.Top);
    end
    else
      Moved := False;
    try
      // call win32 paint handler
      CallDefaultWindowProc(Handle, WM_PAINT, WPARAM(PaintMsg.DC), 0);
    finally
      // restore DC origin
      if Moved then
        MoveWindowOrgEx(PaintMsg.DC, ClientBoundRect.Left, ClientBoundRect.Top);
    end;
  end;

  procedure CallMouseWheelHandler;
  var
    ScrollInfo: Windows.tagScrollInfo;
    WParam: Windows.WParam;
    ScrollMsg, ScrollBar: dword;
    ScrollOffset: integer;
    Pos: TPoint;
  begin
    if not TWinControl(Sender).HandleAllocated then
      exit;

    // why coords are client? - they must be screen
    with TLMMouseEvent(Message) do
    begin
      Pos.X := X;
      Pos.Y := Y;
    end;
    ClientToScreen(Handle, Pos);
    WParam := Windows.WParam(Longint(PointToSmallPoint(Pos)));
    with TLMMouseEvent(Message) do
    begin
      X := Pos.X;
      Y := Pos.Y;
    end;

    with TLMessage(Message) do
    begin
      Result := CallDefaultWindowProc(Handle, Msg, WParam, LParam);
      // Windows handled it, so exit here.
      if Result<>0 then exit;
    end;

    // send scroll message
    FillChar(ScrollInfo, sizeof(ScrollInfo), #0);
    ScrollInfo.cbSize := sizeof(ScrollInfo);
    ScrollInfo.fMask := SIF_PAGE or SIF_POS or SIF_RANGE;
    // if mouse is over horizontal scrollbar, scroll horizontally

    if Windows.SendMessage(Handle, WM_NCHITTEST, 0, WParam) = HTHSCROLL then
    begin
      ScrollBar := SB_HORZ;
      ScrollMsg := WM_HSCROLL;
    end else
    begin
      ScrollBar := SB_VERT;
      ScrollMsg := WM_VSCROLL;
    end;
    if Windows.GetScrollInfo(Handle, ScrollBar, ScrollInfo) then
    begin
      with TLMMouseEvent(Message) do
      begin
        if Mouse.WheelScrollLines < 0 then
          // -1 means, scroll one page
          ScrollOffset := (WheelDelta * integer(ScrollInfo.nPage)) div 120
        else
          ScrollOffset := (WheelDelta * Mouse.WheelScrollLines) div 120;
        WParam := Windows.WParam(ScrollInfo.nPos - ScrollOffset);
        if WParam > ScrollInfo.nMax - integer(ScrollInfo.nPage) + 1 then
          WParam := ScrollInfo.nMax - integer(ScrollInfo.nPage) + 1;
        if WParam < ScrollInfo.nMin then
          WParam := ScrollInfo.nMin;
        WParam := SB_THUMBPOSITION or (WParam shl 16);
      end;
      Windows.PostMessage(Handle, ScrollMsg, WParam, HWND(nil));
    end;
  end;

begin
  Handle := ObjectToHwnd(Sender);
  case TLMessage(Message).Msg of
    LM_PAINT:
      CallWin32PaintHandler;
    LM_MOUSEWHEEL: // provide default wheel scrolling functionality
      CallMouseWheelHandler;
    LM_ERASEBKGND,
    LM_GETDLGCODE,
    LM_HELP:
      with TLMessage(Message) do
        Result := CallDefaultWindowProc(Handle, Msg, WParam, LParam);
  else
    if TLMessage(Message).Msg >= WM_USER then
      with TLMessage(Message) do
        Result := CallDefaultWindowProc(Handle, Msg, WParam, LParam);
  end;
end;

{------------------------------------------------------------------------------
  Method:  CallNextHookEx
  Params:  HHk    - handle of the current hook
           NCode  - Hook code
           WParam - Word parameter
           LParam - Long-integer parameter
  Returns: The handle of the next hook procedure

  Calls the next procedure in the hook chain
 ------------------------------------------------------------------------------}
function TWin32WidgetSet.CallNextHookEx(HHk: HHOOK; NCode: Integer; WParam: WParam; LParam: LParam): Integer;
begin
  Result := Windows.CallNextHookEx(hhk, ncode, Windows.WPARAM(wParam), Windows.LPARAM(lParam));
end;

{------------------------------------------------------------------------------
  Method:  CallWindowProc
  Params: LPPrevWndFunc - Address of specified window procedure
          Handle        - Handle of window receiving messages
          Msg           - The message sent
          WParam        - Word parameter
          LParam        - Long-integer parameter
  Returns: Message result

  Passes message information to the specified window procedure
 ------------------------------------------------------------------------------}
function TWin32WidgetSet.CallWindowProc(LPPrevWndFunc: TFarProc; Handle: HWND;
    Msg: UINT; WParam: WParam; LParam: LParam): Integer;
begin
  Result := Windows.CallWindowProc(WNDPROC(LPPrevWndFunc), Handle, Msg, Windows.WPARAM(WParam), Windows.LPARAM(LParam));
end;*)

{------------------------------------------------------------------------------
  Method:  ClientToScreen
  Params:  Handle - Handle of window
           P      - container that contains coordinates
  Returns: Whether the call was successful

  Converts client coordinates to screen coordinates
 ------------------------------------------------------------------------------}
function TCDWidgetSet.ClientToScreen(Handle: HWND; var P: TPoint): Boolean;
begin
  Result := False;
{  Result := Boolean(Windows.ClientToScreen(Handle, @P));
  if not Result then exit;
  Result := GetLCLClientBoundsOffset(Handle, ORect);
  if not Result then exit;
  inc(P.X, ORect.Left);
  inc(P.Y, ORect.Top);}
end;

{------------------------------------------------------------------------------
  Method:  ClipboardGetData
  Params:  ClipboardType - clipboard type
           FormatID      - a registered format identifier (0 is invalid)
           Stream        - If format is available, it will be appended to this
                           stream
  Returns: true on success
 ------------------------------------------------------------------------------}
function TCDWidgetSet.ClipboardGetData(ClipboardType: TClipboardType;
  FormatID: TClipboardFormat; Stream: TStream): Boolean;
var
  DataHandle: HGLOBAL;
  Data: pointer;
  Size: integer;
  {$IFDEF VerboseWin32Clipbrd}
  DbgFormatID: integer;
  {$ENDIF}
  Bitmap: TBitmap;
  BufferStream: TMemoryStream;
  BufferWideString: widestring;
  BufferString: ansistring;

  function ReadClipboardToStream(DestStream: TStream): Boolean;
  begin
    Result := false;

    DataHandle := Windows.GetClipboardData(FormatID);
    if DataHandle<>HWND(0) then
    begin
      Size := Windows.GlobalSize(DataHandle);
      if Size>0 then
      begin
        Data := Windows.GlobalLock(DataHandle);
        try
          DestStream.Write(Data^, Size);
        finally
          Windows.GlobalUnlock(DataHandle);
        end;
        Result := true;
      end;
    end;
  end;

begin
  Result := false;
  {$IFDEF VerboseWin32Clipbrd}
  debugln('TWin32WidgetSet.ClipboardGetData FormatID=',dbgs(FormatID));
  Windows.OpenClipboard(Windows.HWND(nil));
  DbgFormatID := 0;
  repeat
    DbgFormatID := EnumClipboardFormats(DbgFormatID);
    debugln('Available FormatID=',dbgs(DbgFormatID), ' ', ClipboardFormatToMimeType(DbgFormatID));
  until (DbgFormatID=0);
  Windows.CloseClipboard;
  {$ENDIF}
  if FormatID=PredefinedClipboardFormat(pcfDelphiBitmap)
    then FormatID := CF_BITMAP;
  if (FormatID=0) or (Stream=nil) or
    not Windows.IsClipboardFormatAvailable(FormatID) then exit;

  if Windows.OpenClipboard(Windows.HWND(nil)) then
    try
      case FormatID of
      Windows.CF_BITMAP:
        begin
          Bitmap:= TBitmap.Create;
          Bitmap.TransparentColor := clNone;
          DataHandle := Windows.GetClipboardData(FormatID);
          Bitmap.SetHandles(DataHandle, 0);
          Bitmap.SaveToStream(Stream);
          Bitmap.Free;
          Result := true;
        end;
      { In the case of unicode text, it's necessary to
        convert it from UTF-16 to UTF-8 }
      Windows.CF_UNICODETEXT, Windows.CF_TEXT:
        begin
          BufferStream := TMemoryStream.Create;
          try
            Result := ReadClipboardToStream(BufferStream);

            if Size>0 then
            begin
              BufferStream.Position := 0;
              if FormatID=Windows.CF_UNICODETEXT then
              begin;
                SetLength(BufferWideString, Size div 2);
                BufferStream.Read(BufferWideString[1], Size);
                //BufferString may have pending #0 's (or garbage after a #0)
                Size := Pos(#0, BufferWideString);
                if Size > 0 then
                  SetLength(BufferWideString, Size);
                BufferString := UTF16ToUTF8(BufferWideString);
              end
              else
              begin
                SetLength(BufferString, Size - 1);
                BufferStream.Read(BufferString[1], Size);
                //BufferString may have pending #0 's (or garbage after a #0)
                Size := Pos(#0, BufferString);
                if Size > 0 then
                  SetLength(BufferString, Size - 1);
                BufferString := AnsiToUtf8(BufferString);
              end;
              Stream.Write(BufferString[1], Length(BufferString));
            end;
          finally
            BufferStream.Free;
          end;
        end
      else
        Result := ReadClipboardToStream(Stream)
      end;
    finally
      Windows.CloseClipboard;
    end;
end;

{------------------------------------------------------------------------------
  Method:  ClipboardGetOwnerShip
  Params:  ClipboardType - Type of clipboard, the win32 interface only handles
                           ctClipBoard
           OnRequestProc - TClipboardRequestEvent is defined in LCLType.pp
                           If OnRequestProc is nil the onwership will end.
           FormatCount - number of formats
           Formats - array of TClipboardFormat. The supported formats the owner
                      provides.

  Returns: true on success

  Sets the supported formats and requests ownership for the clipboard.
  The OnRequestProc is used to get the data from the LCL and to put it on the
  clipboard.
  If someone else requests the ownership, the OnRequestProc will be executed
  with the invalid FormatID 0 to notify the old owner of the lost of ownership.
 ------------------------------------------------------------------------------}
function TCDWidgetSet.ClipboardGetOwnerShip(ClipboardType: TClipboardType;
  OnRequestProc: TClipboardRequestEvent; FormatCount: Integer;
  Formats: PClipboardFormat): Boolean;
begin
  Result := false;

end;

(*{------------------------------------------------------------------------------
  Function: CombineRgn
  Params:  Dest, Src1, Src2, fnCombineMode
  Returns: longint

  Combine the 2 Source Regions into the Destination Region using the specified
  Combine Mode. The Destination must already be initialized. The Return value
  is the Destination's Region type, or ERROR.

  The Combine Mode can be one of the following:
      RGN_AND  : Gets a region of all points which are in both source regions

      RGN_COPY : Gets an exact copy of the first source region

      RGN_DIFF : Gets a region of all points which are in the first source
                 region but not in the second.(Source1 - Source2)

      RGN_OR   : Gets a region of all points which are in either the first
                 source region or in the second.(Source1 + Source2)

      RGN_XOR  : Gets all points which are in either the first Source Region
                 or in the second, but not in both.

  The result can be one of the following constants
      Error
      NullRegion
      SimpleRegion
      ComplexRegion

 ------------------------------------------------------------------------------}
function TWin32WidgetSet.CombineRgn(Dest, Src1, Src2 : HRGN; fnCombineMode : Longint) : Longint;
begin
  Result := Windows.CombineRgn(Dest, Src1, Src2, fnCombineMode);
end;

{------------------------------------------------------------------------------
  Method:  CreateBitmap
  Params:  Width      - bitmap width, in pixels
           Height     - bitmap height, in pixels
           Planes     - number of color planes
           BitCount   - number of bits required to identify a color
           BitmapBits - pointer to array containing color data
  Returns: A handle to a bitmap

  The CreateBitmap function creates a bitmap with the specified width, height,
  and color format (color planes and bits per pixel).
 ------------------------------------------------------------------------------}
function TWin32WidgetSet.CreateBitmap(Width, Height: Integer; Planes, BitCount: LongInt; BitmapBits: Pointer): HBITMAP;
begin
  Result := Windows.CreateBitmap(Width, Height, Planes, BitCount, BitmapBits);
end;

{------------------------------------------------------------------------------
  Method:   CreateBrushIndirect
  Params:  LogBrush - record describing brush
  Returns: identifier of a logical brush

  The CreateBrushIndirect function creates a logical brush that has the
  specified style, color, and pattern.
 ------------------------------------------------------------------------------}
function TWin32WidgetSet.CreateBrushIndirect(const LogBrush: TLogBrush): HBRUSH;
var
  LB: Windows.LogBrush;
begin
  LB.lbStyle := LogBrush.lbStyle;
  LB.lbColor := ColorToRGB(TColor(LogBrush.lbColor));
  LB.lbHatch := LogBrush.lbHatch;
  Result := Windows.CreateBrushIndirect(LB);
end;

{------------------------------------------------------------------------------
  Method:  CreateCaret
  Params:  Handle - handle to owner window
           Bitmap - handle to bitmap for caret shape
           Width  - caret width
           Height - caret height
  Returns: Whether the function succeeded

  Creates a new shape for the system caret and assigns ownership of the caret
  to the specified window
 ------------------------------------------------------------------------------}
function TWin32WidgetSet.CreateCaret(Handle: HWND; Bitmap: HBITMAP; Width, Height: Integer): Boolean;
begin
{$ifdef DEBUG_CARET}
  DebugLn('[CreateCaret] for window ', IntToHex(Handle, 8));
{$endif}
  Result := Boolean(Windows.CreateCaret(Handle, Bitmap, Width, Height));
end;

{------------------------------------------------------------------------------
  Method:  CreateCompatibleBitmap
  Params: DC     - handle to device context
          Width  - width of bitmap, in pixels
          Height - height of bitmap, in pixels
  Returns: a handle to the bitmap

  Creates a bitmap compatible with the specified device context.
 ------------------------------------------------------------------------------}
function TWin32WidgetSet.CreateCompatibleBitmap(DC: HDC; Width, Height: Integer): HBITMAP;
begin
  Result := Windows.CreateCompatibleBitmap(DC, Width, Height);
end;

{------------------------------------------------------------------------------
  Method:  CreateCompatibleDC
  Params:  DC - handle to memory device context
  Returns: handle to a memory device context

  Creates a memory device context (DC) compatible with the specified device.
 ------------------------------------------------------------------------------}
function TWin32WidgetSet.CreateCompatibleDC(DC: HDC): HDC;
begin
  Result := Windows.CreateCompatibleDC(DC);
end;

{------------------------------------------------------------------------------
  Method:  CreateFontIndirect
  Params:  LogFont - logical font record
  Returns: a handle to a logical font

  Creates a logical font that has the characteristics specified in the
  specified record.
 ------------------------------------------------------------------------------}
function TWin32WidgetSet.CreateFontIndirect(const LogFont: TLogFont): HFONT;
var
{$ifdef WindowsUnicodeSupport}
  TempLogFontW: TLogFontW;
  TempLogFont: TLogFontA absolute TempLogFontW;
{$else}
  TempLogFont: TLogFont;
{$endif}
  FontName: String;
begin
  FontName := LogFont.lfFaceName;

  TempLogFont := LogFont;
  if FontName = DefFontData.Name then
  begin
    {$ifdef WindowsUnicodeSupport}
    if UnicodeEnabledOS then
      TempLogFontW.lfFaceName := UTF8ToUTF16(FMetrics.lfMessageFont.lfFaceName) // FMetrics must be UTF16
    else
      Move(FMetrics.lfMessageFont.lfFaceName, TempLogFont.lfFaceName, LF_FACESIZE);
    {$else}
    Move(FMetrics.lfMessageFont.lfFaceName, TempLogFont.lfFaceName, LF_FACESIZE);
    {$endif}
    if TempLogFont.lfHeight = 0 then
      TempLogFont.lfHeight := FMetrics.lfMessageFont.lfHeight;
  end
  else
  begin
  {$ifdef WindowsUnicodeSupport}
  if UnicodeEnabledOS then
    TempLogFontW.lfFaceName := UTF8ToUTF16(FontName)
  else
    TempLogFont.lfFaceName := Utf8ToAnsi(FontName);
  {$endif}
  end;
  {$ifdef WindowsUnicodeSupport}
  if UnicodeEnabledOS then
    Result := Windows.CreateFontIndirectW(@TempLogFontW)
  else
    Result := Windows.CreateFontIndirectA(@TempLogFont)
  {$else}
  Result := Windows.CreateFontIndirect(@TempLogFont);
  {$endif}
end;*)

(*{------------------------------------------------------------------------------
  Method:  CreateIconIndirect
  Params:  IconInfo - pointer to Icon/Cursor Information record
  Returns: handle to a created icon/cursor

  Creates an icon or cursor by color and mask bitmaps and other info.
 ------------------------------------------------------------------------------}
function TWin32WidgetSet.CreateIconIndirect(IconInfo: PIconInfo): HICON;
var
  bmp: Windows.TBitmap;
  hbm1, hbm2: HBITMAP;
  SrcDataSize, DataSize: PtrUInt;
  SrcData, Data: PByte;
  Res: Boolean;
begin
  // if we pass the XOR mask as color then we need to move it at the end of AND mask
  // correct passed values
  if (IconInfo^.hbmColor <> 0) and
     (GetObject(IconInfo^.hbmColor, SizeOf(bmp), @bmp) = SizeOf(bmp)) and
     (bmp.bmBitsPixel = 1) then
  begin
    // we must create one mask bitmap where top part of it is IMAGE and bottom is MASK
    DataSize := bmp.bmWidthBytes * abs(bmp.bmHeight) shl 1;
    Data := GetMem(DataSize);

    Res := GetBitmapBytes(bmp, IconInfo^.hbmMask, Rect(0, 0, bmp.bmWidth, bmp.bmHeight), rileWordBoundary, riloTopToBottom, SrcData, SrcDataSize);
    if Res then
    begin
      Move(SrcData^, Data^, SrcDataSize);
      FreeMem(SrcData);
    end;

    Res := Res and GetBitmapBytes(bmp, IconInfo^.hbmColor, Rect(0, 0, bmp.bmWidth, bmp.bmHeight), rileWordBoundary, riloTopToBottom, SrcData, SrcDataSize);
    if Res then
    begin
      Move(SrcData^, Data[DataSize shr 1], SrcDataSize);
      FreeMem(SrcData);
    end;

    if Res then
    begin
      hbm1 := CreateBitmap(bmp.bmWidth, bmp.bmHeight shl 1, bmp.bmPlanes, 1, Data);
      IconInfo^.hbmColor := 0;
      IconInfo^.hbmMask := hbm1;
    end;

    FreeMem(Data);
  end
  else
    hbm1 := 0;

  if (IconInfo^.hbmMask = 0) and (IconInfo^.hbmColor <> 0) and (GetObject(IconInfo^.hbmColor, SizeOf(bmp), @bmp) = SizeOf(bmp)) then
  begin
    hbm2 := CreateBitmap(bmp.bmWidth, bmp.bmHeight, bmp.bmPlanes, 1, nil);
    IconInfo^.hbmMask := hbm2;
  end
  else
    hbm2 := 0;

  Result := Windows.CreateIconIndirect(IconInfo);

  if hbm1 <> 0 then
    DeleteObject(hbm1);

  if hbm2 <> 0 then
    DeleteObject(hbm2);
end;

function TWin32WidgetSet.CreatePatternBrush(ABitmap: HBITMAP): HBRUSH;
begin
  Result := Windows.CreatePatternBrush(ABitmap);
end;

{------------------------------------------------------------------------------
  Method:  CreatePenIndirect
  Params:  LogPen - record that defines the style, width, and color of a pen
  Returns: a handle that identifies a logical cosmetic pen

  Creates a logical cosmetic pen that has the style, width, and color specified
  in a record.
 ------------------------------------------------------------------------------}
function TWin32WidgetSet.CreatePenIndirect(Const LogPen: TLogPen): HPEN;
var
  LP: TLogPen;
begin
  LP := LogPen;
  Lp.lopnColor := ColorToRGB(TColor(Lp.lopnColor));
  Result := Windows.CreatePenIndirect(Windows.LOGPEN(LP));
end;

{------------------------------------------------------------------------------
  Method:  CreatePolygonRgn
  Params:  Points, NumPts, FillMode
  Returns: the handle to the region

  Creates a Polygon, a closed many-sided shaped region. The Points parameter is
  an array of points that give the vertices of the polygon. FillMode=Winding
  determines what points are going to be included in the region. When Winding
  is True, points are selected by using the Winding fill algorithm. When Winding
  is False, points are selected by using using the even-odd (alternative) fill
  algorithm. NumPts indicates the number of points to use.
  The first point is always connected to the last point.
 ------------------------------------------------------------------------------}
function TWin32WidgetSet.CreatePolygonRgn(Points: PPoint; NumPts: Integer;
  FillMode: integer): HRGN;
begin
  Result := Windows.CreatePolygonRgn(LPPOINT(Points)^, NumPts, FillMode);
end;

{------------------------------------------------------------------------------
  Method:  CreateRectRgn
  Params:  X1 - x-coordinate of region's upper-left corner
           Y1 - y-coordinate of region's upper-left corner
           X2 - x-coordinate of region's lower-right corner
           Y2 - y-coordinate of region's lower-right corner
  Returns: the handle to the region

  Creates a rectangular region.
 ------------------------------------------------------------------------------}
function TWin32WidgetSet.CreateRectRgn(X1, Y1, X2, Y2: Integer): HRGN;
begin
  Result := Windows.CreateRectRgn(X1, Y1, X2, Y2);
end;

function TWin32WidgetSet.CreateRoundRectRgn(X1, Y1, X2, Y2, nWidthEllipse,
  nHeightEllipse: Integer): HRGN;
begin
  Result := Windows.CreateRoundRectRgn(X1, Y1, X2, Y2, nWidthEllipse,
    nHeightEllipse);
end;

{------------------------------------------------------------------------------
  Method:  DeleteDC
  Params:  HDC - handle to device context
  Returns: If the function succeeds.

  Deletes the specified device context (DC).
 ------------------------------------------------------------------------------}
function TWin32WidgetSet.DeleteDC(HDC: HDC): Boolean;
begin
  Result := Boolean(Windows.DeleteDC(HDC));
end;

{------------------------------------------------------------------------------
  Method:  DeleteObject
  Params:  GDIObject - handle to graphic object
  Returns: If the function succeeds.

  Deletes a graphic object, freeing all system resources associated with the
  object.
 ------------------------------------------------------------------------------}
function TWin32WidgetSet.DeleteObject(GDIObject: HGDIOBJ): Boolean;
begin
  { Find out if we want to release internal GDI object }
  Result := Boolean(Windows.DeleteObject(GDIObject));
end;

{------------------------------------------------------------------------------
  Method:  DestroyCaret
  Params:  Handle - handle to the window with a caret (on Windows, there is
           only one, global caret, so this parameter is ignored)
  Returns: If the function succeeds

  Destroys the caret but doesn't free the bitmap.
 ------------------------------------------------------------------------------}
function TWin32WidgetSet.DestroyCaret(Handle: HWND): Boolean;
begin
{$ifdef DEBUG_CARET}
  DebugLn('[DestroyCaret] for window ', IntToHex(Handle, 8));
{$endif}
  Result := Boolean(Windows.DestroyCaret);
end;

{------------------------------------------------------------------------------
  Method:  DestroyCursor
  Params:  Handle - handle to the cursor object
  Returns: If the function succeeds

  Destroys the cursor
 ------------------------------------------------------------------------------}
function TWin32WidgetSet.DestroyCursor(Handle: hCursor): Boolean;
begin
  Result := Boolean(Windows.DestroyCursor(Handle));
end;

function TWin32WidgetSet.DestroyIcon(Handle: HICON): Boolean;
begin
  Result := Windows.DestroyIcon(Handle);
end;

function TWin32WidgetSet.DPtoLP(DC: HDC; var Points; Count: Integer): BOOL;
begin
  Result := Windows.DPtoLP(DC, Points, Count);
end;

{------------------------------------------------------------------------------
  Method:  DrawFrameControl
  Params: DC     - handle to device context
          Rect   - bounding rectangle
          UType  - frame-control type
          UState - frame-control state
  Returns: If the function succeeds

  Draws a frame control of the specified type and style.
 ------------------------------------------------------------------------------}
function TWin32WidgetSet.DrawFrameControl(DC: HDC; const Rect: TRect; UType, UState: Cardinal): Boolean;
begin
  Result := Boolean(Windows.DrawFrameControl(DC, @Rect, UType, UState));
end;

function TWin32WidgetSet.DrawFocusRect(DC: HDC; const Rect: TRect): boolean;
begin
  Result:= Windows.DrawFocusRect(DC, PRect(@Rect)^);
end;

{------------------------------------------------------------------------------
  Method:  DrawEdge
  Params: DC       - handle to device context
          Rect     - rectangle coordinates
          Edge     - type of inner and outer edge to draw
          GrfFlags - type of border
  Returns: If the function succeeds.

  Draws one or more edges of a rectangle, not including the
  right and bottom edge.
 ------------------------------------------------------------------------------}
function TWin32WidgetSet.DrawEdge(DC: HDC; Var Rect: TRect; Edge: Cardinal; GrfFlags: Cardinal): Boolean;
begin
  Result := Boolean(Windows.DrawEdge(DC, @Rect, edge, grfFlags));
end;

{------------------------------------------------------------------------------
  Method:  DrawText
  Params:  DC, Str, Count, Rect, Flags
  Returns: If the string was drawn, or CalcRect run

 ------------------------------------------------------------------------------}
function TWin32WidgetSet.DrawText(DC: HDC; Str: PChar; Count: Integer; var Rect: TRect; Flags: Cardinal): Integer;
{$ifdef WindowsUnicodeSupport}
var
  s: AnsiString;
  w: WideString;
{$endif}
begin
 {$ifdef WindowsUnicodeSupport}
  // use temp buffer, if count is set, there might be no null terminator
  if count = -1 then
    s := str
  else
  begin
    SetLength(s, count);
    move(str^, PChar(s)^, count);
  end;
  // the length of utf8 vs Wide/Ansi the strings differ, so recalc.
  if UnicodeEnabledOS then
  begin
    W := UTF8ToUTF16(s);
    Result := Windows.DrawTextW(DC, PWideChar(W), Length(W), @Rect, Flags);
  end
  else
  begin
    S := Utf8ToAnsi(S);
    Result := Windows.DrawText(DC, PChar(S), Length(S), @Rect, Flags);
  end;
 {$else}
  Result := Windows.DrawText(DC, Str, Count, @Rect, Flags);
 {$endif} 
end;

{------------------------------------------------------------------------------
  Method:   Ellipse
  Params:
            DC - handle to device context
            X1 - x-coord. of bounding rectangle's upper-left corner
            Y1 - y-coord. of bounding rectangle's upper-left corner
            X2 - x-coord. of bounding rectangle's lower-right corner
            Y2 - y-coord. of bounding rectangle's lower-right corner
  Returns:  If the function succeeds

  Use Ellipse to draw a filled circle or ellipse.
 ------------------------------------------------------------------------------}
function TWin32WidgetSet.Ellipse(DC: HDC; X1, Y1, X2, Y2: Integer): Boolean;
begin
  Result := Boolean(Windows.Ellipse(DC, X1, Y1, X2, Y2));
end;

{------------------------------------------------------------------------------
  Method:  EmptyClipBoard
  Params: none
  Returns: If the function succeeds

  Empties the clipboard, frees handles to data in the clipboard, and ssigns
  ownership of the clipboard to the window that currently has the clipboard
  open.
 ------------------------------------------------------------------------------}
function TWin32WidgetSet.EmptyClipBoard: Boolean;
begin
  Result := Boolean(Windows.EmptyClipboard);
end;

{------------------------------------------------------------------------------
  Method:  EnableScrollBar
  Params:  Wnd      - handle to window or scroll bar
           WSBFlags - scroll bar type flag
           WArrows  - scroll bar arrow flag
  Returns: Nothing


 ------------------------------------------------------------------------------}
function TWin32WidgetSet.EnableScrollBar(Wnd: HWND; WSBFlags, WArrows: Cardinal): Boolean;
begin
  Result := Boolean(Windows.EnableScrollBar(Wnd, WSBFlags, WArrows));
end;

{------------------------------------------------------------------------------
  Method:  EnableWindow
  Params: HWnd    - handle to window
          BEnable -  whether to enable the window
  Returns: If the window was previously disabled

  Enables or disables mouse and keyboard input to the specified window or
  control.
 ------------------------------------------------------------------------------}
function TWin32WidgetSet.EnableWindow(hWnd: HWND; bEnable: Boolean): Boolean;
begin
  Result := Boolean(Windows.EnableWindow(HWnd, BEnable));
end;

{------------------------------------------------------------------------------
  Method:  EndPaint
  Params:  Handle        - Handle to window
           PS            - PAINTSTRUCT variable with painting information
  Returns: always nonzero.

  The EndPaint function marks the end of painting in the specified window.
  This function is required for each call to the BeginPaint function, but only
  after painting is complete.
 ------------------------------------------------------------------------------}
function TWin32WidgetSet.EndPaint(Handle : hwnd; var PS : TPaintStruct): Integer;
begin
  Result := Integer(Windows.EndPaint(Handle, @PS));
end;

function TWin32WidgetSet.EnumDisplayMonitors(hdc: HDC; lprcClip: PRect; lpfnEnum: MonitorEnumProc; dwData: LPARAM): LongBool;
begin
  Result := MultiMon.EnumDisplayMonitors(hdc, lprcClip, lpfnEnum, dwData);
end;*)

type
  TProcRedirRec = record
    LParam: LParam;
    CallBack: FontEnumExProc;
  end;
  PProcRedirRec = ^TProcRedirRec;

function EnumExProcRedirW(var ELogFont: TEnumLogFontExW; var Metric: TNewTextMetricEx;
    FontType: Longint; Data: LParam): Longint; stdcall;
var
  Rec: PProcRedirRec absolute Data;
  ALogFont: TEnumLogFontExA;
begin
  Move(ELogFont.elfLogFont, ALogFont.elfLogFont, SizeOf(ALogFont.elfLogFont) - SizeOf(ALogFont.elfLogFont.lfFaceName));
  ALogFont.elfLogFont.lfFaceName := UTF16ToUTF8(ELogFont.elfLogFont.lfFaceName);
  ALogFont.elfFullName := UTF16ToUTF8(ELogFont.elfFullName);
  ALogFont.elfStyle := UTF16ToUTF8(ELogFont.elfStyle);
  ALogFont.elfScript := UTF16ToUTF8(ELogFont.elfScript);

  Result := Rec^.CallBack(ALogFont, Metric, FontType, Rec^.LParam);
end;

function EnumExProcRedirA(var ELogFont: TEnumLogFontExA; var Metric: TNewTextMetricEx;
    FontType: Longint; Data: LParam): Longint; stdcall;
var
  Rec: PProcRedirRec absolute Data;
  ALogFont: TEnumLogFontExA;
begin
  ALogFont := ELogFont;
  ALogFont.elfLogFont.lfFaceName := AnsiToUtf8(ELogFont.elfLogFont.lfFaceName);
  ALogFont.elfFullName := AnsiToUtf8(ELogFont.elfFullName);
  ALogFont.elfStyle := AnsiToUtf8(ELogFont.elfStyle);
  ALogFont.elfScript := AnsiToUtf8(ELogFont.elfScript);

  Result := Rec^.CallBack(ALogFont, Metric, FontType, Rec^.LParam);
end;


(*{------------------------------------------------------------------------------
  Function: ExcludeClipRect
  Params:  dc, Left, Top, Right, Bottom
  Returns: integer

  Subtracts all intersecting points of the passed bounding rectangle
  (Left, Top, Right, Bottom) from the Current clipping region in the
  device context (dc).

  The result can be one of the following constants
      Error
      NullRegion
      SimpleRegion
      ComplexRegion


 ------------------------------------------------------------------------------}
function TWin32WidgetSet.ExcludeClipRect(dc: hdc;
  Left, Top, Right, Bottom : Integer) : Integer;
begin
  Result := Windows.ExcludeClipRect(dc, Left, Top, Right, Bottom);
end;

function TWin32WidgetSet.ExtCreatePen(dwPenStyle, dwWidth: DWord;
  const lplb: TLogBrush; dwStyleCount: DWord; lpStyle: PDWord): HPEN;
var
  LB: Windows.LogBrush;
begin
  LB.lbStyle := lplb.lbStyle;
  LB.lbColor := ColorToRGB(TColor(lplb.lbColor));
  LB.lbHatch := lplb.lbHatch;
  Result := Windows.ExtCreatePen(dwPenStyle, dwWidth, LB, dwStyleCount, lpStyle);
end;*)

{$ifdef CD_UseNativeText}
{------------------------------------------------------------------------------
  Method:  ExtTextOut
  Params:  DC      - handle to device context
           X       - x-coordinate of reference point
           Y       - x-coordinate of reference point
           Options - text-output options
           Rect    - optional clipping and/or opaquing rectangle
           Str     - character string to be drawn
           Count   - number of characters in string
           Dx      - pointer to array of intercharacter spacing values
  Returns: If the string was drawn.

  Draws a character string by using the currently selected font.
 ------------------------------------------------------------------------------}
function TCDWidgetSet.ExtTextOut(DC: HDC; X, Y: Integer; Options: Longint; Rect: PRect; Str: PChar; Count: Longint; Dx: PInteger): Boolean;
var
  s: AnsiString;
  w: WideString;
  WinDC: HDC;
  WinBitmap: HBITMAP;
  WinDCBitmapOld: HGDIOBJ;
  lRawImage: TRawImage;
  WinRect: TRect;
  LazImage: TLazIntfImage;
  lCanvas: TLazCanvas;
  LazDC: TLazCanvas absolute DC;
  lSize: Types.TSize;
  Info: Windows.TBitmapInfo;
  BitsPtr: Pointer = nil;
begin
  if DC = 0 then Exit;

  // use temp buffer, if count is set, there might be no null terminator
  if count = -1 then
   s := str
  else
  begin
    SetLength(s, count);
    move(str^, PChar(s)^, count);
  end;
  if Length(s) = 0 then Exit;

  // Buffer bitmap preparation
  WinDC := Windows.CreateCompatibleDC(0);
  if Rect = nil then
  begin
    GetTextExtentPoint(WinDC, Str, Count, lSize);
  end
  else
  begin
    lSize.cx := Rect^.Right - Rect^.Left;
    lSize.cy := Rect^.Bottom - Rect^.Top;
  end;

  // Allocate a DIBSection, Windows itself will allocate the memory for the image
  FillChar(Info, SizeOf(Info), 0);
  Info.bmiHeader.biSize := SizeOf(Info.bmiHeader);
  Info.bmiHeader.biWidth := lSize.cx;
  Info.bmiHeader.biHeight := -lSize.cy; // request top down
  Info.bmiHeader.biPlanes := 1;
  Info.bmiHeader.biBitCount := 32;
  Info.bmiHeader.biCompression := BI_RGB;

  WinBitmap := Windows.CreateDIBSection(WinDC, Info, DIB_RGB_COLORS, BitsPtr, 0, 0);

  // Now connect the Windows bitmap to our own
  lRawImage.Init;
  lRawImage.Description.Init_BPP32_B8G8R8_M1_BIO_TTB(lSize.cx, lSize.cy);
  lRawImage.Data := BitsPtr;
  lRawImage.DataSize := lSize.cx * lSize.cy * 4;
  LazImage := TLazIntfImage.Create(lRawImage, False);
  lCanvas := TLazCanvas.Create(LazImage);
  lCanvas.FillColor(colWhite, True);

  WinDCBitmapOld := Windows.SelectObject(WinDC, WinBitmap);
  Windows.SelectObject(WinDC, GetStockObject(SYSTEM_FONT));
  Windows.SetTextColor(WinDC, clBlack);

  // the length of utf8 vs Wide/Ansi the strings differ, so recalc.
  // TODO: use the real number of chars (and not the lenght)
  W := UTF8ToUTF16(S);
  WinRect := Bounds(0, 0, lSize.cx, lSize.cy);
  Result := Windows.ExtTextOutW(WinDC, 0, 0, Options, nil, PWideChar(W), Length(W), Dx);

  // Draw the output to the canvas
  LazDC.AlphaBlendIgnoringDestPixels(lCanvas, X, Y, 0, 0, lSize.cx, lSize.cy);

  // Cleanup
  lCanvas.Free;
  LazImage.Free;
  Windows.SelectObject(WinDC, WinDCBitmapOld);
  Windows.DeleteObject(WinBitmap);
  Windows.ReleaseDC(0, WinDC);
end;
{$endif}

(*{------------------------------------------------------------------------------
  Function: ExtSelectClipRGN
  Params:  dc, RGN, Mode
  Returns: integer

  Combines the passed Region with the current clipping region in the device
  context (dc), using the specified mode.

  The Combine Mode can be one of the following:
      RGN_AND  : all points which are in both regions

      RGN_COPY : an exact copy of the source region, same as SelectClipRGN

      RGN_DIFF : all points which are in the Clipping Region but
                 but not in the Source.(Clip - RGN)

      RGN_OR   : all points which are in either the Clip Region or
                 in the Source.(Clip + RGN)

      RGN_XOR  : all points which are in either the Clip Region
                 or in the Source, but not in both.

  The result can be one of the following constants
      Error
      NullRegion
      SimpleRegion
      ComplexRegion


 ------------------------------------------------------------------------------}
function TWin32WidgetSet.ExtSelectClipRGN(dc: hdc; rgn : hrgn; Mode : Longint) : Integer;
begin
  Result := Windows.ExtSelectClipRGN(DC, RGN, Mode);
end;

{------------------------------------------------------------------------------
  Method:  FillRect
  Params:  DC    - handle to device context
           Rect  - record with rectangle
           Brush - handle to brush
  Returns: If the function succeeds

  The FillRect function fills a rectangle by using the specified brush.
  This function includes the left and top borders, but excludes the right and
  bottom borders of the rectangle.
 ------------------------------------------------------------------------------}
function TWin32WidgetSet.FillRect(DC: HDC; Const Rect: TRect; Brush: HBRUSH): Boolean;
var
  R: TRect;
begin
  R := Rect;
  Result := Boolean(Windows.FillRect(DC, Windows.RECT(R), Brush));
end;

{------------------------------------------------------------------------------
  Method:  FillRgn
  Params:  DC        - handle to device context
           RegionHnd - handle to region
           Brush     - handle to brush
  Returns: If the function succeeds

  The FillRgn function fills a region by using the specified brush.
 ------------------------------------------------------------------------------}
function TWin32WidgetSet.FillRgn(DC: HDC; RegionHnd: HRGN; hbr: HBRUSH): BOOL;
begin
  Result := Windows.FillRgn(DC, RegionHnd, hbr);
end;

function TWin32WidgetSet.FloodFill(DC: HDC; X, Y: Integer;
  Color: TGraphicsColor; FillStyle: TGraphicsFillStyle; Brush: HBRUSH): Boolean;
const
  FillType : array[TGraphicsFillStyle] of UINT =
    (FLOODFILLSURFACE, FLOODFILLBORDER);
var
  OldBrush: HGDIOBJ;
begin
  OldBrush := Windows.SelectObject(DC, Brush);
  Result := Boolean(Windows.ExtFloodFill(DC, X, Y, ColorToRGB(Color), FillType[FillStyle]));
  Windows.SelectObject(DC, OldBrush);
end;

{------------------------------------------------------------------------------
  Method: Frame3D
  Params: DC         - handle to device context
          Rect       - bounding rectangle
          FrameWidth - width of the frame (ignored on win32)
          Style      - frame style
  Returns: Whether the function was successful

  Draws a 3D border in win32 native style.
  NOTE: This function is mapped to DrawEdge on Windows.
------------------------------------------------------------------------------}
function TWin32WidgetSet.Frame3D(DC: HDC; var ARect: TRect;
  const FrameWidth: Integer; const Style: TBevelCut): Boolean;
const
  Edge: array[TBevelCut] of Integer =
  (
{bvNone   } 0,
{bvLowered} BDR_SUNKENOUTER,
{bvRaised } BDR_RAISEDINNER,
{bvSpace  } 0
  );
var
  I: Integer;
begin
  for I := 0 to FrameWidth - 1 do
    Result := Boolean(DrawEdge(DC, ARect, Edge[Style], BF_RECT or BF_ADJUST));
end;

function TWin32WidgetSet.FrameRect(DC: HDC; const ARect: TRect;
  hBr: HBRUSH) : integer;
begin
  Result := Windows.FrameRect(DC, PRect(@ARect)^, hBr);
end;

{------------------------------------------------------------------------------
  Method:  GetActiveWindow
  Params: none
  Returns: The handle to the active window

  Retrieves the window handle to the active window associated with the thread
  that calls the function.
 ------------------------------------------------------------------------------}
function TWin32WidgetSet.GetActiveWindow: HWND;
begin
  Result := Windows.GetActiveWindow;
end;

{------------------------------------------------------------------------------
  Method:  GetCapture
  Params:  none
  Returns: the handle of the capture window

  Retrieves the handle of the window (if any) that has captured the mouse.
 ------------------------------------------------------------------------------}
function TWin32WidgetSet.GetCapture: HWND;
begin
  Result := Windows.GetCapture;
end;

{------------------------------------------------------------------------------
  Method:  GetCaretPos
  Params:  LPPoint - record to receive coordinates
  Returns: If the function succeeds

  Gets the caret's position, in client coordinates.
 ------------------------------------------------------------------------------}
function TWin32WidgetSet.GetCaretPos(Var LPPoint: TPoint): Boolean;
begin
  Result := Boolean(Windows.GetCaretPos(@LPPoint));
end;

{------------------------------------------------------------------------------
  Method:  GetCharABCWidths
  Params:  DC         - handle of device context
           P2         - first character in range to query
           P3         - last character in range to query
           ABCStructs - character-width record
  Returns: If the function succeeds

  Retrieves the widths, in logical units, of consecutive characters in a given
  range from the current TrueType font.
 ------------------------------------------------------------------------------}
function TWin32WidgetSet.GetCharABCWidths(DC: HDC; P2, P3: UINT; Const ABCStructs): Boolean;
begin
  Result := Boolean(Windows.GetCharABCWidths(DC, P2, P3, ABCStructs));
end;*)

{------------------------------------------------------------------------------
  Method:  GetClientBounds
  Params: Handle - handle of window
          Rect   - record for client coordinates
  Returns: If the function succeeds

  Retrieves the coordinates of a window's client area.
 ------------------------------------------------------------------------------}

(*{------------------------------------------------------------------------------
  Function: GetClipBox
  Params: dc, lprect
  Returns: Integer

  Returns the smallest rectangle which includes the entire current
  Clipping Region.

  The result can be one of the following constants
      Error
      NullRegion
      SimpleRegion
      ComplexRegion


 ------------------------------------------------------------------------------}
function TWin32WidgetSet.GetClipBox(DC : hDC; lpRect : PRect) : Longint;
begin
  Result := Windows.GetClipBox(DC, Windows.LPRECT(lpRect));
end;

{------------------------------------------------------------------------------
  Function: GetClipRGN
  Params: dc, rgn
  Returns: Integer

  Returns the current Clipping Region.

  The result can be one of the following constants
      Error
      NullRegion
      SimpleRegion
      ComplexRegion


 ------------------------------------------------------------------------------}
function TWin32WidgetSet.GetClipRGN(DC : hDC; RGN : hRGN) : Integer;
begin
  Result := Windows.GetClipRGN(DC, RGN);
end;

{------------------------------------------------------------------------------
  Method: GetCurrentObject
  Params:
    DC - A handle to the DC
    uObjectType - The object type to be queried
  Returns: If the function succeeds, the return value is a handle to the specified object.
    If the function fails, the return value is NULL.
 ------------------------------------------------------------------------------}

function TWin32WidgetSet.GetCurrentObject(DC: HDC; uObjectType: UINT): HGDIOBJ;
begin
  Result := Windows.GetCurrentObject(DC, uObjectType);
end;*)

{------------------------------------------------------------------------------
  Method:  GetCursorPos
  Params:  LPPoint - record to receive coordinates
  Returns: True if the function succeeds

  Gets the cursor position, in screen coordinates.
 ------------------------------------------------------------------------------}

(*{------------------------------------------------------------------------------
  Method:  GetDC
  Params:  HWND - handle of window
  Returns: value identifying the device context for the given window's client
           area

  Retrieves a handle of a display device context (DC) for the client area of
  the specified window.
 ------------------------------------------------------------------------------}
function TCDWidgetSet.BackendGetDC(hWnd: HWND): HDC;
var
  lWindowInfo: PWindowInfo;
  //  ORect: TRect;
begin
  {$ifdef VerboseCDWinAPI}
  DebugLn('Trace:> [WinAPI GetDC] hWnd: ', dbghex(hWnd));
  {$endif}

  Result := 0;

  // screenshot taking
  if hWnd = 0 then Exit;

  // Form Canvas
  lWindowInfo := GetWindowInfo(hWNd);
  if lWindowInfo^.Canvas = nil then lWindowInfo^.Canvas := TLazCanvas.create(nil);
  Result := HDC(lWindowInfo^.Canvas);

  {  Result := Windows.GetDC(HWnd);
    if (Result <> 0) and (HWnd <> 0) and GetLCLClientBoundsOffset(HWnd, ORect) then
      MoveWindowOrgEx(Result, ORect.Left, ORect.Top);}

  {$ifdef VerboseCDWinAPI}
  DebugLn('Trace:< [WinAPI GetDC] Result: ', dbghex(Result));
  {$endif}
end;
*)

{------------------------------------------------------------------------------
  Method: GetDeviceCaps
  Params: DC - display device context
          Index - index of needed capability

  Returns device specific information
 ------------------------------------------------------------------------------}

(*function TWin32WidgetSet.GetDCOriginRelativeToWindow(PaintDC: HDC;
  WindowHandle: HWND; var OriginDiff: TPoint): boolean;
var
  DCOrg, winOrg: Windows.POINT;
  ORect: TRect;
begin
  OriginDiff.X := 0;
  OriginDiff.Y := 0;
  // there is no way to get offset for memory or metafile DC => assume it 0
  Result := GetObjectType(PaintDC) = OBJ_DC;
  if not Result then Exit;
  Result := Windows.GetDCOrgEx(PaintDC, DCOrg);
  if not Result then Exit;
  winOrg.X := 0;
  winOrg.Y := 0;
  Result := Windows.ClientToScreen(WindowHandle, winOrg);
  if not Result then exit;
  Result := GetLCLClientBoundsOffset(WindowHandle, ORect);
  if not Result then exit;
  OriginDiff.X := DCOrg.X - winOrg.X - ORect.Left;
  OriginDiff.Y := DCOrg.Y - winOrg.Y - ORect.Top;
  Result := Windows.GetWindowOrgEx(PaintDC, winOrg);
  if not Result then exit;
  dec(OriginDiff.X, winOrg.X);
  dec(OriginDiff.Y, winOrg.Y);
end;

function TWin32WidgetSet.GetDeviceSize(DC: HDC; var P: TPoint): Boolean;
var
  hBitmap: HGDIOBJ;
  hWindow: HWND;
  BitmapInfo: BITMAP;
  ClientRect: TRect;
begin
  // check if memory dc with bitmap
  Result := false;
  case GetObjectType(DC) of
    OBJ_MEMDC:
      begin
        hBitmap := GetCurrentObject(DC, OBJ_BITMAP);
        if hBitmap <> HGDIOBJ(nil) then
        begin
          GetObject(hBitmap, SizeOf(BITMAP), @BitmapInfo);
          P.x := BitmapInfo.bmWidth;
          P.y := BitmapInfo.bmHeight;
          Result := true;
        end;
      end;
    OBJ_DC:
      begin
        hWindow := WindowFromDC(DC);
        if hWindow <> HWND(nil) then
        begin
          Result := GetClientRect(hWindow, ClientRect);
          P.x := ClientRect.Right;
          P.y := ClientRect.Bottom;
        end;
      end;
  else
  end;

  if not Result then
  begin
    // do default
    Result := inherited GetDeviceSize(DC, P);
  end;
end;

function TWin32WidgetSet.GetDIBits(DC: HDC; Bitmap: HBitmap; StartScan, NumScans: UINT; Bits: Pointer; var BitInfo: BitmapInfo; Usage: UINT): Integer;
begin
  Result := Windows.GetDIBits(DC, Bitmap, StartScan, NumScans, Bits, Windows.PBitmapInfo(@BitInfo)^, Usage)
end;

function TWin32WidgetSet.GetBitmapBits(Bitmap: HBITMAP; Count: Longint;  Bits: Pointer): Longint;
begin
  Result := Windows.GetBitmapBits(Bitmap, Count, Bits);
end;

function TWin32WidgetSet.GetBkColor(DC: HDC): TColorRef;
begin
  Result := TColorRef(Windows.GetBkColor(DC));
end;


function TWin32WidgetSet.CreateDIBSection(DC: HDC; const p2: tagBitmapInfo; p3: UINT;
  var p4: Pointer; p5: THandle; p6: DWORD): HBITMAP;
begin
  Result := Windows.CreateDIBSection(DC, Windows.PBitmapInfo(@p2)^,  p3, p4, p5, p6)
end;

{------------------------------------------------------------------------------
  Method:  CreateEllipticRgn
  Params:  p1, p2 = X1 and Y1 top-left position of the ellipse
  Params:  p3, p4 = X2 and Y2 bottom-right position of the ellipse
  Returns: The handle of the region created

  Creates an elliptic region.
 ------------------------------------------------------------------------------}
function TWin32WidgetSet.CreateEllipticRgn(p1, p2, p3, p4: Integer): HRGN;
begin
  Result := Windows.CreateEllipticRgn(p1, p2, p3, p4);
end;

{------------------------------------------------------------------------------
  Function: GetDoubleClickTime
  Params: none
  Returns:

 ------------------------------------------------------------------------------}
function TWin32WidgetSet.GetDoubleClickTime: UINT;
begin
  Result := Windows.GetDoubleClickTime;
end;

{------------------------------------------------------------------------------
  Method:  GetFocus
  Params:  none
  Returns: The handle of the window with focus

  The GetFocus function retrieves the handle of the window that has the focus.
 ------------------------------------------------------------------------------}
function TWin32WidgetSet.GetFocus: HWND;
begin
  Result := Windows.GetFocus;
end;

{------------------------------------------------------------------------------
  Method:  GetForegroundWindow
  Params:  none
  Returns: The handle of the foreground window

  The GetForegroundWindow function returns the window that the user is
  currently working with, session wide.
 ------------------------------------------------------------------------------}
function TWin32WidgetSet.GetForegroundWindow: HWND;
begin
  Result := Windows.GetForegroundWindow;
end;

function TWin32WidgetSet.GetIconInfo(AIcon: HICON; AIconInfo: PIconInfo): Boolean;
begin
  Result := Windows.GetIconInfo(AIcon, AIconInfo);
end;*)

{------------------------------------------------------------------------------
  Method:  GetKeyState
  Params:  NVirtKey - The requested key
  Returns: If the function succeeds, the return value specifies the status of
           the given virtual key. If the high-order bit is 1, the key is down;
           otherwise, it is up. If the low-order bit is 1, the key is toggled.

  The GetKeyState function retrieves the status of the specified virtual key.
 ------------------------------------------------------------------------------}
function TCDWidgetSet.GetKeyState(nVirtKey: Integer): Smallint;
begin
  Result := Windows.GetKeyState(nVirtKey);
end;

(*{------------------------------------------------------------------------------
  Method: GetMapMode
  Params: DC - display device context

  Returns mapping mode for the device context or zero if unsuccessfull
 ------------------------------------------------------------------------------}
function TWin32WidgetSet.GetMapMode(DC: HDC): Integer;
begin
  Result := Windows.GetMapMode(DC);
end;

function TWin32WidgetSet.GetMonitorInfo(hMonitor: HMONITOR; lpmi: LCLType.PMonitorInfo): Boolean;
{$IFDEF WindowsUnicodeSupport}
var
  LocalInfo: TMonitorInfoExW;
{$ENDIF}
begin
  {$IFDEF WindowsUnicodeSupport}
  if (lpmi <> nil) and (lpmi^.cbSize >= SizeOf(TMonitorInfoEx)) then
  begin
    if UnicodeEnabledOS then
    begin
      LocalInfo.cbSize := SizeOf(TMonitorInfoExW);
      Result := MultiMon.GetMonitorInfo(hMonitor, @LocalInfo);
      lpmi^.rcMonitor := LocalInfo.rcMonitor;
      lpmi^.rcWork := LocalInfo.rcWork;
      lpmi^.dwFlags := LocalInfo.dwFlags;
      PMonitorInfoEx(lpmi)^.szDevice := UTF16ToUTF8(LocalInfo.szDevice);
    end
    else
      PMonitorInfoEx(lpmi)^.szDevice := AnsiToUtf8(PMonitorInfoEx(lpmi)^.szDevice);
  end
  else
  {$ENDIF}
    Result := MultiMon.GetMonitorInfo(hMonitor, LPMonitorInfo(lpmi));
end;

{------------------------------------------------------------------------------
  Method:  GetObject
  Params:  GDIObj  - handle to graphics object of interest
           BufSize - size of buffer for object information
           Buf     - pointer to buffer for object information
  Returns: the number of bytes stored into the buffer

  Gets information about a specified graphics object.
 ------------------------------------------------------------------------------}
function TWin32WidgetSet.GetObject(GDIObj: HGDIOBJ; BufSize: Integer; Buf: Pointer): Integer;
{$ifdef WindowsUnicodeSupport}
var
  LF: PLogFontA absolute Buf;
  LFW: TLogFontW;
{$endif}
begin
  {$ifdef WindowsUnicodeSupport}
  if GetObjectType(GDIObj) = OBJ_FONT then
  begin
    if (UnicodeEnabledOS) and (BufSize = Sizeof(LOGFONTA)) then
    begin
      BufSize := SizeOf(LogFontW);
      Result := Windows.GetObjectW(GDIObj, BufSize, @LFW);
      Move(LFW, LF^, SizeOf(LogFontA) - SizeOf(LOGFONTA.lfFaceName));
      LF^.lfFaceName := UTF16ToUTF8(LFW.lfFaceName);
    end
    else
    begin
      Result := Windows.GetObject(GDIObj, BufSize, Buf);
      if (BufSize >= Sizeof(LOGFONTA)) and (Result <= BufSize) then
        LF^.lfFaceName := AnsiToUtf8(LF^.lfFaceName);
    end;
  end
  else
  {$endif}
  Result := Windows.GetObject(GDIObj, BufSize, Buf);
end;

{------------------------------------------------------------------------------
  Method:  GetParent
  Params: Handle - handle of child window
  Returns: the handle of the parent window

  Retrieves the handle of the specified child window's parent window.
 ------------------------------------------------------------------------------}
function TWin32WidgetSet.GetParent(Handle: HWND): HWND;
begin
  Result := Windows.GetParent(Handle);
end;


{------------------------------------------------------------------------------
  Method:  GetProp
  Params: Handle - handle of window
          Str    - string
  Returns: the associated data

  Retrieves a pointer to data from the property list of the given window.
 ------------------------------------------------------------------------------}
function TWin32WidgetSet.GetProp(Handle: HWND; Str: PChar): Pointer;
begin
  Result := Pointer(Windows.GetProp(Handle, Str));
end;


{------------------------------------------------------------------------------
  Method:  GetROP2
  Params: DC - Handle of the device context

  Returns: 0 if unsuccessful, the current Foreground Mixing Mode if successul

  Retrieves the current Foreground Mixing Mode in the given device context
 ------------------------------------------------------------------------------}
function TWin32WidgetSet.GetROP2(DC: HDC): Integer;
begin
	Result := Windows.GetROP2(DC);
end;

function TWin32WidgetSet.GetRGNBox(Rgn: HRGN; lpRect: PRect): Longint;
begin
  Result:= Windows.GetRgnBox(Rgn, Windows.LPRECT(lpRect));
end;

{------------------------------------------------------------------------------
  Method:  GetScrollInfo
  Params:  Handle     - handle of window with scroll bar
           BarFlag    - scroll bar flag
           ScrollInfo - record for scroll parameters
  Returns: If the function retrieved any values.

  Retrieves the parameters of a scroll bar.
 ------------------------------------------------------------------------------}
function TWin32WidgetSet.GetScrollInfo(Handle: HWND; BarFlag: Integer; var ScrollInfo: TScrollInfo): Boolean;
begin
  ScrollInfo.cbSize := SizeOf(ScrollInfo);
  Result := Boolean(Windows.GetScrollInfo(Handle, BarFlag, @ScrollInfo));
end;

{------------------------------------------------------------------------------
  Method:  GetStockObject
  Params:  Value - type of stock object
  Returns: a value identifying the logical object requested

  Retrieves a handle to one of the predefined stock objects.
 ------------------------------------------------------------------------------}
function TWin32WidgetSet.GetStockObject(Value: Integer): THandle;
begin
  Result := Windows.GetStockObject(Value);
end;

{------------------------------------------------------------------------------
  Method:  GetSysColor
  Params:   NIndex - display element whose color is to be retrieved
  Returns:  RGB value

  Retrieves the current color of the specified display element.
 ------------------------------------------------------------------------------}
function TWin32WidgetSet.GetSysColor(NIndex: Integer): DWORD;
begin
  if NIndex = COLOR_FORM then
    NIndex := COLOR_BTNFACE;
  Result := Windows.GetSysColor(nIndex);
end;

function TWin32WidgetSet.GetSysColorBrush(nIndex: Integer): HBrush;
begin
  if NIndex = COLOR_FORM then
    NIndex := COLOR_BTNFACE;
  Result := Windows.GetSysColorBrush(nIndex);
end;*)

{------------------------------------------------------------------------------
  Method:  GetSystemMetrics
  Params: NIndex - system metric to retrieve
  Returns: the requested system metric

  Retrieves various system metrics.
 ------------------------------------------------------------------------------}
function TCDWidgetSet.GetSystemMetrics(NIndex: Integer): Integer;
begin
  Result := Windows.GetSystemMetrics(NIndex);
end;

{$ifdef CD_UseNativeText}
// MaxCount is provided in the number of UTF-8 characters, not bytes
function TCDWidgetSet.GetTextExtentExPoint(DC: HDC; Str: PChar;
  Count, MaxWidth: Integer; MaxCount, PartialWidths: PInteger; var Size: TSize): Boolean;
var
  LCLStr: string;
  w: WideString;
  WinDC: HDC;
begin
  WinDC := Windows.CreateCompatibleDC(0);

  // use temp buffer, if count is set, there might be no null terminator
  if count = -1 then
    LCLStr := Str
  else
  begin
    SetLength(LCLStr, count);
    move(str^, PChar(LCLStr)^, count);
  end;
  // the length of utf8 vs Wide/Ansi the strings differ, so recalc.
  // TODO: use the real number of chars (and not the length)
  w := UTF8ToUTF16(LCLStr);
  Result := Windows.GetTextExtentExPointW(WinDC, PWideChar(W), Length(W),
    MaxWidth, MaxCount, PartialWidths, Size);

  Windows.ReleaseDC(0, WinDC);
end;

{------------------------------------------------------------------------------
  Method:  GetTextExtentPoint
  Params:  DC    - handle of device context
           Str   - text string encoded in UTF-8
           Count - number of bytes in the string
           Size  - TSize record in which the dimensions of the string are to be
                   returned
  Returns: If the function succeeded

  Computes the width and height of the specified string of text.
 ------------------------------------------------------------------------------}
function TCDWidgetSet.GetTextExtentPoint(DC: HDC; Str: PChar; Count: Integer; var Size: TSize): Boolean;
var
  s: AnsiString;
  w: WideString;
  WinDC: HDC;
begin
  WinDC := Windows.CreateCompatibleDC(0);

  // use temp buffer, if count is set, there might be no null terminator
  if count = -1 then
    s := str
  else
  begin
    SetLength(s, count);
    move(str^, PChar(s)^, count);
  end;
  // the length of utf8 vs Wide/Ansi the strings differ, so recalc.
  // TODO: use the real number of chars (and not the length)
  w := UTF8ToUTF16(S);
  {$ifdef WinCE}
  Result := Windows.GetTextExtentPoint32W(WinDC, PWideChar(W), Length(W), Size);
  {$else}
  Result := Windows.GetTextExtentPoint32W(WinDC, PWideChar(W), Length(W), @Size);
  {$endif}

  Windows.ReleaseDC(0, WinDC);
  Result := True;
end;

{------------------------------------------------------------------------------
  Method:  GetTextMetrics
  Params:  DC - handle of device context
           TM - text metrics record
  Returns: If the function succeeds

  Fills the specified buffer with the metrics for the currently selected font.
 ------------------------------------------------------------------------------}
function TCDWidgetSet.GetTextMetrics(DC: HDC; Var TM: TTextMetric): Boolean;
var
  WinDC: HDC;
begin
  WinDC := Windows.CreateCompatibleDC(0);
  Result := Boolean(Windows.GetTextMetrics(WinDC, @TM));
  Windows.ReleaseDC(0, WinDC);
end;
{$endif}

(*function TWin32WidgetSet.GetViewPortExtEx(DC: HDC; Size: PSize): Integer;
begin
  Result := Integer(Windows.GetViewPortExtEx(DC, LPSize(Size)));
end;

function TWin32WidgetSet.GetViewPortOrgEx(DC: HDC; P: PPoint): Integer;
begin
  Result := Integer(Windows.GetViewPortOrgEx(DC, LPPoint(P)));
end;

function TWin32WidgetSet.GetWindowExtEx(DC: HDC; Size: PSize): Integer;
begin
  Result := Integer(Windows.GetWindowExtEx(DC, LPSize(Size)));
end;

{------------------------------------------------------------------------------
  Method:  GetWindowLong
  Params:  Handle - handle of window
           Int    - value to retrieve
  Returns: the requested 32-bit value

  Retrieves information about the specified window.
 ------------------------------------------------------------------------------}
function TWin32WidgetSet.GetWindowLong(Handle: HWND; Int: Integer): PtrInt;
begin
  if UnicodeEnabledOS then
    Result := Windows.GetWindowLongPtrW(Handle, int)
  else
    Result := Windows.GetWindowLongPtr(Handle, int);
end;

{------------------------------------------------------------------------------
  Method:  GetWindowOrgEx
  Params:  DC - handle of device context
           P  - record receiving the window origin
  Returns: 0 if the function fails; non-zero integer otherwise

  Retrieves the x-coordinates and y-coordinates of the window origin for the
  specified device context.
 ------------------------------------------------------------------------------}
function TWin32WidgetSet.GetWindowOrgEx(DC: HDC; P: PPoint): Integer;
begin
  Result := Integer(Windows.GetWindowOrgEx(DC, LPPoint(P)));
end;

{------------------------------------------------------------------------------
  Method:  GetWindowRect
  Params:  Handle - handle of window
           Rect   - record for window coordinates
  Returns: if the function succeeds, the return value is nonzero; if the
           function fails, the return value is zero

  Retrieves the dimensions of the bounding rectangle of the specified window.
 ------------------------------------------------------------------------------}
function TWin32WidgetSet.GetWindowRect(Handle: HWND; Var Rect: TRect): Integer;
begin
  Result := Integer(Windows.GetWindowRect(Handle, @Rect));
end;*)

{------------------------------------------------------------------------------
  Function: GetWindowRelativePosition
  Params:  Handle : HWND;
  Returns: true on success

  returns the current widget Left, Top, relative to the client origin of its
  parent
 ------------------------------------------------------------------------------}
function TCDWidgetSet.BackendGetWindowRelativePosition(Handle: HWND;
  var Left, Top: Integer): Boolean;
begin
  Result := False;

end;

{------------------------------------------------------------------------------
  Function: GetWindowSize
  Params:  Handle : hwnd;
  Returns: true on success

  Returns the current widget Width and Height

  Note: Windows.GetWindowInfo doesnt exist in wince, but
  we can use GetWindowLong and other APIs for most information
  Also GetWindowPlacement doesnt exist
  ------------------------------------------------------------------------------}
(*
function TWin32WidgetSet.GradientFill(DC: HDC; Vertices: PTriVertex;
  NumVertices: Longint; Meshes: Pointer; NumMeshes: Longint; Mode: Longint): Boolean;
begin
  Result := Win32Extra.GradientFill(DC, Windows.PTRIVERTEX(Vertices),
    NumVertices, Meshes, NumMeshes, Mode);
end;

{------------------------------------------------------------------------------
  Method:  HideCaret
  Params:  HWnd - handle to the window with the caret
  Returns: Whether the window owns the caret

  Removes the caret from the screen.
 ------------------------------------------------------------------------------}
function TWin32WidgetSet.HideCaret(HWnd: HWND): Boolean;
begin
{$ifdef DEBUG_CARET}
  DebugLn('[HideCaret] for window ', IntToHex(HWnd, 8));
{$endif}
  Result := Boolean(Windows.HideCaret(hWnd));
end; *)

{------------------------------------------------------------------------------
  Method:  InvalidateRect
  Params: AHandle - handle of window with changed update region
          Rect    - address of rectangle coordinates
          BErase  - specifies whether the background is to be erased
  Returns: if the function succeeds

  Adds a rectangle to the specified window's update region.
 ------------------------------------------------------------------------------}

(*{------------------------------------------------------------------------------
  Method:  InvalidateRgn
  Params:  Handle - handle of window with changed update region
           Rgn    - handle to region to invalidate
           Erase  - specifies whether the background is to be erased
  Returns: if the function succeeds

  Adds a region to the specified window's update region.
 ------------------------------------------------------------------------------}
function TWin32WidgetSet.InvalidateRgn(Handle: HWND; Rgn: HRGN; Erase: Boolean): Boolean;
begin
  Result := Boolean(Windows.InvalidateRgn(Handle, Rgn, Erase));
end;

function TWin32WidgetSet.IsIconic(handle: HWND): boolean;
begin
  Result := Windows.IsIconic(handle);
end;

{------------------------------------------------------------------------------
  Function: IntersectClipRect
  Params:  dc, Left, Top, Right, Bottom
  Returns: Integer

  Shrinks the clipping region in the device context dc to a region of all
  intersecting points between the boundary defined by Left, Top, Right,
  Bottom , and the Current clipping region.

  The result can be one of the following constants
      Error
      NullRegion
      SimpleRegion
      ComplexRegion

 ------------------------------------------------------------------------------}
function TWin32WidgetSet.IntersectClipRect(dc: hdc;
  Left, Top, Right, Bottom: Integer): Integer;
begin
  Result := Windows.IntersectClipRect(DC, Left, Top, Right, Bottom);
end;

{------------------------------------------------------------------------------
  Method: IsWindowEnabled
  Params: handle - window handle
  Returns: true if handle is window, false otherwise
 ------------------------------------------------------------------------------}
function TWin32WidgetSet.IsWindow(handle: HWND): boolean;
begin
  Result := Boolean(Windows.IsWindow(Handle));
end;

{------------------------------------------------------------------------------
  Method: IsWindowEnabled
  Params: handle - window handle
  Returns: true if window is enabled, false otherwise
 ------------------------------------------------------------------------------}
function TWin32WidgetSet.IsWindowEnabled(handle: HWND): boolean;
begin
  Result := Boolean(Windows.IsWindowEnabled(handle));
end;

{------------------------------------------------------------------------------
  Method: IsWindowVisible
  Params: handle - window handle
  Returns: true if window is visible, false otherwise
 ------------------------------------------------------------------------------}
function TWin32WidgetSet.IsWindowVisible(handle: HWND): boolean;
begin
  Result := Boolean(Windows.IsWindowVisible(handle));
end;

function TWin32WidgetSet.IsZoomed(handle: HWND): boolean;
begin
  Result := Windows.IsZoomed(handle);
end;

{------------------------------------------------------------------------------
  Method:  LineTo
  Params:  DC - device context handle
           X  - x-coordinate of line's ending point
           Y  - y-coordinate of line's ending point
  Returns: if the function succeeds

  Draws a line from the current position up to, but not including, the specified point.
 ------------------------------------------------------------------------------}
function TWin32WidgetSet.LineTo(DC: HDC; X, Y: Integer): Boolean;
begin
  Result := Boolean(Windows.LineTo(DC, X, Y));
end;

function TWin32WidgetSet.LPtoDP(DC: HDC; var Points; Count: Integer): BOOL;
begin
  Result := Windows.LPtoDP(DC, Points, Count);
end;

{------------------------------------------------------------------------------
  Method:  MaskBlt
  Params:  DestDC        - The destination device context
           X, Y          - The left/top corner of the destination rectangle
           Width, Height - The size of the destination rectangle
           SrcDC         - The source device context
           XSrc, YSrc    - The left/top corner of the source rectangle
           Mask          - The handle of a monochrome bitmap
           XMask, YMask  - The left/top corner of the mask rectangle
           Rop           - The raster operation to be performed
  Returns: True if succesful

  The MaskBlt function copies a bitmap from a source context into a destination
  context using the specified mask and raster operation.
 ------------------------------------------------------------------------------}
function TWin32WidgetSet.MaskBlt(DestDC: HDC; X, Y, Width, Height: Integer; SrcDC: HDC; XSrc, YSrc: Integer; Mask: HBITMAP; XMask, YMask: Integer; Rop: DWORD): Boolean;
begin
  Result := Boolean(Windows.MaskBlt(DestDC, X, Y, Width, Height, SrcDC, XSrc, YSrc, Mask, XMask, YMask, Rop));
end;

function TWin32WidgetSet.MaskBlt(DestDC: HDC; X, Y, Width, Height: Integer; SrcDC: HDC; XSrc, YSrc: Integer; Mask: HBITMAP; XMask, YMask: Integer): Boolean;
begin
  Result := StretchMaskBlt(DestDC, X, Y, Width, Height, SrcDC, XSrc, YSrc, Width, Height, Mask, XMask, YMask, SRCCOPY);
end;*)

{------------------------------------------------------------------------------
  Method:  MessageBox
  Params:  HWnd      - The handle of parent window
           LPText    - text in message box
           LPCaption - title of message box
           UType     - style of message box
  Returns: 0 if not successful (out of memory), otherwise one of the defined
           values:
             IDABORT
             IDCANCEL
             IDIGNORE
             IDNO
             IDOK
             IDRETRY
             IDYES

 The MessageBox function displays a modal dialog, with text and caption defined,
 and includes buttons.
 ------------------------------------------------------------------------------}

(*function TWin32WidgetSet.MonitorFromPoint(ptScreenCoords: TPoint; dwFlags: DWord): HMONITOR;
begin
  Result := MultiMon.MonitorFromPoint(ptScreenCoords, dwFlags);
end;

function TWin32WidgetSet.MonitorFromRect(lprcScreenCoords: PRect; dwFlags: DWord): HMONITOR;
begin
  Result := MultiMon.MonitorFromRect(lprcScreenCoords, dwFlags);
end;

function TWin32WidgetSet.MonitorFromWindow(hWnd: HWND; dwFlags: DWord): HMONITOR;
begin
  Result := MultiMon.MonitorFromWindow(hWnd, dwFlags);
end;

{------------------------------------------------------------------------------
  Method:  MoveToEx
  Params:  DC       - handle of device context
           X        - x-coordinate of new current position
           Y        - x-coordinate of new current position
           OldPoint - address of old current position
  Returns: If the function succeeds.

  Updates the current position to the specified point.
 ------------------------------------------------------------------------------}
function TWin32WidgetSet.MoveToEx(DC: HDC; X, Y: Integer; OldPoint: PPoint): Boolean;
begin
  Result := Boolean(Windows.MoveToEx(DC, X, Y, LPPOINT(OldPoint)));
end;

function TWin32WidgetSet.OffsetRgn(RGN: HRGN; nXOffset, nYOffset: Integer): Integer;
begin
  Result := Windows.OffsetRgn(RGN, nXOffset, nYOffset);
end;


{------------------------------------------------------------------------------
  Method:  PeekMessage
  Params:  LPMsg         - Where it should put the message
           Handle        - Handle of the window (thread)
           WMsgFilterMin - Lowest MSG to grab
           WMsgFilterMax - Highest MSG to grab
           WRemoveMsg    - Should message be pulled out of the queue
  Returns: Boolean if an event was there

  Checks a thread message queue for a message.
 ------------------------------------------------------------------------------}
function TWin32WidgetSet.PeekMessage(Var LPMsg: TMsg; Handle: HWND; WMsgFilterMin, WMsgFilterMax, WRemoveMsg: UINT): Boolean;
begin
  {$IFDEF WindowsUnicodeSupport}
    if UnicodeEnabledOS then
      Result := Boolean(Windows.PeekMessageW(@LPMsg, Handle, WMsgFilterMin, WMsgFilterMax, WRemoveMsg))
    else
      Result := Boolean(Windows.PeekMessage(@LPMsg, Handle, WMsgFilterMin, WMsgFilterMax, WRemoveMsg));
  {$ELSE}
    Result := Boolean(Windows.PeekMessage(@LPMsg, Handle, WMsgFilterMin, WMsgFilterMax, WRemoveMsg));
  {$ENDIF}
end;

function TWin32WidgetSet.Pie(DC: HDC; EllipseX1, EllipseY1, EllipseX2,
  EllipseY2, StartX, StartY, EndX, EndY: Integer): Boolean;
begin
  Result := Boolean(Windows.Pie(DC,EllipseX1, EllipseY1, EllipseX2, EllipseY2,
                                   StartX, StartY, EndX, EndY));
end;

{------------------------------------------------------------------------------
  Method:  PolyBezier
  Params:  DC, Points, NumPts, Filled, Continous
  Returns: Boolean

  Use Polybezier to draw cubic Bézier curves. The first curve is drawn from the
  first point to the fourth point with the second and third points being the
  control points. If the Continuous flag is TRUE then each subsequent curve
  requires three more points, using the end-point of the previous Curve as its
  starting point, the first and second points being used as its control points,
  and the third point its end-point. If the continous flag is set to FALSE,
  then each subsequent Curve requires 4 additional points, which are used
  excatly as in the first curve. Any additonal points which do not add up to
  a full bezier(4 for Continuous, 3 otherwise) are ingored. There must be at
  least 4 points for an drawing to occur. If the Filled Flag is set to TRUE
  then the resulting Poly-Bézier will be drawn as a Polygon.

 ------------------------------------------------------------------------------}
function TWin32WidgetSet.PolyBezier(DC: HDC; Points: PPoint; NumPts: Integer;
  Filled, Continuous: Boolean): Boolean;
begin
  If Filled or (not Continuous) then
    Result := Inherited PolyBezier(DC,Points,NumPts, Filled, Continuous)
  else
    Result := Boolean(Windows.PolyBezier(DC, LPPOINT(Points)^, NumPts));
end;

{------------------------------------------------------------------------------
  Method:  Polygon
  Params:  DC - handle to device context
           Points - pointer to polygon's vertices
           NumPts - count of polygon's vertices
           Winding
  Returns: If the function succeeds

  Use Polygon to draw a closed, many-sided shape on the canvas, using the value
  of Pen. After drawing the complete shape, Polygon fills the shape using the
  value of Brush.
  The Points parameter is an array of points that give the vertices of the
  polygon.
  Winding determines how the polygon is filled.
  When Winding is True, Polygon
  fills the shape using the Winding fill algorithm. When Winding is False,
  Polygon uses the even-odd (alternative) fill algorithm.
  NumPts indicates the number of points to use.
  The first point is always connected to the last point.
  To draw a polygon on the canvas, without filling it, use the Polyline method,
  specifying the first point a second time at the end.
 ------------------------------------------------------------------------------}
function TWin32WidgetSet.Polygon(DC: HDC; Points: PPoint; NumPts: Integer; Winding: Boolean): Boolean;
var
  PFMode : Longint;
begin
  If Winding then
    PFMode := SetPolyFillMode(DC, Windows.WINDING)
  else
    PFMode := SetPolyFillMode(DC, Windows.ALTERNATE);
  Result := Boolean(Windows.Polygon(DC, LPPOINT(Points)^, NumPts));
  SetPolyFillMode(DC, PFMode);
end;

{------------------------------------------------------------------------------
  Method:  Polyline
  Params:  DC - handle of device context
           Points - address of array containing endpoints
           NumPts - number of points in the array
  Returns: If the function succeeds

  Draws a series of line segments by connecting the points in the specified
  array.
 ------------------------------------------------------------------------------}
function TWin32WidgetSet.Polyline(DC: HDC; Points: PPoint; NumPts: Integer): Boolean;
begin
  Result := Boolean(Windows.Polyline(DC, LPPOINT(Points)^, NumPts));
end;

{------------------------------------------------------------------------------
  Method:  PostMessage
  Params: Handle - handle of destination window
          Msg - message to post
          WParam - first message parameter
          LParam - second message parameter
  Returns: True if succesful

  The PostMessage function places (posts) a message in the message queue and
  then returns without waiting.
 ------------------------------------------------------------------------------}
function TWin32WidgetSet.PostMessage(Handle: HWND; Msg: Cardinal; WParam: WParam; LParam: LParam): Boolean;
begin
  Result := Boolean(Windows.PostMessage(Handle, Msg, WParam, LParam));
end;

{------------------------------------------------------------------------------
  Method:  PtInRegion
  Params:  Rgn  - handle of region
           X, Y - Point coordinates to test
  Returns: If the specified point lies in the region
 ------------------------------------------------------------------------------}
function TWin32WidgetSet.PtInRegion(Rgn: HRGN; X, Y: Integer): Boolean;
begin
  Result := Boolean(Windows.PtInRegion(Rgn, X, Y));
end;

{------------------------------------------------------------------------------
  Method:   RadialArc
  Params:   DC, left, top, right, bottom, sx, sy, ex, ey
  Returns:  Nothing

  Use RadialArc to draw an elliptically curved line with the current Pen. The
  values sx,sy, and ex,ey represent the starting and ending radial-points
  between which the Arc is drawn.

------------------------------------------------------------------------------}
function TWin32WidgetSet.RadialArc(DC: HDC; left, top, right, bottom, sx, sy, ex, ey: Integer): Boolean;
begin
  Result := Boolean(Windows.Arc(DC, left, top, right, bottom, sx, sy, ex, ey));
end;

{------------------------------------------------------------------------------
  Method:   RadialChord
  Params:   DC, x1, y1, x2, y2, sx, sy, ex, ey
  Returns:  Nothing

  Use RadialChord to draw a filled Chord-shape on the canvas. The values sx,sy,
  and ex,ey represent the starting and ending radial-points between which
  the bounding-Arc is drawn.

------------------------------------------------------------------------------}
function TWin32WidgetSet.RadialChord(DC: HDC; x1, y1, x2, y2, sx, sy, ex, ey: Integer): Boolean;
begin
  Result := Boolean(Windows.Chord(DC, x1, y1, x2, y2, sx, sy, ex, ey));
end;

{------------------------------------------------------------------------------
  Method:  RealizePalette
  Params:  DC - handle of device context
  Returns: number of entries in the logical palette mapped to the system
           palette

  Maps palette entries from the current logical palette to the system palette.
 ------------------------------------------------------------------------------}
function TWin32WidgetSet.RealizePalette(DC: HDC): Cardinal;
begin
  Result := Windows.RealizePalette(DC);
end;

{------------------------------------------------------------------------------
  Method:  Rectangle
  Params:  DC - handle of device context
           X1 - x-coordinate of bounding rectangle's upper-left corner
           Y1 - y-coordinate of bounding rectangle's upper-left corner
           X2 - x-coordinate of bounding rectangle's lower-right corner
           Y2 - y-coordinate of bounding rectangle's lower-right corner
  Returns: If the function succeeds

  The Rectangle function draws a rectangle. The rectangle is outlined by using
  the current pen and filled by using the current brush.
 ------------------------------------------------------------------------------}
function TWin32WidgetSet.Rectangle(DC: HDC; X1, Y1, X2, Y2: Integer): Boolean;
begin
  Result := Boolean(Windows.Rectangle(DC, X1, Y1, X2, Y2));
end;

function TWin32WidgetSet.RectVisible(dc : hdc; const ARect: TRect) : Boolean;
begin
  Result := Windows.RectVisible(DC, LPRECT(@ARect)^);
end;

{------------------------------------------------------------------------------
  Function: RedrawWindow
  Params: Wnd:
          lprcUpdate:
          hrgnUpdate:
          flags:
  Returns:

 ------------------------------------------------------------------------------}
function TWin32WidgetSet.RedrawWindow(Wnd: HWND; lprcUpdate: PRECT; hrgnUpdate: HRGN; flags: UINT): Boolean;
begin
  Result := Windows.RedrawWindow(Wnd,lprcUpdate,hrgnUpdate,flags);
end;

{------------------------------------------------------------------------------
  Function: RemoveProp
  Params: Handle: Handle of the object
          Str: Name of the property to remove
  Returns: The handle of the property (0=failure)

 ------------------------------------------------------------------------------}
function TWin32WidgetSet.RemoveProp(Handle: hwnd; Str: PChar): THandle;
begin
  Result := Windows.RemoveProp(Handle, Str);
end;

{------------------------------------------------------------------------------
  Method:  ReleaseCapture
  Params:  none
  Returns: True if succesful

  The ReleaseCapture function releases the mouse capture from a window
  and restores normal mouse input processing.
 ------------------------------------------------------------------------------}
function TWin32WidgetSet.ReleaseCapture: Boolean;
begin
  Result := Boolean(Windows.ReleaseCapture);
end;

{------------------------------------------------------------------------------
  Method:  ReleaseDC
  Params:  HWnd - handle of window
           DC   - handle of device context
  Returns: 1 if the device context was released or 0 if it wasn't

  Releases a device context (DC), freeing it for use by other applications.
 ------------------------------------------------------------------------------}
function TWin32WidgetSet.ReleaseDC(Window: HWND; DC: HDC): Integer;
begin
  Result := Windows.ReleaseDC(Window, DC);
end;

{------------------------------------------------------------------------------
  Method:  RestoreDC
  Params:  DC - handle of device context
           SavedDC - state to be restored
  Returns: if the function succeeds

  Restores a device context (DC) to the specified state.
-------------------------------------------------------------------------------}
function TWin32WidgetSet.RestoreDC(DC: HDC; SavedDC: Integer): Boolean;
begin
  Result := Boolean(Windows.RestoreDC(DC, SavedDC));
end;

{------------------------------------------------------------------------------
  Method:   RoundRect
  Params:   DC, X1, Y1, X2, Y2, RX, RY
  Returns:  true if succesfull, false otherwise

  Draws a Rectangle with optional rounded corners. RY is the radial height
  of the corner arcs, RX is the radial width.
 ------------------------------------------------------------------------------}
function TWin32WidgetSet.RoundRect(DC: HDC; X1, Y1, X2, Y2: Integer; RX, RY : Integer): Boolean;
begin
  Result := Windows.RoundRect(DC, X1, Y1, X2, Y2, RX, RY);
end;

{------------------------------------------------------------------------------
  Method:  SaveDC
  Params:  DC - a DC to save
  Returns: 0 if the functions fails otherwise a positive integer identifing
           the saved DC

  The SaveDC function saves the current state of the specified device
  context (DC) by copying its elements to a context stack.
-------------------------------------------------------------------------------}
function TWin32WidgetSet.SaveDC(DC: HDC): Integer;
begin
  Result := Windows.SaveDC(DC);
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
function TWin32WidgetSet.ScrollWindowEx(HWnd: HWND; DX, DY: Integer; PRcScroll, PRcClip: PRect; HRgnUpdate: HRGN; PRcUpdate: PRect; Flags: UINT): Boolean;
begin
  Result := Windows.ScrollWindowEx(HWnd, DX, DY, Windows.RECT(PRcScroll^), Windows.RECT(PRcClip^), HRgnUpdate, LPRECT(PRcUpdate), Flags) <> ERROR;
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
function TWin32WidgetSet.SelectClipRGN(DC : hDC; RGN : HRGN) : Longint;
begin
  Result := Windows.SelectClipRGN(DC, RGN);
end;

{------------------------------------------------------------------------------
  Method:  SelectObject
  Params:  DC - handle of device context
           GDIObj - handle of object
  Returns: he handle of the object being replaced

  Selects an object into the specified device context.
 ------------------------------------------------------------------------------}
function TWin32WidgetSet.SelectObject(DC: HDC; GDIObj: HGDIOBJ): HGDIOBJ;
begin
  Result := Windows.SelectObject(DC, GDIObj);
end;

{------------------------------------------------------------------------------
  Method:  SelectPalette
  Params:  DC - handle of device context
           Palette - handle of logical color palette
           ForceBackground - whether the logical palette is forced to be a
                             background palette
  Returns: the device context's previous logical palette

  Selects the specified logical palette into a device context.
 ------------------------------------------------------------------------------}
function TWin32WidgetSet.SelectPalette(DC: HDC; Palette: HPALETTE; ForceBackground: Boolean): HPALETTE;
begin
  Result := Windows.SelectPalette(DC, Palette, ForceBackground);
end;

{------------------------------------------------------------------------------
  Method:  SendMessage
  Params: HandleWnd   - handle of destination window
          Msg         - message to send
          WParam      - first message parameter
          LParam      - second message parameter
  Returns: the result of the message processing

  The SendMessage function sends the specified message to a window or windows.
  The function calls the window procedure for the specified window and does
  not return until the window procedure has processed the message.
 ------------------------------------------------------------------------------}
function TWin32WidgetSet.SendMessage(HandleWnd: HWND; Msg: Cardinal; WParam: WParam; LParam: LParam): LResult;
begin
  Result := Windows.SendMessage(HandleWnd, Msg, WParam, LParam);
end;

{------------------------------------------------------------------------------
  Method:  SetActiveWindow
  Params:  Window - Window to focus
  Returns: Old active window

  Sets focus to the specified window, if the current process is on top
 ------------------------------------------------------------------------------}
function TWin32WidgetSet.SetActiveWindow(Window: HWND): HWND;
begin
  Result := Windows.SetActiveWindow(Window);
end;

{------------------------------------------------------------------------------
  Method:  SetBkColor
  Params:  DC    - Device context to change the text background color
           Color - background color value
  Returns: Old Background color

  Sets the current background color to the specified color value.
 ------------------------------------------------------------------------------}
function TWin32WidgetSet.SetBKColor(DC: HDC; Color: TColorRef): TColorRef;
begin
  Result := TColorRef(Windows.SetBkColor(DC, ColorToRGB(TColor(Color))));
end;

{------------------------------------------------------------------------------
  Method:  SetBkMode
  Params: DC     - handle of device context
          BkMode - flag specifying background mode
  Returns: the previous background mode

  Sets the background mix mode of the specified device context.
 ------------------------------------------------------------------------------}
function TWin32WidgetSet.SetBkMode(DC: HDC; BkMode: Integer): Integer;
begin
  // Your code here
  Result := Windows.SetBkMode(DC, BkMode);
end;

function TWin32WidgetSet.SetComboMinDropDownSize(Handle: HWND; MinItemsWidth,
  MinItemsHeight, MinItemCount: integer): boolean;
begin
  Result:= ThemeServices.ThemesEnabled and
    boolean(Windows.SendMessage(Handle, CB_SETMINVISIBLE, MinItemCount, 0));
end;

{------------------------------------------------------------------------------
  Method:  SetCapture
  Params:  Value - Handle of window to capture
  Returns: the handle of the window that had previously captured the mouse

  Sets the mouse capture to the specified window.
 ------------------------------------------------------------------------------}
function TWin32WidgetSet.SetCapture(Value: HWND): HWND;
begin
  Result := Windows.SetCapture(Value);
end;

{------------------------------------------------------------------------------
  Method:  SetCaretPos
  Params:  new position x, y
  Returns: true on success

  Moves the caret to the specified coordinates.
 ------------------------------------------------------------------------------}
function TWin32WidgetSet.SetCaretPos(X, Y: Integer): Boolean;
begin
{$ifdef DEBUG_CARET}
  DebugLn('[SetCaretPos]');
{$endif}
  Result := Boolean(Windows.SetCaretPos(X, Y));
end;

{------------------------------------------------------------------------------
  Method:  SetCaretPosEx
  Params:  Handle - handle of window
           X      - horizontal mouse coordinate
           Y      - vertical mouse coordinate
  Returns: true on success

  Moves the caret to the specified coordinates in the specified window.
 ------------------------------------------------------------------------------}
function TWin32WidgetSet.SetCaretPosEx(Handle: HWND; X, Y: Integer): Boolean;
begin
{$ifdef DEBUG_CARET}
  DebugLn('[SetCaretPosEx] for window ', IntToHex(Handle, 8));
{$endif}
  Result := Windows.SetCaretPos(X, Y);
end;

function TWin32WidgetSet.SetCursor(hCursor: HICON): HCURSOR;
begin
  Result := Windows.SetCursor(hCursor);
end;

{------------------------------------------------------------------------------
  Function: SetCursorPos
  Params: X:
          Y:
  Returns:

 ------------------------------------------------------------------------------}
function TWin32WidgetSet.SetCursorPos(X, Y: Integer): Boolean;
begin
  Result := False;
  Windows.SetCursorPos(X, Y);
  Result := True;
end;*)

{------------------------------------------------------------------------------
  Method:  SetFocus
  Params:  HWnd - Handle of new focus window
  Returns: The old focus window

  The SetFocus function sets the keyboard focus to the specified window
 ------------------------------------------------------------------------------}

(*{------------------------------------------------------------------------------
  Method:  SetForegroundWindow
  Params:  HWnd - The handle of the window
  Returns: True if succesful

  The SetForegroundWindow function brings the specified window to top
    (highest z-index level).
 ------------------------------------------------------------------------------}
function TWin32WidgetSet.SetForegroundWindow(HWnd: HWND): boolean;
begin
  Result := Windows.SetForegroundWindow(HWnd);
end;

function TWin32WidgetSet.SetMenu(AWindowHandle: HWND; AMenuHandle: HMENU): Boolean;
begin
  Result := Windows.SetMenu(AWindowHandle, AMenuHandle);
  AddToChangedMenus(AWindowHandle);
end;

function TWin32WidgetSet.SetParent(hWndChild: HWND; hWndParent: HWND): HWND;
begin
  Result := Windows.SetParent(hWndchild, hWndParent);
end;


{------------------------------------------------------------------------------
  Method:  SetMapMode
  Params:  DC			- Device Context
           fnMapMode		- Mapping mode

  Returns: 0 if unsuccessful or the old Mode if successful
 ------------------------------------------------------------------------------}
function TWin32WidgetSet.SetMapMode(DC: HDC; fnMapMode : Integer): Integer;
begin
  Result := Windows.SetMapMode(DC, fnMapMode);
end;

{------------------------------------------------------------------------------
  Method:  SetProp
  Params:  Handle - handle of window
           Str    - string
           Data   - pointer to data
  Returns: Whether the string and data were successfully added to the property
           list.

  Adds a new entry or changes an existing entry in the property list of the
  specified window.

  NOTE: LCLLinux has no RemoveProp function but Windows API requires all set
  properties to be removed, so I'm keeping a list of windows with properties
  for a properties-enumeration function that's called when the program is quit.

  MWE: that is not really needed anymore since the RemoveProp is now implemented
 ------------------------------------------------------------------------------}
function TWin32WidgetSet.SetProp(Handle: HWND; Str: PChar; Data: Pointer): Boolean;
begin
  Result := Boolean(Windows.SetProp(Handle, Str, Windows.HANDLE(Data)));
end;

{------------------------------------------------------------------------------
  Method:  SetROP2
  Params:  DC			- Device Context
           Mode		- Foreground mixing mode

  Returns: 0 if unsuccessful or the old Mode if successful
 ------------------------------------------------------------------------------}
function TWin32WidgetSet.SetROP2(DC: HDC; Mode: Integer): Integer;
begin
  result := Windows.SetROP2(DC, Mode);
end;

{------------------------------------------------------------------------------
  Method:  SetScrollInfo
  Params:  Handle     - handle of window with scroll bar
           SBStyle    - scroll bar flag
           ScrollInfo - record with scroll parameters
           BRedraw    - is the scroll bar is redrawn?
  Returns: The new position value

  Sets the parameters of a scroll bar.
 ------------------------------------------------------------------------------}
function TWin32WidgetSet.SetScrollInfo(Handle: HWND; SBStyle: Integer; ScrollInfo: TScrollInfo; BRedraw: Boolean): Integer;
begin
  ScrollInfo.cbSize:=sizeof(ScrollInfo);
  if (ScrollInfo.fMask and SIF_Range > 0) then
    ScrollInfo.nMax := Max(ScrollInfo.nMin, ScrollInfo.nMax - 1);
  Result := Windows.SetScrollInfo(Handle, SBStyle, @ScrollInfo, BRedraw);
end;

{------------------------------------------------------------------------------
  Method:  SetStretchBltMode
  Params:  DC          - handle of device context
           StretchMode - strech mode
  Returns: 0 if unsuccesful

  The SetStretchBltMode function sets the StrechBlt mode.
 ------------------------------------------------------------------------------}
function TWin32WidgetSet.SetStretchBltMode(DC: HDC; StretchMode: Integer): Integer;
begin
  Result := Windows.SetStretchBltMode(DC, StretchMode);
end;

{------------------------------------------------------------------------------
  Method:  SetSysColors
  Params:  CElements    - the number of elements
           LPAElements  - array with element numbers
           LPARGBValues - array with colors
  Returns: 0 if unsuccesful

  The SetSysColors function sets the colors for one or more display elements.
 ------------------------------------------------------------------------------}
function TWin32WidgetSet.SetSysColors(CElements: Integer; Const LPAElements; Const LPARGBValues): Boolean;
begin
  Result := Boolean(Windows.SetSysColors(CElements, PInteger(@LPAElements)^, LPColorRef(@LPARGBValues)^));
end;

{------------------------------------------------------------------------------
  Method:  SetTextCharacterExtra
  Params: _HDC       - handle of device context
          NCharExtra - extra-space value
  Returns: the previous intercharacter spacing

  Sets the intercharacter spacing.
 ------------------------------------------------------------------------------}
function TWin32WidgetSet.SetTextCharacterExtra(_HDC: HDC; NCharExtra: Integer): Integer;
begin
  // Your code here
  Result := Windows.SetTextCharacterExtra(_HDC, NCharExtra);
end;

{------------------------------------------------------------------------------
  Method:  SetTextColor
  Params:  DC    - Identifies the device context.
           Color - Specifies the color of the text.
  Returns: The previous color if succesful, CLR_INVALID otherwise

  The SetTextColor function sets the text color for the specified device
  context to the specified color.
 ------------------------------------------------------------------------------}
function TWin32WidgetSet.SetTextColor(DC: HDC; Color: TColorRef): TColorRef;
begin
  Result := TColorRef(Windows.SetTextColor(DC, ColorToRGB(TColor(Color))));
end;

function TWin32WidgetSet.SetViewPortExtEx(DC: HDC; XExtent, YExtent : Integer; OldSize: PSize): Boolean;
begin
  Result := Boolean(Windows.SetViewPortExtEx(DC, XExtent, YExtent, LPSize(OldSize)));
end;

function TWin32WidgetSet.SetViewPortOrgEx(DC: HDC; NewX, NewY: Integer; OldPoint: PPoint): Boolean;
begin
  Result := Boolean(Windows.SetViewPortOrgEx(DC, NewX, NewY, LPPoint(OldPoint)));
end;

function TWin32WidgetSet.SetWindowExtEx(DC: HDC; XExtent, YExtent: Integer; OldSize: PSize): Boolean;
begin
  Result := Boolean(Windows.SetWindowExtEx(DC, XExtent, YExtent, LPSize(OldSize)));
end;

{------------------------------------------------------------------------------
  Procedure: GetWindowLong
  Params:  Handle  - handle of window
           Idx     - value to set
           NewLong - new value
  Returns: Nothing

  Changes an attribute of the specified window.
 ------------------------------------------------------------------------------}
function TWin32WidgetSet.SetWindowLong(Handle: HWND; Idx: Integer; NewLong: PtrInt): PtrInt;
begin
  if UnicodeEnabledOS then
    Result := Windows.SetWindowLongPtrW(Handle, Idx, NewLong)
  else
    Result := Windows.SetWindowLongPtr(Handle, Idx, NewLong);
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
function TWin32WidgetSet.SetWindowOrgEx(DC: HDC; NewX, NewY: Integer;
  OldPoint: PPoint): Boolean;
begin
  Result := Boolean(Windows.SetWindowOrgEx(DC, NewX, NewY, LPPoint(OldPoint)));
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
function TWin32WidgetSet.SetWindowPos(HWnd: HWND; HWndInsertAfter: HWND; X, Y, CX, CY: Integer; UFlags: UINT): Boolean;
var
  Style, ExStyle: PtrInt;
begin
  Style := GetWindowLong(HWnd, GWL_STYLE);
  ExStyle := GetWindowLong(HWnd, GWL_EXSTYLE);
  if (Style and WS_BORDER) <> 0 then
  begin
    // convert top level lcl window coordinaties to win32 coord
    // add twice, top+bottom border
    Inc(CX, 2*Windows.GetSystemMetrics(SM_CXSIZEFRAME));
    Inc(CY, 2*Windows.GetSystemMetrics(SM_CYSIZEFRAME));
  end;
  if (Style and WS_CAPTION) <> 0 then
    if (ExStyle and WS_EX_TOOLWINDOW) <> 0 then
      Inc(CY, Windows.GetSystemMetrics(SM_CYSMCAPTION))
    else
      Inc(CY, Windows.GetSystemMetrics(SM_CYCAPTION));
  Result := Boolean(Windows.SetWindowPos(HWnd, HWndInsertAfter, X, Y, CX, CY, UFlags));
end;

{------------------------------------------------------------------------------
  Method:  SetWindowRgn
  Params:  HWnd - handle of window with caret
  Returns: 0 if failed, another number otherwise

  Defines the part of the window which is visible and received input
 ------------------------------------------------------------------------------}
function TWin32WidgetSet.SetWindowRgn(hWnd: HWND; hRgn: HRGN; bRedraw: Boolean):longint;
begin
  Result := Windows.SetWindowRgn(hWnd, hRgn, bRedraw);
end;

{------------------------------------------------------------------------------
  Method:  ShowCaret
  Params:  HWnd - handle of window with caret
  Returns: if the function succeeds

  Makes the caret visible on the screen at the caret's current position.
 ------------------------------------------------------------------------------}
function TWin32WidgetSet.ShowCaret(HWnd: HWND): Boolean;
begin
//writeln('[TWin32WidgetSet.ShowCaret] A');
{$ifdef DEBUG_CARET}
  DebugLn('[ShowCaret] for window ', IntToHex(HWnd, 8));
{$endif}
  Result := Boolean(Windows.ShowCaret(HWnd));
end;

{------------------------------------------------------------------------------
  Method:   ShowScrollBar
  Params:  Handle - handle of window with scroll bar
           WBar   - scroll bar flag
           BShow  - is the scroll bar visible?
  Returns: If the function succeeds

  Shows or hides the specified scroll bar.
 ------------------------------------------------------------------------------}
function TWin32WidgetSet.ShowScrollBar(Handle: HWND; WBar: Integer; BShow: Boolean): Boolean;
begin
  Result := Boolean(Windows.ShowScrollBar(Handle, WBar, BShow));
  if BShow and Result and ThemeServices.ThemesAvailable then
  begin
    // sometimes on xp scrollbars does not invalidate themself and look as they are unthemed
    // force window frame (scrollbars are not in the client area) to redraw
    Windows.RedrawWindow(Handle, nil, 0, RDW_INVALIDATE or RDW_FRAME or RDW_NOCHILDREN);
  end;
end;

{------------------------------------------------------------------------------
  Method: ShowWindow
  Params: hWnd     - Window handle
          nCmdShow - (SW_SHOWNORMAL, SW_MINIMIZE, SW_SHOWMAXIMIZED)
  Returns: If the function succeeds
------------------------------------------------------------------------------}
function TWin32WidgetSet.ShowWindow(hWnd: HWND; nCmdShow: Integer): Boolean;
begin
  Result := Boolean(Windows.ShowWindow(hWnd, nCmdShow));
end;

{------------------------------------------------------------------------------
  Method:  StretchBlt
  Params:  DestDC              - The destination device context
           X, Y                - The left/top corner of the destination rectangle
           Width, Height       - The size of the destination rectangle
           SrcDC               - The source device context
           XSrc, YSrc          - The left/top corner of the source rectangle
           SrcWidth, SrcHeight - The size of the source rectangle
           Rop                 - The raster operation to be performed
  Returns: True if succesful

  The StretchBlt function copies a bitmap from a source rectangle into a
  destination rectangle using the specified raster operation. If needed it
  resizes the bitmap to fit the dimensions of the destination rectangle.
  Sizing is done according to the stretching mode currently set in the
  destination device context.
 ------------------------------------------------------------------------------}
function TWin32WidgetSet.StretchBlt(DestDC: HDC; X, Y, Width, Height: Integer; SrcDC: HDC; XSrc, YSrc, SrcWidth, SrcHeight: Integer; Rop: Cardinal): Boolean;
begin
  // use stretchmaskblt for alpha images, since that one is customized for alpha
  if IsAlphaDC(DestDC) or IsAlphaDC(SrcDC)
  then Result := StretchMaskBlt(DestDC, X, Y, Width, Height, SrcDC, XSrc, YSrc, SrcWidth, SrcHeight, 0, 0, 0, Rop)
  else Result := Windows.StretchBlt(DestDc, X, Y, Width, Height, SrcDC, XSrc, YSrc, SrcWidth, SrcHeight, Rop);
end;

{------------------------------------------------------------------------------
  Method:  StretchMaskBlt
  Params:  DestDC:                The destination devicecontext
           X, Y:                  The left/top corner of the destination rectangle
           Width, Height:         The size of the destination rectangle
           SrcDC:                 The source devicecontext
           XSrc, YSrc:            The left/top corner of the source rectangle
           SrcWidth, SrcHeight:   The size of the source rectangle
           Mask:                  The handle of a monochrome bitmap
           XMask, YMask:          The left/top corner of the mask rectangle
           Rop:                   The raster operation to be performed
  Returns: True if succesful

  The StretchMaskBlt function copies a bitmap from a source rectangle into a
  destination rectangle using the specified mask and raster operations. If
  needed it resizes the bitmap to fit the dimensions of the destination
  rectangle.  Sizing is done according to the stretching mode currently set in
  the destination device context.
 ------------------------------------------------------------------------------}
function TWin32WidgetSet.StretchMaskBlt(DestDC: HDC; X, Y, Width, Height: Integer; SrcDC: HDC; XSrc, YSrc, SrcWidth, SrcHeight: Integer; Mask: HBITMAP; XMask, YMask: Integer; Rop: DWORD): Boolean;

  function CreatePremultipliedBitmap(AWinBmp: Windows.TBitmap; ABitmap: HBITMAP; out AAlphaBmp: HBITMAP): Boolean;
  var
    Data: Pointer;
    Pixel: PRGBAQuad;
    ByteCount: PtrUInt;
    Header: Windows.TBitmapInfoHeader;
    HasAlpha0, HasAlphaN, HasAlpha255: Boolean;
  begin
    // todo, process only requested rectangle
    Result := False;
    if not GetBitmapBytes(AWinBmp, ABitmap, Rect(0, 0, AWinBmp.bmWidth, AWinBmp.bmHeight), rileDWordBoundary, riloTopToBottom, Data, ByteCount)
    then Exit;

    HasAlpha0 := False;
    HasAlphaN := False;
    HasAlpha255 := False;
    Pixel := Data;
    ByteCount := ByteCount shr 2;
    while ByteCount > 0 do
    begin
      case Pixel^.Alpha of
        0: begin
          Pixel^.Red   := 0;
          Pixel^.Green := 0;
          Pixel^.Blue  := 0;
          HasAlpha0 := True;
        end;
        255: begin
          HasAlpha255 := True;
        end;
      else
        Pixel^.Red   := (Pixel^.Red   * Pixel^.Alpha) div 255;
        Pixel^.Green := (Pixel^.Green * Pixel^.Alpha) div 255;
        Pixel^.Blue  := (Pixel^.Blue  * Pixel^.Alpha) div 255;
        HasAlphaN := True;
      end;
      Inc(Pixel);
      Dec(ByteCount);
    end;

    // only create bitmap when not opaque or not fully transparent
    // (all zero alpha is unlikly for alpha bitmap, so it is probably a bitmap without alpha channel)
    Result := HasAlphaN or (HasAlpha0 and HasAlpha255);
    if Result
    then begin
      FillChar(Header, SizeOf(Header), 0);
      Header.biSize := SizeOf(Header);
      Header.biWidth := AWinBmp.bmWidth;
      Header.biHeight := -AWinBmp.bmHeight;
      Header.biPlanes := 1;
      Header.biBitCount := 32;
      Header.biCompression := BI_RGB;

      AAlphaBmp := Windows.CreateDIBitmap(SrcDC, Header, CBM_INIT, Data, Windows.TBitmapInfo((@Header)^), DIB_RGB_COLORS);
    end;
    Freemem(Data);
  end;
var
  MaskDC, CopyDC, AlphaDC: HDC;
  MaskObj, CopyObj, AlphaObj: HGDIOBJ;
  PrevTextColor, PrevBkColor: COLORREF;
  WinBmp: Windows.TBitmap;
  Bmp, CopyBmp, AlphaBmp: HBITMAP;
  HasAlpha: Boolean;
  Blend: TBlendFunction;
begin
  //DbgDumpBitmap(Mask, 'StretchMaskBlt - Mask');

  // check if the Src has an alpha channel
  Bmp := Windows.GetCurrentObject(SrcDC, OBJ_BITMAP);
  // get info
  HasAlpha := (Windows.GetObject(bmp, SizeOf(WinBmp), @WinBmp) <> 0)
          and (WinBmp.bmBitsPixel = 32)
          and CreatePremultipliedBitmap(WinBmp, Bmp, AlphaBmp);

  if HasAlpha
  then begin
    AlphaDC := Windows.CreateCompatibleDC(SrcDC);
    AlphaObj := Windows.SelectObject(AlphaDC, AlphaBmp);

    // init blendfunction
    Blend.BlendOp := AC_SRC_OVER;
    Blend.BlendFlags := 0;
    Blend.SourceConstantAlpha := 255;
    Blend.AlphaFormat := AC_SRC_ALPHA;
  end;

  Windows.SetStretchBltMode(DestDC, STRETCH_HALFTONE);
  Windows.SetBrushOrgEx(DestDC, 0, 0, nil);
  if Mask = 0 then
  begin
    if HasAlpha
    then begin
      Win32Extra.AlphaBlend(DestDC, X, Y, Width, Height, AlphaDC, XSrc, YSrc, SrcWidth, SrcHeight, Blend);
    end
    else begin
      if (Width = SrcWidth) and (Height = SrcHeight) then
      begin
        Result := Windows.BitBlt(DestDC, X, Y, Width, Height, SrcDC, XSrc, YSrc, Rop);
      end
      else begin
        Result := Windows.StretchBlt(DestDC, X, Y, Width, Height, SrcDC, XSrc, YSrc, SrcWidth, SrcHeight, Rop);
      end;
    end;
  end
  else begin
    MaskDC := Windows.CreateCompatibleDC(DestDC);
    MaskObj := Windows.SelectObject(MaskDC, Mask);

    PrevTextColor := Windows.SetTextColor(DestDC, $00000000);
    PrevBkColor := Windows.SetBkColor(DestDC, $00FFFFFF);

    if HasAlpha
    then begin
      // create copy of masked destination
      CopyDC := Windows.CreateCompatibleDC(DestDC);
      CopyBmp := Windows.CreateCompatibleBitmap(DestDC, Width, Height);
      CopyObj := Windows.SelectObject(CopyDC, CopyBmp);
      Windows.BitBlt(CopyDC, 0, 0, Width, Height, DestDC, X, Y, SRCCOPY);
      // wipe non masked area -> white
      Windows.SetTextColor(CopyDC, $00FFFFFF);
      Windows.SetBkColor(CopyDC, $00000000);
      if (Width = SrcWidth) and (Height = SrcHeight)
      then Windows.BitBlt(CopyDC, 0, 0, Width, Height, MaskDC, XSrc, YSrc, SRCPAINT)
      else Windows.StretchBlt(CopyDC, 0, 0, Width, Height, MaskDC, XSrc, YSrc, SrcWidth, SrcHeight, SRCPAINT);

      // copy source
      Win32Extra.AlphaBlend(DestDC, X, Y, Width, Height, AlphaDC, XSrc, YSrc, SrcWidth, SrcHeight, Blend);
      // wipe masked area -> white
      if (Width = SrcWidth) and (Height = SrcHeight)
      then Windows.BitBlt(DestDC, X, Y, Width, Height, MaskDC, XSrc, YSrc, SRCPAINT)
      else Windows.StretchBlt(DestDC, X, Y, Width, Height, MaskDC, XSrc, YSrc, SrcWidth, SrcHeight, SRCPAINT);

      // paint copied destination
      Windows.BitBlt(DestDC, X, Y, Width, Height, CopyDC, 0, 0, SRCAND);

      // Restore stuff
      Windows.SelectObject(CopyDC, CopyObj);
      Windows.DeleteObject(CopyBmp);
      Windows.DeleteDC(CopyDC);
    end
    else begin
      if (Width = SrcWidth) and (Height = SrcHeight) then
      begin
        Windows.BitBlt(DestDC, X, Y, Width, Height, SrcDC,  XSrc, YSrc, SRCINVERT);
        Windows.BitBlt(DestDC, X, Y, Width, Height, MaskDC, XSrc, YSrc, SRCAND);
        Windows.BitBlt(DestDC, X, Y, Width, Height, SrcDC,  XSrc, YSrc, SRCINVERT);
      end
      else begin
        Windows.StretchBlt(DestDC, X, Y, Width, Height, SrcDC,  XSrc, YSrc, SrcWidth, SrcHeight, SRCINVERT);
        Windows.StretchBlt(DestDC, X, Y, Width, Height, MaskDC, XSrc, YSrc, SrcWidth, SrcHeight, SRCAND);
        Windows.StretchBlt(DestDC, X, Y, Width, Height, SrcDC,  XSrc, YSrc, SrcWidth, SrcHeight, SRCINVERT);
      end;
    end;
    Windows.SetTextColor(DestDC, PrevTextColor);
    Windows.SetBkColor(DestDC, PrevBkColor);
    Windows.SelectObject(MaskDC, MaskObj);
    Windows.DeleteDC(MaskDC);
  end;
  
  if HasAlpha
  then begin
    Windows.SelectObject(AlphaDC, AlphaObj);
    Windows.DeleteObject(AlphaBmp);
    Windows.DeleteDC(AlphaDC);
  end;

  Result := true;
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
function TWin32WidgetSet.SystemParametersInfo(uiAction: DWord; uiParam: DWord; pvParam: Pointer; fWinIni: DWord): LongBool;
begin
  Result := Windows.SystemParametersInfo(uiAction, uiParam, pvParam, fWinIni);
end;

{------------------------------------------------------------------------------
  Method:  TextOut
  Params: DC    - handle of device context
          X     - x-coordinate of starting position
          Y     - y-coordinate of starting position
          Str   - string
          Count - number of characters in string
  Returns: If the function succeeds

  Writes a character string at the specified location, using the currently
  selected font.
 ------------------------------------------------------------------------------}
function TCDWidgetSet.TextOut(DC: HDC; X, Y: Integer; Str: PChar; Count: Integer): Boolean;
var
  ws: widestring;
begin
  ws := UTF8ToUTF16(copy(str,1,Count));
  Result := Boolean(Windows.TextOutW(DC, X, Y, PWideChar(ws), length(ws)));
end;

function TWin32WidgetSet.UpdateWindow(Handle: HWND): Boolean;
begin
  Result:=Windows.UpdateWindow(Handle);
end;

{------------------------------------------------------------------------------
  Method:  WindowFromPoint
  Params: Point: Specifies the x and y Coords
  Returns: The handle of the window.

  Retrieves the handle of the window that contains the specified point.
 ------------------------------------------------------------------------------}
function TWin32WidgetSet.WindowFromPoint(Point: TPoint): HWND;
var
  ProcessID: DWORD;
begin
  Result := Windows.WindowFromPoint(Windows.POINT(Point));
  // check if window created by this process
  Windows.GetWindowThreadProcessId(Result, @ProcessID);
  if ProcessID <> Windows.GetCurrentProcessID then
    Result := 0;
end;


{We interprete CritSection as a pointer to a LPCRITICAL_SECTION structure}
procedure TWin32WidgetSet.InitializeCriticalSection(var CritSection: TCriticalSection);
var
  Crit : LPCRITICAL_SECTION;
begin
  { An OS Compatible TCriticalSection needs to be defined}
  If CritSection <> 0 then
    DeleteCriticalSection(CritSection);
  New(Crit);
  Windows.InitializeCriticalSection(Crit);
  CritSection := TCriticalSection(Crit);
end;

procedure TWin32WidgetSet.EnterCriticalSection(var CritSection: TCriticalSection);
begin
  { An OS Compatible TCriticalSection needs to be defined}
  Windows.EnterCriticalSection(LPCRITICAL_SECTION(CritSection));
end;

procedure TWin32WidgetSet.LeaveCriticalSection(var CritSection: TCriticalSection);
begin
  { An OS Compatible TCriticalSection needs to be defined}
  Windows.LeaveCriticalSection(LPCRITICAL_SECTION(CritSection));
end;

procedure TWin32WidgetSet.DeleteCriticalSection(var CritSection: TCriticalSection);
begin
  { An OS Compatible TCriticalSection needs to be defined}
  if CritSection<>0 then
  begin
    Windows.DeleteCriticalSection(LPCRITICAL_SECTION(CritSection));
    Try
      Dispose(LPCRITICAL_SECTION(CritSection));
    finally
      CritSection := 0;
    end;
  end;
end;*)

//##apiwiz##eps##   // Do not remove



