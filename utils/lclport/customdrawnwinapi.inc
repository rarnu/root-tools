{%MainUnit customdrawnint.pp}
{******************************************************************************
  All CustomDrawn backend independent Winapi implementations.

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

//##apiwiz##sps##   // Do not remove, no wizard declaration before this line

{------------------------------------------------------------------------------
  Function: Arc
  Params: DC: HDC; Left,Top,Right,Bottom,angle1,angle2 : Integer
  Returns: Boolean
 ------------------------------------------------------------------------------}
function TCDWidgetSet.Arc(DC: HDC; Left,Top,Right,Bottom,angle1,angle2 : Integer): Boolean;
var
  LazDC: TLazCanvas absolute DC;
begin
  {$ifdef VerboseCDWinAPI}
  DebugLn(Format(':>[WinAPI Polygon] DC=%s', [dbghex(DC)]));
  {$endif}

  if not IsValidDC(DC) then Exit(False);

  //LazDC.Arc(...);
  Result := True;
end;

(*{------------------------------------------------------------------------------
  Function: AngleChord
  Params: DC: HDC; x1, y1, x2, y2, angle1, angle2: Integer
  Returns: Boolean
 ------------------------------------------------------------------------------}
function TQtWidgetSet.AngleChord(DC: HDC; x1, y1, x2, y2, angle1, angle2: Integer): Boolean;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('[WinAPI AngleChord] DC: ', dbghex(DC));
  {$endif}
  Result := IsValidDC(DC);
  if Result then
    QPainter_drawChord(TQtDeviceContext(DC).Widget, x1, y1, x2, y2, Angle1, Angle2);
end;*)

{------------------------------------------------------------------------------
  Function: BeginPaint
  Params:
  Returns:

  This function is Called:
  - Once on every OnPaint event
 ------------------------------------------------------------------------------}
function TCDWidgetSet.BeginPaint(Handle: hWnd; Var PS : TPaintStruct): hdc;
begin
  {$ifdef VerboseCDDrawing}
    DebugLn('Trace:> [WinAPI BeginPaint] Handle=', dbghex(Handle));
  {$endif}
  Result := 0;

  if Handle = 0 then Exit;

  (*  Widget := TQtWidget(Handle);
  if Widget <> nil then
    DC := TQtDeviceContext.Create(Widget.PaintData.PaintWidget, True)
  else
    DC := TQtDeviceContext.Create(nil, True);

  PS.hdc := HDC(DC);

  if Handle<>0 then
  begin
    // if current handle has paintdata information,
    // setup hdc with it
    //DC.DebugClipRect('BeginPaint: Before');
    if Widget.PaintData.ClipRegion <> nil then
    begin
      //Write('>>> Setting Paint ClipRegion: ');
      //DebugRegion('PaintData.ClipRegion: ', Widget.PaintData.ClipRegion);
      DC.setClipRegion(Widget.PaintData.ClipRegion);
      DC.setClipping(True);
    end;
    if Widget.PaintData.ClipRect <> nil then
    begin
      New(DC.vClipRect);
      DC.vClipRect^ := Widget.PaintData.ClipRect^;
    end;
  end;

  Result := PS.hdc;

  {$ifdef VerboseQtWinAPI}
    WriteLn('Trace:< [WinAPI BeginPaint] Result=', dbghex(Result));
  {$endif}*)
end;

function TCDWidgetSet.BitBlt(DestDC: HDC; X, Y, Width, Height: Integer; SrcDC: HDC; XSrc, YSrc: Integer; Rop: DWORD): Boolean;
begin
  {$ifdef VerboseCDDrawing}
    WriteLn('Trace:> [TCDWidgetSet.BitBlt]');
  {$endif}

  Result := StretchBlt(DestDC, X, Y, Width, Height, SrcDC, XSrc, YSrc, Width,
                       Height, ROP);

  {$ifdef VerboseCDDrawing}
    WriteLn('Trace:< [TCDWidgetSet.BitBlt]');
  {$endif}
end;

(*function TQtWidgetSet.CallNextHookEx(hHk: HHOOK; ncode : Integer; wParam: WParam; lParam : LParam) : Integer;
begin
  {$ifdef VerboseQtWinAPI_MISSING_IMPLEMENTATION}
  	WriteLn('***** [WinAPI TQtWidgetSet.CallNextHookEx] missing implementation ');
  {$endif}
  Result := 0;
end;

function TQtWidgetSet.CallWindowProc(lpPrevWndFunc : TFarProc; Handle : HWND; Msg : UINT; wParam: WParam; lParam : lParam) : Integer;
begin
  {$ifdef VerboseQtWinAPI_MISSING_IMPLEMENTATION}
  	WriteLn('***** [WinAPI TQtWidgetSet.CallWindowProc] missing implementation ');
  {$endif}
  Result := -1;
end;

{------------------------------------------------------------------------------
  Method:  ClientToScreen
  Params:  Handle    -
  Returns:
 ------------------------------------------------------------------------------}
function TQtWidgetSet.ClientToScreen(Handle: HWND; var P: TPoint) : Boolean;
var
  APoint: TQtPoint;
  Pt: TPoint;
begin
  Result := IsValidHandle(Handle);
  if Result then
  begin
    APoint := QtPoint(P.X, P.Y);

    QWidget_mapToGlobal(TQtWidget(Handle).GetContainerWidget, @APoint, @APoint);
    if TQtWidget(Handle).ChildOfComplexWidget = ccwScrollingWinControl then
    begin
      Pt := TQtCustomControl(Handle).viewport.ScrolledOffset;
      dec(APoint.X, Pt.X);
      dec(APoint.Y, Pt.Y);
    end;
    P := Point(APoint.x, APoint.y);
  end;
end;*)

{------------------------------------------------------------------------------
  Method:  ClipboardFormatToMimeType
  Params:  FormatID - a registered format identifier (can't be a predefined format)
  Returns: the corresponding mime type as string
 ------------------------------------------------------------------------------}
function TCDWidgetSet.ClipboardFormatToMimeType(FormatID: TClipboardFormat): string;
begin
  {$ifdef VerboseCDClipboard}
  DebugLn(Format('[TCDWidgetSet.ClipboardFormatToMimeType] FormatID=%d', [FormatID]));
  {$endif}
  if FClipBoardFormats.Count > Integer(FormatID) then
    Result := FClipBoardFormats[FormatID]
  else
    Result := '';
end;

function TCDWidgetSet.ClipboardGetFormats(ClipboardType: TClipboardType;
  var Count: integer; var List: PClipboardFormat): boolean;
var
  i: Integer;
  Str: string;
begin
  {$ifdef VerboseCDClipboard}
  DebugLn('[TCDWidgetSet.GenericClipboardGetFormats]');
  {$endif}
  Result := False;
  Count := 0;
  List := nil;

  Count := FClipBoardFormats.Count;
  GetMem(List, Count * SizeOf(TClipboardFormat));

  for i := 0 to Count - 1 do
  begin
    Str := FClipBoardFormats.Strings[i];
    List[i] := ClipboardRegisterFormat(Str);
  end;

  Result := True;
end;

{------------------------------------------------------------------------------
  Method:  ClipboardRegisterFormat
  Params:  AMimeType - a string (usually a MIME type) identifying a new format
                       type to register
  Returns: the registered Format identifier (TClipboardFormat)
 ------------------------------------------------------------------------------}
function TCDWidgetSet.ClipboardRegisterFormat(const AMimeType: string): TClipboardFormat;
var
  Index: Integer;
begin
  Index := FClipBoardFormats.IndexOf(AMimeType);
  if Index < 0 then
    Index := FClipBoardFormats.Add(AMimeType);
  Result := Index;
  {$ifdef VerboseCDClipboard}
  DebugLn(Format('[TCDWidgetSet.ClipboardRegisterFormat] AMimeType=%s Result=%d', [AMimeType, Index]));
  {$endif}
end;

{------------------------------------------------------------------------------
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
function TCDWidgetSet.CombineRgn(Dest, Src1, Src2: HRGN; fnCombineMode: Longint): Longint;
var
  DestRgn: TLazRegion absolute Dest;
  Src1Rgn: TLazRegion absolute Src1;
  Src2Rgn: TLazRegion absolute Src2;
begin
  Result := ERROR;

  if not IsValidGDIObject(Dest) or not IsValidGDIObject(Src1) then Exit;

  if (fnCombineMode<>RGN_COPY) and not IsValidGDIObject(Src2) then Exit;

  // If the operation is a copy, execute it now, as it will not involve Src2
  // The common code would not work in this case
  if fnCombineMode = RGN_COPY then
  begin
    if Dest <> Src1 then DestRgn.Assign(Src1Rgn);
    Result := DestRgn.GetRegionKind();
    Exit;
  end;

  // Now operations which involve Src2, consider both cases: Dest=Src1 and Dest<>Src1
  if Dest = Src1 then
    DestRgn.CombineWith(Src2Rgn, fnCombineMode)
  else
  begin
    DestRgn.Assign(Src1Rgn);
    DestRgn.CombineWith(Src2Rgn, fnCombineMode);
  end;

  Result := DestRgn.GetRegionKind();
end;

{------------------------------------------------------------------------------
  Method:  CreateBitmap
  Params:
  Returns:

  This functions is for TBitmap support.
  Specifically it is utilized on when a handle for a bitmap is needed
 ------------------------------------------------------------------------------}
function TCDWidgetSet.CreateBitmap(Width, Height: Integer;
  Planes, BitCount: Longint; BitmapBits: Pointer): HBITMAP;
var
  lRawImage: TRawImage;
  lMask: HBitmap;
  NewBits: Pointer;
  NewBitsSize: PtrUInt;
  RSS: PtrUInt;
  ARowStride: PtrUInt;
begin
  {$ifdef VerboseCDBitmap}
    DebugLn('Trace:> [WinAPI CreateBitmap]',
     ' Width:', dbgs(Width),
     ' Height:', dbgs(Height),
     ' Planes:', dbgs(Planes),
     ' BitCount:', dbgs(BitCount),
     ' BitmapBits: ', dbgs(BitmapBits));
  {$endif}

  // for win32 data is aligned to WORD
  // for ARM speed optimization the best is realign data to DWORD

  Result := 0;
  NewBits := nil;
  lRawImage.Init;

  case BitCount of
    1: lRawImage.Description.Init_BPP1(Width, Height);
    15, 16: lRawImage.Description.Init_BPP16_R5G6B5(Width, Height);
    24: lRawImage.Description.Init_BPP24_B8G8R8_BIO_TTB(Width, Height);
    32: lRawImage.Description.Init_BPP32_A8R8G8B8_BIO_TTB(Width, Height);
  else
    lRawImage.Description.Init_BPP32_A8R8G8B8_BIO_TTB(Width, Height);
  end;

  RSS := GetBytesPerLine(Width, BitCount, rileWordBoundary);
  if BitmapBits <> nil then
  begin
    {$ifdef VerboseCDBitmap}
      DebugLn('Trace: [WinAPI CreateBitmap] BitmapBits <> nil');
    {$endif}
    ARowStride := GetBytesPerLine(Width, BitCount, rileDWordBoundary);
    if not CopyImageData(Width, Height, RSS, BitCount, BitmapBits, Types.Rect(0, 0, Width, Height),
      riloBottomToTop, riloBottomToTop, rileDWordBoundary, NewBits, NewBitsSize) then
    begin
      {$ifdef VerboseCDBitmap}
        DebugLn('Trace: [WinAPI CreateBitmap] CopyImageData failed');
      {$endif}
      // this was never tested
      ARowStride := RSS;
      NewBitsSize := RSS * Height;
      NewBits := AllocMem(NewBitsSize);
      System.Move(BitmapBits^, NewBits^, NewBitsSize);
    end;
    lRawImage.Data := NewBits;
    lRawImage.DataSize := NewBitsSize;
    //Result := HBitmap(TQtImage.Create(NewBits, Width, Height, ARowStride, Format, True));
    RawImage_CreateBitmaps(lRawImage, Result, lMask, True);
  end
  else
  begin
    {$ifdef VerboseCDBitmap}
      DebugLn('Trace: [WinAPI CreateBitmap] Creating Data');
    {$endif}
    lRawImage.CreateData(True);
    RawImage_CreateBitmaps(lRawImage, Result, lMask, True);
  end;

  {$ifdef VerboseCDBitmap}
    DebugLn('Trace:< [WinAPI CreateBitmap] Bitmap:', dbghex(Result));
  {$endif}
end;

{------------------------------------------------------------------------------
  Function:  CreateBrushIndirect
  Params:  none
  Returns: Nothing
 ------------------------------------------------------------------------------}
function TCDWidgetSet.CreateBrushIndirect(const LogBrush: TLogBrush): HBRUSH;
var
  lBrush: TFPCustomBrush;
begin
  lBrush := TFPCustomBrush.Create;
  Result := HBRUSH(lBrush);

  {$ifdef VerboseCDDrawing}
    DebugLn(Format(':>[TCDWidgetSet.CreateBrushIndirect]  Style: %d, Color: %8x Result:%x',
      [LogBrush.lbStyle, LogBrush.lbColor, Result]));
  {$endif}

  // brush color
  lBrush.FPColor := TColorToFPColor(LogBrush.lbColor);

  // brush style
  case LogBrush.lbStyle of
    BS_NULL:  lBrush.Style := bsClear; // Same as BS_HOLLOW.
    BS_SOLID: lBrush.Style := bsSolid;
{    BS_HATCHED: // Hatched brushes.
    begin
      case LogBrush.lbHatch of
        HS_BDIAGONAL: QtBrush.Style := QtBDiagPattern;
        HS_CROSS: QtBrush.Style := QtCrossPattern;
        HS_DIAGCROSS: QtBrush.Style := QtDiagCrossPattern;
        HS_FDIAGONAL: QtBrush.Style := QtFDiagPattern;
        HS_HORIZONTAL: QtBrush.Style := QtHorPattern;
        HS_VERTICAL: QtBrush.Style := QtVerPattern;
      else
        QtBrush.Style := QtSolidPattern;
      end;
    end;

    BS_DIBPATTERN,     // A pattern brush defined by a device-independent
           // bitmap (DIB) specification. If lbStyle is BS_DIBPATTERN, the
           // lbHatch member contains a handle to a packed DIB.Windows 95:
           // Creating brushes from bitmaps or DIBs larger than 8x8 pixels
           // is not supported. If a larger bitmap is given, only a portion
           // of the bitmap is used.
    BS_DIBPATTERN8X8,  // Same as BS_DIBPATTERN.
    BS_DIBPATTERNPT,   // A pattern brush defined by a device-independent
           // bitmap (DIB) specification. If lbStyle is BS_DIBPATTERNPT, the
           // lbHatch member contains a pointer to a packed DIB.
    BS_PATTERN,        // Pattern brush defined by a memory bitmap.
    BS_PATTERN8X8:     // Same as BS_PATTERN.
    begin
      QtBrush.setTextureImage(TQtImage(LogBrush.lbHatch).FHandle);
      QtBrush.Style := QtTexturePattern;
    end;  }
  else

  end;

  {$ifdef VerboseCDDrawing}
    DebugLn(':<[WinAPI CreateBrushIndirect] Result: ', dbghex(Result));
  {$endif}
end;

(*function TQtWidgetSet.CreateCaret(Handle: HWND; Bitmap: hBitmap; Width, Height: Integer): Boolean;
begin
  Result := (Handle <> 0) and
    QtCaret.CreateCaret(TQtWidget(Handle), Bitmap, Width, Height);
end;*)

{ In LCL-CustomDrawn it is completely irrelevant if a Bitmap is compatible with the screen,
  so just create any standard bitmap }
function TCDWidgetSet.CreateCompatibleBitmap(DC: HDC; Width, Height: Integer): HBITMAP;
begin
  Result := CreateBitmap(Width, Height, 1, 32, nil);
end;

{------------------------------------------------------------------------------
  Function: CreateCompatibleDC
  Params:  DC - handle to memory device context
  Returns: handle to a memory device context

  Creates a memory device context (DC) compatible with the specified device.

  This is utilized for example for creating a Canvas for a Bitmap, by later using
  SelectObject to select the bitmap
 ------------------------------------------------------------------------------}
function TCDWidgetSet.CreateCompatibleDC(DC: HDC): HDC;
begin
  {$ifdef VerboseCDDrawing}
    DebugLn('[WinAPI CreateCompatibleDC] DC: ', dbghex(DC));
  {$endif}
  Result := HDC(TLazCanvas.Create(nil));
end;

{------------------------------------------------------------------------------
  Function: CreateEllipticRgn
  Params:  p1 - X position of the top-left corner
           p2 - Y position of the top-left corner
           p3 - X position of the bottom-right corner
           p4 - Y position of the bottom-right corner
  Returns: HRGN
 ------------------------------------------------------------------------------}
function TCDWidgetSet.CreateEllipticRgn(p1, p2, p3, p4: Integer): HRGN;
var
  lRegion: TLazRegion;
begin
  {$ifdef VerboseCDRegions}
    DebugLn('[WinAPI CreateEllipticRgn] ');
  {$endif}
  lRegion := TLazRegion.Create;
  lRegion.AddEllipse(p1, p2, p3, p4);
  Result := HRGN(lRegion);
end;

{------------------------------------------------------------------------------
  Function: CreateFontIndirect
  Params:  const LogFont: TLogFont
  Returns: HFONT

  Creates a font GDIObject.
 ------------------------------------------------------------------------------}
function TCDWidgetSet.CreateFontIndirect(const LogFont: TLogFont): HFONT;
begin
  Result := CreateFontIndirectEx(LogFont, '');
end;

{------------------------------------------------------------------------------
  Function: CreateFontIndirectEx
  Params:  const LogFont: TLogFont
  Returns: HFONT

  Creates a font GDIObject.
 ------------------------------------------------------------------------------}
function TCDWidgetSet.CreateFontIndirectEx(const LogFont: TLogFont; const LongFontName: string): HFONT;
var
  lFont: TLazCDCustomFont;
//  FamilyName: string;
begin
  {$ifdef VerboseCDDrawing}
    DebugLn(Format('[TCDWidgetSet.CreateFontIndirectEx]  LongFontName=%s lfHeight=%d',
      [LongFontName, LogFont.lfHeight]));
  {$endif}

  lFont := TLazCDCustomFont.Create;
  Result := HFONT(lFont);

  lFont.Size := Abs(LogFont.lfHeight);

end;

function TCDWidgetSet.CreateIconIndirect(IconInfo: PIconInfo): HICON;
//var
//  AIcon: TCDIcon;
{  APixmap, ATemp: QPixmapH;
  AMask: QBitmapH;}
begin
  Result := 0;
end;

function TCDWidgetSet.CreatePenIndirect(const LogPen: TLogPen): HPEN;
var
  lPen: TFPCustomPen;
begin
  lPen := TFPCustomPen.Create;
  Result := HBRUSH(lPen);

  {$ifdef VerboseCDDrawing}
    DebugLn(Format(':>[TCDWidgetSet.CreatePenIndirect]  Style: %d, Color: %8x Result:"%x',
      [LogPen.lopnStyle, LogPen.lopnColor, Result]));
  {$endif}

  lPen.FPColor := TColorToFPColor(LogPen.lopnColor);

  case LogPen.lopnStyle and PS_STYLE_MASK of
    PS_SOLID:     lPen.Style := psSolid;
    PS_DASH:      lPen.Style := psDash;
    PS_DOT:       lPen.Style := psDot;
    PS_DASHDOT:   lPen.Style := psDashDot;
    PS_DASHDOTDOT:lPen.Style := psDashDotDot;
    PS_NULL:      lPen.Style := psClear;
  else
    lPen.Style := psSolid;
  end;

  lPen.Width := Max(1, LogPen.lopnWidth.X);
end;

{------------------------------------------------------------------------------
  Function: CreatePolygonRgn
  Params:  none
  Returns: HRGN
 ------------------------------------------------------------------------------}
function TCDWidgetSet.CreatePolygonRgn(Points: PPoint; NumPts: Integer; FillMode: integer): HRGN;
var
  lLazRegion: TLazRegion;
  lPoints: array of TPoint;
  i: Integer;
  lFillMode: TLazRegionFillMode;
begin
  lLazRegion := TLazRegion.Create;
  SetLength(lPoints, NumPts);
  for i := 0 to NumPts-1 do
    lPoints[i] := Points[i];

  {fillmode can be ALTERNATE or WINDING as msdn says}
  if FillMode = ALTERNATE then lFillMode := rfmOddEven
  else lFillMode := rfmWinding;

  lLazRegion.AddPolygon(lPoints, lFillMode);
  Result := HRGN(lLazRegion);

  {$ifdef VerboseCDWinAPI}
    DebugLn('[WinAPI CreatePolygonRgn] Result: ', dbghex(Result));
  {$endif}
end;

{------------------------------------------------------------------------------
  Function: CreateRectRgn
  Params:  none
  Returns: HRGN
 ------------------------------------------------------------------------------}
function TCDWidgetSet.CreateRectRgn(X1,Y1,X2,Y2 : Integer): HRGN;
var
  lLazRegion: TLazRegion;
begin
  lLazRegion := TLazRegion.Create;
  lLazRegion.SetAsSimpleRectRegion(Types.Rect(X1, Y1, X2, Y2));
  Result := HRGN(lLazRegion);
  {$ifdef VerboseCDWinAPI}
    DebugLn('Trace: [WinAPI CreateRectRgn] Result: ', dbghex(Result));
  {$endif}
end;

{------------------------------------------------------------------------------
  Procedure: DeleteCriticalSection
  Params:  var CritSection: TCriticalSection
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TCDWidgetSet.DeleteCriticalSection(var CritSection: TCriticalSection);
var
  ACritSec: System.PRTLCriticalSection;
begin
  ACritSec:=System.PRTLCriticalSection(CritSection);
  System.DoneCriticalsection(ACritSec^);
  Dispose(ACritSec);
  CritSection:=0;
end;

{------------------------------------------------------------------------------
  Function: DeleteDC
  Params:  none
  Returns: Nothing
 ------------------------------------------------------------------------------}
function TCDWidgetSet.DeleteDC(hDC: HDC): Boolean;
begin
  {$ifdef VerboseCDWinAPI}
    DebugLn('[WinAPI DeleteDC] Handle: ', dbghex(hDC));
  {$endif}

  Result := False;
  if not IsValidDC(hDC) then exit;
  Result := True;
  TLazCanvas(hDC).Free;
end;

{------------------------------------------------------------------------------
  Function: DeleteObject
  Params:  none
  Returns: Nothing

 ------------------------------------------------------------------------------}
function TCDWidgetSet.DeleteObject(GDIObject: HGDIOBJ): Boolean;
var
  aObject: TObject;
  {$if defined(VerboseCDDrawing) or defined(VerboseCDBitmap)}
    ObjType: string;
  {$endif}
begin
  {$if defined(VerboseCDDrawing) or defined(VerboseCDBitmap)}
    DebugLn('Trace:> [WinAPI DeleteObject] GDIObject: ', dbghex(GDIObject));
    ObjType := 'Unidentifyed';
  {$endif}

  Result := False;

  if GDIObject = 0 then Exit(True);

  if not IsValidGDIObject(GDIObject) then Exit;

  aObject := TObject(GDIObject);

(*  if (aObject is TQtResource) and TQtResource(aObject).FShared then
    Exit(True);*)

  {------------------------------------------------------------------------------
    Font
   ------------------------------------------------------------------------------}
  if aObject is TFPCustomFont then
  begin
    {$if defined(VerboseCDDrawing) or defined(VerboseCDBitmap)}
      ObjType := 'Font';
    {$endif}
  end
  {------------------------------------------------------------------------------
    Brush
   ------------------------------------------------------------------------------}
  else if aObject is TFPCustomBrush then
  begin
    {$if defined(VerboseCDDrawing) or defined(VerboseCDBitmap)}
      ObjType := 'Brush';
    {$endif}
  end
  {------------------------------------------------------------------------------
    Image
   ------------------------------------------------------------------------------}
  else if aObject is TCDBitmap then
  begin
    {$if defined(VerboseCDDrawing) or defined(VerboseCDBitmap)}
      ObjType := 'Image';
    {$endif}
  end
  {------------------------------------------------------------------------------
    Region
   ------------------------------------------------------------------------------}
  else if aObject is TLazRegion then
  begin
    {$if defined(VerboseCDDrawing) or defined(VerboseCDBitmap)}
      ObjType := 'Region';
    {$endif}
  end
  {------------------------------------------------------------------------------
    Pen
   ------------------------------------------------------------------------------}
  else if aObject is TFPCustomPen then
  begin
    {$if defined(VerboseCDDrawing) or defined(VerboseCDBitmap)}
      ObjType := 'Pen';
    {$endif}
  end;

(*  if AObject is TQtResource then
    if TQtResource(AObject).Owner <> nil then
    begin
      // this is an owned (default) resource, let owner free it
      DebugLn('WARNING: Trying to Free a default resource');
      AObject := nil;
    end;*)

  if AObject <> nil then
  begin
    //WriteLn('Delete object: ', PtrUInt(AObject));
    // FreeThenNil(AObject);
  end;

  Result := True;

  {$if defined(VerboseCDDrawing) or defined(VerboseCDBitmap)}
    DebugLn('Trace:< [WinAPI DeleteObject] Result=', dbgs(Result), ' ObjectType=', ObjType);
  {$endif}
end;

{------------------------------------------------------------------------------
  Method:   Ellipse
  Params:   X1, Y1, X2, Y2
  Returns:  Nothing

  Use Ellipse to draw a filled circle or ellipse.
 ------------------------------------------------------------------------------}
function TCDWidgetSet.Ellipse(DC: HDC; X1, Y1, X2, Y2: Integer): Boolean;
var
  LazDC: TLazCanvas absolute DC;
  R: TRect;
begin
  {$ifdef VerboseCDDrawing}
  DebugLn(Format(':>[WinAPI Ellipse] DC=%s', [dbghex(DC)]));
  {$endif}

  if not IsValidDC(DC) then
  begin
    {$ifdef VerboseCDDrawing}
    DebugLn(':<[WinAPI Rectangle] Invalid DC!');
    {$endif}
    Exit(False);
  end;

  LazDC.Ellipse(X1, Y1, X2, Y2);
  Result := True;
end;

procedure TCDWidgetSet.EnterCriticalSection(var CritSection: TCriticalSection);
var
  ACritSec: System.PRTLCriticalSection;
begin
  ACritSec:=System.PRTLCriticalSection(CritSection);
  System.EnterCriticalsection(ACritSec^);
end;

function TCDWidgetSet.ExtCreatePen(dwPenStyle, dwWidth: DWord;
  const lplb: TLogBrush; dwStyleCount: DWord; lpStyle: PDWord): HPEN;
var
  lPen: TFPCustomPen;
begin
  lPen := TFPCustomPen.Create;
  Result := HBRUSH(lPen);

  case dwPenStyle and PS_STYLE_MASK of
    PS_SOLID:     lPen.Style := psSolid;
    PS_DASH:      lPen.Style := psDash;
    PS_DOT:       lPen.Style := psDot;
    PS_DASHDOT:   lPen.Style := psDashDot;
    PS_DASHDOTDOT:lPen.Style := psDashDotDot;
    PS_NULL:      lPen.Style := psClear;
  else
    lPen.Style := psSolid;
  end;

  lPen.Width := 1;
  if (dwPenStyle and PS_TYPE_MASK) = PS_COSMETIC then
    lPen.Width := 1
  else if (dwPenStyle and PS_TYPE_MASK) = PS_GEOMETRIC then
  begin
    lPen.Width := dwWidth;

  end;

  lPen.FPColor := TColorToFPColor(ColorToRGB(lplb.lbColor));

  Result := HPEN(lPen);
end;

function TCDWidgetSet.ExtSelectClipRGN(dc: hdc; rgn: hrgn; Mode: Longint) : Integer;
var
  LazDC: TLazCanvas absolute DC;
  lRegion: TLazRegion absolute rgn;
begin
  {$ifdef VerboseCDWinAPI}
    DebugLn('[TCDWidgetSet.SelectClipRGN] DC=', dbgs(DC),' RGN=', dbghex(RGN));
  {$endif}

  Result := ERROR;

end;

{------------------------------------------------------------------------------
  Function: FillRect
  Params:  none
  Returns: Nothing
 ------------------------------------------------------------------------------}
function TCDWidgetSet.FillRect(DC: HDC; const Rect: TRect; Brush: HBRUSH): Boolean;
var
  LazDC: TLazCanvas absolute DC;
  lOldBrush: HGDIOBJ;
begin
  Result := False;

  {$ifdef VerboseCDDrawing}
    DebugLn('[WinAPI FillRect Rect=', dbgs(Rect),' Brush=', dbghex(Brush));
  {$endif}

  if not IsValidDC(DC) then
    exit;
  if not IsValidGdiObject(Brush) then
    exit;

  lOldBrush := SelectObject(DC, Brush);
  LazDC.FillRect(Rect);
  SelectObject(DC, lOldBrush);

  Result := True;
end;

function TCDWidgetSet.FillRgn(DC: HDC; RegionHnd: HRGN; hbr: HBRUSH): Bool;
var
  LazDC: TLazCanvas absolute DC;
  lRegion: TLazRegion absolute RegionHnd;
  lRegionRect: TRect;
  lOldBrush: HGDIOBJ;
  lOldRegion: TLazRegion;
begin
  {$ifdef VerboseCDWinAPI}
    DebugLn('[TCDWidgetSet.FillRgn] Rgn=', dbgs(RegionHnd),' Brush=', dbghex(hbr));
  {$endif}

  Result := False;

  if not IsValidDC(DC) then exit;
  if hbr = 0 then Exit;
  if RegionHnd = 0 then Exit;

  lOldBrush := SelectObject(DC, hbr);
  try
    lOldRegion := TLazRegion.Create;
    lOldRegion.Assign(TLazRegion(LazDC.ClipRegion));
    lRegionRect := lRegion.GetBoundingRect();
    LazDC.Rectangle(lRegionRect);
  finally
    TLazRegion(LazDC.ClipRegion).Assign(lOldRegion);
    lOldRegion.Free;
    SelectObject(DC, lOldBrush);
  end;

  Result := True;
end;

function TCDWidgetSet.FrameRect(DC: HDC; const ARect: TRect; hBr: HBRUSH): Integer;
var
  LazDC: TLazCanvas absolute DC;
  lOldBrush, lOldPen, lFramePen, lFrameBrush: HGDIOBJ;
  lLogPen: TLogPen;
  lLogBrush: TLogBrush;
begin
  Result := 0;

  {$ifdef VerboseCDDrawing}
    DebugLn('[WinAPI FillRect Rect=', dbgs(ARect),' Brush=', dbghex(hBr));
  {$endif}

  if not IsValidDC(DC) then
    exit;
  if not IsValidGdiObject(hBr) then
    exit;

  // Creates temporary pen and brush to help the drawing
  lLogPen.lopnStyle := PS_SOLID;
  lLogPen.lopnWidth := Types.Point(1, 1);
  lLogPen.lopnColor := FPColorToTColor(TFPCustomBrush(hBR).FPColor);
  lFramePen := CreatePenIndirect(lLogPen);

  lLogBrush.lbStyle := BS_NULL;
  lFrameBrush := CreateBrushIndirect(lLogBrush);

  // Do the drawing
  lOldBrush := SelectObject(DC, lFrameBrush);
  lOldPen := SelectObject(DC, lFramePen);
  LazDC.Rectangle(ARect);
  SelectObject(DC, lOldBrush);
  SelectObject(DC, lOldPen);

  // Delete the helper objects
  DeleteObject(lFramePen);
  DeleteObject(lFrameBrush);

  Result := 1;
end;


function TCDWidgetSet.GetClientRect(handle : HWND; var ARect : TRect) : Boolean;
begin
  {$ifdef VerboseCDWinAPI}
  DebugLn(Format('[WinAPI GetClientRect] Handle=%x', [Handle]));
  {$endif}
  GetClientBounds(Handle, ARect);
  OffsetRect(ARect, -ARect.Left, -ARect.Top);

  Result := True;
end;

function TCDWidgetSet.GetClipBox(DC: hDC; lpRect: PRect): Longint;
var
  LazDC: TLazCanvas;
  lClipRegion: TFPCustomRegion;
begin
  {$ifdef VerboseCDWinAPI}
    DebugLn('[WinAPI GetClipBox] DC ' + dbghex(DC));
  {$endif}

  Result := NULLREGION;
  if lpRect <> nil then
    lpRect^ := Types.Rect(0,0,0,0);

  if DC = 0 then DC := HDC(ScreenDC);

  if not IsValidDC(DC) then
    Result := ERROR;

  if Result = ERROR then Exit;

  LazDC := TLazCanvas(DC);

  if (lpRect<>nil) then
  begin
    lClipRegion := LazDC.ClipRegion;
    if lClipRegion = nil then
    begin
      Result := NULLREGION;
      lpRect^ := Types.Bounds(0, 0, LazDC.Width, LazDC.Height);
    end
    else
    begin
      Result := SIMPLEREGION;
      lpRect^ := lClipRegion.GetBoundingRect();
    end;
  end;
end;

function TCDWidgetSet.GetClipRGN(DC : hDC; RGN : hRGN): Longint;
var
  LazDC: TLazCanvas absolute DC;
  lDestRegion: TLazRegion absolute RGN;
  lDCRegion: TLazRegion;
begin
  {$ifdef VerboseCDWinAPI}
    DebugLn('[WinAPI GetClipRGN] DC ' + dbghex(DC));
  {$endif}

  Result := -1;
  if not IsValidDC(DC) then exit;
  if Rgn = 0 then Exit;

  lDCRegion := TLazRegion(LazDC.ClipRegion);
  if lDCRegion = nil then
    Result := 0
  else
  begin
    lDestRegion.Assign(lDCRegion);
    Result := 1;
  end;
end;

function TCDWidgetSet.GetCurrentObject(DC: HDC; uObjectType: UINT): HGDIOBJ;
var
  LazDC: TLazCanvas;
begin
  {$ifdef VerboseCDWinAPI}
    DebugLn(Format('[TCDWidgetSet.GetCurrentObject uObjectType=%d', [uObjectType]));
  {$endif}

  Result := 0;
  if not IsValidDC(DC) then exit;
  LazDC := TLazCanvas(DC);

  case uObjectType of
    OBJ_BITMAP: Result := HGDIOBJ(LazDC.SelectedBitmap);
    OBJ_BRUSH: Result := HGDIOBJ(LazDC.AssignedBrush);
    OBJ_FONT: Result := HGDIOBJ(LazDC.AssignedFont);
    OBJ_PEN: Result := HGDIOBJ(LazDC.AssignedPen);
  end;
end;

function TCDWidgetSet.GetRgnBox(RGN : HRGN; lpRect : PRect) : Longint;
var
  lLazRegion: TLazRegion absolute RGN;
begin
  if RGN = 0 then
  begin
    Result := ERROR;
    if lpRect <> nil then lpRect^ := Types.Rect(0,0,0,0);
    Exit();
  end;

  //Result := lLazRegion.IsSimpleRectRegion(); TQtRegion(RGN).GetRegionType;
  Result := SIMPLEREGION;
  if lpRect <> nil then lpRect^ := lLazRegion.GetBoundingRect();

  {$ifdef VerboseCDWinAPI}
  Debugln('Trace:> [WinAPI GetRgnBox] Handle: ' + dbghex(RGN));
  {$endif}
end;

function TCDWidgetSet.GetStockObject(Value: Integer): THandle;
begin
  {$ifdef VerboseCDWinAPI}
    DebugLn(Format('Trace:> [WinAPI GetStockObject] Value: %d', [Value]));
  {$endif}

  Result := 0;

  case Value of
    BLACK_BRUSH:         // Black brush.
      Result := THandle(FStockBlackBrush);
    DKGRAY_BRUSH:        // Dark gray brush.
      Result := THandle(FStockDKGrayBrush);
    GRAY_BRUSH:          // Gray brush.
      Result := THandle(FStockGrayBrush);
    LTGRAY_BRUSH:        // Light gray brush.
      Result := THandle(FStockLtGrayBrush);
    NULL_BRUSH:          // Null brush (equivalent to HOLLOW_BRUSH).
      Result := THandle(FStockNullBrush);
    WHITE_BRUSH:         // White brush.
      Result := THandle(FStockWhiteBrush);

    BLACK_PEN:           // Black pen.
      Result := THandle(FStockBlackPen);
    NULL_PEN:            // Null pen.
      Result := THandle(FStockNullPen);
    WHITE_PEN:           // White pen.
      Result := THandle(FStockWhitePen);

    {System font. By default, Windows uses the system font to draw menus,
     dialog box controls, and text. In Windows versions 3.0 and later,
     the system font is a proportionally spaced font; earlier versions of
     Windows used a monospace system font.}
    DEFAULT_GUI_FONT, SYSTEM_FONT:
      Result := THandle(FDefaultGUIFont);

  {$ifdef VerboseCDWinAPI}
  else
    DebugLn(Format('[WinAPI GetStockObject] UNHANDLED Value: %d', [Value]));
  {$endif}
  end;
end;

{------------------------------------------------------------------------------
  Function: TCDWidgetSet.GetSysColor
  Params:   index to the syscolors array
  Returns:  RGB value

 ------------------------------------------------------------------------------}

(*function TQtWidgetSet.GetSysColorBrush(nIndex: Integer): HBrush;

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
end;

{------------------------------------------------------------------------------
  Function: GetSystemMetrics
  Params:
  Returns: Nothing


 ------------------------------------------------------------------------------}
function TQtWidgetSet.GetSystemMetrics(nIndex: Integer): Integer;
var
  R: TRect;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn(Format('Trace:> [TQtWidgetSet.GetSystemMetrics] %d', [nIndex]));
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
      Result := QDesktopWidget_numScreens(QApplication_desktop());
    SM_CMOUSEBUTTONS:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CMOUSEBUTTONS    ');
      end;
    SM_CXBORDER, SM_CYBORDER:
      begin
        // size of frame around controls
        Result := QStyle_pixelMetric(QApplication_style(),
                    QStylePM_DefaultFrameWidth, nil, nil);
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
        Result := QStyle_pixelMetric(QApplication_style(), QStylePM_IndicatorWidth, nil, nil);
      end;
    SM_CYMENUSIZE:
      begin
        Result := QStyle_pixelMetric(QApplication_style(), QStylePM_IndicatorHeight, nil, nil);
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
        QDesktopWidget_screenGeometry(QApplication_desktop(), @R, QDesktopWidget_primaryScreen(QApplication_desktop()));
        Result := R.Right - R.Left;
      end;
    SM_CYSCREEN:
      begin
        QDesktopWidget_screenGeometry(QApplication_desktop(), @R, QDesktopWidget_primaryScreen(QApplication_desktop()));
        Result := R.Bottom - R.Top;
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
        Result := QStyle_pixelMetric(QApplication_style(), QStylePM_MDIFrameWidth, nil, nil);
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
        Result := QWidget_width(QApplication_desktop);
      end;
    SM_CYVIRTUALSCREEN:
      begin
        Result := QWidget_height(QApplication_desktop);
      end;
    SM_CXVSCROLL,
    SM_CYVSCROLL,
    SM_CXHSCROLL,
    SM_CYHSCROLL:
      begin
        Result := QStyle_pixelMetric(QApplication_Style, QStylePM_ScrollBarExtent, nil, nil);
      end;
    SM_CYCAPTION:
      begin
        Result := QStyle_pixelMetric(QApplication_Style, QStylePM_TitleBarHeight, nil, nil);
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
end;  *)

{------------------------------------------------------------------------------
  Function: GetTextColor
  Params:  DC     - A device context
  Returns: TColorRef

  Gets the Font Color currently assigned to the Device Context
 ------------------------------------------------------------------------------}
function TCDWidgetSet.GetTextColor(DC: HDC) : TColorRef;
var
  lFont: TFPCustomFont;
  LazDC: TLazCanvas;
begin
  {$ifdef VerboseCDDrawing}
    DebugLn(Format('[TCDWidgetSet.SetTextColor]  DC: %x', [DC]));
  {$endif}

  Result := 0;
  if not IsValidDC(DC) then Exit;
  LazDC := TLazCanvas(DC);

  if LazDC.Font <> nil then
    Result := FPColorToTColor(LazDC.Font.FPColor);
end;

{------------------------------------------------------------------------------
  Method:  GetWindowOrgEx
  Params:  DC    -
  Returns:
 ------------------------------------------------------------------------------}
function TCDWidgetSet.GetWindowOrgEx(dc: hdc; P: PPoint): Integer;
var
  LazDC: TLazCanvas absolute DC;
begin
  {$ifdef VerboseCDDrawing}
    DebugLn(Format(':>[WinAPI GetWindowOrgEx] DC=%s', [dbghex(DC)]));
  {$endif}
  Result := 0;
  if not IsValidDC(DC) then Exit;
  if P = nil then Exit;

  P^.X := LazDC.WindowOrg.X - LazDC.BaseWindowOrg.X;
  P^.Y := LazDC.WindowOrg.Y - LazDC.BaseWindowOrg.Y;
  Result := 1; // any non-zero will do according to MSDN
  {$ifdef VerboseCDDrawing}
    DebugLn(':<[WinAPI GetWindowOrgEx] Result='+dbgs(p^));
  {$endif}
end;


(*{------------------------------------------------------------------------------
  Method:  GetWindowRect
  Params:  Handle - handle of window
           Rect   - record for window coordinates
  Returns: if the function succeeds, the return value is nonzero; if the
           function fails, the return value is zero

  Retrieves the dimensions of the bounding rectangle of the specified window.
 ------------------------------------------------------------------------------}
function TCDWidgetSet.GetWindowRect(Handle: HWND; var ARect: TRect): Integer;
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
  Function: GetWindowSize
  Params:  Handle : hwnd;
  Returns: true on success

  Returns the current widget Width and Height
 ------------------------------------------------------------------------------}

(*{------------------------------------------------------------------------------
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
end;*)

{------------------------------------------------------------------------------
  Procedure: InitializeCriticalSection
  Params: var CritSection: TCriticalSection
  Returns:
 ------------------------------------------------------------------------------}
procedure TCDWidgetSet.InitializeCriticalSection(var CritSection: TCriticalSection);
var
  ACritSec: System.PRTLCriticalSection;
begin
  New(ACritSec);
  System.InitCriticalSection(ACritSec^);
  CritSection:=TCriticalSection(ACritSec);
end;

(*function TQtWidgetSet.IntersectClipRect(dc: hdc; Left, Top, Right, Bottom: Integer): Integer;
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
end;*)

(*function TCDWidgetSet.IsIconic(Handle: HWND): boolean;
begin
  Result := TCDForm(Handle).LCLForm.FormState = fsMinimized;
end;*)

(*function TQtWidgetSet.IsZoomed(Handle: HWND): boolean;
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

{------------------------------------------------------------------------------
  Function: InvalidateRgn
  Params: aHandle:
          Rect:
          bErase:
  Returns: True if invalidate is successfull.
  Invalidates region of widget.

  Felipe: Invalidating a non-rectangular region is unusual and complicated,
  so for now lets just get the bounding rect and invalidate that instead.
 ------------------------------------------------------------------------------}
function TCDWidgetSet.InvalidateRgn(aHandle: HWND; Rgn: HRGN; Erase: Boolean): Boolean;
var
  lLazRegion: TLazRegion absolute Rgn;
  localRect: TRect;
begin
  {$ifdef VerboseCDWinAPI}
    DebugLn('[WinAPI InvalidateRgn]');
  {$endif}
  if aHandle = 0 then Exit(False);
  if Rgn <> 0 then
  begin
    localRect := lLazRegion.GetBoundingRect();
    Result := InvalidateRect(aHandle, @localRect, Erase);
  end
  else
    Result := InvalidateRect(aHandle, nil, Erase);
end;

{------------------------------------------------------------------------------
  Procedure: LeaveCriticalSection
  Params:  var CritSection: TCriticalSection
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TCDWidgetSet.LeaveCriticalSection(var CritSection: TCriticalSection);
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
function TCDWidgetSet.LineTo(DC: HDC; X, Y: Integer): Boolean;
var
  PenPos, LastPos: TPoint;
  LazDC: TLazCanvas absolute DC;
begin
  {$ifdef VerboseCDDrawing}
    DebugLn(Format('[TCDWidgetSet.LineTo] DC=%x X=%d Y=%d', [DC, X, Y]));
  {$endif}

  Result := False;

  if not IsValidDC(DC) then
  begin

    Exit;
  end;

(*  TQtDeviceContext(DC).getPenPos(@PenPos);
  LastPos := Point(X, Y);
  if TQtDeviceContext(DC).pen.getCosmetic then
    LastPos := TQtDeviceContext(DC).GetLineLastPixelPos(PenPos, LastPos);
  TQtDeviceContext(DC).drawLine(PenPos.X, PenPos.Y, LastPos.X, LastPos.Y);
  MoveToEx(DC, X, Y, nil);*)

  LazDC.LineTo(X, Y);

  Result := True;
end;
(*
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

{------------------------------------------------------------------------------
  Function: MoveToEx
  Params:  none
  Returns: Nothing
 ------------------------------------------------------------------------------}
function TCDWidgetSet.MoveToEx(DC: HDC; X, Y: Integer; OldPoint: PPoint): Boolean;
var
  LazDC: TLazCanvas absolute DC;
begin
  {$ifdef VerboseCDDrawing}
    DebugLn('[WinAPI MoveToEx]',
     ' DC:', dbghex(DC),
     ' X:', dbgs(X),
     ' Y:', dbgs(Y));
  {$endif}

  Result := False;

  if not IsValidDC(DC) then Exit;

  if (OldPoint <> nil) then OldPoint^ := LazDC.PenPos;

  LazDC.PenPos := Types.Point(X, Y);

  Result := True;
end;

(*function TQtWidgetSet.OffsetRgn(RGN: HRGN; nXOffset, nYOffset: Integer): Integer;
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
end;*)

{------------------------------------------------------------------------------
  Function: PolyBezier
  Params:  DC: HDC; Points: PPoint; NumPts: Integer; Filled: Boolean;
           Continuous: Boolean
  Returns: Nothing
 ------------------------------------------------------------------------------}
function TCDWidgetSet.PolyBezier(DC: HDC; Points: PPoint; NumPts: Integer;
  Filled, Continuous: Boolean): Boolean;
begin
  {$ifdef VerboseCDDrawing}
    WriteLn('[WinAPI PolyBezier] DC: ', dbghex(DC));
  {$endif}
  Result := inherited PolyBezier(DC, Points, NumPts, Filled, Continuous);
end;

{------------------------------------------------------------------------------
  Function: Polygon
  Params:  DC: HDC; Points: PPoint; NumPts: Integer; Winding: Boolean
  Returns: Nothing
 ------------------------------------------------------------------------------}
function TCDWidgetSet.Polygon(DC: HDC; Points: PPoint; NumPts: Integer;
  Winding: Boolean): boolean;
var
  LazDC: TLazCanvas absolute DC;
  lPoints: array of TPoint;
  i: Integer;
begin
  {$ifdef VerboseCDDrawing}
  DebugLn(Format(':>[WinAPI Polygon] DC=%s', [dbghex(DC)]));
  {$endif}

  if not IsValidDC(DC) then Exit(False);

  SetLength(lPoints, NumPts);
  for i := 0 to NumPts-1 do
  begin
    {$ifdef VerboseCDDrawing}
    LCLProc.DbgOut(Format(' P=%d,%d', [Points[i].X, Points[i].Y]));
    {$endif}
    lPoints[i] := Points[i];
  end;

  LazDC.Polygon(lPoints);
  Result := True;

  {$ifdef VerboseCDDrawing}
  DebugLn('');
  {$endif}
end;

{------------------------------------------------------------------------------
  Function: Polyline
  Params:  DC: HDC; Points: PPoint; NumPts: Integer
  Returns: Nothing
 ------------------------------------------------------------------------------}
function TCDWidgetSet.Polyline(DC: HDC; Points: PPoint; NumPts: Integer): boolean;
var
  LazDC: TLazCanvas absolute DC;
  lPoints: array of TPoint;
  i: Integer;
begin
  {$ifdef VerboseCDDrawing}
  DebugLn(Format(':>[WinAPI Polygon] DC=%s', [dbghex(DC)]));
  {$endif}

  if not IsValidDC(DC) then Exit(False);

  SetLength(lPoints, NumPts);
  for i := 0 to NumPts-1 do
    lPoints[i] := Points[i];

  LazDC.Polyline(lPoints);
  Result := True;
end;

(*function TQtWidgetSet.PostMessage(Handle: HWND; Msg: Cardinal; wParam: WParam; lParam: LParam): Boolean;
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
end;*)

function TCDWidgetSet.PtInRegion(RGN: HRGN; X, Y: Integer): Boolean;
var
  lRegion: TLazRegion absolute RGN;
begin
  Result := False;

  if not IsValidGDIObject(RGN) then Exit;

  Result := lRegion.IsPointInRegion(X, Y);
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
  {$ifdef VerboseCDDrawing}
  DebugLn(Format(':>[WinAPI Rectangle] DC=%s', [dbghex(DC)]));
  {$endif}

  if not IsValidDC(DC) then
  begin
    {$ifdef VerboseCDDrawing}
    DebugLn(':<[WinAPI Rectangle] Invalid DC!');
    {$endif}
    Exit(False);
  end;

  // ToDo: We can normalize the rectangle, but this is not necessary as
  // TLazCanvas ignores invalid coordinates
{  R := NormalizeRect(Rect(X1, Y1, X2, Y2));
  if IsRectEmpty(R) then Exit(True);}

  LazDC.Rectangle(X1, Y1, X2, Y2);

  Result := True;
end;

function TCDWidgetSet.RectVisible(dc : hdc; const ARect: TRect) : Boolean;
var
  LazDC: TLazCanvas;
begin
  {$ifdef VerboseCDDrawing}
  Debugln('[WinAPI RectVisible]');
  {$endif}
  Result := True;
  if not IsValidDC(DC) then Exit;
  LazDC := TLazCanvas(DC);
  // as MSDN says only clipping region can play here
{  if QtDC.getClipping then
    Result := QtDC.getClipRegion.containsRect(ARect);}
end;

(*{------------------------------------------------------------------------------
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
end;*)

{------------------------------------------------------------------------------
  Function: RestoreDC: Restore a previously saved DC state
  Params:
    DC: Handle to a DeviceContext
    SavedDC: Index of saved state that needs to be restored
  Returns: True if state was successfuly restored.
-------------------------------------------------------------------------------}
function TCDWidgetSet.RestoreDC(DC: HDC; SavedDC: Integer): Boolean;
var
  LazDC: TLazCanvas absolute DC;
begin
  {$ifdef VerboseQTWinAPI}
  WriteLn('Trace:> [WinAPI RestoreDC] DC=', dbghex(DC),' SavedDC=',SavedDC);
  {$Endif}
  Result := False;
  if not IsValidDC(DC) then Exit;

  LazDC.RestoreState(SavedDC);
  Result := True;
  {$ifdef VerboseQTWinAPI}
  WriteLn('Trace:< [WinAPI RestoreDC]');
  {$Endif}
end;

(*function TQtWidgetSet.RoundRect(DC: hDC; X1, Y1, X2, Y2: Integer; RX,RY : Integer): Boolean;
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
end;*)

{------------------------------------------------------------------------------
  Function: SaveDC: save DC state information to a stack
  Params:  DC
  Returns: The index assigned to the or 0 if DC is not valid
-------------------------------------------------------------------------------}
function TCDWidgetSet.SaveDC(DC: HDC): Integer;
var
  LazDC: TLazCanvas absolute DC;
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

  Result := LazDC.SaveState();

  {$ifdef VerboseQTWinAPI}
  WriteLn('Trace:< [WinAPI SaveDC] result=', Result);
  {$Endif}
end;

(*{------------------------------------------------------------------------------
  Function: ScreenToClient
  Params:  Handle: HWND; var P: TPoint
  Returns:
-------------------------------------------------------------------------------}
function TQtWidgetSet.ScreenToClient(Handle : HWND; var P : TPoint) : Integer;
var
  APoint: TQtPoint;
begin
  Result := 0;
  if IsValidHandle(Handle) then
  begin
    APoint := QtPoint(P.X, P.Y);
    QWidget_mapFromGlobal(TQtWidget(Handle).GetContainerWidget, @APoint, @APoint);
    P := Point(APoint.x, APoint.y);
    Result := 1;
  end;
end;

{------------------------------------------------------------------------------
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
end;*)

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
function TCDWidgetSet.SelectClipRGN(DC: hDC; RGN: HRGN): Longint;
begin
  Result := ExtSelectClipRgn(DC, RGN, RGN_COPY);
end;

{------------------------------------------------------------------------------
  Function: SelectObject
  Params:  none
  Returns: The GDI object of the same type previously associated with the DC

  Changes one of the GDI objects (Font, Brush, etc) of a Device Context;
 ------------------------------------------------------------------------------}
function TCDWidgetSet.SelectObject(DC: HDC; GDIObj: HGDIOBJ): HGDIOBJ;
var
  aObject: TObject;
  lFont: TFPCustomFont absolute AObject;
  lPen: TFPCustomPen absolute AObject;
  lBrush: TFPCustomBrush absolute AObject;
  lOrigBrush: TFPCustomBrush;
  {$ifdef VerboseCDDrawing}
  ObjType: string;
  {$endif}
begin
  {$ifdef VerboseCDDrawing}
    DebugLn(Format(':>[TCDWidgetSet.SelectObject] DC=%s GDIObj=%s',
      [dbghex(DC), dbghex(GDIObj)]));
  {$endif}

  Result := 0;

  if not IsValidDC(DC) then
  begin
    {$ifdef VerboseCDDrawing}
      DebugLn(':<[TCDWidgetSet.SelectObject] Invalid DC');
    {$endif}

    Exit;
  end;

  if not IsValidGDIObject(GDIObj) then
  begin
    {$ifdef VerboseCDDrawing}
      DebugLn(':<[TCDWidgetSet.SelectObject] Invalid GDI Object');
    {$endif}

    Exit;
  end;

  aObject := TObject(GDIObj);

  if aObject is TFPCustomFont then
  begin
    {$ifdef VerboseCDDrawing}ObjType := 'Font';{$endif}

    Result := HGDIOBJ(TLazCanvas(DC).AssignedFont);
    TLazCanvas(DC).AssignFontData(lFont); // := doesn't work and Assign() raises exceptions
    TLazCanvas(DC).AssignedFont := lFont;
  end
  else if aObject is TFPCustomPen then
  begin
    {$ifdef VerboseCDDrawing}ObjType := 'Pen';{$endif}

    Result := HGDIOBJ(TLazCanvas(DC).AssignedPen);
    TLazCanvas(DC).AssignPenData(lPen); // := doesn't work and Assign() raises exceptions
    TLazCanvas(DC).AssignedPen := lPen;
  end
  else if aObject is TFPCustomBrush then
  begin
    {$ifdef VerboseCDDrawing}ObjType := 'Brush';{$endif}

    Result := HGDIOBJ(TLazCanvas(DC).AssignedBrush);
    TLazCanvas(DC).AssignBrushData(lBrush); // := doesn't work and Assign() raises exceptions
    TLazCanvas(DC).AssignedBrush := lBrush;
  end
  else if aObject is TCDBitmap then
  begin
    {$ifdef VerboseCDDrawing}ObjType := 'Bitmap';{$endif}

    Result := HGDIOBJ(TLazCanvas(DC).Image);

    TLazCanvas(DC).Image := TCDBitmap(aObject).Image;
    TLazCanvas(DC).SelectedBitmap := aObject;
  end; (*else
  if AObject is TQtRegion then
  begin
    Result := HGDIOBJ(TQtDeviceContext(DC).getClipRegion);
    SelectClipRGN(DC, HRGN(GDIObj));
  end*);

  {$ifdef VerboseCDDrawing}
    DebugLn(':<[TCDWidgetSet.SelectObject] Result=', dbghex(Result), ' ObjectType=', ObjType);
  {$endif}
end;

{------------------------------------------------------------------------------
  Method:  SetTextColor
  Params:  Handle -
  Returns:
 ------------------------------------------------------------------------------}
function TCDWidgetSet.SetTextColor(DC: HDC; Color: TColorRef): TColorRef;
var
  lFont: TFPCustomFont;
  LazDC: TLazCanvas;
begin
  {$ifdef VerboseCDDrawing}
    DebugLn(Format('[TCDWidgetSet.SetTextColor]  DC: %x Color: %8x', [DC, Color]));
  {$endif}

  Result := CLR_INVALID;
  if not IsValidDC(DC) then Exit;
  LazDC := TLazCanvas(DC);

  if LazDC.Font <> nil then
    LazDC.Font.FPColor := TColorToFPColor(Color);
end;

(*{------------------------------------------------------------------------------
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
end;*)

function TCDWidgetSet.SetWindowOrgEx(DC : HDC; NewX, NewY : Integer; OldPoint: PPoint) : Boolean;
var
  P: TPoint;
  LazDC: TLazCanvas absolute DC;
begin
  {$ifdef VerboseCDDrawing}
    DebugLn(Format('[WinAPI SetWindowOrgEx] DC=%x  NewX=%d NewY=%d',
      [DC, NewX, NewY]));
  {$endif}

  Result := False;
  if not IsValidDC(DC) then Exit;

  GetWindowOrgEx(DC, @P);
  if OldPoint <> nil then OldPoint^ := P;

  LazDC.WindowOrg := Types.Point(-NewX, -NewY);
  Result := True;
end;

function TCDWidgetSet.StretchBlt(DestDC: HDC; X, Y, Width, Height: Integer;
  SrcDC: HDC; XSrc, YSrc, SrcWidth, SrcHeight: Integer; ROp: Cardinal): Boolean;
begin
  Result := StretchMaskBlt(DestDC,X,Y,Width,Height,
                          SrcDC,XSrc,YSrc,SrcWidth,SrcHeight,
                          0,0,0,
                          ROp);
end;

function TCDWidgetSet.StretchMaskBlt(DestDC: HDC; X, Y, Width, Height: Integer;
  SrcDC: HDC; XSrc, YSrc, SrcWidth, SrcHeight: Integer; Mask: HBITMAP;
  XMask, YMask: Integer; Rop: DWORD): Boolean;
var
  SrcLazDC: TLazCanvas absolute SrcDC;
  DstLazDC: TLazCanvas absolute DestDC;
  BufferImage: TLazIntfImage = nil;
  BufferDC: TLazCanvas = nil;
  FreeBuffer: Boolean;
  SrcRect, DstRect, MaskRect: TRect;
begin
  {$ifdef VerboseCDDrawing}
    DebugLn('[WinAPI StretchMaskBlt]' +
     ' DestDC:' + dbghex(DestDC) +
     ' SrcDC:' + dbghex(SrcDC) +
     ' X:' + dbgs(X) + ' Y:' + dbgs(Y) +
     ' W:' + dbgs(Width) + ' H:', dbgs(Height) +
     ' XSrc:' + dbgs(XSrc) + ' YSrc:' + dbgs(YSrc) +
     ' WSrc:' + dbgs(SrcWidth) + ' HSrc:' + dbgs(SrcHeight));
  {$endif}

  Result := False;

  // Optimization if no stretch is desired
  if (SrcWidth = Width) and (SrcHeight = Height) then
  begin
    DstLazDC.CanvasCopyRect(SrcLazDC, X, Y, XSrc, YSrc, SrcWidth, SrcHeight);
    Exit;
  end;

    DstLazDC.Interpolation := TFPSharpInterpolation.Create;
  if (XSrc = 0) and (YSrc = 0) and (SrcWidth = SrcLazDC.Width) and (SrcHeight = SrcLazDC.Height) then
  begin
    BufferDC := SrcLazDC;
    BufferImage := TLazIntfImage(SrcLazDC.Image);
    FreeBuffer := False;
  end
  else
  begin
    UpdateControlLazImageAndCanvas(BufferImage, BufferDC,
      SrcWidth, SrcHeight, clfARGB32);
    BufferDC.CanvasCopyRect(SrcLazDC, 0, 0, XSrc, YSrc, SrcWidth, SrcHeight);
    FreeBuffer := True;
  end;

  // Execute the stretch
  DstLazDC.StretchDraw(X, Y, Width, Height, BufferImage);

  // Free the interpolation
  DstLazDC.Interpolation.Free;
  DstLazDC.Interpolation := nil;

  // Free the buffer
  if FreeBuffer then
  begin
    BufferDC.Free;
    BufferImage.Free;
  end;

  Result := True;
end;

function TCDWidgetSet.TextOut(DC: HDC; X,Y : Integer; Str : Pchar; Count: Integer) : Boolean;
begin
  Result:=ExtTextOut(DC, X, Y, 0, nil, Str, Count, nil);
end;

