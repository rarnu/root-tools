{%MainUnit ../interfacebase.pp}
{ $Id: intfbaselcl.inc 48908 2015-04-30 08:34:13Z sekelsenmat $
 ******************************************************************************
                                  TWidgetSet

                           interface communication  stuff

  !! In this file only interface related code as defined in lclintfh.inc
     Most routines implement only the default

  !! Keep this alphabetical !!
 ******************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
//##apiwiz##sps##   // Do not remove

function TWidgetSet.AddEventHandler(AHandle: THandle; AFlags: dword; 
  AEventHandler: TWaitHandleEvent; AData: PtrInt): PEventHandler;
begin
  Result := nil;
end;

function TWidgetSet.AddProcessEventHandler(AHandle: THandle; 
  AEventHandler: TChildExitEvent; AData: PtrInt): PProcessEventHandler;
begin
  Result := nil;
end;

function TWidgetSet.AddPipeEventHandler(AHandle: THandle; 
  AEventHandler: TPipeEvent; AData: PtrInt): PPipeEventHandler;
begin
  Result := nil;
end;

function TWidgetSet.AllocateHWnd(Method: TLCLWndMethod): HWND;
begin
  Result := 0;
end;

function TWidgetSet.AskUser(const DialogCaption, DialogMessage: string;
    DialogType: LongInt; Buttons: TDialogButtons; HelpCtx: Longint): LongInt;
begin
  if QuestionDialogFunction <> nil then
    Result := QuestionDialogFunction(DialogCaption, DialogMessage, DialogType, Buttons, HelpCtx)
  else
    Result := 0;
end;

procedure TWidgetSet.CallDefaultWndHandler(Sender: TObject; var Message);
begin
end;

// the clipboard functions are internally used by TClipboard
function TWidgetSet.ClipboardFormatToMimeType(FormatID: TClipboardFormat): string;
begin
  Result := '';
end;

function TWidgetSet.ClipboardGetData(ClipboardType: TClipboardType;
  FormatID: TClipboardFormat;  Stream: TStream): boolean;
begin
  Result := false;
end;

// ! List will be created. You must free it yourself with FreeMem(List) !
function TWidgetSet.ClipboardGetFormats(ClipboardType: TClipboardType;
  var Count: integer; var List: PClipboardFormat): boolean;
begin
  Result := true;
  Count := 0;
  List := nil;
end;

function TWidgetSet.ClipboardGetOwnerShip(ClipboardType: TClipboardType;
  OnRequestProc: TClipboardRequestEvent;  FormatCount: integer;
  Formats: PClipboardFormat): boolean;
begin
  Result := false;
end;

function TWidgetSet.ClipboardRegisterFormat(
  const AMimeType: string): TClipboardFormat;
begin
  Result := 0;
end;

//todo: remove ?
function TWidgetSet.CreateEmptyRegion: hRGN;
begin
  Result:=CreateRectRGN(0,0,0,0);
end;

function TWidgetSet.CreatePixmapIndirect(const Data: Pointer; const TransColor: Longint): HBITMAP;
begin
  Result := 0;
end;

function TWidgetSet.CreateRegionCopy(SrcRGN: hRGN): hRGN;
begin
  // If the interface has a better way to create a copy it can override this
  Result:=CreateEmptyRegion;
  CombineRGN(Result,SrcRGN,SrcRGN,RGN_COPY);
end;

function TWidgetSet.CreateRubberBand(const ARect: TRect; const ABrush: HBrush): HWND;
begin
  Result := 0;
end;

function TWidgetSet.CreateStandardCursor(ACursor: SmallInt): hCursor;
begin
  Result := 0;
end;

function TWidgetSet.DCClipRegionValid(DC: HDC): boolean;
var
  Clip: hRGN;
begin
  // If the interface has a better way to check a region it can override this
  //debugln('TWidgetSet.DCClipRegionValid DC=',DbgS(DC));
  Clip:=CreateEmptyRegion;
  Result:=GetClipRGN(DC,Clip)>=0;
  DeleteObject(Clip);
end;

procedure TWidgetSet.DeallocateHWnd(Wnd: HWND);
begin

end;

procedure TWidgetSet.DestroyRubberBand(ARubberBand: HWND);
begin

end;

procedure TWidgetSet.DrawDefaultDockImage(AOldRect, ANewRect: TRect; AOperation: TDockImageOperation);

  procedure DefaultDockImage(ARect: TRect);
  const
    PenSize = 4;
  var
    DC: HDC;
    OldPen, NewPen: HPen;
    OldRop: Integer;
    LogPen: TLogPen;
  begin
    LogPen.lopnColor := $00FFFFFF;
    LogPen.lopnWidth := Point(PenSize, 0);
    LogPen.lopnStyle := PS_SOLID;
    DC := GetDC(0);
    try
      NewPen := CreatePenIndirect(LogPen);
      OldPen := SelectObject(DC, NewPen);
      OldRop := SetROP2(DC, R2_XORPEN);
      with ARect do
      begin
        MoveToEx(DC, Left+PenSize, Top+PenSize, nil);
        LineTo(DC, Right-PenSize, Top+PenSize);
        LineTo(DC, Right-PenSize, Bottom-PenSize);
        LineTo(DC, Left+PenSize, Bottom-PenSize);
        LineTo(DC, Left+PenSize, Top+PenSize);
      end;
    finally
      SetROP2(DC, OldRop);
      DeleteObject(SelectObject(DC, OldPen));
      ReleaseDC(0, DC);
    end;
  end;
  
begin
  if AOperation in [disMove, disHide] then
    DefaultDockImage(AOldRect);
  if AOperation in [disMove, disShow] then
    DefaultDockImage(ANewRect);
end;

procedure TWidgetSet.DrawGrid(DC: HDC; const R: TRect; DX, DY: Integer);
var
  X, Y: Integer;
  W, H: Integer;
  SavedDC: Integer;
begin
  SavedDC := SaveDC(DC);
  try
    W := (R.Right - R.Left - 1) div DX;
    H := (R.Bottom - R.Top - 1) div DY;
    
    // remove rows from clip rect
    for Y := 0 to H do
    begin
      ExcludeClipRect(DC, R.Left, R.Top + Y * DY + 1, R.Right + 1, R.Top + (Y + 1) * DY);
    end;

    // draw vertical lines cross excluded rows -> only grid cross points painted 
    for X := 0 to W do
    begin
      if MoveToEx(DC, R.Left + X * DX, R.Top, nil) then
        LineTo(DC, R.Left + X * DX, R.Bottom + 1);
    end;
  finally
    RestoreDC(DC, SavedDC);
  end;
end;

function TWidgetSet.ExtUTF8Out(DC: HDC; X, Y: Integer; Options: Longint;
  Rect: PRect; Str: PChar; Count: Longint; Dx: PInteger): Boolean;
begin
  Result:=ExtTextOut(DC,X,Y,Options,Rect,Str,Count,Dx);
end;

function TWidgetSet.RawImage_CreateBitmaps(const ARawImage: TRawImage; out ABitmap, AMask: HBitmap; ASkipMask: Boolean = False): Boolean;
begin
  Result := False;
end;

function TWidgetSet.RawImage_DescriptionFromBitmap(ABitmap: HBITMAP; out ADesc: TRawImageDescription): Boolean;
begin
  Result := False;
end;

function TWidgetSet.RawImage_DescriptionFromDevice(ADC: HDC; out ADesc: TRawImageDescription): Boolean;
begin
  Result := False;
end;

function TWidgetSet.RawImage_FromBitmap(out ARawImage: TRawImage; ABitmap, AMask: HBITMAP; ARect: PRect): Boolean;
begin
  Result := False;
end;

function TWidgetSet.RawImage_FromDevice(out ARawImage: TRawImage; ADC: HDC; const ARect: TRect): Boolean;
begin
  Result := False;
end;

function TWidgetSet.RawImage_QueryDescription(AFlags: TRawImageQueryFlags; var ADesc: TRawImageDescription): Boolean;
var
  Desc: TRawImageDescription;
begin
  // for most widgetsets, this default will work, however when necessary it can be overridden.
  Desc.Init;
  Result := RawImage_DescriptionFromDevice(0, Desc);
  if not Result then Exit;

  if not (riqfUpdate in AFlags)
  then ADesc.Init;

  if riqfMono in AFlags
  then begin
    ADesc.Format := ricfGray;
    ADesc.Depth := 1;
    ADesc.BitOrder := Desc.MaskBitOrder;
    ADesc.ByteOrder := riboLSBFirst;
    ADesc.LineOrder := Desc.LineOrder;
    ADesc.LineEnd := Desc.MaskLineEnd;
    ADesc.BitsPerPixel := Desc.MaskBitsPerPixel;
    ADesc.RedPrec := 1;
    ADesc.RedShift := Desc.MaskShift
  end
  else if riqfGrey in AFlags
  then begin
    ADesc.Format := ricfGray;
    ADesc.Depth := 8;
    ADesc.BitOrder := Desc.BitOrder;
    ADesc.ByteOrder := Desc.ByteOrder;
    ADesc.LineOrder := Desc.LineOrder;
    ADesc.LineEnd := Desc.LineEnd;
    ADesc.BitsPerPixel := 8;
    ADesc.RedPrec := 8;
    ADesc.RedShift := 0;
  end
  else if riqfRGB in AFlags
  then begin
    ADesc.Format := ricfRGBA;
    ADesc.Depth := Desc.Depth;
    ADesc.BitOrder := Desc.BitOrder;
    ADesc.ByteOrder := Desc.ByteOrder;
    ADesc.LineOrder := Desc.LineOrder;
    ADesc.LineEnd := Desc.LineEnd;
    ADesc.BitsPerPixel := Desc.BitsPerPixel;
    ADesc.RedPrec := Desc.RedPrec;
    ADesc.RedShift := Desc.RedShift;
    ADesc.GreenPrec := Desc.GreenPrec;
    ADesc.GreenShift := Desc.GreenShift;
    ADesc.BluePrec := Desc.BluePrec;
    ADesc.BlueShift := Desc.BlueShift;
  end;
  
  if riqfAlpha in AFlags
  then begin
    ADesc.AlphaPrec := Desc.AlphaPrec;
    ADesc.AlphaShift := Desc.AlphaShift;
  end
  else begin
    if  (Desc.Depth = 32) and (ADesc.Format = ricfRGBA)
    and ([riqfMono, riqfGrey, riqfRGB] * AFlags <> [])
    then ADesc.Depth := 24;
  end;
  
  if riqfMask in AFlags
  then begin
    ADesc.MaskBitsPerPixel := Desc.MaskBitsPerPixel;
    ADesc.MaskShift := Desc.MaskShift;
    ADesc.MaskLineEnd := Desc.MaskLineEnd;
    ADesc.MaskBitOrder := Desc.MaskBitOrder;
  end;

  if riqfPalette in AFlags
  then begin
    ADesc.PaletteColorCount := Desc.PaletteColorCount;
    ADesc.PaletteBitsPerIndex := Desc.PaletteBitsPerIndex;
    ADesc.PaletteShift := Desc.PaletteShift;
    ADesc.PaletteLineEnd := Desc.PaletteLineEnd;
    ADesc.PaletteBitOrder := Desc.PaletteBitOrder;
    ADesc.PaletteByteOrder := Desc.PaletteByteOrder;
  end;
end;

function TWidgetSet.TextUTF8Out(DC: HDC; X, Y: Integer; Str: PChar; Count: Longint): Boolean;
begin
  Result:=TextOut(DC,X,Y,Str,Count);
end;


function TWidgetSet.FontCanUTF8(Font: HFont): boolean;
begin
  Result:=false;
end;

function TWidgetSet.FontIsMonoSpace(Font: HFont): boolean;
begin
  Result:=false;
end;

function TWidgetSet.Frame3d(DC: HDC; var ARect: TRect;
  const FrameWidth: integer; const Style: TGraphicsBevelCut): Boolean;
begin
  Result:= false;
end;

function TWidgetSet.GetAcceleratorString(const AVKey: Byte;
  const AShiftState: TShiftState): String;
// If the interface has a better way to create a string it can override this
begin
  Result := KeyAndShiftStateToKeyString(AVKey, AShiftState);
end;

function TWidgetSet.GetAvailableNativeCanvasTypes(DC: HDC; AAllowFallbackToParent: Boolean): TNativeCanvasTypes; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
begin
  Result := [];
end;

function TWidgetSet.GetAvailableNativeHandleTypes(Handle: HWND; AAllowFallbackToParent: Boolean): TNativeHandleTypes; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
begin
  Result := [];
end;

function TWidgetSet.GetCaretRespondToFocus(handle: HWND;
  var ShowHideOnFocus: boolean): Boolean;
begin
  ShowHideOnFocus := true;
  Result := False;
end;

function TWidgetSet.GetClientBounds(Handle: HWND; var ARect: TRect): Boolean;
begin
  Result := false;
end;

function TWidgetSet.GetCmdLineParamDescForInterface: string;
begin
  Result := '';
end;

function TWidgetSet.GetControlConstraints(Constraints: TObject): boolean;
begin
  Result:=true;
end;

function TWidgetSet.GetDeviceSize(DC: HDC; var p: TPoint): boolean;
begin
  p.X := 0;
  p.Y := 0;
  Result := false;
end;

function TWidgetSet.GetDCOriginRelativeToWindow(PaintDC: HDC;
  WindowHandle: HWND; var OriginDiff: TPoint): boolean;
begin
  OriginDiff.X:=0;
  OriginDiff.Y:=0;
  Result:=true;
end;

function TWidgetSet.GetDesignerDC(WindowHandle: HWND): HDC;
begin
  Result:=GetDC(WindowHandle);
end;

function TWidgetSet.GetLCLOwnerObject(Handle: HWnd): TObject;
begin
  if Handle <> 0
  then Result := TObject(GetProp(Handle,'WinControl'))
  else Result := nil;
end;

function TWidgetSet.GetNativeCanvas(DC: HDC; AHandleType: TNativeCanvasType; AAllowFallbackToParent: Boolean): PtrInt;
begin
  Result := 0;
end;

function TWidgetSet.GetNativeHandle(Handle: HWND; AHandleType: TNativeHandleType; AAllowFallbackToParent: Boolean): PtrInt;
begin
  Result := 0;
end;

// TODO: remove
function TWidgetSet.GetScrollBarSize(Handle: HWND;
  SBStyle: Integer): integer;
begin
  Result := GetSystemMetrics(SBStyle);
end;

function TWidgetSet.GetScrollbarVisible(Handle: HWND;
  SBStyle: Integer): boolean;
begin
  Result := false;
end;

function TWidgetSet.GetWindowRelativePosition(Handle: hwnd;
  var Left, Top: integer): boolean;
{ returns the position of the left, top coordinate relative to the clientorigin
  of its parent. This is normally the Left, Top of a TWinControl. But not
  during moving/sizing. }
var
  ChildRect: TRect;
  ParentLeftTop: TPoint;
  ParentHandle: hWnd;
begin
  Result:=false;
  GetWindowRect(Handle,ChildRect);
  Left:=ChildRect.Left;
  Top:=ChildRect.Top;
  ParentHandle:=GetParent(Handle);
  if ParentHandle<>0 then begin
    ParentLeftTop.X:=0;
    ParentLeftTop.Y:=0;
    if not ClientToScreen(ParentHandle,ParentLeftTop) then exit;
    dec(Left,ParentLeftTop.X);
    dec(Top,ParentLeftTop.Y);
  end;
  Result := true;
end;

function TWidgetSet.GetWindowSize(Handle: hwnd;
  var Width, Height: integer): boolean;
// Returns the current Width and Height
begin
  Result:=false;
end;

function TWidgetSet.IntfSendsUTF8KeyPress: Boolean;
begin
  Result := GetLCLCapability(lcSendsUTF8KeyPress) = LCL_CAPABILITY_YES;
end;

function TWidgetSet.InvalidateFrame(aHandle: HWND; ARect: pRect;
  bErase: Boolean; BorderWidth: integer): Boolean;

  function Min(i1, i2: integer): integer;
  begin
    if i1<=i2 then Result:=i1 else Result:=i2;
  end;

  function Max(i1, i2: integer): integer;
  begin
    if i1<=i2 then Result:=i2 else Result:=i1;
  end;

var
  BorderRect: TRect;
begin
  Result:=false;
  BorderRect:=ARect^;
  // left
  BorderRect.Right:=Min(BorderRect.Right,BorderRect.Left+BorderWidth);
  if not InvalidateRect(aHandle,@BorderRect,bErase) then exit;
  BorderRect.Right:=ARect^.Right;
  // top
  BorderRect.Bottom:=Min(BorderRect.Bottom,BorderRect.Top+BorderWidth);
  if not InvalidateRect(aHandle,@BorderRect,bErase) then exit;
  BorderRect.Bottom:=ARect^.Bottom;
  // right
  BorderRect.Left:=Max(BorderRect.Left,BorderRect.Right-BorderWidth);
  if not InvalidateRect(aHandle,@BorderRect,bErase) then exit;
  BorderRect.Left:=ARect^.Left;
  // bottom
  BorderRect.Top:=Max(BorderRect.Top,BorderRect.Bottom-BorderWidth);
  if not InvalidateRect(aHandle,@BorderRect,bErase) then exit;
  Result:=true;
end;

function TWidgetSet.IsDesignerDC(WindowHandle: HWND; DC: HDC): Boolean;
begin
  Result := False;
end;

function TWidgetSet.IsMobilePlatform: Boolean;
begin
  Result := False;
  // A good default would be: Result := Application.ApplicationType in [atPDA, atKeyPadDevice, atTV, atMobileEmulator];
  // But Forms is not in the uses clause
end;

function TWidgetSet.IsCDIntfControl(AWinControl: TObject): Boolean;
begin
  Result := False;
end;

function TWidgetSet.MoveWindowOrgEx(dc: hdc; dX,dY: Integer): boolean;
var
  P: TPoint;
  lResult: Integer;
begin
  lResult := GetWindowOrgEx(dc, @P);
  if lResult <> 0 then
    Result:=SetWindowOrgEx(dc, P.x-dX, P.y-dY, @P)
  else
    Result := False;

  {$ifdef DEBUG_WINDOW_ORG}
  DbgAppendToFile(ExtractFilePath(ParamStr(0)) + '1.log',
    Format(':> [TWidgetSet.MoveWindowOrgEx] DC=%d P.x=%d P.y=%d dx=%d dy=%d lResult=%d Result=%d',
    [DC, P.x, P.y, dx, dy, lResult, Integer(Result)]));
  {$endif}
end;

function TWidgetSet.PromptUser(const DialogCaption, DialogMessage: String;
  DialogType: longint; Buttons: PLongint;
  ButtonCount, DefaultIndex, EscapeResult: Longint): Longint;
begin
  if PromptDialogFunction <> nil then
    Result := PromptDialogFunction(DialogCaption, DialogMessage, DialogType,
       Buttons, ButtonCount, DefaultIndex, EscapeResult, True, 0, 0)
  else
    Result := 0;
end;

function TWidgetSet.PromptUserAtXY(const DialogCaption,
  DialogMessage: String;
  DialogType: longint; Buttons: PLongint;
  ButtonCount, DefaultIndex, EscapeResult: Longint;
  X, Y: Longint): Longint;
begin
  if PromptDialogFunction<>nil then
    Result := PromptDialogFunction(DialogCaption, DialogMessage, DialogType,
       Buttons, ButtonCount, DefaultIndex, EscapeResult, False, X, Y)
  else
    Result := 0;
end;

function TWidgetSet.RadialArc(DC: HDC;
  left, top, right, bottom, sx, sy, ex, ey: Integer): Boolean;
var
  A1, A2: Extended;
begin
  Coords2Angles(left, top, right-left, bottom-top, sx, sy, ex, ey, A1, A2);
  Result := Arc(DC, left, top, right, bottom, RoundToInt(A1), RoundToInt(A2));
end;

function TWidgetSet.RadialChord(DC: HDC;
  x1, y1, x2, y2, sx, sy, ex, ey: Integer): Boolean;
var
  A1, A2: Extended;
Begin
  Coords2Angles(x1, y1, x2-x1, y2-y1, sx, sy, ex, ey, A1, A2);
  Result := AngleChord(DC, x1, y1, x2, y2, RoundToInt(A1), RoundToInt(A2));
End;

function TWidgetSet.RadialPie(DC: HDC; x1, y1, x2, y2,
  Angle1, Angle2: Integer): Boolean;
var
  Points: PPoint;
  Count: Longint;
begin
  Result := False;
  Points := nil;
  Count := 0;
  PolyBezierArcPoints(x1, y1, x2 - x1, y2 - y1, Angle1, Angle2, 0, Points, Count);
  Inc(Count,2);
  ReallocMem(Points, Count*SizeOf(TPoint));
  Points[Count - 2] := CenterPoint(Rect(x1, y1, x2, y2));
  Points[Count - 1] := Points[0];
  Polygon(DC, Points, Count, True);
  ReallocMem(Points, 0);
  Result := True;
end;

function TWidgetSet.RegroupMenuItem(hndMenu: HMENU; GroupIndex: Integer): Boolean;
begin
  Result := false;
end;

procedure TWidgetSet.RemoveEventHandler(var AHandler: PEventHandler);
begin
end;

procedure TWidgetSet.RemoveProcessEventHandler(var AHandler: PProcessEventHandler);
begin
end;

procedure TWidgetSet.RemovePipeEventHandler(var AHandler: PPipeEventHandler);
begin
end;

function TWidgetSet.ReleaseDesignerDC(hWnd: HWND; DC: HDC): Integer;
begin
  Result := ReleaseDC(hWnd, DC);
end;

function TWidgetSet.RequestInput(const InputCaption, InputPrompt: String;
  MaskInput: Boolean; var Value: String): Boolean;
begin
  if InputDialogFunction<>nil then
    Result := InputDialogFunction(InputCaption, InputPrompt, MaskInput, Value)
  else
    Result := false;
end;

procedure TWidgetSet.SendCachedLCLMessages;
begin
end;

function TWidgetSet.SetCaretRespondToFocus(handle: HWND;
  ShowHideOnFocus: boolean): Boolean;
begin
  Result := False;
end;

function TWidgetSet.SetComboMinDropDownSize(Handle: HWND;
  MinItemsWidth, MinItemsHeight, MinItemCount: integer): boolean;
begin
  Result := false;
end;

procedure TWidgetSet.SetEventHandlerFlags(AHandler: PEventHandler; NewFlags: dword);
begin
end;

procedure TWidgetSet.SetRubberBandRect(const ARubberBand: HWND; const ARect: TRect);
begin

end;

// This routine is only for platforms which need a special combobox dialog, like Android
// It returns true if a dialog was provided for doing this task or false otherwise
// The process is assynchronous, so the result will be given in LCLIntf.OnShowSelectItemDialogResult
function TWidgetSet.ShowSelectItemDialog(const AItems: TStrings; APos: TPoint): Boolean;
begin
  Result := False;
end;

function TWidgetSet.StretchMaskBlt(DestDC: HDC;
  X, Y, Width, Height: Integer; SrcDC: HDC;
  XSrc, YSrc, SrcWidth, SrcHeight: Integer;
  Mask: HBITMAP; XMask, YMask: Integer; Rop: DWORD): Boolean;
begin
  Result := False;
end;

//##apiwiz##eps##   // Do not remove
