{%MainUnit customdrawnint.pas}
{******************************************************************************
                  All CustomDrawn interface support routines
                   Initial Revision  : Sat Jan 17 19:00:00 2004


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

//##apiwiz##sps##   // Do not remove

function TCDWidgetSet.AskUser(const DialogCaption, DialogMessage: string;
  DialogType: LongInt; Buttons: TDialogButtons; HelpCtx: Longint): LongInt;
begin
end;

(*{------------------------------------------------------------------------------
  Function: CreateEmptyRegion
  Params:
  Returns: valid empty region
 ------------------------------------------------------------------------------}
function TQtWidgetSet.CreateEmptyRegion: hRGN;
begin
  Result:= HRGN(TQtRegion.Create(True));
end;

{------------------------------------------------------------------------------
  Function: CreateStandardCursor
  Params:
  Returns:
 ------------------------------------------------------------------------------}
function TQtWidgetSet.CreateStandardCursor(ACursor: SmallInt): HCURSOR;
var
  CursorShape: QtCursorShape;
begin
  Result := 0;
  if ACursor < crLow then Exit;
  if ACursor > crHigh then Exit;

  // TODO: map is better
  case ACursor of
    crNone      : CursorShape := QtBlankCursor;
    crArrow     : CursorShape := QtArrowCursor;
    crCross     : CursorShape := QtCrossCursor;
    crIBeam     : CursorShape := QtIBeamCursor;
    crSizeAll   : CursorShape := QtSizeAllCursor;
    crSizeNESW  : CursorShape := QtSizeBDiagCursor;
    crSizeNS    : CursorShape := QtSizeVerCursor;
    crSizeNWSE  : CursorShape := QtSizeFDiagCursor;
    crSizeWE    : CursorShape := QtSizeHorCursor;
    crSizeNW    : CursorShape := QtSizeFDiagCursor;
    crSizeN     : CursorShape := QtSizeVerCursor;
    crSizeNE    : CursorShape := QtSizeBDiagCursor;
    crSizeW     : CursorShape := QtSizeHorCursor;
    crSizeE     : CursorShape := QtSizeHorCursor;
    crSizeSW    : CursorShape := QtSizeBDiagCursor;
    crSizeS     : CursorShape := QtSizeVerCursor;
    crSizeSE    : CursorShape := QtSizeFDiagCursor;
    crUpArrow   : CursorShape := QtUpArrowCursor;
    crHourGlass : CursorShape := QtWaitCursor;
    crHSplit    : CursorShape := QtSplitHCursor;
    crVSplit    : CursorShape := QtSplitVCursor;
    crNo        : CursorShape := QtForbiddenCursor;
    crAppStart  : CursorShape := QtBusyCursor;
    crHelp      : CursorShape := QtWhatsThisCursor;
    crHandPoint : CursorShape := QtPointingHandCursor;
  else
    CursorShape := QtCursorShape(-1);
  end;
  if CursorShape <> QtCursorShape(-1) then
    Result := HCURSOR(TQtCursor.Create(CursorShape));
end;*)

(*{------------------------------------------------------------------------------
  Function: FontIsMonoSpace
  Params:
  Returns:
 ------------------------------------------------------------------------------}
function TQtWidgetSet.FontIsMonoSpace(Font: HFont): Boolean;
var
  QtFontInfo: QFontInfoH;
begin
  Result := IsValidGDIObject(Font);
  if Result then
  begin
    QtFontInfo := QFontInfo_create(TQtFont(Font).FHandle);
    try
  	  Result := QFontInfo_fixedPitch(QtFontInfo);
    finally
	    QFontInfo_destroy(QtFontInfo);
    end;
  end;
end;*)


{------------------------------------------------------------------------------
  Function: PromptUser
  Params:
  Returns:
 ------------------------------------------------------------------------------}

{------------------------------------------------------------------------------
  Function: RawImage_FromDevice
  Params: ADC:
          ARect:
          ARawImage:
  Returns:

  This function is utilized when the function TBitmap.LoadFromDevice is called

  The main use for this function is to get a screenshot. It may have other uses,
   but this is the only one implemented here.

  MWE: exept for the desktop, there is always a bitmep selected in the DC.
       So get this internal bitmap and pass it to RawImage_FromBitmap
 ------------------------------------------------------------------------------}
function TCDWidgetSet.RawImage_FromDevice(out ARawImage: TRawImage; ADC: HDC; const ARect: TRect): Boolean;
var
  Image: XLib.PXImage;
  //XImage: XLib.TXImage;
  Pdata: Pchar;
  RootWindow: TWindow;
  PlaneMask: culong;
  lx,ly,lwidth,lheight: culong;
  lDataLength: Integer;

procedure DescriptionFix(var ArawImage: TRawImage; Awidth,AHeight: Integer);
Var
  Description: TRawImageDescription absolute ArawImage.Description;
begin
  // setup an artificial ScanLineImage with format RGB 32 bit, 32bit depth format
  //FillChar(Description, SizeOf(Description), 0);
  with Description do begin;
    Format := ricfRGBA;
    Depth := 24; // used bits per pixel
    Width := AWidth;
    Height := AHeight;
    BitOrder := riboBitsInOrder;
    ByteOrder := riboLSBFirst;
    LineOrder := riloTopToBottom;
    BitsPerPixel := 32; // bits per pixel. can be greater than Depth.
    LineEnd := rileDWordBoundary;
    RedPrec := 8; // red precision. bits for red
    RedShift := 16;
    GreenPrec := 8;
    GreenShift := 8; // bitshift. Direction: from least to most signifikant
    BluePrec := 8;
    BlueShift := 0;
    AlphaPrec := 0;
    AlphaShift := 0;
    MaskBitsPerPixel := 1;
    MaskShift:= 0;
    MaskLineEnd:=rileByteBoundary;
    MaskBitOrder:=riboBitsInOrder;
    PaletteColorCount:= 0;
    PaletteBitsPerIndex:=0;
    PaletteShift:= 0;
    PaletteLineEnd:=rileTight;
    PaletteBitOrder:=riboBitsInOrder;
    PaletteByteOrder:=riboLSBFirst;
  end;
end;

begin
  {$ifdef VerboseCDWinAPI}
    DebugLn('Trace:> [WinAPI GetRawImageFromDevice] SrcDC: ', dbghex(ADC),
     ' SrcWidth: ', dbgs(ARect.Right - ARect.Left),
     ' SrcHeight: ', dbgs(ARect.Bottom - ARect.Top));
  {$endif}

  // create a raw Image to hold screenshot
  lx:=ARect.Left;
  ly:=ARect.Top;
  lwidth:=ARect.Right-ARect.Left;
  lheight:=Arect.Bottom-ARect.Top;
  ARawImage.Init;
  // We should get the description from the drawable but this may do
  ARawImage.Description.Init_BPP32_A8R8G8B8_BIO_TTB(lwidth, lheight);
  ARawImage.CreateData(True);
  // Get root Window
  RootWindow:= XDefaultRootWindow(FDisplay);
  // Get Plane mask
  PlaneMask:=XAllPlanes();
  // Take the screenshot
  Image:= XGetImage(
               FDisplay,RootWindow, // Screen, Window
               lx,ly, // x , y
               lwidth,lheight, // width, height
               PlaneMask, // All planes
               ZPixmap);  // Image format
  // copy screen shot to our rawImage
  lDataLength:= ARawImage.DataSize;
  System.Move( Image^.Data^,ARawImage.Data^,lDataLength);
  // We should fix the description from the drawable but this will do for now
  DescriptionFix(ARawImage,lwidth,lheight);
  ARawImage.Mask:=nil;
  ARawImage.Palette:=nil;
  Result := True;
end;

//##apiwiz##eps##   // Do not remove, no wizard declaration after this line
