{%MainUnit ../lclintf.pas}
{ $Id: lclintf.inc 48908 2015-04-30 08:34:13Z sekelsenmat $
******************************************************************************
  All interface communication related stuff goes here.
  This file is used by LCLIntf.pas
  If a procedure is platform dependent then it should call:
    WidgetSet.MyDependentProc

  If a procedure insn't platform dependent, it is no part of InterfaseBase has
  to be implementerd here

  !! Keep this alphabetical !!

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

******************************************************************************
  These functions redirect to the platform specific interface object.

  Note:
    the section for not referring WidgetSet is at the end
 ******************************************************************************}
//##apiwiz##sps##   // Do not remove

function AddEventHandler(AHandle: THandle; AFlags: dword; 
  AEventHandler: TWaitHandleEvent; AData: PtrInt): PEventHandler;
begin
  Result := WidgetSet.AddEventHandler(AHandle, AFlags, AEventHandler, AData);
end;

function AddPipeEventHandler(AHandle: THandle; AEventHandler: TPipeEvent; 
  AData: PtrInt): PPipeEventHandler;
begin
  Result:=WidgetSet.AddPipeEventHandler(AHandle, AEventHandler, AData);
end;

function AddProcessEventHandler(AHandle: THandle; AEventHandler: TChildExitEvent; 
  AData: PtrInt): PProcessEventHandler;
begin
  Result:=WidgetSet.AddProcessEventHandler(AHandle, AEventHandler, AData);
end;

function AllocateHWnd(Method: TLCLWndMethod): HWND;
begin
  Result := WidgetSet.AllocateHWnd(Method);
end;

function AskUser(const DialogCaption, DialogMessage: string; DialogType: LongInt; Buttons: TDialogButtons; HelpCtx: Longint): LongInt;
begin
  Result := WidgetSet.AskUser(DialogCaption, DialogMessage, DialogType, Buttons, HelpCtx);
end;

procedure CallDefaultWndHandler(Sender: TObject; var Message);
begin
  WidgetSet.CallDefaultWndHandler(Sender,Message);
end;

// the clipboard functions are internally used by TClipboard
function ClipboardFormatToMimeType(FormatID: TClipboardFormat): string;
begin
  Result := WidgetSet.ClipboardFormatToMimeType(FormatID);
end;

function ClipboardGetData(ClipboardType: TClipboardType;
  FormatID: TClipboardFormat; Stream: TStream): boolean;
begin
  Result := WidgetSet.ClipboardGetData(ClipboardType, FormatID, Stream);
end;

// ! List will be created. You must free it yourself with FreeMem(List) !
function ClipboardGetFormats(ClipboardType: TClipboardType;
  var Count: integer; var List: PClipboardFormat): boolean;
begin
  Result := WidgetSet.ClipboardGetFormats(ClipboardType,Count,List);
end;

function ClipboardGetOwnerShip(ClipboardType: TClipboardType;
  OnRequestProc: TClipboardRequestEvent;  FormatCount: integer;
  Formats: PClipboardFormat): boolean;
begin
  Result := WidgetSet.ClipboardGetOwnerShip(ClipboardType, OnRequestProc,
                   FormatCount, Formats);
end;

function ClipboardRegisterFormat(const AMimeType: string): TClipboardFormat;
begin
  Result := WidgetSet.ClipboardRegisterFormat(AMimeType);
end;

function CreateEmptyRegion: hRGN;
begin
  Result := WidgetSet.CreateEmptyRegion;
end;

function CreatePixmapIndirect(const Data: Pointer; const TransColor: Longint): HBITMAP;
begin
  Result := WidgetSet.CreatePixmapIndirect(Data, TransColor);
end;

function CreateRegionCopy(SrcRGN: hRGN): hRGN;
begin
  Result := WidgetSet.CreateRegionCopy(SrcRGN);
end;

function CreateStandardCursor(ACursor: SmallInt): hCursor;
begin
  Result := WidgetSet.CreateStandardCursor(ACursor);
end;

function DCClipRegionValid(DC: HDC): boolean;
begin
  Result := WidgetSet.DCClipRegionValid(DC);
end;

function CreateRubberBand(const ARect: TRect; const ABrush: HBrush = 0): HWND;
begin
  Result := WidgetSet.CreateRubberBand(ARect, ABrush);
end;

procedure DeallocateHWnd(Wnd: HWND);
begin
  WidgetSet.DeallocateHWnd(Wnd);
end;

procedure DestroyRubberBand(ARubberBand: HWND);
begin
  WidgetSet.DestroyRubberBand(ARubberBand);
end;

procedure DrawDefaultDockImage(AOldRect, ANewRect: TRect; AOperation: TDockImageOperation);
begin
  WidgetSet.DrawDefaultDockImage(AOldRect, ANewRect, AOperation);
end;

procedure DrawGrid(DC: HDC; const R: TRect; DX, DY: Integer);
begin
  WidgetSet.DrawGrid(DC, R, DX, DY);
end;

function ExtUTF8Out(DC: HDC; X, Y: Integer; Options: Longint; Rect: PRect;
  Str: PChar; Count: Longint; Dx: PInteger): Boolean;
begin
  Result := WidgetSet.ExtUTF8Out(DC,X,Y,Options,Rect,Str,Count,Dx);
end;

function RawImage_CreateBitmaps(const ARawImage: TRawImage; out ABitmap, AMask: HBitmap; ASkipMask: Boolean): Boolean;
begin
  Result := WidgetSet.RawImage_CreateBitmaps(ARawImage, ABitmap, AMask, ASkipMask);
end;

function RawImage_DescriptionFromBitmap(ABitmap: HBITMAP; out ADesc: TRawImageDescription): Boolean;
begin
  Result := WidgetSet.RawImage_DescriptionFromBitmap(ABitmap, ADesc);
end;

function RawImage_DescriptionFromDevice(ADC: HDC; out ADesc: TRawImageDescription): Boolean;
begin
  Result := WidgetSet.RawImage_DescriptionFromDevice(ADC, ADesc);
end;

function RawImage_FromBitmap(out ARawImage: TRawImage; ABitmap, AMask: HBITMAP; ARect: PRect): Boolean;
begin
  Result := WidgetSet.RawImage_FromBitmap(ARawImage, ABitmap, AMask, ARect);
end;

function RawImage_FromDevice(out ARawImage: TRawImage; ADC: HDC; const ARect: TRect): Boolean;
begin
  Result := WidgetSet.RawImage_FromDevice(ARawImage, ADC, ARect);
end;

function RawImage_QueryDescription(AFlags: TRawImageQueryFlags; var ADesc: TRawImageDescription): Boolean;
begin
  Result := WidgetSet.RawImage_QueryDescription(AFlags, ADesc);
end;


function TextUTF8Out(DC: HDC; X, Y: Integer;  Str: PChar; Count: Longint): Boolean;
begin
  Result := WidgetSet.TextUTF8Out(DC,X,Y,Str,Count);
end;

function FontCanUTF8(Font: HFont): boolean;
begin
  Result := WidgetSet.FontCanUTF8(Font){%H-};
end;

function FontIsMonoSpace(Font: HFont): boolean;
begin
  Result := WidgetSet.FontIsMonoSpace(Font);
end;

function Frame3d(DC: HDC; var ARect: TRect; const FrameWidth: integer;
  const Style: TGraphicsBevelCut): boolean;
begin
  Result := WidgetSet.Frame3d(DC, ARect, FrameWidth, Style);
end;

function GetAvailableNativeCanvasTypes(DC: HDC; AAllowFallbackToParent: Boolean): TNativeCanvasTypes;
begin
  Result := Widgetset.GetAvailableNativeCanvasTypes(DC, AAllowFallbackToParent);
end;

function GetAvailableNativeHandleTypes(Handle: HWND; AAllowFallbackToParent: Boolean): TNativeHandleTypes;
begin
  Result := Widgetset.GetAvailableNativeHandleTypes(Handle, AAllowFallbackToParent);
end;

function GetCaretRespondToFocus(handle: HWND; var ShowHideOnFocus: boolean): Boolean;
begin
  Result := WidgetSet.GetCaretRespondToFocus(handle,ShowHideOnFocus);
end;

{------------------------------------------------------------------------------
  Function: GetClientBounds
  Params: handle:
          ARect:
          Result:
  Returns: true on success

  Returns the client bounds of a control. The client bounds is the rectangle of
  the inner area of a control, where the child controls are visible. The
  coordinates are relative to the control's left and top.
 ------------------------------------------------------------------------------}
function GetClientBounds(Handle: HWND; var ARect: TRect): Boolean;
begin
  Result := WidgetSet.GetClientBounds(Handle, ARect);
end;

{------------------------------------------------------------------------------
  Function: GetCmdLineParamDescForInterface
  Params: none
  Returns: ansistring

  Returns a description of the command line parameters, that are understood by
  the interface.
 ------------------------------------------------------------------------------}
function GetCmdLineParamDescForInterface: string;
begin
  Result := WidgetSet.GetCmdLineParamDescForInterface;
end;

{------------------------------------------------------------------------------
  Function: GetControlConstraints
  Params: Constraints: TObject
  Returns: true on success

  Updates the constraints object (e.g. TSizeConstraints) with interface specific
  bounds. For instance, vertical scrollbars under gtk are fixed in width. So,
  it sets MinInterfaceWidth and MaxInterfaceWidth. This is used by the auto
  aligning/sizing algorithms of the LCL.
 ------------------------------------------------------------------------------}
function GetControlConstraints(Constraints: TObject): boolean;
begin
  Result := WidgetSet.GetControlConstraints(Constraints);
end;

function GetDCOriginRelativeToWindow(PaintDC: HDC; WindowHandle: HWND;
  var OriginDiff: TPoint): boolean;
begin
  Result := WidgetSet.GetDCOriginRelativeToWindow(PaintDC,WindowHandle,
                                                        OriginDiff);
end;

function GetDesignerDC(WindowHandle: HWND): HDC;
begin
  Result := WidgetSet.GetDesignerDC(WindowHandle);
end;

function GetDeviceSize(DC: HDC; var p: TPoint): boolean;
begin
  Result := WidgetSet.GetDeviceSize(DC,p);
end;

function GetLCLOwnerObject(Handle: HWnd): TObject;
begin
  Result := WidgetSet.GetLCLOwnerObject(Handle);
end;

function GetScrollBarSize(Handle: HWND; SBStyle: Integer): integer;
begin
  Result := WidgetSet.GetScrollBarSize(Handle, SBStyle);
end;

function GetScrollbarVisible(Handle: HWND; SBStyle: Integer): boolean;
begin
  Result := WidgetSet.GetScrollbarVisible(Handle, SBStyle);
end;

function GetAcceleratorString(const AVKey: Byte; const AShiftState: TShiftState): String;
begin
  Result := WidgetSet.GetAcceleratorString(AVKey, AShiftState);
end;

function GetNativeCanvas(DC: HDC; AHandleType: TNativeCanvasType; AAllowFallbackToParent: Boolean): PtrInt;
begin
  Result := Widgetset.GetNativeCanvas(DC, AHandleType, AAllowFallbackToParent);
end;

function GetNativeHandle(Handle: HWND; AHandleType: TNativeHandleType; AAllowFallbackToParent: Boolean): PtrInt;
begin
  Result := Widgetset.GetNativeHandle(Handle, AHandleType, AAllowFallbackToParent);
end;

{------------------------------------------------------------------------------
  function GetWindowRelativePosition(Handle: hwnd;
    var Left, Top: integer): boolean;

  Returns the Left, Top, relative to the client origin of its parent.
------------------------------------------------------------------------------}
function GetWindowRelativePosition(Handle: hwnd;
  var Left, Top: integer): boolean;
begin
  Result := WidgetSet.GetWindowRelativePosition(Handle,Left,Top);
end;

{------------------------------------------------------------------------------
  function IntfSendsUTF8KeyPress: Boolean;

  Returns true if interface sends UTF8 KeyPress notifications.
------------------------------------------------------------------------------}
function IntfSendsUTF8KeyPress: Boolean;
begin
  Result := WidgetSet.IntfSendsUTF8KeyPress{%H-};
end;

{------------------------------------------------------------------------------
  function InvalidateFrame(aHandle: HWND; ARect: pRect; bErase: Boolean;
    BorderWidth: integer): Boolean;

  Calls InvalidateRect for the borderframe.
------------------------------------------------------------------------------}
function InvalidateFrame(aHandle: HWND; ARect: pRect; bErase: Boolean; BorderWidth: integer): Boolean;
begin
  Result := WidgetSet.InvalidateFrame(aHandle,ARect,bErase,BorderWidth);
end;

function IsDesignerDC(WindowHandle: HWND; DC: HDC): Boolean;
begin
  Result := WidgetSet.IsDesignerDC(WindowHandle, DC);
end;

// Indicates of we have fullscreen-only forms like in a mobile platform
function IsMobilePlatform: Boolean;
begin
  Result := WidgetSet.IsMobilePlatform();
end;

function IsCDIntfControl(AWinControl: TObject): Boolean;
begin
  Result := WidgetSet.IsCDIntfControl(AWinControl);
end;

function MoveWindowOrgEx(dc: hdc; dX,dY: Integer): boolean;
begin
  Result := WidgetSet.MoveWindowOrgEx(DC, dX, dY);
end;

function PromptUser(const DialogMessage: String; DialogType: longint; Buttons: PLongint;
  ButtonCount, DefaultIndex, EscapeResult: Longint): Longint;
begin
  Result := WidgetSet.PromptUser('', DialogMessage, DialogType, Buttons, ButtonCount, DefaultIndex, EscapeResult);
end;

function PromptUser(const DialogCaption, DialogMessage: String; DialogType: longint; Buttons: PLongint;
  ButtonCount, DefaultIndex, EscapeResult: Longint): Longint;
begin
  Result := WidgetSet.PromptUser(DialogCaption, DialogMessage, DialogType, Buttons, ButtonCount, DefaultIndex, EscapeResult);
end;

function PromptUserAtXY(const DialogMessage: String; DialogType: longint; Buttons: PLongint;
  ButtonCount, DefaultIndex, EscapeResult: Longint; X, Y: Longint): Longint;
begin
  Result := WidgetSet.PromptUserAtXY('', DialogMessage, DialogType, Buttons, ButtonCount, DefaultIndex, EscapeResult, X, Y);
end;

function PromptUserAtXY(const DialogCaption, DialogMessage: String; DialogType: longint; Buttons: PLongint;
  ButtonCount, DefaultIndex, EscapeResult: Longint; X, Y: Longint): Longint;
begin
  Result := WidgetSet.PromptUserAtXY(DialogCaption, DialogMessage, DialogType, Buttons, ButtonCount, DefaultIndex, EscapeResult, X, Y);
end;

function RadialChord(DC: HDC; x1, y1, x2, y2, sx, sy, ex, ey: Integer): Boolean;
begin
  Result := WidgetSet.RadialChord(DC, x1, y1, x2, y2, sx, sy, ex, ey);
end;

function RadialArc(DC: HDC; left, top, right, bottom, sx, sy, ex, ey: Integer): Boolean;
begin
  Result := WidgetSet.RadialArc(DC, left, top, right, bottom, sx, sy, ex, ey);
end;

function RadialPie(DC: HDC; x1, y1, x2, y2,
  Angle1,Angle2: Integer): Boolean;
begin
  Result := WidgetSet.RadialPie(DC, x1, y1, x2, y2, Angle1, Angle2);
end;

function RegroupMenuItem(hndMenu: HMENU; GroupIndex: integer): Boolean;
begin
  Result := WidgetSet.RegroupMenuItem(hndMenu,GroupIndex);
end;

function ReleaseDesignerDC(hWnd: HWND; DC: HDC): Integer;
begin
  Result := WidgetSet.ReleaseDesignerDC(hWnd, DC);
end;

procedure RemoveEventHandler(var AHandler: PEventHandler);
begin
  WidgetSet.RemoveEventHandler(AHandler);
end;

procedure RemoveProcessEventHandler(var AHandler: PProcessEventHandler);
begin
  WidgetSet.RemoveProcessEventHandler(AHandler);
end;

procedure RemovePipeEventHandler(var AHandler: PPipeEventHandler);
begin
  WidgetSet.RemovePipeEventHandler(AHandler);
end;

function RequestInput(const InputCaption, InputPrompt: String; MaskInput: Boolean; var Value: String): Boolean;
begin
  Result := WidgetSet.RequestInput(InputCaption, InputPrompt, MaskInput, Value);
end;

procedure SendCachedLCLMessages;
begin
end;

function SetCaretRespondToFocus(Handle: hWnd; ShowHideOnFocus: boolean):Boolean;
begin
  Result := WidgetSet.SetCaretRespondToFocus(Handle,ShowHideOnFocus);
end;

function SetComboMinDropDownSize(Handle: HWND;
  MinItemsWidth, MinItemsHeight, MinItemCount: integer): boolean;
begin
  Result := WidgetSet.SetComboMinDropDownSize(Handle,
                                   MinItemsWidth, MinItemsHeight, MinItemCount);
end;

procedure SetEventHandlerFlags(AHandler: PEventHandler; NewFlags: dword);
begin
  WidgetSet.SetEventHandlerFlags(AHandler, NewFlags);
end;

procedure SetRubberBandRect(const ARubberBand: HWND; const ARect: TRect);
begin
  WidgetSet.SetRubberBandRect(ARubberBand, ARect);
end;

function ShowSelectItemDialog(const AItems: TStrings; APos: TPoint): Boolean;
begin
  Result := Widgetset.ShowSelectItemDialog(AItems, APos);
end;

function StretchMaskBlt(DestDC: HDC; X, Y, Width, Height: Integer;
  SrcDC: HDC; XSrc, YSrc, SrcWidth, SrcHeight: Integer;
  Mask: HBITMAP; XMask, YMask: Integer; Rop: DWORD): Boolean;
begin
  Result := WidgetSet.StretchMaskBlt(DestDC, X, Y, Width, Height,
               SrcDC, XSrc, YSrc, SrcWidth, SrcHeight, Mask, XMask, YMask, Rop);
end;

//##apiwiz##eps##   // Do not remove

{******************************************************************************
  Platform independent stuff
 ******************************************************************************}

//##apiwiz##spi##   // Do not remove


{------------------------------------------------------------------------------
  Function: ExcludeClipRgn
  Params:  dc, RGN
  Returns: integer

  Subtracts all intersecting points of the passed Region from the Current
  clipping region in the device context (dc).

  The result can be one of the following constants
      Error
      NullRegion
      SimpleRegion
      ComplexRegion

 ------------------------------------------------------------------------------}
function ExcludeClipRgn(dc: hdc; rgn: hrgn): Integer;
begin
  Result := ExtSelectClipRGN(DC, RGN, RGN_DIFF);
end;

procedure NotifyUser(const DialogMessage: String; DialogType: longint);
begin
  PromptUser(DialogMessage, DialogType, [idButtonOK], 0, -1);
end;

procedure NotifyUser(const DialogCaption, DialogMessage: String; DialogType: longint);
begin
  PromptUser(DialogCaption, DialogMessage, DialogType, [idButtonOK], 0, -1);
end;

procedure NotifyUserAtXY(const DialogMessage: String; DialogType: longint; X, Y: Longint);
begin
  PromptUserAtXY(DialogMessage, DialogType, [idButtonOK], 0, -1, X, Y);
end;

procedure NotifyUserAtXY(const DialogCaption, DialogMessage: String; DialogType: longint; X, Y: Longint);
begin
  PromptUserAtXY(DialogCaption, DialogMessage, DialogType, [idButtonOK], 0, -1, X, Y);
end;

function PromptUser(const DialogMessage: String; DialogType: longint; Buttons: Array of Longint;
  DefaultIndex, EscapeResult: Longint): Longint;
begin
  Result := PromptUser(DialogMessage, DialogType, @Buttons[Low(Buttons)],
    High(Buttons) - Low(Buttons) + 1, DefaultIndex, EscapeResult);
end;

function PromptUser(const DialogCaption, DialogMessage: String; DialogType: longint; Buttons: Array of Longint;
  DefaultIndex, EscapeResult: Longint): Longint;
begin
  Result := PromptUser(DialogCaption, DialogMessage, DialogType, @Buttons[Low(Buttons)],
    High(Buttons) - Low(Buttons) + 1, DefaultIndex, EscapeResult);
end;

function PromptUserAtXY(const DialogMessage: String; DialogType: longint; Buttons: Array of Longint;
  DefaultIndex, EscapeResult: Longint; X, Y: Longint): Longint;
begin
  Result := PromptUserAtXY(DialogMessage, DialogType, @Buttons[Low(Buttons)],
    High(Buttons) - Low(Buttons) + 1, DefaultIndex, EscapeResult, X, Y);
end;

function PromptUserAtXY(const DialogCaption, DialogMessage: String; DialogType: longint; Buttons: Array of Longint;
  DefaultIndex, EscapeResult: Longint; X, Y: Longint): Longint;
begin
  Result := PromptUserAtXY(DialogCaption, DialogMessage, DialogType, @Buttons[Low(Buttons)],
    High(Buttons) - Low(Buttons) + 1, DefaultIndex, EscapeResult, X, Y);
end;

//##apiwiz##epi##   // Do not remove
