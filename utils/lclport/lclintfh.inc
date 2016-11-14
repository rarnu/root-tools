// This include file is used by two units: interfacebase.pp and lclintf.pas
// Do not add a %MainUnit directive here.
(******************************************************************************
  All interface communication related stuff goes here.
  This file is used by both LCLLinux and InterfaceBase
  If a procedure is platform dependent then it should have the following tail:
    {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
  If a procedure isn't platform dependent, it is no part of InterfaseBase and
  should be surrounded by:
    {$IFNDEF IF_BASE_MEMBER}
      procedure MyProc;
    {$ENDIF}

  !! Keep this alphabetical and place a pointer to the independent part
  !! if needed

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

 ******************************************************************************)

{******************************************************************************
  Platform specific stuff

  Note:
    the IFNDEF IF_BASE_MEMBER section is at the end
 ******************************************************************************}
//##apiwiz##sps##    // Do not remove

function AddEventHandler(AHandle: THandle; AFlags: dword; AEventHandler: TWaitHandleEvent; AData: PtrInt): PEventHandler; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function AddProcessEventHandler(AHandle: THandle; AEventHandler: TChildExitEvent; AData: PtrInt): PProcessEventHandler; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function AddPipeEventHandler(AHandle: THandle; AEventHandler: TPipeEvent; AData: PtrInt): PPipeEventHandler; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function AllocateHWnd(Method: TLCLWndMethod): HWND; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function AskUser(const DialogCaption, DialogMessage: string; DialogType: LongInt; Buttons: TDialogButtons; HelpCtx: Longint): LongInt; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}

procedure CallDefaultWndHandler(Sender: TObject; var Message); {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function ClipboardFormatToMimeType(FormatID: TClipboardFormat): string; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function ClipboardGetData(ClipboardType: TClipboardType;
  FormatID: TClipboardFormat; Stream: TStream): boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
// ! ClipboardGetFormats: List will be created. You must free it yourself with FreeMem(List) !
function ClipboardGetFormats(ClipboardType: TClipboardType;
  var Count: integer; var List: PClipboardFormat): boolean;  {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function ClipboardGetOwnerShip(ClipboardType: TClipboardType;
  OnRequestProc: TClipboardRequestEvent;  FormatCount: integer;
  Formats: PClipboardFormat): boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function ClipboardRegisterFormat(const AMimeType: string): TClipboardFormat; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function CreateEmptyRegion: hRGN; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function CreatePixmapIndirect(const Data: Pointer; const TransColor: Longint): HBITMAP; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function CreateRegionCopy(SrcRGN: hRGN): hRGN; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function CreateRubberBand(const ARect: TRect; const ABrush: HBrush = 0): HWND; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function CreateStandardCursor(ACursor: SmallInt): hCursor; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}

function DCClipRegionValid(DC: HDC): boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
procedure DeallocateHWnd(Wnd: HWND); {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
procedure DestroyRubberBand(ARubberBand: HWND); {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
procedure DrawDefaultDockImage(AOldRect, ANewRect: TRect; AOperation: TDockImageOperation); {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
procedure DrawGrid(DC: HDC; const R: TRect; DX, DY: Integer); {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}

function ExtUTF8Out(DC: HDC; X, Y: Integer; Options: Longint; Rect: PRect; Str: PChar; Count: Longint; Dx: PInteger): Boolean;  {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}

function FontCanUTF8(Font: HFont): boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF} deprecated;
function FontIsMonoSpace(Font: HFont): boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function Frame3d(DC: HDC; var ARect: TRect; const FrameWidth : integer; const Style : TGraphicsBevelCut): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}

function GetAcceleratorString(const AVKey: Byte; const AShiftState: TShiftState): String; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function GetAvailableNativeCanvasTypes(DC: HDC; AAllowFallbackToParent: Boolean = False): TNativeCanvasTypes; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function GetAvailableNativeHandleTypes(Handle: HWND; AAllowFallbackToParent: Boolean = False): TNativeHandleTypes; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function GetCaretRespondToFocus(handle: HWND; var ShowHideOnFocus: boolean): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function GetClientBounds(handle : HWND; var ARect: TRect) : Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function GetCmdLineParamDescForInterface: string; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function GetControlConstraints(Constraints: TObject): boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function GetDCOriginRelativeToWindow(PaintDC: HDC; WindowHandle: HWND; var OriginDiff: TPoint): boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function GetDesignerDC(WindowHandle: HWND): HDC; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function GetDeviceSize(DC: HDC; var p: TPoint): boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function GetLCLOwnerObject(Handle: HWnd): TObject; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function GetNativeCanvas(DC: HDC; AHandleType: TNativeCanvasType; AAllowFallbackToParent: Boolean = False): PtrInt; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function GetNativeHandle(Handle: HWND; AHandleType: TNativeHandleType; AAllowFallbackToParent: Boolean = False): PtrInt; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function GetScrollBarSize(Handle: HWND; SBStyle: Integer): integer; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function GetScrollbarVisible(Handle: HWND; SBStyle: Integer): boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function GetWindowRelativePosition(Handle : hwnd; var Left, Top: integer): boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}

function IntfSendsUTF8KeyPress: Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF} deprecated;
function InvalidateFrame(aHandle : HWND; ARect : pRect; bErase : Boolean; BorderWidth: integer) : Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function IsDesignerDC(WindowHandle: HWND; DC: HDC): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function IsMobilePlatform: Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function IsCDIntfControl(AWinControl: TObject): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}

function MoveWindowOrgEx(dc : hdc; dX,dY : Integer): boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}

function PromptUser(const DialogCaption, DialogMessage : String; DialogType : longint; Buttons : PLongint; ButtonCount, DefaultIndex, EscapeResult : Longint) : Longint;{$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function PromptUserAtXY(const DialogCaption, DialogMessage : String; DialogType : longint; Buttons : PLongint; ButtonCount, DefaultIndex, EscapeResult : Longint; X, Y : Longint) : Longint;{$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}

function RadialArc(DC: HDC; left, top, right, bottom, sx, sy, ex, ey : Integer): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function RadialChord(DC: HDC; x1, y1, x2, y2, sx, sy, ex, ey : Integer): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function RadialPie(DC: HDC; x1, y1, x2, y2, Angle1, Angle2: Integer): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function RawImage_CreateBitmaps(const ARawImage: TRawImage; out ABitmap, AMask: HBitmap; ASkipMask: Boolean = False): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function RawImage_DescriptionFromBitmap(ABitmap: HBITMAP; out ADesc: TRawImageDescription): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function RawImage_DescriptionFromDevice(ADC: HDC; out ADesc: TRawImageDescription): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function RawImage_FromBitmap(out ARawImage: TRawImage; ABitmap, AMask: HBITMAP; ARect: PRect = nil): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function RawImage_FromDevice(out ARawImage: TRawImage; ADC: HDC; const ARect: TRect): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function RawImage_QueryDescription(AFlags: TRawImageQueryFlags; var ADesc: TRawImageDescription): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function RegroupMenuItem(hndMenu: HMENU; GroupIndex: integer): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function ReleaseDesignerDC(hWnd: HWND; DC: HDC): Integer;  {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
procedure RemoveEventHandler(var AHandler: PEventHandler); {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
procedure RemoveProcessEventHandler(var AHandler: PProcessEventHandler); {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
procedure RemovePipeEventHandler(var AHandler: PPipeEventHandler); {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function RequestInput(const InputCaption, InputPrompt : String; MaskInput : Boolean; var Value : String) : Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}

procedure SendCachedLCLMessages; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function SetCaretRespondToFocus(handle: HWND; ShowHideOnFocus: boolean): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function SetComboMinDropDownSize(Handle: HWND; MinItemsWidth, MinItemsHeight, MinItemCount: integer): boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
procedure SetEventHandlerFlags(AHandler: PEventHandler; NewFlags: dword); {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
procedure SetRubberBandRect(const ARubberBand: HWND; const ARect: TRect); {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function ShowSelectItemDialog(const AItems: TStrings; APos: TPoint): Boolean;{$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function StretchMaskBlt(DestDC: HDC; X, Y, Width, Height: Integer; SrcDC: HDC; XSrc, YSrc, SrcWidth, SrcHeight: Integer; Mask: HBITMAP; XMask, YMask: Integer; Rop: DWORD): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}

function TextUTF8Out(DC: HDC; X, Y: Integer;  Str: PChar; Count: Longint): Boolean;  {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}

//##apiwiz##eps##    // Do not remove

{******************************************************************************
  Platform independent stuff
 ******************************************************************************}

{$IFNDEF IF_BASE_MEMBER}

//##apiwiz##spi##    // Do not remove

function ExcludeClipRgn(dc: hdc; rgn : hrgn) : Integer;


procedure NotifyUser(const DialogMessage : String; DialogType : longint);
procedure NotifyUser(const DialogCaption, DialogMessage : String; DialogType : longint);
procedure NotifyUserAtXY(const DialogMessage : String; DialogType : longint; X, Y : Longint);
procedure NotifyUserAtXY(const DialogCaption, DialogMessage : String; DialogType : longint; X, Y : Longint);

function PromptUser(const DialogMessage : String; DialogType : longint; Buttons : Array of Longint; DefaultIndex, EscapeResult : Longint) : Longint;
function PromptUser(const DialogMessage : String; DialogType : longint; Buttons : PLongint; ButtonCount, DefaultIndex, EscapeResult : Longint) : Longint;
function PromptUser(const DialogCaption, DialogMessage : String; DialogType : longint; Buttons : Array of Longint; DefaultIndex, EscapeResult : Longint) : Longint;
function PromptUserAtXY(const DialogMessage : String; DialogType : longint; Buttons : Array of Longint; DefaultIndex, EscapeResult : Longint; X, Y : Longint) : Longint;
function PromptUserAtXY(const DialogMessage : String; DialogType : longint; Buttons : PLongint; ButtonCount, DefaultIndex, EscapeResult : Longint; X, Y : Longint) : Longint;
function PromptUserAtXY(const DialogCaption, DialogMessage : String; DialogType : longint; Buttons : Array of Longint; DefaultIndex, EscapeResult : Longint; X, Y : Longint) : Longint;

//##apiwiz##epi##    // Do not remove

{$ENDIF}
