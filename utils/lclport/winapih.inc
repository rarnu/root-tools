{%MainUnit ../lclintf.pas}
// included by interfacebase.pp and lclintf.pp

(******************************************************************************
  All Winapi related stuff goes here.
  This file is used by both LCLIntf.pas and InterfaceBase.pp
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
//##apiwiz##sps##   // Do not remove

function Arc(DC: HDC; Left,Top,Right,Bottom,Angle16Deg, Angle16DegLength: Integer): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function AngleChord(DC: HDC; x1,y1,x2,y2,angle1,angle2 : Integer): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}

function BeginPaint(Handle : hwnd; var PS : TPaintStruct): hdc; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function BitBlt(DestDC: HDC; X, Y, Width, Height: Integer; SrcDC: HDC; XSrc, YSrc: Integer; Rop: DWORD): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}

function CallNextHookEx(hhk : HHOOK; ncode : Integer; WParam: WParam; LParam: LParam) : Integer; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function CallWindowProc(lpPrevWndFunc : TFarProc; Handle : HWND; Msg : UINT; WParam: WParam; LParam: LParam): Integer; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
//function CharLower --> independent
//function CharLowerBuff --> independent
//function CharUpper --> independent
function ClientToScreen(Handle : HWND; var P : TPoint) : Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function CombineRgn(Dest, Src1, Src2 : HRGN; fnCombineMode : Longint) : Longint; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function CreateBitmap(Width, Height: Integer; Planes, BitCount: Longint; BitmapBits: Pointer): HBITMAP; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function CreateBrushIndirect(const LogBrush: TLogBrush): HBRUSH; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function CreateBrushWithRadialGradient(const LogBrush: TLogRadialGradient): HBRUSH; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function CreateCaret(Handle: HWND; Bitmap: hBitmap; width, Height: Integer): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function CreateCompatibleBitmap(DC: HDC; Width, Height: Integer): HBITMAP; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function CreateCompatibleDC(DC: HDC): HDC; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function CreateDIBitmap(DC: HDC; var InfoHeader: TBitmapInfoHeader;
  dwUsage: DWORD; InitBits: PChar; var InitInfo: TBitmapInfo;
  wUsage: UINT): HBITMAP; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function CreateDIBSection(DC: HDC; const BitmapInfo: tagBitmapInfo; Usage: UINT;
  var Bits: Pointer; SectionHandle: THandle; Offset: DWORD): HBITMAP; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function CreateEllipticRgn(X1, Y1, X2, Y2: Integer): HRGN; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
//function CreateEllipticRgnIndirect --> independent
//function CreateFont --> independent
function CreateFontIndirect(const LogFont: TLogFont): HFONT; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function CreateFontIndirectEx(const LogFont: TLogFont; const LongFontName: string): HFONT; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function CreateIconIndirect(IconInfo: PIconInfo): HICON; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function CreatePalette(const LogPalette: TLogPalette): HPalette; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function CreatePatternBrush(ABitmap: HBITMAP): HBRUSH; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
//function CreatePen --> independent
function CreatePenIndirect(const LogPen: TLogPen): HPEN; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function CreatePolygonRgn(Points: PPoint; NumPts: Integer; FillMode: integer): HRGN; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function CreateRectRgn(X1,Y1,X2,Y2 : Integer): HRGN; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
//function CreateRectRgnIndirect --> independent
function CreateRoundRectRgn(X1, Y1, X2, Y2, nWidthEllipse, nHeightEllipse: Integer): HRGN; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}

procedure DeleteCriticalSection(var CritSection: TCriticalSection); {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function DeleteDC(hDC: HDC): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function DeleteObject(GDIObject: HGDIOBJ): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function DestroyCaret(Handle : HWND): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function DestroyCursor(Handle: HCURSOR): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function DestroyIcon(Handle: HICON): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}

function DPtoLP(DC: HDC; var Points; Count: Integer): BOOL; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function DrawFrameControl(DC: HDC; const Rect : TRect; uType, uState : Cardinal) : Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function DrawFocusRect(DC: HDC; const Rect: TRect): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function DrawEdge(DC: HDC; var Rect: TRect; edge: Cardinal; grfFlags: Cardinal): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function DrawText(DC: HDC; Str: PChar; Count: Integer; var Rect: TRect; Flags: Cardinal): Integer; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}

function EnableScrollBar(Wnd: HWND; wSBflags, wArrows: Cardinal): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function EnableWindow(hWnd: HWND; bEnable: Boolean): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function EndPaint(Handle : hwnd; var PS : TPaintStruct): Integer; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
procedure EnterCriticalSection(var CritSection: TCriticalSection); {$IFDEF IF_BASE_MEMBER}virtual;{$ELSE}overload;{$ENDIF}
function EnumDisplayMonitors(hdc: HDC; lprcClip: PRect; lpfnEnum: MonitorEnumProc; dwData: LPARAM): LongBool; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function EnumFontFamilies(DC: HDC; Family: Pchar; EnumFontFamProc: FontEnumProc; LParam:Lparam):longint; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function EnumFontFamiliesEx(DC: HDC; lpLogFont:PLogFont; Callback: FontEnumExProc; Lparam:LParam; Flags: dword): longint; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}

function Ellipse(DC: HDC; x1, y1, x2, y2: Integer): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
//function EqualRect --> independent
function EqualRgn(Rgn1: HRGN; Rgn2: HRGN): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function ExcludeClipRect(dc: hdc; Left, Top, Right, Bottom : Integer) : Integer; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function ExtCreatePen(dwPenStyle, dwWidth: DWord; const lplb: TLogBrush; dwStyleCount: DWord; lpStyle: PDWord): HPEN; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function ExtTextOut(DC: HDC; X, Y: Integer; Options: Longint; Rect: PRect; Str: PChar; Count: Longint; Dx: PInteger): Boolean;  {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function ExtSelectClipRGN(dc: hdc; rgn : hrgn; Mode : Longint) : Integer;  {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}

function FillRect(DC: HDC; const Rect: TRect; Brush: HBRUSH): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function FillRgn(DC: HDC; RegionHnd: HRGN; hbr: HBRUSH): BOOL; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function FloodFill(DC: HDC; X, Y: Integer;
                   Color: TGraphicsColor;
                   FillStyle: TGraphicsFillStyle;
                   Brush: HBRUSH): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function FrameRect(DC: HDC; const ARect: TRect; hBr: HBRUSH): Integer; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}

function GetActiveWindow : HWND; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function GetBitmapBits(Bitmap: HBITMAP; Count: Longint;  Bits: Pointer): Longint; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function GetBkColor(DC: HDC): TColorRef; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function GetCapture : HWND; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function GetCaretPos(var lpPoint: TPoint): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function GetClientRect(handle : HWND; var Rect: TRect) : Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function GetClipBox(DC : hDC; lpRect : PRect) : Longint; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function GetClipRGN(DC : hDC; RGN : hRGN) : Longint; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function GetCurrentObject(DC: HDC; uObjectType: UINT): HGDIOBJ; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function GetCursorPos(var lpPoint: TPoint): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function GetCharABCWidths(DC: HDC; p2, p3: UINT; const ABCStructs): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF} //pbd
function GetDC(hWnd: HWND): HDC; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function GetDeviceCaps(DC: HDC; Index: Integer): Integer; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function GetDIBits(DC: HDC; Bitmap: HBitmap; StartScan, NumScans: UINT; Bits: Pointer; var BitInfo: BitmapInfo; Usage: UINT): Integer;{$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function GetDoubleClickTime: UINT; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function GetFocus: HWND; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function GetFontLanguageInfo(DC: HDC): DWord; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function GetForegroundWindow: HWND; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function GetIconInfo(AIcon: HICON; AIconInfo: PIconInfo): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function GetKeyState(nVirtKey: Integer): Smallint; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function GetMapMode(DC: HDC): Integer; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function GetMonitorInfo(hMonitor: HMONITOR; lpmi: PMonitorInfo): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function GetObject(GDIObject: HGDIOBJ; BufSize: Integer; Buf: Pointer): Integer; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function GetPaletteEntries(Palette: HPALETTE; StartIndex, NumEntries: UINT;
  var PaletteEntries): UINT; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function GetParent(Handle : HWND): HWND; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function GetProp(Handle : hwnd; Str : PChar): Pointer;{$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function GetRgnBox(RGN : HRGN; lpRect : PRect) : Longint; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function GetROP2(DC: HDC): integer; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function GetScrollInfo(Handle: HWND; SBStyle: Integer; var ScrollInfo: TScrollInfo): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
//function GetScrollPos --> independent
//function GetScrollRange --> independent
function GetStockObject(Value: Integer): THandle; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function GetSysColor(nIndex: Integer): DWORD; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function GetSysColorBrush(nIndex: Integer): HBrush; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function GetSystemPaletteEntries(DC: HDC; StartIndex, NumEntries: UINT;
  var PaletteEntries): UINT; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function GetSystemMetrics(nIndex: Integer): Integer;  {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function GetTextColor(DC: HDC) : TColorRef; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function GetTextExtentExPoint(DC: HDC; Str: PChar; Count, MaxWidth: Integer; MaxCount, PartialWidths: PInteger; var Size: TSize): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function GetTextExtentPoint(DC: HDC; Str: PChar; Count: Integer; var Size: TSize): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function GetTextExtentPoint32(DC: HDC; Str: PChar; Count: Integer; var Size: TSize): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function GetTextMetrics(DC: HDC; var TM: TTextMetric): Boolean;  {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function GetViewPortExtEx(DC: HDC; Size: PSize): Integer; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function GetViewPortOrgEx(DC: HDC; P: PPoint): Integer; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function GetWindowExtEx(DC: HDC; Size: PSize): Integer; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function GetWindowLong(Handle : hwnd; int : Integer): PtrInt; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function GetWindowRect(Handle : hwnd; var Rect : TRect): Integer; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function GetWindowSize(Handle : hwnd; var Width, Height: integer): boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function GetWindowOrgEx(dc : hdc; var P: TPoint): Integer; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF} // because of delphi compatibility
function GetWindowOrgEx(dc : hdc; P : PPoint): Integer; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF} // an old declaration
function GradientFill(DC: HDC; Vertices: PTriVertex; NumVertices : Longint;
  Meshes: Pointer; NumMeshes : Longint; Mode : Longint): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}

function HideCaret(hWnd: HWND): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}

//function IndexToOverlayMask --> independent
//function InflateRect --> independent
procedure InitializeCriticalSection(var CritSection: TCriticalSection); {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function IntersectClipRect(dc: hdc; Left, Top, Right,Bottom: Integer): Integer; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
//function IntersectRect --> independent
function InvalidateRect(aHandle : HWND; ARect : pRect; bErase : Boolean) : Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function InvalidateRgn(Handle: HWND; Rgn: HRGN; Erase: Boolean): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
//function IsCharAlphaNumeric --> independent
function IsDBCSLeadByte(TestChar: Byte): boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
//function IsRectEmpty --> independent
function IsIconic(handle: HWND): boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function IsWindow(handle: HWND): boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function IsWindowEnabled(handle: HWND): boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function IsWindowVisible(handle: HWND): boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function IsZoomed(handle: HWND): boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}

//function KillTimer --> independent

procedure LeaveCriticalSection(var CritSection: TCriticalSection); {$IFDEF IF_BASE_MEMBER}virtual;{$ELSE}overload;{$ENDIF}
function LineTo(DC: HDC; X, Y: Integer): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function LoadBitmap(hInstance: THandle; lpBitmapName: PChar): HBitmap; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function LoadCursor(hInstance: THandle; lpCursorName: PChar): HCursor; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function LoadIcon(hInstance: THandle; lpIconName: PChar): HIcon; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function LPtoDP(DC: HDC; var Points; Count: Integer): BOOL; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}

//function MakeLong --> independent
//function MakeROP4 --> independent
//function MakeWord --> independent
function MaskBlt(DestDC: HDC; X, Y, Width, Height: Integer; SrcDC: HDC; XSrc, YSrc: Integer; Mask: HBITMAP; XMask, YMask: Integer; Rop: DWORD): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function MaskBlt(DestDC: HDC; X, Y, Width, Height: Integer; SrcDC: HDC; XSrc, YSrc: Integer; Mask: HBITMAP; XMask, YMask: Integer): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function MessageBox(hWnd: HWND; lpText, lpCaption: PChar; uType: Cardinal = MB_OK): integer; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function MonitorFromPoint(ptScreenCoords: TPoint; dwFlags: DWord): HMONITOR; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function MonitorFromRect(lprcScreenCoords: PRect; dwFlags: DWord): HMONITOR; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function MonitorFromWindow(hWnd: HWND; dwFlags: DWord): HMONITOR; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}

function MoveToEx(DC: HDC; X, Y: Integer; OldPoint: PPoint): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}

//function OffsetRect --> independent
function OffsetRgn(RGN: HRGN; nXOffset, nYOffset: Integer): Integer; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}

function PaintRgn(DC: HDC; RGN: HRGN): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function PeekMessage(var lpMsg : TMsg; Handle : HWND; wMsgFilterMin, wMsgFilterMax,wRemoveMsg : UINT): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function Pie(DC: HDC; x1, y1, x2, y2, sx, sy, ex, ey: Integer): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function PolyBezier(DC: HDC; Points: PPoint;  NumPts: Integer; Filled, Continuous: boolean): boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function Polygon(DC: HDC; Points: PPoint;  NumPts: Integer; Winding: boolean): boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function Polyline(DC: HDC; Points: PPoint; NumPts: Integer): boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function PostMessage(Handle: HWND; Msg: Cardinal; WParam: WParam; LParam: LParam): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function PtInRegion(RGN: HRGN; X, Y: Integer): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
//function PtInRect --> independent

function RealizePalette(DC: HDC): Cardinal; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function Rectangle(DC: HDC; X1, Y1, X2, Y2: Integer): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function RectInRegion(RGN: HRGN; ARect: TRect): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function RectVisible(DC: HDC; const ARect: TRect): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function RedrawWindow(Wnd: HWND; lprcUpdate: PRECT; hrgnUpdate: HRGN; flags: UINT): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function ReleaseCapture : Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function ReleaseDC(hWnd: HWND; DC: HDC): Integer; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function RemoveProp(Handle: hwnd; Str: PChar): THandle; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function RestoreDC(DC: HDC; SavedDC: Integer): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function RoundRect(DC : hDC; X1, Y1, X2, Y2: Integer; RX,RY : Integer): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}

function SaveDC(DC: HDC): Integer; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function ScreenToClient(Handle : HWND; var P : TPoint) : Integer; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
//function ScrollWindow --> independent deprecated calls ScrollWindowEx
function ScrollWindowEx(hWnd: HWND; dx, dy: Integer; prcScroll, prcClip: PRect; hrgnUpdate: HRGN; prcUpdate: PRect; flags: UINT): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function SelectClipRGN(DC : hDC; RGN : HRGN) : Longint; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function SelectObject(DC: HDC; GDIObj: HGDIOBJ): HGDIOBJ; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function SelectPalette(DC: HDC; Palette: HPALETTE; ForceBackground: Boolean): HPALETTE; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function SendMessage(HandleWnd: HWND; Msg: Cardinal; WParam: WParam; LParam: LParam): LResult; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function SetActiveWindow(Handle: HWND): HWND;  {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function SetBkColor(DC: HDC; Color: TColorRef): TColorRef; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF} //pbd
function SetBkMode(DC: HDC; bkMode : Integer): Integer; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function SetCapture (AHandle: HWND): HWND; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function SetCaretPos(X, Y: Integer): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function SetCaretPosEx(handle: HWnd; X, Y: Integer): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function SetCursor(hCursor: HICON): HCURSOR; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function SetCursorPos(X, Y: Integer): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function SetFocus(hWnd: HWND): HWND; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function SetForegroundWindow(hWnd : HWND): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function SetMapMode(DC: HDC; fnMapMode : Integer): Integer; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function SetMenu(AWindowHandle: HWND; AMenuHandle: HMENU): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function SetParent(hWndChild: HWND; hWndParent: HWND): HWND; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function SetProp(Handle: hwnd; Str : PChar; Data : Pointer) : Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
//function SetRect --> independent
function SetRectRgn(aRGN: HRGN; X1, Y1, X2, Y2 : Integer): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
//function SetRectEmpty --> independent
function SetROP2(DC: HDC; Mode: Integer): Integer; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function SetScrollInfo(Handle: HWND; SBStyle: Integer; ScrollInfo: TScrollInfo; Redraw : Boolean): Integer; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
//function SetScrollPos --> independent
//function SetScrollRange --> independent
function SetStretchBltMode(DC: HDC; StretchMode: Integer): Integer; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function SetSysColors(cElements: Integer; const lpaElements; const lpaRgbValues): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function SetTextCharacterExtra(_hdc : hdc; nCharExtra : Integer):Integer; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function SetTextColor(DC: HDC; Color: TColorRef): TColorRef; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
//function SetTimer --> independent
function SetWindowLong(Handle: HWND; Idx: Integer; NewLong : PtrInt): PtrInt;{$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function SetViewPortExtEx(DC: HDC; XExtent, YExtent : Integer; OldSize: PSize): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function SetViewPortOrgEx(DC: HDC; NewX, NewY: Integer; OldPoint: PPoint): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function SetWindowExtEx(DC: HDC; XExtent, YExtent: Integer; OldSize: PSize): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function SetWindowOrgEx(dc : hdc; NewX, NewY: Integer; OldPoint: PPoint): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function SetWindowPos(hWnd: HWND; hWndInsertAfter: HWND;
       X, Y, cx, cy: Integer; uFlags: UINT): Boolean;  {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function SetWindowRgn(hWnd: HWND; hRgn: HRGN; bRedraw: Boolean): longint; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function ShowCaret(hWnd: HWND): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function ShowScrollBar(Handle: HWND; wBar: Integer; bShow: Boolean): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function ShowWindow(hWnd: HWND; nCmdShow: Integer): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function StretchBlt(DestDC: HDC; X, Y, Width, Height: Integer; SrcDC: HDC; XSrc, YSrc, SrcWidth, SrcHeight: Integer; Rop: Cardinal): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function StretchDIBits(DC: HDC; DestX, DestY, DestWidth, DestHeight, SrcX,
  SrcY, SrcWidth, SrcHeight: Integer; Bits: Pointer; var BitsInfo: TBitmapInfo;
  Usage: UINT; Rop: DWORD): Integer; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
//function SubtractRect --> independent
function SystemParametersInfo(uiAction: DWord; uiParam: DWord; pvParam: Pointer; fWinIni: DWord): LongBool; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}

function TextOut(DC: HDC; X,Y : Integer; Str : Pchar; Count: Integer) : Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}

function UpdateWindow(Handle: HWND): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}

//function UnionRect --> independent

function WindowFromPoint(Point : TPoint) : HWND; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
//##apiwiz##eps##     // Do not remove
{******************************************************************************
  Platform independent stuff
 ******************************************************************************}

{$IFNDEF IF_BASE_MEMBER}

//##apiwiz##spi##    // Do not remove

function AdjustWindowRectEx(Var Rect: TRect; Style1: Word; MenuExist : Boolean;
  Style2 : Word) : Boolean;

function CharLower(c: char): char; inline;
function CharLowerBuff(pStr : PChar; Len : Integer): Integer;
function CharUpper(c: char): char; inline;
function CharUpperBuff(pStr : PChar; Len : Integer): Integer;
function CopyRect(var DestRect: TRect; const SrcRect: TRect): Boolean;
function CreateEllipticRgnIndirect(const ARect: TRect): HRGN;
function CreateFont(Height, Width, Escapement, Orientation, Weight: Integer;
  Italic, Underline, StrikeOut, CharSet, OutputPrecision, ClipPrecision,
  Quality, PitchAndFamily: Byte; FaceName: PChar): HFONT;
function CreatePen(Style, Width: Integer; Color: TColorRef): HPEN;
function CreateRectRgnIndirect(const ARect: TRect): HRGN;
function CreateSolidBrush(Color: TColorRef): HBRUSH;

function EqualRect(const lprc1, lprc2: TRect): Boolean;

function GetScrollPos(Handle: HWND; nBar: Integer): Integer;
function GetScrollRange(Handle: HWND; nBar: Integer; var lpMinPos, lpMaxPos: Integer): Boolean;
function GetRValue(RGB : DWORD) : BYTE; inline;
function GetGValue(RGB : DWORD) : BYTE; inline;
function GetBValue(RGB : DWORD) : BYTE; inline;
procedure GetRGBValues(RGB : DWORD; out R, G, B: Byte); inline;
procedure GetRGBIntValues(RGB : DWORD; out R, G, B: integer); inline;

function IndexToOverlayMask(iOverlay: UINT): UINT; inline;
function InflateRect(var ARect: TRect; dx, dy: Integer): Boolean;
function IntersectRect(var DestRect: TRect; const SrcRect1, SrcRect2: TRect): Boolean; inline;
function IsCharAlphaNumeric(c : Char) : Boolean;
function IsRectEmpty(const ARect: TRect): Boolean;

function KillTimer(Wnd: HWND; uIDEvent: UINT_PTR): BOOL;

function MakeLong(A,B: Word): DWORD; inline;
function MakeROP4(fore, back: DWORD): DWORD;inline;
function MakeWord(A,B: Byte): Word; inline;
function MakeWParam(l, h: Word): WPARAM; inline;
function MakeLParam(l, h: Word): LPARAM; inline;
function MakeLResult(l, h: Word): LRESULT; inline;

function OffsetRect(var Rect: TRect; dx,dy: Integer): Boolean; inline;

function PtInRect(const Rect : TRect; const Point : TPoint) : Boolean; inline;
function PointToSmallPoint(const P : TPoint) : TSmallPoint; inline;

function RGB(R, G, B : Byte) : TColorRef; inline;

function ScrollWindow(hWnd: HWND; XAmount, YAmount: Integer; Rect, ClipRect: PRect): Boolean; inline;
function SetRect(var ARect : TRect; aLeft,aTop,aRight,aBottom : Integer) : Boolean;
function SetRectEmpty(var ARect: TRect): Boolean;
function SetScrollPos(Handle: HWND; nBar, nPos: Integer; bRedraw: Boolean): Integer;
function SetScrollRange(Handle: HWND; nBar, nMinPos, nMaxPos: Integer; bRedraw: Boolean): Boolean;
function SetTimer(Wnd: HWND; nIDEvent: UINT_PTR; uElapse: UINT;  lpTimerFunc: TTimerProc): UINT_PTR;
function SmallPointToPoint(const P : TSmallPoint) : Tpoint;
function SubtractRect(out lprcDst: TRect; const lprcSrc1, lprcSrc2: TRect): Boolean;

function UnionRect(var DestRect: TRect; const SrcRect1, SrcRect2: TRect): Boolean; //pbd


//##apiwiz##epi##    // Do not remove


{$ENDIF}


