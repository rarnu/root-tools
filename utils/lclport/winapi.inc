{%MainUnit ../lclintf.pas}

(******************************************************************************
  All Winapi related stuff goes here.
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
 ******************************************************************************)

{******************************************************************************
  These functions redirect to the platform specific interface object.

  Note:
    the section for not referring WidgetSet is at the end
 ******************************************************************************}
//##apiwiz##sps##   // Do not remove

function Arc(DC: HDC; Left, Top, Right, Bottom, Angle16Deg, Angle16DegLength: Integer): Boolean;
begin
  Result := WidgetSet.Arc(DC, Left, Top, Right, Bottom, Angle16Deg, Angle16DegLength);
end;

function AngleChord(DC: HDC; x1, y1, x2, y2, angle1, angle2: Integer): Boolean;
begin
  Result := WidgetSet.AngleChord(DC, x1, y1, x2, y2, angle1, angle2);
end;

function BeginPaint(Handle: hWnd; Var PS : TPaintStruct) : hdc;
begin
  Result := WidgetSet.BeginPaint(Handle,PS);
end;

function BitBlt(DestDC: HDC; X, Y, Width, Height: Integer; SrcDC: HDC; XSrc, YSrc: Integer; Rop: DWORD): Boolean;
begin
  Result := WidgetSet.BitBlt(DestDC, X, Y, Width, Height, SrcDC, XSrc, YSrc, Rop);
end;

function CallNextHookEx(hhk : HHOOK; ncode : Integer; WParam: WParam; LParam: LParam) : Integer;
begin
  Result := WidgetSet.CallNextHookEx(hhk, ncode, WParam, LParam);
end;

function CallWindowProc(lpPrevWndFunc :TFarProc; Handle : HWND; Msg : UINT; WParam: WParam; LParam: LParam): Integer;
begin
  Result := WidgetSet.CallWindowProc(lpPrevWndFunc, Handle, Msg, WParam, LParam);
end;

function ClientToScreen(Handle : HWND; var P : TPoint) : Boolean;
begin
  Result := WidgetSet.ClientToScreen(Handle, P);
end;

function CombineRgn(Dest, Src1, Src2 : HRGN; fnCombineMode : Longint) : Longint;
begin
  Result := WidgetSet.CombineRgn(Dest, Src1, Src2, fnCombineMode);
end;

function CreateBitmap(Width, Height: Integer; Planes, BitCount: Longint;
  BitmapBits: Pointer): HBITMAP;
begin
  Result := WidgetSet.CreateBitmap(Width, Height, Planes, BitCount,
                                         BitmapBits);
end;

function CreateDIBitmap(DC: HDC; var InfoHeader: TBitmapInfoHeader;
  dwUsage: DWORD; InitBits: PChar; var InitInfo: TBitmapInfo;
  wUsage: UINT): HBITMAP;
begin
  Result := WidgetSet.CreateDIBitmap(DC,InfoHeader,dwUsage,InitBits,
                                           InitInfo,wUsage);
end;

function CreateDIBSection(DC: HDC; const BitmapInfo: tagBitmapInfo; Usage: UINT;
  var Bits: Pointer; SectionHandle: THandle; Offset: DWORD): HBITMAP;
begin
  Result := WidgetSet.CreateDIBSection(DC,BitmapInfo,Usage,
                                             Bits,SectionHandle,Offset);
end;

function CreateBrushIndirect(const LogBrush: TLogBrush): HBRUSH;
begin
  Result := WidgetSet.CreateBrushIndirect(LogBrush);
end;

function CreateBrushWithRadialGradient(const LogBrush: TLogRadialGradient): HBRUSH;
begin
  Result := WidgetSet.CreateBrushWithRadialGradient(LogBrush);
end;

function CreateCaret(Handle : HWND; Bitmap : hBitmap; width, Height : Integer) : Boolean;
begin
  Result := WidgetSet.CreateCaret(Handle, Bitmap, width, Height);
end;

function CreateCompatibleBitmap(DC: HDC; Width, Height: Integer): HBITMAP;
begin
  Result := WidgetSet.CreateCompatibleBitmap(DC, Width, Height);
end;

function CreateCompatibleDC(DC: HDC): HDC;
begin
  Result := WidgetSet.CreateCompatibleDC(DC);
end;

function CreateFontIndirect(const LogFont: TLogFont): HFONT;
begin
  Result := WidgetSet.CreateFontIndirect(LogFont);
end;

function CreateFontIndirectEx(const LogFont: TLogFont;
  const LongFontName: string): HFONT;
begin
  Result := WidgetSet.CreateFontIndirectEx(LogFont, LongFontName);
end;

function CreateIconIndirect(IconInfo: PIconInfo): HICON;
begin
  Result := WidgetSet.CreateIconIndirect(IconInfo);
end;

function CreatePalette(const LogPalette: TLogPalette): HPalette;
begin
  Result := WidgetSet.CreatePalette(LogPalette);
end;

function CreatePatternBrush(ABitmap: HBITMAP): HBRUSH;
var
  LogBrush: TLogBrush;
begin
  LogBrush.lbStyle := BS_PATTERN;
  LogBrush.lbColor := 0;
  LogBrush.lbHatch := ABitmap;
  Result := WidgetSet.CreateBrushIndirect(LogBrush);
end;

function CreatePenIndirect(const LogPen: TLogPen): HPEN;
begin
  Result := WidgetSet.CreatePenIndirect(LogPen);
end;

function CreatePolygonRgn(Points: PPoint; NumPts: Integer; FillMode: integer): HRGN;
begin
  Result := WidgetSet.CreatePolygonRgn(Points, NumPts, FillMode);
end;

function CreateRectRgn(X1,Y1,X2,Y2 : Integer): HRGN;
begin
  Result := WidgetSet.CreateRectRgn(X1,Y1,X2,Y2);
end;

function CreateRoundRectRgn(X1, Y1, X2, Y2, nWidthEllipse, nHeightEllipse: Integer): HRGN;
begin
  Result := WidgetSet.CreateRoundRectRgn(X1, Y1, X2, Y2, nWidthEllipse, nHeightEllipse);
end;

procedure DeleteCriticalSection(var CritSection: TCriticalSection);
begin
  WidgetSet.DeleteCriticalSection(CritSection);
end;

function DeleteDC(hDC: HDC): Boolean;
begin
  Result := WidgetSet.DeleteDC(hDC);
end;

function DeleteObject(GDIObject: HGDIOBJ): Boolean;
begin
  Result := WidgetSet.DeleteObject(GDIObject);
end;

function DestroyCaret(Handle : HWND): Boolean;
begin
  Result := WidgetSet.DestroyCaret(Handle);
end;

function DestroyCursor(Handle: hCursor): Boolean;
begin
  Result := WidgetSet.DestroyCursor(Handle);
end;

function DestroyIcon(Handle: HICON): Boolean;
begin
  Result := WidgetSet.DestroyIcon(Handle);
end;

function DrawFrameControl(DC: HDC; const Rect : TRect; uType, uState : Cardinal) : Boolean;
begin
  Result := WidgetSet.DrawFrameControl(DC, Rect, uType, uState);
end;

function DrawFocusRect(DC: HDC; const Rect: TRect): boolean;
begin
  Result := WidgetSet.DrawFocusRect(DC, Rect);
end;

function DrawEdge(DC: HDC; var Rect: TRect; edge: Cardinal; grfFlags: Cardinal): Boolean;
begin
  Result := WidgetSet.DrawEdge(DC, Rect, edge, grfFlags);
end;

function DrawText(DC: HDC; Str: PChar; Count: Integer; var Rect: TRect; Flags: Cardinal): Integer;
begin
  Result := WidgetSet.DrawText(DC, Str, Count, Rect, Flags);
end;

function DPtoLP(DC: HDC; var Points; Count: Integer): BOOL;
begin
  Result := WidgetSet.DPtoLP(DC,Points,Count);
end;

function Ellipse(DC: HDC; x1,y1,x2,y2: Integer): Boolean;
begin
  Result := WidgetSet.Ellipse(DC,x1,y1,x2,y2);
end;

function EnableScrollBar(Wnd: HWND; wSBflags, wArrows: Cardinal): Boolean;
begin
  Result := WidgetSet.EnableScrollBar(Wnd, wSBflags, wArrows);
end;

function EnableWindow(hWnd: HWND; bEnable: Boolean): Boolean;
begin
  Result := WidgetSet.EnableWindow(hWnd, bEnable);
end;

function EndPaint(Handle : hwnd; var PS : TPaintStruct): Integer;
begin
  Result:=WidgetSet.EndPaint(Handle,PS);
end;

procedure EnterCriticalSection(var CritSection: TCriticalSection);
begin
  WidgetSet.EnterCriticalSection(CritSection);
end;

function EnumDisplayMonitors(hdc: HDC; lprcClip: PRect; lpfnEnum: MonitorEnumProc; dwData: LPARAM): LongBool;
begin
  Result := Widgetset.EnumDisplayMonitors(hdc, lprcClip, lpfnEnum, dwData);
end;

function EnumFontFamilies(DC: HDC; Family: Pchar;
  EnumFontFamProc: FontEnumProc; LParam:Lparam):longint;
begin
  Result := WidgetSet.EnumFontFamilies(DC, Family, EnumFontFamProc, LParam);
end;

function EnumFontFamiliesEx(DC: HDC; lpLogFont: PLogFont;
  Callback: FontEnumExProc; LParam: Lparam; flags: dword): longint;
begin
  Result := WidgetSet.EnumFontFamiliesEx(DC, lpLogFont, Callback, LParam, flags);
end;

function EqualRgn(Rgn1: HRGN; Rgn2: HRGN): Boolean;
begin
  Result := WidgetSet.EqualRgn(Rgn1, Rgn2);
end;

function ExcludeClipRect(dc: hdc; Left, Top, Right, Bottom : Integer) : Integer;
begin
  Result := WidgetSet.ExcludeClipRect(DC,Left,Top,Right,Bottom);
end;

function ExtCreatePen(dwPenStyle, dwWidth: DWord; const lplb: TLogBrush; dwStyleCount: DWord; lpStyle: PDWord): HPEN;
begin
  Result := WidgetSet.ExtCreatePen(dwPenStyle, dwWidth, lplb, dwStyleCount, lpStyle);
end;

function ExtTextOut(DC: HDC; X, Y: Integer; Options: Longint; Rect: PRect; Str: PChar; Count: Longint; Dx: PInteger): Boolean;
begin
  Result := WidgetSet.ExtTextOut(DC, X, Y, Options, Rect, Str, Count, Dx);
end;

function ExtSelectClipRGN(dc: hdc; rgn : hrgn; Mode : Longint) : Integer;
begin
  Result := WidgetSet.ExtSelectClipRGN(DC, RGN, Mode);
end;

function FillRect(DC: HDC; const Rect: TRect; Brush: HBRUSH): Boolean;
begin
  Result := WidgetSet.FillRect(DC, Rect, Brush);
end;

function FillRgn(DC: HDC; RegionHnd: HRGN; hbr: HBRUSH): BOOL;
begin
  Result := WidgetSet.FillRgn(DC,RegionHnd,hbr);
end;

function FloodFill(DC: HDC; X, Y: Integer;
  Color: TGraphicsColor; FillStyle: TGraphicsFillStyle;
  Brush: HBRUSH): Boolean;
begin
  Result := WidgetSet.FloodFill(DC,X,Y,Color,FillStyle,Brush);
end;

function FrameRect(DC: HDC; const ARect: TRect; hBr: HBRUSH): Integer;
begin
  Result:= WidgetSet.FrameRect(DC, ARect, hBr);
end;

function GetActiveWindow : HWND;
begin
  Result := WidgetSet.GetActiveWindow;
end;

function GetBitmapBits(Bitmap: HBITMAP; Count: Longint;  Bits: Pointer): Longint;
begin
  Result := WidgetSet.GetBitmapBits(Bitmap, Count, Bits);
end;

function GetBkColor(DC: HDC): TColorRef;
begin
  Result := WidgetSet.GetBkColor(DC);
end;

function GetCapture: HWND;
begin
  Result := WidgetSet.GetCapture;
end;

function GetCaretPos(var lpPoint: TPoint): Boolean;
begin
  Result := WidgetSet.GetCaretPos(lpPoint);
end;

{------------------------------------------------------------------------------
  Function: GetClientRect
  Params: handle:
          Result:
  Returns: true on success

  Returns the client rectangle of a control. Left and Top are always 0.
  The client rectangle is the size of the inner area of a control, where the
  child controls are visible.
  A child control with Align=alClient will completely fill the clientrect.
 ------------------------------------------------------------------------------}
function GetClientRect(handle : HWND; var Rect : TRect) : Boolean;
begin
  Result := WidgetSet.GetClientRect(handle, Rect);
end;

function GetClipBox(DC : hDC; lpRect : PRect) : Longint;
begin
  Result := WidgetSet.GetClipBox(DC, lpRect);
end;

function GetClipRGN(DC : hDC; RGN : hRGN) : Longint;
begin
  Result := WidgetSet.GetClipRGN(DC, RGN);
end;

function GetCursorPos(var lpPoint:TPoint): Boolean;
begin
  Result := WidgetSet.GetCursorPos(lpPoint);
end;

function GetCurrentObject(DC: HDC; uObjectType: UINT): HGDIOBJ;
begin
  Result := WidgetSet.GetCurrentObject(DC, uObjectType);
end;

function GetCharABCWidths(DC: HDC; p2, p3: UINT; const ABCStructs): Boolean;
begin
  Result := WidgetSet.GetCharABCWidths(DC, p2, p3, ABCStructs);
end;

function GetDC(hWnd: HWND): HDC;
begin
  Result := WidgetSet.GetDC(hWnd);
end;

function GetDIBits(DC: HDC; Bitmap: HBitmap; StartScan, NumScans: UINT;
  Bits: Pointer; var BitInfo: BitmapInfo; Usage: UINT): Integer;
begin
  Result := WidgetSet.GetDIBits(DC, Bitmap, StartScan, NumScans, Bits,
                                      BitInfo, Usage);
end;

function GetDeviceCaps(DC: HDC; Index: Integer): Integer;
begin
  Result := WidgetSet.GetDeviceCaps(DC, Index);
end;

function GetDoubleClickTime: UINT;
begin
  Result := WidgetSet.GetDoubleClickTime;
end;


function GetFocus: HWND;
begin
  Result := WidgetSet.GetFocus;
end;

function GetFontLanguageInfo(DC: HDC): DWord;
begin
  Result := WidgetSet.GetFontLanguageInfo(DC);
end;

function GetForegroundWindow: HWND;
begin
  Result := WidgetSet.GetForegroundWindow;
end;

function GetIconInfo(AIcon: HICON; AIconInfo: PIconInfo): Boolean;
begin
  Result := WidgetSet.GetIconInfo(AIcon, AIconInfo);
end;

function GetKeyState(nVirtKey: Integer): Smallint;
begin
  Result := WidgetSet.GetKeyState(nVirtKey);
end;

function GetMapMode(DC: HDC): Integer;
begin
  Result := WidgetSet.GetMapMode(DC);
end;

function GetMonitorInfo(hMonitor: HMONITOR; lpmi: PMonitorInfo): Boolean;
begin
  Result := Widgetset.GetMonitorInfo(hMonitor, lpmi);
end;

function GetObject(GDIObject: HGDIOBJ; BufSize: Integer; Buf: Pointer): Integer;
begin
  Result := WidgetSet.GetObject(GDIObject, BufSize, Buf);
end;

function GetPaletteEntries(Palette: HPALETTE; StartIndex, NumEntries: UINT;
  var PaletteEntries): UINT;
begin
  Result := WidgetSet.GetPaletteEntries(Palette,StartIndex,NumEntries,
                                              PaletteEntries);
end;

function GetParent(Handle : HWND): HWND;
begin
  Result := WidgetSet.GetParent(Handle);
end;

function GetProp(Handle : hwnd; Str : PChar): Pointer;
begin
  Result := WidgetSet.GetProp(Handle,Str);
end;

function GetRGNBox(RGN : HRGN; lpRect : PRect) : Longint;
begin
  Result := WidgetSet.GetRGNBox(RGN, lpRect);
end;

function GetROP2(DC: HDC): integer;
begin
  result := WidgetSet.GetROP2(DC);
end;

function GetScrollInfo(Handle: HWND; SBStyle: Integer; var ScrollInfo: TScrollInfo): Boolean;
begin
  Result := WidgetSet.GetScrollInfo(Handle, SBStyle, ScrollInfo);
end;

function GetStockObject(Value : Integer): THandle;
begin
  Result := WidgetSet.GetStockObject(Value);
end;

function GetSysColor(nIndex: Integer): DWORD;
begin
  Result := WidgetSet.GetSysColor(nIndex);
end;

function GetSysColorBrush(nIndex: Integer): HBrush;
begin
  Result := WidgetSet.GetSysColorBrush(nIndex);
end;

function GetSystemMetrics(nIndex: Integer): Integer;
begin
  Result := WidgetSet.GetSystemMetrics(nIndex);
end;

function GetSystemPaletteEntries(DC: HDC; StartIndex, NumEntries: UINT;
  var PaletteEntries): UINT;
begin
  Result := WidgetSet.GetSystemPaletteEntries(DC,StartIndex,NumEntries,
                                                    PaletteEntries);
end;

function GetTextColor(DC: HDC) : TColorRef;
begin
  Result := WidgetSet.GetTextColor(DC);
end;

function GetTextExtentExPoint(DC: HDC; Str: PChar; Count, MaxWidth: Integer;
  MaxCount, PartialWidths: PInteger; var Size: TSize): Boolean;
begin
  Result := WidgetSet.GetTextExtentExPoint(DC,Str,Count,MaxWidth,MaxCount,
                                                 PartialWidths,Size);
end;

function GetTextExtentPoint(DC: HDC; Str: PChar; Count: Integer;
  var Size: TSize): Boolean;
begin
  Result := WidgetSet.GetTextExtentPoint(DC, Str, Count, Size);
end;

function GetTextExtentPoint32(DC: HDC; Str: PChar; Count: Integer;
  var Size: TSize): Boolean;
begin
  Result := WidgetSet.GetTextExtentPoint32(DC, Str, Count, Size);
end;

function GetTextMetrics(DC: HDC; var TM: TTextMetric): Boolean;
begin
  Result := WidgetSet.GetTextMetrics(DC, TM);
end;

function GetViewPortExtEx(DC: HDC; Size: PSize): Integer;
begin
  Result := WidgetSet.GetViewPortExtEx(DC, Size);
end;

function GetViewPortOrgEx(DC: HDC; P: PPoint): Integer;
begin
  Result := WidgetSet.GetViewPortOrgEx(DC, P);
end;

function GetWindowExtEx(DC: HDC; Size: PSize): Integer;
begin
  Result := WidgetSet.GetWindowExtEx(DC, Size);
end;

function GetWindowLong(Handle : hwnd; int : Integer): PtrInt;
begin
  Result := WidgetSet.GetWindowLong(Handle, int);
end;

{------------------------------------------------------------------------------
  function GetWindowRect(Handle : hwnd; var Rect : TRect): Integer;

  After the call, ARect will be the control area in screen coordinates.
  That means, Left and Top will be the screen coordinate of the TopLeft pixel
  of the Handle object and Right and Bottom will be the screen coordinate of
  the BottomRight pixel.
------------------------------------------------------------------------------}
function GetWindowRect(Handle: hwnd; var Rect: TRect): Integer;
begin
  Result := WidgetSet.GetWindowRect(Handle, Rect);
end;

{------------------------------------------------------------------------------
  function GetWindowSize(Handle: hwnd; var Width, Height: integer): boolean;

  Returns the Width and Height
------------------------------------------------------------------------------}
function GetWindowSize(Handle: hwnd; var Width, Height: integer): boolean;
begin
  Result := WidgetSet.GetWindowSize(Handle, Width, Height);
end;

function GetWindowOrgEx(dc: hdc; var P: TPoint): Integer;
begin
  Result := WidgetSet.GetWindowOrgEx(dc, P);
end;

function GetWindowOrgEx(dc: hdc; P : PPoint): Integer;
begin
 Result := WidgetSet.GetWindowOrgEx(dc, P);
end;

function GradientFill(DC: HDC; Vertices: PTriVertex; NumVertices: Longint;
  Meshes: Pointer; NumMeshes: Longint; Mode: Longint): Boolean;
begin
  Result := WidgetSet.GradientFill(DC, Vertices, NumVertices, Meshes, NumMeshes, Mode);
end;

function HideCaret(hWnd: HWND): Boolean;
begin
  Result := WidgetSet.HideCaret(hWnd);
end;

{------------------------------------------------------------------------------
  Function: IntersectClipRect
  Params:  dc: hdc; Left, Top, Right, Bottom: Integer
  Returns: Integer

  Shrinks the current clipping region in the device context dc to the boundary
  defined by Left, Top, Right, Bottom.

  The result can be one of the following constants
      Error
      NullRegion
      SimpleRegion
      ComplexRegion
      Region_Error

 ------------------------------------------------------------------------------}
function IntersectClipRect(dc: hdc; Left, Top, Right, Bottom: Integer): Integer;
begin
  Result := WidgetSet.IntersectClipRect(dc,Left,Top,Right,Bottom);
end;

function InvalidateRect(aHandle : HWND; ARect : pRect; bErase : Boolean) : Boolean;
begin
  Result := WidgetSet.InvalidateRect(aHandle, ARect, bErase);
end;

function InvalidateRgn(Handle: HWND; Rgn: HRGN; Erase: Boolean): Boolean;
begin
  Result := WidgetSet.InvalidateRgn(Handle, Rgn, Erase);
end;

function IsDBCSLeadByte(TestChar: Byte): boolean;
begin
  Result := WidgetSet.IsDBCSLeadByte(TestChar);
end;

function IsIconic(handle: HWND): boolean;
begin
  Result := WidgetSet.IsIconic(Handle);
end;

function IsWindow(handle: HWND): boolean;
begin
  Result := WidgetSet.IsWindow(Handle);
end;

function IsWindowEnabled(handle: HWND): boolean;
begin
  Result := WidgetSet.IsWindowEnabled(Handle);
end;

function IsWindowVisible(handle: HWND): boolean;
begin
  Result := WidgetSet.IsWindowVisible(Handle);
end;

function IsZoomed(handle: HWND): boolean;
begin
  Result := WidgetSet.IsZoomed(Handle);
end;

procedure InitializeCriticalSection(var CritSection: TCriticalSection);
begin
  WidgetSet.InitializeCriticalSection(CritSection);
end;

procedure LeaveCriticalSection(var CritSection: TCriticalSection);
begin
  WidgetSet.LeaveCriticalSection(CritSection);
end;

function LineTo(DC: HDC; X, Y: Integer): Boolean;
begin
  Result := WidgetSet.LineTo(DC, X, Y);
end;

function LoadBitmap(hInstance: THandle; lpBitmapName: PChar): HBitmap;
begin
  Result := Widgetset.LoadBitmap(hInstance, lpBitmapName);
end;

function LoadCursor(hInstance: THandle; lpCursorName: PChar): HCursor;
begin
  Result := Widgetset.LoadCursor(hInstance, lpCursorName);
end;

function LoadIcon(hInstance: THandle; lpIconName: PChar): HIcon;
begin
  Result := Widgetset.LoadIcon(hInstance, lpIconName);
end;

function LPtoDP(DC: HDC; var Points; Count: Integer): BOOL;
begin
  Result := WidgetSet.LPtoDP(DC, Points, Count);
end;

function MaskBlt(DestDC: HDC; X, Y, Width, Height: Integer; SrcDC: HDC; XSrc, YSrc: Integer; Mask: HBITMAP; XMask, YMask: Integer): Boolean;
begin
  Result := WidgetSet.MaskBlt(DestDC, X, Y, Width, Height, SrcDC, XSrc, YSrc, Mask, XMask, YMask);
end;

function MaskBlt(DestDC: HDC; X, Y, Width, Height: Integer; SrcDC: HDC; XSrc, YSrc: Integer; Mask: HBITMAP; XMask, YMask: Integer; Rop: DWORD): Boolean;
begin
  Result := WidgetSet.MaskBlt(DestDC, X, Y, Width, Height, SrcDC, XSrc, YSrc, Mask, XMask, YMask, Rop);
end;

function MessageBox(hWnd: HWND; lpText, lpCaption: PChar; uType : Cardinal): integer;
begin
  Result:= WidgetSet.MessageBox(hWnd, lpText, lpCaption, uType);
end;

function MonitorFromPoint(ptScreenCoords: TPoint; dwFlags: DWord): HMONITOR;
begin
  Result := Widgetset.MonitorFromPoint(ptScreenCoords, dwFlags);
end;

function MonitorFromRect(lprcScreenCoords: PRect; dwFlags: DWord): HMONITOR;
begin
  Result := Widgetset.MonitorFromRect(lprcScreenCoords, dwFlags);
end;

function MonitorFromWindow(hWnd: HWND; dwFlags: DWord): HMONITOR;
begin
  Result := Widgetset.MonitorFromWindow(hWnd, dwFlags);
end;

function MoveToEx(DC: HDC; X, Y: Integer; OldPoint: PPoint): Boolean;
begin
  Result := WidgetSet.MoveToEx(DC, X, Y, OldPoint);
end;

function OffsetRgn(RGN: HRGN; nXOffset, nYOffset: Integer): Integer;
begin
  Result := WidgetSet.OffsetRgn(RGN, nXOffset, nYOffset);
end;

function PaintRgn(DC: HDC; RGN: HRGN): Boolean;
begin
  Result := WidgetSet.PaintRgn(DC, RGN);
end;

function PeekMessage(var lpMsg : TMsg; Handle : HWND;
  wMsgFilterMin, wMsgFilterMax,wRemoveMsg : UINT): Boolean;
begin
  Result := WidgetSet.PeekMessage(lpMsg,Handle,
                                        wMsgFilterMin,wMsgFilterMax,wRemoveMsg);
end;

function Pie(DC: HDC; x1, y1, x2, y2, sx, sy, ex, ey: Integer): Boolean;
begin
  Result := WidgetSet.Pie(DC,x1, y1, x2, y2, sx, sy, ex, ey);
end;

function PolyBezier(DC: HDC; Points: PPoint; NumPts: Integer;
  Filled, Continuous: boolean): boolean;
begin
  Result := WidgetSet.PolyBezier(DC,Points,NumPts,Filled,Continuous);
end;

function Polygon(DC: HDC; Points: PPoint; NumPts: Integer;
  Winding: boolean): boolean;
begin
  Result := WidgetSet.Polygon(DC, Points, NumPts, Winding);
end;

function Polyline(DC: HDC; Points: PPoint; NumPts: Integer): boolean;
begin
  Result := WidgetSet.Polyline(DC,Points,NumPts);
end;

function PostMessage(Handle: HWND; Msg: Cardinal; WParam: WParam; LParam: LParam): Boolean;
begin
  Result := WidgetSet.PostMessage(Handle, Msg, WParam, LParam);
end;

function PtInRegion(RGN: HRGN; X, Y: Integer) : Boolean;
begin
  Result := WidgetSet.PtInRegion(RGN,X,Y);
end;

function RealizePalette(DC: HDC): Cardinal;
begin
  Result := WidgetSet.RealizePalette(DC);
end;

function Rectangle(DC: HDC; X1, Y1, X2, Y2: Integer): Boolean;
begin
  Result := WidgetSet.Rectangle(DC, X1, Y1, X2, Y2);
end;

function RectInRegion(RGN: HRGN; ARect: TRect): Boolean;
begin
  Result := WidgetSet.RectInRegion(RGN, ARect);
end;

function RectVisible(dc : hdc; const ARect: TRect) : Boolean;
begin
  Result := WidgetSet.RectVisible(dc, ARect);
end;

function RedrawWindow(Wnd: HWND; lprcUpdate: PRECT; hrgnUpdate: HRGN; flags: UINT): Boolean;
begin
  Result := WidgetSet.RedrawWindow(Wnd, lprcUpdate, hrgnUpdate, flags);
end;

function ReleaseCapture:  Boolean;
begin
  Result := WidgetSet.ReleaseCapture;
end;

function ReleaseDC(hWnd: HWND; DC: HDC): Integer;
begin
  Result := WidgetSet.ReleaseDC(hWnd, DC);
end;

function RemoveProp(Handle: hwnd; Str: PChar): THandle;
begin
  Result := WidgetSet.RemoveProp(Handle, Str);
end;

function RoundRect(DC : hDC; X1, Y1, X2, Y2: Integer; RX,RY : Integer): Boolean;
begin
  Result := WidgetSet.RoundRect(DC, X1, Y1, X2, Y2, RX, RY);
end;

function RestoreDC(DC: HDC; SavedDC: Integer): Boolean;
begin
  Result := WidgetSet.RestoreDC(DC, SavedDC)
end;

function SaveDC(DC: HDC) : Integer;
begin
  Result := WidgetSet.SaveDC(DC)
end;

function ScreenToClient(Handle : HWND; var P : TPoint) : Integer;
begin
  Result := WidgetSet.ScreenToClient(Handle, P);
end;

function ScrollWindowEx(hWnd: HWND; dx, dy: Integer; prcScroll, prcClip: PRect; hrgnUpdate: HRGN; prcUpdate: PRect; flags: UINT): Boolean;
begin
  Result := WidgetSet.ScrollWindowEx(hWnd, dx, dy, prcScroll, prcClip, hrgnUpdate, prcUpdate, flags);
end;

function SendMessage(HandleWnd: HWND; Msg: Cardinal; WParam: WParam; LParam: LParam): LResult;
begin
  Result := WidgetSet.SendMessage(HandleWnd, Msg, WParam, LParam);
end;

function SetActiveWindow(Handle: HWND): HWND;
begin
  Result := WidgetSet.SetActiveWindow(Handle);
end;

function SetBkColor(DC: HDC; Color: TColorRef): TColorRef;  //pbd
begin
  Result := WidgetSet.SetBkColor(DC, Color);
end;

function SetBkMode(DC: HDC; bkMode : Integer) : Integer;
begin
  Result := WidgetSet.SetBkMode(DC, bkMode);
end;

function SetCapture(AHandle: HWND): HWND;
begin
  Result := WidgetSet.SetCapture(AHandle);
end;

function SetCaretPos(X, Y: Integer): Boolean;
begin
  Result := WidgetSet.SetCaretPos(X, Y);
end;

function SetCaretPosEx(Handle: HWnd; X, Y: Integer): Boolean;
begin
  Result := WidgetSet.SetCaretPosEx(Handle, X, Y);
end;

function SetCursor(hCursor: HICON): HCURSOR;
begin
  Result := WidgetSet.SetCursor(hCursor);
end;

function SetCursorPos(X, Y: Integer): Boolean;
begin
  Result := WidgetSet.SetCursorPos(X, Y);
end;

function SetMapMode(DC: HDC; fnMapMode : Integer): Integer;
begin
  Result := WidgetSet.SetMapMode(DC, fnMapMode);
end;

function SetMenu(AWindowHandle: HWND; AMenuHandle: HMENU): Boolean;
begin
  Result := WidgetSet.SetMenu(AWindowHandle, AMenuHandle);
end;

function SetParent(hWndChild: HWND; hWndParent: HWND): HWND;
begin
  Result := WidgetSet.SetParent(hWndChild, hWndParent);
end;

function SetProp(Handle: hwnd; Str : PChar; Data : Pointer) : Boolean;
begin
  Result := WidgetSet.SetProp(Handle,Str,Data);
end;

function SetRectRgn(aRGN: HRGN; X1, Y1, X2, Y2 : Integer): Boolean;
begin
  Result := WidgetSet.SetRectRgn(aRGN, X1, Y1, X2, Y2);
end;

function SetROP2(DC: HDC; Mode: Integer): Integer;
begin
  Result := WidgetSet.SetRop2(Dc, Mode);
end;

function SelectClipRGN(DC : hDC; RGN : HRGN) : Longint;
begin
  Result := WidgetSet.SelectClipRGN(DC, RGN);
end;

function SelectObject(DC: HDC; GDIObj: HGDIOBJ): HGDIOBJ;
begin
  Result := WidgetSet.SelectObject(DC, GDIObj);
end;

function SelectPalette(DC: HDC; Palette: HPALETTE; ForceBackground: Boolean): HPALETTE;
begin
  Result := WidgetSet.SelectPalette(DC, Palette, ForceBackground);
end;

function SetFocus(hWnd: HWND): HWND;
begin
//DebugLn('[winapi.inc SetFocus] A');
  Result := WidgetSet.SetFocus(hWnd);
//DebugLn('[winapi.inc SetFocus] END');
end;

function SetForegroundWindow(hWnd: HWND): boolean;
begin
  Result := WidgetSet.SetForegroundWindow(hWnd);
end;

function SetScrollInfo(Handle : HWND; SBStyle : Integer;
  ScrollInfo: TScrollInfo; Redraw : Boolean): Integer;
begin
  Result := WidgetSet.SetSCrollInfo(Handle, SBStyle, ScrollInfo, Redraw)
end;

function SetStretchBltMode(DC: HDC; StretchMode: Integer): Integer;
begin
  Result := WidgetSet.SetStretchBltMode(DC,StretchMode);
end;

function SetSysColors(cElements: Integer; const lpaElements; const lpaRgbValues): Boolean;
begin
  Result := WidgetSet.SetSysColors(cElements, lpaElements, lpaRgbValues);
end;

function SetTextCharacterExtra(_hdc : hdc; nCharExtra : Integer):Integer;
begin
  Result := WidgetSet.SetTextCharacterExtra(_hdc, nCharExtra);
end;

function SetTextColor(DC: HDC; Color: TColorRef): TColorRef;
begin
  Result := WidgetSet.SetTextColor(DC, Color);
end;

function SetViewPortExtEx(DC: HDC; XExtent, YExtent : Integer; OldSize: PSize): Boolean;
begin
  Result := WidgetSet.SetViewPortExtEx(dc, XExtent, YExtent, OldSize);
end;

function SetViewPortOrgEx(DC: HDC; NewX, NewY: Integer; OldPoint: PPoint): Boolean;
begin
  Result := WidgetSet.SetViewPortOrgEx(dc, NewX, NewY, OldPoint);
end;

function SetWindowExtEx(DC: HDC; XExtent, YExtent: Integer; OldSize: PSize): Boolean;
begin
  Result := WidgetSet.SetWindowExtEx(dc, XExtent, YExtent, OldSize);
end;

function SetWindowLong(Handle: HWND; Idx: Integer; NewLong : PtrInt): PtrInt;
begin
  Result := WidgetSet.SetWindowLong(handle, Idx, NewLong);
end;

function SetWindowOrgEx(dc : hdc; NewX, NewY : Integer;
  OldPoint: PPoint) : Boolean;
begin
  Result := WidgetSet.SetWindowOrgEx(dc, NewX, NewY, OldPoint);
end;

function SetWindowPos(hWnd: HWND; hWndInsertAfter: HWND; X, Y, cx, cy: Integer; uFlags: UINT): Boolean;
begin
  Result := WidgetSet.SetWindowPos(hWnd, hWndInsertAfter, X, Y, cx, cy, uFlags);
end;

function SetWindowRgn(hWnd: HWND; hRgn: HRGN; bRedraw: Boolean):longint;
begin
  Result := WidgetSet.SetWindowRgn(hWnd, hRgn, bRedraw);
end;

function ShowCaret(hWnd: HWND): Boolean;
begin
  Result := WidgetSet.ShowCaret(hWnd)
end;

function ShowScrollBar(Handle: HWND; wBar: Integer; bShow: Boolean): Boolean;
begin
  Result := WidgetSet.ShowScrollBar(Handle, wBar, bShow);
end;

function ShowWindow(hWnd: HWND; nCmdShow: Integer): Boolean;
begin
  Result := WidgetSet.ShowWindow(hWnd,nCmdShow);
end;

function StretchBlt(DestDC: HDC; X, Y, Width, Height: Integer;
  SrcDC: HDC; XSrc, YSrc, SrcWidth, SrcHeight: Integer; Rop: Cardinal): Boolean;
begin
  Result := WidgetSet.StretchBlt(DestDC, X, Y, Width, Height,
                                   SrcDC, XSrc, YSrc, SrcWidth, SrcHeight, Rop);
end;

function StretchDIBits(DC: HDC; DestX, DestY, DestWidth, DestHeight, SrcX,
  SrcY, SrcWidth, SrcHeight: Integer; Bits: Pointer; var BitsInfo: TBitmapInfo;
  Usage: UINT; Rop: DWORD): Integer;
begin
  Result := WidgetSet.StretchDIBits(DC,DestX,DestY,DestWidth,DestHeight,
                          SrcX,SrcY,SrcWidth,SrcHeight,Bits,BitsInfo,Usage,Rop);
end;

function SystemParametersInfo(uiAction: DWord; uiParam: DWord; pvParam: Pointer; fWinIni: DWord): LongBool;
begin
  Result := WidgetSet.SystemParametersInfo(uiAction, uiParam, pvParam, fWinIni);
end;

function TextOut(DC: HDC; X,Y : Integer; Str : Pchar; Count: Integer) : Boolean;
begin
  Result := WidgetSet.TextOut(DC, X, Y, Str, Count);
end;

function UpdateWindow(Handle: HWND): Boolean;
begin
  Result := WidgetSet.UpdateWindow(Handle);
end;

function WindowFromPoint(Point : TPoint) : HWND;
begin
  Result := WidgetSet.WindowFromPoint(Point);
end;

//##apiwiz##eps##   // Do not remove

{******************************************************************************
  Platform independent stuff
 ******************************************************************************}

//##apiwiz##spi##   // Do not remove
{------------------------------------------------------------------------------
  Function: AdjustWindowRectEx
  Params:
  Returns:

 ------------------------------------------------------------------------------}
function AdjustWindowRectEx( Var Rect: TRect; Style1: Word; MenuExist : Boolean;
  Style2 : Word) : Boolean;
begin
  // ToDo:
  Result := true;
  if MenuExist
  then Rect.Top := Rect.Top + 25;
end;

{------------------------------------------------------------------------------
  Function: CharLower
  Params:
  Returns:

 ------------------------------------------------------------------------------}
function CharLower(c: char): char; inline;
begin
  Result := LowerCaseChars[c];
end;

{------------------------------------------------------------------------------
  Function: CharLowerBuff
  Params: pStr:
          Len:
  Returns:

 ------------------------------------------------------------------------------}
function CharLowerBuff(pStr: PChar; Len: Integer): Integer;
begin
  Result := len;
  while len > 0 do
  begin
    pStr^ := LowerCaseChars[pStr^];
    inc(pStr);
    dec(len);
  end;
end;

{------------------------------------------------------------------------------
  Function: CharUpper
  Params:
  Returns:

 ------------------------------------------------------------------------------}
function CharUpper(c: char): char; inline;
begin
  Result := UpperCaseChars[c];
end;

{------------------------------------------------------------------------------
  Function: CharUpperBuff
  Params: pStr:
          Len:
  Returns:

 ------------------------------------------------------------------------------}
function CharUpperBuff(pStr: PChar; Len: Integer): Integer;
begin
  Result := len;
  while len > 0 do
  begin
    pStr^ := UpperCaseChars[pStr^];
    inc(pStr);
    dec(len);
  end;
end;

{------------------------------------------------------------------------------
  Function: CopyRect  pbd
  Params:
  Returns:

 ------------------------------------------------------------------------------}
function CopyRect(var DestRect: TRect; const SrcRect: TRect): Boolean;
begin
  System.Move(SrcRect, DestRect, SizeOf(TRect));
  Result := True;
end;

{------------------------------------------------------------------------------
  function CreateEllipticRgnIndirect(const ARect: TRect): HRGN;
 ------------------------------------------------------------------------------}
function CreateEllipticRgn(X1, Y1, X2, Y2: Integer): HRGN;
begin
  Result := WidgetSet.CreateEllipticRgn(X1, Y1, X2, Y2);
end;

{------------------------------------------------------------------------------
  function CreateEllipticRgnIndirect(const ARect: TRect): HRGN;
 ------------------------------------------------------------------------------}
function CreateEllipticRgnIndirect(const ARect: TRect): HRGN;
begin
  with ARect do
    Result:=CreateEllipticRgn(Left,Top,Right,Bottom);
end;

{------------------------------------------------------------------------------
  Function: CreateFont
  Params:
  Returns:

 ------------------------------------------------------------------------------}
function CreateFont(Height, Width, Escapement, Orientation, Weight: Integer;
  Italic, Underline, StrikeOut, CharSet, OutputPrecision, ClipPrecision,
  Quality, PitchAndFamily: Byte; FaceName: PChar): HFONT;
var
  LogFont: TLogFont;
begin

  FillChar(LogFont,SizeOf(LogFont),0);
  with LogFont do
  begin
    lfHeight := Height;
    lfWidth := Width;
    lfEscapement := Escapement;
    lfOrientation := Orientation;
    lfWeight := Weight;
    lfItalic := Italic;
    lfUnderline := Underline;
    lfStrikeOut := StrikeOut;
    lfCharSet := CharSet;
    lfOutPrecision := OutputPrecision;
    lfClipPrecision := ClipPrecision;
    lfQuality := Quality;
    lfPitchAndFamily := PitchAndFamily;
    StrLCopy(@lfFaceName[0], FaceName, SizeOf(lfFaceName)-1);
  end;

  Result := CreateFontIndirect(LogFont);
end;

{------------------------------------------------------------------------------
  Function: CreatePen
  Params:
  Returns:

 ------------------------------------------------------------------------------}
function CreatePen(Style, Width: Integer; Color: TColorRef): HPEN;
var
  LogPen: TLogPen;
begin
  with LogPen do
  begin
    lopnStyle := Style;
    lopnWidth.X := Width;
    lopnColor := Color;
  end;

  Result := CreatePenIndirect(LogPen);
end;

{------------------------------------------------------------------------------
  function CreateRectRgnIndirect(const ARect: TRect): HRGN;
-------------------------------------------------------------------------------}
function CreateRectRgnIndirect(const ARect: TRect): HRGN;
begin
  with ARect do
    Result:=CreateRectRgn(Left,Top,Right,Bottom);
end;

{------------------------------------------------------------------------------
  Function: CreateSolidBrush
  Params:
  Returns:

 ------------------------------------------------------------------------------}
function CreateSolidBrush(Color: TColorRef): HBRUSH;
var
  LogBrush: TLogBrush;
begin
  LogBrush.lbStyle := BS_SOLID;
  LogBrush.lbColor := Color;
  LogBrush.lbHatch := 0;
  Result := CreateBrushIndirect(LogBrush);
end;

{------------------------------------------------------------------------------
  Function: EqualRect
  Params:   Rect to Compare
  Returns:
  Quicker with a comparemem? whats the fpc equiv?
 ------------------------------------------------------------------------------}
function EqualRect(const lprc1, lprc2: TRect): Boolean;
begin
  //Result := CompareMem(@lprc1, @lprc2, SizeOf(TRect);
  Result := (lprc1.Left = lprc2.Left) And (lprc1.Right = lprc2.Right) And
            (lprc1.Top = lprc2.Top) And (lprc1.Bottom = lprc2.Bottom);
end;

{------------------------------------------------------------------------------
  Function: GetScrollPos
  Params:   Handle, nBar
  Returns:

 ------------------------------------------------------------------------------}
function GetScrollPos(Handle: HWND; nBar: Integer): Integer;
var
  Info: TScrollInfo;
begin
  Info.fMask:=SIF_POS;
  Info.nPos:=0;
  GetScrollInfo(Handle, nBar, Info);
  Result := Info.nPos;
end;

function GetRValue(RGB : DWORD) : BYTE; inline;
begin
  Result := RGB and $ff;
end;

function GetGValue(RGB : DWORD) : BYTE; inline;
begin
  Result := (RGB shr 8) and $ff;
end;

function GetBValue(RGB : DWORD) : BYTE; inline;
begin
  Result := (RGB shr 16) and $ff;
end;

procedure GetRGBValues(RGB : DWORD; out R, G, B: Byte); inline;
begin
  R := RGB and $ff;
  G := (RGB shr 8) and $ff;
  B := (RGB shr 16) and $ff;
end;

procedure GetRGBIntValues(RGB : DWORD; out R, G, B: integer); inline;
begin
  R := RGB and $ff;
  G := (RGB shr 8) and $ff;
  B := (RGB shr 16) and $ff;
end;


{------------------------------------------------------------------------------
  Function: GetScrollRange
  Params:   Handle, nBar, lpMinPos, lpMaxPos
  Returns:

 ------------------------------------------------------------------------------}
function GetScrollRange(Handle: HWND; nBar: Integer; var lpMinPos, lpMaxPos: Integer): Boolean;
var
  Info: TScrollInfo;
begin
  Info.fMask:=SIF_RANGE;
  Result := GetScrollInfo(Handle, nBar, Info);
  lpMinPos := Info.nMin;
  lpMaxPos := Info.nMax;
end;

{------------------------------------------------------------------------------
  Function: IndexToOverlayMask
  Params: iOverlay:
  Returns:

 ------------------------------------------------------------------------------}
function IndexToOverlayMask(iOverlay: UINT): UINT; inline;
begin
  Result := iOverlay shl 8;
end;


{------------------------------------------------------------------------------
  Function: InflateRect
  Params:   ARect: points to structure that increases or decreases in size.
            dx   : amount to increase the rectangle width.
            dy   : amount to increase the rectangle height.
  Returns:  True if succesful

  Increases or decreases the width and height of the specified rectangle.
 ------------------------------------------------------------------------------}
function InflateRect(var ARect: TRect; dx, dy: Integer): Boolean;
begin
  // make sure, that after deflating holds: Left<=Right
  if (dx<0) and (ARect.Right-ARect.Left+2*dx<0) then begin
    ARect.Left:=(ARect.Left+ARect.Right) div 2;
    ARect.Right:=ARect.Left;
  end else begin
    dec(ARect.Left,dx);
    inc(ARect.Right,dx);
  end;

  // make sure, that after deflating holds: Top<=Bottom
  if (dy<0) and (ARect.Bottom-ARect.Top+2*dy<0) then begin
    ARect.Top:=(ARect.Top+ARect.Bottom) div 2;
    ARect.Bottom:=ARect.Top;
  end else begin
    dec(ARect.Top,dy);
    inc(ARect.Bottom,dy);
  end;

  Result := True;
end;

{------------------------------------------------------------------------------
  Function: IntersectRect
  Params:  var DestRect: TRect; const SrcRect1, SrcRect2: TRect
  Returns: Boolean

  Intersects SrcRect1 and SrcRect2 into DestRect.
  Intersecting means that DestRect will be the overlapping area of SrcRect1 and
  SrcRect2. If SrcRect1 and SrcRect2 do not overlapp the Result is false, else
  true.
 ------------------------------------------------------------------------------}
function IntersectRect(var DestRect: TRect;
  const SrcRect1, SrcRect2: TRect): Boolean;
begin
  Result := Types.IntersectRect(DestRect,SrcRect1,SrcRect2);
end;

{------------------------------------------------------------------------------
  Function: IsCharAlphaNumeric
  Params: c:
  Returns:

 ------------------------------------------------------------------------------}
function IsCharAlphaNumeric(c : Char) : Boolean;
begin
  // your code here
  Result := c in ['a'..'z','A'..'Z','0'..'9'];
end;

{------------------------------------------------------------------------------
  Function: IsRectEmpty
  Params: const lprc: TRect
  Returns: Boolean

  Returns true if ARect is the area of the rectangle is empty,
  i.e. right coordinate is less than or equal to the left coordinate,
  or the bottom coordinate is less than or equal to the top coordinate
 ------------------------------------------------------------------------------}
function IsRectEmpty(const ARect: TRect): Boolean;
begin
  with ARect do
    Result := (Right <= Left) or (Bottom <= Top);
end;

{------------------------------------------------------------------------------
  Function: KillTimer
  Params: hWnd:
          uIDEvent:
  Returns:

 ------------------------------------------------------------------------------}
function KillTimer(Wnd: HWND; uIDEvent: UINT_PTR): BOOL;
var
  MapID: record
    case Boolean of
      True: (nIDEvent: UINT_PTR; Wnd: HWND);
      False: (filler: array[1..2] of Pointer);
  end;
  ID: Cardinal;
  InfoPtr: PTimerInfo;
begin
  if MTimerMap = nil then Exit(False);
  if MTimerInfo = nil then Exit(False);

  // make sure all is zero
  FillByte(MapID, SizeOf(MapID), 0);
  MapID.Wnd := Wnd;
  MapID.nIDEvent := uIdEvent;

  if not MTimerMap.GetData(MapID, ID) then Exit(False);

  InfoPtr := MTimerInfo.GetDataPtr(ID);
  if InfoPtr = nil then Exit(False);

  Result := Widgetset.DestroyTimer(InfoPtr^.Handle);
  MTimerInfo.Delete(ID);
  MTimerMap.Delete(MapID);
end;

{------------------------------------------------------------------------------
  Function: MakeLong
  Params: A:
          B:
  Returns:

 ------------------------------------------------------------------------------}
function MakeLong(A, B: Word): DWORD; inline;
begin
  Result := A or B shl 16;
end;


{------------------------------------------------------------------------------
  Function: MakeLParam
  Params:
  Returns:

 ------------------------------------------------------------------------------}
function MakeLParam(l, h: Word): LPARAM; inline;
begin
  Result := MakeLong(l, h);
end;

{------------------------------------------------------------------------------
  Function: MakeLResult
  Params:
  Returns:

 ------------------------------------------------------------------------------}
function MakeLResult(l, h: Word): LRESULT; inline;
begin
  Result := MakeLong(l, h);
end;

{------------------------------------------------------------------------------
  Function: MakeROP4
  Params: fore:
          back:
  Returns:

 ------------------------------------------------------------------------------}
function MakeRop4(fore,back: DWORD): DWORD; inline;
begin
  Result := ((back shl 8) and $FF000000) or fore;
end;

{------------------------------------------------------------------------------
  Function: MakeWord
  Params: A:
          B:
  Returns:

 ------------------------------------------------------------------------------}
function MakeWord(A, B: Byte): Word; inline;
begin
  Result := A or B shl 8;
end;


{------------------------------------------------------------------------------
  Function: MakeWParam
  Params:
  Returns:

 ------------------------------------------------------------------------------}
function MakeWParam(l, h: Word): WPARAM; inline;
begin
  Result := MakeLong(l, h);
end;

{------------------------------------------------------------------------------
  Function: OffSetRect
  Params:   Rect: points to structure that moves.
            dx  : amount to move the rect to left or right.  Must be negative to move to left.
            dy  : amount to move the rect up or down.  Mmust be negative to move up.
  Returns:  True if succesful

  Moves the rectangle up or down, left or right.
 ------------------------------------------------------------------------------}
function OffSetRect(var Rect: TRect; dx,dy: Integer): Boolean; inline;
begin
  Result:=LCLProc.OffsetRect(Rect,dx,dy);
end;

{------------------------------------------------------------------------------
  Function: PointtoSmallPoint
  Params:
  Returns:

 ------------------------------------------------------------------------------}
function PointtoSmallPoint(const P : TPoint) : TSmallPoint; inline;
begin
  Result.X := P.X;
  Result.Y := P.Y;
end;

{------------------------------------------------------------------------------
  Function: PtInRect
  Params:  Rect
           Point
  Returns: True if point is in rect

  Determines if the POINT is within the rect
  It is considered inside if it lies on the left top, or within the rectangle.
  It is outside ther rect if it's on the bottom or right.
 ------------------------------------------------------------------------------}
function PtInRect(const Rect : TRect; const Point : TPoint) : Boolean; inline;
begin
  Result := ((Point.X >= Rect.Left) and
             (Point.X < Rect.Right) and
             (Point.Y >= Rect.Top)  and
             (Point.Y < Rect.Bottom)
          );
end;

{------------------------------------------------------------------------------
  Function: RGB
  Params:   R: Red color
            G: Green Color
            B: Blue Color
  Returns:  The return value is the resultant RGB color

  The RGB macro selects a red, green, blue (RGB) color based on the arguments
  supplied and the color capabilities of the output device.
 ------------------------------------------------------------------------------}

function RGB(R, G, B : Byte) : TColorRef; inline;
begin
  Result := R or (G shl 8) or (B Shl 16);
end;

{------------------------------------------------------------------------------
  Function: ScrollWindow   In progress pbd
  Params:   Handle
            XAmount +scroll down -scroll up
            Rect: Rect to move
            ClipRect: Boundaries at which the pixels go to /dev/nul
  Returns:  More than a simple boolean but for compatibilty bool will do

  scrolls a window or portion of a window
 ------------------------------------------------------------------------------}
function ScrollWindow(hWnd: HWND; XAmount, YAmount: Integer;
  Rect, ClipRect: PRect): Boolean; inline;
begin
  Result := ScrollWindowEx(hWnd, XAmount, YAmount, Rect, ClipRect, 0, nil, SW_INVALIDATE);
end;

{------------------------------------------------------------------------------
  Function: SetRect
  Params:
  Returns:
 ------------------------------------------------------------------------------}
function SetRect(var ARect : TRect; aLeft,aTop,aRight,aBottom : Integer) : Boolean;
begin
  Result := True;
  ARect.Left := aLeft;
  ARect.Top := aTop;
  ARect.Right := aRight;
  ARect.Bottom := aBottom;
End;

{------------------------------------------------------------------------------
  Function: SetRectEmpty
  Params: Rect to clear
  Returns: essentially nothing


 ------------------------------------------------------------------------------}
function SetRectEmpty(var ARect: TRect): Boolean;
begin
  FillChar(ARect, SizeOf(TRect), 0);
  Result := True;
end;

{------------------------------------------------------------------------------
  Function: SetScrollPos
  Params:   Handle, nBar, nPos, bRedraw
  Returns:  The old position

  The SetScrollPos function sets the position of the scroll box (thumb) in the
  specified scroll bar and, if requested, redraws the scroll bar to reflect
  the new position of the scroll box.
 ------------------------------------------------------------------------------}
function SetScrollPos(Handle: HWND; nBar, nPos: Integer; bRedraw: Boolean): Integer;
var
  Info: TScrollInfo;
begin
  Result := GetScrollPos(Handle, nBar);

  Info.fMask := SIF_POS;
  Info.nPos := nPos;
  SetScrollInfo(Handle, nBar, Info, bRedraw);
end;

{------------------------------------------------------------------------------
  Function: SetScrollRange
  Params:   Handle, nBar, nMinPos, nMaxPos, bRedraw
  Returns:  True is succesful

  The SetScrollRange function sets the minimum and maximum position values
  for the specified scroll bar.
 ------------------------------------------------------------------------------}
function SetScrollRange(Handle: HWND; nBar, nMinPos, nMaxPos: Integer; bRedraw: Boolean): Boolean;
var
  Info: TScrollInfo;
begin
  Info.fMask := SIF_RANGE;
  Info.nMin := nMinPos;
  Info.nMAx := nMaxPos;
  SetScrollInfo(Handle, nBar, Info, bRedraw);
  Result := True;
end;

{------------------------------------------------------------------------------
  Function: SetTimer
  Params: hWnd:
          nIDEvent:
          uElapse:
          lpTimerFunc:
  Returns:

 ------------------------------------------------------------------------------}
function SetTimer(Wnd: HWND; nIDEvent: UINT_PTR; uElapse: UINT;  lpTimerFunc: TTimerProc): UINT_PTR;
var
  MapID: record
    case Boolean of
      True: (nIDEvent: UINT_PTR; Wnd: HWND);
      False: (filler: array[1..2] of Pointer);
  end;
  ID: Cardinal;
  Info: TTimerInfo;
  InfoPtr: PTimerInfo;
  Existing: Boolean;
begin
  if MTimerMap = nil
  then MTimerMap := TMap.Create({$ifdef CPU32}itu8{$else}itu16{$endif}, SizeOf(Cardinal));
  if MTimerInfo = nil
  then MTimerInfo := TMap.Create(itu4, SizeOf(TTimerInfo));

  // make sure all is zero
  FillByte(MapID, SizeOf(MapID), 0);
  MapID.Wnd := Wnd;
  MapID.nIDEvent := nIdEvent;

  InfoPtr := nil;
  Existing := MTimerMap.GetData(MapID, ID);
  if Existing
  then begin
    InfoPtr := MTimerInfo.GetDataPtr(ID);
    if InfoPtr = nil
    then Existing := False
    else Widgetset.DestroyTimer(InfoPtr^.Handle);
  end;

  if not Existing
  then begin
    // new id
    Inc(MTimerSeq);
    ID := MTimerSeq;
    Info.Wnd := Wnd;
    // for null windows we create an ID else the ID passed is used
    if Wnd = 0
    then Info.IDEvent := ID
    else Info.IDEvent := nIdEvent;
    MapID.nIDEvent := Info.IDEvent;
    MTimerMap.Add(MapID, ID);
    InfoPtr := @Info;
  end;

  InfoPtr^.TimerProc := lpTimerFunc;
  InfoPtr^.Handle := WidgetSet.CreateTimer(uElapse, @TTimerID(PtrUInt(ID)).TimerNotify);

  if not Existing
  then MTimerInfo.Add(ID, Info); // add after all data is set

  if InfoPtr^.Handle <> 0
  then Exit(ID); // success

  // cleanup
  MTimerInfo.Delete(ID);
  MTimerMap.Delete(MapID);
  Result := 0;
end;

{------------------------------------------------------------------------------
  Function:
  Params:
  Returns:

 ------------------------------------------------------------------------------}
function SmallPointtoPoint(const P : TSmallPoint) : Tpoint;
begin
  Result.X := P.X;
  Result.Y := P.Y;
end;

{------------------------------------------------------------------------------
  Function: SubtractRect
  Params: lprcDst:
          lprcSrc1:
          lprcSrc2:
  Returns:

  The resulting rectangle is the bounding box of the geometric difference
 ------------------------------------------------------------------------------}
function SubtractRect(out lprcDst: TRect; const lprcSrc1, lprcSrc2: TRect): Boolean;
var
  L,R,T,B: Boolean;
begin
  // check overlapping edges
  L := lprcSrc2.Left <= lprcSrc1.Left;
  R := lprcSrc2.Right >= lprcSrc1.Right;
  T := lprcSrc2.Top <= lprcSrc1.Top;
  B := lprcSrc2.Bottom >= lprcSrc1.Bottom;

  // assume no change
  lprcDst := lprcSrc1;

  if L and R
  then begin
    // left and right overlap, so there is somthing to substract from top or bottom
    if T
    then begin
      if B
      then begin
        // empty rect;
        lprcDst.Right := lprcSrc1.Left;
        lprcDst.Bottom := lprcSrc1.Top;
        Exit(False);
      end;
      lprcDst.Top := lprcSrc2.Bottom;
      Exit(True);
    end;

    if B
    then lprcDst.Bottom := lprcSrc2.Top;
    Exit(True);
  end;

  if T and B
  then begin
    // top and bottom overlap, so there is somthing to substract from either left or right
    if L
    then lprcDst.Left := lprcSrc2.Right
    else
      if R
      then lprcDst.Right := lprcSrc2.Left;
  end;

  Result := True;
end;

{------------------------------------------------------------------------------
  Function: UnionRect          pbd
  Params: var DestRect: TRect; const SrcRect1, SrcRect2: TRect
  Returns: Boolean 0 on failure

  Creates the union rectangle of SrcRect1 and SrcRect2 into DestRect.
  The union rectangle encloses SrcRect1 and SrcRect2.
 ------------------------------------------------------------------------------}
function UnionRect(var DestRect: TRect;
  const SrcRect1, SrcRect2: TRect): Boolean;
begin
  if IsRectEmpty(SrcRect1) then
    DestRect := SrcRect2
  else
  if IsRectEmpty(SrcRect2) then
    DestRect := SrcRect1
  else begin
    DestRect.Left := Min(SrcRect1.Left, SrcRect2.Left);
    DestRect.Top := Min(SrcRect1.Top, SrcRect2.Top);
    DestRect.Right := Max(SrcRect1.Right, SrcRect2.Right);
    DestRect.Bottom := Max(SrcRect1.Bottom, SrcRect2.Bottom);
  end;
  Result := not IsRectEmpty(DestRect);
end;

//##apiwiz##epi##   // Do not remove
