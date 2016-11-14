{%MainUnit customdrawnint.pas}


function Arc(DC: HDC; Left,Top,Right,Bottom,angle1,angle2 : Integer): Boolean; override;
function BeginPaint(Handle: hWnd; Var PS : TPaintStruct) : hdc; override;
function BitBlt(DestDC: HDC; X, Y, Width, Height: Integer; SrcDC: HDC; XSrc, YSrc: Integer; Rop: DWORD): Boolean; override;
function ClientToScreen(Handle: HWND; var P: TPoint) : Boolean; override;

function ClipboardFormatToMimeType(FormatID: TClipboardFormat): string; override;
function ClipboardGetData(ClipboardType: TClipboardType;
  FormatID: TClipboardFormat; Stream: TStream): boolean; override;
function ClipboardGetFormats(ClipboardType: TClipboardType;
  var Count: integer; var List: PClipboardFormat): boolean; override;
function ClipboardGetOwnerShip(ClipboardType: TClipboardType;
  OnRequestProc: TClipboardRequestEvent;  FormatCount: integer;
  Formats: PClipboardFormat): boolean; override;
function ClipboardRegisterFormat(const AMimeType: string): TClipboardFormat; override;

function CombineRgn(Dest, Src1, Src2: HRGN; fnCombineMode: Longint): Longint;  override;
function CreateBitmap(Width, Height: Integer; Planes, BitCount: Longint; BitmapBits: Pointer): HBITMAP; override;
function CreateBrushIndirect(const LogBrush: TLogBrush): HBRUSH; override;
function CreateCompatibleBitmap(DC: HDC; Width, Height: Integer): HBITMAP; override;
function CreateCompatibleDC(DC: HDC): HDC; override;
function CreateEllipticRgn(p1, p2, p3, p4: Integer): HRGN; override;
function CreateFontIndirect(const LogFont: TLogFont): HFONT; override;
function CreateFontIndirectEx(const LogFont: TLogFont; const LongFontName: string): HFONT; override;
function CreateIconIndirect(IconInfo: PIconInfo): HICON; override;
function CreatePenIndirect(const LogPen: TLogPen): HPEN; override;
function CreatePolygonRgn(Points: PPoint; NumPts: Integer; FillMode: integer): HRGN; override;
function CreateRectRgn(X1, Y1, X2, Y2: Integer): HRGN; override;

procedure DeleteCriticalSection(var CritSection: TCriticalSection); override;
function DeleteDC(hDC: HDC): Boolean; override;
function DeleteObject(GDIObject: HGDIOBJ): Boolean; override;
function Ellipse(DC: HDC; x1, y1, x2, y2: Integer): Boolean; override;
procedure EnterCriticalSection(var CritSection: TCriticalSection); override;
function ExtCreatePen(dwPenStyle, dwWidth: DWord; const lplb: TLogBrush; dwStyleCount: DWord; lpStyle: PDWord): HPEN; override;
function ExtSelectClipRGN(dc: hdc; rgn : hrgn; Mode : Longint) : Integer;  override;

function FillRect(DC: HDC; const Rect: TRect; Brush: HBRUSH): Boolean; override;
function FillRgn(DC: HDC; RegionHnd: HRGN; hbr: HBRUSH): Bool; override;
function FrameRect(DC: HDC; const ARect: TRect; hBr: HBRUSH): Integer; override;

function GetClientRect(handle : HWND; var ARect : TRect) : Boolean; override;
function GetClipBox(DC : hDC; lpRect : PRect) : Longint; override;
function GetClipRGN(DC: hDC; RGN: hRGN): Longint; override;
function GetCurrentObject(DC: HDC; uObjectType: UINT): HGDIOBJ; override;
function GetKeyState(nVirtKey: Integer): Smallint; override;
function GetRgnBox(RGN : HRGN; lpRect : PRect) : Longint; override;
function GetStockObject(Value: Integer): THandle; override;
function GetSystemMetrics(nIndex: Integer): Integer; override;
function GetTextColor(DC: HDC) : TColorRef; Override;
function GetWindowOrgEx(dc : hdc; P : PPoint): Integer; override;
function BackendGetWindowRelativePosition(Handle: hwnd; var Left, Top: Integer): boolean;
function InvalidateRgn(aHandle: HWND; Rgn: HRGN; Erase: Boolean): Boolean; override;
procedure InitializeCriticalSection(var CritSection: TCriticalSection); override;

procedure LeaveCriticalSection(var CritSection: TCriticalSection); override;
function LineTo(DC: HDC; X, Y: Integer): Boolean; override;

function MoveToEx(DC: HDC; X, Y: Integer; OldPoint: PPoint): Boolean; override;

function PolyBezier(DC: HDC; Points: PPoint; NumPts: Integer; Filled, Continuous: boolean): boolean; override;
function Polygon(DC: HDC; Points: PPoint; NumPts: Integer; Winding: boolean): boolean; override;
function Polyline(DC: HDC; Points: PPoint; NumPts: Integer): boolean; override;
function PtInRegion(RGN: HRGN; X, Y: Integer) : Boolean; override;

function Rectangle(DC: HDC; X1, Y1, X2, Y2: Integer): Boolean; override;
function RectVisible(dc : hdc; const ARect: TRect) : Boolean; override;
function RestoreDC(DC: HDC; SavedDC: Integer): Boolean; override;

function SaveDC(DC: HDC): Integer; override;
function ScreenToClient(Handle : HWND; var P : TPoint) : Integer; override;
//function ScrollWindowEx(HWnd: HWND; DX, DY: Integer; PRcScroll, PRcClip: PRect; HRgnUpdate: HRGN; PRcUpdate: PRect; Flags: UINT): Boolean; override;
function SelectClipRGN(DC : hDC; RGN : HRGN) : Longint; override;
function SelectObject(DC: HDC; GDIObj: HGDIOBJ): HGDIOBJ; override;
function SetTextColor(DC: HDC; Color: TColorRef): TColorRef; override;
function SetWindowOrgEx(DC : HDC; NewX, NewY : Integer; OldPoint: PPoint) : Boolean; override;
function StretchBlt(DestDC: HDC; X, Y, Width, Height: Integer;
  SrcDC: HDC; XSrc, YSrc, SrcWidth, SrcHeight: Integer; ROp: Cardinal): Boolean; override;
function StretchMaskBlt(DestDC: HDC; X, Y, Width, Height: Integer;
  SrcDC: HDC; XSrc, YSrc, SrcWidth, SrcHeight: Integer; Mask: HBITMAP;
  XMask, YMask: Integer; Rop: DWORD): Boolean; override;

function TextOut(DC: HDC; X,Y : Integer; Str : Pchar; Count: Integer) : Boolean; override;



