{                        ------------------------------
                               winproc.pp
                         ------------------------------

 Misc types and procedures for LCL-CustomDrawn-Windows

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}

unit customdrawn_winproc;

{$mode objfpc}{$H+}

interface

uses
  Windows, CTypes, Classes, SysUtils,
  // LCL
  LCLType, Interfacebase, LMessages, lclintf, LCLProc,
  Forms, graphtype, IntfGraphics, lazcanvas,
  //
  customdrawnproc;

type
  MCHITTESTINFO = record
    cbSize: UINT;
    pt    : TPoint;
    uHit  : UINT;          // out param
    st    : SYSTEMTIME;
  end;
  TMCMHitTestInfo = MCHITTESTINFO;
  PMCMHitTestInfo = ^TMCMHitTestInfo;

  // Window information snapshot
  tagWINDOWINFO = record
    cbSize: DWORD;
    rcWindow: TRect;
    rcClient: TRect;
    dwStyle: DWORD;
    dwExStyle: DWORD;
    dwWindowStatus: DWORD;
    cxWindowBorders: UINT;
    cyWindowBorders: UINT;
    atomWindowType: ATOM;
    wCreatorVersion: WORD;
  end;
  PTAGWINDOWINFO = ^tagWINDOWINFO;

type
  { lazarus win32 Interface definition for additional timer data needed to find the callback}
  PWinCETimerInfo = ^TWinCETimerinfo;
  TWinCETimerInfo = record
    TimerID: UINT_PTR;         // the windows timer ID for this timer
    TimerFunc: TWSTimerProc; // owner function to handle timer
  end;

{$ifdef WinCE}
function EnumDisplayMonitors(hdc: HDC; lprcClip: PRect; lpfnEnum: MONITORENUMPROC; dwData: LPARAM): LongBool; cdecl; external KernelDLL name 'EnumDisplayMonitors';
function GetMonitorInfoW(hMonitor: HMONITOR; lpmi: PMonitorInfo): LongBool; cdecl; external KernelDLL name 'GetMonitorInfo';
function MonitorFromWindow(hWnd: HWND; dwFlags: DWORD): HMONITOR; cdecl; external KernelDLL name 'MonitorFromWindow';
function MonitorFromRect(lprcScreenCoords: PRect; dwFlags: DWord): HMONITOR; cdecl; external KernelDLL name 'MonitorFromRect';
function MonitorFromPoint(ptScreenCoords: TPoint; dwFlags: DWord): HMONITOR; cdecl; external KernelDLL name 'MonitorFromPoint';
{$else}
function EnumDisplayMonitors(hdc: HDC; lprcClip: PRect; lpfnEnum: MONITORENUMPROC; dwData: LPARAM): LongBool; stdcall; external 'user32.dll' name 'EnumDisplayMonitors';
function GetMonitorInfoW(hMonitor: HMONITOR; lpmi: PMonitorInfo): LongBool; stdcall; external 'user32.dll' name 'GetMonitorInfoW';
function MonitorFromWindow(hWnd: HWND; dwFlags: DWORD): HMONITOR; stdcall; external 'user32.dll' name 'MonitorFromWindow';
function MonitorFromRect(lprcScreenCoords: PRect; dwFlags: DWord): HMONITOR; stdcall; external 'user32.dll' name 'MonitorFromRect';
function MonitorFromPoint(ptScreenCoords: TPoint; dwFlags: DWord): HMONITOR; stdcall; external 'user32.dll' name 'MonitorFromPoint';
// from win32extra.pp
function GetWindowInfo(hwnd: HWND; pwi: PTAGWINDOWINFO): BOOL; stdcall; external 'user32.dll' name 'GetWindowInfo';
{$endif}

type
  TMouseDownFocusStatus = (mfNone, mfFocusSense, mfFocusChanged);

  PProcessEvent = ^TProcessEvent;
  TProcessEvent = record
    Handle: THandle;
    Handler: PEventHandler;
    UserData: PtrInt;
    OnEvent: TChildExitEvent;
  end;

var
  // FTimerData contains the currently running timers
  FTimerData : TList;   // list of PWin32Timerinfo

  MouseDownTime: dword;
  MouseDownPos: TPoint;
  MouseDownWindow: HWND = 0;
  MouseDownFocusWindow: HWND;
  MouseDownFocusStatus: TMouseDownFocusStatus = mfNone;
  ComboBoxHandleSizeWindow: HWND = 0;//just do not know the use yet
  IgnoreNextCharWindow: HWND = 0;  // ignore next WM_(SYS)CHAR message
  OnClipBoardRequest: TClipboardRequestEvent = nil;

type
  TEventType = (etNotify, etKey, etKeyPress, etMouseWheel, etMouseUpDown);

  PStayOnTopWindowsInfo = ^TStayOnTopWindowsInfo;
  TStayOnTopWindowsInfo = record
    AppHandle: HWND;
    SystemTopAlso: Boolean;
    StayOnTopList: TList;
  end;

  TWindowsVersion = (
    wvUnknown,
    //
    wince_1,
    wince_2,
    wince_3,
    wince_4,
    wince_5,
    wince_6,
    wince_6_1,
    wince_6_5,
    wince_7,
    wince_other,
    //
    wv95,
    wvNT4,
    wv98,
    wvMe,
    wv2000,
    wvXP,
    wvServer2003,
    //wvServer2003R2,  // has the same major/minor as wvServer2003
    wvVista,
    //wvServer2008,    // has the same major/minor as wvVista
    wv7,
    wv8,
    wvLater
  );

function WM_To_String(WM_Message: Integer): string;
function WindowPosFlagsToString(Flags: UINT): string;
function ObjectToHWND(Const AObject: TObject): HWND;

function BytesPerLine(nWidth, nBitsPerPixel: Integer): PtrUInt;
function CreateDIBSectionFromDescription(ADC: HDC; const ADesc: TRawImageDescription; out ABitsPtr: Pointer): HBITMAP;
procedure FillRawImageDescriptionColors(var ADesc: TRawImageDescription);
procedure FillRawImageDescription(const ABitmapInfo: Windows.TBitmap; out ADesc: TRawImageDescription);
function WinProc_RawImage_FromBitmap(out ARawImage: TRawImage; ABitmap, AMask: HBITMAP; ARect: PRect = nil): Boolean;
function WinProc_RawImage_CreateBitmaps(const ARawImage: TRawImage; out ABitmap, AMask: HBitmap; ASkipMask: Boolean): Boolean;

{$ifndef WinCE}
function GetBitmapOrder(AWinBmp: Windows.TBitmap; ABitmap: HBITMAP):TRawImageLineOrder;
{$endif}
function GetBitmapBytes(AWinBmp: Windows.TBitmap; ABitmap: HBITMAP; const ARect: TRect; ALineEnd: TRawImageLineEnd; ALineOrder: TRawImageLineOrder; out AData: Pointer; out ADataSize: PtrUInt): Boolean;
function IsAlphaBitmap(ABitmap: HBITMAP): Boolean;
function IsAlphaDC(ADC: HDC): Boolean;

function GetLastErrorText(AErrorCode: Cardinal): WideString;

procedure GetWin32ControlPos(Window, Parent: HWND; var Left, Top: integer);

procedure UpdateWindowStyle(Handle: HWnd; Style: integer; StyleMask: integer);

function GetFileVersion(FileName: string): dword;

procedure RemoveStayOnTopFlags(AppHandle: HWND; ASystemTopAlso: Boolean = False);
procedure RestoreStayOnTopFlags(AppHandle: HWND);

procedure AddToChangedMenus(Window: HWnd);
procedure RedrawMenus;
function GetControlText(AHandle: HWND): string;

{ String functions that may be moved to the RTL in the future }
procedure WideStrCopy(Dest, Src: PWideChar);
function WideStrLCopy(dest, source: PWideChar; maxlen: SizeInt): PWideChar;
function WideStrCmp(W1, W2: PWideChar): Integer;

{ Automatic detection of platform }

function IsHiResMode: Boolean;
procedure UpdateWindowsVersion;

var
  DefaultWindowInfo: TWindowInfo;
  WindowInfoAtom: ATOM;
  OverwriteCheck: Integer = 0;
  ChangedMenus: TList; // list of HWNDs which menus needs to be redrawn

  WindowsVersion: TWindowsVersion = wvUnknown;

const
  ClsName: array[0..6] of WideChar = ('W', 'i', 'n', 'd', 'o', 'w', #0);
  ClsHintName: array[0..10] of WideChar = ('H', 'i', 'n', 't', 'W', 'i', 'n', 'd', 'o', 'w', #0);

implementation

uses customdrawnint;

var
  InRemoveStayOnTopFlags: Integer = 0;

{------------------------------------------------------------------------------
  Function: WM_To_String
  Params: WM_Message - a WinDows message
  Returns: A WinDows-message name

  Converts a winDows message identIfier to a string
 ------------------------------------------------------------------------------}
function WM_To_String(WM_Message: Integer): string;
Begin
 Case WM_Message of
  $0000: Result := 'WM_NULL';
  $0001: Result := 'WM_CREATE';
  $0002: Result := 'WM_DESTROY';
  $0003: Result := 'WM_MOVE';
  $0005: Result := 'WM_SIZE';
  $0006: Result := 'WM_ACTIVATE';
  $0007: Result := 'WM_SETFOCUS';
  $0008: Result := 'WM_KILLFOCUS';
  $000A: Result := 'WM_ENABLE';
  $000B: Result := 'WM_SETREDRAW';
  $000C: Result := 'WM_SETTEXT';
  $000D: Result := 'WM_GETTEXT';
  $000E: Result := 'WM_GETTEXTLENGTH';
  $000F: Result := 'WM_PAINT';
  $0010: Result := 'WM_CLOSE';
  $0011: Result := 'WM_QUERYENDSESSION';
  $0012: Result := 'WM_QUIT';
  $0013: Result := 'WM_QUERYOPEN';
  $0014: Result := 'WM_ERASEBKGND';
  $0015: Result := 'WM_SYSCOLORCHANGE';
  $0016: Result := 'WM_EndSESSION';
  $0017: Result := 'WM_SYSTEMERROR';
  $0018: Result := 'WM_SHOWWINDOW';
  $0019: Result := 'WM_CTLCOLOR';
  $001A: Result := 'WM_WININICHANGE or WM_SETTINGCHANGE';
  $001B: Result := 'WM_DEVMODECHANGE';
  $001C: Result := 'WM_ACTIVATEAPP';
  $001D: Result := 'WM_FONTCHANGE';
  $001E: Result := 'WM_TIMECHANGE';
  $001F: Result := 'WM_CANCELMODE';
  $0020: Result := 'WM_SETCURSOR';
  $0021: Result := 'WM_MOUSEACTIVATE';
  $0022: Result := 'WM_CHILDACTIVATE';
  $0023: Result := 'WM_QUEUESYNC';
  $0024: Result := 'WM_GETMINMAXINFO';
  $0026: Result := 'WM_PAINTICON';
  $0027: Result := 'WM_ICONERASEBKGND';
  $0028: Result := 'WM_NEXTDLGCTL';
  $002A: Result := 'WM_SPOOLERSTATUS';
  $002B: Result := 'WM_DRAWITEM';
  $002C: Result := 'WM_MEASUREITEM';
  $002D: Result := 'WM_DELETEITEM';
  $002E: Result := 'WM_VKEYTOITEM';
  $002F: Result := 'WM_CHARTOITEM';
  $0030: Result := 'WM_SETFONT';
  $0031: Result := 'WM_GETFONT';
  $0032: Result := 'WM_SETHOTKEY';
  $0033: Result := 'WM_GETHOTKEY';
  $0037: Result := 'WM_QUERYDRAGICON';
  $0039: Result := 'WM_COMPAREITEM';
  $003D: Result := 'WM_GETOBJECT';
  $0041: Result := 'WM_COMPACTING';
  $0044: Result := 'WM_COMMNOTIFY { obsolete in Win32}';
  $0046: Result := 'WM_WINDOWPOSCHANGING';
  $0047: Result := 'WM_WINDOWPOSCHANGED';
  $0048: Result := 'WM_POWER';
  $004A: Result := 'WM_COPYDATA';
  $004B: Result := 'WM_CANCELJOURNAL';
  $004E: Result := 'WM_NOTIFY';
  $0050: Result := 'WM_INPUTLANGCHANGEREQUEST';
  $0051: Result := 'WM_INPUTLANGCHANGE';
  $0052: Result := 'WM_TCARD';
  $0053: Result := 'WM_HELP';
  $0054: Result := 'WM_USERCHANGED';
  $0055: Result := 'WM_NOTIFYFORMAT';
  $007B: Result := 'WM_CONTEXTMENU';
  $007C: Result := 'WM_STYLECHANGING';
  $007D: Result := 'WM_STYLECHANGED';
  $007E: Result := 'WM_DISPLAYCHANGE';
  $007F: Result := 'WM_GETICON';
  $0080: Result := 'WM_SETICON';
  $0081: Result := 'WM_NCCREATE';
  $0082: Result := 'WM_NCDESTROY';
  $0083: Result := 'WM_NCCALCSIZE';
  $0084: Result := 'WM_NCHITTEST';
  $0085: Result := 'WM_NCPAINT';
  $0086: Result := 'WM_NCACTIVATE';
  $0087: Result := 'WM_GETDLGCODE';
  $00A0: Result := 'WM_NCMOUSEMOVE';
  $00A1: Result := 'WM_NCLBUTTONDOWN';
  $00A2: Result := 'WM_NCLBUTTONUP';
  $00A3: Result := 'WM_NCLBUTTONDBLCLK';
  $00A4: Result := 'WM_NCRBUTTONDOWN';
  $00A5: Result := 'WM_NCRBUTTONUP';
  $00A6: Result := 'WM_NCRBUTTONDBLCLK';
  $00A7: Result := 'WM_NCMBUTTONDOWN';
  $00A8: Result := 'WM_NCMBUTTONUP';
  $00A9: Result := 'WM_NCMBUTTONDBLCLK';
  $0100: Result := 'WM_KEYFIRST or WM_KEYDOWN';
  $0101: Result := 'WM_KEYUP';
  $0102: Result := 'WM_CHAR';
  $0103: Result := 'WM_DEADCHAR';
  $0104: Result := 'WM_SYSKEYDOWN';
  $0105: Result := 'WM_SYSKEYUP';
  $0106: Result := 'WM_SYSCHAR';
  $0107: Result := 'WM_SYSDEADCHAR';
  $0108: Result := 'WM_KEYLAST';
  $010D: Result := 'WM_IME_STARTCOMPOSITION';
  $010E: Result := 'WM_IME_ENDCOMPOSITION';
  $010F: Result := 'WM_IME_COMPOSITION or WM_IME_KEYLAST';
  $0110: Result := 'WM_INITDIALOG';
  $0111: Result := 'WM_COMMAND';
  $0112: Result := 'WM_SYSCOMMAND';
  $0113: Result := 'WM_TIMER';
  $0114: Result := 'WM_HSCROLL';
  $0115: Result := 'WM_VSCROLL';
  $0116: Result := 'WM_INITMENU';
  $0117: Result := 'WM_INITMENUPOPUP';
  $011F: Result := 'WM_MENUSELECT';
  $0120: Result := 'WM_MENUCHAR';
  $0121: Result := 'WM_ENTERIDLE';
  $0122: Result := 'WM_MENURBUTTONUP';
  $0123: Result := 'WM_MENUDRAG';
  $0124: Result := 'WM_MENUGETOBJECT';
  $0125: Result := 'WM_UNINITMENUPOPUP';
  $0126: Result := 'WM_MENUCOMMAND';
  $0132: Result := 'WM_CTLCOLORMSGBOX';
  $0133: Result := 'WM_CTLCOLOREDIT';
  $0134: Result := 'WM_CTLCOLORLISTBOX';
  $0135: Result := 'WM_CTLCOLORBTN';
  $0136: Result := 'WM_CTLCOLORDLG';
  $0137: Result := 'WM_CTLCOLORSCROLLBAR';
  $0138: Result := 'WM_CTLCOLORSTATIC';
  $0200: Result := 'WM_MOUSEFIRST or WM_MOUSEMOVE';
  $0201: Result := 'WM_LBUTTONDOWN';
  $0202: Result := 'WM_LBUTTONUP';
  $0203: Result := 'WM_LBUTTONDBLCLK';
  $0204: Result := 'WM_RBUTTONDOWN';
  $0205: Result := 'WM_RBUTTONUP';
  $0206: Result := 'WM_RBUTTONDBLCLK';
  $0207: Result := 'WM_MBUTTONDOWN';
  $0208: Result := 'WM_MBUTTONUP';
  $0209: Result := 'WM_MBUTTONDBLCLK';
  $020A: Result := 'WM_MOUSEWHEEL or WM_MOUSELAST';
  $0210: Result := 'WM_PARENTNOTIFY';
  $0211: Result := 'WM_ENTERMENULOOP';
  $0212: Result := 'WM_EXITMENULOOP';
  $0213: Result := 'WM_NEXTMENU';
  $0214: Result := 'WM_SIZING';
  $0215: Result := 'WM_CAPTURECHANGED';
  $0216: Result := 'WM_MOVING';
  $0218: Result := 'WM_POWERBROADCAST';
  $0219: Result := 'WM_DEVICECHANGE';
  $0220: Result := 'WM_MDICREATE';
  $0221: Result := 'WM_MDIDESTROY';
  $0222: Result := 'WM_MDIACTIVATE';
  $0223: Result := 'WM_MDIRESTORE';
  $0224: Result := 'WM_MDINEXT';
  $0225: Result := 'WM_MDIMAXIMIZE';
  $0226: Result := 'WM_MDITILE';
  $0227: Result := 'WM_MDICASCADE';
  $0228: Result := 'WM_MDIICONARRANGE';
  $0229: Result := 'WM_MDIGETACTIVE';
  $0230: Result := 'WM_MDISETMENU';
  $0231: Result := 'WM_ENTERSIZEMOVE';
  $0232: Result := 'WM_EXITSIZEMOVE';
  $0233: Result := 'WM_DROPFILES';
  $0234: Result := 'WM_MDIREFRESHMENU';
  $0281: Result := 'WM_IME_SETCONTEXT';
  $0282: Result := 'WM_IME_NOTIFY';
  $0283: Result := 'WM_IME_CONTROL';
  $0284: Result := 'WM_IME_COMPOSITIONFULL';
  $0285: Result := 'WM_IME_SELECT';
  $0286: Result := 'WM_IME_CHAR';
  $0288: Result := 'WM_IME_REQUEST';
  $0290: Result := 'WM_IME_KEYDOWN';
  $0291: Result := 'WM_IME_KEYUP';
  $02A1: Result := 'WM_MOUSEHOVER';
  $02A3: Result := 'WM_MOUSELEAVE';
  $0300: Result := 'WM_CUT';
  $0301: Result := 'WM_COPY';
  $0302: Result := 'WM_PASTE';
  $0303: Result := 'WM_CLEAR';
  $0304: Result := 'WM_UNDO';
  $0305: Result := 'WM_RENDERFORMAT';
  $0306: Result := 'WM_RENDERALLFORMATS';
  $0307: Result := 'WM_DESTROYCLIPBOARD';
  $0308: Result := 'WM_DRAWCLIPBOARD';
  $0309: Result := 'WM_PAINTCLIPBOARD';
  $030A: Result := 'WM_VSCROLLCLIPBOARD';
  $030B: Result := 'WM_SIZECLIPBOARD';
  $030C: Result := 'WM_ASKCBFORMATNAME';
  $030D: Result := 'WM_CHANGECBCHAIN';
  $030E: Result := 'WM_HSCROLLCLIPBOARD';
  $030F: Result := 'WM_QUERYNEWPALETTE';
  $0310: Result := 'WM_PALETTEISCHANGING';
  $0311: Result := 'WM_PALETTECHANGED';
  $0312: Result := 'WM_HOTKEY';
  $0317: Result := 'WM_PRINT';
  $0318: Result := 'WM_PRINTCLIENT';
  $0358: Result := 'WM_HANDHELDFIRST';
  $035F: Result := 'WM_HANDHELDLAST';
  $0380: Result := 'WM_PENWINFIRST';
  $038F: Result := 'WM_PENWINLAST';
  $0390: Result := 'WM_COALESCE_FIRST';
  $039F: Result := 'WM_COALESCE_LAST';
  $03E0: Result := 'WM_DDE_FIRST or WM_DDE_INITIATE';
  $03E1: Result := 'WM_DDE_TERMINATE';
  $03E2: Result := 'WM_DDE_ADVISE';
  $03E3: Result := 'WM_DDE_UNADVISE';
  $03E4: Result := 'WM_DDE_ACK';
  $03E5: Result := 'WM_DDE_DATA';
  $03E6: Result := 'WM_DDE_REQUEST';
  $03E7: Result := 'WM_DDE_POKE';
  $03E8: Result := 'WM_DDE_EXECUTE or WM_DDE_LAST';
  $0400: Result := 'WM_USER';
  $8000: Result := 'WM_APP';
  Else
    Result := 'Unknown(' + IntToStr(WM_Message) + ')';
  End; {Case}
End;

function WindowPosFlagsToString(Flags: UINT): string;
var
  FlagsStr: string;
begin
  FlagsStr := '';
  if (Flags and SWP_DRAWFRAME) <> 0 then
    FlagsStr := FlagsStr + '|SWP_DRAWFRAME';
  if (Flags and SWP_HIDEWINDOW) <> 0 then
    FlagsStr := FlagsStr + '|SWP_HIDEWINDOW';
  if (Flags and SWP_NOACTIVATE) <> 0 then
    FlagsStr := FlagsStr + '|SWP_NOACTIVATE';
  if (Flags and SWP_NOCOPYBITS) <> 0 then
    FlagsStr := FlagsStr + '|SWP_NOCOPYBITS';
  if (Flags and SWP_NOMOVE) <> 0 then
    FlagsStr := FlagsStr + '|SWP_NOMOVE';
  if (Flags and SWP_NOOWNERZORDER) <> 0 then
    FlagsStr := FlagsStr + '|SWP_NOOWNERZORDER';
  if (Flags and SWP_NOREDRAW) <> 0 then
    FlagsStr := FlagsStr + '|SWP_NOREDRAW';
  if (Flags and SWP_NOSENDCHANGING) <> 0 then
    FlagsStr := FlagsStr + '|SWP_NOSENDCHANGING';
  if (Flags and SWP_NOSIZE) <> 0 then
    FlagsStr := FlagsStr + '|SWP_NOSIZE';
  if (Flags and SWP_NOZORDER) <> 0 then
    FlagsStr := FlagsStr + '|SWP_NOZORDER';
  if (Flags and SWP_SHOWWINDOW) <> 0 then
    FlagsStr := FlagsStr + '|SWP_SHOWWINDOW';
  if Length(FlagsStr) > 0 then
    FlagsStr := Copy(FlagsStr, 2, Length(FlagsStr)-1);
  Result := FlagsStr;
end;

{------------------------------------------------------------------------------
  Procedure: GetWin32KeyInfo
  Params:  Event      - Requested info
           KeyCode    - the ASCII key code of the eventkey
           VirtualKey - the virtual key code of the eventkey
           SysKey     - True If the key is a syskey
           ExtEnded   - True If the key is an extended key
           Toggle     - True If the key is a toggle key and its value is on
  Returns: Nothing

  GetWin32KeyInfo returns information about the given key event
 ------------------------------------------------------------------------------}
{
procedure GetWin32KeyInfo(const Event: Integer; var KeyCode, VirtualKey: Integer; var SysKey, Extended, Toggle: Boolean);
Const
  MVK_UNIFY_SIDES = 1;
Begin
  //DebugLn('TRACE:Using function GetWin32KeyInfo which isn''t implemented yet');
  KeyCode := Word(Event);
  VirtualKey := MapVirtualKey(KeyCode, MVK_UNIFY_SIDES);
  SysKey := (VirtualKey = VK_SHIFT) Or (VirtualKey = VK_CONTROL) Or (VirtualKey = VK_MENU);
  ExtEnded := (SysKey) Or (VirtualKey = VK_INSERT) Or (VirtualKey = VK_HOME) Or (VirtualKey = VK_LEFT) Or (VirtualKey = VK_UP) Or (VirtualKey = VK_RIGHT) Or (VirtualKey = VK_DOWN) Or (VirtualKey = VK_PRIOR) Or (VirtualKey = VK_NEXT) Or (VirtualKey = VK_END) Or (VirtualKey = VK_DIVIDE);
  Toggle := Lo(GetKeyState(VirtualKey)) = 1;
End;
}

{------------------------------------------------------------------------------
  Function: ObjectToHWND
  Params: AObject - An LCL Object
  Returns: The Window handle of the given object

  Returns the Window handle of the given object, 0 if no object available
 ------------------------------------------------------------------------------}
function ObjectToHWND(Const AObject: TObject): HWND;
Var
  Handle: HWND;
Begin
  Handle:=0;
end;

function BytesPerLine(nWidth, nBitsPerPixel: Integer): PtrUInt;
begin
  Result := ((nWidth * nBitsPerPixel + 31) and (not 31) ) div 8;
end;

procedure FillRawImageDescriptionColors(var ADesc: TRawImageDescription);
begin
  case ADesc.BitsPerPixel of
    1,4,8:
      begin
        // palette mode, no offsets
        ADesc.Format := ricfGray;
        ADesc.RedPrec := ADesc.BitsPerPixel;
        ADesc.GreenPrec := 0;
        ADesc.BluePrec := 0;
        ADesc.RedShift := 0;
        ADesc.GreenShift := 0;
        ADesc.BlueShift := 0;
      end;
    16:
      begin
        // 5-6-5 mode
        //roozbeh all changed from 5-5-5 to 5-6-5
        ADesc.RedPrec := 5;
        ADesc.GreenPrec := 6;
        ADesc.BluePrec := 5;
        ADesc.RedShift := 11;
        ADesc.GreenShift := 5;
        ADesc.BlueShift := 0;
        ADesc.Depth := 16;
      end;
    24:
      begin
        // 8-8-8 mode
        ADesc.RedPrec := 8;
        ADesc.GreenPrec := 8;
        ADesc.BluePrec := 8;
        ADesc.RedShift := 16;
        ADesc.GreenShift := 8;
        ADesc.BlueShift := 0;
      end;
  else    //  32:
    // 8-8-8-8 mode, high byte can be native alpha or custom 1bit maskalpha
    ADesc.AlphaPrec := 8;
    ADesc.RedPrec := 8;
    ADesc.GreenPrec := 8;
    ADesc.BluePrec := 8;
    ADesc.AlphaShift := 24;
    ADesc.RedShift := 16;
    ADesc.GreenShift := 8;
    ADesc.BlueShift := 0;
    ADesc.Depth := 32;
  end;
end;


procedure FillRawImageDescription(const ABitmapInfo: Windows.TBitmap; out ADesc: TRawImageDescription);
begin
  ADesc.Init;

  ADesc.Format := ricfRGBA;

  ADesc.Depth := ABitmapInfo.bmBitsPixel;             // used bits per pixel
  ADesc.Width := ABitmapInfo.bmWidth;
  ADesc.Height := ABitmapInfo.bmHeight;
  ADesc.BitOrder := riboReversedBits;
  ADesc.ByteOrder := riboLSBFirst;
  ADesc.LineOrder := riloTopToBottom;
  ADesc.BitsPerPixel := ABitmapInfo.bmBitsPixel;      // bits per pixel. can be greater than Depth.
  ADesc.LineEnd := rileDWordBoundary;

  if ABitmapInfo.bmBitsPixel <= 8
  then begin
    // each pixel is an index in the palette
    // TODO, ColorCount
    ADesc.PaletteColorCount := 0;
  end
  else ADesc.PaletteColorCount := 0;

  FillRawImageDescriptionColors(ADesc);

  ADesc.MaskBitsPerPixel := 1;
  ADesc.MaskShift := 0;
  ADesc.MaskLineEnd := rileWordBoundary; // CreateBitmap requires word boundary
  ADesc.MaskBitOrder := riboReversedBits;
end;

function WinProc_RawImage_FromBitmap(out ARawImage: TRawImage; ABitmap, AMask: HBITMAP; ARect: PRect = nil): Boolean;
var
  WinDIB: Windows.TDIBSection;
  WinBmp: Windows.TBitmap absolute WinDIB.dsBm;
  ASize: Integer;
  R: TRect;
begin
  ARawImage.Init;
  FillChar(WinDIB, SizeOf(WinDIB), 0);
  ASize := Windows.GetObject(ABitmap, SizeOf(WinDIB), @WinDIB);
  if ASize = 0
  then Exit(False);

  //DbgDumpBitmap(ABitmap, 'FromBitmap - Image');
  //DbgDumpBitmap(AMask, 'FromMask - Mask');

  FillRawImageDescription(WinBmp, ARawImage.Description);
  // if it is not DIB then alpha in bitmaps is not supported => use 0 alpha prec
  if ASize < SizeOf(WinDIB) then
    ARawImage.Description.AlphaPrec := 0;

  if ARect = nil
  then begin
    R := Rect(0, 0, WinBmp.bmWidth, WinBmp.bmHeight);
  end
  else begin
    R := ARect^;
    if R.Top > WinBmp.bmHeight then
      R.Top := WinBmp.bmHeight;
    if R.Bottom > WinBmp.bmHeight then
      R.Bottom := WinBmp.bmHeight;
    if R.Left > WinBmp.bmWidth then
      R.Left := WinBmp.bmWidth;
    if R.Right > WinBmp.bmWidth then
      R.Right := WinBmp.bmWidth;
  end;

  ARawImage.Description.Width := R.Right - R.Left;
  ARawImage.Description.Height := R.Bottom - R.Top;

  // copy bitmap
  Result := GetBitmapBytes(WinBmp, ABitmap, R, ARawImage.Description.LineEnd, ARawImage.Description.LineOrder, ARawImage.Data, ARawImage.DataSize);

  // check mask
  if AMask <> 0 then
  begin
    if Windows.GetObject(AMask, SizeOf(WinBmp), @WinBmp) = 0
    then Exit(False);

    Result := GetBitmapBytes(WinBmp, AMask, R, ARawImage.Description.MaskLineEnd, ARawImage.Description.LineOrder, ARawImage.Mask, ARawImage.MaskSize);
  end
  else begin
    ARawImage.Description.MaskBitsPerPixel := 0;
  end;
end;

{------------------------------------------------------------------------------
  Function: RawImage_CreateBitmaps
  Params: ARawImage:
          ABitmap:
          AMask:
          ASkipMask: When set there is no mask created
  Returns:

 ------------------------------------------------------------------------------}
{$ifdef WinCE}
function WinProc_RawImage_CreateBitmaps(const ARawImage: TRawImage; out ABitmap, AMask: HBitmap; ASkipMask: Boolean): Boolean;
var
  ADesc: TRawImageDescription absolute ARawImage.Description;
  DC: HDC;
  BitsPtr: Pointer;
  DataSize: PtrUInt;
begin
  Result := False;
  AMask := 0;

  if not ((ADesc.BitsPerPixel = 1) and (ADesc.LineEnd = rileWordBoundary)) then
  begin
    DC := Windows.GetDC(0);
    AMask := 0;
    ABitmap := CreateDIBSectionFromDescription(DC, ADesc, BitsPtr);
    //DbgDumpBitmap(ABitmap, 'CreateBitmaps - Image');
    Windows.ReleaseDC(0, DC);

    Result := ABitmap <> 0;
    if not Result then Exit;
    if BitsPtr = nil then Exit;

    // copy the image data
    DataSize := BytesPerLine(ADesc.Width, ADesc.BitsPerPixel) * ADesc.Height;
    if DataSize > ARawImage.DataSize
    then DataSize := ARawImage.DataSize;
    Move(ARawImage.Data^, BitsPtr^, DataSize);
  end
  else
    ABitmap := Windows.CreateBitmap(ADesc.Width, ADesc.Height, 1, 1, ARawImage.Data);

  if ASkipMask then Exit(True);

  AMask := Windows.CreateBitmap(ADesc.Width, ADesc.Height, 1, 1, ARawImage.Mask);
  //DbgDumpBitmap(ABitmap, 'CreateBitmaps - Mask');
  Result := AMask <> 0;
end;
{$else}
function WinProc_RawImage_CreateBitmaps(const ARawImage: TRawImage; out ABitmap, AMask: HBitmap; ASkipMask: Boolean): Boolean;
var
  ADesc: TRawImageDescription absolute ARawImage.Description;

  function DoBitmap: Boolean;
  var
    DC: HDC;
    Info: record
      Header: Windows.TBitmapInfoHeader;
      Colors: array[0..1] of Cardinal; // reserve extra color for mono bitmaps
    end;
    DstLinePtr, SrcLinePtr: PByte;
    SrcPixelPtr, DstPixelPtr: PByte;
    DstLineSize, SrcLineSize: PtrUInt;
    x, y: Integer;
    Ridx, Gidx, Bidx, Aidx, Align, SrcBytes, DstBpp: Byte;
  begin
    if (ADesc.BitsPerPixel = 1) and (ADesc.LineEnd = rileWordBoundary)
    then begin
      // default BW, word aligned bitmap
      ABitmap := Windows.CreateBitmap(ADesc.Width, ADesc.Height, 1, 1, ARawImage.Data);
      Exit(ABitmap <> 0);
    end;

    // for 24 bits images, BPP can be 24 or 32
    // 32 shouldn't be use since we don't fill the alpha channel

    if ADesc.Depth = 24
    then DstBpp := 24
    else DstBpp := ADesc.BitsPerPixel;

    FillChar(Info, SizeOf(Info), 0);
    Info.Header.biSize := SizeOf(Info.Header);
    Info.Header.biWidth := ADesc.Width;
    if ADesc.LineOrder = riloTopToBottom
    then Info.Header.biHeight := -ADesc.Height // create top to bottom
    else Info.Header.biHeight := ADesc.Height; // create bottom to top
    Info.Header.biPlanes := 1;
    Info.Header.biBitCount := DstBpp;
    Info.Header.biCompression := BI_RGB;
    {Info.Header.biSizeImage := 0;}
    { first color is black, second color is white, for monochrome bitmap }
    Info.Colors[1] := $FFFFFFFF;

    DC := Windows.GetDC(0);
    // Use createDIBSection, since only devicedepth bitmaps can be selected into a DC
    // when they are created with createDIBitmap
    //  ABitmap := Windows.CreateDIBitmap(DC, Info.Header, CBM_INIT, ARawImage.Data, Windows.PBitmapInfo(@Info)^, DIB_RGB_COLORS);
    ABitmap := Windows.CreateDIBSection(DC, Windows.PBitmapInfo(@Info)^, DIB_RGB_COLORS, DstLinePtr, 0, 0);
    Windows.ReleaseDC(0, DC);

    if ABitmap = 0
    then begin
      Exit(False);
    end;
    if DstLinePtr = nil then Exit(False);

    DstLineSize := Windows.MulDiv(DstBpp, ADesc.Width, 8);
    // align to DWord
    Align := DstLineSize and 3;
    if Align > 0
    then Inc(DstLineSize, 4 - Align);

    SrcLinePtr := ARawImage.Data;
    SrcLineSize := ADesc.BytesPerLine;

    // copy the image data
    if ADesc.Depth >= 24
    then begin
      // check if a pixel copy is needed
      // 1) Windows uses alpha channel in 32 bpp modes, despite documentation statement that it is ignored. Tested under Windows XP SP3
      // Wine also relies on this undocumented behaviour!
      // So, we need to cut unused A-channel, otherwise we would get black image
      //
      // 2) incompatible channel order
      ADesc.GetRGBIndices(Ridx, Gidx, Bidx, Aidx);

      if ((ADesc.BitsPerPixel = 32) and (ADesc.Depth = 24))
      or (Bidx <> 0) or (Gidx <> 1) or (Ridx <> 2)
      then begin
        // copy pixels
        SrcBytes := ADesc.BitsPerPixel div 8;

        for y := 0 to ADesc.Height - 1 do
        begin
          DstPixelPtr := DstLinePtr;
          SrcPixelPtr := SrcLinePtr;
          for x := 0 to ADesc.Width - 1 do
          begin
            DstPixelPtr[0] := SrcPixelPtr[Bidx];
            DstPixelPtr[1] := SrcPixelPtr[Gidx];
            DstPixelPtr[2] := SrcPixelPtr[Ridx];

            Inc(DstPixelPtr, 3); //move to the next dest RGB triple
            Inc(SrcPixelPtr, SrcBytes);
          end;

          Inc(DstLinePtr, DstLineSize);
          Inc(SrcLinePtr, SrcLineSize);
        end;

        Exit(True);
      end;
    end;

    // no pixelcopy needed
    // check if we can move using one call
    if ADesc.LineEnd = rileDWordBoundary
    then begin
      Move(SrcLinePtr^, DstLinePtr^, DstLineSize * ADesc.Height);
      Exit(True);
    end;

    //Can't use just one move, as different alignment
    for y := 0 to ADesc.Height - 1 do
    begin
      Move(SrcLinePtr^, DstLinePtr^, DstLineSize);
      Inc(DstLinePtr, DstLineSize);
      Inc(SrcLinePtr, SrcLineSize);
    end;

    Result := True;
  end;

begin
  AMask := 0;
  Result := DoBitmap;
  if not Result then Exit;

  //DbgDumpBitmap(ABitmap, 'CreateBitmaps - Image');
  if ASkipMask then Exit;

  AMask := Windows.CreateBitmap(ADesc.Width, ADesc.Height, 1, 1, ARawImage.Mask);
  Result := AMask <> 0;
  //DbgDumpBitmap(AMask, 'CreateBitmaps - Mask');
end;
{$endif}

function CreateDIBSectionFromDescription(ADC: HDC; const ADesc: TRawImageDescription; out ABitsPtr: Pointer): HBITMAP;
  function GetMask(APrec, AShift: Byte): Cardinal;
  begin
    Result := ($FFFFFFFF shr (32-APrec)) shl AShift;
  end;

var
  Info: record
    Header: Windows.TBitmapInfoHeader;
    Colors: array[0..3] of Cardinal; // reserve extra color for colormasks
  end;
begin
  FillChar(Info, sizeof(Info), 0);
  Info.Header.biSize := sizeof(Windows.TBitmapInfoHeader);
  Info.Header.biWidth := ADesc.Width;
  Info.Header.biHeight := -ADesc.Height;
  Info.Header.biPlanes := 1;
  Info.Header.biBitCount := ADesc.BitsPerPixel;
  // TODO: palette support
  Info.Header.biClrUsed := 0;
  Info.Header.biClrImportant := 0;
  Info.Header.biSizeImage := BytesPerLine(Info.Header.biWidth, Info.Header.biBitCount) * ADesc.Height;
  // CE only supports bitfields
  if ADesc.BitsPerPixel > 8
  then Info.Header.biCompression := BI_BITFIELDS
  else Info.Header.biCompression := BI_RGB;

  if ADesc.BitsPerPixel = 1
  then begin
    // mono bitmap: first color is black, second is white
    Info.Colors[1] := $FFFFFFFF;
  end
  else begin
    // when 24bpp, CE only supports B8G8R8 encoding
    // TODO: check the description
    Info.Colors[0] := GetMask(ADesc.RedPrec, ADesc.RedShift);
    Info.Colors[1] := GetMask(ADesc.GreenPrec, ADesc.GreenShift);
    Info.Colors[2] := GetMask(ADesc.BluePrec, ADesc.BlueShift);
  end;

  // Use createDIBSection, since only devicedepth bitmaps can be selected into a DC
  // when they are created with createDIBitmap
  Result := Windows.CreateDIBSection(ADC, Windows.PBitmapInfo(@Info)^, DIB_RGB_COLORS, ABitsPtr, 0, 0);

  //DbgDumpBitmap(Result, 'CreateDIBSectionFromDescription - Image');
end;

function CreateDIBSectionFromDDB(ASource: HBitmap; out ABitsPtr: Pointer): HBitmap;
var
  ADC, SrcDC, DstDC: HDC;
  ADesc: TRawImageDescription;
  SrcOldBm, DstOldBm: HBitmap;
begin
  Result := 0;

  // get source bitmap description
  if not RawImage_DescriptionFromBitmap(ASource, ADesc) then
    Exit;

  // create apropriate dib section
  ADC := GetDC(0);
  Result := CreateDIBSectionFromDescription(ADC, ADesc, ABitsPtr);
  ReleaseDC(0, ADC);

  if Result = 0 then
    Exit;

  // copy source bitmap into destination
  SrcDC := CreateCompatibleDC(0);
  SrcOldBm := SelectObject(SrcDC, ASource);
  DstDC := CreateCompatibleDC(0);
  DstOldBm := SelectObject(DstDC, Result);
  Windows.BitBlt(DstDC, 0, 0, ADesc.Width, ADesc.Height, SrcDC, 0, 0, SRCCOPY);
  SelectObject(SrcDC, SrcOldBm);
  SelectObject(DstDC, DstOldBm);
  DeleteDC(SrcDC);
  DeleteDC(DstDC);
end;

{$ifndef Wince}
function GetBitmapOrder(AWinBmp: Windows.TBitmap; ABitmap: HBITMAP): TRawImageLineOrder;
  procedure DbgLog(const AFunc: String);
  begin

  end;

var
  SrcPixel: PCardinal absolute AWinBmp.bmBits;
  OrgPixel, TstPixel: Cardinal;
  Scanline: Pointer;
  DC: HDC;
  Info: record
    Header: Windows.TBitmapInfoHeader;
    Colors: array[Byte] of Cardinal; // reserve extra color for colormasks
  end;

  FullScanLine: Boolean; // win9x requires a full scanline to be retrieved
                         // others won't fail when one pixel is requested
begin
  if AWinBmp.bmBits = nil
  then begin
    // no DIBsection so always bottom-up
    Exit(riloBottomToTop);
  end;

  // try to figure out the orientation of the given bitmap.
  // Unfortunately MS doesn't provide a direct function for this.
  // So modify the first pixel to see if it changes. This pixel is always part
  // of the first scanline of the given bitmap.
  // When we request the data through GetDIBits as bottom-up, windows adjusts
  // the data when it is a top-down. So if the pixel doesn't change the bitmap
  // was internally a top-down image.

  FullScanLine := Win32Platform = VER_PLATFORM_WIN32_WINDOWS;
  if FullScanLine
  then ScanLine := GetMem(AWinBmp.bmWidthBytes);

  FillChar(Info.Header, sizeof(Windows.TBitmapInfoHeader), 0);
  Info.Header.biSize := sizeof(Windows.TBitmapInfoHeader);
  DC := Windows.GetDC(0);
  if Windows.GetDIBits(DC, ABitmap, 0, 1, nil, Windows.PBitmapInfo(@Info)^, DIB_RGB_COLORS) = 0
  then begin
    DbgLog('Getinfo');
    // failed ???
    Windows.ReleaseDC(0, DC);
    Exit(riloBottomToTop);
  end;

  // Get only 1 pixel (or full scanline for win9x)
  OrgPixel := 0;
  if FullScanLine
  then begin
    if Windows.GetDIBits(DC, ABitmap, 0, 1, ScanLine, Windows.PBitmapInfo(@Info)^, DIB_RGB_COLORS) = 0
    then DbgLog('OrgPixel')
    else OrgPixel := PCardinal(ScanLine)^;
  end
  else begin
    Info.Header.biWidth := 1;
    if Windows.GetDIBits(DC, ABitmap, 0, 1, @OrgPixel, Windows.PBitmapInfo(@Info)^, DIB_RGB_COLORS) = 0
    then DbgLog('OrgPixel');
  end;

  // modify pixel
  SrcPixel^ := not SrcPixel^;

  // get test
  TstPixel := 0;
  if FullScanLine
  then begin
    if Windows.GetDIBits(DC, ABitmap, 0, 1, ScanLine, Windows.PBitmapInfo(@Info)^, DIB_RGB_COLORS) = 0
    then DbgLog('TstPixel')
    else TstPixel := PCardinal(ScanLine)^;
  end
  else begin
    if Windows.GetDIBits(DC, ABitmap, 0, 1, @TstPixel, Windows.PBitmapInfo(@Info)^, DIB_RGB_COLORS) = 0
    then DbgLog('TstPixel');
  end;

  if OrgPixel = TstPixel
  then Result := riloTopToBottom
  else Result := riloBottomToTop;

  // restore pixel & cleanup
  SrcPixel^ := not SrcPixel^;
  Windows.ReleaseDC(0, DC);
  if FullScanLine
  then FreeMem(Scanline);
end;
{$endif}

{$ifdef WinCE}
//function GetBitmapBytes(ABitmap: HBITMAP; const ARect: TRect; ALineEnd: TRawImageLineEnd; var AData: Pointer; var ADataSize: PtrUInt): Boolean;
function GetBitmapBytes(AWinBmp: Windows.TBitmap; ABitmap: HBITMAP; const ARect: TRect; ALineEnd: TRawImageLineEnd; ALineOrder: TRawImageLineOrder; out AData: Pointer; out ADataSize: PtrUInt): Boolean;
var
  Section: Windows.TDIBSection;
  DIBCopy: HBitmap;
  DIBData: Pointer;
begin
  Result := False;
  // first try if the bitmap is created as section
  if (Windows.GetObject(ABitmap, SizeOf(Section), @Section) > 0) and (Section.dsBm.bmBits <> nil)
  then begin
    with Section.dsBm do
      Result := CopyImageData(bmWidth, bmHeight, bmWidthBytes, bmBitsPixel, bmBits, ARect, riloTopToBottom, riloTopToBottom, ALineEnd, AData, ADataSize);
    Exit;
  end;

  // bitmap is not a section, retrieve only bitmap
  if Windows.GetObject(ABitmap, SizeOf(Section.dsBm), @Section) = 0
  then Exit;

  DIBCopy := CreateDIBSectionFromDDB(ABitmap, DIBData);
  if DIBCopy = 0 then
    Exit;
  if (Windows.GetObject(DIBCopy, SizeOf(Section), @Section) > 0) and (Section.dsBm.bmBits <> nil)
  then begin
    with Section.dsBm do
      Result := CopyImageData(bmWidth, bmHeight, bmWidthBytes, bmBitsPixel, bmBits, ARect, riloTopToBottom, riloTopToBottom, ALineEnd, AData, ADataSize);
  end;

  DeleteObject(DIBCopy);

  Result := True;
end;
{$else}
function GetBitmapBytes(AWinBmp: Windows.TBitmap; ABitmap: HBITMAP; const ARect: TRect; ALineEnd: TRawImageLineEnd; ALineOrder: TRawImageLineOrder; out AData: Pointer; out ADataSize: PtrUInt): Boolean;
var
  DC: HDC;
  Info: record
    Header: Windows.TBitmapInfoHeader;
    Colors: array[Byte] of TRGBQuad; // reserve extra colors for palette (256 max)
  end;
  H: Cardinal;
  R: TRect;
  SrcData: PByte;
  SrcSize: PtrUInt;
  SrcLineBytes: Cardinal;
  SrcLineOrder: TRawImageLineOrder;
  StartScan: Integer;
begin
  SrcLineOrder := GetBitmapOrder(AWinBmp, ABitmap);
  SrcLineBytes := (AWinBmp.bmWidthBytes + 3) and not 3;

  if AWinBmp.bmBits <> nil
  then begin
    // this is bitmapsection data :) we can just copy the bits

    // We cannot trust windows with bmWidthBytes. Use SrcLineBytes which takes
    // DWORD alignment into consideration
    with AWinBmp do
      Result := CopyImageData(bmWidth, bmHeight, SrcLineBytes, bmBitsPixel, bmBits, ARect, SrcLineOrder, ALineOrder, ALineEnd, AData, ADataSize);
    Exit;
  end;

  // retrieve the data though GetDIBits

  // initialize bitmapinfo structure
  Info.Header.biSize := sizeof(Info.Header);
  Info.Header.biPlanes := 1;
  Info.Header.biBitCount := AWinBmp.bmBitsPixel;
  Info.Header.biCompression := BI_RGB;
  Info.Header.biSizeImage := 0;

  Info.Header.biWidth := AWinBmp.bmWidth;
  H := ARect.Bottom - ARect.Top;
  // request a top-down DIB
  if AWinBmp.bmHeight > 0
  then begin
    Info.Header.biHeight := -AWinBmp.bmHeight;
    StartScan := AWinBmp.bmHeight - ARect.Bottom;
  end
  else begin
    Info.Header.biHeight := AWinBmp.bmHeight;
    StartScan := ARect.Top;
  end;
  // adjust height
  if StartScan < 0
  then begin
    Inc(H, StartScan);
    StartScan := 0;
  end;

  // alloc buffer
  SrcSize := SrcLineBytes * H;
  GetMem(SrcData, SrcSize);

  DC := Windows.GetDC(0);
  Result := Windows.GetDIBits(DC, ABitmap, StartScan, H, SrcData, Windows.PBitmapInfo(@Info)^, DIB_RGB_COLORS) <> 0;
  Windows.ReleaseDC(0, DC);

  // since we only got the needed scanlines, adjust top and bottom
  R.Left := ARect.Left;
  R.Top := 0;
  R.Right := ARect.Right;
  R.Bottom := H;

  with Info.Header do
    Result := Result and CopyImageData(biWidth, H, SrcLineBytes, biBitCount, SrcData, R, riloTopToBottom, ALineOrder, ALineEnd, AData, ADataSize);

  FreeMem(SrcData);
end;
{$endif}

function IsAlphaBitmap(ABitmap: HBITMAP): Boolean;
var
  Info: Windows.BITMAP;
begin
  FillChar(Info, SizeOf(Info), 0);
  Result := (GetObject(ABitmap, SizeOf(Info), @Info) <> 0)
        and (Info.bmBitsPixel = 32);
end;

function IsAlphaDC(ADC: HDC): Boolean;
begin
  Result := (GetObjectType(ADC) = OBJ_MEMDC)
        and IsAlphaBitmap(GetCurrentObject(ADC, OBJ_BITMAP));
end;

function GetLastErrorText(AErrorCode: Cardinal): WideString;
var
  r: cardinal;
  tmp: PWideChar;
begin
  tmp := nil;
  r := Windows.FormatMessage(
    FORMAT_MESSAGE_ALLOCATE_BUFFER or FORMAT_MESSAGE_FROM_SYSTEM or FORMAT_MESSAGE_ARGUMENT_ARRAY,
    nil, AErrorCode, LANG_NEUTRAL, @tmp, 0, nil);

  if r = 0 then Exit('');

  Result := tmp;
  SetLength(Result, Length(Result)-2);

  if tmp <> nil
  then LocalFree(HLOCAL(tmp));
end;

(***********************************************************************
  Widget member Functions
************************************************************************)

{-------------------------------------------------------------------------------
  function LCLBoundsNeedsUpdate(Sender: TWinControl;
    SendSizeMsgOnDiff: boolean): boolean;

  Returns true if LCL bounds and win32 bounds differ for the control.
-------------------------------------------------------------------------------}

{-------------------------------------------------------------------------------
  function GetLCLClientOriginOffset(Sender: TObject;
    var LeftOffset, TopOffset: integer): boolean;

  Returns the difference between the client origin of a win32 handle
  and the definition of the LCL counterpart.
  For example:
    TGroupBox's client area is the area inside the groupbox frame.
    Hence, the LeftOffset is the frame width and the TopOffset is the caption
    height.
-------------------------------------------------------------------------------}

procedure GetWin32ControlPos(Window, Parent: HWND; var Left, Top: integer);
var
  parRect, winRect: Windows.TRect;
begin
  Windows.GetWindowRect(Window, @winRect);
  Windows.GetWindowRect(Parent, @parRect);
  Left := winRect.Left - parRect.Left;
  Top := winRect.Top - parRect.Top;
end;


{
  Updates the window style of the window indicated by Handle.
  The new style is the Style parameter.
  Only the bits set in the StyleMask are changed,
  the other bits remain untouched.
  If the bits in the StyleMask are not used in the Style,
  there are cleared.
}
procedure UpdateWindowStyle(Handle: HWnd; Style: integer; StyleMask: integer);
var
  CurrentStyle,
  NewStyle : PtrInt;
begin
  CurrentStyle := Windows.GetWindowLong(Handle, GWL_STYLE);
  NewStyle := (Style and StyleMask) or (CurrentStyle and (not StyleMask));
  Windows.SetWindowLong(Handle, GWL_STYLE, NewStyle);
end;

function GetFileVersion(FileName: string): dword;
var
  buf: pointer;
  lenBuf: dword;
  fixedInfo: ^VS_FIXEDFILEINFO;
  WideBuffer: widestring;
begin
  Result := $FFFFFFFF;
  WideBuffer := UTF8Decode(FileName);
  lenBuf := GetFileVersionInfoSizeW(PWideChar(WideBuffer), lenBuf);
  if lenBuf > 0 then
  begin
    GetMem(buf, lenBuf);
    if GetFileVersionInfoW(PWideChar(WideBuffer), 0, lenBuf, buf) then
    begin
      VerQueryValue(buf, '\', pointer(fixedInfo), lenBuf);
      Result := fixedInfo^.dwFileVersionMS;
    end;
    FreeMem(buf);
  end;
end;

procedure RemoveStayOnTopFlags(AppHandle: HWND; ASystemTopAlso: Boolean = False);
var
  StayOnTopWindowsInfo: PStayOnTopWindowsInfo;
  WindowInfo: TWindowInfo;
  I: Integer;
begin
{  //WriteLn('RemoveStayOnTopFlags ', InRemoveStayOnTopFlags);
  if InRemoveStayOnTopFlags = 0 then
  begin
    New(StayOnTopWindowsInfo);
    StayOnTopWindowsInfo^.AppHandle := AppHandle;
    StayOnTopWindowsInfo^.SystemTopAlso := ASystemTopAlso;
    StayOnTopWindowsInfo^.StayOnTopList := TList.Create;
    WindowInfo := GetWindowInfo(AppHandle);
    WindowInfo^.StayOnTopList := StayOnTopWindowsInfo^.StayOnTopList;
    EnumThreadWindows(GetWindowThreadProcessId(AppHandle, nil),
      @EnumStayOnTopRemove, LPARAM(StayOnTopWindowsInfo));
    for I := 0 to WindowInfo^.StayOnTopList.Count - 1 do
      SetWindowPos(HWND(WindowInfo^.StayOnTopList[I]), HWND_NOTOPMOST, 0, 0, 0, 0,
        SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE or SWP_NOOWNERZORDER or SWP_DRAWFRAME);
    Dispose(StayOnTopWindowsInfo);
  end;
  inc(InRemoveStayOnTopFlags);}
end;

procedure RestoreStayOnTopFlags(AppHandle: HWND);
var
  WindowInfo: TWindowInfo;
  I: integer;
begin
{  //WriteLn('RestoreStayOnTopFlags ', InRemoveStayOnTopFlags);
  if InRemoveStayOnTopFlags = 1 then
  begin
    WindowInfo := GetWindowInfo(AppHandle);
    if WindowInfo^.StayOnTopList <> nil then
    begin
      for I := 0 to WindowInfo^.StayOnTopList.Count - 1 do
        SetWindowPos(HWND(WindowInfo^.StayOnTopList.Items[I]),
          HWND_TOPMOST, 0, 0, 0, 0,
          SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE or SWP_NOOWNERZORDER or SWP_DRAWFRAME);
      FreeAndNil(WindowInfo^.StayOnTopList);
    end;
  end;
  if InRemoveStayOnTopFlags > 0 then
    dec(InRemoveStayOnTopFlags);}
end;

function WndClassName(Wnd: HWND): String; inline;
var
  winClassName: array[0..19] of widechar;
begin
  GetClassName(Wnd, @winClassName, 20);
  Result := winClassName;
end;

function IsAlienWindow(Wnd: HWND): Boolean;

const
  // list window class names is taken here:
  // http://www.pocketpcdn.com/print/articles/?&atb.set(c_id)=51&atb.set(a_id)=7165&atb.perform(details)=
  AlienWindowClasses: array[0..7] of String =
  (
    'menu_worker',        // can be also found by SHFindMenuBar
    'MS_SOFTKEY_CE_1.0',  // google about that one. as I understand it related to bottom menu too
    'Default Ime',
    'Ime',
    'static',
    'OLEAUT32',
    'FAKEIMEUI',
    'tooltips_class32'
  );

var
  i: integer;
  WndName: String;
begin
  WndName := WndClassName(Wnd);
  Result := False;
  for i := Low(AlienWindowClasses) to High(AlienWindowClasses) do
    if WndName = AlienWindowClasses[i] then
      Exit(True);
end;

{procedure LogWindow(Window: HWND);
begin
  DbgAppendToFile(ExtractFilePath(ParamStr(0)) + '1.log',
    'Window = ' + IntToStr(Window) + ' ClassName = ' + WndClassName(Window) + ' Thread id = ' + IntToStr(GetWindowThreadProcessId(Window, nil)));
end;}


function GetControlText(AHandle: HWND): string;
var
  TextLen: dword;
  tmpWideStr: WideString;
begin
  TextLen := GetWindowTextLength(AHandle);
  SetLength(tmpWideStr, TextLen+1);
  GetWindowTextW(AHandle, PWideChar(tmpWideStr), TextLen + 1);
  Result := UTF8Encode(tmpWideStr);
end;

procedure WideStrCopy(Dest, Src: PWideChar);
var
  counter : longint;
Begin
  counter := 0;
  while Src[counter] <> #0 do
  begin
    Dest[counter] := Src[counter];
    Inc(counter);
  end;
  Dest[counter] := #0;
end;

{ Exactly equal to StrLCopy but for PWideChars
  Copyes a widestring up to a maximal length, in WideChars }
function WideStrLCopy(dest, source: PWideChar; maxlen: SizeInt): PWideChar;
var
  counter: SizeInt;
begin
  counter := 0;

  while (Source[counter] <> #0)  and (counter < MaxLen) do
  begin
    Dest[counter] := Source[counter];
    Inc(counter);
  end;

  { terminate the string }
  Dest[counter] := #0;
  Result := Dest;
end;

function WideStrCmp(W1, W2: PWideChar): Integer;
var
  counter: Integer;
Begin
  counter := 0;
  While W1[counter] = W2[counter] do
  Begin
    if (W2[counter] = #0) or (W1[counter] = #0) then
       break;
    Inc(counter);
  end;
  Result := ord(W1[counter]) - ord(W2[counter]);
end;


function IsHiResMode: Boolean;
begin
  {$ifdef MSWindows}
  Result := False;
  {$else}
  Result := Screen.Width > 240;
  {$endif}
end;


{-------------------------------------------------------------------------------
  procedure AddToChangedMenus(Window: HWnd);

  Adds Window to the list of windows which need to redraw the main menu.
-------------------------------------------------------------------------------}
procedure AddToChangedMenus(Window: HWnd);
begin
  if ChangedMenus.IndexOf(Pointer(Window)) = -1 then // Window handle is not yet in the list
    ChangedMenus.Add(Pointer(Window));
end;

{------------------------------------------------------------------------------
  Method: RedrawMenus
  Params:  None
  Returns: Nothing

  Redraws all changed menus
 ------------------------------------------------------------------------------}
procedure RedrawMenus;
var
  I: integer;
begin
  for I := 0 to  ChangedMenus.Count - 1 do
    DrawMenuBar(HWND(ChangedMenus[I]));
  ChangedMenus.Clear;
end;

procedure UpdateWindowsVersion;
{$ifdef WinCE}
var
  versionInfo: OSVERSIONINFO;
begin
  WindowsVersion := wince_other;

  System.FillChar(versionInfo, sizeof(OSVERSIONINFO), #0);
  versionInfo.dwOSVersionInfoSize := sizeof(OSVERSIONINFO);

  if GetVersionEx(@versionInfo) then
  begin
    case versionInfo.dwMajorVersion of
    1: WindowsVersion := wince_1;
    2: WindowsVersion := Wince_2;
    3: WindowsVersion := Wince_3;
    4: WindowsVersion := Wince_4;
    5:
    begin
      if versionInfo.dwMinorVersion = 2 then WindowsVersion := Wince_6
      else WindowsVersion := Wince_5;
    end;
    6: WindowsVersion := Wince_6;
    end;
  end;
end;
{$else}
begin
  case Win32MajorVersion of
    0..3:;
    4: begin
     if Win32Platform = VER_PLATFORM_WIN32_NT
     then WindowsVersion := wvNT4
     else
       case Win32MinorVersion of
         10: WindowsVersion := wv98;
         90: WindowsVersion := wvME;
       else
         WindowsVersion :=wv95;
       end;
    end;
    5: begin
     case Win32MinorVersion of
       0: WindowsVersion := wv2000;
       1: WindowsVersion := wvXP;
     else
       // XP64 has also a 5.2 version
       // we could detect that based on arch and versioninfo.Producttype
       WindowsVersion := wvServer2003;
     end;
    end;
    6: begin
     case Win32MinorVersion of
       0: WindowsVersion := wvVista;
       1: WindowsVersion := wv7;
     else
       WindowsVersion := wvLater;
     end;
    end;
  else
    WindowsVersion := wvLater;
  end;
end;
{$endif}

end.
