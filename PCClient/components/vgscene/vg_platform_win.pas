unit vg_platform_win;

interface

uses
  Windows, CommDlg, Messages, Variants, ActiveX, MMSystem, ShellAPI,
  MultiMon, Classes, SysUtils, vg_scene, vg_forms;

type

  {$IFDEF FPC}
  TvgLongint = DWORD;
  {$ELSE}
  TvgLongint = Longint;
  {$ENDIF}

  MySTGMEDIUM = record // for compatible
    Tymed : DWord;
    Case Integer Of
      0 : (HBITMAP             : hBitmap;       UnkForRelease :  Pointer {IUnknown});
      1 : (HMETAFILEPICT       : THandle );
      2 : (HENHMETAFILE        : THandle  );
      3 : (HGLOBAL             : hGlobal       );
      4 : (lpszFileName        : POleStr    );
      5 : (stm                : Pointer{IStream}  );
      6 : (stg                : Pointer{IStorage} );
   end;

  TDropTarget = class(TComponent, IDropTarget)
  private
    Form: TvgCustomForm;
    function GetDataObject: TvgDragObject;
    function DragEnter(const dataObj: IDataObject; grfKeyState: TvgLongint;
      pt: TPoint; var dwEffect: TvgLongint): HResult; stdcall;
    function DragOver(grfKeyState: TvgLongint; pt: TPoint;
      var dwEffect: TvgLongint): HResult; stdcall;
    function DragLeave: HResult; stdcall;
    function Drop(const dataObj: IDataObject; grfKeyState: TvgLongint; pt: TPoint;
      var dwEffect: TvgLongint): HResult; stdcall;
  end;

  TFormatEtcArray = array of TFormatEtc;

  TDataObjectInfo = record
    FormatEtc: TFormatEtc;
    StgMedium: TStgMedium;
    OwnedByDataObject: Boolean
  end;
  TDataObjectInfoArray = array of TDataObjectInfo;

  TDropSource = class(TComponent, IDataObject, IDropSource)
  private
    Data: TvgDragObject;
    Formats: TDataObjectInfoArray;
    // IDropSource implementation
    function QueryContinueDrag(fEscapePressed: bool;
      grfKeyState: Longint): HRESULT; stdcall;
    function GiveFeedback(dwEffect: Longint): HRESULT; stdcall;
    // IDataObject implementation
    function GetData(const FormatEtcIn: TFormatEtc;
      out Medium: TStgMedium):HRESULT; stdcall;
    function GetDataHere(const FormatEtc: TFormatEtc;
      out Medium: TStgMedium):HRESULT; stdcall;
    function QueryGetData(const FormatEtc: TFormatEtc): HRESULT; stdcall;
    function GetCanonicalFormatEtc(const FormatEtc: TFormatEtc;
      out FormatEtcout: TFormatEtc): HRESULT; stdcall;
    {$IFDEF FPC}
    function SetData(const FormatEtc: TFormatEtc; const Medium: TStgMedium;
      fRelease: Bool): HRESULT; stdcall;
    {$ELSE}
    function SetData(const FormatEtc: TFormatEtc; var Medium: TStgMedium;
      fRelease: Bool): HRESULT; stdcall;
    {$ENDIF}
    function EnumFormatEtc(dwDirection: TvgLongint;
      out EnumFormatEtc: IEnumFormatEtc): HRESULT; stdcall;
    function dAdvise(const FormatEtc: TFormatEtc; advf: TvgLongint;
      const advsink: IAdviseSink; out dwConnection: TvgLongint): HRESULT; stdcall;
    function dUnadvise(dwConnection: TvgLongint): HRESULT; stdcall;
    function EnumdAdvise(out EnumAdvise: IEnumStatData): HRESULT; stdcall;
    { For IDropSourceHelper }
    function FindFormatEtc(TestFormatEtc: TFormatEtc): integer;
    function EqualFormatEtc(FormatEtc1, FormatEtc2: TFormatEtc): Boolean;
    function HGlobalClone(HGlobal: THandle): THandle;
    function LoadGlobalBlock(Format: TClipFormat;
      var MemoryBlock: Pointer): Boolean;
    function SaveGlobalBlock(Format: TClipFormat;
      MemoryBlock: Pointer; MemoryBlockSize: integer): Boolean;
    function RetrieveOwnedStgMedium(Format: TFormatEtc; var StgMedium: TStgMedium): HRESULT;
    function StgMediumIncRef(const InStgMedium: TStgMedium;
      var OutStgMedium: TStgMedium; CopyInMedium: Boolean): HRESULT;
    function CanonicalIUnknown(TestUnknown: IUnknown): IUnknown;
  end;

  TvgPlatformWin = class(TvgPlatform)
  private
    FWindowClass: TWndClassW;
    procedure UpdateLayer(AForm: TvgCustomForm);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { App }
    procedure Run; override;
    procedure Terminate; override;
    function HandleMessage: boolean; override;
    procedure WaitMessage; override;
    { Timer }
    function CreateTimer(Interval: integer; TimerFunc: TvgTimerProc): THandle; override;
    function DestroyTimer(Timer: THandle): boolean; override;
    function GetTick: single; override;
    { Window }
    function CreateWindow(AForm: TvgCustomForm): THandle; override;
    procedure DestroyWindow(AForm: TvgCustomForm); override;
    procedure ReleaseWindow(AForm: TvgCustomForm); override;
    procedure ShowWindow(AForm: TvgCustomForm); override;
    procedure HideWindow(AForm: TvgCustomForm); override;
    function ShowWindowModal(AForm: TvgCustomForm): TModalResult; override;
    procedure InvalidateWindowRect(AForm: TvgCustomForm; R: TvgRect); override;
    procedure SetWindowRect(AForm: TvgCustomForm; ARect: TvgRect); override;
    function GetWindowRect(AForm: TvgCustomForm): TvgRect; override;
    function GetClientSize(AForm: TvgCustomForm): TvgPoint; override;
    procedure SetWindowCaption(AForm: TvgCustomForm; ACaption: WideString); override;
    procedure SetCapture(AForm: TvgCustomForm); override;
    procedure ReleaseCapture(AForm: TvgCustomForm); override;
    function ClientToScreen(AForm: TvgCustomForm; const Point: TvgPoint): TvgPoint; override;
    function ScreenToClient(AForm: TvgCustomForm; const Point: TvgPoint): TvgPoint; override;
    { Drag and Drop }
    procedure BeginDragDrop(AForm: TvgCustomForm; const Data: TvgDragObject; ABitmap: TvgBitmap); override;
    { Clipboard }
    procedure SetClipboard(Value: Variant); override;
    function GetClipboard: Variant; override;
    { Mouse }
    function GetMousePos: TvgPoint; override;
    { Screen }
    function GetScreenSize: TvgPoint; override;
    { International }
    function GetCurrentLangID: string; override;
    { Dialogs }
    function DialogOpenFiles(var FileName: WideString; AInitDir: WideString; AllowMulti: boolean): boolean; override;
  end;

implementation

const
  WM_ADDUPDATERECT = WM_USER + 123;
  WM_RELEASEFORM   = WM_USER + 125;

var
  WindowAtom: TAtom;
  WindowAtomString: string;
  DropAtom: TAtom;
  DropAtomString: string;
  CF_VGOBJECT: Longint;
  NoStaysOpenList: TList;

function HBmpFromBitmap(Bitmap: TvgBitmap): THandle;
var
  BitmapInfo: TBitmapInfo;
  BitmapHandle: cardinal;
  BitmapBits: Pointer;
begin
  with BitmapInfo.bmiHeader do
  begin
    biSize := SizeOf(TBitmapInfoHeader);
    biPlanes := 1;
    biBitCount := 32;
    biCompression := BI_RGB;
    biWidth := Bitmap.Width;
    if biWidth <= 0 then biWidth := 1;
    biHeight := -Bitmap.Height;
    if biHeight >= 0 then biHeight := -1;
  end;
  Result := CreateDIBSection(0, BitmapInfo, DIB_RGB_COLORS, Pointer(BitmapBits), 0, 0);
  if BitmapBits <> nil then
  begin
    Move(Bitmap.StartLine^, BitmapBits^, Bitmap.Width * Bitmap.Height * 4);
  end;
end;

{ TvgPlatformWin }

constructor TvgPlatformWin.Create;
begin
  inherited;
  WindowAtomString := Format('VPF%.8X',[GetCurrentProcessID]);
  WindowAtom := GlobalAddAtom(PChar(WindowAtomString));
  DropAtomString := Format('VPFDROP%.8X',[GetCurrentProcessID]);
  DropAtom := GlobalAddAtom(PChar(DropAtomString));
  CF_VGOBJECT := RegisterClipBoardFormat(PChar('WindowAtomString'));
  NoStaysOpenList := TList.Create;
  Application := TvgApplication.Create(nil);
end;

destructor TvgPlatformWin.Destroy;
begin
  FreeAndNil(Application);
  NoStaysOpenList.Free;
  inherited;
end;

{ App =========================================================================}

procedure TvgPlatformWin.Run;
begin
  Application.RealCreateForms;
  repeat
    try
      Application.HandleMessage;
    except
      Application.HandleException(Self);
    end;
  until Application.Terminated;
end;

procedure TvgPlatformWin.Terminate;
begin
  PostQuitMessage(0);
end;

function TvgPlatformWin.HandleMessage: boolean;
var
  Msg: TMsg;
begin
  Result := false;
  if PeekMessage(Msg, 0, 0, 0, PM_REMOVE) then
  begin
    Result := true;
    if Msg.Message <> WM_QUIT then
    begin
      TranslateMessage(Msg);
      DispatchMessage(Msg);
    end
    else
      Application.Terminated := true;
  end;
end;

procedure TvgPlatformWin.WaitMessage;
begin
  Windows.WaitMessage;
end;

{ Timer =======================================================================}

type
  PWin32TimerInfo = ^TWin32Timerinfo;
  TWin32TimerInfo = record
    TimerID: UINT;           // the windows timer ID for this timer
    TimerFunc: TvgTimerProc; // owner function to handle timer
  end;

var
  FTimerData : TList;   // list of PWin32Timerinfo

procedure TimerCallBackProc(window_hwnd : hwnd; msg : Longint; idEvent: UINT; dwTime: Longint); stdcall;
Var
  TimerInfo: PWin32TimerInfo;
  n: Integer;
begin
  n := FTimerData.Count;
  while (n > 0) do
  begin
    dec(n);
    TimerInfo := FTimerData[n];
    if TimerInfo^.TimerID = idEvent then
    begin
      TimerInfo^.TimerFunc;
      break;
    end;
  end;
end;

function TvgPlatformWin.CreateTimer(Interval: integer;
  TimerFunc: TvgTimerProc): THandle;
var
  TimerInfo: PWin32TimerInfo;
begin
  Result := 0;
  if (Interval > 0) and (Assigned(TimerFunc)) then
  begin
    New(TimerInfo);
    TimerInfo^.TimerFunc := TimerFunc;
    TimerInfo^.TimerID := Windows.SetTimer(0, 0, Interval, @TimerCallBackProc);
    if TimerInfo^.TimerID=0 then
      dispose(TimerInfo)
    else
    begin
      if FTimerData = nil then
        FTimerData := TList.Create;
      FTimerData.Add(TimerInfo);
      Result := TimerInfo^.TimerID;
    end;
  end;
end;

function TvgPlatformWin.DestroyTimer(Timer: THandle): boolean;
var
  n : integer;
  TimerInfo : PWin32Timerinfo;
begin
  Result:= false;
  if FTimerData = nil then Exit;
  n := FTimerData.Count;
  while (n > 0) do
  begin
    dec(n);
    TimerInfo := FTimerData[n];
    if (TimerInfo^.TimerID = UINT(Timer)) then
    begin
      Result := Boolean(Windows.KillTimer(0, UINT(Timer)));
      FTimerData.Delete(n);
      Dispose(TimerInfo);
    end;
  end;
end;

function TvgPlatformWin.GetTick: single;
begin
  Result := timeGetTime / 1000;
end;

{ Window ======================================================================}

type
  PRgnRects = ^TRgnRects;
  TRgnRects = array [0..0] of TRect;

type

  PBlendFunction = ^TBlendFunction;
  _BLENDFUNCTION = packed record
    BlendOp: BYTE;
    BlendFlags: BYTE;
    SourceConstantAlpha: BYTE;
    AlphaFormat: BYTE;
  end;
  TBlendFunction = _BLENDFUNCTION;
  BLENDFUNCTION = _BLENDFUNCTION;

const

  WS_EX_LAYERED = $00080000;
  LWA_COLORKEY = $00000001;
  LWA_ALPHA = $00000002;
  ULW_COLORKEY = $00000001;
  ULW_ALPHA = $00000002;
  ULW_OPAQUE = $00000004;

var
  UpdateLayeredWindow: function (hWnd: HWND; hdcDst: HDC; pptDst: PPOINT;
    psize: PSIZE; hdcSrc: HDC; pptSrc: PPOINT; crKey: COLORREF;
    pblend: PBlendFunction; dwFlags: Longint): BOOL; stdcall;
  User32Lib: THandle;

function FindWindow(Handle: HWnd): TvgCustomForm;
begin
  Result := nil;
  if (Handle <> 0) then
  begin
    if GlobalFindAtom(PChar(WindowAtomString)) = WindowAtom then
      Result := Pointer(GetProp(Handle, MakeIntAtom(WindowAtom)))
    else
      Result := nil;
  end;
end;

function FindDropTarget(Handle: HWnd): TDropTarget;
begin
  Result := nil;
  if (Handle <> 0) then
  begin
    if GlobalFindAtom(PChar(DropAtomString)) = DropAtom then
      Result := Pointer(GetProp(Handle, MakeIntAtom(DropAtom)))
    else
      Result := nil;
  end;
end;

function KeysToShiftState(Keys: Word): TShiftState;
begin
  Result := [];
  if Keys and MK_SHIFT <> 0 then Include(Result, ssShift);
  if Keys and MK_CONTROL <> 0 then Include(Result, ssCtrl);
  if Keys and MK_LBUTTON <> 0 then Include(Result, ssLeft);
  if Keys and MK_RBUTTON <> 0 then Include(Result, ssRight);
  if Keys and MK_MBUTTON <> 0 then Include(Result, ssMiddle);
  if GetKeyState(VK_MENU) < 0 then Include(Result, ssAlt);
end;

function KeyDataToShiftState(KeyData: Longint): TShiftState;
const
  AltMask = $20000000;
{$IFDEF LINUX}
  CtrlMask = $10000000;
  ShiftMask = $08000000;
{$ENDIF}
begin
  Result := [];
  if GetKeyState(VK_SHIFT) < 0 then Include(Result, ssShift);
  if GetKeyState(VK_CONTROL) < 0 then Include(Result, ssCtrl);
  if KeyData and AltMask <> 0 then Include(Result, ssAlt);
{$IFDEF LINUX}
  if KeyData and CtrlMask <> 0 then Include(Result, ssCtrl);
  if KeyData and ShiftMask <> 0 then Include(Result, ssShift);
{$ENDIF}
end;

function WMPaint(hwnd: HWND; uMsg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
var
  i, rgnStatus: integer;
  rgn: HRgn;
  rgnSize: integer;
  rgnData: PRgnData;
  R: windows.TRect;
  Wnd: TvgCustomForm;
  UpdateRects: array of TvgRect;
  PS: TPaintStruct;
begin
  Wnd := FindWindow(hwnd);
  if Wnd = nil then
  begin
    Result := DefWindowProcW(hwnd, uMsg, wParam, lParam);    // Default result if nothing happens
    Exit;
  end;
  rgn := CreateRectRgn(0, 0, 1, 1);
  rgnStatus := GetUpdateRgn(Wnd.Handle, rgn, false);
  if (rgnStatus = 2) or (rgnStatus = 3) then
  begin
    rgnSize := GetRegionData(rgn, $FFFF, nil);
    if rgnSize > 0 then
    begin
      GetMem(rgnData, rgnSize);
      rgnSize := GetRegionData(rgn, rgnSize, rgnData);
      if rgnSize = rgnSize then
      begin
        SetLength(UpdateRects, rgnData.rdh.nCount);
        for i := 0 to rgnData.rdh.nCount - 1 do
        begin
          R := PRgnRects(@rgnData.buffer[0])[i];
          with R do
            UpdateRects[i] := vgRect(left, top, right, bottom);
        end;
      end;
      FreeMem(rgnData, rgnSize);

      Wnd.Context := BeginPaint(Wnd.Handle, PS);
      Wnd.PaintRects(UpdateRects);
      Wnd.Context := 0;
      EndPaint(Wnd.Handle, PS);
      Result := DefWindowProcW(hwnd, uMsg, wParam, lParam);    // Default result if nothing happens
    end
    else
    begin
      Result := DefWindowProcW(hwnd, uMsg, wParam, lParam);    // Default result if nothing happens
    end;
  end
  else
  begin
    Result := DefWindowProcW(hwnd, uMsg, wParam, lParam);    // Default result if nothing happens
  end;
  DeleteObject(rgn);
end;

function WndProc(hwnd: HWND; uMsg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
var
  UpdateRects: array of TvgRect;
  Wnd: TvgCustomForm;

 procedure ProcessUpdateMessages;
 var
   Msg: TMsg;
 begin
   SetLength(UpdateRects, 1);
   UpdateRects[0] := vgRect(TSmallPoint(cardinal(wParam)).X, TSmallPoint(cardinal(wParam)).Y,
     TSmallPoint(cardinal(lParam)).X, TSmallPoint(cardinal(lParam)).Y);
   while PeekMessage(Msg, hwnd, WM_ADDUPDATERECT, WM_ADDUPDATERECT, PM_REMOVE) do
   begin
     if Msg.message = WM_QUIT then
     begin
       { Repost WM_QUIT messages }
       PostQuitMessage(Msg.WParam);
       Break;
     end;
     SetLength(UpdateRects, Length(UpdateRects) + 1);
     UpdateRects[High(UpdateRects)] := vgRect(TSmallPoint(cardinal(Msg.wParam)).X, TSmallPoint(cardinal(Msg.wParam)).Y,
       TSmallPoint(cardinal(Msg.lParam)).X, TSmallPoint(cardinal(Msg.lParam)).Y);
   end;
 end;                         

 procedure CloseNoStaysOpen;
 var
   i: integer;
 begin
   if not Wnd.StaysOpen then Exit;
   if NoStaysOpenList.Count > 0 then
   begin
     for i := NoStaysOpenList.Count - 1 downto 0 do
       TvgCustomForm(NoStaysOpenList[i]).Close;
   end;
 end;
var
  PS: TPaintStruct;
  R: TRect;
  P: TPoint;
  H: boolean;
  Key: Word;
  Ch: WideChar;
  tme: TTRACKMOUSEEVENT;
begin
  Wnd := FindWindow(hwnd);
  if (Wnd <> nil) then
  begin
    case uMsg of
      WM_ACTIVATE:
        begin
          if not (fsRecreating in Wnd.FormState) then
          begin
            if wParam > 0 then
              Wnd.Activate
            else
            begin
              Wnd.Deactivate;
              CloseNoStaysOpen;
            end;
          end;
          Result := 0;
        end;
      WM_MOUSEACTIVATE:
        begin
          if not (fsRecreating in Wnd.FormState) then
          begin
            if not Wnd.ShowActivated then
              Result := MA_NOACTIVATE
            else
              Result := DefWindowProcW(hwnd, uMsg, wParam, lParam);    // Default result if nothing happens
          end;
        end;
      WM_ERASEBKGND:
        begin
          Result := 1;
        end;
      WM_PAINT:
        begin
          Result := WMPaint(hwnd, uMsg, wParam, lParam);
        end;
      WM_ADDUPDATERECT:
        begin
          ProcessUpdateMessages;
          Wnd.PaintRects(UpdateRects);
          TvgPlatformWin(Platform).UpdateLayer(Wnd);
        end;
      WM_WINDOWPOSCHANGED:
        begin
          Result := DefWindowProcW(hwnd, uMsg, wParam, lParam);    // Default result if nothing happens
          GetWindowRect(hWnd, R);
          Wnd.SetBounds(R.Left, R.Top, R.Right - R.Left, R.Bottom - R.Top);
        end;
      WM_CLOSE:
        begin
          Wnd.Close;
        end;
      WM_LBUTTONDOWN:
        begin
          CloseNoStaysOpen;
          GetCursorPos(P);
          ScreenToClient(Wnd.Handle, P);
          Wnd.MouseDown(mbLeft, KeysToShiftState(wParam), P.X, P.Y);
        end;
      WM_LBUTTONUP:
        begin
          GetCursorPos(P);
          ScreenToClient(Wnd.Handle, P);
          Wnd.MouseUp(mbLeft, KeysToShiftState(wParam), P.X, P.Y);
        end;
      WM_RBUTTONDOWN:
        begin
          CloseNoStaysOpen;
          GetCursorPos(P);
          ScreenToClient(Wnd.Handle, P);
          Wnd.MouseDown(mbRight, KeysToShiftState(wParam), P.X, P.Y);
        end;
      WM_RBUTTONUP:
        begin
          GetCursorPos(P);
          ScreenToClient(Wnd.Handle, P);
          Wnd.MouseUp(mbRight, KeysToShiftState(wParam), P.X, P.Y);
        end;
      WM_MOUSEMOVE:
        begin
          GetCursorPos(P);
          ScreenToClient(Wnd.Handle, P);
          Wnd.MouseMove(KeysToShiftState(wParam), P.X, P.Y);
          tme.cbSize := SizeOf(tme);
          tme.hwndTrack := Result;
          tme.dwFlags := TME_LEAVE;
          tme.dwHoverTime := 1;
          TrackMouseEvent(tme);
        end;
      WM_MOUSELEAVE:
        begin
          Wnd.MouseLeave;
        end;
      WM_MOUSEWHEEL:
        begin
          H := false;
          Wnd.MouseWheel(KeysToShiftState(wParam), TSmallPoint(cardinal(wParam)).Y, H);
          Result := Integer(H = true);
        end;
      WM_GETDLGCODE:
        begin
          Result := DLGC_WANTTAB or dlgc_WantArrows or DLGC_WANTCHARS;
        end;
      WM_CHAR:
        begin
          Ch := WideChar(wParam);
          Key := 0;
          Wnd.KeyDown(Key, Ch, KeyDataToShiftState(lParam));
          Wnd.KeyUp(Key, Ch, KeyDataToShiftState(lParam));
          Result := 0;
        end;
      WM_KEYDOWN:
        begin
          Ch := #0;
          Key := wParam;
          Wnd.KeyDown(Key, Ch, KeyDataToShiftState(lParam));
          Result := 0;
        end;
      WM_KEYUP:
        begin
          Ch := #0;
          Key := wParam;
          Wnd.KeyUp(Key, Ch, KeyDataToShiftState(lParam));
          Result := 0;
        end;
      WM_RELEASEFORM:
        begin
          Wnd.Free;
        end;
      else
        Result := DefWindowProcW(hwnd, uMsg, wParam, lParam);    // Default result if nothing happens
    end
  end
  else
    Result := DefWindowProcW(hwnd, uMsg, wParam, lParam);    // Default result if nothing happens
end;

function TvgPlatformWin.CreateWindow(AForm: TvgCustomForm): THandle;
var
  DropTarget: TDropTarget;
  Style, ExStyle: cardinal;
begin
  FillChar(FWindowClass, SizeOf(FWindowClass), 0);
  FWindowClass.style := CS_OWNDC;
  FWindowClass.lpfnWndProc := @WndProc;
  FWindowClass.cbClsExtra := 0;
  FWindowClass.cbWndExtra := 0;
  FWindowClass.hInstance := hInstance;
  FWindowClass.hIcon := LoadIcon(hInstance, PChar('DEFAULTICON'));
  FWindowClass.hCursor := LoadCursor(0, IDC_ARROW);
  FWindowClass.hbrBackground := GetStockObject(NULL_BRUSH);
  FWindowClass.lpszMenuName := nil;
  FWindowClass.lpszClassName := PWideChar(WideString('VPF' + AForm.ClassName));
  Windows.RegisterClassW(FWindowClass);

  Style := 0;
  ExStyle := 0;
  if AForm.Transparency then
  begin
    Style := Style or WS_POPUP;
    ExStyle := ExStyle or WS_EX_LAYERED;
    if (Application.MainForm <> nil) and (AForm <> Application.MainForm) then
      ExStyle := ExStyle or WS_EX_TOOLWINDOW; // disable taskbar
  end
  else
  begin
    case AForm.BorderStyle of
      bsNone:
        begin
          Style := Style or WS_POPUP or WS_SYSMENU;
          ExStyle := ExStyle {or WS_EX_TOOLWINDOW}; // disable taskbar
        end;
      bsSingle, bsToolWindow:
        Style := Style or (WS_CAPTION or WS_BORDER);
      bsSizeable, bsSizeToolWin:
        Style := Style or (WS_CAPTION or WS_THICKFRAME);
    end;
    if AForm.BorderStyle in [bsToolWindow, bsSizeToolWin] then
      ExStyle := ExStyle or WS_EX_TOOLWINDOW;
    if AForm.BorderStyle <> bsNone then
    begin
      if biMinimize in AForm.BorderIcons then Style := Style or WS_MINIMIZEBOX;
      if biMaximize in AForm.BorderIcons then Style := Style or WS_MAXIMIZEBOX;
      if biSystemMenu in AForm.BorderIcons then Style := Style or WS_SYSMENU;
    end;
  end;
{  if not AForm.ShowActivated then
    ExStyle := ExStyle or WS_EX_NOACTIVATE;}

  Result := Windows.CreateWindowExW(ExStyle,
     FWindowClass.lpszClassName, PWideChar(AForm.Caption),
     Style,
     round(AForm.Left), round(AForm.Top),
     round(AForm.Width), round(AForm.Height),
     GetDesktopWindow,
     0, hInstance, nil);
  SetProp(Result, MakeIntAtom(WindowAtom), THandle(AForm));

  DropTarget := TDropTarget.Create(nil);
  DropTarget.Form := AForm;
  SetProp(Result, MakeIntAtom(DropAtom), THandle(DropTarget));

  RegisterDragDrop(Result, DropTarget);

  if AForm.Transparency then
    UpdateLayer(AForm);

end;

procedure TvgPlatformWin.DestroyWindow(AForm: TvgCustomForm);
var
  DropTarget: TDropTarget;
begin
  HideWindow(AForm);

  RevokeDragDrop(AForm.Handle);

  DropTarget := FindDropTarget(AForm.Handle);
  if DropTarget <> nil then
    DropTarget.Free;

  Windows.DestroyWindow(AForm.Handle);
end;

procedure TvgPlatformWin.ReleaseWindow(AForm: TvgCustomForm);
begin
  PostMessage(AForm.Handle, WM_RELEASEFORM, 0, 0);
end;

procedure TvgPlatformWin.InvalidateWindowRect(AForm: TvgCustomForm; R: TvgRect);
var
  WR: TRect;
  Wnd: TvgCustomForm;
begin
  R := vgRect(Trunc(R.Left), Trunc(R.Top), Trunc(R.Right) + 1, Trunc(R.Bottom) + 1);
  if not vgIntersectRect(R, vgRect(0, 0, AForm.Width, AForm.Height)) then Exit;
  Wnd := FindWindow(AForm.Handle);
  if Wnd.Transparency then
  begin
    PostMessage(AForm.Handle, WM_ADDUPDATERECT, Integer(SmallPoint(round(R.Left), round(R.Top))), Integer(SmallPoint(round(R.Right), round(R.Bottom))));
  end
  else
  begin
    WR := Rect(Trunc(R.Left), Trunc(R.Top), Trunc(R.Right), Trunc(R.Bottom));
    Windows.InvalidateRect(AForm.Handle, @WR, false);
  end;
end;

procedure TvgPlatformWin.UpdateLayer(AForm: TvgCustomForm);
var
  Blend: TBLENDFUNCTION;
  Origin, Size, BitmapOrigin: Windows.TPoint;
  i, j: integer;
  SaveBits: PvgColorRecArray;
begin
  if AForm.Canvas = nil then Exit;
  if AForm.Canvas.Handle = 0 then Exit;
  Origin := Point(AForm.Left, AForm.Top);
  Size := Point(AForm.Width, AForm.Height);
  { Update }
  with Blend do
  begin
    BlendOp := AC_SRC_OVER;
    AlphaFormat := $01; //AC_SRC_ALPHA;
    BlendFlags := 0;
    SourceConstantAlpha := $FF;
  end;
  BitmapOrigin := Point(0, 0);

  UpdateLayeredWindow(AForm.Handle, 0, @Origin, @Size, AForm.Canvas.Handle, @BitmapOrigin, $00000000, @Blend, ULW_ALPHA);
end;

function TvgPlatformWin.GetWindowRect(AForm: TvgCustomForm): TvgRect;
var
  R: TRect;
begin
  Windows.GetWindowRect(AForm.Handle, R);
  with R do
    Result := vgRect(Left, Top, Right, Bottom);
end;

procedure TvgPlatformWin.SetWindowRect(AForm: TvgCustomForm; ARect: TvgRect);
begin
  with ARect do
    if not AForm.ShowActivated then
      SetWindowPos(AForm.Handle, 0, round(Left), round(Top), round(Right - Left), round(Bottom - Top), SWP_NOACTIVATE)
    else
      SetWindowPos(AForm.Handle, 0, round(Left), round(Top), round(Right - Left), round(Bottom - Top), 0)
end;

procedure TvgPlatformWin.SetWindowCaption(AForm: TvgCustomForm;
  ACaption: WideString);
begin
  SetWindowTextW(AForm.Handle, PWideChar(ACaption));
end;

procedure TvgPlatformWin.ReleaseCapture(AForm: TvgCustomForm);
begin
  Windows.ReleaseCapture;
end;

procedure TvgPlatformWin.SetCapture(AForm: TvgCustomForm);
begin
  Windows.SetCapture(AForm.Handle);
end;

function TvgPlatformWin.GetClientSize(AForm: TvgCustomForm): TvgPoint;
var
  R: TRect;
begin
  GetClientRect(AForm.Handle, R);
  Result := vgPoint(R.Right - R.Left, R.Bottom - R.Top);
end;

procedure TvgPlatformWin.HideWindow(AForm: TvgCustomForm);
begin
  if NoStaysOpenList.IndexOf(AForm) >= 0 then
    NoStaysOpenList.Remove(AForm);
  Windows.ShowWindow(AForm.Handle, SW_HIDE);
end;

procedure TvgPlatformWin.ShowWindow(AForm: TvgCustomForm);
begin
  if not AForm.ShowActivated then
    Windows.ShowWindow(AForm.Handle, SW_SHOWNOACTIVATE)
  else
    Windows.ShowWindow(AForm.Handle, SW_SHOW);
  if AForm.Transparency then
    UpdateLayer(AForm);
  if AForm.TopMost then
    SetWindowPos(AForm.Handle, HWND_TOPMOST, 0, 0, 0, 0, SWP_FRAMECHANGED or SWP_NOACTIVATE or SWP_NOMOVE or SWP_NOSIZE);
  if not AForm.StaysOpen and (NoStaysOpenList.IndexOf(AForm) < 0) then
    NoStaysOpenList.Add(AForm);
end;

type
  PTaskWindow = ^TTaskWindow;
  TTaskWindow = record
    Next: PTaskWindow;
    Window: HWnd;
  end;

var
  TaskActiveWindow: HWnd = 0;
  TaskFirstWindow: HWnd = 0;
  TaskFirstTopMost: HWnd = 0;
  TaskWindowList: PTaskWindow = nil;

function DoDisableWindow(Window: HWnd; Data: Longint): Bool; stdcall;
var
  P: PTaskWindow;
begin
  if (Window <> TaskActiveWindow) and IsWindowVisible(Window) and
    IsWindowEnabled(Window) then
  begin
    New(P);
    P^.Next := TaskWindowList;
    P^.Window := Window;
    TaskWindowList := P;
    EnableWindow(Window, False);
  end;
  Result := True;
end;

procedure EnableTaskWindows(WindowList: Pointer);
var
  P: PTaskWindow;
begin
  while WindowList <> nil do
  begin
    P := WindowList;
    if IsWindow(P^.Window) then EnableWindow(P^.Window, True);
    WindowList := P^.Next;
    Dispose(P);
  end;
end;

function DisableTaskWindows(ActiveWindow: HWnd): Pointer;
var
  SaveActiveWindow: HWND;
  SaveWindowList: Pointer;
begin
  Result := nil;
  SaveActiveWindow := TaskActiveWindow;
  SaveWindowList := TaskWindowList;
  TaskActiveWindow := ActiveWindow;
  TaskWindowList := nil;
  try
    try
      EnumThreadWindows(GetCurrentThreadID, @DoDisableWindow, 0);
      Result := TaskWindowList;
    except
      EnableTaskWindows(TaskWindowList);
      raise;
    end;
  finally
    TaskWindowList := SaveWindowList;
    TaskActiveWindow := SaveActiveWindow;
  end;
end;

function TvgPlatformWin.ShowWindowModal(AForm: TvgCustomForm): TModalResult;
var
  WindowList: Pointer;
  ActiveWindow: HWnd;
begin
  if GetCapture <> 0 then SendMessage(GetCapture, WM_CANCELMODE, 0, 0);
  Windows.ReleaseCapture;
  ActiveWindow := GetActiveWindow;
  WindowList := DisableTaskWindows(0);
  AForm.Show;
  AForm.ModalResult := mrNone;
  repeat
    Application.HandleMessage;
    if Application.Terminated then
      AForm.ModalResult := mrCancel
    else
      if AForm.ModalResult <> mrNone then
        AForm.CloseModal;
  until AForm.ModalResult <> mrNone;
  AForm.Hide;
  EnableTaskWindows(WindowList);
  if ActiveWindow <> 0 then SetActiveWindow(ActiveWindow);
  Result := AForm.ModalResult;
end;

function TvgPlatformWin.ClientToScreen(AForm: TvgCustomForm; const Point: TvgPoint): TvgPoint;
var
  P: TPoint;
begin
  P := Classes.Point(round(Point.X), round(Point.Y));
  Windows.ClientToScreen(AForm.Handle, P);
  Result := vgPoint(P.X, P.Y);
end;

function TvgPlatformWin.ScreenToClient(AForm: TvgCustomForm; const Point: TvgPoint): TvgPoint;
var
  P: TPoint;
begin
  P := Classes.Point(round(Point.X), round(Point.Y));
  Windows.ScreenToClient(AForm.Handle, P);
  Result := vgPoint(P.X, P.Y);
end;

{ Drag and Drop ===============================================================}

const
  IID_IDropTargetHelper: TGUID = (
    D1:$4657278b; D2:$411b; D3:$11d2; D4:($83,$9a,$00,$c0,$4f,$d9,$18,$d0));
  SID_IDropTargetHelper = '{4657278B-411B-11d2-839A-00C04FD918D0}';
  CLSID_DragDropHelper: TGUID = (
    D1:$4657278a; D2:$411b; D3:$11d2; D4:($83,$9a,$00,$c0,$4f,$d9,$18,$d0));

type
  {_$EXTERNALSYM IDropTargetHelper}
  IDropTargetHelper = interface(IUnknown)
    [SID_IDropTargetHelper]
    function DragEnter(hwndTarget: HWND; const DataObj: IDataObject;
      var pt: TPoint; dwEffect: Longint): HResult; stdcall;
    function DragLeave: HResult; stdcall;
    function DragOver(var pt: TPoint; dwEffect: Longint): HResult; stdcall;
    function Drop(const DataObj: IDataObject; var pt: TPoint;
      dwEffect: Longint): HResult; stdcall;
    function Show(Show: BOOL): HResult; stdcall;
  end;

var
  FDropTargetHelper: IDropTargetHelper;
  FDataObj: IDataObject;

function TDropTarget.GetDataObject: TvgDragObject;
var
  formatEtc: TFORMATETC;
  stgMedium: TSTGMEDIUM;
  str: wideString;
  drop: HDrop;
  i, numFiles: integer;
  buffer : array[0..MAX_PATH] of widechar;
begin
  FillChar(Result, SizeOf(Result), 0);
  if not Assigned(FDataObj) then Exit;
  // get file name first
  with formatEtc do
  begin
    cfFormat := CF_HDROP;
    ptd := nil;
    dwAspect := DVASPECT_CONTENT;
    lindex := -1;
    tymed := TYMED_HGLOBAL;
  end;
  // get VG
  formatEtc.cfFormat := CF_VGOBJECT;
  if FDataObj.GetData(formatEtc, stgMedium) = S_OK then
  begin
    Result := TDropSource(stgMedium.hGlobal).Data;
    Exit;
  end;
  // files
  str := '';
  formatEtc.cfFormat := CF_HDROP;
  if FDataObj.GetData(formatEtc, stgMedium) = S_OK then
  begin
    try
      {Lock the global memory handle to get a pointer to the data}
      drop := HDrop(GlobalLock(stgMedium.hGlobal));
      { Replace Text }
      numFiles := DragQueryFile(drop, $FFFFFFFF, nil, 0);
      SetLength(Result.Files, numFiles);
      for i := 0 to numFiles - 1 do
      begin
        DragQueryFileW(drop, i, @buffer, sizeof(buffer));
        Result.Files[i] := buffer;
        if i = 0 then
          Result.Data := Result.Files[0];
      end;
    finally
      {Finished with the pointer}
      GlobalUnlock(stgMedium.hGlobal);
      {Free the memory}
      ReleaseStgMedium({$IFDEF FPC}@{$ENDIF}stgMedium);
    end;
    Exit;
  end;
  // get text
  formatEtc.cfFormat := CF_UNICODETEXT;
  if FDataObj.GetData(formatEtc, stgMedium) = S_OK then
  begin
    try
      {Lock the global memory handle to get a pointer to the data}
      str := PWideChar(GlobalLock(stgMedium.hGlobal));
      Result.Data := str;
    finally
      {Finished with the pointer}
      GlobalUnlock(stgMedium.hGlobal);
      {Free the memory}
      ReleaseStgMedium({$IFDEF FPC}@{$ENDIF}stgMedium);
    end;
    Exit;
  end;
end;

function TDropTarget.DragEnter(const dataObj: IDataObject; grfKeyState: TvgLongint;
      pt: TPoint; var dwEffect: TvgLongint): HResult;
begin
  try
    FDataObj := dataObj;
    Result := S_OK;
    dwEffect := DROPEFFECT_NONE;
    if (Succeeded(CoCreateInstance(CLSID_DragDropHelper, nil, CLSCTX_INPROC_SERVER,
      IDropTargetHelper, FDropTargetHelper))) and
      (FDropTargetHelper <> nil) then
    begin
      if (Failed(FDropTargetHelper.DragEnter(Form.Handle, DataObj, pt, dwEffect))) then
        FDropTargetHelper := nil;
    end;
  except
    dwEffect := DROPEFFECT_NONE;
    Result := E_UNEXPECTED;
  end;
end;

function TDropTarget.DragOver(grfKeyState: TvgLongint; pt: TPoint;
      var dwEffect: TvgLongint): HResult;
var
  P: TvgPoint;
  Accept: boolean;
begin
  try
    dwEffect := DROPEFFECT_NONE;
    Result := S_OK;
    Windows.ScreenToClient(Form.Handle, pt);
    P := vgPoint(pt.X, pt.Y);
    Accept := false;
    Form.DragOver(GetDataObject, P, Accept);
    if Accept then
      dwEffect := DROPEFFECT_LINK;
    if FDropTargetHelper <> nil then
      FDropTargetHelper.DragOver(pt, dwEffect);
  except
    dwEffect := DROPEFFECT_NONE;
    Result := E_UNEXPECTED;
  end;
end;

function TDropTarget.DragLeave: HResult;
begin
  Form.DragLeave;
  if (FDropTargetHelper <> nil) then
    FDropTargetHelper.DragLeave;
  FDropTargetHelper := nil;
  FDataObj := nil;
  Result := S_OK;
end;

function TDropTarget.Drop(const dataObj: IDataObject; grfKeyState: TvgLongint; pt: TPoint;
      var dwEffect: TvgLongint): HResult;
var
  P: TvgPoint;
begin
  try
    if (dataObj = nil) then Exit;
    Windows.ScreenToClient(Form.Handle, pt);
    P := vgPoint(pt.X, pt.Y);
    Form.DragDrop(GetDataObject, P);
    if (FDropTargetHelper <> nil) then
      FDropTargetHelper.Drop(DataObj, pt, dwEffect)
  finally
    FDataObj := nil;
    FDropTargetHelper := nil;
  end;
end;

{ TDropSource }

{ IDropSource }

function TDropSource.QueryContinueDrag(fEscapePressed: bool;
  grfKeyState: Integer): HRESULT;
var
  ContinueDrop: Boolean;
begin
  if fEscapePressed then
    Result := DRAGDROP_S_CANCEL
  else
  if (grfKeyState and (MK_LBUTTON or MK_RBUTTON) = 0) then
  begin
    ContinueDrop := true;
    if ContinueDrop then
      Result := DRAGDROP_S_DROP
    else
      Result := DRAGDROP_S_CANCEL;
  end
  else
    Result := S_OK;
end;

function TDropSource.GiveFeedback(dwEffect: Integer): HRESULT;
begin
  Result := S_OK; //DRAGDROP_S_USEDEFAULTCURSORS;
end;

{ IDataObject }

function TDropSource.dAdvise(const FormatEtc: TFormatEtc; advf: TvgLongint;
  const advsink: IAdviseSink; out dwConnection: TvgLongint): HRESULT;
begin
  Result := OLE_E_ADVISENOTSUPPORTED;
end;

function TDropSource.dUnadvise(dwConnection: TvgLongint): HRESULT;
begin
  Result := OLE_E_ADVISENOTSUPPORTED;
end;

function TDropSource.EnumdAdvise(out EnumAdvise: IEnumStatData): HRESULT;
begin
  Result := OLE_E_ADVISENOTSUPPORTED;
end;

function TDropSource.EnumFormatEtc(dwDirection: TvgLongint;
  out EnumFormatEtc: IEnumFormatEtc): HRESULT;
begin
  if (dwDirection = DATADIR_GET) then
    Result := S_OK
  else
    if (dwDirection = DATADIR_SET) then
      Result := E_NOTIMPL;
end;

function TDropSource.GetCanonicalFormatEtc(const FormatEtc: TFormatEtc;
  out FormatEtcout: TFormatEtc): HRESULT;
begin
  Result := DATA_S_SAMEFORMATETC;
end;

function TDropSource.EqualFormatEtc(FormatEtc1, FormatEtc2: TFormatEtc): Boolean;
begin
  Result := (FormatEtc1.cfFormat = FormatEtc2.cfFormat) and
            (FormatEtc1.ptd = FormatEtc2.ptd) and
            (FormatEtc1.dwAspect = FormatEtc2.dwAspect) and
            (FormatEtc1.lindex = FormatEtc2.lindex) and
            (FormatEtc1.tymed = FormatEtc2.tymed)
end;

function TDropSource.FindFormatEtc(TestFormatEtc: TFormatEtc): integer;
var
  i: integer;
  Found: Boolean;
begin
  i := 0;
  Found := False;
  Result := -1;
  while (i < Length(Formats)) and not Found do
  begin
    Found := EqualFormatEtc(Formats[i].FormatEtc, TestFormatEtc);
    if Found then
      Result := i;
    Inc(i);
  end
end;

function TDropSource.HGlobalClone(HGlobal: THandle): THandle;
// Returns a global memory block that is a copy of the passed memory block.
var
  Size: LongWord;
  Data, NewData: PChar;
begin
  Size := GlobalSize(HGlobal);
  Result := GlobalAlloc(GPTR, Size);
  Data := GlobalLock(hGlobal);
  try
    NewData := GlobalLock(Result);
    try
      Move(Data, NewData, Size);
    finally
      GlobalUnLock(Result);
    end
  finally
    GlobalUnLock(hGlobal)
  end
end;

function TDropSource.LoadGlobalBlock(Format: TClipFormat;
  var MemoryBlock: Pointer): Boolean;
var
  FormatEtc: TFormatEtc;
  StgMedium: TStgMedium;
  GlobalObject: Pointer;
begin
  Result := False;

  FormatEtc.cfFormat := Format;
  FormatEtc.ptd := nil;
  FormatEtc.dwAspect := DVASPECT_CONTENT;
  FormatEtc.lindex := -1;
  FormatEtc.tymed := TYMED_HGLOBAL;

  if Succeeded(QueryGetData(FormatEtc)) and Succeeded(GetData(FormatEtc, StgMedium)) then
  begin
    MemoryBlock := AllocMem( GlobalSize(StgMedium.hGlobal));
    GlobalObject := GlobalLock(StgMedium.hGlobal);
    try
      if Assigned(MemoryBlock) and Assigned(GlobalObject) then
      begin
        Move(GlobalObject^, MemoryBlock^, GlobalSize(StgMedium.hGlobal));
      end
    finally
      GlobalUnLock(StgMedium.hGlobal);
    end
  end;
end;

function TDropSource.SaveGlobalBlock(Format: TClipFormat;
  MemoryBlock: Pointer; MemoryBlockSize: integer): Boolean;
var
  FormatEtc: TFormatEtc;
  StgMedium: TStgMedium;
  GlobalObject: Pointer;
begin
  FormatEtc.cfFormat := Format;
  FormatEtc.ptd := nil;
  FormatEtc.dwAspect := DVASPECT_CONTENT;
  FormatEtc.lindex := -1;
  FormatEtc.tymed := TYMED_HGLOBAL;

  StgMedium.tymed := TYMED_HGLOBAL;
  MySTGMEDIUM(StgMedium).unkForRelease := nil;
  StgMedium.hGlobal := GlobalAlloc(GHND or GMEM_SHARE, MemoryBlockSize);
  GlobalObject := GlobalLock(StgMedium.hGlobal);
  try
    try
      Move(MemoryBlock^, GlobalObject^, MemoryBlockSize);
      Result := Succeeded( SetData(FormatEtc, StgMedium, True))
    except
      Result := False;
    end
  finally
    GlobalUnLock(StgMedium.hGlobal);
  end
end;

function TDropSource.RetrieveOwnedStgMedium(Format: TFormatEtc; var StgMedium: TStgMedium): HRESULT;
var
  i: integer;
begin
  Result := E_INVALIDARG;
  i := FindFormatEtc(Format);
  if (i > -1) and Formats[i].OwnedByDataObject then
    Result := StgMediumIncRef(Formats[i].StgMedium, StgMedium, False)
end;

function TDropSource.StgMediumIncRef(const InStgMedium: TStgMedium;
  var OutStgMedium: TStgMedium; CopyInMedium: Boolean): HRESULT;
begin
  Result := S_OK;
  // Simply copy all fields to start with.
  OutStgMedium := InStgMedium;
  case InStgMedium.tymed of
    TYMED_HGLOBAL:
      begin
        if CopyInMedium then
        begin
          // Generate a unique copy of the data passed
          OutStgMedium.hGlobal := HGlobalClone(InStgMedium.hGlobal);
          if OutStgMedium.hGlobal = 0 then
            Result := E_OUTOFMEMORY
        end else
          // Don't generate a copy just use ourselves and the copy previoiusly saved
          MySTGMEDIUM(OutStgMedium).unkForRelease := Pointer(Self as IDataObject) // Does increase RefCount
      end;
    TYMED_FILE:
      begin
        if CopyInMedium then
        begin
          OutStgMedium.lpszFileName := CoTaskMemAlloc(lstrLenW(InStgMedium.lpszFileName));
//!!          StrCopyW(PWideChar(OutStgMedium.lpszFileName), PWideChar(InStgMedium.lpszFileName))
        end
        else
          MySTGMEDIUM(OutStgMedium).unkForRelease := Pointer(Self as IDataObject) // Does increase RefCount
      end;
    TYMED_ISTREAM:
      IUnknown(MySTGMEDIUM(OutStgMedium).stm)._AddRef;
    TYMED_ISTORAGE:
      IUnknown( MySTGMEDIUM(OutStgMedium).stg)._AddRef;
    TYMED_GDI:
      if not CopyInMedium then
        MySTGMEDIUM(OutStgMedium).unkForRelease := Pointer(Self as IDataObject) // Does not increase RefCount
     else
       Result := DV_E_TYMED; // Don't know how to copy GDI objects right now
    TYMED_MFPICT:
      if not CopyInMedium then
        MySTGMEDIUM(OutStgMedium).unkForRelease := Pointer(Self as IDataObject) // Does not increase RefCount
      else
        Result := DV_E_TYMED; // Don't know how to copy MetaFile objects right now
    TYMED_ENHMF:
      if not CopyInMedium then
        MySTGMEDIUM(OutStgMedium).unkForRelease := Pointer(Self as IDataObject) // Does not increase RefCount
      else
        Result := DV_E_TYMED; // Don't know how to copy enhanced metafiles objects right now
  else
    Result := DV_E_TYMED
  end;

  // I still have to do this. The Compiler will call _Release on the above Self as IDataObject
  // casts which is not what is necessary.  The DataObject is released correctly.
  if Assigned(MySTGMEDIUM(OutStgMedium).unkForRelease) and (Result = S_OK) then
    IUnknown(MySTGMEDIUM(OutStgMedium).unkForRelease)._AddRef
end;

function TDropSource.GetData(const FormatEtcIn: TFormatEtc;
  out Medium: TStgMedium): HRESULT;
var
  Global: cardinal;
  P: Pointer;
  W: WideString;
  B: TvgBitmap;
  BitmapHandle: cardinal;
begin
  FillChar(Medium, SizeOf(Medium), 0);
  Result := DV_E_FORMATETC;
  if QueryGetData(FormatEtcIn) <> S_OK then Exit;

  if FormatEtcIn.cfFormat = CF_VGOBJECT then
  begin
    Medium.tymed := TYMED_HGLOBAL;
    Medium.hGlobal := Cardinal(Self);
    Result := S_OK;
    Exit;
  end;
  case FormatEtcIn.cfFormat of
    CF_UNICODETEXT:
      begin
        W := Data.Data;
        Global := GlobalAlloc(0, (Length(W) + 1) * 2);
        P := GlobalLock(Global);
        try
          Move(PWideChar(W)^, P^, GlobalSize(Global));
        finally
          GlobalUnlock(Global);
        end;
        Medium.tymed := TYMED_HGLOBAL;
        Medium.hGlobal := Global;
        Result := S_OK;
      end;
    CF_BITMAP:
      begin
        if VarIsObject(Data.Data) and (VariantToObject(Data.Data) is TvgBitmap) then
        begin
          B := TvgBitmap(VariantToObject(Data.Data));
          BitmapHandle := HBmpFromBitmap(B);
          if BitmapHandle <> 0 then
          begin
            Medium.tymed := TYMED_GDI;
            Medium.hBitmap := BitmapHandle;
            Result := S_OK;
          end;
        end
      end;
  end;
  if Result <> S_OK then
    if Assigned(Formats) then
    begin
      { Do we support this type of Data? }
      Result := QueryGetData(FormatEtcIn);
      if Result = S_OK then
      begin
        // If the data is owned by the IDataObject just retrieve and return it.
        if RetrieveOwnedStgMedium(FormatEtcIn, Medium) = E_INVALIDARG then
          Result := E_UNEXPECTED
      end
    end
end;

function TDropSource.GetDataHere(const FormatEtc: TFormatEtc;
  out Medium: TStgMedium): HRESULT;
begin
  Result := E_NOTIMPL;
end;

function TDropSource.QueryGetData(const FormatEtc: TFormatEtc): HRESULT;
var
  i: integer;
  FormatAvailable, Handled: Boolean;
begin
  Result:= DV_E_FORMATETC;
  if FormatEtc.cfFormat = CF_VGOBJECT then
  begin
    Result := S_OK;
    Exit;
  end;
  case FormatEtc.cfFormat of
    CF_UNICODETEXT:
      begin
        if VarIsStr(Data.Data) and not VarIsObject(Data.Data) then
          Result := S_OK;
      end;
    CF_BITMAP:
      begin
        if VarIsObject(Data.Data) and (VariantToObject(Data.Data) is TvgBitmap) then
          Result := S_OK;
      end;
  end;
  if Result <> S_OK then
  begin
    if Assigned(Formats) then
    begin
      i := 0;
      Result := DV_E_FORMATETC;
      while (i < Length(Formats)) and (Result = DV_E_FORMATETC)  do
      begin
        if Formats[i].FormatEtc.cfFormat = formatetc.cfFormat then
        begin
          if (Formats[i].FormatEtc.dwAspect = formatetc.dwAspect) then
          begin
            if (Formats[i].FormatEtc.tymed and formatetc.tymed <> 0) then
              Result := S_OK
            else
              Result := DV_E_TYMED;
          end else
            Result := DV_E_DVASPECT;
        end else
          Result := DV_E_FORMATETC;
        Inc(i)
      end
    end
    else
      Result := E_UNEXPECTED;
  end;
end;

function TDropSource.CanonicalIUnknown(TestUnknown: IUnknown): IUnknown;
// Uses COM object identity: An explicit call to the IUnknown::QueryInterface
// method, requesting the IUnknown interface, will always return the same
// pointer.
begin
  if Assigned(TestUnknown) then
  begin
    if Supports(TestUnknown, IUnknown, Result) then
      IUnknown(Result)._Release // Don't actually need it just need the pointer value
    else
      Result := TestUnknown
  end else
    Result := TestUnknown
end;

{$IFDEF FPC}
function TDropSource.SetData(const FormatEtc: TFormatEtc;
  const Medium: TStgMedium; fRelease: Bool): HRESULT;
{$ELSE}
function TDropSource.SetData(const FormatEtc: TFormatEtc;
  var Medium: TStgMedium; fRelease: Bool): HRESULT;
{$ENDIF}
var
  Index: integer;
begin
  // See if we already have a format of that type available.
  Index := FindFormatEtc(FormatEtc);
  if Index > - 1 then
  begin
    // Yes we already have that format type stored.  Just use the TClipboardFormat
    // in the List after releasing the data
    ReleaseStgMedium(Formats[Index].StgMedium);
    FillChar(Formats[Index].StgMedium, SizeOf(Formats[Index].StgMedium), #0);
  end else
  begin
    // It is a new format so create a new TDataObjectInfo record and store it in
    // the Format array
    SetLength(Formats, Length(Formats) + 1);
    Formats[Length(Formats) - 1].FormatEtc := FormatEtc;
    Index := Length(Formats) - 1;
  end;
  // The data is owned by the TClipboardFormat object
  Formats[Index].OwnedByDataObject := True;

  if fRelease then
  begin
    // We are simply being given the data and we take control of it.
    Formats[Index].StgMedium := Medium;
    Result := S_OK
  end
  else
    // We need to reference count or copy the data and keep our own references
    // to it.
    Result := StgMediumIncRef(Medium, Formats[Index].StgMedium, True);

    // Can get a circular reference if the client calls GetData then calls
    // SetData with the same StgMedium.  Because the unkForRelease and for
    // the IDataObject can be marshalled it is necessary to get pointers that
    // can be correctly compared.
    // See the IDragSourceHelper article by Raymond Chen at MSDN.
    if Assigned(MySTGMEDIUM(Formats[Index].StgMedium).unkForRelease) then
    begin
      if CanonicalIUnknown(Self) =
        CanonicalIUnknown(IUnknown( MySTGMEDIUM(Formats[Index].StgMedium).unkForRelease)) then
      begin
        IUnknown( MySTGMEDIUM(Formats[Index].StgMedium).unkForRelease)._Release;
        MySTGMEDIUM(Formats[Index].StgMedium).unkForRelease := nil
      end;
    end;
end;

{ Platform DragDrop }

type
  PSHDRAGIMAGE = ^TSHDRAGIMAGE;
  {_$EXTERNALSYM _SHDRAGIMAGE}
  _SHDRAGIMAGE = packed record
    sizeDragImage: TSize;               { The length and Width of the rendered image }
    ptOffset: TPoint;                   { The Offset from the mouse cursor to the upper left corner of the image }
    hbmpDragImage: HBitmap;             { The Bitmap containing the rendered drag images }
    crColorKey: COLORREF;               { The COLORREF that has been blitted to the background of the images }
  end;
  TSHDRAGIMAGE = _SHDRAGIMAGE;
  {_$EXTERNALSYM SHDRAGIMAGE}
  SHDRAGIMAGE = _SHDRAGIMAGE;

const
  IID_IDragSourceHelper: TGUID = (
    D1:$de5bf786; D2:$477a; D3:$11d2; D4:($83,$9d,$00,$c0,$4f,$d9,$18,$d0));
  SID_IDragSourceHelper = '{DE5BF786-477A-11d2-839D-00C04FD918D0}';

type
  {_$EXTERNALSYM IDragSourceHelper}
  IDragSourceHelper = interface(IUnknown)
    [SID_IDragSourceHelper]
    function InitializeFromBitmap(var shdi: TSHDRAGIMAGE;
      const DataObj: IDataObject): HResult; stdcall;
    function InitializeFromWindow(hwnd: HWND; var pt: TPoint;
      const DataObj: IDataObject): HResult; stdcall;
  end;

procedure TvgPlatformWin.BeginDragDrop(AForm: TvgCustomForm; const Data: TvgDragObject; ABitmap: TvgBitmap);
var
  D: TDropSource;
  DropEffect: Longint;
  DragSourceHelper: IDragSourceHelper;
  shDragImage: TSHDRAGIMAGE;
begin
  D := TDropSource.Create(nil);
  D.Data := Data;

  if (Succeeded(CoCreateInstance(CLSID_DragDropHelper, nil, CLSCTX_INPROC_SERVER,
    IDragSourceHelper, DragSourceHelper))) and (DragSourceHelper <> nil) then
  begin
    Fillchar(shDragImage, SizeOf(shDragImage), 0);
    shDragImage.sizeDragImage.cx := ABitmap.Width;
    shDragImage.sizeDragImage.cy := ABitmap.Height;
    shDragImage.ptOffset.X := (ABitmap.Width div 2);
    shDragImage.ptOffset.Y := (ABitmap.Height div 2);
    shDragImage.hbmpDragImage := HBmpFromBitmap(ABitmap);
    if not Succeeded(DragSourceHelper.InitializeFromBitmap(shDragImage, D)) then
      DeleteObject(shDragImage.hbmpDragImage);
  end;

  DoDragDrop(D, D, DROPEFFECT_LINK or DROPEFFECT_COPY or DROPEFFECT_MOVE, {$IFDEF FPC}@{$ENDIF}DropEffect);

  if Assigned(DragSourceHelper) then
    DragSourceHelper := nil;

  D.Free;
end;

{ Clipboard }

function TvgPlatformWin.GetClipboard: Variant;
var
  Data: THandle;
  W: WideString;
begin
  Result := NULL;
  OpenClipboard(0);
  Data := GetClipboardData(CF_UNICODETEXT);
  if Data <> 0 then
  begin
    W := PWideChar(GlobalLock(Data));
    Result := W;
    GlobalUnlock(Data);
  end;
  CloseClipboard;                 
end;

procedure TvgPlatformWin.SetClipboard(Value: Variant);
var
  Data: THandle;
  DataPtr: Pointer;
  W: WideString;
begin
  if not VarIsObject(Value) and VarIsStr(Value) then
  begin
    OpenClipboard(0);
    EmptyClipboard;
    try
      W := Value;
      Data := GlobalAlloc(GMEM_MOVEABLE + GMEM_DDESHARE, (Length(W) + 1) * 2);
      try
        DataPtr := GlobalLock(Data);
        try
          Move(PWideChar(W)^, DataPtr^, GlobalSize(Data));
          SetClipboardData(CF_UNICODETEXT, Data);
        finally
          GlobalUnlock(Data);
        end;
      except
        GlobalFree(Data);
        raise;
      end;
    finally
      CloseClipboard;
    end;
  end;
end;

{ Mouse }

function TvgPlatformWin.GetMousePos: TvgPoint;
var
  P: TPoint;
begin
  GetCursorPos(P);
  Result := vgPoint(P.X, P.Y);
end;

{ Screen }

function TvgPlatformWin.GetScreenSize: TvgPoint;
var
  R: TRect;
begin
  Windows.GetWindowRect(GetDesktopWindow, R);
  Result := vgPoint(R.Right, R.Bottom);
end;

function TvgPlatformWin.GetCurrentLangID: string;
var
  Lang, FallbackLang: string;
  Buffer: array[1..4] of {$ifdef Wince}WideChar{$else}char{$endif};
  Country: string;
  UserLCID: LCID;
begin
  //defaults
  Lang := '';
  FallbackLang:='';
  UserLCID := GetUserDefaultLCID;
  if GetLocaleInfo(UserLCID, LOCALE_SABBREVLANGNAME, @Buffer[1], 4)<>0 then
    FallbackLang := lowercase(copy(Buffer,1,2));
  if GetLocaleInfo(UserLCID, LOCALE_SABBREVCTRYNAME, @Buffer[1], 4)<>0 then begin
    Country := copy(Buffer,1,2);

    // some 2 letter codes are not the first two letters of the 3 letter code
    // there are probably more, but first let us see if there are translations
    if (Buffer='PRT') then Country:='PT';

    Lang := FallbackLang+'_'+Country;
  end;
  Result := FallbackLang;
end;

function TvgPlatformWin.DialogOpenFiles;
const
  FileNameBufferLen = 1000;
var
  Flags: DWord;
  OpenFile: TOpenFilenameW;
  FileNameBuffer: PWideChar;
  FileNameBufferSize: Integer;
  InitialDir, Filter, DefaultExt: WideString;
begin
  Result := false;
  InitialDir := AInitDir;
  if (FileName <> '') and (FileName[length(FileName)] = PathDelim) then
  begin
    // if the filename contains a directory, set the initial directory
    // and clear the filename
    InitialDir := Copy(FileName, 1, Length(FileName) - 1);
    FileName := '';
  end;
  DefaultExt := '*';

  FileNameBuffer := AllocMem(FileNameBufferLen * 2 + 2);

  if Length(FileName) > FileNameBufferLen then
    FileNameBufferSize := FileNameBufferLen
  else
    FileNameBufferSize := Length(FileName);

  Move(PWideChar(FileName)^, FileNameBuffer^, FileNameBufferSize * 2);

  Filter := 'All File Types(*.*)'+#0+'*.*'+#0; // Default -> avoid empty combobox

  FillChar(OpenFile, SizeOf(OpenFile), 0);
  OpenFile.hInstance := hInstance;
  with OpenFile do
  begin
    lStructSize := SizeOf(OpenFile);
    hWndOwner := 0;

    nFilterIndex := 0;

    lpStrFile := FileNameBuffer;
    lpstrFilter := PWideChar(Filter);
    lpstrTitle := PWideChar(WideString('Title'));
    lpstrInitialDir := PWideChar(InitialDir);
    lpstrDefExt := PWideChar(DefaultExt);
    lpStrFile := FileNameBuffer;

    nMaxFile := FileNameBufferLen + 1; // Size in TCHARs

    Flags := OFN_EXPLORER;
    if AllowMulti then Flags := Flags or OFN_ALLOWMULTISELECT;
  end;
  Result := GetOpenFileNameW({$IFDEF FPC}@{$ENDIF}OpenFile);
  if Result then
    FileName := FileNameBuffer;                 
  FreeMem(FileNameBuffer);
end;

initialization
  OleInitialize(nil);
  User32Lib := LoadLibrary(User32);
  if User32Lib <> 0 then
    @UpdateLayeredWindow := GetProcAddress(User32Lib, 'UpdateLayeredWindow');
  DefaultPlatformClass := TvgPlatformWin;
finalization
  if FTimerData <> nil then
    FreeAndNil(FTimerData);
  if User32Lib <> 0 then
    FreeLibrary(User32Lib);
end.