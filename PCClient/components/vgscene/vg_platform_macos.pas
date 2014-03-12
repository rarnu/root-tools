unit vg_platform_macos;

{$mode objfpc}{$H+}
{$modeswitch objectivec1}

interface

uses
  {$IFDEF DARWIN}
  CocoaAll,
  CFBase, CFString,
  CGContext, CGImage, CGBitmapContext, CGGeometry, CGColorSpace,
  {$ENDIF}
  Classes, SysUtils, Variants, vg_scene, vg_forms;

type

  { TvgPlatformCocoa }

  TvgPlatformCocoa = class(TvgPlatform)
  private
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
    { International }
    function GetCurrentLangID: string; override;
    { Dialogs }
    function DialogOpenFiles(var FileName: WideString; AInitDir: WideString; AllowMulti: boolean): boolean; override;
  end;

implementation

type
  ApplicationDelegate = objcclass(NSObject)
    procedure applicationWillTerminate(notification : NSNotification); message 'applicationWillTerminate:';
 end;

var
  pool   : NSAutoreleasePool;
  appDel : ApplicationDelegate;
  NSVGScenePBoardtype: NSString;

procedure ApplicationDelegate.applicationWillTerminate(notification : NSNotification);
begin
  Application.Free;
  Application := nil;
end;

{ TvgPlatformCocoa }

constructor TvgPlatformCocoa.Create(AOwner: TComponent);
begin
  inherited;
  pool := NSAutoreleasePool.new;
  appDel := ApplicationDelegate.alloc.init;
  NSApplication.sharedApplication.setDelegate(appDel);
  Application := TvgApplication.Create(nil);
  NSVGScenePBoardtype := NSStr(PChar('NSVGScenePBoardtype' + IntToStr(Integer(Pointer(Application)))));
end;

destructor TvgPlatformCocoa.Destroy;
begin
  pool.release;
  inherited;
end;

{ App =========================================================================}

procedure TvgPlatformCocoa.Run;
begin
  Application.RealCreateForms;
  NSApp.run;
end;

procedure TvgPlatformCocoa.Terminate;
begin
  NSApp.terminate(nil);
end;

function TvgPlatformCocoa.HandleMessage: boolean;
var
  event : NSEvent;
begin
  event := NSApp.nextEventMatchingMask_untilDate_inMode_dequeue(NSAnyEventMask, nil, NSDefaultRunLoopMode, true);
  NSApp.sendEvent(event);
end;

procedure TvgPlatformCocoa.WaitMessage;
var
  event : NSEvent;
begin
  event := NSApp.nextEventMatchingMask_untilDate_inMode_dequeue(NSAnyEventMask, NSDate.distantFuture, NSDefaultRunLoopMode, true);
  NSApp.sendEvent(event);
end;

{ Timer =======================================================================}

type

  TCocoaTimerObject = objcclass(NSObject)
    func : TvgTimerProc;
    procedure timerEvent; message 'timerEvent';
    class function initWithFunc(afunc: TvgTimerProc): TCocoaTimerObject; message 'initWithFunc:';
  end;

procedure TCocoaTimerObject.timerEvent;
begin
  if Assigned(@func) then func;
end;

class function TCocoaTimerObject.initWithFunc(afunc: TvgTimerProc): TCocoaTimerObject;
begin
  Result:=alloc;
  Result.func:=afunc;
end;

function TvgPlatformCocoa.CreateTimer(Interval: integer;
  TimerFunc: TvgTimerProc): THandle;
var
  timer : NSTimer;
  user  : TCocoaTimerObject;
begin
  user := TCocoaTimerObject.initWithFunc(TimerFunc);

  timer := NSTimer.timerWithTimeInterval_target_selector_userInfo_repeats(
    Interval/1000, user, objcselector(user.timerEvent), user, True);

  NSRunLoop.currentRunLoop.addTimer_forMode(timer, NSDefaultRunLoopMode);

  {user is retained (twice, because it's target), by the timer and }
  {released (twice) on timer invalidation}
  user.release;

  Result := Integer(Pointer(timer));
end;

function TvgPlatformCocoa.DestroyTimer(Timer: THandle): boolean;
var
  obj : NSObject;
begin
  obj := NSObject(PtrInt(Timer));
  try
    Result := Assigned(obj) and obj.isKindOfClass_(NSTimer);
  except
    Result := false;
  end;
  if not Result then Exit;
  NSTimer(obj).invalidate;
end;

function TvgPlatformCocoa.GetTick: single;
var
  H, M, S, MS: word;
begin
  DecodeTime(time, H, M, S, MS);
  Result := ((((H * 60 * 60) + (M * 60) + S) * 1000) + MS) / 1000;
end;

{ Window ======================================================================}

const
	kCGBaseWindowLevelKey = 0;
	kCGMinimumWindowLevelKey = 1;
	kCGDesktopWindowLevelKey = 2;
	kCGBackstopMenuLevelKey = 3;
	kCGNormalWindowLevelKey = 4;
	kCGFloatingWindowLevelKey = 5;
	kCGTornOffMenuWindowLevelKey = 6;
	kCGDockWindowLevelKey = 7;
	kCGMainMenuWindowLevelKey = 8;
	kCGStatusWindowLevelKey = 9;
	kCGModalPanelWindowLevelKey = 10;
	kCGPopUpMenuWindowLevelKey = 11;
	kCGDraggingWindowLevelKey = 12;
	kCGScreenSaverWindowLevelKey = 13;
	kCGMaximumWindowLevelKey = 14;
	kCGOverlayWindowLevelKey = 15;
	kCGHelpWindowLevelKey = 16;
	kCGUtilityWindowLevelKey = 17;
	kCGDesktopIconWindowLevelKey = 18;
	kCGCursorWindowLevelKey = 19;
	kCGAssistiveTechHighWindowLevelKey = 20;
	kCGNumberOfWindowLevelKeys = 21;	{ Must be last. }

type

  TvgNSWindow = objcclass;

  { TvgNSView }

  TvgNSView = objcclass(NSView)
  public
    NSWnd: TvgNSWindow;
    function menuForEvent(event: NSEvent): NSMenu; override;
    procedure rightMouseDown(event: NSEvent); override;
    procedure rightMouseUp(event: NSEvent); override;
    procedure drawRect(r: NSRect); override;
  end;

  { TvgNSWindow }

  TvgNSWindow = objcclass(NSWindow)
  protected
    function windowShouldClose(sender : id): LongBool; message 'windowShouldClose:';
    procedure windowWillClose(notification: NSNotification); message 'windowWillClose:';
    procedure windowDidBecomeKey(notification: NSNotification); message 'windowDidBecomeKey:';
    procedure windowDidResignKey(notification: NSNotification); message 'windowDidResignKey:';
    procedure windowDidResize(notification: NSNotification); message 'windowDidResize:';
    procedure windowDidMove(notification: NSNotification); message 'windowDidMove:';
    function draggingEntered(sender: NSDraggingInfoProtocol): NSDragOperation; message 'draggingEntered:';
    procedure draggingExited(sender: id); message 'draggingExited:';
    function draggingUpdated(sender: NSDraggingInfoProtocol): NSDragOperation; message 'draggingUpdated:';
    function performDragOperation(sender: NSDraggingInfoProtocol): Boolean; message 'performDragOperation:';
  public
    NeedUpdateShadow: boolean;
    Wnd: TvgCustomForm;
    View: TvgNSView;
    LastEvent: NSEvent; // for DragNDrop
    function acceptsFirstResponder: Boolean; override;
    function canBecomeKeyWindow: Boolean; override;
    procedure mouseUp(event: NSEvent); override;
    procedure mouseDown(event: NSEvent); override;
    procedure mouseDragged(event: NSEvent); override;
    procedure mouseEntered(event: NSEvent); override;
    procedure mouseExited(event: NSEvent); override;
    procedure mouseMoved(event: NSEvent); override;
    procedure keyDown(event: NSEvent); override;
    procedure keyUp(event: NSEvent); override;
  end;

function TvgNSView.menuForEvent(event: NSEvent): NSMenu;
begin
  inherited menuForEvent(event);
end;

procedure TvgNSView.rightMouseDown(event: NSEvent);
var
  mp : NSPoint;
begin
  mp := event.locationInWindow;
  mp.y := bounds.size.height-mp.y;
  NSWnd.Wnd.MouseDown(mbRight, [ssRight], mp.x, mp.y);
  inherited rightMouseDown(event);
end;

procedure TvgNSView.rightMouseUp(event: NSEvent);
var
  mp : NSPoint;
begin
  mp := event.locationInWindow;
  mp.y := bounds.size.height-mp.y;
  NSWnd.Wnd.MouseUp(mbRight, [ssRight], mp.x, mp.y);
  inherited rightMouseUp(event);
end;

procedure TvgNSView.drawRect(r: NSRect);
var
  VR: TvgRect;
  nctx: NSGraphicsContext;
begin
  VR := vgRect(r.origin.x, bounds.size.height - r.origin.y - r.size.height, r.origin.x + r.size.width, bounds.size.height - r.origin.y);
  nctx := NSGraphicsContext.currentContext;
  if NSWnd <> nil then
  begin
    NSWnd.Wnd.Canvas.Handle := cardinal(nctx.graphicsPort);
    NSWnd.Wnd.PaintRects([VR]);
    NSWnd.Wnd.Canvas.Handle := 0;

    if NSWnd.NeedUpdateShadow and NSWnd.isVisible then
    begin
      NSWnd.invalidateShadow;
      NSWnd.NeedUpdateShadow := false;
    end;
  end;
end;

function TvgNSWindow.windowShouldClose(sender: id): LongBool;
begin
  if Application = nil then Exit;
  if Application.Terminated then Exit;
  Result := wnd.CloseQuery;
end;

procedure TvgNSWindow.windowWillClose(notification: NSNotification);
begin
  if Application = nil then Exit;
  if Application.Terminated then Exit;
  wnd.Close;
end;

procedure TvgNSWindow.windowDidBecomeKey(notification: NSNotification);
begin
  wnd.Activate;
end;

procedure TvgNSWindow.windowDidResignKey(notification: NSNotification);
begin
  if not wnd.StaysOpen and wnd.Visible then
  begin
    wnd.Close;
  end
  else
    wnd.Deactivate;
end;

procedure TvgNSWindow.windowDidResize(notification: NSNotification);
begin
  Wnd.SetBounds(round(frame.origin.x), round(NSScreen.mainScreen.frame.size.height - frame.origin.y - frame.size.height), round(frame.size.width), round(frame.size.height));
end;

procedure TvgNSWindow.windowDidMove(notification: NSNotification);
begin
  Wnd.SetBounds(round(frame.origin.x), round(NSScreen.mainScreen.frame.size.height - frame.origin.y - frame.size.height), round(frame.size.width), round(frame.size.height));
end;

var
  GlobalData: TvgDragObject;

function GetDataObject(sender: NSDraggingInfoProtocol): TvgDragObject;
var
  pboard: NSPasteboard;
  str: NSString;
  arr: NSArray;
  W: WideString;
  i: integer;
begin
  FillChar(Result, SizeOf(Result), 0);

  pboard := sender.draggingPasteboard;
  if pboard.types.containsObject(NSVGscenePboardType) then
  begin
    Result := GlobalData;
    Exit;
  end;
  if pboard.types.containsObject(NSStringPboardType) then
  begin
    str := pboard.stringForType(NSStringPboardType);
    W := UTF8Decode(str.UTF8String);
    Result.Data := W;
  end;
  if pboard.types.containsObject(NSFilenamesPboardType) then
  begin
    arr := pboard.propertyListForType(NSFilenamesPboardType);
    SetLength(Result.Files, arr.count);
    for i := 0 to arr.count - 1 do
    begin
      str := arr.objectAtIndex(i);
      W := UTF8Decode(str.UTF8String);
      Result.Files[i] := W;
    end;
  end;
end;

function TvgNSWindow.draggingEntered(sender: NSDraggingInfoProtocol): NSDragOperation;
var
  mp : NSPoint;
  P: TvgPoint;
begin
  mp := sender.draggingLocation;
  mp.y := View.bounds.size.height - mp.y;
  P := vgPoint(mp.x, mp.y);
  Wnd.DragEnter(GetDataObject(sender), P);
  Result := NSDragOperationEvery;
end;

procedure TvgNSWindow.draggingExited(sender: id);
begin
  Wnd.DragLeave;
end;

function TvgNSWindow.draggingUpdated(sender: NSDraggingInfoProtocol): NSDragOperation;
var
  mp : NSPoint;
  P: TvgPoint;
  Accept: boolean;
begin
  mp := sender.draggingLocation;
  mp.y := View.bounds.size.height - mp.y;
  P := vgPoint(mp.x, mp.y);
  Accept := false;
  Wnd.DragOver(GetDataObject(sender), P, Accept);
  if Accept then
    Result := NSDragOperationLink
  else
    Result := NSDragOperationNone;
end;

function TvgNSWindow.performDragOperation(sender: NSDraggingInfoProtocol): Boolean;
var
  mp : NSPoint;
  P: TvgPoint;
begin
  mp := sender.draggingLocation;
  mp.y := View.bounds.size.height - mp.y;
  P := vgPoint(mp.x, mp.y);
  Wnd.DragDrop(GetDataObject(sender), P);
  Result := true;
end;

function TvgNSWindow.acceptsFirstResponder: Boolean;
begin
  Result := true;
end;

function TvgNSWindow.canBecomeKeyWindow: Boolean;
begin
  Result := true;
end;

procedure TvgNSWindow.mouseUp(event: NSEvent);
var
  mp : NSPoint;
begin
  mp := event.locationInWindow;
  mp.y := NSView(event.window.contentView).bounds.size.height-mp.y;
  Wnd.MouseUp(mbLeft, [ssLeft], mp.x, mp.y);
  inherited mouseUp(event);
end;

procedure TvgNSWindow.mouseDown(event: NSEvent);
var
  mp : NSPoint;
begin
  mp:=event.locationInWindow;
  mp.y:=NSView(event.window.contentView).bounds.size.height-mp.y;
  LastEvent := event;
  Wnd.MouseDown(mbLeft, [ssLeft], mp.x, mp.y);
  LastEvent := nil;

  inherited mouseDown(event);
end;

procedure TvgNSWindow.mouseDragged(event: NSEvent);
var
  mp : NSPoint;
begin
  mp:=event.locationInWindow;
  mp.y:=NSView(event.window.contentView).bounds.size.height-mp.y;
  Wnd.MouseMove([ssLeft], mp.x, mp.y);
  inherited mouseMoved(event);
end;

procedure TvgNSWindow.mouseMoved(event: NSEvent);
var
  mp : NSPoint;
begin
  mp:=event.locationInWindow;
  mp.y:=NSView(event.window.contentView).bounds.size.height-mp.y;
  Wnd.MouseMove([], mp.x, mp.y);
  inherited mouseMoved(event);
end;

procedure TvgNSWindow.keyDown(event: NSEvent);
var
  S: WideString;
  K: word;
  Ch: System.WideChar;
  M: NSUInteger;
  Shift: TShiftState;
  IsChar: boolean;
begin
  M := event.modifierFlags;
  Shift := [];
  IsChar := true;
  if M and NSShiftKeyMask = NSShiftKeyMask then
  begin
    Shift := Shift + [ssShift];
    M := M and not NSShiftKeyMask;
  end;
  if M and NSControlKeyMask = NSControlKeyMask then
  begin
    Shift := Shift + [ssCtrl];
    M := M and not NSControlKeyMask;
  end;
  if M and NSAlternateKeyMask = NSAlternateKeyMask then
  begin
    Shift := Shift + [ssAlt];
    M := M and not NSAlternateKeyMask;
  end;
  if M and NSCommandKeyMask = NSCommandKeyMask then
  begin
    Shift := Shift + [ssMeta];
    M := M and not NSCommandKeyMask;
  end;
  if M and NSNumericPadKeyMask = NSNumericPadKeyMask then
  begin
    M := M and not NSCommandKeyMask;
    IsChar := false;
  end;
  if M and NSFunctionKeyMask = NSFunctionKeyMask then
  begin
    M := M and not NSFunctionKeyMask;
    IsChar := false;
  end;
  if IsChar then
  begin
    S := Utf8Decode(event.characters.UTF8String);
    if S <> '' then
    begin
      K := 0;
      Ch := S[1];
      Wnd.KeyDown(K, Ch, Shift);
    end;
  end;
  inherited keyDown(event);
end;

procedure TvgNSWindow.keyUp(event: NSEvent);
var
  S: WideString;
  K: word;
  Ch: System.WideChar;
  M: NSUInteger;
  Shift: TShiftState;
  IsChar: boolean;
begin
  M := event.modifierFlags;
  Shift := [];
  IsChar := true;
  if M and NSShiftKeyMask = NSShiftKeyMask then
  begin
    Shift := Shift + [ssShift];
    M := M and not NSShiftKeyMask;
  end;
  if M and NSControlKeyMask = NSControlKeyMask then
  begin
    Shift := Shift + [ssCtrl];
    M := M and not NSControlKeyMask;
  end;
  if M and NSAlternateKeyMask = NSAlternateKeyMask then
  begin
    Shift := Shift + [ssAlt];
    M := M and not NSAlternateKeyMask;
  end;
  if M and NSCommandKeyMask = NSCommandKeyMask then
  begin
    Shift := Shift + [ssMeta];
    M := M and not NSCommandKeyMask;
  end;
  if M and NSNumericPadKeyMask = NSNumericPadKeyMask then
  begin
    M := M and not NSCommandKeyMask;
    IsChar := false;
  end;
  if M and NSFunctionKeyMask = NSFunctionKeyMask then
  begin
    M := M and not NSFunctionKeyMask;
    IsChar := false;
  end;
  if IsChar then
  begin
    S := Utf8Decode(event.characters.UTF8String);
    if S <> '' then
    begin
      K := 0;
      Ch := S[1];
      Wnd.KeyUp(K, Ch, Shift);
    end;
  end;
  inherited keyUp(event);
end;

procedure TvgNSWindow.mouseEntered(event: NSEvent);
begin
  inherited mouseEntered(event);
end;

procedure TvgNSWindow.mouseExited(event: NSEvent);
begin
  inherited mouseExited(event);
end;

function TvgPlatformCocoa.CreateWindow(AForm: TvgCustomForm): THandle;
var
  Style: NSUInteger;
  R: NSRect;
begin
  if AForm.Transparency then
    Style := NSBorderlessWindowMask
  else
  begin
    Style := NSTitledWindowMask or NSUnifiedTitleAndToolbarWindowMask;
    if AForm.BorderStyle <> bsNone then
    begin
      if biMinimize in AForm.BorderIcons then Style := Style or NSMiniaturizableWindowMask;
      if biMaximize in AForm.BorderIcons then Style := Style or NSWindowZoomButton;
      if biSystemMenu in AForm.BorderIcons then Style := Style or NSClosableWindowMask;
    end;
    if AForm.BorderStyle in [bsSizeable, bsSizeToolWin] then
      Style := Style or NSResizableWindowMask;
  end;
  R := NSWindow.contentRectForFrameRect_styleMask(NSMakeRect(AForm.Left, NSScreen.mainScreen.frame.size.height - AForm.Top - AForm.Height, AForm.width, AForm.Height), Style);
  Result := cardinal(TvgNSWindow.alloc.initWithContentRect_styleMask_backing_defer(
             R,
             Style,
             NSBackingStoreBuffered, True));
  TvgNSWindow(Pointer(Result)).setDelegate(TvgNSWindow(Pointer(Result)));
  TvgNSWindow(Pointer(Result)).setAcceptsMouseMovedEvents(True);
  TvgNSWindow(Pointer(Result)).setBackgroundColor(NSColor.clearColor);
  TvgNSWindow(Pointer(Result)).setShowsToolbarButton(true);
  TvgNSWindow(Pointer(Result)).useOptimizedDrawing(true);
  TvgNSWindow(Pointer(Result)).setTitle(NSSTR(PChar(UTF8Encode(AForm.Caption))));
  if AForm.TopMost then
    TvgNSWindow(Pointer(Result)).setLevel(kCGModalPanelWindowLevelKey);
  TvgNSWindow(Pointer(Result)).Wnd := AForm;

  TvgNSWindow(Pointer(Result)).View := TvgNSView.alloc.init;
  TvgNSWindow(Pointer(Result)).View.NSWnd := TvgNSWindow(Pointer(Result));
  TvgNSWindow(Pointer(Result)).setContentView(TvgNSWindow(Pointer(Result)).View);

  if AForm.Transparency then
    TvgNSWindow(Pointer(Result)).setOpaque(false)
  else
    TvgNSWindow(Pointer(Result)).setOpaque(true);
  TvgNSWindow(Pointer(Result)).setHasShadow(true);

  TvgNSWindow(Pointer(Result)).registerForDraggedTypes(NSArray.arrayWithObjects(NSStringPboardType, NSFilenamesPboardType, NSVGScenePBoardtype, nil))
end;

procedure TvgPlatformCocoa.DestroyWindow(AForm: TvgCustomForm);
begin
  TvgNSWindow(Pointer(AForm.Handle)).setDelegate(nil);
  TvgNSWindow(Pointer(AForm.Handle)).View.NSWnd := nil;
  TvgNSWindow(Pointer(AForm.Handle)).View := nil;
  TvgNSWindow(Pointer(AForm.Handle)).Wnd := nil;
  TvgNSWindow(Pointer(AForm.Handle)).release;
  AForm.Handle := 0;

  if not (fsRecreating in AForm.FormState) and not (AForm.ShowActivated) and (AForm <> Application.MainForm) then
    NSWindow(Application.MainForm.Handle).makeKeyAndOrderFront(nil);
end;

procedure TvgPlatformCocoa.ReleaseWindow(AForm: TvgCustomForm);
begin
  TvgNSWindow(Pointer(AForm.Handle)).setHasShadow(false);
  TvgNSWindow(Pointer(AForm.Handle)).View.NSWnd := nil;
  TvgNSWindow(Pointer(AForm.Handle)).View := nil;
  TvgNSWindow(Pointer(AForm.Handle)).Wnd := nil;
  AForm.Handle := 0;
end;

procedure TvgPlatformCocoa.SetWindowRect(AForm: TvgCustomForm; ARect: TvgRect);
begin
  NSWindow(AForm.Handle).setFrame_display(NSRect(CGRectMake(ARect.Left, NSScreen.mainScreen.frame.size.height - ARect.Bottom, ARect.Right - ARect.Left, ARect.Bottom - ARect.Top)), true);
end;

procedure TvgPlatformCocoa.InvalidateWindowRect(AForm: TvgCustomForm; R: TvgRect);
begin
  if not vgIntersectRect(R, vgRect(0, 0, AForm.Width, AForm.Height)) then Exit;
  TvgNSWindow(Pointer(AForm.Handle)).View.setNeedsDisplayInRect(NSRect(CGRectMake(R.left, TvgNSWindow(Pointer(AForm.Handle)).View.bounds.size.height - R.bottom, R.right - R.left, R.bottom - R.top)));
end;

function TvgPlatformCocoa.GetWindowRect(AForm: TvgCustomForm): TvgRect;
begin

end;

procedure TvgPlatformCocoa.SetWindowCaption(AForm: TvgCustomForm;
  ACaption: WideString);
begin
  NSWindow(AForm.Handle).setTitle(NSSTR(PChar(UTF8Encode(ACaption))));
end;

procedure TvgPlatformCocoa.ReleaseCapture(AForm: TvgCustomForm);
begin
//  Windows.ReleaseCapture;
end;

procedure TvgPlatformCocoa.SetCapture(AForm: TvgCustomForm);
begin
//  Windows.SetCapture(AHandle);
end;

function TvgPlatformCocoa.GetClientSize(AForm: TvgCustomForm): TvgPoint;
begin
  Result := vgPoint(TvgNSWindow(Pointer(AForm.Handle)).View.frame.size.width, TvgNSWindow(Pointer(AForm.Handle)).View.frame.size.height);
end;

procedure TvgPlatformCocoa.HideWindow(AForm: TvgCustomForm);
begin
  NSWindow(AForm.Handle).orderOut(nil);
end;

procedure TvgPlatformCocoa.ShowWindow(AForm: TvgCustomForm);
var
  frame: NSRect;
begin
  if AForm.ShowActivated then
  begin
    NSWindow(AForm.Handle).makeKeyAndOrderFront(nil);
  end
  else
  begin
    NSWindow(AForm.Handle).makeKeyAndOrderFront(nil);
  end;
  frame := TvgNSWindow(Pointer(AForm.Handle)).frame;
  AForm.SetBounds(round(frame.origin.x), round(NSScreen.mainScreen.frame.size.height - frame.origin.y - frame.size.height), round(frame.size.width), round(frame.size.height));
  if AForm.Transparency then
    TvgNSWindow(Pointer(AForm.Handle)).NeedUpdateShadow := true;
end;

function TvgPlatformCocoa.ShowWindowModal(AForm: TvgCustomForm): TModalResult;
var
  Wnd: TvgCustomForm;
begin
{  if GetCapture <> 0 then SendMessage(GetCapture, WM_CANCELMODE, 0, 0);
  Windows.ReleaseCapture;
  Wnd.Show;
  Wnd.ModalResult := 0;
  repeat
    Application.HandleMessage;
    if Application.Terminated then
      Wnd.ModalResult := mrCancel
{    else
      if Wnd.ModalResult <> 0 then CloseModal;}
  until Wnd.ModalResult <> 0;
  Wnd.Hide;
  Result := Wnd.ModalResult; }
end;

function TvgPlatformCocoa.ClientToScreen(AForm: TvgCustomForm; const Point: TvgPoint): TvgPoint;
var
  np: NSPoint;
begin
  np := NSPoint(Point);
  np.y := NSWindow(AForm.Handle).contentView.bounds.size.height - np.y;
  Result := TvgPoint(NSWindow(AForm.Handle).convertBaseToScreen(np));
  Result.y := NSScreen.mainScreen.frame.size.height - Result.y;
end;

function TvgPlatformCocoa.ScreenToClient(AForm: TvgCustomForm; const Point: TvgPoint): TvgPoint;
begin
  Result := TvgPoint(NSWindow(AForm.Handle).convertScreenToBase(NSPoint(Point)));
end;

{ Drag and Drop ===============================================================}

function NSImageFromBitmap(Bmp: TvgBitmap): NSImage;
var
  mem: TMemoryStream;
  data: NSData;
begin
  mem := TMemoryStream.Create;
  Bmp.SaveToStream(mem);
  data := NSData.dataWithBytes_length(mem.Memory, mem.Size);
  Result := NSImage.alloc.initWithData(data);
  mem.Free;
end;

procedure TvgPlatformCocoa.BeginDragDrop(AForm: TvgCustomForm;
  const Data: TvgDragObject; ABitmap: TvgBitmap);
var
  img: NSImage;
  loc: NSPoint;
  pboard: NSPasteboard;
begin
  img := NSImageFromBitmap(ABitmap);
  pboard := NSPasteboard.pasteboardWithName(NSDragPboard);
  pboard.declareTypes_owner(NSArray.arrayWithObjects(NSVGScenePBoardtype, nil), pboard);
{  if not VarIsObject(Data.Data) and VarIsStr(Data.Data) then
    pboard.setString_forType(NSStr(PChar(Utf8Encode(Data.Data))), NSStringPBoardtype);}
  GlobalData := Data;
  loc := TvgNSWindow(Pointer(AForm.Handle)).LastEvent.locationInWindow;
  loc.x := loc.x - (ABitmap.Width div 2);
  loc.y := loc.y - (ABitmap.Height div 2);
  NSWindow(AForm.Handle).dragImage_at_offset_event_pasteboard_source_slideBack(img, loc,
    NSSize(vgPoint(0, 0)), TvgNSWindow(Pointer(AForm.Handle)).LastEvent, pboard, TvgNSWindow(Pointer(AForm.Handle)).View, true);
end;

{ Clipboard ===============================================================}

procedure TvgPlatformCocoa.SetClipboard(Value: Variant);
var
  W: WideString;
  pb: NSPasteboard;
  types: NSArray;
  str: NSString;
begin
  if VarIsStr(Value) then
  begin
    W := Value;
    str := NSString.alloc.initWithUTF8String(PChar(UTF8Encode(W)));
    pb := NSPasteboard.generalPasteboard;
    types := NSArray.arrayWithObjects(NSStringPBoardtype, nil);
    pb.declareTypes_owner(types, pb);
    pb.setString_forType(str, NSStringPBoardtype);
  end;
end;

function TvgPlatformCocoa.GetClipboard: Variant;
var
  W: WideString;
  pb: NSPasteboard;
  str: NSString;
begin
  Result := NULL;
  pb := NSPasteboard.generalPasteboard;
  str := pb.stringForType(NSStringPBoardtype);
  if str <> nil then
  begin
    W := UTF8Decode(str.UTF8String);
    Result := W;
  end;
end;

{ Mouse  ===============================================================}

function TvgPlatformCocoa.GetMousePos: TvgPoint;
var
  p: NSPoint;
begin
  p := NSEvent.mouseLocation;
  Result := vgPoint(p.x, p.y);
  Result.y := NSScreen.mainScreen.frame.size.height - Result.y;
end;

{ International ===============================================================}

function TvgPlatformCocoa.GetCurrentLangID: string;
begin
  Result := NSLocale.currentLocale.localeIdentifier.UTF8String;
  if Length(Result) > 2 then
    Delete(Result, 3, MaxInt);
end;

{ Dialogs ===============================================================}

function TvgPlatformCocoa.DialogOpenFiles(var FileName: WideString;
  AInitDir: WideString; AllowMulti: boolean): boolean;
begin
  Result := false;
end;

initialization
  DefaultPlatformClass := TvgPlatformCocoa;
finalization
end.