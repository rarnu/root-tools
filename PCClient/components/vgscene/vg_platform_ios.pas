unit vg_platform_ios;

{$mode objfpc}{$H+}
{$modeswitch objectivec1}

interface

uses
  {$IFDEF DARWIN}
  iPhoneAll,
  CFBase, CFString,
  CGContext, CGImage, CGBitmapContext, CGGeometry, CGColorSpace,
  {$ENDIF}
  Classes, SysUtils, Variants, TypInfo, vg_scene, vg_forms;

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
    { Keyboard }
    function ShowVirtualKeyboard(AControl: TvgObject): boolean; override;
    function HideVirtualKeyboard: boolean; override;
  end;

implementation

type
  TvgHackForm = class(TvgForm);

  ApplicationDelegate = objcclass(NSObject)
    procedure applicationDidFinishLaunching(notification: UIApplication); message 'applicationDidFinishLaunching:';
    procedure applicationWillTerminate(notification : UIApplication); message 'applicationWillTerminate:';
    procedure DeviceOrientationDidChange(n: NSNotification); message 'DeviceOrientationDidChange:';
 end;

var
  pool: NSAutoreleasePool;

procedure ApplicationDelegate.applicationDidFinishLaunching(notification: UIApplication);
begin
  Application.RealCreateForms;
  NSNotificationCenter.defaultCenter.addObserver_selector_name_object(Self, objcselector(DeviceOrientationDidChange),
    UIDeviceOrientationDidChangeNotification, nil);
  UIDevice.currentDevice.beginGeneratingDeviceOrientationNotifications;
end;

procedure ApplicationDelegate.applicationWillTerminate(notification : UIApplication);
begin
end;

procedure ApplicationDelegate.DeviceOrientationDidChange(n: NSNotification);
var
  Angle: single;
begin
  case UIDevice.currentDevice.orientation of
    UIDeviceOrientationUnknown: Angle := 0;
    UIDeviceOrientationPortrait: Angle := 0;
    UIDeviceOrientationPortraitUpsideDown: Angle := 180;
    UIDeviceOrientationLandscapeLeft: Angle := 90;
    UIDeviceOrientationLandscapeRight: Angle := -90;
    UIDeviceOrientationFaceUp: Angle := 0;
    UIDeviceOrientationFaceDown: Angle := 0;
  end;
  exit;
  if Application.MainForm = nil then Exit;
  if not (Application.MainForm is TvgForm) then Exit;
  if TvgVisualObject(TvgForm(Application.MainForm).Root).Rotateangle <> Angle then
  begin
    TvgVisualObject(TvgForm(Application.MainForm).Root).Rotateangle := Angle;
    TvgForm(Application.MainForm).RealignRoot;
  end;
end;

{ TvgPlatformCocoa }

constructor TvgPlatformCocoa.Create(AOwner: TComponent);
begin
  inherited;
  pool := NSAutoreleasePool.new;
  Application := TvgApplication.Create(nil);
end;

destructor TvgPlatformCocoa.Destroy;
begin
  Application.Free;
  Application := nil;
  pool.release;
  inherited;
end;

{ App =========================================================================}

procedure TvgPlatformCocoa.Run;
begin
  ExitCode := UIApplicationMain(argc, argv, nil, NSSTR('ApplicationDelegate'));
end;

procedure TvgPlatformCocoa.Terminate;
begin
//  NSApp.terminate(nil);
end;

function TvgPlatformCocoa.HandleMessage: boolean;
begin
  NSRunLoop.currentRunLoop.runUntilDate(NSDate.dateWithTimeIntervalSinceNow(0.1));
  Result := false;
end;

procedure TvgPlatformCocoa.WaitMessage;
begin
  NSRunLoop.currentRunLoop.runMode_beforeDate(NSDefaultRunLoopMode, NSDate.date);
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

  Result := cardinal(timer);
end;

function TvgPlatformCocoa.DestroyTimer(Timer: THandle): boolean;
var
  obj : NSObject;
begin
  obj := NSObject(Timer);
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

type

  TvgNSWindow = objcclass;

  { TvgNSView }

  TvgNSView = objcclass(UIView)
  public
    NSWnd: TvgNSWindow;
    Rotating: boolean;
    procedure drawRect(r: CGRect); override;
    procedure touchesBegan_withEvent(touches: NSSetPointer; event: UIEvent); override;
    procedure touchesMoved_withEvent(touches: NSSetPointer; event: UIEvent); override;
    procedure touchesEnded_withEvent(touches: NSSetPointer; event: UIEvent); override;
  end;

  { TvgNSViewController }

  TvgNSViewController = objcclass(UIViewController)
  public
    Wnd: TvgNSWindow;
    procedure loadView; override;
    function shouldAutorotateToInterfaceOrientation(AinterfaceOrientation: UIInterfaceOrientation): Boolean; override;
    procedure willRotateToInterfaceOrientation_duration(toInterfaceOrientation: UIInterfaceOrientation; duration: NSTimeInterval); override;
    procedure didRotateFromInterfaceOrientation(fromInterfaceOrientation: UIInterfaceOrientation); override;
    procedure viewWillAppear(animated: Boolean); override;
    procedure viewWillDisappear(animated: Boolean); override;
  end;

  { TvgNSWindow }

  TvgNSWindow = objcclass(UIWindow)
  protected
  public
    Orientation: UIInterfaceOrientation;
    Wnd: TvgCustomForm;
    View: TvgNSView;
    Controller: TvgNSViewController;
    Text: UITextField;
    function textField_shouldChangeCharactersInRange_replacementString(textField: UITextField; range: NSRange; string_: NSStringPointer): Boolean; message 'textField:shouldChangeCharactersInRange:replacementString:';
    function textFieldShouldReturn(textField: UITextField): Boolean; message 'textFieldShouldReturn:';
    function textFieldShouldClear(textField: UITextField): Boolean; message 'textFieldShouldClear:';
  end;

{ TvgNSView }

  function GetTouchCoord(touches: NSSetPointer; Window: UIView; var x, y: single): Boolean;
  var
      st    : NSSet;
      touch : UITouch;
      p     : CGPoint;
  begin
      Result := Assigned(touches);
      if not Result then Exit;
      st := NSSet(touches);
      Result := st.count = 1;
      if not Result then Exit;

      touch := UITouch(st.anyObject);
      p := touch.locationInView(Window);
      x := p.x;
      y := p.y;
  end;

procedure TvgNSView.touchesBegan_withEvent(touches: NSSetPointer; event: UIEvent);
var
  x, y : single;
begin
  if not GetTouchCoord(touches, self, x, y) then Exit;
  NSWnd.Wnd.MouseDown(mbLeft, [], x, y);
  inherited touchesBegan_withEvent(touches, event);
end;

procedure TvgNSView.touchesMoved_withEvent(touches: NSSetPointer; event: UIEvent);
var
  x, y : single;
begin
  if not GetTouchCoord(touches, self, x, y) then Exit;
  NSWnd.Wnd.MouseMove([ssLeft], x, y);
  inherited touchesMoved_withEvent(touches, event);
end;

procedure TvgNSView.touchesEnded_withEvent(touches: NSSetPointer; event: UIEvent);
var
  x, y : single;
begin
  if not GetTouchCoord(touches, self, x, y) then Exit;
  NSWnd.Wnd.MouseUp(mbLeft, [], x, y);
  inherited touchesEnded_withEvent(touches, event);
end;

procedure TvgNSView.drawRect(r: CGRect);
begin
  if NSWnd <> nil then
  begin
    NSWnd.Wnd.Canvas.Handle := THandle(UIGraphicsGetCurrentContext);
    NSWnd.Wnd.PaintRects([vgRect(r.origin.x, r.origin.y, r.origin.x + r.size.width, r.origin.y + r.size.height)]);
{    NSWnd.Wnd.Canvas.Fill.Style := vgBrushSolid;
    NSWnd.Wnd.Canvas.Fill.SolidColor := $FF000000 or random($FFFFFF);
    NSWnd.Wnd.Canvas.FillRect(vgRect(0, 0, NSWnd.Wnd.Canvas.Width, NSWnd.Wnd.Canvas.Height), 15, 15, AllCorners, 1);}
    NSWnd.Wnd.Canvas.Handle := 0;
  end;
end;

{ TvgNSViewController }

procedure TvgNSViewController.loadView;
begin
  Wnd.Orientation := InterfaceOrientation;

  Wnd.View := TvgNSView.alloc.initwithFrame(CGRectMake(0, 20, Wnd.bounds.size.Width, Wnd.bounds.size.Height - 20)) ;
  Wnd.View.NSWnd := Wnd;
  setView(Wnd.View);

  Wnd.Text := UITextField.alloc.initWithFrame(CGRectMake(0, 0, 2000, 0));
  Wnd.Text.setDelegate(Wnd);
  Wnd.View.addSubview(Wnd.Text);
end;

function TvgNSViewController.shouldAutorotateToInterfaceOrientation(
  AinterfaceOrientation: UIInterfaceOrientation): Boolean;
begin
  Result := true;
end;

procedure TvgNSViewController.willRotateToInterfaceOrientation_duration(
  toInterfaceOrientation: UIInterfaceOrientation; duration: NSTimeInterval);
begin
  inherited willRotateToInterfaceOrientation_duration(toInterfaceOrientation, duration);
  Wnd.View.Rotating := true;
  Wnd.Wnd.SetBounds(0, 0, round(view.frame.size.Width), round(view.frame.size.Height))
end;

procedure TvgNSViewController.didRotateFromInterfaceOrientation(fromInterfaceOrientation: UIInterfaceOrientation);
begin
  inherited didRotateFromInterfaceOrientation(fromInterfaceOrientation);
  Wnd.Orientation := InterfaceOrientation;
  wnd.View.Rotating := false;
  Wnd.Wnd.SetBounds(0, 0, round(view.frame.size.Width), round(view.frame.size.Height))
end;

procedure TvgNSViewController.viewWillAppear(animated: Boolean);
begin
  inherited viewWillAppear(animated);
end;

procedure TvgNSViewController.viewWillDisappear(animated: Boolean);
begin
  inherited viewWillDisappear(animated);
end;

{ %vgNSWindow }

function TvgNSWindow.textField_shouldChangeCharactersInRange_replacementString(
  textField: UITextField; range: NSRange; string_: NSStringPointer): Boolean;
var
  C: WideString;
  Ch: Widechar;
  K: Word;
begin
  C := UTF8Decode(NSString(string_).UTF8String);
  if Length(C) > 0 then
  begin
    K := 0;
    Ch := C[1];
    Wnd.KeyDown(K, Ch, []);
  end
  else
  begin
    K := 8;
    Ch := #0;
    Wnd.KeyDown(K, Ch, []);
  end;
  Result := true;
end;

function TvgNSWindow.textFieldShouldReturn(textField: UITextField): Boolean;
var
  Ch: Widechar;
  K: Word;
begin
  K := VK_RETURN;
  Ch := #0;
  Wnd.KeyDown(K, Ch, []);
  Result := true;
end;

function TvgNSWindow.textFieldShouldClear(textField: UITextField): Boolean;
begin
  Result := true;
end;

function TvgPlatformCocoa.CreateWindow(AForm: TvgCustomForm): THandle;
begin
  Result := THandle(TvgNSWindow.alloc.initWithFrame(UIScreen.mainScreen.bounds));

  AForm.Handle := Result;
  TvgNSWindow(Result).Wnd := AForm;

  TvgNSWindow(Result).Controller := TvgNSViewController.alloc.initWithNibName_bundle(nil, nil);
  TvgNSWindow(Result).Controller.Wnd := TvgNSWindow(Result);

  TvgNSWindow(Result).setAutoresizesSubviews(true);
  TvgNSWindow(Result).AddSubView(TvgNSWindow(Result).Controller.View);

  AForm.SetBounds(0, 0, round(TvgNSWindow(Result).View.bounds.size.Width), round(TvgNSWindow(Result).View.bounds.size.Height));
end;

procedure TvgPlatformCocoa.DestroyWindow(AForm: TvgCustomForm);
begin
  TvgNSWindow(AForm.Handle).View.NSWnd := nil;
  TvgNSWindow(AForm.Handle).View := nil;
  TvgNSWindow(AForm.Handle).Wnd := nil;
  TvgNSWindow(AForm.Handle).release;
  AForm.Handle := 0;
end;

procedure TvgPlatformCocoa.ReleaseWindow(AForm: TvgCustomForm);
begin
  TvgNSWindow(AForm.Handle).View.NSWnd := nil;
  TvgNSWindow(AForm.Handle).View := nil;
  TvgNSWindow(AForm.Handle).Wnd := nil;
  AForm.Handle := 0;
end;

procedure TvgPlatformCocoa.SetWindowRect(AForm: TvgCustomForm; ARect: TvgRect);
begin
  if AForm.Handle <> 0 then
  begin
    AForm.SetBounds(0, 0, round(TvgNSWindow(AForm.Handle).bounds.size.Width), round(TvgNSWindow(AForm.Handle).bounds.size.Height));
  end;
end;

procedure TvgPlatformCocoa.InvalidateWindowRect(AForm: TvgCustomForm; R: TvgRect);
begin
  TvgNSWindow(AForm.Handle).View.setNeedsDisplayInRect(CGRectMake(R.left, R.top, R.right - R.left, R.bottom - R.top));
end;

function TvgPlatformCocoa.GetWindowRect(AForm: TvgCustomForm): TvgRect;
begin
end;

procedure TvgPlatformCocoa.SetWindowCaption(AForm: TvgCustomForm; ACaption: WideString);
begin
end;

procedure TvgPlatformCocoa.ReleaseCapture(AForm: TvgCustomForm);
begin
end;

procedure TvgPlatformCocoa.SetCapture(AForm: TvgCustomForm);
begin
end;

function TvgPlatformCocoa.GetClientSize(AForm: TvgCustomForm): TvgPoint;
begin
  if (TvgNSWindow(AForm.Handle).Orientation = UIInterfaceOrientationPortrait) or
     (TvgNSWindow(AForm.Handle).Orientation = UIInterfaceOrientationPortraitUpsideDown) then
    Result := TvgPoint(TvgNSWindow(AForm.Handle).View.frame.size)
  else
    with TvgNSWindow(AForm.Handle).View.frame.size do
      Result := vgPoint(Height, Width);
end;

procedure TvgPlatformCocoa.HideWindow(AForm: TvgCustomForm);
begin
end;

procedure TvgPlatformCocoa.ShowWindow(AForm: TvgCustomForm);
begin
  TvgNSWindow(AForm.Handle).makeKeyAndVisible;
end;

function TvgPlatformCocoa.ShowWindowModal(AForm: TvgCustomForm): TModalResult;
begin
  Result := mrOk;
end;

function TvgPlatformCocoa.ClientToScreen(AForm: TvgCustomForm; const Point: TvgPoint): TvgPoint;
begin
  Result := Point;
end;

function TvgPlatformCocoa.ScreenToClient(AForm: TvgCustomForm; const Point: TvgPoint): TvgPoint;
begin
  Result := Point;
end;

{ Drag and Drop ===============================================================}

procedure TvgPlatformCocoa.BeginDragDrop(AForm: TvgCustomForm;
  const Data: TvgDragObject; ABitmap: TvgBitmap);
begin
end;

{ Clipboard ===============================================================}

procedure TvgPlatformCocoa.SetClipboard(Value: Variant);
begin
end;

function TvgPlatformCocoa.GetClipboard: Variant;
begin
end;

{ Mouse  ===============================================================}

function TvgPlatformCocoa.GetMousePos: TvgPoint;
begin
  Result := vgPoint(0, 0);
end;

{ International ===============================================================}

function TvgPlatformCocoa.GetCurrentLangID: string;
begin
  Result := 'en'
end;

{ Dialogs ===============================================================}

function TvgPlatformCocoa.DialogOpenFiles(var FileName: WideString;
  AInitDir: WideString; AllowMulti: boolean): boolean;
begin
  Result := false;
end;

{ Keyboard }

function TvgPlatformCocoa.ShowVirtualKeyboard(AControl: TvgObject): boolean;
var
  W: WideString;
  S: NSString;
begin
  W := GetWideStrProp(AControl, 'Text');
  S := NSString.alloc.initWithUTF8String(PChar(UTF8Encode(W)));
  TvgNSWindow(Application.MainForm.Handle).Text.setText(S);
  TvgNSWindow(Application.MainForm.Handle).Text.becomeFirstResponder;
  S.release;
  Result := true;
end;

function TvgPlatformCocoa.HideVirtualKeyboard: boolean;
begin
  TvgNSWindow(Application.MainForm.Handle).Text.resignFirstResponder;
  Result := true;
end;

initialization
  DefaultPlatformClass := TvgPlatformCocoa;
  GlobalDisableFocusEffect := true;
finalization
end.