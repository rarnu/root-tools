unit vg_app_macos;

{$I vg_define.inc}
{$mode objfpc}{$H+}
{$modeswitch objectivec1}

interface

uses
  {$IFDEF DARWIN}
  iPhoneAll,
  CFBase, CFString,
  CGContext, CGImage, CGBitmapContext, CGGeometry, CGColorSpace,
  {$ENDIF}
  Classes, SysUtils, TypInfo, vg_scene, vg_app;

type

  { TAppDelegate }

  TAppDelegate = objcclass(NSObject)
    procedure applicationDidFinishLaunching(application: UIApplication); message 'applicationDidFinishLaunching:';
    procedure applicationWillTerminate(application: UIApplication); message 'applicationWillTerminate:';
  public
    procedure DeviceOrientationDidChange(n: NSNotification); message 'DeviceOrientationDidChange:';
  end;

  { TNSView }

  TNSView = objcclass(UIView)
    FForm: TObject;
  public
    procedure drawRect(c: CGRect); override;
    procedure touchesBegan_withEvent(touches: NSSetPointer; event: UIEvent); override;
    procedure touchesMoved_withEvent(touches: NSSetPointer; event: UIEvent); override;
    procedure touchesEnded_withEvent(touches: NSSetPointer; event: UIEvent); override;
    procedure touchesCancelled_withEvent(touches: NSSetPointer; event: UIEvent); override;
  end;


  { TTextFieldDelegate }

  { TNSWindow }

  TNSWindow = objcclass(UIWindow)
    FForm: TObject;
    FView: TNSView;
    FText: UITextField;
  public
    function textField_shouldChangeCharactersInRange_replacementString(textField: UITextField; range: NSRange; string_: NSStringPointer): Boolean; message 'textField:shouldChangeCharactersInRange:replacementString:';
    function textFieldShouldReturn(textField: UITextField): Boolean; message 'textFieldShouldReturn:';
    function textFieldShouldClear(textField: UITextField): Boolean; message 'textFieldShouldClear:';
  end;

implementation

uses vg_forms, vg_controls;

{ TAppDelegate }

procedure TAppDelegate.applicationDidFinishLaunching(application: UIApplication);
begin
  vg_app.Application.RealCreateForms;
  NSNotificationCenter.defaultCenter.addObserver_selector_name_object(Self, objcselector(DeviceOrientationDidChange),
    UIDeviceOrientationDidChangeNotification, nil);
  UIDevice.currentDevice.beginGeneratingDeviceOrientationNotifications;
end;

procedure TAppDelegate.applicationWillTerminate(application: UIApplication);
begin
  UIDevice.currentDevice.endGeneratingDeviceOrientationNotifications;
  NSNotificationCenter.defaultCenter.removeObserver(Self);
end;

procedure TAppDelegate.DeviceOrientationDidChange(n: NSNotification);
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
  if vg_app.Application.MainForm = nil then Exit;
  if not (vg_app.Application.MainForm is TvgForm) then Exit;
  if TvgVisualObject(TvgForm(vg_app.Application.MainForm).Root).Rotateangle <> Angle then
  begin
    TvgVisualObject(TvgForm(vg_app.Application.MainForm).Root).Rotateangle := Angle;
    TvgForm(vg_app.Application.MainForm).RealignRoot;
  end;
end;

type
  THackForm = class(TForm);

{ TNSView }

procedure TNSView.drawRect(c: CGRect);
var
  cg : CGContextRef;
begin
  // getting current context
  cg := UIGraphicsGetCurrentContext;

  THackForm(FForm).FCtx := cg;
  THackForm(FForm).PaintRects([vgRect(c.origin.x, c.origin.y, c.origin.x + c.size.width, c.origin.y + c.size.height)]);
  THackForm(FForm).FCtx := nil;
end;

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

procedure TNSView.touchesBegan_withEvent(touches: NSSetPointer; event: UIEvent);
var
  x, y : single;
begin
  if not GetTouchCoord(touches, self, x, y) then Exit;
  THackForm(FForm).MouseDown(mbLeft, [], x, y);
  inherited touchesBegan_withEvent(touches, event);
end;

procedure TNSView.touchesMoved_withEvent(touches: NSSetPointer; event: UIEvent);
var
  x, y : single;
begin
  if not GetTouchCoord(touches, self, x, y) then Exit;
  THackForm(FForm).MouseMove([ssLeft], x, y);
  inherited touchesMoved_withEvent(touches, event);
end;

procedure TNSView.touchesEnded_withEvent(touches: NSSetPointer; event: UIEvent);
var
  x, y : single;
begin
  if not GetTouchCoord(touches, self, x, y) then Exit;
  THackForm(FForm).MouseUp(mbLeft, [], x, y);
  inherited touchesEnded_withEvent(touches, event);
end;

procedure TNSView.touchesCancelled_withEvent(touches: NSSetPointer; event: UIEvent);
begin
  inherited touchesCancelled_withEvent(touches, event);
end;

{ TNSWindow }

function TNSWindow.textField_shouldChangeCharactersInRange_replacementString(
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
    THackForm(FForm).KeyDown(K, Ch, []);
  end
  else
  begin
    K := 8;
    Ch := #0;
    THackForm(FForm).KeyDown(K, Ch, []);
  end;
  Result := true;
end;

function TNSWindow.textFieldShouldReturn(textField: UITextField): Boolean;
var
  Ch: Widechar;
  K: Word;
begin
  K := VK_RETURN;
  Ch := #0;
  THackForm(FForm).KeyDown(K, Ch, []);
  Result := true;
end;

function TNSWindow.textFieldShouldClear(textField: UITextField): Boolean;
begin
  if TvgForm(vg_app.Application.MainForm).FTextObject <> nil then
    SetPropValue(TvgForm(vg_app.Application.MainForm).FTextObject, 'Text', '');
  Result := true;
end;

end.