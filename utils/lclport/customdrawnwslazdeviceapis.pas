{
 *****************************************************************************
 *                         CustomDrawnLazDeviceAPIS.pas                      *
 *                                ----------                                 * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit CustomDrawnWSLazDeviceAPIS;

{$mode objfpc}{$H+}

{$I customdrawndefines.inc}

interface

uses
  // RTL
  Types, Math,
  {$ifdef CD_Android}
  jni2,
  {$endif}
  // LCL
  LazDeviceAPIs, LCLProc,
  // Widgetset
  customdrawnint, WSLazDeviceAPIs;

type
  { TWSLazDeviceAPIS }
  
  { TCDWSLazDeviceAPIs }

  TCDWSLazDeviceAPIs = class(TWSLazDeviceAPIs)
  public
    //
    class procedure RequestPositionInfo(AMethod: TLazPositionMethod); override;
    //
    class procedure SendMessage(AMsg: TLazDeviceMessage); override;
    //
    class procedure StartReadingAccelerometerData(); override;
    class procedure StopReadingAccelerometerData(); override;
    // TLazDevice
    class function GetDeviceManufacturer: string; override;
    class function GetDeviceModel: string; override;
    class function GetScreenRotation(AScreenIndex: Integer): TScreenRotation; override;
    class procedure Vibrate(ADurationMS: Cardinal); override;
  end;

implementation

{ TCDWSLazDeviceAPIs }

{$if defined(CD_Windows) or defined(CD_Cocoa) or defined(CD_X11)}
class procedure TCDWSLazDeviceAPIs.RequestPositionInfo(
  AMethod: TLazPositionMethod);
begin

end;

class procedure TCDWSLazDeviceAPIs.SendMessage(AMsg: TLazDeviceMessage);
begin

end;

class procedure TCDWSLazDeviceAPIs.StartReadingAccelerometerData;
begin

end;

class procedure TCDWSLazDeviceAPIs.StopReadingAccelerometerData;
begin

end;

class function TCDWSLazDeviceAPIs.GetDeviceManufacturer: string;
begin
  Result := '';
end;

class function TCDWSLazDeviceAPIs.GetDeviceModel: string;
begin
  Result := '';
end;

class procedure TCDWSLazDeviceAPIs.Vibrate(ADurationMS: Cardinal);
begin

end;

class function TCDWSLazDeviceAPIs.GetScreenRotation(AScreenIndex: Integer): TScreenRotation;
begin

end;

{$endif}

{$ifdef CD_Android}
class procedure TCDWSLazDeviceAPIs.RequestPositionInfo(
  AMethod: TLazPositionMethod);
var
  lPositionMethod: jint;
begin
  // Prepare the input
  case AMethod of
    pmGPS: lPositionMethod := 1;
    pmNetwork: lPositionMethod := 2;
  else
    Exit;
  end;
  javaEnvRef^^.SetIntField(javaEnvRef, javaActivityObject, JavaField_lclkind, lPositionMethod);
  // Call the method
  javaEnvRef^^.CallVoidMethod(javaEnvRef, javaActivityObject, javaMethod_LCLDoRequestPositionInfo);
end;

class procedure TCDWSLazDeviceAPIs.SendMessage(AMsg: TLazDeviceMessage);
var
  lJavaString: jstring;
  lStr: String;
begin
  // Prepare the input
  // String fields
  lStr := AMsg.Body;
  lJavaString :=javaEnvRef^^.NewStringUTF(javaEnvRef, PChar(lStr));
  javaEnvRef^^.SetObjectField(javaEnvRef, javaActivityObject, JavaField_lcltext, lJavaString);
  javaEnvRef^^.DeleteLocalRef(javaEnvRef, lJavaString);
  //
  lStr := AMsg.destinationAddress.Text;
  lJavaString :=javaEnvRef^^.NewStringUTF(javaEnvRef, PChar(lStr));
  javaEnvRef^^.SetObjectField(javaEnvRef, javaActivityObject, JavaField_lcldestination, lJavaString);
  javaEnvRef^^.DeleteLocalRef(javaEnvRef, lJavaString);
  // Message type
  javaEnvRef^^.SetIntField(javaEnvRef, javaActivityObject, JavaField_lclkind, 1);
  // Call the method
  javaEnvRef^^.CallVoidMethod(javaEnvRef, javaActivityObject, javaMethod_LCLDoSendMessage);
end;

class procedure TCDWSLazDeviceAPIs.StartReadingAccelerometerData;
begin
  // Call the method
  javaEnvRef^^.CallVoidMethod(javaEnvRef, javaActivityObject, javaMethod_LCLDoStartReadingAccelerometer);
end;

class procedure TCDWSLazDeviceAPIs.StopReadingAccelerometerData;
begin
  // Call the method
  javaEnvRef^^.CallVoidMethod(javaEnvRef, javaActivityObject, javaMethod_LCLDoStopReadingAccelerometer);
end;

class function TCDWSLazDeviceAPIs.GetDeviceManufacturer: string;
var
  lFieldID: JFieldID;
  lJavaString: JString;
  lNativeString: PChar;
begin
  lFieldID := javaEnvRef^^.GetStaticFieldID(javaEnvRef, javaAndroidOSBuildClass, 'MANUFACTURER', 'Ljava/lang/String;');
  lJavaString := JString(javaEnvRef^^.GetStaticObjectField(javaEnvRef, javaAndroidOSBuildClass, lFieldID));
  lNativeString := javaEnvRef^^.GetStringUTFChars(javaEnvRef, lJavaString, nil);
  Result := lNativeString;
  javaEnvRef^^.ReleaseStringUTFChars(javaEnvRef, lJavaString, lNativeString);
end;

class function TCDWSLazDeviceAPIs.GetDeviceModel: string;
var
  lFieldID: JFieldID;
  lJavaString: JString;
  lNativeString: PChar;
begin
  lFieldID := javaEnvRef^^.GetStaticFieldID(javaEnvRef, javaAndroidOSBuildClass, 'MODEL', 'Ljava/lang/String;');
  lJavaString := JString(javaEnvRef^^.GetStaticObjectField(javaEnvRef, javaAndroidOSBuildClass, lFieldID));
  lNativeString := javaEnvRef^^.GetStringUTFChars(javaEnvRef, lJavaString, nil);
  Result := lNativeString;
  javaEnvRef^^.ReleaseStringUTFChars(javaEnvRef, lJavaString, lNativeString);
end;

class function TCDWSLazDeviceAPIs.GetScreenRotation(AScreenIndex: Integer
  ): TScreenRotation;
var
  windowManagerClass, displayClass : jclass;
  javaMethodId_WindowService, javaMethodId_getDefaultDisplay, javaMethodId_getRotation : JMethodID;
  windowManagerObject, displayObject: JObject;
  javaString_WINDOW_SERVICE, javaString_getDefaultDisplay : JString;
  lRotation: Jint;
const
  ROTATION_0 = 0;
  ROTATION_180 = 2;
  ROTATION_270 = 3;
  ROTATION_90 = 1;
begin
  windowManagerClass := javaEnvRef^^.FindClass(javaEnvRef, 'android/view/WindowManager');
  displayClass := javaEnvRef^^.FindClass(javaEnvRef, 'android/view/Display');

  // get the string Context.WINDOW_SERVICE remember that NewStringUTF does not require ReleaseStringUTFChars
  javaString_WINDOW_SERVICE := javaEnvRef^^.NewStringUTF(javaEnvRef, pchar('getWindowManager'));

  // get method id for WindowManager object
  javaMethodId_WindowService := javaEnvRef^^.GetMethodID(javaEnvRef, javaAndroidAppActivityClass, 'getWindowManager', '()Landroid/view/WindowManager;');

  // Get the WindowManager object
  // Window w = (Window) getSystemService(Context.WINDOW_SERVICE);
  windowManagerObject := javaEnvRef^^.CallObjectMethod(javaEnvRef, javaActivityObject, javaMethodId_WindowService);

  javaString_getDefaultDisplay := javaEnvRef^^.NewStringUTF(javaEnvRef, pchar('getDefaultDisplay'));

  // Get method id for Display object
  javaMethodId_getDefaultDisplay := javaEnvRef^^.GetMethodID(javaEnvRef, windowManagerClass, 'getDefaultDisplay', '()Landroid/view/Display;');

  // Get the display object
  displayObject := javaEnvRef^^.CallObjectMethod(javaEnvRef, windowManagerObject, javaMethodId_getDefaultDisplay);

  // Get method id for method: getRotation
  javaMethodId_getRotation := javaEnvRef^^.GetMethodID(javaEnvRef, displayClass, 'getRotation', '()I');

  // Now call getRotation method in the display object
  lRotation := javaEnvRef^^.CallIntMethod(javaEnvRef, displayObject, javaMethodId_getRotation);
  case lRotation of
  ROTATION_180: Result := srRotation_180;
  ROTATION_270: Result := srRotation_270;
  ROTATION_90:  Result := srRotation_90;
  else
    Result := srRotation_0;
  end;
end;

class procedure TCDWSLazDeviceAPIs.Vibrate(ADurationMS: Cardinal);
var
  lVibratorObject: JObject;
  javaMethod_vibrate: JMethodID;
  javaString_VIBRATOR_SERVICE: JString;
  // array for the parameters
  lParams: array[0..0] of JValue;
const
  javaConstant_VIBRATOR_SERVICE = 'vibrator';
begin
  // First IDs
  javaMethod_vibrate := javaEnvRef^^.GetMethodID(javaEnvRef, javaAndroidOSVibratorClass, 'vibrate', '(J)V');

  // get the string Context.VIBRATOR_SERVICE remember that NewStringUTF does not require ReleaseStringUTFChars
  javaString_VIBRATOR_SERVICE := javaEnvRef^^.NewStringUTF(javaEnvRef, pchar(javaConstant_VIBRATOR_SERVICE));

  // Get the vibrator object
  // Vibrator v = (Vibrator) getSystemService(Context.VIBRATOR_SERVICE);
  lParams[0].l := javaString_VIBRATOR_SERVICE;
  lVibratorObject := javaEnvRef^^.CallObjectMethodA(javaEnvRef, javaActivityObject, javaMethod_getSystemService, @lParams[0]);

  // Now call the method from the vibrator object
  lParams[0].j := ADurationMS;
  javaEnvRef^^.CallVoidMethodA(javaEnvRef, lVibratorObject, javaMethod_Vibrate, @lParams[0]);
end;
{$endif}

end.
