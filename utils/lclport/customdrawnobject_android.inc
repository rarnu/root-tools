{%MainUnit customdrawnint.pas}

{******************************************************************************
                                   customdrawnobject_win.inc
 ******************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}

const
  javaConstant_CLIPBOARD_SERVICE = 'clipboard';

function Java_com_pascal_lclproject_LCLActivity_LCLOnTimer(
    env:PJNIEnv; this:jobject; ATimer: jobject; ATimerIDIndex: jint): jint; cdecl;
var
  lTimer: TCDTimer;
begin
  {$ifdef VerboseCDEvents}
  __android_log_write(ANDROID_LOG_INFO,'lclapp',PChar(
    Format('LCLOnTimer called ATimer=%x', [PtrInt(ATimer)])));
  {$endif}
  eventResult := 0;

  lTimer := FindTimerWithNativeHandle(PtrInt(ATimer));

  if lTimer <> nil then lTimer.TimerFunc();

  // This sends messages like Invalidate requests
  Result := eventResult;
end;

function Java_com_pascal_lclproject_LCLActivity_LCLOnSensorChanged(
  env:PJNIEnv; this:jobject; ASensorKind: jint; AValues: JDoubleArray): jint; cdecl;
var
  arraydata: PDouble;
  arraylen: jsize;
  lIsCopy: jboolean;
  lSensorDataInt: Integer;
  lMessagingStatus: TLazMessagingStatus;
  lUnixTimeStamp: Int64;
begin
  Result := 0;

  if (javaEnvRef = nil) then Exit;

  // Get the elements and length
  lIsCopy := 0;
  arraylen := javaEnvRef^^.GetArrayLength(javaEnvRef, AValues);
  arraydata := javaEnvRef^^.GetDoubleArrayElements(javaEnvRef, AValues, lIsCopy);

  // Send the data to the LCL
  case ASensorKind of
    -11: // Defined by ourselves for Messaging Status
    begin
      lSensorDataInt := Round(arraydata[0]);
      case lSensorDataInt of
        1: lMessagingStatus := mssSentSuccessfully;
        2: lMessagingStatus := mssSendingGeneralError;
        3: lMessagingStatus := mssNoService;
        5: lMessagingStatus := mssRadioOff;
        10:lMessagingStatus := mssReceivedSuccessfully;
        11:lMessagingStatus := mssReceivingGeneralError;
      else
        lMessagingStatus := mssSendingGeneralError;
      end;

      if Assigned(Messaging.OnMessagingStatus) then
        Messaging.OnMessagingStatus(nil, lMessagingStatus);
    end;
    -10: // Defined by ourselves for PositionInfo
    begin
      PositionInfo.latitude := arraydata[0];
      PositionInfo.longitude := arraydata[1];
      PositionInfo.altitude := arraydata[2];
      PositionInfo.accuracy := arraydata[3];
      PositionInfo.altitudeAccuracy := PositionInfo.accuracy;
      PositionInfo.speed := arraydata[4];
      lUnixTimeStamp := Round(arraydata[5]);
      PositionInfo.timeStamp :=  UnixToDateTime(lUnixTimeStamp);

      if Assigned(PositionInfo.OnPositionRetrieved) then
        PositionInfo.OnPositionRetrieved(PositionInfo);
    end;
    1: // ACCELEROMETER
    begin
      Accelerometer.xaxis := -1*arraydata[0];
      Accelerometer.yaxis := -1*arraydata[1];
      Accelerometer.zaxis := -1*arraydata[2];
      if Assigned(Accelerometer.OnSensorChanged) then
        Accelerometer.OnSensorChanged(Accelerometer);
    end;
  end;

  // Don't forget to release it
  javaEnvRef^^.ReleaseDoubleArrayElements(javaEnvRef, AValues, arraydata, 0);

  // This sends messages like Invalidate requests
  Result := eventResult;
end;

{$IFnDEF WithOldDebugln}
{$ELSE}
procedure TCDWidgetSet.AndroidDebugLn(AStr: string);
begin
  __android_log_write(ANDROID_LOG_INFO, 'lclapp', PChar(AccumulatedStr+AStr));
  AccumulatedStr := '';
end;
{$ENDIF}

function TCDWidgetSet.GetMimeTypeFromFileName(AFileName: string): string;
var
  lExt: String;
begin
  lExt := SysUtils.ExtractFileExt(AFileName);
  // First the most common formats
  if AnsiCompareText(lExt, '.png') = 0 then Result := 'image/png'
  else if AnsiCompareText(lExt, '.txt') = 0 then Result := 'text/plain'
  else if AnsiCompareText(lExt, '.jpg') = 0 then Result := 'image/jpeg'
  else if AnsiCompareText(lExt, '.jpeg') = 0 then Result := 'image/jpeg'
  else if AnsiCompareText(lExt, '.pdf') = 0 then Result := 'application/pdf'
  else if AnsiCompareText(lExt, '.xml') = 0 then Result := 'application/xml'
  else if AnsiCompareText(lExt, '.svg') = 0 then Result := 'image/svg+xml'
  else if AnsiCompareText(lExt, '.swf') = 0 then Result := 'application/x-shockwave-flash'
  else if AnsiCompareText(lExt, '.htm') = 0 then Result := 'text/html'
  else if AnsiCompareText(lExt, '.html') = 0 then Result := 'text/html'
  // Now all images
  else if AnsiCompareText(lExt, '.xpm') = 0 then Result := 'image/x-xpixmap'
  else if AnsiCompareText(lExt, '.gif') = 0 then Result := 'image/gif'
  else if AnsiCompareText(lExt, '.tiff') = 0 then Result := 'image/tiff'
  else if AnsiCompareText(lExt, '.tif') = 0 then Result := 'image/tiff'
  else if AnsiCompareText(lExt, '.ico') = 0 then Result := 'image/x-icon'
  else if AnsiCompareText(lExt, '.icns') = 0 then Result := 'image/icns'
  else if AnsiCompareText(lExt, '.ppm') = 0 then Result := 'image/x-portable-pixmap'
  else if AnsiCompareText(lExt, '.bmp') = 0 then Result := 'image/bmp'
  // Now all textual formats
  else if AnsiCompareText(lExt, '.pas') = 0 then Result := 'text/plain'
  else if AnsiCompareText(lExt, '.pp') = 0 then Result := 'text/plain'
  else if AnsiCompareText(lExt, '.inc') = 0 then Result := 'text/plain'
  else if AnsiCompareText(lExt, '.c') = 0 then Result := 'text/plain'
  else if AnsiCompareText(lExt, '.cpp') = 0 then Result := 'text/plain'
  else if AnsiCompareText(lExt, '.java') = 0 then Result := 'text/plain'
  else if AnsiCompareText(lExt, '.log') = 0 then Result := 'text/plain'
  // Now all videos
  else if AnsiCompareText(lExt, '.mp4') = 0 then Result := 'video/*'
  else if AnsiCompareText(lExt, '.avi') = 0 then Result := 'video/vnd.avi' // also possible video/x-msvideo
  else if AnsiCompareText(lExt, '.mpeg') = 0 then Result := 'video/MPEG'
  else if AnsiCompareText(lExt, '.mpg') = 0 then Result := 'video/MPEG'
  else if AnsiCompareText(lExt, '.mov') = 0 then Result := 'video/quicktime'
  // Now all sounds
  else if AnsiCompareText(lExt, '.mp3') = 0 then Result := 'audio/mpeg'
  else if AnsiCompareText(lExt, '.ogg') = 0 then Result := 'audio/ogg'
  else if AnsiCompareText(lExt, '.wav') = 0 then Result := 'audio/x-wav'
  else if AnsiCompareText(lExt, '.mid') = 0 then Result := 'audio/midi'
  else if AnsiCompareText(lExt, '.midi') = 0 then Result := 'audio/midi'
  else if AnsiCompareText(lExt, '.au') = 0 then Result := 'audio/basic'
  else if AnsiCompareText(lExt, '.snd') = 0 then Result := 'audio/basic'
  // Now all documents
  else if AnsiCompareText(lExt, '.rtf') = 0 then Result := 'text/rtf'
  else if AnsiCompareText(lExt, '.eps') = 0 then Result := 'application/Postscript'
  else if AnsiCompareText(lExt, '.ps') = 0 then Result := 'application/Postscript'
  //
  else if AnsiCompareText(lExt, '.xls') = 0 then Result := 'application/vnd.ms-excel'
  else if AnsiCompareText(lExt, '.doc') = 0 then Result := 'application/msword'
  else if AnsiCompareText(lExt, '.ppt') = 0 then Result := 'application/vnd.ms-powerpoint'
  //
  else if AnsiCompareText(lExt, '.odt') = 0 then Result := 'application/vnd.oasis.opendocument.text'
  else if AnsiCompareText(lExt, '.ods') = 0 then Result := 'application/vnd.oasis.opendocument.spreadsheet'
  else if AnsiCompareText(lExt, '.odp') = 0 then Result := 'application/vnd.oasis.opendocument.presentation'
  else if AnsiCompareText(lExt, '.odg') = 0 then Result := 'application/vnd.oasis.opendocument.graphics'
  else if AnsiCompareText(lExt, '.odc') = 0 then Result := 'application/vnd.oasis.opendocument.chart'
  else if AnsiCompareText(lExt, '.odf') = 0 then Result := 'application/vnd.oasis.opendocument.formula'
  else if AnsiCompareText(lExt, '.odi') = 0 then Result := 'application/vnd.oasis.opendocument.image'
  //
  else if AnsiCompareText(lExt, '.xlsx') = 0 then Result := 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet'
  else if AnsiCompareText(lExt, '.pptx') = 0 then Result := 'application/vnd.openxmlformats-officedocument.presentationml.presentation'
  else if AnsiCompareText(lExt, '.docx') = 0 then Result := 'application/vnd.openxmlformats-officedocument.wordprocessingml.document'
  // Now compressed archives
  else if AnsiCompareText(lExt, '.zip') = 0 then Result := 'application/zip'
  else if AnsiCompareText(lExt, '.tar') = 0 then Result := 'application/x-tar'
  // If we can't auto-detect just suppose it is text!
  else Result := 'text/plain';
end;

function TCDWidgetSet.GetAppHandle: THandle;
begin
  Result := 0;
end;

{------------------------------------------------------------------------------
  Method: TCDWidgetSet.Create
  Params:  None
  Returns: Nothing

  Constructor for the class.
 ------------------------------------------------------------------------------}
procedure TCDWidgetSet.BackendCreate;
begin
  // Setup DebugLn
  {$IFnDEF WithOldDebugln}

  {$ELSE}
  DebugLnProc := @AndroidDebugLn;
  DebugOutProc := @AccumulatingDebugOut;
  {$ENDIF}

  {$ifdef CD_UseNativeText}
  // Create the dummy screen DC
  ScreenBitmapRawImage.Init;
  ScreenBitmapHeight := 100;
  ScreenBitmapWidth := 100;
  ScreenBitmapRawImage.Description.Init_BPP32_A8R8G8B8_BIO_TTB(ScreenBitmapWidth, ScreenBitmapHeight);
  ScreenBitmapRawImage.CreateData(True);
  ScreenImage := TLazIntfImage.Create(0, 0);
  ScreenImage.SetRawImage(ScreenBitmapRawImage);
  ScreenDC := TLazCanvas.Create(ScreenImage);

  DefaultFontAndroidSize := 16;
  {$endif}
end;

{------------------------------------------------------------------------------
  Method: TWinCEWidgetSet.Destroy
  Params:  None
  Returns: Nothing

  destructor for the class.
 ------------------------------------------------------------------------------}
procedure TCDWidgetSet.BackendDestroy;
begin
  {$ifdef CD_UseNativeText}
  // Free the dummy screen DC
  ScreenImage.Free;
  ScreenDC.Free;
  {$endif}
end;

{------------------------------------------------------------------------------
  Method: TWinCEWidgetSet.AppInit
  Params:  None
  Returns: Nothing

  initialize Windows
 ------------------------------------------------------------------------------}
procedure TCDWidgetSet.AppRun(const ALoop: TApplicationMainLoop);
begin
  {$ifdef VerboseCDApplication}
  DebugLn('TCDWidgetSet.AppRun');
  {$endif}
end;

(*
function TWinCEWidgetSet.GetAppHandle: THandle;
begin
  Result:= FAppHandle;
end;

procedure TWinCEWidgetSet.SetAppHandle(const AValue: THandle);
begin
  // Do it only if handle is not yet created (for example for DLL initialization)
  // if handle is already created we can't reassign it
  if AppHandle = 0 then
    FAppHandle := AValue;
end;*)

{------------------------------------------------------------------------------
  Method: TWinCEWidgetSet.AppMinimize
  Params:  None
  Returns: Nothing

  Minimizes the whole application to the taskbar
 ------------------------------------------------------------------------------}
procedure TCDWidgetSet.AppMinimize;
begin
  // calling Activity.Free doesnt close the app, only hides it, so it is good for AppMinimize
   javaEnvRef^^.CallVoidMethod(javaEnvRef, javaActivityObject, javaMethod_Activity_finish);
end;

{------------------------------------------------------------------------------
  Method: TWinCEWidgetSet.AppRestore
  Params:  None
  Returns: Nothing

  Restore minimized whole application from taskbar
 ------------------------------------------------------------------------------}

procedure TCDWidgetSet.AppRestore;
begin
//  Windows.SendMessage(FAppHandle, WM_SYSCOMMAND, SC_RESTORE, 0);
end;


{------------------------------------------------------------------------------
  Method: TWinCEWidgetSet.AppBringToFront
  Params:  None
  Returns: Nothing

  Brings the entire application on top of all other non-topmost programs
 ------------------------------------------------------------------------------}
procedure TCDWidgetSet.AppBringToFront;
begin
end;

(*
procedure TWinCEWidgetSet.SetDesigning(AComponent: TComponent);
begin
  //if Data<>nil then EnableWindow((AComponent As TWinControl).Handle, boolean(Data^));
end;

{------------------------------------------------------------------------------
  Method: TWinCEWidgetSet.SetCallback
  Params: Msg    - message for which to set a callback
          Sender - object to which callback will be sent
  Returns:  nothing

  Applies a Message to the sender
 ------------------------------------------------------------------------------}
procedure TWinCEWidgetSet.SetCallback(Msg: LongInt; Sender: TObject);
var
  Window: HWnd;
begin
  //DebugLn('Trace:TWinCEWidgetSet.SetCallback - Start');
  //DebugLn(Format('Trace:TWinCEWidgetSet.SetCallback - Class Name --> %S', [Sender.ClassName]));
  //DebugLn(Format('Trace:TWinCEWidgetSet.SetCallback - Message Name --> %S', [GetMessageName(Msg)]));
  if Sender Is TControlCanvas then
    Window := TControlCanvas(Sender).Handle
  else if Sender Is TCustomForm then
    Window := TCustomForm(Sender).Handle
  else
    Window := TWinControl(Sender).Handle;
  if Window=0 then exit;

  //DebugLn('Trace:TWinCEWidgetSet.SetCallback - Exit');
end;

{------------------------------------------------------------------------------
  Method: TWinCEWidgetSet.RemoveCallbacks
  Params:   Sender - object from which to remove callbacks
  Returns:  nothing

  Removes Call Back Signals from the sender
 ------------------------------------------------------------------------------}
procedure TWinCEWidgetSet.RemoveCallbacks(Sender: TObject);
var
  Window: HWnd;
begin
  if Sender Is TControlCanvas then
    Window := TControlCanvas(Sender).Handle
  else if Sender Is TCustomForm then
    Window := TCustomForm(Sender).Handle
  else
    Window := (Sender as TWinControl).Handle;
  if Window=0 then exit;
end;*)

{------------------------------------------------------------------------------
  Method: TWinCEWidgetSet.AppProcessMessages
  Params:  None
  Returns: Nothing

  Handle all pending messages
 ------------------------------------------------------------------------------}
(*
procedure TWinCEWidgetSet.CheckPipeEvents;
var
  lHandler: PPipeEventInfo;
//  lBytesAvail: dword;
//  SomethingChanged: Boolean;
  ChangedCount:integer;
begin
  lHandler := FWaitPipeHandlers;
  ChangedCount := 0;
  while (lHandler <> nil) and (ChangedCount < 10) do
  begin
    {
    roozbeh : ooops not supported
    SomethingChanged:=true;
    if Windows.PeekNamedPipe(lHandler^.Handle, nil, 0, nil, @lBytesAvail, nil) then
    begin
      if lBytesAvail <> 0 then
        lHandler^.OnEvent(lHandler^.UserData, [prDataAvailable])
      else
        SomethingChanged := false;
    end else
      lHandler^.OnEvent(lHandler^.UserData, [prBroken]);
    if SomethingChanged then
      lHandler := FWaitPipeHandlers
    else begin
      lHandler := lHandler^.Next;
      ChangedCount := 0;
    end;
    inc(ChangedCount);}
  end;
end;*)

{------------------------------------------------------------------------------
  Method: TWinCEWidgetSet.AppWaitMessage
  Params:  None
  Returns: Nothing

  Passes execution control to Windows
 ------------------------------------------------------------------------------}
//roozbeh:new update...whole procedure body is added.what is it?

{------------------------------------------------------------------------------
  Method: TCDWidgetSet.AppTerminate
  Params:  None
  Returns: Nothing

  Terminates the application
 ------------------------------------------------------------------------------}

procedure TCDWidgetSet.AppSetIcon(const Small, Big: HICON);
begin
end;

procedure TCDWidgetSet.AppSetTitle(const ATitle: string);
begin
end;

procedure TCDWidgetSet.AppSetVisible(const AVisible: Boolean);
begin
end;

function TCDWidgetSet.AppRemoveStayOnTopFlags(const ASystemTopAlso: Boolean = False): Boolean;
begin
end;

function TCDWidgetSet.AppRestoreStayOnTopFlags(const ASystemTopAlso: Boolean = False): Boolean;
begin
end;

procedure TCDWidgetSet.AppSetMainFormOnTaskBar(const DoSet: Boolean);
begin
end;

{------------------------------------------------------------------------------
  function: CreateTimer
  Params: Interval:
          TimerFunc: Callback
  Returns: a Timer id (use this ID to destroy timer)

  Design: A timer which calls TimerCallBackProc, is created.
    The TimerCallBackProc calls the TimerFunc.
 ------------------------------------------------------------------------------}
function TCDWidgetSet.CreateTimer(Interval: integer; TimerFunc: TWSTimerProc) : THandle;
var
  lTimer: TCDTimer;
  lTimerObject, lGlobalTimerObject: JObject;
begin
  lTimer := TCDTimer.Create;

  Result := THandle(lTimer);

  lTimer.Interval := Interval;
  lTimer.TimerFunc := TimerFunc;

  // Prepare the input
  javaEnvRef^^.SetIntField(javaEnvRef, javaActivityObject, javaField_lcltimerinterval, Interval);

  // Call the method
  javaEnvRef^^.CallVoidMethod(javaEnvRef, javaActivityObject, javaMethod_LCLDoCreateTimer);

  // Read the output
  lTimerObject := javaEnvRef^^.GetObjectField(javaEnvRef, javaActivityObject, javaField_lcltimerid);
  lGlobalTimerObject := javaEnvRef^^.NewGlobalRef(javaEnvRef, lTimerObject);
  lTimer.NativeHandle := PtrInt(lTimerObject);
  lTimer.NativeGlobalReference := PtrInt(lGlobalTimerObject);

  // Add it to our list
  AddTimer(lTimer);

end;

{------------------------------------------------------------------------------
  function: DestroyTimer
  Params: TimerHandle
  Returns:
 ------------------------------------------------------------------------------}
function TCDWidgetSet.DestroyTimer(TimerHandle: THandle) : boolean;
var
  lTimer: TCDTimer;
begin
  lTimer := TCDTimer(TimerHandle);
  Result := False;

  if lTimer = nil then Exit;

  // Prepare the input
  javaEnvRef^^.SetObjectField(javaEnvRef, javaActivityObject, javaField_lcltimerid, jobject(lTimer.NativeGlobalReference));

  // Call the method
  javaEnvRef^^.CallVoidMethod(javaEnvRef, javaActivityObject, javaMethod_LCLDoDestroyTimer);
  javaEnvRef^^.DeleteGlobalRef(javaEnvRef, JObject(lTimer.NativeGlobalReference));

  // Remove from the list
  RemoveTimer(lTimer);
  lTimer.Free;

  Result := True;
end;
(*
procedure TWinCEWidgetSet.HandleWakeMainThread(Sender: TObject);
begin
  // wake up GUI thread by sending a message to it
  Windows.PostMessage(AppHandle, WM_NULL, 0, 0);
end;
*)

