{%MainUnit customdrawnint.pp}

function TCDWidgetSet.ClientToScreen(Handle: HWND; var P: TPoint) : Boolean;
begin
  Result := False;
end;

function TCDWidgetSet.ClipboardGetData(ClipboardType: TClipboardType;
  FormatID: TClipboardFormat; Stream: TStream): boolean;
var
  // Java IDs
  javaAndroidTextClipboardManagerClass: JClass;
  javaJavaLangCharSequenceClass: JClass;
  javaMethod_ClipboardManager_getText: jmethodid = nil;
  javaMethod_ClipboardManager_hasText: jmethodid = nil;
  javaMethod_CharSequence_toString: jmethodid = nil;
  // Java Object instances
  lJavaString: jstring;
  lClipboardManagerObject: JObject;
  lJavaCharSequence: JObject;
  javaString_CLIPBOARD_SERVICE: JString;
  //
  lNativeString: PChar;
  lChar: Char;
  i: Integer;
  // array for the parameters
  lParams: array[0..0] of JValue;
begin
  {$ifdef VerboseCDClipboard}
  DebugLn(Format('[TCDWidgetSet.ClipboardGetData] FormatId=%d', [FormatId]));
  {$endif}
  Result := False;

  if (javaEnvRef = nil) then Exit;
  if Stream = nil then Exit;

  // First IDs
  javaAndroidTextClipboardManagerClass := javaEnvRef^^.FindClass(javaEnvRef,'android/text/ClipboardManager');
  javaJavaLangCharSequenceClass := javaEnvRef^^.FindClass(javaEnvRef,'java/lang/CharSequence');

  javaMethod_ClipboardManager_getText := javaEnvRef^^.GetMethodID(javaEnvRef, javaAndroidTextClipboardManagerClass, 'getText', '()Ljava/lang/CharSequence;');
  javaMethod_ClipboardManager_hasText := javaEnvRef^^.GetMethodID(javaEnvRef, javaAndroidTextClipboardManagerClass, 'hasText', '()Z');
  javaMethod_CharSequence_toString := javaEnvRef^^.GetMethodID(javaEnvRef, javaJavaLangCharSequenceClass, 'toString', '()Ljava/lang/String;');

  // Gets a handle to the Clipboard Manager
  //ClipboardManager clipboard = (ClipboardManager) getSystemService(Context.CLIPBOARD_SERVICE);
  javaString_CLIPBOARD_SERVICE := javaEnvRef^^.NewStringUTF(javaEnvRef, pchar(javaConstant_CLIPBOARD_SERVICE));
  lParams[0].l := javaString_CLIPBOARD_SERVICE;
  lClipboardManagerObject := javaEnvRef^^.CallObjectMethodA(javaEnvRef, javaActivityObject, javaMethod_getSystemService, @lParams[0]);

  // clipboard.hasText
  Result := javaEnvRef^^.CallBooleanMethod(javaEnvRef, lClipboardManagerObject, javaMethod_ClipboardManager_hasText) <> 0;
  if not Result then
  begin
    {$ifdef VerboseCDClipboard}
    DebugLn(':<[TCDWidgetSet.ClipboardGetData] Result=False');
    {$endif}
    Exit;
  end;

  // lCharSequence = clipboard.getText();
  lJavaCharSequence := javaEnvRef^^.CallObjectMethod(javaEnvRef, lClipboardManagerObject, javaMethod_ClipboardManager_getText);

  // lString = lCharSequence.toString();
  lJavaString := javaEnvRef^^.CallObjectMethod(javaEnvRef, lJavaCharSequence, javaMethod_CharSequence_toString);
  lNativeString := javaEnvRef^^.GetStringUTFChars(javaEnvRef, lJavaString, nil);
  lChar := #1;
  i := 0;
  while lChar <> #0 do
  begin
    lChar := lNativeString[i];
    Stream.WriteByte(Byte(lChar));
    {$ifdef VerboseCDClipboard}
    DebugLn(Format(':[TCDWidgetSet.ClipboardGetData] Writing char %d="%s"', [Byte(lChar), string(lChar)]));
    {$endif}
    Inc(i);
  end;
  javaEnvRef^^.ReleaseStringUTFChars(javaEnvRef, lJavaString, lNativeString);
  {$ifdef VerboseCDClipboard}
  DebugLn(Format(':<[TCDWidgetSet.ClipboardGetData] lNativeString=%s lJavaString=%x', [StrPas(lNativeString), PtrInt(lJavaString)]));
  {$endif}
end;

function TCDWidgetSet.ClipboardGetOwnerShip(ClipboardType: TClipboardType;
  OnRequestProc: TClipboardRequestEvent; FormatCount: integer;
  Formats: PClipboardFormat): boolean;
var
  // Java IDs
  javaAndroidTextClipboardManagerClass: JClass;
  javaMethod_ClipboardManager_setText: jmethodid = nil;
  // Java Object instances
  lJavaString: jstring;
  lClipboardManagerObject: JObject;
  javaString_CLIPBOARD_SERVICE: JString;
  // array for the parameters
  lParams: array[0..0] of JValue;
  //
  lStringStream: TStringStream;
  lTextFormatIndex: TClipboardFormat;
  lTextFormatFound: Boolean = False;
  i: Integer;
begin
  {$ifdef VerboseCDClipboard}
  DebugLn(Format('[TCDWidgetSet.ClipboardGetOwnerShip] OnRequestProc=%x', [PtrInt(OnRequestProc)]));
  {$endif}
  Result := True; // Result always true, so that we can at least use internally the clipboard

  if (javaEnvRef = nil) then Exit;
  if OnRequestProc = nil then Exit;

  // Check if there is text in the list. If there isn't, give up
  lTextFormatIndex := Self.ClipboardRegisterFormat('text/plain');
  for i := 0 to FormatCount-1 do
  begin
    if Formats[i] = lTextFormatIndex then
    begin
      lTextFormatFound := True;
      Break;
    end;
  end;
  // With API level 8 we can only have text support
  if not lTextFormatFound then
  begin
    Exit;

  end;

  // First IDs
  javaAndroidTextClipboardManagerClass := javaEnvRef^^.FindClass(javaEnvRef,'android/text/ClipboardManager');
  javaMethod_ClipboardManager_setText := javaEnvRef^^.GetMethodID(javaEnvRef, javaAndroidTextClipboardManagerClass, 'setText', '(Ljava/lang/CharSequence;)V');

  // Gets a handle to the Clipboard Manager
  //ClipboardManager clipboard = (ClipboardManager) getSystemService(Context.CLIPBOARD_SERVICE);
  javaString_CLIPBOARD_SERVICE := javaEnvRef^^.NewStringUTF(javaEnvRef, pchar(javaConstant_CLIPBOARD_SERVICE));
  lParams[0].l := javaString_CLIPBOARD_SERVICE;
  lClipboardManagerObject := javaEnvRef^^.CallObjectMethodA(javaEnvRef, javaActivityObject, javaMethod_getSystemService, @lParams[0]);

  // clipboard.setText
  lStringStream := TStringStream.Create('');
  try
    OnRequestProc(lTextFormatIndex, lStringStream);
    lJavaString := javaEnvRef^^.NewStringUTF(javaEnvRef, PChar(lStringStream.DataString));
    lParams[0].l := lJavaString;
    javaEnvRef^^.CallVoidMethodA(javaEnvRef, lClipboardManagerObject, javaMethod_ClipboardManager_setText, @lParams[0]);
  finally
    lStringStream.Free;
  end;
end;

{$ifdef CD_UseNativeText}
{------------------------------------------------------------------------------
  Function: ExtTextOut
  Params:  none
  Returns: Nothing
 ------------------------------------------------------------------------------}
function TCDWidgetSet.ExtTextOut(DC: HDC; X, Y: Integer; Options: Longint;
  Rect: PRect; Str: PChar; Count: Longint; Dx: PInteger): Boolean;
var
  lJavaString: jstring;
  lJavaBitmap: jobject;
  pixels: PCardinal;
  lImage: TLazIntfImage = nil;
  lCanvas: TLazCanvas = nil;
  lWidth, lHeight: jint;
  lDestCanvas: TLazCanvas;
  lFontSize: Integer;
  // array for the parameters
  lParams: array[0..0] of JValue;
begin
  {$ifdef VerboseCDText}
    DebugLn(Format(':>[WinAPI ExtTextOut] DC=%x javaEnvRef=%x Str=%s X=%d Y=%d',
      [DC, PtrInt(javaEnvRef), StrPas(Str), X, Y]));
  {$endif}

  Result := False;

  if (Str = nil) or (Str = '') then Exit;

  if ((Options and (ETO_OPAQUE + ETO_CLIPPED)) <> 0) and (Rect = nil) then
    exit;

  if not IsValidDC(DC) then Exit;
  lDestCanvas := TLazCanvas(DC);

  if (lDestCanvas.Font = nil) or (lDestCanvas.Font.Size = 0) then lFontSize := DefaultFontAndroidSize
  else lFontSize := Abs(lDestCanvas.Font.Size);

  if (javaEnvRef = nil) then Exit;

  // Prepare the input
  lJavaString :=javaEnvRef^^.NewStringUTF(javaEnvRef, Str);
  javaEnvRef^^.SetObjectField(javaEnvRef, javaActivityObject, JavaField_lcltext, lJavaString);
  javaEnvRef^^.DeleteLocalRef(javaEnvRef, lJavaString);
  javaEnvRef^^.SetIntField(javaEnvRef, javaActivityObject, javaField_lcltextsize, lFontSize);

  // Call the method to measure the text
  javaEnvRef^^.CallVoidMethod(javaEnvRef, javaActivityObject, javaMethod_LCLDoGetTextBounds);

  // Call the method to draw the text
  lParams[0].i := FPColorToAndroidColor(lDestCanvas.Font.FPColor);
  javaEnvRef^^.CallVoidMethodA(javaEnvRef, javaActivityObject, javaMethod_LCLDoDrawText, @lParams[0]);

  // Get the bitmap with the text
  lJavaBitmap := javaEnvRef^^.GetObjectField(javaEnvRef, javaActivityObject, javaField_lclbitmap);
  lWidth := javaEnvRef^^.GetIntField(javaEnvRef, javaActivityObject, javaField_lclwidth);
  lHeight := javaEnvRef^^.GetIntField(javaEnvRef, javaActivityObject, javaField_lclheight);

  {$ifdef VerboseCDText}
    DebugLn(Format(':[WinAPI ExtTextOut] lWidth=%d lHeight=%d DestCanvasSize=%d, %d lFontSize=%d',
      [lWidth, lHeight, lDestCanvas.Width, lDestCanvas.Height, lFontSize]));
  {$endif}

  // ---------------------------
  // Now copy it pixel per pixel
  // ---------------------------

  // Lock the bitmap
  AndroidBitmap_lockPixels(javaEnvRef, lJavaBitmap, @pixels);

  // Prepare the non-native image and canvas
  UpdateControlLazImageAndCanvas(lImage, lCanvas, lWidth, lHeight, clfRGBA32, pixels, True, False, False);

  lDestCanvas.AlphaBlendIgnoringDestPixels(lCanvas, X, Y, 0, 0, lWidth, lHeight);

  // Release the helper objects
  lCanvas.Free;
  lImage.Free;
  // Release the bitmap lock
  AndroidBitmap_unlockPixels(javaEnvRef, lJavaBitmap);

  {$ifdef VerboseCDText}
    DebugLn(':<[WinAPI ExtTextOut]');
  {$endif}

  Result := True;

end;
{$endif}

function TCDWidgetSet.GetKeyState(nVirtKey: Integer): Smallint;
begin
  Result := 0;
end;

function TCDWidgetSet.GetSystemMetrics(nIndex: Integer): Integer;
var
  R: TRect;
begin
  {$ifdef VerboseCDWinAPI}
    DebugLn(Format(':>[TCDWidgetSet.GetSystemMetrics] nIndex=%d javaEnvRef=%x', [nIndex, PtrInt(javaEnvRef)]));
  {$endif}
  Result := 0;
  case nIndex of
    SM_ARRANGE:
      begin
        {$ifdef VerboseQtWinAPI}
          WriteLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_ARRANGE          ');
        {$endif}
      end;
    SM_CLEANBOOT:
      begin
        {$ifdef VerboseQtWinAPI}
          WriteLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CLEANBOOT          ');
        {$endif}
      end;
    SM_CMONITORS:
      Result := 1;
    SM_CMOUSEBUTTONS:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CMOUSEBUTTONS    ');
      end;
{    SM_CXBORDER, SM_CYBORDER:
      begin
        // size of frame around controls
        Result := QStyle_pixelMetric(QApplication_style(),
                    QStylePM_DefaultFrameWidth, nil, nil);
      end;}
    SM_CXCURSOR:
      begin
        Result := 32; // recomended in docs
      end;
    SM_CYCURSOR:
      begin
        Result := 32; // recomended in docs
      end;
    SM_CXDOUBLECLK:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CXDOUBLECLK      ');
      end;
    SM_CYDOUBLECLK:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CYDOUBLECLK      ');
      end;
    SM_CXDRAG:
      begin
        Result := 2;
      end;
    SM_CYDRAG:
      begin
        Result := 2;
      end;
    SM_CXEDGE:
      begin
        Result := 2;
      end;
    SM_CYEDGE:
      begin
        Result := 2;
      end;
    SM_CXFIXEDFRAME:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CXFIXEDFRAME     ');
      end;
    SM_CYFIXEDFRAME:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CYFIXEDFRAME     ');
      end;
    SM_CXFULLSCREEN:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CXFULLSCREEN     ');
      end;
    SM_CYFULLSCREEN:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CYFULLSCREEN     ');
      end;
    SM_CXHTHUMB:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CXHTHUMB         ');
      end;
    SM_CXICON,
    SM_CYICON:
      begin
        Result := 32;
      end;
    SM_CXICONSPACING:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CXICONSPACING    ');
      end;
    SM_CYICONSPACING:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CYICONSPACING    ');
      end;
    SM_CXMAXIMIZED:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CXMAXIMIZED      ');
      end;
    SM_CYMAXIMIZED:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CYMAXIMIZED      ');
      end;
    SM_CXMAXTRACK:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CXMAXTRACK       ');
      end;
    SM_CYMAXTRACK:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CYMAXTRACK       ');
      end;
    SM_CXMENUCHECK:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CXMENUCHECK      ');
      end;
    SM_CYMENUCHECK:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CYMENUCHECK      ');
      end;
{    SM_CXMENUSIZE:
      begin
        Result := QStyle_pixelMetric(QApplication_style(), QStylePM_IndicatorWidth, nil, nil);
      end;
    SM_CYMENUSIZE:
      begin
        Result := QStyle_pixelMetric(QApplication_style(), QStylePM_IndicatorHeight, nil, nil);
      end;}
    SM_CXMIN:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CXMIN            ');
      end;
    SM_CYMIN:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CYMIN            ');
      end;
    SM_CXMINIMIZED:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CXMINIMIZED      ');
      end;
    SM_CYMINIMIZED:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CYMINIMIZED      ');
      end;
    SM_CXMINSPACING:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CXMINSPACING     ');
      end;
    SM_CYMINSPACING:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CYMINSPACING     ');
      end;
    SM_CXMINTRACK:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CXMINTRACK       ');
      end;
    SM_CYMINTRACK:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CYMINTRACK       ');
      end;
    SM_CXSCREEN:
    begin
      Result := 100; // avoid errors if this is called too early
      if javaEnvRef = nil then Exit;
      Result := javaEnvRef^^.GetIntField(javaEnvRef, javaActivityObject, javaField_lclscreenwidth);
      if Result = 0 then Result := 100;
    end;
    SM_CYSCREEN:
    begin
      Result := 100; // avoid errors if this is called too early
      if javaEnvRef = nil then Exit;
      Result := javaEnvRef^^.GetIntField(javaEnvRef, javaActivityObject, javaField_lclscreenheight);
      if Result = 0 then Result := 100;
    end;
    SM_CXSIZE:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CXSIZE           ');
      end;
    SM_CYSIZE:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CYSIZE           ');
      end;
{    SM_CXSIZEFRAME,
    SM_CYSIZEFRAME:
      begin
        Result := QStyle_pixelMetric(QApplication_style(), QStylePM_MDIFrameWidth, nil, nil);
      end;}
    SM_CXSMICON,
    SM_CYSMICON:
      begin
        Result := 16
      end;
    SM_CXSMSIZE:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CXSMSIZE         ');
      end;
    SM_CYSMSIZE:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CYSMSIZE         ');
      end;
    SM_CXVIRTUALSCREEN:
    begin
      if javaEnvRef = nil then Exit;
      Result := javaEnvRef^^.GetIntField(javaEnvRef, javaActivityObject, javaField_lclscreenwidth);
    end;
    SM_CYVIRTUALSCREEN:
    begin
      if javaEnvRef = nil then Exit;
      Result := javaEnvRef^^.GetIntField(javaEnvRef, javaActivityObject, javaField_lclscreenheight);
    end;
    SM_CXVSCROLL,
    SM_CYVSCROLL,
    SM_CXHSCROLL,
{    SM_CYHSCROLL:
    begin
      Result := QStyle_pixelMetric(QApplication_Style, QStylePM_ScrollBarExtent, nil, nil);
    end;
    SM_CYCAPTION:
    begin
      Result := QStyle_pixelMetric(QApplication_Style, QStylePM_TitleBarHeight, nil, nil);
    end;}
    SM_CYKANJIWINDOW:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CYKANJIWINDOW    ');
      end;
    SM_CYMENU:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CYMENU           ');
      end;
    SM_CYSMCAPTION:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CYSMCAPTION      ');
      end;
    SM_CYVTHUMB:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CYVTHUMB         ');
      end;
    SM_DBCSENABLED:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_DBCSENABLED      ');
      end;
    SM_DEBUG:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_DEBUG            ');
      end;
    SM_MENUDROPALIGNMENT:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_MENUDROPALIGNMENT');
      end;
    SM_MIDEASTENABLED:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_MIDEASTENABLED   ');
      end;
    SM_MOUSEPRESENT:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_MOUSEPRESENT     ');
      end;
    SM_MOUSEWHEELPRESENT:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_MOUSEWHEELPRESENT');
      end;
    SM_NETWORK:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_NETWORK          ');
      end;
    SM_PENWINDOWS:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_PENWINDOWS       ');
      end;
    SM_SECURE:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_SECURE           ');
      end;
    SM_SHOWSOUNDS:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_SHOWSOUNDS       ');
      end;
    SM_SLOWMACHINE:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_SLOWMACHINE      ');
      end;
    SM_SWAPBUTTON:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_SWAPBUTTON       ');
      end;
  end;
  {$ifdef VerboseCDWinAPI}
    DebugLn(':<[TCDWidgetSet.GetSystemMetrics] Result=' + dbghex(Result));
  {$endif}
end;

{$ifdef CD_UseNativeText}
{------------------------------------------------------------------------------
  Function: GetTextExtentExPoint
  Params: http://msdn.microsoft.com/en-us/library/dd144935%28VS.85%29.aspx
  Returns: True on success
 ------------------------------------------------------------------------------}
function TCDWidgetSet.GetTextExtentExPoint(DC: HDC; Str: PChar; Count,
  MaxWidth: Integer; MaxCount, PartialWidths: PInteger; var Size: Types.TSize
  ): Boolean;
var
  LazDC: TLazCanvas absolute DC;
  lTextStr: string;
  lMaxCount: Integer;
  arraydata_obj: JFloatArray;
  arraydata: PSingle;
  i: Integer;
  lFontSize: Integer;
  lJavaString: jstring;
  lIsCopy: jboolean;
begin
  {$ifdef VerboseCDText}
    DebugLn(Format('[WinAPI GetTextExtentExPoint] DC=%x javaEnvRef=%x Str=%s MaxWidth=%d',
      [DC, PtrInt(javaEnvRef), StrPas(Str), MaxWidth]));
  {$endif}
//  Result := inherited GetTextExtentExPoint(DC, Str, Count, MaxWidth, MaxCount, PartialWidths, Size);

  Result := False;

  if not IsValidDC(DC) then Exit;

  lTextStr := StrPas(Str);
  if Count <> Length(lTextStr) then SetLength(lTextStr, Count);

  if (LazDC.Font = nil) or (LazDC.Font.Size = 0) then lFontSize := DefaultFontAndroidSize
  else lFontSize := LazDC.Font.Size;

  if (javaEnvRef = nil) then Exit;

  // Prepare the input
  lJavaString :=javaEnvRef^^.NewStringUTF(javaEnvRef, PChar(lTextStr));
  javaEnvRef^^.SetObjectField(javaEnvRef, javaActivityObject, JavaField_lcltext, lJavaString);
  javaEnvRef^^.DeleteLocalRef(javaEnvRef, lJavaString);
  javaEnvRef^^.SetIntField(javaEnvRef, javaActivityObject, javaField_lcltextsize, lFontSize);
  javaEnvRef^^.SetIntField(javaEnvRef, javaActivityObject, javaField_lclmaxwidth, MaxWidth);

  // Call the method
  javaEnvRef^^.CallVoidMethod(javaEnvRef, javaActivityObject, javaMethod_LCLDoGetTextPartialWidths);

  // Read the output
  lMaxCount := javaEnvRef^^.GetIntField(javaEnvRef, javaActivityObject, javaField_lclmaxcount);
  {$ifdef VerboseCDText}
  DebugLn(Format(':[WinAPI GetTextExtentExPoint] MaxCount=%d', [lMaxCount]));
  {$endif}

  if MaxCount <> nil then MaxCount^ := lMaxCount;

  if PartialWidths <> nil then
  begin
    lIsCopy := 0;
    // Get the object field, returns JObject (because Array is instance of Object)
    arraydata_obj := javaEnvRef^^.GetObjectField(javaEnvRef, javaActivityObject, javaField_lclpartialwidths);
    // Get the elements (you probably have to fetch the length of the array as well
    arraydata := javaEnvRef^^.GetFloatArrayElements(javaEnvRef, arraydata_obj, lIsCopy);

    for i := 0 to lMaxCount-1 do
    begin
      PartialWidths[i] := Round(arraydata[i]);
      {$ifdef VerboseCDText}
      DebugLn(Format(':[WinAPI GetTextExtentExPoint] i=%d PartialWidth=%d',
        [i, PartialWidths[i]]));
      {$endif}
    end;

    // Don't forget to release it
    javaEnvRef^^.ReleaseFloatArrayElements(javaEnvRef, arraydata_obj, arraydata, 0);
  end;

  // Now calculate the general size
  GetTextExtentPoint(DC, PChar(lTextStr), lMaxCount, Size);
end;

{------------------------------------------------------------------------------
  Function: GetTextExtentPoint
  Params:  none
  Returns: Nothing
 ------------------------------------------------------------------------------}
function TCDWidgetSet.GetTextExtentPoint(DC: HDC; Str: PChar; Count: Integer;
  var Size: Types.TSize): Boolean;
var
  lJavaString: jstring;
  LazDC: TLazCanvas;
  lFontSize: Integer;
begin
  {$ifdef VerboseCDText}
    DebugLn(Format('[WinAPI GetTextExtentPoint] DC=%x javaEnvRef=%x', [DC, PtrInt(javaEnvRef)]));
  {$endif}

  Result := False;

  if not IsValidDC(DC) then Exit;
  LazDC := TLazCanvas(DC);

  if (LazDC.Font = nil) or (LazDC.Font.Size = 0) then lFontSize := DefaultFontAndroidSize
  else lFontSize := LazDC.Font.Size;

  if (javaEnvRef = nil) then Exit;

  // Prepare the input
  lJavaString :=javaEnvRef^^.NewStringUTF(javaEnvRef, Str);
  javaEnvRef^^.SetObjectField(javaEnvRef, javaActivityObject, JavaField_lcltext, lJavaString);
  javaEnvRef^^.DeleteLocalRef(javaEnvRef, lJavaString);
  javaEnvRef^^.SetIntField(javaEnvRef, javaActivityObject, javaField_lcltextsize, lFontSize);

  // Call the method
  javaEnvRef^^.CallVoidMethod(javaEnvRef, javaActivityObject, javaMethod_LCLDoGetTextBounds);

  // Read the output
  Size.cx := javaEnvRef^^.GetIntField(javaEnvRef, javaActivityObject, javaField_lclwidth);
  Size.cy := javaEnvRef^^.GetIntField(javaEnvRef, javaActivityObject, javaField_lclheight);

  {$ifdef VerboseCDText}
    DebugLn(Format('[WinAPI GetTextExtentPoint] Size=%d, %d', [Size.cx, Size.cy]));
  {$endif}

  Result := True;
end;

{------------------------------------------------------------------------------
  Function: GetTextMetrics
  Params:  DC     - A device context with a font selected
           TM     - The structure to receive the font information
  Returns: If successfull
 ------------------------------------------------------------------------------}
function TCDWidgetSet.GetTextMetrics(DC: HDC; var TM: TTextMetric): Boolean;
var
  lAverageCharWidth: Integer;
  lJavaString: jstring;
  LazDC: TLazCanvas;
  lFontSize: Integer;
begin
  {$ifdef VerboseCDText}
    DebugLn(Format('[WinAPI GetTextMetrics] DC=%x javaEnvRef=%x', [DC, PtrInt(javaEnvRef)]));
  {$endif}

  Result := False;

  if not IsValidDC(DC) then Exit;
  LazDC := TLazCanvas(DC);

  if (LazDC.Font = nil) or (LazDC.Font.Size = 0) then lFontSize := DefaultFontAndroidSize
  else lFontSize := LazDC.Font.Size;

  if (javaEnvRef = nil) then Exit;

  // Prepare the input for getting the average width of a char
  lJavaString :=javaEnvRef^^.NewStringUTF(javaEnvRef, PChar('x'));
  javaEnvRef^^.SetObjectField(javaEnvRef, javaActivityObject, JavaField_lcltext, lJavaString);
  javaEnvRef^^.DeleteLocalRef(javaEnvRef, lJavaString);
  javaEnvRef^^.SetIntField(javaEnvRef, javaActivityObject, javaField_lcltextsize, lFontSize);

  // Call the method
  javaEnvRef^^.CallVoidMethod(javaEnvRef, javaActivityObject, javaMethod_LCLDoGetTextBounds);

  lAverageCharWidth := javaEnvRef^^.GetIntField(javaEnvRef, javaActivityObject, javaField_lclwidth);

  // Prepare the input for getting the max height of a text and other metrics
  lJavaString :=javaEnvRef^^.NewStringUTF(javaEnvRef, PChar('Íg'));
  javaEnvRef^^.SetObjectField(javaEnvRef, javaActivityObject, JavaField_lcltext, lJavaString);
  javaEnvRef^^.DeleteLocalRef(javaEnvRef, lJavaString);
  javaEnvRef^^.SetIntField(javaEnvRef, javaActivityObject, javaField_lcltextsize, lFontSize);

  // Call the method
  javaEnvRef^^.CallVoidMethod(javaEnvRef, javaActivityObject, javaMethod_LCLDoGetTextBounds);

  // Read the output
  TM.tmHeight := javaEnvRef^^.GetIntField(javaEnvRef, javaActivityObject, javaField_lclheight);
  TM.tmAscent := javaEnvRef^^.GetIntField(javaEnvRef, javaActivityObject, javaField_lcltextascent);
  TM.tmDescent := javaEnvRef^^.GetIntField(javaEnvRef, javaActivityObject, javaField_lcltextdescent);
  TM.tmInternalLeading := 0;
  TM.tmExternalLeading := javaEnvRef^^.GetIntField(javaEnvRef, javaActivityObject, javaField_lcltextleading);
  TM.tmAveCharWidth := lAverageCharWidth;
  TM.tmMaxCharWidth := TM.tmAveCharWidth; // Just a not very good guess for now
{    FontWeight := QtDC.font.getWeight;
    case FontWeight of
      25: TM.tmWeight := FW_LIGHT;
      50: TM.tmWeight := FW_NORMAL;
      63: TM.tmWeight := FW_SEMIBOLD;
      75: TM.tmWeight := FW_BOLD;
      87: TM.tmWeight := FW_HEAVY;
      else
        TM.tmWeight := Round(FontWeight * 9.5);
    end;}
  TM.tmOverhang := 0;
  TM.tmDigitizedAspectX := 0;
  TM.tmDigitizedAspectY := 0;
  TM.tmFirstChar := 'a';
  TM.tmLastChar := 'z';
  TM.tmDefaultChar := 'x';
  TM.tmBreakChar := '?';
{  TM.tmItalic := Ord(QtDC.Font.getItalic);
  TM.tmUnderlined := Ord(QtDC.Font.getUnderline);
  TM.tmStruckOut := Ord(QtDC.Font.getStrikeOut);}

  { Defaults to a TrueType font.
    Note that the meaning of the FIXED_PITCH constant is the opposite of
    the name implies, according to MSDN docs. Just a small inconsistency
    on Windows API that we have to mimic. }
  {if QtDC.font.fixedPitch then
    TM.tmPitchAndFamily := TRUETYPE_FONTTYPE
  else}
    TM.tmPitchAndFamily := FIXED_PITCH or TRUETYPE_FONTTYPE;

  TM.tmCharSet := DEFAULT_CHARSET;

  Result := True;
end;
{$endif}

{------------------------------------------------------------------------------
  Function: GetWindowRelativePosition
  Params:  Handle : HWND;
  Returns: true on success

  returns the current widget Left, Top, relative to the client origin of its
  parent
 ------------------------------------------------------------------------------}
function TCDWidgetSet.BackendGetWindowRelativePosition(Handle: HWND; var Left, Top: integer): boolean;
begin
  {$ifdef VerboseCDWinAPI}
    DebugLn('[WinAPI BackendGetWindowRelativePosition]');
  {$endif}

  Left := 0;
  Top := 0;
  Result := True;
end;

{------------------------------------------------------------------------------
  Function: ScreenToClient
  Params:  Handle: HWND; var P: TPoint
  Returns:
-------------------------------------------------------------------------------}
function TCDWidgetSet.ScreenToClient(Handle : HWND; var P : TPoint) : Integer;
begin
  Result := 0;
end;

