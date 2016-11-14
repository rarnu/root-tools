{%MainUnit ../graphics.pp}

{******************************************************************************
                                     TCustomBitmap
 ******************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}

// ct9999 MOD for CodeTyphon Studio


function TCustomBitmap.BitmapHandleAllocated: boolean;
begin
  // for custombitmap handle = bitmaphandle
  Result := FSharedImage.FHandle <> 0;
end;

function TCustomBitmap.CanShareImage(AClass: TSharedRasterImageClass): Boolean;
begin
  Result := (AClass <> TSharedCustomBitmap)
         and inherited CanShareImage(AClass);
end;

procedure TCustomBitmap.Changed(Sender: TObject);
begin
  // When the bitmap is changed by the canvas, the rawimage data isn't valid anymore
  if Sender = FCanvas
  then TSharedCustomBitmap(FSharedImage).FImage.FreeData;
  inherited Changed(Sender);
end;

procedure TCustomBitmap.Clear;
begin
  FPixelFormat := pfDevice;
  inherited Clear;
end;

procedure TCustomBitmap.FreeImage;
begin
  inherited FreeImage;
  TSharedCustomBitmap(FSharedImage).FreeImage;
end;

constructor TCustomBitmap.Create;
begin
  inherited Create;
  FPixelFormat := pfDevice;
end;

destructor TCustomBitmap.Destroy;
begin
  FreeMaskHandle;
  inherited Destroy;
end;

procedure TCustomBitmap.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TCustomBitmap then
  begin
    FPixelFormat := TCustomBitmap(Source).FPixelFormat;
    FPixelFormatNeedsUpdate := TCustomBitmap(Source).FPixelFormatNeedsUpdate;
  end;
end;

function TCustomBitmap.CreateDefaultBitmapHandle(const ADesc: TRawImageDescription): HBITMAP;
var
  DC: HDC;
  BI: TBitmapInfo;
  P: Pointer;
begin
  if ADesc.Depth = 1
  then begin
    Result := CreateBitmap(ADesc.Width, ADesc.Height, 1, ADesc.Depth, nil);
    //AType := bmDDB;
  end
  else begin
    // on windows we need a DIB section
    FillChar(BI.bmiHeader, SizeOf(BI.bmiHeader), 0);
    BI.bmiHeader.biSize := SizeOf(BI.bmiHeader);
    BI.bmiHeader.biWidth := ADesc.Width;
    BI.bmiHeader.biHeight := -ADesc.Height; // request top down
    BI.bmiHeader.biPlanes := 1;
    BI.bmiHeader.biBitCount := ADesc.Depth;
    BI.bmiHeader.biCompression := BI_RGB;
    DC := GetDC(0);
    p := nil;
    Result := CreateDIBSection(DC, BI, DIB_RGB_COLORS, p, 0, 0);
    //AType := bmDIB;
    ReleaseDC(0, DC);

    // fallback for other widgetsets not implementing CreateDIBSection
    // we need the DIB section anyway someday if we want a scanline
    if Result = 0
    then begin
      Result := CreateBitmap(ADesc.Width, ADesc.Height, 1, ADesc.Depth, nil);
      //AType := bmDDB;
    end;
  end;
end;

procedure TCustomBitmap.FreeMaskHandle;
begin
  if FMaskHandle = 0 then Exit;

  DeleteObject(FMaskHandle);
  FMaskHandle := 0;
end;

procedure TCustomBitmap.HandleNeeded;
begin
  BitmapHandleNeeded;
end;

function TCustomBitmap.MaskHandleAllocated: boolean;
begin
  Result := FMaskHandle <> 0;
end;

procedure TCustomBitmap.MaskHandleNeeded;
var
  ImagePtr: PRawImage;
  MaskImage: TRawImage;
  msk, dummy: HBITMAP;
begin
  if FMaskHandle <> 0 then Exit;
  if not Masked then Exit;

  if TransparentMode = tmAuto then
  begin
    BitmapHandleNeeded; // create together with bitmaphandle

    if FMaskHandle <> 0 then Exit;
    ImagePtr := GetRawImagePtr;
    if ImagePtr^.Description.Format = ricfNone then Exit;

    // check if we have mask data
    if  ImagePtr^.IsMasked(False)
    then begin
      // move mask to image data, so we only have to create one handle
      // (and don't have to think about imagehandle format)

      MaskImage.Init;
      MaskImage.Description := ImagePtr^.Description.GetDescriptionFromMask;
      MaskImage.DataSize := ImagePtr^.MaskSize;
      MaskImage.Data := ImagePtr^.Mask;

      if CreateCompatibleBitmaps(MaskImage, msk, dummy, True)
      then begin
        if BitmapHandleAllocated
        then UpdateHandles(BitmapHandle, msk)
        else UpdateHandles(0, msk);
        Exit;
      end;
    end;
  end;

  // no data or transparent color is set - create ourselves
  CreateMask;
end;

function TCustomBitmap.PaletteAllocated: boolean;
begin
  Result := TSharedCustomBitmap(FSharedImage).FPalette <> 0;
end;

procedure TCustomBitmap.PaletteNeeded;
begin
  // TODO: implement
end;

procedure TCustomBitmap.RawimageNeeded(ADescOnly: Boolean);
var OldChangeEvent: TNotifyEvent;
    ImagePtr: PRawImage;
    Flags: TRawImageQueryFlags;
begin
ImagePtr:=@TSharedCustomBitmap(FSharedImage).FImage;
if (ImagePtr^.Description.Format=ricfNone)or
   ((not ADescOnly)and((ImagePtr^.Data=nil)or(ImagePtr^.DataSize<=0))and(ImagePtr^.Description.Width<>0)and(ImagePtr^.Description.Height<>0)) then
   begin
   // use savestream if present
   if FSharedImage.FSaveStream <> nil then
      begin
      FSharedImage.FSaveStream.Position := 0;
      OldChangeEvent := OnChange;
      try
      OnChange := nil;
      ReadStream(FSharedImage.FSaveStream, FSharedImage.FSaveStream.Size);
      FPixelFormatNeedsUpdate := True;
      finally OnChange := OldChangeEvent; end;
      end;
   // use handle
   if FSharedImage.FHandle <> 0 then
      begin
      if ADescOnly or not RawImage_FromBitmap(ImagePtr^,FSharedImage.FHandle,FMaskHandle) then
         ImagePtr^.Description:=GetDescriptionFromBitmap(FSharedImage.FHandle);
      FPixelFormatNeedsUpdate:=True;
      end;
   // setup ImagePtr, fill description if not set
   if ImagePtr^.Description.Format = ricfNone then
      begin
      // use query to get a default description without alpha, since alpha drawing
      // is not yet supported (unless asked for)
      // use var and not pixelformat property since it requires a rawimagedescription (which we are creating)
      case FPixelFormat of
           pf1bit:Flags := [riqfMono, riqfMask, riqfPalette];
           pf4bit,pf8bit: Flags := [riqfRGB, riqfMask, riqfPalette];
           pf24bit:Flags := [riqfRGB,riqfMask];
           pf32bit:Flags := [riqfRGB, riqfMask, riqfAlpha];
           else
           Flags:=[riqfRGB,riqfMask];
           end;
      ImagePtr^.Description:=QueryDescription(Flags,ImagePtr^.Description.Width,ImagePtr^.Description.Height);
      // atleast for now let pixelformat reflect the created description
      FPixelFormatNeedsUpdate:=True;
      end;
   if (not ADescOnly)and(ImagePtr^.Data=nil)and(ImagePtr^.DataSize<=0) then ImagePtr^.CreateData(True);// setup data
   end;
end;

function TCustomBitmap.ReleaseHandle: HBITMAP;
begin
  HandleNeeded;
  Result := FSharedImage.ReleaseHandle;
end;

procedure TCustomBitmap.SetBitmapHandle(const AValue: HBITMAP);
begin
  inherited SetBitmapHandle(AValue);
end;

function TCustomBitmap.LazarusResourceTypeValid(const ResourceType: string): boolean;
var
  ResType: String;
begin
  if Length(ResourceType) < 3 then Exit(False);

  ResType := UpperCase(ResourceType);
  case ResType[1] of
    'B': begin
      Result := (ResType = 'BMP') or (ResType = 'BITMAP');
    end;
    'X': begin
      Result := Restype = 'XPM';
    end;
  else
    Result := False;
  end;
end;

function TCustomBitmap.GetHandleType: TBitmapHandleType;
begin
  Result := TSharedCustomBitmap(FSharedImage).HandleType;
end;

function TCustomBitmap.GetMaskHandle: HBITMAP;
begin
  MaskHandleNeeded;
  Result := FMaskHandle;
end;

procedure TCustomBitmap.SetHandleType(AValue: TBitmapHandleType);
begin
  if HandleType = AValue then exit;

end;

procedure TCustomBitmap.SetMonochrome(AValue: Boolean);
begin
  if Monochrome = AValue then exit;
  if not AValue then Exit;

  if AValue
  then PixelFormat := pf1bit
  else PixelFormat := pfDevice;
end;

procedure TCustomBitmap.SetPixelFormat(AValue: TPixelFormat);
begin
if AValue<>FPixelFormat then
   begin
   {$IFDEF VerboseLCLTodos}{$note todo copy image into new format }{$ENDIF}
   FreeImage;
   FPixelFormat:=AValue;
   UpdatePixelFormat;
   end;
end;

procedure TCustomBitmap.SetSize(AWidth, AHeight: integer);
var
  SCB: TSharedCustomBitmap;
  CurIntfImage, NewIntfImage: TLazIntfImage;
  NewRawImage: TRawImage;
begin
  RawImageNeeded(True);
  
  if AWidth < 0 then AWidth := 0;
  if AHeight < 0 then AHeight := 0;

  SCB := TSharedCustomBitmap(FSharedImage);
  if  (SCB.FImage.Description.Height = cardinal(AHeight))
  and (SCB.FImage.Description.Width = cardinal(AWidth))
  then Exit;

  UnshareImage(False);
  // FSHaredImage might have been changed by UnshareImage
  SCB := TSharedCustomBitmap(FSharedImage);

  // for delphi compatibility copy old image
  RawImageNeeded(False);
  if  (SCB.FImage.Description.Height >= cardinal(AHeight))
  and (SCB.FImage.Description.Width >= cardinal(AWidth))
  then begin
    // use the faster ExtractRect. Since it calculates the intersection of source
    // and requested rect we can only use it when shrinking the image.
    SCB.FImage.ExtractRect(Rect(0, 0, AWidth, AHeight), NewRawImage);
  end
  else begin
    // use slow copy of pixeldata till rawimage can also copy to larger destination
  
    NewRawImage.Description := SCB.FImage.Description;
    NewRawImage.Description.Width := AWidth;
    NewRawImage.Description.Height := AHeight;
    NewRawImage.ReleaseData;

    if SCB.FImage.DataSize > 0 then
    begin
      NewRawImage.CreateData(True);
      CurIntfImage := TLazIntfImage.Create(SCB.FImage, False);
      NewIntfImage := TLazIntfImage.Create(NewRawImage, False);
      NewIntfImage.CopyPixels(CurIntfImage);
      CurIntfImage.Free;
      NewIntfImage.Free;
    end;
  end;

  SCB.FImage.FreeData;
  SCB.FImage := NewRawImage;
  // size was changed => update HDC and HBITMAP
  FreeCanvasContext;
  SCB.FreeHandle;
  FreeMaskHandle;
  Changed(Self);
end;

procedure TCustomBitmap.UpdatePixelFormat;
begin
  RawimageNeeded(True);
  FPixelFormat := TSharedCustomBitmap(FSharedImage).GetPixelFormat;
  FPixelFormatNeedsUpdate := False;
end;

function TCustomBitmap.GetMonochrome: Boolean;
begin
  RawImageNeeded(False);
  Result := TSharedCustomBitmap(FSharedImage).FImage.Description.Depth = 1;
end;

procedure TCustomBitmap.UnshareImage(CopyContent: boolean);
var
  NewImage: TSharedCustomBitmap;
  OldImage: TSharedCustomBitmap;
begin
  if FSharedImage.RefCount <= 1 then Exit;
  
  // release old FImage and create a new one
  OldImage := FSharedImage as TSharedCustomBitmap;
  NewImage := GetSharedImageClass.Create as TSharedCustomBitmap;
  try
    NewImage.Reference;
    if CopyContent and OldImage.ImageAllocated
    then begin
      // force a complete rawimage, so we can copy it
      RawimageNeeded(False);
      OldImage.FImage.ExtractRect(Rect(0, 0, Width, Height), NewImage.FImage);
    end
    else begin
      // keep width, height and bpp
      NewImage.FImage.Description := OldImage.FImage.Description;
    end;

    FreeCanvasContext;
    FSharedImage := NewImage;
    NewImage := nil; // transaction sucessful
    OldImage.Release;
  finally
    // in case something goes wrong, keep old and free new
    NewImage.Free;
  end;
end;

function TCustomBitmap.UpdateHandles(ABitmap, AMask: HBITMAP): Boolean;
begin
  // Update sets the handles corresponding to our rawimage and/or savestream, so
  // we do not free them here.
  
  Result := False;

  if FSharedImage.FHandle <> ABitmap
  then begin
    FSharedImage.FreeHandle;
    // get the properties from new bitmap
    FSharedImage.FHandle := ABitmap;
    Result := True;
  end;


  if FMaskHandle <> AMask
  then begin
    FreeMaskHandle;
    FMaskHandle := AMask;
    Result := True;
  end;
end;

function TCustomBitmap.GetBitmapHandle: HBITMAP;
begin
  BitmapHandleNeeded;
  Result := FSharedImage.FHandle;
end;

procedure TCustomBitmap.SetHandles(ABitmap, AMask: HBITMAP);
begin
  if FSharedImage.FHandle <> ABitmap
  then begin
    // if the handle is set externally we should unshare ourselves
    FreeCanvasContext;
    UnshareImage(false);
    FreeSaveStream;
    TSharedCustomBitmap(FSharedImage).FreeImage;
  end;

  if UpdateHandles(ABitmap, AMask)
  then begin
    FPixelFormatNeedsUpdate := True;
    FMasked := AMask <> 0;
    Changed(Self);
  end;
end;

procedure TCustomBitmap.SetHandle(AValue: THandle);
begin
  // for TCustomBitmap BitmapHandle = Handle
  BitmapHandle := AValue;
end;

function TCustomBitmap.InternalReleaseBitmapHandle: HBITMAP;
begin
  Result := FSharedImage.ReleaseHandle;
end;

function TCustomBitmap.InternalReleaseMaskHandle: HBITMAP;
begin
  Result := FMaskHandle;
  FMaskHandle := 0;
end;

function TCustomBitmap.InternalReleasePalette: HPALETTE;
begin
  Result := TSharedCustomBitmap(FSharedImage).ReleasePalette;
end;

function TCustomBitmap.GetPalette: HPALETTE;
begin
  PaletteNeeded;
  Result := TSharedCustomBitmap(FSharedImage).FPalette;
end;

function TCustomBitmap.GetPixelFormat: TPixelFormat;
begin
  if FPixelFormatNeedsUpdate
  then UpdatePixelFormat;
  Result := FPixelFormat;
end;

function TCustomBitmap.GetRawImagePtr: PRawImage;
begin
  RawimageNeeded(False);
  Result := @TSharedCustomBitmap(FSharedImage).FImage;
end;

function TCustomBitmap.GetRawImageDescriptionPtr: PRawImageDescription;
begin
  RawimageNeeded(True);
  Result := @TSharedCustomBitmap(FSharedImage).FImage.Description;
end;



