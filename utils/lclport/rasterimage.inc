{%MainUnit ../graphics.pp}

{******************************************************************************
                                     TRasterImage
 ******************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}

procedure TRasterImage.Assign(Source: TPersistent);
  procedure CopyMask(AMask: HBITMAP);
  var
    RI: TRawImage;
    msk, dummy: HBITMAP;
    Res: Boolean;
  begin
    // we need a winapi.CopyImage here (would make things easier)
    // in theory, it should not matter if a HBITMAP was created as bitmap or as mask
    // since there is a description problem in gtk, create both (we cannot create mask only)
    // todo: fix gtk
    if not RawImage_FromBitmap(RI, AMask, AMask) then Exit;
    msk := 0;
    dummy := 0;
    RawImage_CreateBitmaps(RI, dummy, msk {, True});
    RI.FreeData;
    DeleteObject(dummy);

    if BitmapHandleAllocated
    then Res := UpdateHandles(BitmapHandle, msk)
    else Res := UpdateHandles(0, msk);
    if not Res then DeleteObject(msk);
  end;


var
  SrcImage: TRasterImage absolute Source;
  SrcFPImage: TFPCustomImage absolute Source;
  SrcRawImage, DstRawImage: PRawImage;
  IntfImage: TLazIntfImage;
  ImgHandle,ImgMaskHandle: HBitmap;
begin
  if Source = Self then exit;

  if Source is TRasterImage
  then begin
    if MaskHandleAllocated
    then begin
      // Clear mask first mask
      if BitmapHandleAllocated
      then UpdateHandles(BitmapHandle, 0)
      else UpdateHandles(0, 0);
    end;

    FTransparentMode := SrcImage.FTransparentMode;
    FTransparentColor := SrcImage.FTransparentColor;
    FMasked := SrcImage.FMasked;

    // -> check if already shared
    if SrcImage.FSharedImage <> FSharedImage
    then begin
      // image is not shared => new image data
      // -> free canvas (interface handles)
      FreeCanvasContext;
      // release old FImage
      FSharedImage.Release;

      // We only can share images of the same type ...
      if CanShareImage(SrcImage.GetSharedImageClass)
      then begin
        // share FImage with assigned graphic
        FSharedImage := SrcImage.FSharedImage;
        FSharedImage.Reference;

        // when updating, unshare
        // Since we "share" it first, the unshare code will create a copy
        if (FUpdateCount > 0)
        or (SrcImage.FUpdateCount > 0)
        then begin
          UnshareImage(True);
          FreeSaveStream;
        end;
      end
      else begin
        // not sharable, create rawimage copy
        FSharedImage := GetSharedImageClass.Create;
        FSharedImage.Reference;

        // copy raw image
        SrcRawImage := SrcImage.GetRawImagePtr;
        DstRawImage := GetRawImagePtr;
        if (SrcRawImage <> nil) and (DstRawImage <> nil)
        then with SrcRawImage^ do
          ExtractRect(Rect(0, 0, Description.Width, Description.Height), DstRawImage^);
      end;
    end;

    if SrcImage.MaskHandleAllocated
    then CopyMask(SrcImage.MaskHandle);


    if FUpdateCount = 0
    then Changed(Self);

    Exit;
  end;

  if Source is TFPCustomImage
  then begin
    // todo: base on rawimage
  
    IntfImage := TLazIntfImage.Create(0,0,[]);
    try
      if BitmapHandleAllocated
      then IntfImage.DataDescription := GetDescriptionFromBitmap(BitmapHandle, 0, 0)
      else IntfImage.DataDescription := GetDescriptionFromDevice(0, 0, 0);
      IntfImage.Assign(SrcFPImage);
      IntfImage.CreateBitmaps(ImgHandle, ImgMaskHandle);
      SetHandles(ImgHandle, ImgMaskHandle);
    finally
      IntfImage.Free;
    end;
    if FUpdateCount = 0
    then Changed(Self);
    
    Exit;
  end;

  // fall back to default
  inherited Assign(Source);
end;

procedure TRasterImage.BeginUpdate(ACanvasOnly: Boolean);
begin
  if FUpdateCount = 0
  then begin
    UnshareImage(True);
    FUpdateCanvasOnly := ACanvasOnly;
  end
  else begin
    // if we are updating all, then requesting a canvas only won't change it
    // if we are updating canvas only, then requesting all is an error
    if FUpdateCanvasOnly and not ACanvasOnly
    then raise EInvalidGraphicOperation.Create(rsRasterImageUpdateAll);
  end;

  Inc(FUpdateCount);
end;

procedure TRasterImage.Clear;
begin
  if Empty then Exit;

  FreeSaveStream;
  SetSize(0, 0);
  if FUpdateCount = 0
  then Changed(Self);
end;

procedure TRasterImage.BitmapHandleNeeded;
var
  ImgHandle, ImgMaskHandle: HBitmap;
  ImagePtr: PRawImage;
  DevDesc: TRawImageDescription;
  QueryFlags: TRawImageQueryFlags;
  W, H: Integer;
  SkipMask: Boolean;
begin
  if BitmapHandleAllocated then exit;

  ImagePtr := GetRawImagePtr;
  if ImagePtr = nil then Exit;

  ImgMaskHandle := 0;

  // we must skip mask creation if
  // a) we already have mask
  // b) mask needs to be created another way - using TransparentColor
  // c) there's no mask data
  SkipMask := MaskHandleAllocated
           or (TransparentMode = tmFixed)
           or not ImagePtr^.IsMasked(False);

  if not CreateCompatibleBitmaps(ImagePtr^, ImgHandle, ImgMaskHandle, SkipMask)
  then begin

    // create a default handle
    W := Width;
    if W < 1 then W := 1;
    H := Height;
    if H < 1 then H := 1;

    if ImagePtr^.Description.Depth = 1
    then QueryFlags := [riqfMono]
    else QueryFlags := [riqfRGB];
    if ImagePtr^.Description.AlphaPrec <> 0
    then Include(QueryFlags, riqfAlpha);
    if ImagePtr^.Description.MaskBitsPerPixel <> 0
    then Include(QueryFlags, riqfMask);
    QueryDescription(DevDesc, QueryFlags, W, H);
    ImgHandle := CreateDefaultBitmapHandle(DevDesc);
  end;

  // if we do not have new mask then either use old one or use none
  if SkipMask and MaskHandleAllocated
  then UpdateHandles(ImgHandle, MaskHandle)
  else UpdateHandles(ImgHandle, ImgMaskHandle);
end;

function TRasterImage.CanShareImage(AClass: TSharedRasterImageClass): Boolean;
begin
  // We only can share images of the same type.
  // And if we are some "end" graphic type
  Result := (AClass <> TSharedCustomBitmap)
        and (AClass = GetSharedImageClass);
end;


procedure TRasterImage.Draw(DestCanvas: TCanvas; const DestRect: TRect);
var
  UseMaskHandle: HBitmap;
  SrcDC: hDC;
  DestDC: hDC;
begin
  if (Width=0) or (Height=0)
  then Exit;

  BitmapHandleNeeded;
  if not BitmapHandleAllocated then Exit;

  if Masked then
    UseMaskHandle:=MaskHandle
  else
    UseMaskHandle:=0;

  SrcDC := Canvas.GetUpdatedHandle([csHandleValid]);
  DestCanvas.Changing;
  DestDC := DestCanvas.GetUpdatedHandle([csHandleValid]);
  StretchMaskBlt(DestDC,
          DestRect.Left,DestRect.Top,
          DestRect.Right-DestRect.Left,DestRect.Bottom-DestRect.Top,
          SrcDC,0,0,Width,Height, UseMaskHandle,0,0,DestCanvas.CopyMode);
  DestCanvas.Changed;
end;

procedure TRasterImage.EndUpdate(AStreamIsValid: Boolean = False);
begin
  if FUpdatecount = 0
  then raise EInvalidGraphicOperation.Create(rsRasterImageEndUpdate);
  
  Dec(FUpdatecount);
  if FUpdatecount > 0 then Exit;

  if not FUpdateCanvasOnly
  then begin
    FreeCanvasContext;
    // delete bitmaphandle too
    if BitmapHandleAllocated
    then DeleteObject(InternalReleaseBitmapHandle)

  end;

  if not AStreamIsValid
  then FreeSaveStream;
  Changed(Self);
end;

constructor TRasterImage.Create;
begin
  inherited Create;
  FSharedImage := GetSharedImageClass.Create;
  FSharedImage.Reference;
  FTransparentColor := clDefault; // for Delphi compatibility. clDefault means:
                                  // use Left,Bottom pixel as transparent pixel
end;

destructor TRasterImage.Destroy;
begin
  FreeCanvasContext;
  FSharedImage.Release;
  FSharedImage := nil;
  FreeAndNil(FCanvas);
  inherited Destroy;
end;

procedure TRasterImage.FreeCanvasContext;
begin
  if (FCanvas <> nil) then TBitmapCanvas(FCanvas).FreeDC;
end;

function TRasterImage.GetCanvas: TCanvas;
begin
  if FCanvas = nil then
    CreateCanvas;
  Result := FCanvas;
end;

procedure TRasterImage.CreateCanvas;
begin
  if FCanvas <> nil then Exit;

  FCanvas := TBitmapCanvas.Create(Self);
  FCanvas.OnChanging := @CanvasChanging;
  FCanvas.OnChange := @Changed;
end;

procedure TRasterImage.FreeImage;
begin
  SetHandle(0);
end;

procedure TRasterImage.LoadFromBitmapHandles(ABitmap, AMask: HBitmap; ARect: PRect);
var
  RawImg: TRawImage;
  ImgHandle, ImgMaskHandle: HBitmap;
begin
  //DebugLn('TRasterImage.CreateFromBitmapHandles A SrcRect=',dbgs(SrcRect));
  if not RawImage_FromBitmap(RawImg, ABitmap, AMask, ARect) then
    raise EInvalidGraphicOperation.Create('TRasterImage.LoadFromBitmapHandles Get RawImage');
  ImgHandle:=0;
  ImgMaskHandle:=0;
  try
    //DebugLn('TRasterImage.CreateFromBitmapHandles B SrRect=',dbgs(SrcRect));
    if not RawImage_CreateBitmaps(RawImg, ImgHandle, ImgMaskHandle) then
      raise EInvalidGraphicOperation.Create('TRasterImage.LoadFromBitmapHandles Create bitmaps');
    SetHandles(ImgHandle, ImgMaskHandle);
    ImgHandle:=0;
    ImgMaskHandle:=0;
  finally
    RawImg.FreeData;
    if ImgHandle<>0 then DeleteObject(ImgHandle);
    if ImgMaskHandle<>0 then DeleteObject(ImgMaskHandle);
  end;
end;

procedure TRasterImage.LoadFromDevice(DC: HDC);
var
  IntfImg: TLazIntfImage;
  ImgHandle, ImgMaskHandle: HBitmap;
begin
  ImgHandle:=0;
  ImgMaskHandle:=0;
  IntfImg:=nil;
  try
    // create the interface image
    IntfImg:=TLazIntfImage.Create(0,0,[]);
    // get a snapshot
    IntfImg.LoadFromDevice(DC);
    // create HBitmap
    IntfImg.CreateBitmaps(ImgHandle, ImgMaskHandle);
    // feed HBitmap into a TRasterImage
    SetHandles(ImgHandle, ImgMaskHandle);
    ImgHandle:=0;
    ImgMaskHandle:=0;
  finally
    IntfImg.Free;
    if ImgHandle<>0 then DeleteObject(ImgHandle);
    if ImgMaskHandle<>0 then DeleteObject(ImgMaskHandle);
  end;
end;

procedure TRasterImage.Mask(ATransparentColor: TColor);
begin
  CreateMask(ATransparentColor);
end;

procedure TRasterImage.SetTransparentColor(AValue: TColor);
begin
  if FTransparentColor = AValue then exit;
  FTransparentColor := AValue;

  if AValue = clDefault
  then FTransparentMode := tmAuto
  else FTransparentMode := tmFixed;

  if MaskHandleAllocated
  then MaskHandle := 0
  else Changed(Self);
end;

procedure TRasterImage.Changed(Sender: TObject);
begin
  if FUpdateCount > 0 then Exit;

  //FMaskBitsValid := False;
  if Sender = FCanvas
  then FreeSaveStream;

  inherited Changed(Sender);
end;

function TRasterImage.CreateDefaultBitmapHandle(
  const ADesc: TRawImageDescription): HBITMAP;
begin
  Result := 0;
end;

procedure TRasterImage.CanvasChanging(Sender: TObject);
begin
  if FUpdateCount > 0 then Exit;

  // called before the canvas is modified
  // -> make sure the handle is unshared (otherwise the modifications will also
  //    modify all copies)
  // -> Savestream will be freed when changed (so it can be loaded by canvas)
  UnshareImage(True);
end;

procedure TRasterImage.LoadFromStream(AStream: TStream);
begin
  LoadFromStream(AStream, AStream.Size - AStream.Position);
end;

procedure TRasterImage.LoadFromStream(AStream: TStream; ASize: Cardinal);
var
  WorkStream: TMemoryStream;
  OldPos, NewSize: Int64;
begin
  BeginUpdate;
  UnshareImage(False);
  Clear; // clear old saved stream, allocated handles, etc
  if ASize = 0
  then begin
    EndUpdate(False);
    Exit;
  end;

  WorkStream := nil;
  try
    WorkStream := TMemoryStream.Create;
    WorkStream.SetSize(ASize);
    OldPos := AStream.Position;
    WorkStream.CopyFrom(AStream, ASize);
    WorkStream.Position := 0;
    ReadStream(WorkStream, ASize);
    NewSize := WorkStream.Position;
    if NewSize < ASize
    then begin
      // the size given was different than the image loaded
      // MWE: original code adjusted the streampos, do we stil need this ?
      AStream.Position := OldPos + NewSize;
      WorkStream.SetSize(NewSize);
    end;
    // Store our worstream as savestream
    FSharedImage.SaveStream := WorkStream;
    WorkStream := nil;
  finally
    // if something went wrong, free the workstream
    WorkStream.Free;
  end;
  EndUpdate(True);
end;

function TRasterImage.GetRawImage: TRawImage;
var
  p: PRawImage;
begin
  p := GetRawImagePtr;
  if p = nil
  then Result{%H-}.Init
  else Result := p^;
end;

function TRasterImage.GetScanline(ARow: Integer): Pointer;
var
  RI: TRawImage;
begin
  RI := GetRawImage;
  Result := RI.GetLineStart(ARow);
end;

function TRasterImage.GetTransparentColor: TColor;
begin
  if FTransparentColor = clDefault
  then Result := RequestTransparentColor
  else Result := FTransparentColor;
end;

procedure TRasterImage.GetSupportedSourceMimeTypes(List: TStrings);
begin
  if (ClassType=TBitmap) or (ClassType=TPixmap) or (ClassType=TCustomIcon) then
  begin
    List.Clear;
    List.Add(PredefinedClipboardMimeTypes[pcfBitmap]);
    List.Add(PredefinedClipboardMimeTypes[pcfDelphiBitmap]);
    List.Add(PredefinedClipboardMimeTypes[pcfPixmap]);
  end else
    inherited GetSupportedSourceMimeTypes(List);
end;

function TRasterImage.GetTransparent: Boolean;
var
  Desc: PRawImageDescription;
begin
  if Masked
  then begin
    // postpone description generation since we know we are transparent here
    Result := True
  end
  else begin
    Desc := GetRawImageDescriptionPtr;
    Result := (Desc <> nil)
          and (Desc^.Format <> ricfNone)
          and (Desc^.AlphaPrec > 0);
    //TODO:  check for transparency through palette etc.
  end;
end;

function TRasterImage.GetWidth: Integer;
var
  Desc: PRawImageDescription;
begin
  Desc := GetRawImageDescriptionPtr;
  if (Desc = nil) or (Desc^.Format = ricfNone)
  then Result := 0
  else Result := Desc^.Width;
end;

function TRasterImage.HandleAllocated: boolean;
begin
  Result := FSharedImage.FHandle <> 0;
end;

function TRasterImage.GetMimeType: string;
begin
  {$IFDEF VerboseLCLTodos}{$note: implement}{$ENDIF}

  (*
  if (ClassType=TBitmap) or (ClassType=TPixmap) or (ClassType=TCustomIcon) then begin
    if FSharedImage.SaveStream<>nil then begin
      case FSharedImage.SaveStreamType of
      bnXPixmap: Result:=PredefinedClipboardMimeTypes[pcfPixmap];
      else
        Result:=PredefinedClipboardMimeTypes[pcfBitmap];
      end;
    end else
      Result:=PredefinedClipboardMimeTypes[pcfBitmap];
  end else
  *)

  Result:=inherited GetMimeType;
end;

procedure TRasterImage.LoadFromIntfImage(IntfImage: TLazIntfImage);
var
  ImgHandle, ImgMaskHandle: HBitmap;
begin
  IntfImage.CreateBitmaps(ImgHandle, ImgMaskHandle, not IntfImage.HasMask);
  SetHandles(ImgHandle, ImgMaskHandle);
end;

procedure TRasterImage.FreeSaveStream;
begin
  if FSharedImage.FSaveStream = nil then exit;
  //DebugLn(['TRasterImage.FreeSaveStream A ',ClassName,' ',FImage.FSaveStream.Size]);
  UnshareImage(false);
  FreeAndNil(FSharedImage.FSaveStream);
end;

procedure TRasterImage.LoadFromMimeStream(AStream: TStream; const AMimeType: string);
begin
  {$IFDEF VerboseLCLTodos}{$note Make oo}{$ENDIF}
  if (ClassType=TBitmap) or (ClassType=TPixmap) or (ClassType=TCustomIcon) then begin
    if (AnsiCompareText(AMimeType,PredefinedClipboardMimeTypes[pcfBitmap])=0)
    or (AnsiCompareText(AMimeType,PredefinedClipboardMimeTypes[pcfDelphiBitmap])=0)
    or (AnsiCompareText(AMimeType,PredefinedClipboardMimeTypes[pcfPixmap])=0) then
    begin
      LoadFromStream(AStream);
      exit;
    end;
  end;
  inherited LoadFromMimeStream(AStream, AMimeType);
end;

procedure TRasterImage.LoadFromRawImage(const AImage: TRawImage; ADataOwner: Boolean);
var
  img: PRawImage;
begin
  BeginUpdate;
  try
    Clear;
    if AImage.Description.Format = ricfNone then Exit; // empty image

    img := GetRawImagePtr;
    img^.Description := AImage.Description;
    if ADataOwner
    then begin
      img^.DataSize := AImage.DataSize;
      img^.Data := AImage.Data;
      img^.MaskSize := AImage.MaskSize;
      img^.Mask := AImage.Mask;
      img^.PaletteSize := AImage.PaletteSize;
      img^.Palette := AImage.Palette;
    end
    else begin
      // copy needed
      img^.DataSize := AImage.DataSize;
      if img^.DataSize > 0
      then begin
        GetMem(img^.Data, img^.DataSize);
        Move(AImage.Data^, img^.Data^, img^.DataSize);
      end
      else img^.Data := nil;

      img^.MaskSize := AImage.MaskSize;
      if img^.MaskSize > 0
      then begin
        GetMem(img^.Mask, img^.MaskSize);
        Move(AImage.Mask^, img^.Mask^, img^.MaskSize);
      end
      else img^.Mask := nil;

      img^.PaletteSize := AImage.PaletteSize;
      if img^.PaletteSize > 0
      then begin
        GetMem(img^.Palette, img^.PaletteSize);
        Move(AImage.Palette^, img^.Palette^, img^.PaletteSize);
      end
      else img^.Palette := nil;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TRasterImage.SaveToStream(AStream: TStream);
  procedure Error;
  begin
    raise FPImageException.Create(rsErrorWhileSavingBitmap);
  end;
var
  Size, BytesWritten: Int64;
begin
  SaveStreamNeeded;
  if FSharedImage.SaveStream = nil
  then Error;

  FSharedImage.SaveStream.Position := 0;
  Size := FSharedImage.SaveStream.Size;
  if AStream is TMemoryStream
  then TMemoryStream(AStream).SetSize(AStream.Position + Size);

  BytesWritten := AStream.CopyFrom(FSharedImage.SaveStream, Size);
  if BytesWritten <> Size
  then Error;
end;

procedure TRasterImage.SetBitmapHandle(AValue: HBITMAP);
begin
  if MaskHandleAllocated
  then SetHandles(AValue, MaskHandle)
  else SetHandles(AValue, 0);
end;

procedure TRasterImage.SetHandle(AValue: THandle);
begin
  if FSharedImage.FHandle = AValue then Exit;
  FreeCanvasContext;
  UnshareImage(False);
  FSharedImage.FHandle := AValue;
  if FUpdateCount = 0
  then Changed(Self);
end;

procedure TRasterImage.SetMaskHandle(AValue: HBITMAP);
begin
  if BitmapHandleAllocated
  then SetHandles(BitmapHandle, AValue)
  else SetHandles(0, AValue);
end;

procedure TRasterImage.SetMasked(AValue: Boolean);
begin
  if AValue = Masked then Exit;

  FMasked := AValue;
  Changed(Self);
end;

procedure TRasterImage.SetTransparentMode(AValue: TTransparentMode);
begin
  if AValue = TransparentMode then exit;
  FTransparentMode := AValue;

  if AValue = tmAuto
  then TransparentColor := clDefault
  else TransparentColor := RequestTransparentColor;
end;

procedure TRasterImage.SetTransparent(AValue: Boolean);
var
  lTransparent: Boolean;
begin
  lTransparent := GetTransparent();
  if AValue = lTransparent then Exit;

  // some delphi compatibility, we can only change transparency through the mask.
  Masked := AValue;
end;

// release handles without freeing them
// useful for creating a HBitmap
function TRasterImage.ReleaseBitmapHandle: HBITMAP;
begin
  BitmapHandleNeeded;
  FreeCanvasContext;
  Result := InternalReleaseBitmapHandle;
end;

function TRasterImage.ReleaseMaskHandle: HBITMAP;
begin
  MaskHandleNeeded;
  FreeCanvasContext;
  Result := InternalReleaseMaskHandle;
end;

function TRasterImage.ReleasePalette: HPALETTE;
begin
  PaletteNeeded;
  FreeCanvasContext;
  Result := InternalReleasePalette;
end;

procedure TRasterImage.SaveStreamNeeded;
var
  WorkStream: TMemoryStream;
begin
  if FUpdateCount > 0
  then raise EInvalidGraphicOperation.Create(rsRasterImageSaveInUpdate);

  if FSharedImage.SaveStream <> nil then Exit;
  
  WorkStream := TMemoryStream.Create;
  try
    WriteStream(WorkStream);
    // Store our worstream as savestream
    FSharedImage.SaveStream := WorkStream;
    WorkStream := nil;
  finally
    // if something went wrong, free the workstream
    WorkStream.Free;
  end;
end;

function TRasterImage.CreateIntfImage: TLazIntfImage;
begin
  {$IFDEF VerboseLCLTodos}{$note todo: create based on rawimage}{$ENDIF}
  Result := TLazIntfImage.Create(0,0,[]);
  Result.LoadFromBitmap(BitmapHandle, MaskHandle);
end;

procedure TRasterImage.CreateMask(AColor: TColor);
var
  IntfImage: TLazIntfImage;
  ImgHandle, MskHandle: HBitmap;
  TransColor: TFPColor;
begin
  //DebugLn(['TRasterImage.CreateMask ',Width,'x',Height,' ',Transparent,' ',dbgs(ord(TransparentMode)),' ',dbgs(TransparentColor)]);
  if (Width = 0)
  or (Height = 0)
  or (AColor = clNone)
  or (  (FTransparentMode = tmFixed)
    and (FTransparentColor = clNone)
    and (AColor = clDefault)
  )
  then begin
    MaskHandle := 0;
    Exit;
  end;
  
  {$IFDEF VerboseLCLTodos}{$note todo: move to IntfImage}{$ENDIF}
  IntfImage := TLazIntfImage.Create(0,0,[]);
  try
    // force handle creation here, since at next step we will check for mask handle
    ImgHandle := BitmapHandle;
    // load from bitmap needs a mask handle otherwise no mask description is
    // created.
    if MaskHandleAllocated
    then MskHandle := MaskHandle 
    else MskHandle := CreateBitmap(Width, Height, 1, 1, nil);
    IntfImage.LoadFromBitmap(ImgHandle, MskHandle);
    if not MaskHandleAllocated
    then DeleteObject(MskHandle);
    ImgHandle := 0;

    if AColor = clDefault
    then begin
      if (FTransparentMode = tmFixed) and (FTransparentColor <> clDefault)
      then TransColor := TColorToFPColor(ColorToRGB(FTransparentColor))
      else TransColor := IntfImage.Colors[0, IntfImage.Height - 1];
    end
    else TransColor := TColorToFPColor(ColorToRGB(AColor));

    IntfImage.Mask(TransColor);

    IntfImage.CreateBitmaps(ImgHandle, MskHandle);
    MaskHandle := MskHandle;
    DeleteObject(ImgHandle);
  finally
    IntfImage.Free;
  end;
end;

function TRasterImage.GetEmpty: boolean;
begin
  Result := FSharedImage.IsEmpty;
end;

function TRasterImage.GetHandle: THandle;
begin
  HandleNeeded;
  Result := FSharedImage.FHandle;
end;

function TRasterImage.GetHeight: Integer;
var
  Desc: PRawImageDescription;
begin
  Desc := GetRawImageDescriptionPtr;
  if (Desc = nil) or (Desc^.Format = ricfNone)
  then Result := 0
  else Result := Desc^.Height;
end;

function TRasterImage.GetMasked: Boolean;
begin
  Result := FMasked;
end;

class function TRasterImage.GetSharedImageClass: TSharedRasterImageClass;
begin
  Result := TSharedRasterImage;
end;

procedure TRasterImage.GetSize(out AWidth, AHeight: Integer);
var
  Desc: PRawImageDescription;
begin
  Desc := GetRawImageDescriptionPtr;
  if (Desc = nil) or (Desc^.Format = ricfNone)
  then begin
    AWidth := 0;
    AHeight := 0;
  end
  else begin
    AWidth := Desc^.Width;
    AHeight := Desc^.Height;
  end;
end;



procedure TRasterImage.ReadData(Stream: TStream);
  function GetImageClass: TRasterImageClass;
  const
    // need to repeat here since they aren't defined yet
    IconSignature: array [0..3] of char = #0#0#1#0;
    CursorSignature: array [0..3] of char = #0#0#2#0;
  var
    Sig: array[0..7] of Char;
    Position: Int64;
  begin
    Position := Stream.Position;
    Stream.Read(Sig[0], SizeOf(Sig));
    Stream.Position := Position;

    if (Sig[0] = 'B') and (Sig[1] = 'M') then Exit(TBitmap);
    if CompareMem(@Sig[0], @PNGcomn.Signature[0], 8) then Exit(TPortableNetworkGraphic);
    if CompareMem(@Sig[0], @IconSignature[0], 4) then Exit(TIcon);
    if CompareMem(@Sig[0], @CursorSignature[0], 4) then Exit(TCursorImage);
    if TestStreamIsXPM(Stream) then Exit(TPixmap);

    Result := nil;
  end;

var
  Size: Longint;
  ImageClass: TRasterImageClass;
  Image: TRasterImage;
begin
  Stream.Read(Size, SizeOf(Size));
  Size := LEtoN(Size);
  
  // pre laz 0.9.26 there was no strict relation between graphic format and
  // classtype, so we need to check if we need some conversion
  if Size >= 8
  then ImageClass := GetImageClass
  else ImageClass := nil;

  if (ImageClass = nil) or ClassType.InheritsFrom(ImageClass)
  then begin
    // no conversion needed, or it wasn't a known "old" format
    LoadFromStream(Stream, Size);
    Exit;
  end;
  
  Image := ImageClass.Create;
  Image.LoadFromStream(Stream, Size);
  try
    Assign(Image);
  finally
    Image.Free;
  end;
end;

procedure TRasterImage.WriteData(Stream: TStream);
  procedure Error;
  begin
    raise FPImageException.Create(rsErrorWhileSavingBitmap);
  end;
var
  Size: Longint;
begin
  SaveStreamNeeded;
  if FSharedImage.SaveStream = nil
  then Error;

  Size := NtoLE(Longint(FSharedImage.SaveStream.Size));
  Stream.Write(Size, SizeOf(Size));
  SaveToStream(Stream);
end;

function TRasterImage.RequestTransparentColor: TColor;
var
  RawImagePtr: PRawImage;
  IntfImage: TLazIntfImage;
begin
  // if RawImage exits then use it to get pixel overwise get it from the canvas
  if Empty then
  begin
    Result := clNone;
    Exit;
  end;

  RawImagePtr := GetRawImagePtr;
  if RawImagePtr <> nil then
  begin
    IntfImage := TLazIntfImage.Create(RawImagePtr^, False);
    try
      Result := FPColorToTColor(IntfImage.Colors[0, Height - 1]);
    finally
      IntfImage.Free;
    end;
  end
  else
    Result := Canvas.GetPixel(0, Height - 1);
end;

procedure TRasterImage.SetWidth(AWidth: Integer);
begin
  SetSize(AWidth, Height);
end;

procedure TRasterImage.SetHeight(AHeight: Integer);
begin
  SetSize(Width, AHeight);
end;

// included by graphics.pp


