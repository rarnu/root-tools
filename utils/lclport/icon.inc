{%MainUnit ../graphics.pp}

{******************************************************************************
                                    TCustomIcon
 ******************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}

const
  IconSignature: array [0..3] of char = #0#0#1#0;

type
  TIconHeader = {packed} record // packed it not needed
    idReserved: Word; // 0
    idType: Word;     // 1 - Icon, 2 - Cursor
    idCount: Word;    // number of icons in file
  end;

  TIconDirEntry = {packed} record // packing not needed
    bWidth: Byte;           // a value of 0 means 256
    bHeight: Byte;          // a value of 0 means 256
    bColorCount: Byte;      // number of entires in pallette table below
    bReserved: Byte;        // not used  = 0
    case Byte of
      1: (
        // icon
        wPlanes: Word;      // number of planes, should be 0 or 1
        wBpp: Word;         // bits per pixel
        // common
        dwBytesInRes: Longint;  // total number bytes in images including pallette
                                // data: XOR, AND and bitmap info header
        dwImageOffset: Longint; // pos of image as offset from the beginning of file
      );
      2:(
        // cursor
        wXHotSpot: Word;
        wYHotSpot: Word;
      );
  end;
  
  PIconDirEntry = ^TIconDirEntry;

  // executables and libraries has the next structures for icons and cursors
  PGrpIconDirEntry = ^TGrpIconDirEntry;
  TGrpIconDirEntry = packed record
    bWidth: Byte;           // Width, in pixels, of the image
    bHeight: Byte;          // Height, in pixels, of the image
    bColorCount: Byte;      // Number of colors in image (0 if >=8bpp)
    bReserved: Byte;        // Reserved
    wPlanes: Word;          // color planes
    wBpp: Word;             // bits per pixel
    dwBytesInRes: Dword;    // how many bytes in this resource?
    nID: Word;              // the ID
  end;

  PGrpCursorDirEntry = ^TGrpCursorDirEntry;
  TGrpCursorDirEntry = packed record
    wWidth: Word;           // Width, in pixels, of the image
    wHeight: Word;          // Height, in pixels, of the image
    wPlanes: Word;          // color planes
    wBitCount: Word;        // bits per pixel
    dwBytesInRes: Dword;    // how many bytes in this resource?
    nID: Word;              // the ID
  end;

  TLocalHeader = packed record
    xHotSpot: Word;
    yHotSpot: Word;
  end;

  PNewHeader = ^TNewHeader;
  TNewHeader = packed record
    idReserved: Word; // Reserved (must be 0)
    idType: Word;     // Resource type (1 for icons)
    idCount: Word;    //  How many images?
  end;

function TestStreamIsIcon(const AStream: TStream): boolean;
var
  Signature: array[0..3] of char;
  ReadSize: Integer;
  OldPosition: TStreamSeekType;
begin
  OldPosition:=AStream.Position;
  ReadSize:=AStream.Read(Signature, SizeOf(Signature));
  Result:=(ReadSize=SizeOf(Signature)) and CompareMem(@Signature,@IconSignature,4);
  AStream.Position:=OldPosition;
end;

////////////////////////////////////////////////////////////////////////////////

{ TSharedIcon }

procedure TSharedIcon.FreeHandle;
begin
  if FHandle = 0 then Exit;

  DestroyIcon(FHandle);
  FHandle := 0;
end;

procedure TSharedIcon.UpdateFromHandle(NewHandle: THandle);
var
  Info: TIconInfo;
begin
  FreeHandle;
  FHandle := NewHandle;
  // get the icon information
  if WidgetSet.GetIconInfo(FHandle, @Info) then
    Add(GetImagesClass.Create(Info));
end;

function TSharedIcon.IsEmpty: boolean;
begin
  Result := inherited IsEmpty and (Count = 0);
end;

function TSharedIcon.GetImage(const AIndex: Integer): TIconImage;
begin
  Result := TIconImage(FImages[AIndex]);
end;

class function TSharedIcon.GetImagesClass: TIconImageClass;
begin
  Result := TIconImage;
end;

procedure TSharedIcon.Add(AIconImage: TIconImage);
begin
  FImages.Add(AIconImage);
end;

constructor TSharedIcon.Create;
begin
  inherited Create;
  FImages := TFPList.Create;
end;

procedure TSharedIcon.Delete(AIndex: Integer);
var
  Image: TIconImage;
begin
  Image := TIconImage(FImages[AIndex]);
  FImages.Delete(AIndex);
  Image.Free;
end;

destructor TSharedIcon.Destroy;
begin
  Clear;
  FreeAndNil(FImages);
  inherited Destroy;
end;

procedure TSharedIcon.Clear;
var
  n: Integer;
begin
  for n := 0 to FImages.Count - 1 do
    TObject(FImages[n]).Free;
  FImages.Clear;
end;

function TSharedIcon.GetIndex(AFormat: TPixelFormat; AHeight, AWidth: Word): Integer;
var
  //List: TFPList;
  Image: TIconImage;
begin
  for Result := 0 to FImages.Count -1 do
  begin
    Image := TIconImage(FImages[Result]);
    if Image.PixelFormat <> AFormat then Continue;
    if Image.Height <> AHeight then Continue;
    if Image.Width <> AWidth then Continue;
    // found
    Exit;
  end;
  Result := -1;
end;

function TSharedIcon.Count: Integer;
begin
  Result := FImages.Count;
end;

////////////////////////////////////////////////////////////////////////////////

{ TIconImage }

constructor TIconImage.Create(AFormat: TPixelFormat; AHeight, AWidth: Word);
begin
  inherited Create;
  FHeight := AHeight;
  FWidth := AWidth;
  FPixelFormat := AFormat;
end;

constructor TIconImage.Create(const AImage: TRawImage);
begin
  inherited Create;
  UpdateFromImage(AImage);
end;

constructor TIconImage.Create(const AInfo: TIconInfo);
var
  AImage: TRawImage;
begin
  inherited Create;
  FHandle := AInfo.hbmColor;
  FMaskHandle := AInfo.hbmMask;
  if RawImage_FromBitmap(AImage, FHandle, FMaskHandle) then
    UpdateFromImage(AImage);
end;

destructor TIconImage.Destroy;
begin
  if FHandle <> 0
  then DeleteObject(FHandle);
  FHandle := 0;
  if FMaskHandle <> 0
  then DeleteObject(FMaskHandle);
  FMaskHandle := 0;
  if FPalette <> 0
  then DeleteObject(FPalette);
  FPalette := 0;
  FImage.FreeData;
  inherited Destroy;
end;

function TIconImage.GetPalette: HPALETTE;
begin
  // TODO: implement
  Result := FPalette
end;

procedure TIconImage.RawImageNeeded(ADescOnly: Boolean);
var
  ImagePtr: PRawImage;
  Flags: TRawImageQueryFlags;
begin
  ImagePtr := @FImage;
  if ImagePtr^.Description.Format <> ricfNone
  then begin
    // description valid
    if ADescOnly then Exit;
    if (ImagePtr^.Data <> nil) and (ImagePtr^.DataSize > 0) then Exit;
    if ImagePtr^.Description.Width = 0 then Exit;  // no data
    if ImagePtr^.Description.Height = 0 then Exit; // no data
  end;

  if FHandle <> 0
  then begin
    if ADescOnly
    or not RawImage_FromBitmap(ImagePtr^, FHandle, FMaskHandle)
    then ImagePtr^.Description := GetDescriptionFromBitmap(FHandle);
    Exit;
  end;

  case PixelFormat of
    pf1bit: Flags := [riqfMono, riqfMask];
    pf4bit,
    pf8bit: Flags := [riqfRGB, riqfMask, riqfPalette];
    pf32bit: Flags := [riqfRGB, riqfMask, riqfAlpha];
  else
    Flags := [riqfRGB, riqfMask];
  end;
  ImagePtr^.Description := QueryDescription(Flags, Width, Height);
end;

procedure TIconImage.UpdateFromImage(const AImage: TRawImage);
begin
  FImage := AImage;
  FHeight := FImage.Description.Height;
  FWidth := FImage.Description.Width;

  case FImage.Description.Depth of
     1: FPixelFormat := pf1Bit;
     4: FPixelFormat := pf4Bit;
     8: FPixelFormat := pf8Bit;
    15: FPixelFormat := pf15Bit;
    16: FPixelFormat := pf16Bit;
    24: FPixelFormat := pf24Bit;
    32: FPixelFormat := pf32Bit;
  else
    FPixelFormat := pfCustom;
  end;
end;

function TIconImage.ReleaseHandle: HBITMAP;
begin
  Result := Handle;
  FHandle := 0;
end;

function TIconImage.ReleaseMaskHandle: HBITMAP;
begin
  Result := MaskHandle;
  FMaskHandle := 0;
end;

function TIconImage.ReleasePalette: HPALETTE;
begin
  Result := Palette;
  FPalette := 0;
end;

function TIconImage.UpdateHandles(ABitmap, AMask: HBITMAP): Boolean;
begin
  Result := False;
  
  if FHandle <> ABitmap
  then begin
    if FHandle <> 0
    then DeleteObject(FHandle);
    FHandle := ABitmap;
    Result := True;
  end;

  if FMaskHandle <> AMask
  then begin
    if FMaskHandle <> 0
    then DeleteObject(FMaskHandle);
    FMaskHandle := AMask;
    Result := True;
  end;
end;


////////////////////////////////////////////////////////////////////////////////

{ TCustomIcon }

procedure TCustomIcon.Add(AFormat: TPixelFormat; AHeight, AWidth: Word);
begin
  if GetIndex(AFormat, AHeight, AWidth) <> -1
  then raise EInvalidGraphicOperation.Create(rsDuplicateIconFormat);
  
  UnshareImage(True);
  if TSharedIcon(FSharedImage).FImages.Add(TIconImage.Create(AFormat, AHeight, AWidth)) = 0
  then begin
    // First added
    FCurrent := 0;
    UpdateCurrentView;
  end;
end;

procedure TCustomIcon.Assign(Source: TPersistent);
begin
  BeginUpdate;
  if Source is TCustomIcon
  then begin
    FCurrent := -1;
  end
  else
  if Source is TRasterImage
  then begin
    Clear;
    
    with TRasterImage(Source) do
      Self.Add(PixelFormat, Height, Width);

    AssignImage(TRasterImage(Source));
    EndUpdate;
    Exit;
  end;

  inherited Assign(Source);

  if Source is TCustomIcon
  then begin
    FCurrent := TCustomIcon(Source).Current;

    // temporary hack since TRasterImage assign cannot handle multiply rawimages
    if TCustomIcon(Source).GetSharedImageClass <> GetSharedImageClass
    then UnshareImage(True);
  end;

  EndUpdate;
end;

procedure TCustomIcon.AssignImage(ASource: TRasterImage);
var
  Image, NewImage: TIconImage;
  RawImg: PRawImage;
  RawMsk: TRawImage;
begin
  if FCurrent = -1
  then raise EInvalidGraphicOperation.Create(rsIconNoCurrent);

  if ASource = nil
  then raise EInvalidGraphicOperation.Create(rsIconImageEmpty);

  Image := TIconImage(TSharedIcon(FSharedImage).FImages[FCurrent]);

  if (Image.Width <> ASource.Width)
  or (Image.Height <> ASource.Height)
  then raise EInvalidGraphicOperation.Create(rsIconImageSize);
  
  if Image.PixelFormat <> ASource.PixelFormat
  then raise EInvalidGraphicOperation.Create(rsIconImageFormat);

  UnshareImage(True);
  FreeCanvasContext;
  
  RawImg := ASource.GetRawImagePtr;
  NewImage := TIconImage.Create(Image.PixelFormat, Image.Height, Image.Width);
  try
    NewImage.FImage.Description := RawImg^.Description;

    // image
    NewImage.FImage.DataSize := RawImg^.DataSize;
    if NewImage.FImage.DataSize > 0
    then begin
      NewImage.FImage.Data := GetMem(NewImage.FImage.DataSize);
      Move(RawImg^.Data^, NewImage.FImage.Data^, NewImage.FImage.DataSize);
    end;

    // mask
    // in theory, it should not matter if a HBITMAP was created as bitmap or as mask
    // since there is a description problem in gtk, create both (we cannot create mask only)
    // todo: fix gtk
    if ASource.MaskHandleAllocated
    and RawImage_FromBitmap(RawMsk, ASource.MaskHandle, ASource.MaskHandle)
    then begin
      NewImage.FImage.MaskSize := RawMsk.MaskSize;
      if NewImage.FImage.MaskSize > 0
      then begin
        NewImage.FImage.Mask := GetMem(NewImage.FImage.MaskSize);
        Move(RawMsk.Mask^, NewImage.FImage.Mask^, NewImage.FImage.MaskSize);

        // prevent cleanup
        RawMsk.MaskSize := 0;
        RawMsk.Mask := nil;
      end;
      RawMsk.FreeData;
    end
    else begin
      NewImage.FImage.MaskSize := RawImg^.MaskSize;
      if NewImage.FImage.MaskSize > 0
      then begin
        NewImage.FImage.Mask := GetMem(NewImage.FImage.MaskSize);
        Move(RawImg^.Mask^, NewImage.FImage.Mask^, NewImage.FImage.MaskSize);
      end;
    end;

    // palette
    NewImage.FImage.PaletteSize := RawImg^.PaletteSize;
    if NewImage.FImage.PaletteSize > 0
    then begin
      NewImage.FImage.Palette := GetMem(NewImage.FImage.PaletteSize);
      Move(RawImg^.Palette^, NewImage.FImage.Palette^, NewImage.FImage.PaletteSize);
    end;

    // this cannot be shcanged without adjusting data
    // NewImage.FImage.Description.MaskBitsPerPixel := 1;

    TSharedIcon(FSharedImage).FImages[FCurrent] := NewImage;
    NewImage := nil;
    Image.Free;

  finally
    NewImage.Free;
  end;

  Changed(Self);
end;

procedure TCustomIcon.Clear;
begin
  if not Empty then
  begin
    FreeSaveStream;
    FSharedImage.Release;
    FSharedImage := GetSharedImageClass.Create;
    FSharedImage.Reference;
    FCurrent := -1;
    Changed(Self);
  end;
end;

function TCustomIcon.BitmapHandleAllocated: boolean;
begin
  Result := (FCurrent <> -1) and (TIconImage(TSharedIcon(FSharedImage).FImages[FCurrent]).FHandle <> 0);
end;

constructor TCustomIcon.Create;
begin
  inherited Create;
  FCurrent := -1;
  FRequestedSize := Size(0, 0);
  // per definition an icon is masked, but maybe we should make it settable for alpha images
  FMasked := True;
end;

procedure TCustomIcon.Delete(Aindex: Integer);
begin
  UnshareImage(True);
  TSharedIcon(FSharedImage).Delete(AIndex);
  if FCurrent = AIndex
  then begin
    FCurrent := -1;
    UpdateCurrentView;
  end
  else if FCurrent > AIndex
  then begin
    Dec(FCurrent);
  end;
end;

function TCustomIcon.GetCount: Integer;
begin
  Result := TSharedIcon(FSharedImage).Count;
end;

procedure TCustomIcon.GetDescription(Aindex: Integer; out AFormat: TPixelFormat; out AHeight, AWidth: Word);
var
  Image: TIconImage;
begin
  Image := TIconImage(TSharedIcon(FSharedImage).FImages[Aindex]);
  AFormat := Image.PixelFormat;
  AHeight := Image.Height;
  AWidth := Image.Width;
end;

class function TCustomIcon.GetFileExtensions: string;
begin
  Result:='ico';
end;

function TCustomIcon.GetBitmapHandle: HBITMAP;
begin
  if FCurrent = -1
  then Result := 0
  else begin
    BitmapHandleNeeded;
    Result := TIconImage(TSharedIcon(FSharedImage).FImages[FCurrent]).Handle;
  end;
end;

class function TCustomIcon.GetDefaultSize: TSize;
begin
  Result := Size(GetSystemMetrics(SM_CXICON), GetSystemMetrics(SM_CYICON));
end;

function TCustomIcon.GetIndex(AFormat: TPixelFormat; AHeight, AWidth: Word): Integer;
begin
  Result := TSharedIcon(FSharedImage).GetIndex(AFormat, AHeight, AWidth);
end;

function TCustomIcon.GetMaskHandle: HBITMAP;
begin
  if FCurrent = -1
  then Result := 0
  else begin
    MaskHandleNeeded;
    Result := TIconImage(TSharedIcon(FSharedImage).FImages[FCurrent]).MaskHandle;
  end;
end;

function TCustomIcon.GetPalette: HPALETTE;
begin
  if FCurrent = -1
  then Result := 0
  else begin
    PaletteNeeded;
    Result := TIconImage(TSharedIcon(FSharedImage).FImages[FCurrent]).Palette;
  end;
end;

function TCustomIcon.GetPixelFormat: TPixelFormat;
begin
  if FCurrent = -1
  then Result := pfCustom
  else Result := TIconImage(TSharedIcon(FSharedImage).FImages[FCurrent]).PixelFormat;
end;

function TCustomIcon.GetRawImagePtr: PRawImage;
begin
  if FCurrent = -1
  then Result := nil
  else begin
    TIconImage(TSharedIcon(FSharedImage).FImages[FCurrent]).RawImageNeeded(False);
    Result := @TIconImage(TSharedIcon(FSharedImage).FImages[FCurrent]).FImage;
  end;
end;

function TCustomIcon.GetRawImageDescriptionPtr: PRawImageDescription;
begin
  if FCurrent = -1
  then Result := nil
  else begin
    TIconImage(TSharedIcon(FSharedImage).FImages[FCurrent]).RawImageNeeded(True);
    Result := @TIconImage(TSharedIcon(FSharedImage).FImages[FCurrent]).FImage.Description;
  end;
end;

function TCustomIcon.GetTransparent: Boolean;
begin
  Result := True;
end;

class function TCustomIcon.GetStreamSignature: Cardinal;
begin
  Result := 0;
end;

class function TCustomIcon.GetTypeID: Word;
begin
  Result := 0;
end;

class function TCustomIcon.GetSharedImageClass: TSharedRasterImageClass;
begin
  Result := TSharedIcon;
end;

procedure TCustomIcon.HandleNeeded;
begin
  {$IFDEF VerboseLCLTodos}{$note TODO implement some WSclass call}{$ENDIF}
end;

function TCustomIcon.InternalReleaseBitmapHandle: HBITMAP;
begin
  if FCurrent = -1
  then Result := 0
  else Result := TIconImage(TSharedIcon(FSharedImage).FImages[FCurrent]).ReleaseHandle;
end;

function TCustomIcon.InternalReleaseMaskHandle: HBITMAP;
begin
  if FCurrent = -1
  then Result := 0
  else Result := TIconImage(TSharedIcon(FSharedImage).FImages[FCurrent]).ReleaseMaskHandle;
end;

function TCustomIcon.InternalReleasePalette: HPALETTE;
begin
  if FCurrent = -1
  then Result := 0
  else Result := TIconImage(TSharedIcon(FSharedImage).FImages[FCurrent]).ReleasePalette;
end;

function TCustomIcon.LazarusResourceTypeValid(const ResourceType: string): boolean;
var
  ResType: String;
begin
  if Length(ResourceType) < 3 then Exit(False);

  ResType := UpperCase(ResourceType);
  case ResType[1] of
    'I': begin
      Result := (ResType = 'ICO') or (ResType = 'ICON');
    end;
  else
    Result := inherited LazarusResourceTypeValid(ResType);
  end;
end;

procedure TCustomIcon.LoadFromResourceName(Instance: THandle; const ResName: String);
var
  ResType: TResourceType;
  ResHandle: TFPResourceHandle;
begin
  ResType := GetResourceType;
  if ResType = nil then Exit;

  ResHandle := FindResource(Instance, PChar(ResName), PChar(ResType));
  if ResHandle = 0 then
    raise EResNotFound.Create(Format('[TCustomIcon.LoadFromResourceName] The resource "%s" was not found', [ResName])); // todo: valid exception
  LoadFromResourceHandle(Instance, ResHandle);
end;

procedure TCustomIcon.LoadFromResourceID(Instance: THandle; ResID: PtrInt);
var
  ResType: TResourceType;
  ResHandle: TFPResourceHandle;
begin
  ResType := GetResourceType;
  if ResType = nil then Exit;

  ResHandle := FindResource(Instance, PChar(ResID), PChar(ResType));
  if ResHandle = 0 then
    raise EResNotFound.Create(Format('[TCustomIcon.LoadFromResourceID] The resource #%d was not found', [ResID])); // todo: valid exception
  LoadFromResourceHandle(Instance, ResHandle);
end;

procedure TCustomIcon.LoadFromResourceHandle(Instance: THandle; ResHandle: TFPResourceHandle);
begin
end;

function TCustomIcon.MaskHandleAllocated: boolean;
begin
  Result := (FCurrent <> -1) and (TIconImage(TSharedIcon(FSharedImage).FImages[FCurrent]).FMaskHandle <> 0);
end;

procedure TCustomIcon.MaskHandleNeeded;
var
  ImagePtr: PRawImage;
  ImgHandle, dummy: HBITMAP;
  MaskImage: TRawImage;
begin
  if FCurrent = -1 then Exit;
  if MaskHandleAllocated then exit;

  ImagePtr := GetRawImagePtr;
  if (ImagePtr = nil) or
     (ImagePtr^.Description.Format = ricfNone) or
     (ImagePtr^.Description.MaskBitsPerPixel = 0) then
     Exit;

  MaskImage.Init;
  MaskImage.Description := ImagePtr^.Description.GetDescriptionFromMask;
  MaskImage.DataSize := ImagePtr^.MaskSize;
  MaskImage.Data := ImagePtr^.Mask;

  // CreateCompatibleBitmaps cannot work with empty Data => create dummy data
  if ImagePtr^.Mask = nil then
    MaskImage.CreateData(True);

  if CreateCompatibleBitmaps(MaskImage, ImgHandle, Dummy, True)
  then begin
    if BitmapHandleAllocated
    then UpdateHandles(BitmapHandle, ImgHandle)
    else UpdateHandles(0, ImgHandle);
  end;

  if ImagePtr^.Mask = nil then
    MaskImage.FreeData;
end;

function TCustomIcon.PaletteAllocated: boolean;
begin
  Result := (FCurrent <> -1) and (TIconImage(TSharedIcon(FSharedImage).FImages[FCurrent]).FPalette <> 0);
end;

procedure TCustomIcon.PaletteNeeded;
begin
  // nothing to do, handled by image itself
end;

function TCustomIcon.CanShareImage(AClass: TSharedRasterImageClass): Boolean;
begin
  // temporary hack to make Assign work between cursors, icons and icnsicons
  Result := AClass.InheritsFrom(TSharedIcon);
end;

procedure TCustomIcon.CheckRequestedSize;
begin
  if (FRequestedSize.cx = 0) and (FRequestedSize.cy = 0) then
    FRequestedSize := GetDefaultSize;

  // if someone set only height then set width = height
  if FRequestedSize.cx = 0 then
    FRequestedSize.cx := FRequestedSize.cy;

  // if someone set only width then set height = width
  if FRequestedSize.cy = 0 then
    FRequestedSize.cy := FRequestedSize.cx;
end;

procedure TCustomIcon.ReadData(Stream: TStream);
var
  Signature: array [0..3] of Char;
  Size: longint absolute Signature;
  Position: Int64;
begin
  // Check it the stream is prefixed with a size.
  // Delphi doesn't, while we do.

  Position := Stream.Position;
  Stream.Read(Signature, SizeOf(Signature));
  Stream.Position := Position;
  if Cardinal(Signature) = GetStreamSignature
  then begin
    // Assume Icon - stream without explicit size
    LoadFromStream(Stream);
  end
  else begin
    // use inherited to read, so "old" streams are converted
    inherited;
  end;
end;

procedure TCustomIcon.ReadStream(AStream: TMemoryStream; ASize: Longint);
var
  Header: TIconHeader;
  StreamStart: Int64;
  IconDir: array of TIconDirEntry;
  n: Integer;
  IconImage: TIconImage;
  IntfImage: TLazIntfImage;
  PNGSig: array[0..7] of Byte;
  PNGReader: TLazReaderPNG;
  DIBReader: TLazReaderDIB;
  ImgReader: TFPCustomImageReader;
  LazReader: ILazImageReader;
  RawImg: TRawImage;
begin
  StreamStart := AStream.Position;
  AStream.Read(Header, SizeOf(Header));

  {$ifdef FPC_BIG_ENDIAN}
  // adjust header
  Header.idType := LEtoN(Header.idType);
  Header.idCount := LEtoN(Header.idCount);
  {$endif}
  
  if (Header.idType <> 1) and (Header.idType <> 2)
  then raise EInvalidGraphic.Create('Stream is not an Icon type');
  
  if Header.idCount = 0
  then begin
    AStream.Seek(StreamStart + ASize, soBeginning);
    FCurrent := -1;
    Exit;
  end;

  SetLength(IconDir, Header.idCount);
  AStream.Read(IconDir[0], Header.idCount * SizeOf(IconDir[0]));
  
  PNGReader := nil;
  DIBReader := nil;
  IntfImage := nil;
  try
    for n := 0 to Header.idCount - 1 do
    begin
      {$ifdef FPC_BIG_ENDIAN}
      // adjust entry
      IconDir[n].wXHotSpot := LEtoN(IconDir[n].wXHotSpot);
      IconDir[n].wYHotSpot := LEtoN(IconDir[n].wYHotSpot);
      IconDir[n].dwBytesInRes := LEtoN(IconDir[n].dwBytesInRes);
      IconDir[n].dwImageOffset := LEtoN(IconDir[n].dwImageOffset);
      {$endif}
      
      AStream.Seek(StreamStart + IconDir[n].dwImageOffset, soBeginning);
      
      ImgReader := nil;
      if (IconDir[n].bWidth = 0) or (IconDir[n].bHeight = 0)
      then begin
        // PNG or DIB image
        // Vista icons are PNG in this case, but there exist also "old style" icons
        // with DIB image
        
        // don't use PNGReader.CheckContents(AStream) since it uses internally
        // an exception for checking, which is not "nice" when debugging.
        AStream.Read(PNGSig, SizeOf(PNGSig));
        AStream.Seek(StreamStart + IconDir[n].dwImageOffset, soBeginning);

        if QWord(PNGComn.Signature) = QWord(PNGSig)
        then begin
          if PNGReader = nil
          then PNGReader := TLazReaderPNG.Create;
          ImgReader := PNGReader;
        end;
      end;
      
      if ImgReader = nil
      then begin
        // DIB image
        if DIBReader = nil
        then DIBReader := TLazReaderIconDIB.Create;
        ImgReader := DIBReader;
      end;
      
      // create or reset intfimage
      if IntfImage = nil
      then IntfImage := TLazIntfImage.Create(0,0,[])
      else IntfImage.SetSize(0,0);

      if Supports(ImgReader, ILazImageReader, LazReader) 
      then LazReader.UpdateDescription := True
      else IntfImage.DataDescription := QueryDescription([riqfRGB, riqfAlpha, riqfMask]); // fallback to default
      ImgReader.ImageRead(AStream, IntfImage);

      // Add image
      IntfImage.GetRawImage(RawImg, True);
      // Paul: don't set MaskBitsPerPixel to zero => windows will fail with no mask
      // Even empty mask is better than no mask. But maybe CreateIconIndirect must be fixed on windows?
      RawImg.Description.MaskBitsPerPixel := 1;
      with TSharedIcon(FSharedImage) do
      begin
        IconImage := GetImagesClass.Create(RawImg);
        if IconImage is TCursorImageImage then
          TCursorImageImage(IconImage).HotSpot := Point(IconDir[n].wXHotSpot, IconDir[n].wYHotSpot);
        FImages.Add(IconImage);
      end;
    end;
  finally
    LazReader := nil;
    DIBReader.Free;
    PNGReader.Free;
    IntfImage.Free;
  end;
  // Adjust all entries and find best (atm the order: best width, best height, max depth)
  CheckRequestedSize;
  FCurrent := GetBestIndexForSize(FRequestedSize);
end;

procedure TCustomIcon.Remove(AFormat: TPixelFormat; AHeight, AWidth: Word);
var
  idx: Integer;
begin
  idx := GetIndex(AFormat, AHeight, AWidth);
  if idx <> -1 then Delete(idx);
end;

procedure TCustomIcon.SetCurrent(const AValue: Integer);
begin
  if FCurrent = AValue then exit;
  FCurrent := AValue;
  UpdateCurrentView;
end;

procedure TCustomIcon.SetHandles(ABitmap, AMask: HBITMAP);
begin
  {$IFDEF VerboseLCLTodos}{$note Implement me (or raise exception)}{$ENDIF}
end;

procedure TCustomIcon.SetMasked(AValue: Boolean);
begin
  // nothing
end;

function TCustomIcon.GetBestIndexForSize(ASize: TSize): Integer;
var
  BestDepth, i, dx, dy, dd: Integer;
  CurRawImage: TRawImage;
  ScreenDC: HDC;
begin
  Result := -1;
  
  if ASize.cx <= 0 then
  begin
    ASize.cx := GetSystemMetrics(SM_CXICON);
    if ASize.cx = -1 then
      ASize.cx := 32;
  end;

  if ASize.cy <= 0 then
  begin
    ASize.cy := GetSystemMetrics(SM_CYICON);
    if ASize.cy = -1 then
      ASize.cy := 32;
  end;

  ScreenDC := GetDC(0);
  BestDepth := GetDeviceCaps(ScreenDC, BITSPIXEL);
  ReleaseDC(0, ScreenDC);

  dx := MaxInt;
  dy := MaxInt;
  dd := MaxInt;

  for i := 0 to Count - 1 do
  begin
    CurRawImage := TIconImage(TSharedIcon(FSharedImage).FImages[i]).FImage;
    if Abs(ASize.cx - CurRawImage.Description.Width) < dx then
    begin
      dx := Abs(ASize.cx - CurRawImage.Description.Width);
      Result := i;
    end
    else
    if Abs(ASize.cx - CurRawImage.Description.Width) = dx then
    begin
      if Abs(ASize.cy - CurRawImage.Description.Height) < dy then
      begin
        dy := Abs(ASize.cy - CurRawImage.Description.Height);
        Result := i;
      end
      else
      if Abs(ASize.cy - CurRawImage.Description.Height) = dy then
      begin
        if Abs(BestDepth - CurRawImage.Description.Depth) < dd then
        begin
          dd := Abs(BestDepth - CurRawImage.Description.Depth);
          Result := i;
        end;
      end;
    end
  end;
end;

procedure TCustomIcon.SetPixelFormat(AValue: TPixelFormat);
begin
  raise EInvalidGraphicOperation.Create(rsIconImageFormatChange);
end;

procedure TCustomIcon.SetTransparent(Value: Boolean);
begin
  // nothing
end;

procedure TCustomIcon.SetSize(AWidth, AHeight: integer);
begin
  if FCurrent <> -1
  then raise EInvalidGraphicOperation.Create(rsIconImageSizeChange)
  else FRequestedSize := Size(AWidth, AHeight);
end;

procedure TCustomIcon.UnshareImage(CopyContent: boolean);
var
  NewIcon, OldIcon: TSharedIcon;
  n: Integer;
  OldImage, NewImage: TIconImage;
  OldSharedImage: TSharedImage;
begin
  if FSharedImage.RefCount <= 1 then Exit;

  NewIcon := GetSharedImageClass.Create as TSharedIcon;
  try
    NewIcon.Reference;
    if CopyContent
    then begin
      OldIcon := FSharedImage as TSharedIcon;
      for n := 0 to OldIcon.FImages.Count -1 do
      begin
        OldImage := TIconImage(OldIcon.FImages[n]);
        NewImage := TIconImage.Create(OldImage.PixelFormat, OldImage.Height, OldImage.Width);
        NewIcon.FImages.Add(NewImage);
        NewImage.FImage.Description := OldImage.FImage.Description;
        NewImage.FImage.DataSize := OldImage.FImage.DataSize;
        if NewImage.FImage.DataSize > 0
        then begin
          NewImage.FImage.Data := GetMem(NewImage.FImage.DataSize);
          Move(OldImage.FImage.Data^, NewImage.FImage.Data^, NewImage.FImage.DataSize);
        end;
        NewImage.FImage.MaskSize := OldImage.FImage.MaskSize;
        if NewImage.FImage.MaskSize > 0
        then begin
          NewImage.FImage.Mask := GetMem(NewImage.FImage.MaskSize);
          Move(OldImage.FImage.Mask^, NewImage.FImage.Mask^, NewImage.FImage.MaskSize);
        end;
        NewImage.FImage.PaletteSize := OldImage.FImage.PaletteSize;
        if NewImage.FImage.PaletteSize > 0
        then begin
          NewImage.FImage.Palette := GetMem(NewImage.FImage.PaletteSize);
          Move(OldImage.FImage.Palette^, NewImage.FImage.Palette^, NewImage.FImage.PaletteSize);
        end;
      end;
    end;
    FreeCanvasContext;
    OldSharedImage := FSharedImage;
    FSharedImage := NewIcon;

    NewIcon := nil; // transaction sucessful
    OldSharedImage.Release;
  finally
    // in case something goes wrong, keep old and free new
    NewIcon.Free;
  end;
end;

procedure TCustomIcon.UpdateCurrentView;
begin
  FreeCanvasContext;
  Changed(Self);
end;

procedure TCustomIcon.SetHandle(AValue: THandle);
begin
  if FSharedImage.FHandle <> AValue
  then begin
    // if the handle is set externally we should unshare ourselves
    FreeCanvasContext;
    UnshareImage(false);
    FreeSaveStream;
    TSharedIcon(FSharedImage).Clear;
  end;

  if UpdateHandle(AValue)
  then begin
    if (TSharedIcon(FSharedImage).Count > 0) then
      FCurrent := 0
    else
      FCurrent := -1;
    Changed(Self);
  end;
end;

function TCustomIcon.UpdateHandle(AValue: HICON): Boolean;
begin
  Result := FSharedImage.FHandle <> AValue;
  if Result then
    TSharedIcon(FSharedImage).UpdateFromHandle(AValue);
end;

function TCustomIcon.UpdateHandles(ABitmap, AMask: HBITMAP): Boolean;
var
  Image: TIconImage;
begin
  if FCurrent = -1
  then begin
    Result := False;
    Exit;
  end;
  
  Image := TIconImage(TSharedIcon(FSharedImage).FImages[FCurrent]);
  Result := Image.UpdateHandles(ABitmap, AMask);
end;

procedure TCustomIcon.WriteStream(AStream: TMemoryStream);
  procedure GetMaskData(ARawImg: TRawImage; AIconImage: TIconImage; AMskPtr: Pointer; AMskSize: Cardinal);
  var
    SrcRawImg, DstRawImg: TRawImage;
    SrcDesc: TRawImageDescription absolute SrcRawImg.Description;
    DstDesc: TRawImageDescription absolute DstRawImg.Description;
    SrcImage, DstImage: TLazIntfImage;
    NeedFreeData: Boolean;
  begin
    NeedFreeData := True;
    if (AIconImage.MaskHandle = 0)
    or not RawImage_FromBitmap(SrcRawImg, AIconImage.MaskHandle, 0)
    then begin
      SrcRawImg.Init;
      SrcRawImg.Description := ARawImg.Description.GetDescriptionFromMask;
      SrcRawImg.Data := ARawImg.Mask;
      SrcRawImg.DataSize := ARawImg.MaskSize;
      NeedFreeData := False;
    end;

    DstRawImg.Init;
    DstRawImg.Data := AMskPtr;
    DstRawImg.DataSize := AMskSize;

    DstDesc.Format := ricfGray;
    DstDesc.Width := AIconImage.Width;
    DstDesc.Height := AIconImage.Height;
    DstDesc.Depth := 1;
    DstDesc.BitOrder := riboReversedBits;
    DstDesc.ByteOrder := riboLSBFirst;
    DstDesc.LineOrder := riloBottomToTop;
    DstDesc.LineEnd := rileDWordBoundary;
    DstDesc.BitsPerPixel := 1;
    DstDesc.RedPrec := 1;
    DstDesc.RedShift := 0;

    if SrcDesc.IsEqual(DstDesc)
    then begin
      Move(ARawImg.Mask^, AMskPtr^, ARawImg.MaskSize);
      Exit;
    end;

    SrcImage := TLazIntfImage.Create(SrcRawImg, False);
    DstImage := TLazIntfImage.Create(DstRawImg, False);
    DstImage.CopyPixels(SrcImage);

    SrcImage.Free;
    DstImage.Free;
    if NeedFreeData then
      SrcRawImg.FreeData;
  end;

var
  Header: TIconHeader;
  StreamStart: Int64;
  IconDir: array of TIconDirEntry;
  n: Integer;
  ImageCount: Word;
  IconImage: TIconImage;
  IntfImage: TLazIntfImage;
  PNGWriter: TFPWriterPNG;
  BMPWriter: TFPWriterBMP;
  BmpPtr: PByte;
  MskPtr: PByte;
  MskSize: Cardinal;
  MemStream: TMemoryStream;
  RawImg: TRawImage;
begin
  ImageCount := TSharedIcon(FSharedImage).Count;
  StreamStart := AStream.Position;

  Header.idReserved := 0;
  Header.idType := NtoLE(GetTypeID);
  Header.idCount := LEtoN(ImageCount);
  AStream.Write(Header, SizeOf(Header));

  if ImageCount = 0 then Exit;

  SetLength(IconDir, ImageCount);
  FillChar(IconDir[0], ImageCount * SizeOf(IconDir[0]), 0);

  // write empty dirlist first, so the images can be written after it.
  // we'll update it later
  AStream.Write(IconDir[0], ImageCount * SizeOf(IconDir[0]));

  PNGWriter := nil;
  BMPWriter := nil;
  MemStream := nil;
  IntfImage := nil;
  try
    for n := 0 to ImageCount - 1 do
    begin
      IconImage := TIconImage(TSharedIcon(FSharedImage).FImages[n]);
      RawImg := IconImage.FImage;

      // set offset
      IconDir[n].dwImageOffset := NtoLE(DWord(AStream.Position - StreamStart));

      // create or reset intfimage
      if IntfImage = nil
      then IntfImage := TLazIntfImage.Create(RawImg, False)
      else IntfImage.SetRawImage(RawImg, False);

      // user temp mem stream for storage.
      if MemStream = nil
      then MemStream := TMemoryStream.Create
      else MemStream.Position := 0;

      // write image data
      if (IconImage.Width >= 255) or (IconImage.Height >= 255)
      then begin
        // PNG or DIB image
        // Vista icons are PNG in this case, but there exist also "old style" icons
        // with DIB image, we use PNG
        // (dir.width and dir.height stay 0 in this case)

        if PNGWriter = nil
        then begin
          PNGWriter := TFPWriterPNG.Create;
          PNGWriter.Indexed := False;
          PNGWriter.WordSized := False;
        end;
        PNGWriter.GrayScale := RawImg.Description.Format = ricfGray;
        PNGWriter.UseAlpha  := RawImg.Description.AlphaPrec > 0;
        PNGWriter.ImageWrite(MemStream, IntfImage);

        IconDir[n].wBpp := NtoLE(Word(RawImg.Description.BitsPerPixel));
        IconDir[n].dwBytesInRes := NtoLE(DWord(MemStream.Position));

        MemStream.SaveToStream(AStream);
      end
      else begin
        // DIB image
        IconDir[n].bHeight := IconImage.Height;
        IconDir[n].bWidth := IconImage.Width;

        // since there is no DIB writer, write a BMP to a temp stream and skip the file header

        if BMPWriter = nil
        then begin
          BMPWriter := TFPWriterBMP.Create;
          BMPWriter.RLECompress := False;
        end;
        case IconImage.PixelFormat of
          pfDevice: BMPWriter.BitsPerPixel := QueryDescription([riqfRGB]).BitsPerPixel;
          pfCustom: BMPWriter.BitsPerPixel := RawImg.Description.BitsPerPixel;
        else
          BMPWriter.BitsPerPixel := PIXELFORMAT_BPP[IconImage.PixelFormat];
        end;

        BMPWriter.ImageWrite(MemStream, IntfImage);
        // adjust BMP data so it is a IconDIB
        BmpPtr := PByte(MemStream.Memory) + SizeOf(TBitMapFileHeader);
        // double the height to accommodate the mask
        PBitMapInfoHeader(BmpPtr)^.biHeight := NtoLE(LEtoN(PBitMapInfoHeader(BmpPtr)^.biHeight) * 2);

        // write mask.
        // align to dword
        MskSize := (((IconImage.Width + 31) shr 5) shl 2) * IconImage.Height;
        // alloc "buffer"
        if MemStream.Size < MemStream.Position + MskSize
        then begin
          MemStream.Size := MemStream.Position + MskSize;
          // reallocation, recalculate bmpptr
          BmpPtr := PByte(MemStream.Memory) + SizeOf(TBitMapFileHeader);
        end;
        MskPtr := PByte(MemStream.Memory) + MemStream.Position;
        MemStream.Seek(MskSize, soCurrent);

        if (RawImg.Mask = nil)
        or (RawImg.MaskSize = 0)
        then FillChar(MskPtr^, MskSize, 0)
        else GetMaskData(RawImg, IconImage, MskPtr, MskSize);

        // write
        AStream.WriteBuffer(BmpPtr^, MemStream.Position - SizeOf(TBitMapFileHeader));

        IconDir[n].dwBytesInRes := NtoLE(DWord(MemStream.Position - SizeOf(TBitMapFileHeader)));
        IconDir[n].wBpp := NtoLE(Word(BMPWriter.BitsPerPixel));
      end;


      if IconImage is TCursorImageImage
      then begin
        IconDir[n].wXHotSpot := NtoLE(Word(TCursorImageImage(IconImage).HotSpot.X));
        IconDir[n].wYHotSpot := NtoLE(Word(TCursorImageImage(IconImage).HotSpot.Y));
      end
      else begin
        IconDir[n].wPlanes := NtoLE(Word(1));
      end;
    end;
  finally
    PNGWriter.Free;
    BMPWriter.Free;
    MemStream.Free;
    IntfImage.Free;
  end;

  // update directory
  AStream.Seek(StreamStart + SizeOf(Header), soBeginning);
  AStream.Write(IconDir[0], ImageCount * SizeOf(IconDir[0]));
end;

////////////////////////////////////////////////////////////////////////////////

{ TIcon }

function TIcon.GetIconHandle: HICON;
begin
  Result := GetHandle;
end;

class function TIcon.GetTypeID: Word;
begin
  Result := 1; //icon
end;

function TIcon.ReleaseHandle: HICON;
// simply return the current handle and set to 0 without freeing handles
begin
  HandleNeeded;
  Result := FSharedImage.ReleaseHandle;
end;

function TIcon.GetResourceType: TResourceType;
begin
  Result := RT_GROUP_ICON;
end;

procedure TIcon.SetIconHandle(const AValue: HICON);
begin
  SetHandle(AValue);
end;

class function TIcon.GetStreamSignature: Cardinal;
begin
  Result := Cardinal(IconSignature);
end;

procedure TIcon.HandleNeeded;
var
  IconInfo: TIconInfo;
begin
  if FSharedImage.FHandle <> 0 then Exit;

  IconInfo.fIcon := True;
  IconInfo.hbmColor := BitmapHandle;
  IconInfo.hbmMask := MaskHandle;
  FSharedImage.FHandle := WidgetSet.CreateIconIndirect(@IconInfo);
end;

procedure TIcon.LoadFromResourceHandle(Instance: THandle; ResHandle: TFPResourceHandle);
var
  GlobalHandle: TFPResourceHGlobal;
  Dir: PNewHeader;
  DirEntry: PGrpIconDirEntry;
  IconEntry: TIconDirEntry;
  i, offset: integer;
  Stream: TMemoryStream;
  IconStream: TResourceStream;
begin
  // build a usual ico stream using several RT_ICON resources
  GlobalHandle := LoadResource(Instance, ResHandle);
  if GlobalHandle = 0 then
    Exit;
  Dir := LockResource(GlobalHandle);
  if Dir = nil then
    Exit;

  Stream := TMemoryStream.Create;
  try
    // write icon header
    Stream.Write(Dir^, SizeOf(TIconHeader));
    // write icon entries headers
    offset := Stream.Position + SizeOf(IconEntry) * LEtoN(Dir^.idCount);
    DirEntry := PGrpIconDirEntry(PChar(Dir) + SizeOf(Dir^));
    for i := 0 to LEtoN(Dir^.idCount) - 1 do
    begin
      Move(DirEntry^, IconEntry, SizeOf(DirEntry^));
      IconEntry.dwImageOffset := NtoLE(offset);
      inc(offset, LEtoN(IconEntry.dwBytesInRes));
      Stream.Write(IconEntry, SizeOf(IconEntry));
      Inc(DirEntry);
    end;
    // write icons data
    DirEntry := PGrpIconDirEntry(PChar(Dir) + SizeOf(Dir^));
    for i := 0 to LEtoN(Dir^.idCount) - 1 do
    begin
      IconStream := TResourceStream.CreateFromID(Instance, LEtoN(DirEntry^.nID), RT_ICON);
      try
        Stream.CopyFrom(IconStream, IconStream.Size);
      finally
        IconStream.Free;
      end;
      Inc(DirEntry);
    end;
    Stream.Position := 0;
    ReadData(Stream);
  finally
    Stream.Free;
    UnLockResource(GlobalHandle);
    FreeResource(GlobalHandle);
  end;
end;


