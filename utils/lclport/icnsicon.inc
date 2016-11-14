{%MainUnit ../graphics.pp}

{******************************************************************************
                               TicnsIcon
 ******************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}

{ TIcnsList }

function TIcnsList.GetItem(Index: Integer): PIcnsRec;
begin
  Result := inherited Get(Index);
end;

procedure TIcnsList.SetItem(Index: Integer; const AValue: PIcnsRec);
begin
  inherited Put(Index, AValue);
end;

procedure TIcnsList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if Action = lnDeleted then
    Dispose(PIcnsRec(Ptr));
  inherited Notify(Ptr, Action);
end;

function TIcnsList.Add(AIconType: TicnsIconType; ARawImage: TRawImage): Integer;
var
  Rec: PIcnsRec;
begin
  New(Rec);
  Rec^.IconType := AIconType;
  Rec^.RawImage := ARawImage;
  Result := inherited Add(Rec);
end;

{ TIcnsIcon }

procedure TIcnsIcon.IcnsAdd(AIconType: TicnsIconType; ARawImage: TRawImage);

  function GetMaskList: TIcnsList;
  begin
    if FMaskList = nil then
      FMaskList := TIcnsList.Create;
    Result := FMaskList;
  end;

  function GetImageList: TIcnsList;
  begin
    if FImageList = nil then
      FImageList := TIcnsList.Create;
    Result := FImageList;
  end;

begin
  if AIconType in icnsMaskTypes
  then GetMaskList.Add(AIconType, ARawImage)
  else GetImageList.Add(AIconType, ARawImage);
end;

procedure TIcnsIcon.IcnsProcess;

  procedure MergeMask(var AImage, AMask: TRawImage);
  var
    LazIntfImage, LazIntfMask: TLazIntfImage;
    Col, Row: Integer;
    Color: TFpColor;
  begin
    if AMask.Description.Depth = 1 then
    begin
      // merge mask
      AImage.Description.MaskBitOrder := AMask.Description.MaskBitOrder;
      AImage.Description.MaskLineEnd := AMask.Description.MaskLineEnd;
      AImage.Description.MaskBitsPerPixel := AMask.Description.MaskBitsPerPixel;
      AImage.Description.MaskShift := AMask.Description.MaskShift;
      AImage.MaskSize := AMask.MaskSize;
      AImage.Mask := ReallocMem(AImage.Mask, AMask.MaskSize);
      Move(AMask.Mask^, AImage.Mask^, AMask.MaskSize);
    end
    else
    begin
      LazIntfImage := TLazIntfImage.Create(AImage, False);
      LazIntfMask := TLazIntfImage.Create(AMask, False);
      for Row := 0 to LazIntfImage.Height - 1 do
        for Col := 0 to LazIntfImage.Width - 1 do
        begin
          Color := LazIntfImage.Colors[Col,Row];
          Color.alpha := LazIntfMask.Colors[Col,Row].alpha;
          LazIntfImage.Colors[Col,Row] := Color;
        end;
      LazIntfMask.Free;
      LazIntfImage.Free;
    end;
  end;

var
  i, AIndex: integer;
  ImagesForMask: TicnsIconTypes;
  IconImage: TIconImage;
begin
  // merge separate image and masc rawdata together

  if FMaskList <> nil then
  begin
    for i := 0 to FMaskList.Count - 1 do
    begin
      ImagesForMask := icnsMaskToImageMap[FMaskList[i]^.IconType];
      for AIndex := 0 to FImageList.Count - 1 do
        if FImageList[AIndex]^.IconType in ImagesForMask then
          MergeMask(FImageList[AIndex]^.RawImage, FMaskList[i]^.RawImage);
      // dispose RawImage since no more needed
      FMaskList[i]^.RawImage.FreeData;
    end;
    FreeAndNil(FMaskList);
  end;

  for i := 0 to FImageList.Count - 1 do
  begin
    if FImageList[i]^.IconType in icnsWithAlpha then
    begin
      // todo: we have no jpeg 2000 reader to decompress their data => skip for now
      FImageList[i]^.RawImage.FreeData;
      Continue;
    end;

    // Add image
    with TSharedIcon(FSharedImage) do
    begin
      IconImage := GetImagesClass.Create(FImageList[i]^.RawImage);
      Add(IconImage);
    end;
  end;
  FreeAndNil(FImageList);
  CheckRequestedSize;
  FCurrent := GetBestIndexForSize(FRequestedSize);
end;

class function TIcnsIcon.GetSharedImageClass: TSharedRasterImageClass;
begin
  Result := TSharedIcnsIcon;
end;

constructor TIcnsIcon.Create;
begin
  inherited Create;
  FImageList := nil;
  FMaskList := nil;
end;

destructor TIcnsIcon.Destroy;
begin
  inherited Destroy;
  FImageList.Free;
  FMaskList.Free;
end;

procedure TIcnsIcon.ReadData(Stream: TStream);
var
  Resource: TIconFamilyResource;
  Position: Int64;
begin
  Position := Stream.Position;
  Stream.Read(Resource, SizeOf(Resource));
  if Resource.resourceType = kIconFamilyType then
  begin
    Stream.Position := Position;
    LoadFromStream(Stream, BEtoN(Resource.resourceSize))
  end else
  begin
    Stream.Position := Position;
    LoadFromStream(Stream);
  end;
end;

procedure TIcnsIcon.ReadStream(AStream: TMemoryStream; ASize: Longint);
var
  Resource: TIconFamilyResource;

  IntfImage: TLazIntfImage;
  ImgReader: TLazReaderIcnsPart;
  LazReader: ILazImageReader;
  RawImg: TRawImage;
begin
  AStream.Read(Resource, SizeOf(Resource));

  if (Resource.resourceType <> kIconFamilyType) then
    raise EInvalidGraphic.Create('Stream is not an ICNS type');

  IntfImage := nil;
  ImgReader := nil;

  Resource.resourceSize := BEtoN(Resource.resourceSize);

  if ASize > Resource.resourceSize then
    ASize := Resource.resourceSize;

  while AStream.Position < ASize do
  begin
    if IntfImage = nil
    then IntfImage := TLazIntfImage.Create(0,0,[])
    else IntfImage.SetSize(0,0);

    if ImgReader = nil
    then ImgReader := TLazReaderIcnsPart.Create;

    if Supports(ImgReader, ILazImageReader, LazReader)
    then LazReader.UpdateDescription := True
    else IntfImage.DataDescription := QueryDescription([riqfRGB, riqfAlpha, riqfMask]); // fallback to default

    ImgReader.ImageRead(AStream, IntfImage);
    IntfImage.GetRawImage(RawImg, True);

    IcnsAdd(ImgReader.IconType, RawImg);
  end;

  LazReader := nil;
  IntfImage.Free;
  ImgReader.Free;

  IcnsProcess;
end;


// only 24 bit RGB is RLE encoded the same way as TIFF or TGA RLE
// data is encoded channel by channel:
// high bit = 0 => length = low 0..6 bits + 1; read length times next value
// high bit = 1 => length = value - 125      ; read one value and repeat length times

function EncodeTiffRLE(const Src: array of byte; Offset, Count: Integer; var Dst: array of Byte; DstOffset: Integer): Integer;
var
  cnt   : Integer;
  i,j,k : Integer;
  d     : Integer;
  last  : Integer;
const
  LenRLEOffset = 125;
  MaxRLEDiff   = 255-LenRLEOffset;

begin
  if Count = 0 then 
  begin
    Result := 0;
    Exit;
  end;
  i := Offset;
  d := DstOffset;
  last := Offset+Count;

  while i < last do 
  begin
    if (i < last-2) and (Src[i] = Src[i+1]) and (Src[i]=Src[i+2]) then 
    begin
      j := i;
      inc(i);
      while (i < last) and (Src[i] = Src[i-1]) do 
        inc(i);
      cnt := i - j;
      while cnt > 0 do 
      begin
        k := Min(MaxRLEDiff, cnt);
        if k > 2 then 
        begin
          Dst[d] := byte(k+LenRLEOffset); inc(d);
          Dst[d] := Src[j]; inc(d);
          dec(cnt, k);
        end 
        else 
        begin
          dec(i,k);
          cnt := 0;
          k := 0;
        end;
      end;

    end 
    else 
    begin
      j := i;
      if (i < last-1) and (Src[i] = Src[i+1]) then 
        inc(i);

      if last-i > 2 then 
      begin
        inc(i);
        while (i < last) and (Src[i] <> Src[i-1]) do inc(i);
        if i < last then dec(i);
      end 
      else
        i := last;

      cnt := i - j;
      while cnt > 0 do 
      begin
        k := Min(128, cnt);
        Dst[d] := k-1;
        inc(d);
        Move(Src[j], Dst[d], k);
        inc(j, k);
        inc(d, k);
        dec(cnt, k);
      end;
    end;
  end;

  Result := d - DstOffset;
end;

{ !!! WARNING !!! the following code might be INTEL ONLY! Needs to be tested on PowerPC }
function CompressRGBImage(RGBAImage: TLazIntfImage; Stream: TStream): Int64;
var
  src : array of byte;
  dst : array of byte;
  i   : Integer;
  raw : TRawImage;
  sz  : Integer;
  pb  : PByteArray;
  k   : Integer;
  j   : Integer;
  StreamPos : Int64;  
begin
  StreamPos := Stream.Position;
  sz :=  RGBAImage.Width*RGBAImage.Height;
  SetLength(src, sz);
  SetLength(dst, sz*2);
  RGBAImage.GetRawImage(raw, false);
  pb:=PByteArray(raw.Data);

  { red , green, blue values are in separate RLE blocks } 
  for i := 0 to 2 do
  begin
    k:=i+1; {the first element is Alpha, skip it}
    for j := 0 to sz - 1 do 
    begin
      src[j]:=pb^[k];
      inc(k,4);
    end;
    k := EncodeTiffRLE(src, 0, sz, dst, 0);
    Stream.Write(dst[0], k);
  end;
  
  Result := Stream.Position-StreamPos;
  Stream.Position:=StreamPos;
end;

{ !!! WARNING !!! the following code might be INTEL ONLY! Needs to be tested on PowerPC }
function CompressMaskImage(RGBAImage: TLazIntfImage; Stream: TStream): Int64;
var
  src : array of Byte;
  i   : Integer;
  arr : PIntegerArray;
  row : TRawImage;
  StreamPos : Int64;
begin
  StreamPos := Stream.Position;
  SetLength(src, RGBAImage.Width*RGBAImage.Height);
  RGBAImage.GetRawImage(row, false);
  arr := PIntegerArray(row.Data);

  for i := 0 to length(src) - 1 do 
    //src[i] := byte((arr^[i] shr 24) and $FF);
    src[i] := byte( arr^[i] and $FF);
    //src[i]:=255;
  Stream.Write(src[0], length(src));
  
  Result := Stream.Position-StreamPos;
  Stream.Position:=StreamPos;
end;

procedure TIcnsIcon.WriteStream(AStream: TMemoryStream);
var
  mem         : array [0..63] of TMemoryStream;
  icnType     : TicnsIconType;
  id          : array [0..63] of FourCharCode;
  FCode       : FourCharCode;
  el          : TIconFamilyElement;
  i, j, n     : integer;
  totalsz     : LongWord;
  ImageCount  : Integer;
  RawImg      : TRawImage;
  IconImage   : TIconImage;

  IntfImage   : TLazIntfImage;
  SrcImage    : TLazIntfImage;
begin
  ImageCount := TSharedIcon(FSharedImage).Count;
  if ImageCount = 0 then Exit;

  IntfImage:=nil;
  j := 0;
  System.FillChar(mem, sizeof(mem), 0);

  for n := 0 to ImageCount - 1 do
  begin
    IconImage := TIconImage(TSharedIcon(FSharedImage).FImages[n]);
    icnType := GetDataTypeRGB(IconImage.Width, IconImage.Height, FCode);
    
    if icnType = iitNone then Continue; {image is improper size. Skip it}
    
    IconImage.RawImageNeeded(false);
    RawImg := IconImage.FImage;
    IntfImage := TLazIntfImage.Create(IconImage.Width, IconImage.Height, [riqfRGB, riqfAlpha, riqfUpdate]);
    IntfImage.CreateData;
    try
      SrcImage := TLazIntfImage.Create(RawImg, False);    
      try
        IntfImage.CopyPixels( SrcImage, 0,0, true);
      finally
        SrcImage.Free;
      end;

      // write image data
      if (IconImage.Width >= 256)
      then begin
        // todo: Jpeg2000
      end
      else begin
        { compressing RGB data value }
        id[j] := FCode;
        mem[j]:= TMemoryStream.Create;

        // Apple bug? preceding 4 zero-bytes is required for 128x128 icon
        if IconImage.Width = 128 then mem[j].WriteDWord(0);
        CompressRGBImage(IntfImage, mem[j]);
        mem[j].Position:=0;
        inc(j);
        
        { compressing Mask data value }
        GetMaskType8bit(IconImage.Height, IconImage.Width, id[j]);
        mem[j]:=TMemoryStream.Create;           
        CompressMaskImage(IntfImage, mem[j]);
        inc(j);
      end;
    finally
      IntfImage.Free;
    end;
  end;
  
  if j = 0 then Exit; {no images to write}

  totalsz := sizeof(TIconFamilyElement);
  for i := 0 to j - 1 do
    inc(totalsz, mem[i].Size + sizeof(TIconFamilyElement));

  el.elementType := kIconFamilyType;
  el.elementSize := BEtoN(totalsz); {sizes are big-endian}
  AStream.Write(el, sizeof(el));

  for i := 0 to j - 1 do begin
    el.elementType := id[i];
    el.elementSize := BEtoN( LongWord(mem[i].Size + sizeof(TIconFamilyElement)) ); {sizes are big-endian }
    AStream.Write( el, sizeof(el) );
    AStream.CopyFrom(mem[i], mem[i].Size)
  end;

  for i := 0 to j - 1 do mem[i].Free;
end;

class function TIcnsIcon.GetFileExtensions: string;
begin
  Result := 'icns';
end;

function TIcnsIcon.LazarusResourceTypeValid(const ResourceType: string): boolean;
begin
  Result := (UpperCase(ResourceType) = 'ICNS');
end;
