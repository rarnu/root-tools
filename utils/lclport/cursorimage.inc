{%MainUnit ../graphics.pp}

{******************************************************************************
                             TCursorImage
 ******************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
const
  CursorSignature: array [0..3] of char = #0#0#2#0;

function TestStreamIsCursor(const AStream: TStream): boolean;
var
  Signature: array[0..3] of char;
  ReadSize: Integer;
  OldPosition: TStreamSeekType;
begin
  OldPosition:=AStream.Position;
  ReadSize:=AStream.Read(Signature, SizeOf(Signature));
  Result:=(ReadSize=SizeOf(Signature)) and CompareMem(@Signature,@CursorSignature,4);
  AStream.Position:=OldPosition;
end;

{ TSharedCursorImage }

procedure TSharedCursorImage.FreeHandle;
begin
  if FHandle = 0 then Exit;

  DestroyCursor(FHandle);
  FHandle := 0;
end;

class function TSharedCursorImage.GetImagesClass: TIconImageClass;
begin
  Result := TCursorImageImage;
end;

////////////////////////////////////////////////////////////////////////////////

{ TCursorImage }

class function TCursorImage.GetFileExtensions: string;
begin
  Result := 'cur';
end;

function TCursorImage.GetResourceType: TResourceType;
begin
  Result := RT_GROUP_CURSOR;
end;

procedure TCursorImage.LoadFromResourceHandle(Instance: THandle; ResHandle: TFPResourceHandle);
var
  GlobalHandle: TFPResourceHGlobal;
  Dir: PNewHeader;
  DirEntry: PGrpCursorDirEntry;
  IconEntry: TIconDirEntry;
  LocalHeader: TLocalHeader;
  i, offset: integer;
  Stream: TMemoryStream;
  CursorStream: TResourceStream;
  ResourceStreams: TObjectList;
begin
  // build a usual cur stream using several RT_CURSOR resources
  GlobalHandle := LoadResource(Instance, ResHandle);
  if GlobalHandle = 0 then
    Exit;
  Dir := LockResource(GlobalHandle);
  if Dir = nil then
    Exit;

  Stream := TMemoryStream.Create;
  try
    // write cursor header
    Stream.Write(Dir^, SizeOf(TIconHeader));
    // write cursor entries headers
    ResourceStreams := TObjectList.Create(True);
    try
      offset := Stream.Position + SizeOf(IconEntry) * LEtoN(Dir^.idCount);
      DirEntry := PGrpCursorDirEntry(PChar(Dir) + SizeOf(Dir^));
      for i := 0 to LEtoN(Dir^.idCount) - 1 do
      begin
        CursorStream := TResourceStream.CreateFromID(Instance, LEtoN(DirEntry^.nID), RT_CURSOR);
        ResourceStreams.Add(CursorStream);

        // hot spots are stored in the local header of cursor
        CursorStream.Read(LocalHeader, SizeOf(LocalHeader));

        IconEntry.bWidth := DirEntry^.wWidth;
        IconEntry.bHeight := DirEntry^.wHeight div 2; // in cursor resource the height is doubled
        IconEntry.bColorCount := 0;
        IconEntry.bReserved := 0;
        IconEntry.wXHotSpot := LocalHeader.xHotSpot;
        IconEntry.wYHotSpot := LocalHeader.yHotSpot;
        IconEntry.dwImageOffset := NtoLE(offset);
        IconEntry.dwBytesInRes := DirEntry^.dwBytesInRes - SizeOf(LocalHeader);
        inc(offset, LEtoN(IconEntry.dwBytesInRes));
        Stream.Write(IconEntry, SizeOf(IconEntry));
        Inc(DirEntry);
      end;
      // write cursors data
      for i := 0 to ResourceStreams.Count - 1 do
      begin
        CursorStream := TResourceStream(ResourceStreams[i]);
        Stream.CopyFrom(CursorStream, CursorStream.Size - CursorStream.Position);
      end;
    finally
      ResourceStreams.Free;
    end;
    Stream.Position := 0;
    ReadData(Stream);
  finally
    Stream.Free;
    UnLockResource(GlobalHandle);
    FreeResource(GlobalHandle);
  end;
end;

class function TCursorImage.GetSharedImageClass: TSharedRasterImageClass;
begin
  Result := TSharedCursorImage;
end;

class function TCursorImage.GetTypeID: Word;
begin
  Result := 2;
end;

function TCursorImage.LazarusResourceTypeValid(const ResourceType: string): boolean;
var
  ResType: String;
begin
  if Length(ResourceType) < 3 then Exit(False);

  ResType := UpperCase(ResourceType);
  case ResType[1] of
    'C': begin
      Result := (ResType = 'CUR') or (ResType = 'CURSOR');
    end;
  else
    Result := inherited LazarusResourceTypeValid(ResType);
  end;
end;

function TCursorImage.ReleaseHandle: HCURSOR;
begin
  HandleNeeded;
  Result := FSharedImage.ReleaseHandle;
end;

function TCursorImage.GetCursorHandle: HCURSOR;
begin
  Result := GetHandle;
end;

procedure TCursorImage.SetCursorHandle(AValue: HCURSOR);
begin
  SetHandle(AValue);
end;

function TCursorImage.GetHotSpot: TPoint;
begin
  if FCurrent = -1
  then Result := Point(0, 0)
  else Result := TCursorImageImage(TSharedCursorImage(FSharedImage).FImages[FCurrent]).HotSpot;
end;

procedure TCursorImage.SetHotSpot(const P: TPoint);
begin
  if FCurrent >= 0 then
    TCursorImageImage(TSharedCursorImage(FSharedImage).FImages[FCurrent]).HotSpot := P;
end;

procedure TCursorImage.SetCenterHotSpot;
var
  AImage: TCursorImageImage;
begin
  if FCurrent >= 0 then
  begin
    AImage := TCursorImageImage(TSharedCursorImage(FSharedImage).FImages[FCurrent]);
    AImage.HotSpot := Point(AImage.Width div 2, AImage.Height div 2);
  end;
end;

procedure TCursorImage.HandleNeeded;
var
  IconInfo: TIconInfo;
  h: TPoint;
begin
  if FSharedImage.FHandle <> 0 then Exit;

  IconInfo.fIcon := False;
  H := HotSpot;
  IconInfo.xHotspot := H.X;
  IconInfo.yHotSpot := H.Y;
  IconInfo.hbmMask := MaskHandle;
  IconInfo.hbmColor := BitmapHandle;
  FSharedImage.FHandle := WidgetSet.CreateIconIndirect(@IconInfo);
end;

class function TCursorImage.GetDefaultSize: TSize;
begin
  Result := Size(GetSystemMetrics(SM_CXCURSOR), GetSystemMetrics(SM_CYCURSOR));
end;

class function TCursorImage.GetStreamSignature: Cardinal;
begin
  Result := Cardinal(CursorSignature);
end;


