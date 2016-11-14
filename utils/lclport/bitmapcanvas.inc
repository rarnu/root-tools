{%MainUnit ../graphics.pp}

{******************************************************************************
                                  TBitmapCanvas
 ******************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
{------------------------------------------------------------------------------
  Method: TBitmapCanvas.Create
  Params:  ABitMap: the owner of the class
  Returns: Nothing

  Constructor for the class.
 ------------------------------------------------------------------------------}
constructor TBitmapCanvas.Create(AImage: TRasterImage);
begin
  inherited Create;
  // set FImage after inherited create to make sure that the inherited Create
  // won't trigger a call to FImage
  FImage := AImage;
end;

{------------------------------------------------------------------------------
  Method:  TBitmapCanvas.CreateHandle
  Params:  None
  Returns: Nothing

  Creates the handle.
 ------------------------------------------------------------------------------}
procedure TBitmapCanvas.CreateHandle;
var
  DC: HDC;
begin
  if HandleAllocated then exit;
  if FImage = nil then exit;
  
  FImage.BitmapHandleNeeded;
  FImage.PaletteNeeded;
  if FImage.FSharedImage.BitmapCanvas <> nil then
    (FImage.FSharedImage.BitmapCanvas as TBitmapCanvas).FreeDC;
  FImage.FSharedImage.BitmapCanvas := Self;
  DC := CreateCompatibleDC(0);

  if FImage.BitmapHandleAllocated
  then FOldBitmap := SelectObject(DC, FImage.BitmapHandle)
  else FOldBitmap := 0;

  if FImage.PaletteAllocated
  then begin
    FOldPalette := SelectPalette(DC, FImage.Palette, True);
    RealizePalette(DC);
  end
  else begin
    FOldPalette := 0;
  end;
  
  // since moveto doesn't force a unshare, it is possible that a moveto was
  // done in the old DC.
  with PenPos do
    LCLIntf.MoveToEx(DC, X, Y, nil);

  Handle := DC;
  //DebugLn('TBitmapCanvas.CreateHandle END Self=',DbgS(Self),' DC=',DbgS(DC),
  //  ' Handle=',DbgS(GetUpdatedHandle([csHandleValid])));
end;

{------------------------------------------------------------------------------
  Method: TBitmapCanvas.Destroy
  Params:  None
  Returns: Nothing

  Destructor for the class.
 ------------------------------------------------------------------------------}
destructor TBitmapCanvas.Destroy;
begin
  FreeDC;
  inherited Destroy;
end;

{------------------------------------------------------------------------------
  Method:  TControlCanvas.FreeDC
  Params:  None
  Returns: Nothing

  Frees the device context
 ------------------------------------------------------------------------------}
procedure TBitmapCanvas.FreeDC;
var
  OldHandle: HBITMAP;
begin
  if not HandleAllocated then exit;
  //DebugLn('TBitmapCanvas.FreeDC START Self=',DbgS(Self),' FBitmap=',DbgS(FBitmap));

  if FImage = nil
  then begin
    Handle := 0;
    Exit;
  end;
  
  OldHandle := FHandle;
  Handle := 0;
  FImage.FSharedImage.BitmapCanvas := nil;

  if FOldBitmap <> 0
  then begin
    SelectObject(OldHandle, FOldBitmap);
    FOldBitmap := 0;
  end;

  if FOldPalette <> 0
  then begin
    SelectPalette(OldHandle, FOldPalette, True);
    FOldPalette := 0;
  end;
  DeleteDC(OldHandle);
end;

// included by graphics.pp

