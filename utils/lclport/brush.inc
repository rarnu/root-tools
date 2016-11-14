{%MainUnit ../graphics.pp}
{******************************************************************************
                                     TBrush
 ******************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}

{ TBrushHandleCache }

procedure TBrushHandleCache.RemoveItem(Item: TResourceCacheItem);
begin
  DeleteObject(HGDIOBJ(Item.Handle));
  inherited RemoveItem(Item);
end;

constructor TBrushHandleCache.Create;
begin
  inherited Create(SizeOf(TLogBrush));
end;

{ TBrush }

{------------------------------------------------------------------------------
  Method: TBrush.SetColor
  Params: Value: the new value
  Returns:  nothing

  Sets the style of a brush
 ------------------------------------------------------------------------------}
procedure TBrush.SetColor(Value: TColor);
begin
  SetColor(Value, TColorToFPColor(Value));
end;

{------------------------------------------------------------------------------
  Method: TBrush.SetStyle
  Params: Value: the new value
  Returns:  nothing

  Sets the style of a brush
 ------------------------------------------------------------------------------}
procedure TBrush.SetStyle(Value : TBrushStyle);
begin
  if Style <> Value then
  begin
    FreeReference;
    // reset bitmap
    FBitmap := nil;
    inherited SetStyle(Value);
    Changed;
  end;
end;

{------------------------------------------------------------------------------
  Method: TBrush.SetBitmap
  Params: Value: the new value
  Returns:  nothing

  Sets the style of a brush
 ------------------------------------------------------------------------------}
procedure TBrush.SetBitmap(Value: TCustomBitmap);
begin
  if FBitmap <> Value then
  begin
    FreeReference;
    FBitmap := Value;
    // reset style and color
    inherited SetStyle(bsSolid);
    FColor := clWhite;
    inherited SetFPColor(TColorToFPColor(FColor));
    Changed;
  end;
end;

{------------------------------------------------------------------------------
  Method:  TBrush.Create
  Params:  none
  Returns: Nothing

  Constructor for the class.
 ------------------------------------------------------------------------------}
constructor TBrush.Create;
begin
  inherited Create;
  FBitmap := nil;
  FColor := clWhite;
  inherited SetFPColor(TColorToFPColor(FColor));
  DelayAllocate := True;
  FInternalUpdateIndex := -1;
  inherited SetStyle(bsSolid);
end;

{------------------------------------------------------------------------------
  Method: TBrush.Destroy
  Params:  None
  Returns: Nothing

  Destructor for the class.
 ------------------------------------------------------------------------------}
destructor TBrush.Destroy;
begin
  FreeReference;
  inherited Destroy;
end;

function TBrush.EqualsBrush(ABrush: TBrush): boolean;
begin
  Result := Assigned(ABrush) and
    (Color = ABrush.Color) and
    (FPColor = ABrush.FPColor) and
    (Style = ABrush.Style) and
    (Bitmap = ABrush.Bitmap);
end;

{------------------------------------------------------------------------------
  Method: TBrush.Assign
  Params: Source: Another brush
  Returns:  nothing

  Copies the source brush to itself
 ------------------------------------------------------------------------------}
procedure TBrush.Assign(Source: TPersistent);
begin
  if Source is TBrush then
  begin
    if not EqualsBrush(TBrush(Source)) then
    begin
      FreeReference;
      FColor := TBrush(Source).Color;
      inherited SetFPColor(TFPCanvasHelper(Source).FPColor);
      inherited SetStyle(TBrush(Source).Style);
      FBitmap := TBrush(Source).Bitmap;
      Changed;
    end;
  end
  else
    inherited Assign(Source);
end;

{------------------------------------------------------------------------------
  Method: TBrush.SetHandle
  Params:   a brush handle
  Returns:  nothing

  sets the brush to an external created brush
 ------------------------------------------------------------------------------}
procedure TBrush.SetHandle(const Value: HBRUSH);
begin
  if HBRUSH(FReference.Handle) = Value then Exit;

  FreeReference;
  FReference._lclHandle := TLCLHandle(Value);
  FInternalUpdateIndex := GraphicsUpdateCount;
  Changed;
end;

{------------------------------------------------------------------------------
  Function: TBrush.GetHandle
  Params:   none
  Returns:  a handle to a brush gdiobject

  Creates a brush if needed
 ------------------------------------------------------------------------------}
function TBrush.GetHandle: HBRUSH;
begin
  Result := HBRUSH(Reference.Handle);
end;

{------------------------------------------------------------------------------
  Method:  TBrush.FreeReference
  Params:  none
  Returns: Nothing

  Frees a brush handle if needed
 ------------------------------------------------------------------------------}

procedure TBrush.FreeReference;
var
  CacheItem: TResourceCacheItem;
begin
  if not FReference.Allocated then Exit;

  Changing;
  if FBrushHandleCached then
  begin
    BrushResourceCache.Lock;
    try
      CacheItem := BrushResourceCache.FindItem(FReference.Handle);
      if CacheItem <> nil then
        CacheItem.DecreaseRefCount;
      FBrushHandleCached := False;
    finally
      BrushResourceCache.Unlock;
    end;
  end else
    DeleteObject(HGDIOBJ(FReference.Handle));
  FReference._lclHandle := 0;
end;

function TBrush.GetReference: TWSBrushReference;
begin
  if FInternalUpdateIndex <> GraphicsUpdateCount then
    FreeReference;
  ReferenceNeeded;
  Result := FReference;
end;

function TBrush.GetColor: TColor;
begin
  Result := FColor;
  if (Result = clDefault) and Assigned(Canvas) and (Canvas is TCanvas) then
    Result := TCanvas(Canvas).GetDefaultColor(dctBrush);
end;

procedure TBrush.ReferenceNeeded;
var
  LogBrush: TLogBrush;
  CachedBrush: TBlockResourceCacheDescriptor;
begin
  if FReference.Allocated then Exit;

  FillChar(LogBrush, SizeOf(LogBrush), 0);
  with LogBrush do
  begin
    if FBitmap <> nil then
    begin
      lbStyle := BS_PATTERN;
      lbHatch := FBitmap.Handle;
    end else
    begin
      lbHatch := 0;
      case Style of
        bsSolid: lbStyle := BS_SOLID;
        bsClear: lbStyle := BS_HOLLOW;
      else
        lbStyle := BS_HATCHED;
        lbHatch := Ord(Style) - Ord(bsHorizontal);
      end;
    end;
    lbColor := ColorRef(GetColor);
  end;

  BrushResourceCache.Lock;
  try
    CachedBrush := BrushResourceCache.FindDescriptor(@LogBrush);
    if CachedBrush <> nil then
    begin
      CachedBrush.Item.IncreaseRefCount;
      FReference._lclHandle := CachedBrush.Item.Handle;
    end else
    begin
      if LogBrush.lbStyle <> BS_PATTERN then
        if (LogBrush.lbStyle = BS_SOLID) and IsSysColor(LogBrush.lbColor) then
          FReference._lclHandle := TLCLHandle(GetSysColorBrush(SysColorToSysColorIndex(TColor(LogBrush.lbColor))))
        else
          FReference._lclHandle := TLCLHandle(CreateBrushIndirect(LogBrush))
      else
        FReference._lclHandle := TLCLHandle(CreatePatternBrush(LogBrush.lbHatch));
      BrushResourceCache.AddResource(FReference.Handle, @LogBrush);
    end;
    FBrushHandleCached := True;
  finally
    BrushResourceCache.Unlock;
  end;
  FInternalUpdateIndex := GraphicsUpdateCount;
end;

procedure TBrush.DoChange(var Msg);
begin
  Changed;
end;

procedure TBrush.DoAllocateResources;
begin
  inherited DoAllocateResources;
  GetReference;
end;

procedure TBrush.DoDeAllocateResources;
begin
  FreeReference;
  inherited DoDeAllocateResources;
end;

procedure TBrush.DoCopyProps(From: TFPCanvasHelper);
begin
  if From is TBrush then
  begin
    FreeReference;
    inherited DoCopyProps(From);
    //TODO: query new parameters
    Changed;
  end else
    inherited DoCopyProps(From);
end;

procedure TBrush.SetColor(const NewColor: TColor; const NewFPColor: TFPColor);
begin
  if (NewColor = Color) and (NewFPColor = FPColor) and (Style <> bsClear) then Exit;
  FreeReference;
  // reset bitmap
  FBitmap := nil;
  FColor := NewColor;
  inherited SetFPColor(NewFPColor);
  if Style = bsClear then
    inherited SetStyle(bsSolid);
  Changed;
end;

procedure TBrush.SetFPColor(const AValue: TFPColor);
begin
  if FPColor <> AValue then
    SetColor(FPColorToTColor(AValue), AValue);
end;
