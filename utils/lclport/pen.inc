{%MainUnit ../graphics.pp}
{******************************************************************************
                                     TPen
 ******************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}

type
  TExtPenAndPattern = record
    ExtPen: TExtLogPen;
    Pattern: TPenPattern;
  end;
  PExtPenAndPattern = ^TExtPenAndPattern;

function CompareExtPenAndPatternWithResDesc(Key: PExtPenAndPattern; Desc: TPenHandleCacheDescriptor): integer;
begin
  Result := CompareMemRange(@Key^.ExtPen, @Desc.ExtPen,
                          SizeOf(Key^.ExtPen));
  if Result <> 0 then
    Exit;

  Result := CompareValue(Length(Key^.Pattern), Length(Desc.Pattern));
  if Result <> 0 then
    Exit;

  if Length(Key^.Pattern) > 0 then
  begin
    Result := CompareMemRange(@Key^.Pattern[0], @Desc.Pattern[0],
                            SizeOf(Key^.Pattern[0]) * Length(Key^.Pattern));
  end;
end;

{ TPenHandleCache }

procedure TPenHandleCache.RemoveItem(Item: TResourceCacheItem);
begin
  DeleteObject(HGDIOBJ(Item.Handle));
  inherited RemoveItem(Item);
end;

constructor TPenHandleCache.Create;
begin
  inherited Create;
  FResourceCacheDescriptorClass := TPenHandleCacheDescriptor;
end;

function TPenHandleCache.CompareDescriptors(Tree: TAvgLvlTree; Desc1,
  Desc2: Pointer): integer;
var
  Descriptor1: TPenHandleCacheDescriptor absolute Desc1;
  Descriptor2: TPenHandleCacheDescriptor absolute Desc2;
begin
  Result := CompareMemRange(@Descriptor1.ExtPen, @Descriptor2.ExtPen,
                          SizeOf(Descriptor1.ExtPen));
  if Result <> 0 then
    Exit;

  Result := CompareValue(Length(Descriptor1.Pattern), Length(Descriptor2.Pattern));
  if Result <> 0 then
    Exit;

  if Length(Descriptor1.Pattern) > 0 then
  begin
    Result := CompareMemRange(@Descriptor1.Pattern[0], @Descriptor2.Pattern[0],
                            SizeOf(Descriptor1.Pattern[0]) * Length(Descriptor1.Pattern));
  end;
end;

function TPenHandleCache.FindPen(APen: TLCLHandle): TResourceCacheItem;
var
  ANode: TAvgLvlTreeNode;
begin
  ANode := FItems.FindKey(@APen,
                          TListSortCompare(@ComparePHandleWithResourceCacheItem));
  if ANode <> nil then
    Result := TResourceCacheItem(ANode.Data)
  else
    Result := nil;
end;

function TPenHandleCache.FindPenDesc(const AExtPen: TExtLogPen;
  const APattern: TPenPattern): TPenHandleCacheDescriptor;
var
  ExtPenAndPattern: TExtPenAndPattern;
  ANode: TAvgLvlTreeNode;
begin
  ExtPenAndPattern.ExtPen := AExtPen;
  ExtPenAndPattern.Pattern := APattern;
  ANode := FDescriptors.Findkey(@ExtPenAndPattern,
                           TListSortCompare(@CompareExtPenAndPatternWithResDesc));
  if ANode <> nil then
    Result := TPenHandleCacheDescriptor(ANode.Data)
  else
    Result := nil;
end;

function TPenHandleCache.Add(APen: TLCLHandle; const AExtPen: TExtLogPen;
  const APattern: TPenPattern): TPenHandleCacheDescriptor;
var
  Item: TResourceCacheItem;
begin

  // find cache item with APen
  Item := FindPen(APen);
  if Item = nil then
  begin
    // create new item
    Item := TResourceCacheItem.Create(Self, APen);
    FItems.Add(Item);
  end;

  // create descriptor
  Result := TPenHandleCacheDescriptor.Create(Self, Item);
  Result.ExtPen := AExtPen;
  Result.Pattern := APattern;
  FDescriptors.Add(Result);
  if FindPenDesc(AExtPen, APattern) = nil then
  begin

  end;
end;

{ TPen }

{------------------------------------------------------------------------------
  Method: TPen.SetColor
  Params: Value: the new value
  Returns:  nothing

  Sets the style of a pen
 ------------------------------------------------------------------------------}
procedure TPen.SetColor(Value : TColor);
begin
  if FColor <> Value then
    SetColor(Value, TColorToFPColor(Value));
end;

{------------------------------------------------------------------------------
  Method: TPen.SetStyle
  Params: Value: the new value
  Returns:  nothing

  Sets the style of a pen
 ------------------------------------------------------------------------------}
procedure TPen.SetStyle(Value : TPenStyle);
begin
  if Style <> Value then
  begin
    FreeReference;
    inherited SetStyle(Value);
    Changed;
  end;
end;

{------------------------------------------------------------------------------
  Method: TPen.SetMode
  Params: Value: the new value
  Returns:  nothing

  Sets the Mode of a pen
 ------------------------------------------------------------------------------}
procedure TPen.SetMode(Value : TPenMode);
begin
  if Mode <> Value then
  begin
    FreeReference;
    inherited SetMode(Value);
    Changed;
  end;
end;

{------------------------------------------------------------------------------
  Method: TPen.SetWidth
  Params: Value: the new value
  Returns:  nothing

  Sets the style of a pen
 ------------------------------------------------------------------------------}
procedure TPen.SetWidth(Value : Integer);
begin
  if (Width <> Value) then
  begin
    FreeReference;
    inherited SetWidth(Value);
    Changed;
  end;
end;

{------------------------------------------------------------------------------
  Method:  TPen.Create
  Params:  none
  Returns: Nothing

  Constructor for the class.
 ------------------------------------------------------------------------------}
constructor TPen.Create;
begin
  inherited Create;
  DelayAllocate := True;
  FCosmetic := True;
  {$IFDEF HasFPEndCap}
  inherited SetEndCap(pecRound);
  {$ELSE}
  FEndCap := pecRound;
  {$ENDIF}
  {$IFDEF HasFPJoinStyle}
  inherited SetJoinStyle(pjsRound);
  {$ELSE}
  FJoinStyle := pjsRound;
  {$ENDIF}
  inherited SetWidth(1);
  inherited SetStyle(psSolid);
  inherited SetMode(pmCopy);
  inherited SetFPColor(colBlack);
  Color := clBlack;
end;

{------------------------------------------------------------------------------
  Method: TPen.Destroy
  Params:  None
  Returns: Nothing

  Destructor for the class.
 ------------------------------------------------------------------------------}
destructor TPen.Destroy;
begin
  FreeReference;
  inherited Destroy;
end;

{------------------------------------------------------------------------------
  Method: TPen.Assign
  Params: Source: Another pen
  Returns:  nothing

  Copies the source pen to itself
 ------------------------------------------------------------------------------}
procedure TPen.Assign(Source : Tpersistent);
var
  APen: TPen absolute Source;
begin
  if Source is TPen then
  begin
    Width := APen.Width;
    SetColor(APen.Color, TFPCanvasHelper(Source).FPColor);
    Style := APen.Style;
    Mode := APen.Mode;
    Cosmetic := APen.Cosmetic;
    JoinStyle := APen.JoinStyle;
    EndCap := APen.EndCap;
  end
  else
    inherited Assign(Source);
end;

function TPen.GetPattern: TPenPattern;
begin
  Result := FPattern;
end;

procedure TPen.SetPattern(APattern: TPenPattern);

  function PatternsDiffer: Boolean;
  var
    l1, l2, m: integer;
  begin
    l1 := Length(FPattern);
    l2 := Length(APattern);
    m := min(l1, l2);
    Result := (l1 <> l2) or
              ((m > 0) and not CompareMem(@APattern[0], @FPattern[0], m * SizeOf(LongWord)));
  end;

begin
  if PatternsDiffer then
  begin
    FreeReference;
    FPattern := APattern;
    Changed;
  end;
end;

{------------------------------------------------------------------------------
  Method: TPen.SetHandle
  Params:   a pen handle
  Returns:  nothing

  sets the pen to an external created pen
 ------------------------------------------------------------------------------}
procedure TPen.SetHandle(const Value: HPEN);
begin
  if HPEN(FReference.Handle) = Value then Exit;

  FreeReference;
  FReference._lclHandle := TLCLHandle(Value);
  Changed;
end;

procedure TPen.SetJoinStyle(AValue: TPenJoinStyle);
begin
  if JoinStyle <> AValue then
  begin
    FreeReference;
    {$IFDEF HasFPJoinStyle}
    inherited SetJoinStyle(AValue);
    {$ELSE}
    FJoinStyle := AValue;
    {$ENDIF}
    Changed;
  end;
end;

{------------------------------------------------------------------------------
  Function: TPen.GetHandle
  Params:   none
  Returns:  a handle to a pen gdiobject

  Creates a pen if needed
 ------------------------------------------------------------------------------}
function TPen.GetHandle: HPEN;
begin
  Result := HPEN(Reference.Handle);
end;

function TPen.GetReference: TWSPenReference;
begin
  ReferenceNeeded;
  Result := FReference;
end;

procedure TPen.ReferenceNeeded;
const
  PEN_STYLES: array[TPenStyle] of DWord = (
 { psSolid       } PS_SOLID,
 { psDash        } PS_DASH,
 { psDot         } PS_DOT,
 { psDashDot     } PS_DASHDOT,
 { psDashDotDot  } PS_DASHDOTDOT,
 { psinsideFrame } PS_INSIDEFRAME,
 { psPattern     } PS_USERSTYLE,
 { psClear       } PS_NULL
  );

  PEN_GEOMETRIC: array[Boolean] of DWord = (
  { false }  PS_COSMETIC,
  { true  }  PS_GEOMETRIC
  );

  PEN_ENDCAP: array[TPenEndCap] of DWord = (
  { pecRound  } PS_ENDCAP_ROUND,
  { pecSquare } PS_ENDCAP_SQUARE,
  { pecFlat   } PS_ENDCAP_FLAT
  );

  PEN_JOIN: array[TPenJoinStyle] of DWord = (
  { pjsRound } PS_JOIN_ROUND,
  { pjsBevel } PS_JOIN_BEVEL,
  { pjsMiter } PS_JOIN_MITER
  );
var
  ALogPen: TLogPen;
  AExtPen: TExtLogPen;
  ALogBrush: TLogBrush;
  CachedPen: TPenHandleCacheDescriptor;
  IsGeometric: Boolean;
begin
  if FReference.Allocated then Exit;

  IsGeometric := (Width > 1) or not Cosmetic;

  FillChar(AExtPen, SizeOf(AExtPen), 0);
  with AExtPen do
  begin
    elpPenStyle := PEN_STYLES[Style] or PEN_GEOMETRIC[IsGeometric];
    if IsGeometric then
      elpPenStyle := elpPenStyle or PEN_ENDCAP[EndCap] or PEN_JOIN[JoinStyle];
    if IsGeometric then
      elpWidth := Width
    else
      elpWidth := 0;
    elpBrushStyle := BS_SOLID;
    elpColor := TColorRef(FColor);
  end;

  PenResourceCache.Lock;
  try
    if Style = psPattern then
      CachedPen := PenResourceCache.FindPenDesc(AExtPen, FPattern)
    else
      CachedPen := PenResourceCache.FindPenDesc(AExtPen, nil);

    if CachedPen <> nil then
    begin
      CachedPen.Item.IncreaseRefCount;
      FReference._lclHandle := CachedPen.Item.Handle;
    end else
    begin
      // choose which function to use: CreatePenIndirect or ExtCreatePen
      if ((AExtPen.elpPenStyle and PS_STYLE_MASK) = AExtPen.elpPenStyle) and
          (AExtPen.elpPenStyle <> PS_USERSTYLE) then
      begin
        // simple pen
        ALogPen.lopnStyle := AExtPen.elpPenStyle;
        ALogPen.lopnWidth := Point(AExtPen.elpWidth, 0);
        ALogPen.lopnColor := AExtPen.elpColor;
        FReference._lclHandle := TLCLHandle(CreatePenIndirect(ALogPen));
      end
      else
      begin
        // extended pen
        ALogBrush.lbStyle := AExtPen.elpBrushStyle;
        ALogBrush.lbColor := AExtPen.elpColor;
        ALogBrush.lbHatch := AExtPen.elpHatch;
        if (Style = psPattern) and (Length(FPattern) > 0) then
          FReference._lclHandle := TLCLHandle(ExtCreatePen(AExtPen.elpPenStyle,
            AExtPen.elpWidth, ALogBrush, Length(FPattern), @FPattern[0]))
        else
          FReference._lclHandle := TLCLHandle(ExtCreatePen(AExtPen.elpPenStyle,
            AExtPen.elpWidth, ALogBrush, 0, nil));
      end;

      if Style = psPattern then
        PenResourceCache.Add(FReference.Handle, AExtPen, FPattern)
      else
        PenResourceCache.Add(FReference.Handle, AExtPen, nil);
    end;
    FPenHandleCached := True;
  finally
    PenResourceCache.Unlock;
  end;
end;

procedure TPen.SetCosmetic(const AValue: Boolean);
begin
  if Cosmetic <> AValue then
  begin
    FreeReference;
    FCosmetic := AValue;
    Changed;
  end;
end;

procedure TPen.SetEndCap(AValue: TPenEndCap);
begin
  if EndCap <> AValue then
  begin
    FreeReference;
    {$IFDEF HasFPEndCap}
    inherited SetEndCap(AValue);
    {$ELSE}
    FEndCap := AValue;
    {$ENDIF}
    Changed;
  end;
end;

{------------------------------------------------------------------------------
  Method:  TPen.FreeReference
  Params:  none
  Returns: Nothing

  Frees a pen handle if needed
 ------------------------------------------------------------------------------}

procedure TPen.FreeReference;
begin
  if not FReference.Allocated then Exit;

  Changing;
  if FPenHandleCached then
  begin
    PenResourceCache.Lock;
    try
      PenResourceCache.FindPen(FReference.Handle).DecreaseRefCount;
      FPenHandleCached := False;
    finally
      PenResourceCache.Unlock;
    end;
  end else
    DeleteObject(HGDIOBJ(FReference.Handle));
  FReference._lclHandle := 0;
end;

procedure TPen.DoAllocateResources;
begin
  inherited DoAllocateResources;
  GetReference;
end;

procedure TPen.DoDeAllocateResources;
begin
  FreeReference;
  inherited DoDeAllocateResources;
end;

procedure TPen.DoCopyProps(From: TFPCanvasHelper);
var
  APen: TPen absolute From;
begin
  if From is TPen then
  begin
    FreeReference;
    inherited DoCopyProps(From);
    FCosmetic := APen.Cosmetic;
    EndCap := APen.EndCap;
    JoinStyle := APen.JoinStyle;
    //TODO: query new parameters
    Changed;
  end else
    inherited DoCopyProps(From);
end;

procedure TPen.SetColor(const NewColor: TColor; const NewFPColor: TFPColor);
begin
  if (NewColor = Color) and (NewFPColor = FPColor) then Exit;
  FreeReference;
  FColor := NewColor;
  inherited SetFPColor(NewFPColor);
  Changed;
end;

procedure TPen.SetFPColor(const AValue: TFPColor);
begin
  if FPColor <> AValue then
    SetColor(FPColorToTColor(AValue), AValue);
end;

