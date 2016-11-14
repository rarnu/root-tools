{%MainUnit ../graphics.pp}

{******************************************************************************
                                     TREGION
 ******************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}

{------------------------------------------------------------------------------
  Method:  TRegion.Create
  Params:  none
  Returns: Nothing

  Constructor for the class.
 ------------------------------------------------------------------------------}
constructor TRegion.Create;
begin
  inherited Create;
end;

{------------------------------------------------------------------------------
  Method: TRegion.Destroy
  Params:  None
  Returns: Nothing

  Destructor for the class.
 ------------------------------------------------------------------------------}
destructor TRegion.Destroy;
begin
  FreeReference;
  inherited Destroy;
end;

{------------------------------------------------------------------------------
  Method: TRegion.Assign
  Params: Source: Another brush
  Returns:  nothing

  Copies the source brush to itself
 ------------------------------------------------------------------------------}
procedure TRegion.Assign(Source : Tpersistent);
begin
  if Source is TRegion then
  begin
    Exit;
  end;

  inherited Assign(Source);
end;

procedure TRegion.AddRectangle(X1, Y1, X2, Y2: Integer);
var
  lRGN: HRGN;
begin
  ReferenceNeeded();
  lRGN := CreateRectRgn(X1, Y1, X2, Y2);
  CombineRgn(FReference.Handle, FReference.Handle, lRGN, RGN_OR);
  DeleteObject(lRGN);
end;

{------------------------------------------------------------------------------
  Method: TRegion.SetHandle
  Params:   a region handle
  Returns:  nothing

  sets the region to an external created region
 ------------------------------------------------------------------------------}
procedure TRegion.SetHandle(const Value: HRGN);
begin
  if HRGN(FReference.Handle) = Value then Exit;

  FreeReference;
  FReference._lclHandle := TLCLHandle(Value);
  //TODO: query region description data
  Changed;
end;

{------------------------------------------------------------------------------
  Function: TRegion.GetHandle
  Params:   none
  Returns:  a handle to a Region gdiobject

  Creates a region if needed
 ------------------------------------------------------------------------------}
function TRegion.GetHandle: HRGN;
begin
  Result := HRGN(Reference.Handle);
end;

function TRegion.GetReference: TWSRegionReference;
begin
  ReferenceNeeded;
  Result := FReference;
end;

procedure TRegion.ReferenceNeeded;
// var
//  i: Integer;
//  lSource1, lSource2, lDest: HRGN;
begin
  if FReference.Allocated then Exit;

  FReference._lclHandle := TLCLHandle(CreateRectRgn(0, 0, 0, 0));

{ At the moment all operations are executed on a live handle, so not yet used

  for i := 0 to Length(RegionOperations) - 1 do
  begin
//    TRegionCombineMode = (rgnAnd, rgnCopy, rgnDiff, rgnOr, rgnXOR);
    case RegionOperations[i].ROType of
      rgnNewRect:
      begin
        lDest := LCLIntf.CreateRectRgn(
          RegionOperations[i].Rect.Left, RegionOperations[i].Rect.Top,
          RegionOperations[i].Rect.Left + RegionOperations[i].Rect.Width,
          RegionOperations[i].Rect.Top + RegionOperations[i].Rect.Height);
        AddSubRegion(lDest);
      end;
      rgnCombine:
      begin
        RegionOperations[i].CombineMode:=;

        CombineRgn(lDest, lSource1, lSource2);
      end;
    end;
    Source1, Source2, Dest: Integer; // Index to the list of sub-regions, -1 indicates the main region
    CombineMode: TRegionCombineMode; // Used only if ROType=rgnCombine
    Rect: TRect; // Used for ROType=rgnNewRect
  end;}
end;

{------------------------------------------------------------------------------
  Method:  TRegion.FreeReference
  Params:  none
  Returns: Nothing

  Frees a region handle if needed
 ------------------------------------------------------------------------------}

procedure TRegion.AddOperation(AOp: TRegionOperation);
begin

end;

procedure TRegion.ClearSubRegions();
begin

end;

procedure TRegion.AddSubRegion(AHandle: HRGN);
begin

end;

procedure TRegion.FreeReference;
begin
  if not FReference.Allocated then Exit;

  DeleteObject(HGDIOBJ(FReference.Handle));
  FReference._lclHandle := 0;
end;

{------------------------------------------------------------------------------
  Method: TRegion.SetClipRect
  Params: Value: the new value
  Returns:  nothing

  Sets/clips the region to a rectangle value
 ------------------------------------------------------------------------------}
procedure TRegion.SetClipRect(Value : TRect);
begin
//  if ClipRect = Value then Exit;

//  FreeReference;
//  ClipRect := Value;

//  Changed;
end;

{------------------------------------------------------------------------------
  Method: TRegion.GetClipRect
  Params: none
  Returns:  current rect

  Gets the regions current clip rect
 ------------------------------------------------------------------------------}
function TRegion.GetClipRect : TRect;
begin
  Result := Rect(0, 0, 0, 0);
//  Result := ClipRect;
{  if FRegionData.Reference.Allocated
  then begin
    GetRgnBox(HRGN(FRegionData.Reference.Handle), @Result);
  end
  else begin
    if FRegionData.Polygon <> nil
    then begin
      ReferenceNeeded;
      if FRegionData.Reference.Allocated
      then Result := GetClipRect
      else Result := FRegionData.Rect;
      //FreeReference; // ??? why free (mwe)
    end
    else
      Result := FRegionData.Rect;
  end;}
end;

// included by graphics.pp

