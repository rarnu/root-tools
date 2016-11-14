{%MainUnit ../graphics.pp}

{ TSharedCustomBitmap

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}

// ct9999 MOD for CodeTyphon Studio

constructor TSharedCustomBitmap.Create;
begin
  inherited Create;
  FImage.Init;
end;

destructor TSharedCustomBitmap.Destroy;
begin
  FreeAndNil(FSaveStream);
  FreeHandle;
  FreeImage;
  inherited Destroy;
end;

procedure TSharedCustomBitmap.FreeHandle;
begin
  FreePalette;
  inherited FreeHandle;
end;

procedure TSharedCustomBitmap.FreeImage;
begin
  FImage.FreeData;
  FImage.Description.Format := ricfNone;
end;

procedure TSharedCustomBitmap.FreePalette;
begin
if FPalette<>0 then
   begin
   DeleteObject(FPalette);
   FPalette:=0;
   end;
end;

function TSharedCustomBitmap.GetHeight: Integer;
begin
if FImage.Description.Format<>ricfNone then Result:=FImage.Description.Height else Result:=0;
end;

function TSharedCustomBitmap.HandleAllocated: boolean;
begin
  Result := FHandle <> 0;
end;

function TSharedCustomBitmap.ImageAllocated: boolean;
begin
  Result := FImage.Description.Format <> ricfNone;
end;

function TSharedCustomBitmap.ReleasePalette: HPALETTE;
begin
  Result := FPalette;
  FPalette := 0;
end;

function TSharedCustomBitmap.GetPixelFormat: TPixelFormat;
begin
if FImage.Description.Format=ricfNone then Result:=pfDevice else
   begin
   case FImage.Description.Depth of
        1:   Result:=pf1Bit;
        4:   Result:=pf4Bit;
        8:   Result:=pf8Bit;
        15:  Result:=pf15Bit;
        16:  Result:=pf16Bit;
        24:  Result:=pf24Bit;
        32:  Result:=pf32Bit;
        else Result:=pfCustom;
        end;
   end;
end;

function TSharedCustomBitmap.IsEmpty: boolean;
begin
  Result := inherited IsEmpty and not
    (
      (FImage.Description.Format <> ricfNone) and
      (FImage.Data <> nil) and
      (FImage.DataSize <> 0)
    );
end;

function TSharedCustomBitmap.GetWidth: Integer;
begin
if FImage.Description.Format<>ricfNone then Result:=FImage.Description.Width else Result:=0;
end;

// included by graphics.pp
