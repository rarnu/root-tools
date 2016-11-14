{%MainUnit ../graphics.pp}

{******************************************************************************
                                TGIFImage
 ******************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}

{ TGIFImage }

constructor TGIFImage.Create;
begin
  inherited Create;
end;

class function TGIFImage.IsStreamFormatSupported(Stream: TStream): Boolean;
var
  Pos: Int64;
  Signature: array [0..5] of char;
begin
  Pos := Stream.Position;
  try
    Stream.Read(Signature, SizeOf(Signature));
    Result := (Signature='GIF89a') or (Signature='GIF87a');
  finally
    Stream.Position := Pos;
  end;
end;

procedure TGIFImage.FinalizeReader(AReader: TFPCustomImageReader);
begin
  inherited;
  with TFPReaderGif(AReader) do
  begin
    FTransparent := Transparent;
    FInterlaced := Interlace;
    FBitsPerPixel := BitsPerPixel;
  end;
end;

class function TGIFImage.GetFileExtensions: string;
begin
  Result := 'gif';
end;

class function TGIFImage.GetReaderClass: TFPCustomImageReaderClass;
begin
  Result := TFPReaderGif;
end;

class function TGIFImage.GetSharedImageClass: TSharedRasterImageClass;
begin
  Result := TSharedGIFImage;
end;

procedure TGIFImage.InitializeReader(AImage: TLazIntfImage;
  AReader: TFPCustomImageReader);
begin
  inherited;
  //Nothing by now. Maybe not needed at all.
end;
