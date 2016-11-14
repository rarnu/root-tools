{%MainUnit ../graphics.pp}

{******************************************************************************
                         TPortableNetworkGraphic
 ******************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}

{ TPortableNetworkGraphic }

class function TPortableNetworkGraphic.GetFileExtensions: string;
begin
  Result:='png';
end;

class function TPortableNetworkGraphic.GetReaderClass: TFPCustomImageReaderClass;
begin
  Result := TLazReaderPNG;
end;

class function TPortableNetworkGraphic.GetSharedImageClass: TSharedRasterImageClass;
begin
  Result := TSharedPortableNetworkGraphic;
end;

class function TPortableNetworkGraphic.IsStreamFormatSupported(Stream: TStream): Boolean;
const
  Signature: array[0..7] of Byte = ($89, $50, $4E, $47, $0D, $0A, $1A, $0A);
var
  Pos: Int64;
  SigCheck: array[0..7] of byte;
  r: integer;
begin
  Pos := Stream.Position;
  try
    Stream.Read(SigCheck, SizeOf(SigCheck));
    Result := False;
    for r := 0 to 7 do
      if (SigCheck[r] <> Signature[r]) then
        Exit;
    Result := True;
  finally
    Stream.Position := Pos;
  end;
end;

class function TPortableNetworkGraphic.GetWriterClass: TFPCustomImageWriterClass;
begin
  Result := TLazWriterPNG;
end;

procedure TPortableNetworkGraphic.InitializeWriter(AImage: TLazIntfImage; AWriter: TFPCustomImageWriter);
var
  W: TFPWriterPNG absolute AWriter;
begin
  inherited InitializeWriter(AImage, AWriter);
  if AWriter is TFPWriterPNG
  then W.Indexed := W.Indexed or PaletteAllocated;
end;

// included by graphics.pp
