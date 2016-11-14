{%MainUnit ../graphics.pp}

{******************************************************************************
                         TPortableAnyMapGraphic
 ******************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}

{ TPortableAnyMapGraphic }

class function TPortableAnyMapGraphic.GetFileExtensions: string;
begin
  Result:='pbm;pgm;ppm';
end;

class function TPortableAnyMapGraphic.GetReaderClass: TFPCustomImageReaderClass;
begin
  Result := TFPReaderPNM;
end;

class function TPortableAnyMapGraphic.GetSharedImageClass: TSharedRasterImageClass;
begin
  Result := TSharedPortableAnyMapGraphic;
end;

class function TPortableAnyMapGraphic.IsStreamFormatSupported(Stream: TStream): Boolean;
var
  Pos: Int64;
  C: Char;
begin
  Pos := Stream.Position;
  try
    Stream.ReadBuffer(C, 1);
    Result := (C = 'P');
    if not Result then
      Exit;
    Stream.ReadBuffer(C, 1);
    Result := (Ord(C)-Ord('0')) in [1..6];
  finally
    Stream.Position := Pos;
  end;
end;

class function TPortableAnyMapGraphic.GetWriterClass: TFPCustomImageWriterClass;
begin
  Result := TFPWriterPNM;
end;


// included by graphics.pp
