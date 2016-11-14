{%MainUnit ../graphics.pp}

{******************************************************************************
                                TTiffImage
 ******************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}

{ TTiffImage }

constructor TTiffImage.Create;
begin
  inherited Create;
{ct9999} FSoftware := 'TTiffImage - Typhon';
end;

procedure TTiffImage.FinalizeReader(AReader: TFPCustomImageReader);
var
  D, T: TDateTime;
  S: String;
  YY, MM, DD, HH, NN, SS: Integer;
  {$IF FPC_FULLVERSION>=20601}
  IFD: TTiffIFD;
  {$ELSE}
  IFD: TTiffIDF;
  {$ENDIF}
begin
  IFD:=TFPReaderTiff(AReader).FirstImg;
  FArtist := IFD.Artist;
  FCopyright := IFD.Copyright;

  S := IFD.DateAndTime;
  // YYYY:MM:DD HH:MM:SS
  // 0000000001111111111
  // 1234567890123456789
  if  TryStrToInt(Copy(S, 1, 4), YY)
  and TryStrToInt(Copy(S, 6, 2), MM)
  and TryStrToInt(Copy(S, 9, 2), DD)
  and TryStrToInt(Copy(S, 12, 2), HH)
  and TryStrToInt(Copy(S, 15, 2), NN)
  and TryStrToInt(Copy(S, 18, 2), SS)
  and TryEncodeDate(YY, MM, DD, D)
  and TryEncodeTime(HH, NN, SS, 0, T)
  then FDateTime := ComposeDateTime(D,T)
  else FDateTime := 0;

  FDocumentName := IFD.DocumentName;
  FHostComputer := IFD.HostComputer;
  FImageDescription := IFD.ImageDescription;
  FMake := IFD.Make_ScannerManufacturer;
  FModel := IFD.Model_Scanner;
  case IFD.ResolutionUnit of
    1: FResolutionUnit := tuNone;
    2: FResolutionUnit := tuInch;
    3: FResolutionUnit := tuCentimeter;
  else
    FResolutionUnit := tuUnknown;
  end;
  FSoftware := IFD.Software;
  FXResolution := IFD.XResolution;
  FYResolution := IFD.YResolution;
  inherited;
end;

class function TTiffImage.GetFileExtensions: string;
begin
  Result := 'tif;tiff';
end;

class function TTiffImage.GetReaderClass: TFPCustomImageReaderClass;
begin
  Result := TLazReaderTiff;
end;

class function TTiffImage.GetSharedImageClass: TSharedRasterImageClass;
begin
  Result := TSharedTiffImage;
end;

class function TTiffImage.GetWriterClass: TFPCustomImageWriterClass;
begin
  Result := TLazWriterTiff;
end;

procedure TTiffImage.InitializeReader(AImage: TLazIntfImage; AReader: TFPCustomImageReader);
begin
  inherited;
end;

procedure TTiffImage.InitializeWriter(AImage: TLazIntfImage; AWriter: TFPCustomImageWriter);
begin
  inherited;

  if FArtist = ''
  then AImage.RemoveExtra(TiffArtist)
  else AImage.Extra[TiffArtist] := FArtist;

  if FCopyright = ''
  then AImage.RemoveExtra(TiffCopyright)
  else AImage.Extra[TiffCopyright] := FCopyright;

  if FDateTime = 0
  then AImage.RemoveExtra(TiffDateTime)
  else AImage.Extra[TiffDateTime] := FormatDateTime('YYYY:MM:DD HH:NN:SS', FDateTime);

  if FDocumentName = ''
  then AImage.RemoveExtra(TiffDocumentName)
  else AImage.Extra[TiffDocumentName] := FDocumentName;

  if FImageDescription = ''
  then AImage.RemoveExtra(TiffImageDescription)
  else AImage.Extra[TiffImageDescription] := FImageDescription;

  case FResolutionUnit of
    tuNone: AImage.Extra[TiffResolutionUnit] := '1';
    tuInch: AImage.Extra[TiffResolutionUnit] := '2';
    tuCentimeter: AImage.Extra[TiffResolutionUnit] := '3';
  else
    AImage.RemoveExtra(TiffResolutionUnit);
  end;

  if (FXResolution.Denominator = 0) and (FXResolution.Numerator = 0)
  then AImage.RemoveExtra(TiffXResolution)
  else AImage.Extra[TiffXResolution] := TiffRationalToStr(FXResolution);

  if (FYResolution.Denominator = 0) and (FYResolution.Numerator = 0)
  then AImage.RemoveExtra(TiffYResolution)
  else AImage.Extra[TiffYResolution] := TiffRationalToStr(FYResolution);

  if FHostComputer = ''
  then AImage.RemoveExtra(LazTiffHostComputer)
  else AImage.Extra[LazTiffHostComputer] := FHostComputer;

  if FMake = ''
  then AImage.RemoveExtra(LazTiffMake)
  else AImage.Extra[LazTiffMake] := FMake;

  if FModel = ''
  then AImage.RemoveExtra(LazTiffModel)
  else AImage.Extra[LazTiffModel] := FModel;

  if FSoftware = ''
  then AImage.RemoveExtra(LazTiffSoftware)
  else AImage.Extra[LazTiffSoftware] := FSoftware;

end;

