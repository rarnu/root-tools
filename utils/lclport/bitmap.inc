{%MainUnit ../graphics.pp}

{******************************************************************************
                                    TBitmap
 ******************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}

function TestStreamIsBMP(const AStream: TStream): boolean;
var
  Signature: array[0..1] of Char;
  ReadSize: Integer;
  OldPosition: TStreamSeekType;
begin
  OldPosition:=AStream.Position;
  ReadSize:=AStream.Read(Signature, SizeOf(Signature));
  Result:=(ReadSize=2) and (Signature[0]='B') and (Signature[1]='M');
  //debugln('TestStreamIsBMP ',DbgStr(Signature[0]),' ',DbgStr(Signature[1]));
  AStream.Position:=OldPosition;
end;

type

  { THeaderStream }

  THeaderStream = class(TStream)
  private
    FSource: TStream;
    FSourceStart: Int64;
    FHeadPos: Integer;
    FHeadPtr: PByte;
    FHeadSize: Integer;
  protected
  public
    function Read(var Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    constructor Create(ASource: TStream; AHeader: Pointer; ASize: Integer);
  end;

{ THeaderStream }

constructor THeaderStream.Create(ASource: TStream; AHeader: Pointer; ASize: Integer);
begin
  inherited Create;
  FSource := ASource;
  FSourceStart := ASource.Position;
  FHeadPtr := AHeader;
  FHeadSize := ASize;
end;

function THeaderStream.Read(var Buffer; Count: Longint): Longint;
var
  len: Integer;
  buf: PByte;
begin
  if Count <= 0 then Exit(0);

  if FHeadPos < FHeadSize
  then begin
    len := Min(FHeadSize - FHeadPos, Count);
    Move(FHeadPtr[FHeadPos], Buffer, len);
    Dec(Count, len);
    Inc(FHeadPos, len);
    if Count = 0 then Exit(len);
    buf := @Buffer;
    Inc(buf, len);
  end
  else begin
    len := 0;
    buf := @Buffer;
  end;
  Result := FSource.Read(buf^, Count) + len;
end;

function THeaderStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  case Origin of
    soBeginning: begin
      Result := Offset;
    end;
    soCurrent: begin
      Result := FHeadPos + Offset;
      if FHeadPos = FHeadSize
      then Inc(Result, FSource.Position);
    end;
    soEnd: begin
      Result := FHeadSize + FSource.Size - FSourceStart + Offset;
    end;
  end;

  if Result < FHeadSize
  then begin
    FHeadPos := Result;
    FSource.Seek(FSourceStart, soBeginning);
  end
  else begin
    FHeadPos := FHeadSize;
    FSource.Seek(FSourceStart + Result - FHeadSize, soBeginning);
  end;
end;


{ TBitmap }

class function TBitmap.GetFileExtensions: string;
begin
  Result:='bmp';
end;

function TBitmap.GetResourceType: TResourceType;
begin
  Result := RT_BITMAP;
end;

procedure TBitmap.LoadFromStream(AStream: TStream; ASize: Cardinal);
var
  S: THeaderStream;
  Header: TBitmapFileHeader;
begin
  if AStream is TResourceStream then
  begin
    FillChar(Header, SizeOf(Header), 0);

{ Create a BMP header ordered as it would be on disc, noting that if the CPU
  is big-endian this will be the "wrong way round" for numeric operations.      }

    {$IFNDEF ENDIAN_BIG}
    Header.bfType := $4d42;
    Header.bfSize := SizeOf(Header) + ASize;
    {$ELSE}
    Header.bfType := $424d;
    Header.bfSize := swap(SizeOf(Header) + ASize);
    {$ENDIF} 
    //Header.bfOffBits := 0; //data imediately follows

    S := THeaderStream.Create(AStream, @Header, SizeOf(Header));
    try
      inherited LoadFromStream(S, SizeOf(Header) + ASize);
    finally
      S.Free;
    end;
  end
  else
    inherited LoadFromStream(AStream, ASize);
end;

class function TBitmap.GetReaderClass: TFPCustomImageReaderClass;
begin
  Result := TLazReaderBMP;
end;

class function TBitmap.GetSharedImageClass: TSharedRasterImageClass;
begin
  Result := TSharedBitmap;
end;
    
class function TBitmap.GetWriterClass: TFPCustomImageWriterClass;
begin
  Result := TLazWriterBMP;
end;

procedure TBitmap.InitializeReader(AImage: TLazIntfImage; AReader: TFPCustomImageReader);
var
  LazReader: TLazReaderBMP absolute AReader;
begin
  inherited;

  if not (AReader is TLazReaderBMP) then Exit;
  
  // TransparentMode
  //   tmAuto: use left bottom pixel
  //   tmFixed: use color
  //
  // TransparentColor:
  //   clDefault: use left, bottom pixel color as transparent color (*)
  //   clNone: load image opaque (*)
  //   otherwise: use TransparentColor as transparent color
  //
  //   (*) these are Lazarus extensions

  if (TransparentMode = tmAuto) or (TransparentColor = clDefault)
  then begin
    LazReader.MaskMode := lrmmAuto;
  end
  else begin
    if TransparentColor = clNone
    then begin
      LazReader.MaskMode := lrmmNone;
    end
    else begin
      LazReader.MaskMode := lrmmColor;
      LazReader.MaskColor := TColorToFPColor(TransparentColor);
    end;
  end;
end;

