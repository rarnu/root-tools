{  $Id: graphtype.pp 50668 2015-12-05 23:29:47Z mattias $  }
{
 /***************************************************************************
                                graphtype.pp
                                ------------
                    Graphic related platform independent types
                    and utility functions.
                    Initial Revision  : Sat Feb 02 0:02:58 2002

 ***************************************************************************/

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit GraphType;

{$mode objfpc}{$H+}

interface

uses
  FPCAdds, Classes, SysUtils, LCLType, LCLProc, types;

{$ifdef Trace}
{$ASSERTIONS ON}
{$endif}

type
  TGraphicsColor = -$7FFFFFFF-1..$7FFFFFFF;
  TGraphicsFillStyle =
  (
    fsSurface, // fill till the color (it fills all except this color)
    fsBorder   // fill this color (it fills only connected pixels of this color)
  );
  TGraphicsBevelCut =
  (
    bvNone,
    bvLowered,
    bvRaised,
    bvSpace
  );
  TGraphicsDrawEffect =
  (
    gdeNormal,      // no effect
    gdeDisabled,    // grayed image
    gdeHighlighted, // a bit highlighted image
    gdeShadowed,    // a bit shadowed image
    gde1Bit         // 1 Bit image (for non-XP windows buttons)
  );

//------------------------------------------------------------------------------
// raw image data
type
  { Colorformat: Higher values means higher intensity.
    For example: Red=0 means no red, Alpha=0 means transparent }
  TRawImageColorFormat = (
    ricfNone,   // Uninitialized
    ricfRGBA,   // one pixel contains red, green, blue and alpha
                // If AlphaPrec=0 then there is no alpha.
                // Same for RedPrec, GreenPrec and BluePrec.
    ricfGray    // R=G=B. The Red stores the Gray. AlphaPrec can be >0.
    );

  TRawImageByteOrder = (
    riboLSBFirst, // least significant byte first
    riboMSBFirst  // most significant byte first
    );
    
  TRawImageBitOrder = (
    riboBitsInOrder, // Bit 0 is pixel 0
    riboReversedBits // Bit 0 is pixel 7 (Bit 1 is pixel 6, ...)
    );

  TRawImageLineEnd = (
    rileTight,         // no gap at end of lines
    rileByteBoundary,  // each line starts at byte boundary. For example:
                       // If BitsPerPixel=3 and Width=1, each line has a gap
                       // of 5 unused bits at the end.
    rileWordBoundary,  // each line starts at word (16bit) boundary
    rileDWordBoundary, // each line starts at double word (32bit) boundary
    rileQWordBoundary, // each line starts at quad word (64bit) boundary
    rileDQWordBoundary // each line starts at double quad word (128bit) boundary
    );

  TRawImageLineOrder = (
    riloTopToBottom, // The line 0 is the top line
    riloBottomToTop  // The line 0 is the bottom line
    );

  TRawImageQueryFlag = (
    riqfMono,        // Include a description for a mono image
    riqfGrey,        // Include a description for a grey image
    riqfRGB,         // Include a description for a RGB image
    riqfAlpha,       // Include a description for an Alpha channel
    riqfMask,        // Include a description for a Mask
    riqfPalette,     // Include a description for a Palette
    riqfUpdate       // Update given description (instead of clearing it)
  );
  TRawImageQueryFlags = set of TRawImageQueryFlag;

  { TRawImageDescription }

  TRawImageDescription = object
    Format: TRawImageColorFormat;
    Width: cardinal;
    Height: cardinal;
    Depth: Byte; // used bits per pixel
    BitOrder: TRawImageBitOrder;
    ByteOrder: TRawImageByteOrder;
    LineOrder: TRawImageLineOrder;
    LineEnd: TRawImageLineEnd;
    BitsPerPixel: Byte; // bits per pixel. can be greater than Depth.
    RedPrec: Byte;      // red or gray precision. bits for red
    RedShift: Byte;     // bitshift. Direction: from least to most significant
    GreenPrec: Byte;
    GreenShift: Byte;
    BluePrec: Byte;
    BlueShift: Byte;
    AlphaPrec: Byte;
    AlphaShift: Byte;

    // The next values are only valid, if there is a mask (MaskBitsPerPixel > 0)
    // Masks are always separate with a depth of 1 bpp. One pixel can occupy
    // one byte at most
    // a value of 1 means that pixel is masked
    // a value of 0 means the pixel value is shown
    MaskBitsPerPixel: Byte; // bits per mask pixel, usually 1, 0 when no mask
    MaskShift: Byte;        // the shift (=position) of the mask bit
    MaskLineEnd: TRawImageLineEnd;
    MaskBitOrder: TRawImageBitOrder;

    // The next values are only valid, if there is a palette (PaletteColorCount > 0)
    PaletteColorCount: Word;   // entries in color palette. 0 when no palette.
    PaletteBitsPerIndex: Byte; // bits per palette index, this can be larger than the colors used
    PaletteShift: Byte;        // bitshift. Direction: from least to most significant
    PaletteLineEnd: TRawImageLineEnd;
    PaletteBitOrder: TRawImageBitOrder;
    PaletteByteOrder: TRawImageByteOrder;
    
    // don't use a constructor here, it will break compatibility with a record
    procedure Init;

    // 1-bit mono format
    procedure Init_BPP1(AWidth, AHeight: integer);

    // 16-bits formats
    procedure Init_BPP16_R5G6B5(AWidth, AHeight: integer);

    // Formats in RGB order
    procedure Init_BPP24_R8G8B8_BIO_TTB(AWidth, AHeight: integer);
    procedure Init_BPP24_R8G8B8_BIO_TTB_UpsideDown(AWidth, AHeight: integer);
    procedure Init_BPP32_A8R8G8B8_BIO_TTB(AWidth, AHeight: integer);
    procedure Init_BPP32_R8G8B8A8_BIO_TTB(AWidth, AHeight: integer);

    // Formats in Windows pixels order: BGR
    procedure Init_BPP24_B8G8R8_BIO_TTB(AWidth, AHeight: integer);
    procedure Init_BPP24_B8G8R8_M1_BIO_TTB(AWidth, AHeight: integer);
    procedure Init_BPP32_B8G8R8_BIO_TTB(AWidth, AHeight: integer);
    procedure Init_BPP32_B8G8R8_M1_BIO_TTB(AWidth, AHeight: integer);
    procedure Init_BPP32_B8G8R8A8_BIO_TTB(AWidth, AHeight: integer);
    procedure Init_BPP32_B8G8R8A8_M1_BIO_TTB(AWidth, AHeight: integer);

    function GetDescriptionFromMask: TRawImageDescription;
    function GetDescriptionFromAlpha: TRawImageDescription;

    //returns indices of channels in four-element array
    procedure GetRGBIndices(out Ridx, Gidx, Bidx, Aidx:Byte);

    function BytesPerLine: PtrUInt;
    function BitsPerLine: PtrUInt;
    function MaskBytesPerLine: PtrUInt;
    function MaskBitsPerLine: PtrUInt;

    function AsString: string;
    function IsEqual(ADesc: TRawImageDescription): Boolean;
  end;
  PRawImageDescription = ^TRawImageDescription;
  
  // Note: not all devices/images have all parts at any time. But if a part can
  // be applied to the device/image, the 'Description' describes its structure.


  TRawImagePosition = record
    Byte: PtrUInt;
    Bit: cardinal;
  end;
  PRawImagePosition = ^TRawImagePosition;

  { TRawImage }

  TRawImage = object
    Description: TRawImageDescription;
    Data: PByte;
    DataSize: PtrUInt;
    Mask: PByte;
    MaskSize: PtrUInt;
    Palette: PByte;
    PaletteSize: PtrUInt;
    
    // don't use a constructor here, it will break compatibility with a record
    procedure Init;
    procedure CreateData(AZeroMem: Boolean);

    procedure FreeData;
    procedure ReleaseData;
    procedure ExtractRect(const ARect: TRect; out ADst: TRawImage);

    function  GetLineStart(ALine: Cardinal): PByte;
    procedure PerformEffect(const ADrawEffect: TGraphicsDrawEffect; CreateNewData: Boolean = True; FreeOldData: boolean = false);
    function  ReadBits(const APosition: TRawImagePosition; APrec, AShift: Byte): Word;
    procedure ReadChannels(const APosition: TRawImagePosition; out ARed, AGreen, ABlue, AAlpha: Word);
    procedure ReadMask(const APosition: TRawImagePosition; out AMask: Boolean);
    procedure WriteBits(const APosition: TRawImagePosition; APrec, AShift: Byte; ABits: Word);
    procedure WriteChannels(const APosition: TRawImagePosition; ARed, AGreen, ABlue, AAlpha: Word);
    procedure WriteMask(const APosition: TRawImagePosition; AMask: Boolean);

    function  IsMasked(ATestPixels: Boolean): Boolean;
    function  IsTransparent(ATestPixels: Boolean): Boolean;
    function  IsEqual(AImage: TRawImage): Boolean;
  end;
  PRawImage = ^TRawImage;

  { TRawImageLineStarts }

  TRawImageLineStarts = object
  private
    FWidth: Cardinal;
    FHeight: Cardinal;
    FBitsPerPixel: Byte;
    FLineEnd: TRawImageLineEnd;
    FLineOrder: TRawImageLineOrder;
  public
    Positions: array of TRawImagePosition;

    // don't use a constructor here, it will break compatibility with a record
    procedure Init(AWidth, AHeight: cardinal; ABitsPerPixel: Byte; ALineEnd: TRawImageLineEnd; ALineOrder: TRawImageLineOrder);
    function GetPosition(x, y: cardinal): TRawImagePosition;
  end;
  PRawImageLineStarts = ^TRawImageLineStarts;

const
  DefaultByteOrder = {$IFDEF Endian_Little}riboLSBFirst{$ELSE}riboMSBFirst{$ENDIF};


function GetBytesPerLine(AWidth: Cardinal; ABitsPerPixel: Byte; ALineEnd: TRawImageLineEnd): PtrUInt;
function GetBitsPerLine(AWidth: Cardinal; ABitsPerPixel: Byte; ALineEnd: TRawImageLineEnd): PtrUInt;

function CopyImageData(AWidth, AHeight, ARowStride: Integer; ABPP: Word;
                       ASource: Pointer; const ARect: TRect; ASourceOrder: TRawImageLineOrder;
                       ADestinationOrder: TRawImageLineOrder; ADestinationEnd: TRawImageLineEnd;
                       out ADestination: Pointer; out ASize: PtrUInt): Boolean;
function RawImageQueryFlagsToString(AFlags: TRawImageQueryFlags): string;

var
  MissingBits: array[0..15] of array[0..7] of word;

implementation

uses
  Math;

{------------------------------------------------------------------------------
  Function: CopyImageData
 ------------------------------------------------------------------------------}

function CopyImageData(AWidth, AHeight, ARowStride: Integer; ABPP: Word; ASource: Pointer; const ARect: TRect; ASourceOrder: TRawImageLineOrder;
                       ADestinationOrder: TRawImageLineOrder; ADestinationEnd: TRawImageLineEnd; out ADestination: Pointer; out ASize: PtrUInt): Boolean;
const
  SIZEMAP: array[TRawImageLineEnd] of Byte = (
    0, 0, 1, 3, 7, 15
  );
var
  W, H, RS, x, LineBytes, LineCount, CopySize, ZeroSize, DstRowInc: Integer;
  P, DstRowPtr: PByte;
  ShiftL, ShiftR: Byte;
  SrcPtr: PByte absolute ASource;
  DstPtr: PByte absolute ADestination;
begin
  // check if we are within bounds
  Result := False;
  if ARect.Left < 0 then Exit;
  if ARect.Top < 0 then Exit;

  W := ARect.Right - ARect.Left;
  H := ARect.Bottom - ARect.Top;
  if W < 0 then Exit;
  if H < 0 then Exit;
  
  // calc destination rowstride
  RS := (W * ABPP + 7) shr 3;
  x := RS and SIZEMAP[ADestinationEnd];
  if x <> 0
  then Inc(RS, 1 + SIZEMAP[ADestinationEnd] - x);

  // check if we can copy all
  if  (ARect.Left = 0) and (ARect.Top = 0)
  and (ARect.Right = AWidth) and (ARect.Bottom = AHeight)
  and (ASourceOrder = ADestinationOrder)
  and (ARowStride = RS)
  then begin
    // full copy
    ASize := AHeight * ARowStride;
    GetMem(ADestination, ASize);
    Move(ASource^, ADestination^, ASize);
    Exit(True);
  end;

  // partial copy

  // calc numer of lines to copy
  if AHeight - ARect.Top < H
  then LineCount := AHeight - ARect.Top
  else LineCount := H;

  ASize := H * RS;
  GetMem(ADestination, ASize);

  if (W = AWidth)
  and (ASourceOrder = ADestinationOrder)
  and (RS = ARowStride)
  then begin
    // easy case, only LineCount lines to copy
    CopySize := LineCount * ARowStride;
    ZeroSize := ASize - CopySize;
    
    if ASourceOrder = riloTopToBottom
    then begin
      // top to bottom, adjust start
      Inc(SrcPtr, ARect.Top * ARowStride);
      Move(SrcPtr[0], DstPtr[0], CopySize);
      // wipe remaining
      if ZeroSize > 0
      then FillChar(DstPtr[CopySize], ZeroSize, 0);
    end
    else begin
      // bottom to top
      // wipe remaining
      if ZeroSize > 0
      then FillChar(DstPtr[0], ZeroSize, 0);
      
      x := AHeight - ARect.Bottom;
      if x > 0
      then Inc(SrcPtr, x * ARowStride);
      Move(SrcPtr[0], DstPtr[ZeroSize], CopySize);
    end;
    Exit(True);
  end;

  // calc number of bytes to copy
  // and wipe destination when source width is smaller than destination
  // I'm to lazy to zero line by line, so we might do to much here
  if AWidth < W
  then begin
    LineBytes := ((AWidth - ARect.Left) * ABPP + 7) shr 3;
    FillByte(DstPtr[0], ASize, 0);
  end
  else begin
    LineBytes := Min(RS, ARowStride);
    if H <> LineCount
    then FillByte(DstPtr[0], ASize, 0);
  end;

  // move to start line
  DstRowPtr := ADestination;
  if ASourceOrder = riloTopToBottom
  then begin
    Inc(SrcPtr, ARect.Top * ARowStride);
  end
  else begin
    x := AHeight - ARect.Bottom;
    if x >= 0
    then Inc(SrcPtr, x * ARowStride)
    else Inc(DstRowPtr, -x * RS);
  end;
  
  // check source and destionation order
  if ASourceOrder = ADestinationOrder
  then begin
    DstRowInc := RS;
  end
  else begin
    // reversed, so fill destination backwards
    DstRowInc := -RS;
    Inc(DstRowPtr, (LineCount - 1) * RS);
  end;

  // move to left pixel
  Inc(SrcPtr, (ARect.Left * ABPP) shr 3);

  // check if we can do byte copies
  ShiftL := (ARect.Left * ABPP) and 7;
  if ShiftL = 0
  then begin
    // Partial width, byte aligned
    while LineCount > 0 do
    begin
      Move(SrcPtr^, DstRowPtr^, LineBytes);
      Inc(SrcPtr, ARowStride);
      Inc(DstRowPtr, DstRowInc);
      Dec(LineCount);
    end;
    Exit(True);
  end;

  // Partial width, not aligned
  ShiftR := 8 - ShiftL;
  while LineCount > 0 do
  begin
    P := DstRowPtr;
    for x := 0 to RS - 1 do
    begin
      P^ := Byte(SrcPtr[x] shl ShiftL) or Byte(SrcPtr[x+1] shr ShiftR);
      Inc(P);
    end;
    Inc(SrcPtr, ARowStride);
    Inc(DstRowPtr, DstRowInc);
    Dec(LineCount);
  end;
  Result := True;
end;

function RawImageQueryFlagsToString(AFlags: TRawImageQueryFlags): string;
begin
  Result := '';
  if riqfMono in AFlags then Result := Result + 'riqfMono ';
  if riqfGrey in AFlags then Result := Result + 'riqfGrey ';
  if riqfRGB in AFlags then Result := Result + 'riqfRGB ';
  if riqfAlpha in AFlags then Result := Result + 'riqfAlpha ';
  if riqfMask in AFlags then Result := Result + 'riqfMask ';
  if riqfPalette in AFlags then Result := Result + 'riqfPalette ';
  if riqfUpdate in AFlags then Result := Result + 'riqfUpdate ';
end;

{------------------------------------------------------------------------------
  Function: GetBytesPerLine
 ------------------------------------------------------------------------------}

function GetBytesPerLine(AWidth: Cardinal; ABitsPerPixel: Byte; ALineEnd: TRawImageLineEnd): PtrUInt;
begin
  Result := (GetBitsPerLine(AWidth, ABitsPerPixel, ALineEnd) + 7) shr 3;
end;

{------------------------------------------------------------------------------
  Function: GetBitsPerLine
 ------------------------------------------------------------------------------}
 
function GetBitsPerLine(AWidth: Cardinal; ABitsPerPixel: Byte; ALineEnd: TRawImageLineEnd): PtrUInt;
begin
  Result := AWidth * ABitsPerPixel;
  case ALineEnd of
    rileTight: ;
    rileByteBoundary:   Result := (Result +  7) and not PtrUInt(7);
    rileWordBoundary:   Result := (Result + 15) and not PtrUInt(15);
    rileDWordBoundary:  Result := (Result + 31) and not PtrUInt(31);
    rileQWordBoundary:  Result := (Result + 63) and not PtrUInt(63);
    rileDQWordBoundary: Result := (Result +127) and not PtrUInt(127);
  end;
end;

{------------------------------------------------------------------------------
  Function: RawImage_ReadBits
 ------------------------------------------------------------------------------}
 
procedure RawImage_ReadBits(AData: PByte; const APosition: TRawImagePosition;
                       ABitsPerPixel, APrec, AShift: Byte;
                       ABitOrder: TRawImageBitOrder; out ABits: Word);
var
  PB: PByte;
  PW: PWord  absolute PB;
  PC: PCardinal absolute PB;
  PrecMask: Word;
begin
  PrecMask := (Word(1) shl APrec) - 1;
  PB := @AData[APosition.Byte];
  case ABitsPerPixel of
  1,2,4:
      begin
        if ABitOrder = riboBitsInOrder then
          ABits := (PB^ shr (AShift + APosition.Bit)) and PrecMask
        else
          ABits := (PB^ shr (AShift + 7 - APosition.Bit)) and PrecMask;
      end;
  8:  begin
        ABits := (PB^ shr AShift) and PrecMask;
      end;
  16: begin
        {$IFDEF VerboseLCLTodos}{$note check endian and/or source byte order}{$ENDIF}
        ABits := (PW^ shr AShift) and PrecMask;
      end;
  32: begin
        {$IFDEF VerboseLCLTodos}{$note check endian and/or source byte order}{$ENDIF}
        ABits := (PC^ shr AShift) and PrecMask;
      end;
  else
    ABits:=0;
  end;

  if APrec<16
  then begin
    // add missing bits
    ABits := ABits shl (16 - APrec);
    ABits := ABits or MissingBits[APrec, ABits shr 13];
  end;
end;

{------------------------------------------------------------------------------
  Function: RawImage_WriteBits
 ------------------------------------------------------------------------------}

procedure RawImage_WriteBits(AData: PByte; const APosition: TRawImagePosition;
                       ABitsPerPixel, APrec, AShift: Byte;
                       ABitOrder: TRawImageBitOrder; ABits: Word);
var
  PB: PByte;
  PW: PWord absolute PB;
  PC: PCardinal absolute PB;
  PrecMask: Cardinal;
  BitShift: Integer;
begin
  PB := @AData[APosition.Byte];
  PrecMask := (Cardinal(1) shl APrec) - 1;
  ABits := ABits shr (16 - APrec);

  case ABitsPerPixel of
  1,2,4:
      begin
        if ABitOrder = riboBitsInOrder
        then BitShift := AShift + APosition.Bit
        else BitShift := AShift + 7 - APosition.Bit;

        PrecMask := not(PrecMask shl BitShift);
        PB^ := (PB^ and PrecMask) or (ABits shl BitShift);
      end;
  8:  begin
        PrecMask := not(PrecMask shl aShift);
        PB^ := (PB^ and PrecMask) or (ABits shl AShift);
      end;
  16: begin
        {$IFDEF VerboseLCLTodos}{$note check endian and/or source byte order}{$ENDIF}
        PrecMask := not(PrecMask shl AShift);
        PW^ := (PW^ and PrecMask) or (ABits shl AShift);
      end;
  32: begin
        {$IFDEF VerboseLCLTodos}{$note check endian and/or source byte order}{$ENDIF}
        PrecMask := not(PrecMask shl AShift);
        PC^ := (PC^ and PrecMask) or (ABits shl AShift);
      end;
  end;
end;

{ TRawImageDescription }

procedure TRawImageDescription.Init;
begin
  FillChar(Self, SizeOf(Self), 0);
end;

// 1-bit mono format
procedure TRawImageDescription.Init_BPP1(AWidth, AHeight: integer);
begin
  // setup an artificial ScanLineImage with format RGB 24 bit, 24bit depth format
  FillChar(Self, SizeOf(Self), 0);

  Format := ricfGray;
  Depth := 1; // used bits per pixel
  Width := AWidth;
  Height := AHeight;
  BitOrder := riboBitsInOrder;
  ByteOrder := riboLSBFirst;
  LineOrder := riloTopToBottom;
  BitsPerPixel := 1; // bits per pixel. can be greater than Depth.
  LineEnd := rileDWordBoundary;
  RedPrec := 1; // grey precision
  RedShift := 0;
  GreenPrec := 0;
  GreenShift := 0; // bitshift. Direction: from least to most significant
  BluePrec := 0;
  BlueShift:=0;
//  AlphaPrec:=0;
//  MaskBitsPerPixel:=0;
end;

procedure TRawImageDescription.Init_BPP16_R5G6B5(AWidth, AHeight: integer);
begin
  // setup an artificial ScanLineImage with format RGB 24 bit, 24bit depth format
  FillChar(Self, SizeOf(Self), 0);

  Format := ricfRGBA;
  Depth := 16; // used bits per pixel
  Width := AWidth;
  Height := AHeight;
  BitOrder := riboBitsInOrder;
  ByteOrder := riboLSBFirst;
  LineOrder := riloTopToBottom;
  BitsPerPixel := 24; // bits per pixel. can be greater than Depth.
  LineEnd := rileDWordBoundary;
  RedPrec := 5; // red precision. bits for red
  RedShift := 0;
  GreenPrec := 6;
  GreenShift := 5; // bitshift. Direction: from least to most significant
  BluePrec := 5;
  BlueShift:=11;
//  AlphaPrec:=0;
//  MaskBitsPerPixel:=0;
end;

procedure TRawImageDescription.Init_BPP24_R8G8B8_BIO_TTB(AWidth, AHeight: integer);
begin
  // setup an artificial ScanLineImage with format RGB 24 bit, 24bit depth format
  FillChar(Self, SizeOf(Self), 0);

  Format := ricfRGBA;
  Depth := 24; // used bits per pixel
  Width := AWidth;
  Height := AHeight;
  BitOrder := riboBitsInOrder;
  ByteOrder := riboLSBFirst;
  LineOrder := riloTopToBottom;
  BitsPerPixel := 24; // bits per pixel. can be greater than Depth.
  LineEnd := rileDWordBoundary;
  RedPrec := 8; // red precision. bits for red
  RedShift := 0;
  GreenPrec := 8;
  GreenShift := 8; // bitshift. Direction: from least to most significant
  BluePrec := 8;
  BlueShift:=16;
//  AlphaPrec:=0;
//  MaskBitsPerPixel:=0;
end;

procedure TRawImageDescription.Init_BPP24_R8G8B8_BIO_TTB_UpsideDown(AWidth, AHeight: integer);
begin
  // setup an artificial ScanLineImage with format RGB 24 bit, 24bit depth format
  FillChar(Self, SizeOf(Self), 0);

  Format := ricfRGBA;
  Depth := 24; // used bits per pixel
  Width := AWidth;
  Height := AHeight;
  BitOrder := riboBitsInOrder;
  ByteOrder := riboLSBFirst;
  LineOrder := riloBottomToTop;
  BitsPerPixel := 24; // bits per pixel. can be greater than Depth.
  LineEnd := rileDWordBoundary;
  RedPrec := 8; // red precision. bits for red
  RedShift := 0;
  GreenPrec := 8;
  GreenShift := 8; // bitshift. Direction: from least to most significant
  BluePrec := 8;
  BlueShift:=16;
//  AlphaPrec:=0;
//  MaskBitsPerPixel:=0;
end;

procedure TRawImageDescription.Init_BPP32_A8R8G8B8_BIO_TTB(AWidth, AHeight: integer);
begin
  // setup an artificial ScanLineImage with format RGB 32 bit, 32bit depth format
  FillChar(Self, SizeOf(Self), 0);

  Format := ricfRGBA;
  Depth := 32; // used bits per pixel
  Width := AWidth;
  Height := AHeight;
  BitOrder := riboBitsInOrder;
  ByteOrder := riboLSBFirst;
  LineOrder := riloTopToBottom;
  BitsPerPixel := 32; // bits per pixel. can be greater than Depth.
  LineEnd := rileDWordBoundary;
  RedPrec := 8; // red precision. bits for red
  RedShift := 8;
  GreenPrec := 8;
  GreenShift := 16; // bitshift. Direction: from least to most signifikant
  BluePrec := 8;
  BlueShift := 24;
  AlphaPrec := 8;
  AlphaShift := 0;
//  MaskBitsPerPixel := 0;
end;

procedure TRawImageDescription.Init_BPP32_R8G8B8A8_BIO_TTB(AWidth, AHeight: integer);
begin
  // setup an artificial ScanLineImage with format RGB 32 bit, 32bit depth format
  FillChar(Self, SizeOf(Self), 0);

  Format := ricfRGBA;
  Depth := 32; // used bits per pixel
  Width := AWidth;
  Height := AHeight;
  BitOrder := riboBitsInOrder;
  ByteOrder := riboLSBFirst;
  LineOrder := riloTopToBottom;
  BitsPerPixel := 32; // bits per pixel. can be greater than Depth.
  LineEnd := rileDWordBoundary;
  RedPrec := 8; // red precision. bits for red
  RedShift := 0;
  GreenPrec := 8;
  GreenShift := 8; // bitshift. Direction: from least to most signifikant
  BluePrec := 8;
  BlueShift := 16;
  AlphaPrec := 8;
  AlphaShift := 24;
//  MaskBitsPerPixel := 0;
end;

procedure TRawImageDescription.Init_BPP24_B8G8R8_BIO_TTB(AWidth, AHeight: integer);
{ pf24bit:

 Format=ricfRGBA HasPalette=false Depth=24 PaletteColorCount=0
 BitOrder=riboBitsInOrder ByteOrder=DefaultByteOrder
 LineOrder=riloTopToBottom
 BitsPerPixel=24 LineEnd=rileDWordBoundary
 RedPrec=8 RedShift=16 GreenPrec=8 GreenShift=8 BluePrec=8 BlueShift=0
}
begin
  // setup an artificial ScanLineImage with format RGB 24 bit, 24bit depth format
  FillChar(Self, SizeOf(Self), 0);

  Format := ricfRGBA;
  Depth := 24; // used bits per pixel
  Width := AWidth;
  Height := AHeight;
  BitOrder := riboBitsInOrder;
  ByteOrder := riboLSBFirst;
  LineOrder := riloTopToBottom;
  BitsPerPixel := 24; // bits per pixel. can be greater than Depth.
  LineEnd := rileDWordBoundary;
  RedPrec := 8; // red precision. bits for red
  RedShift := 16;
  GreenPrec := 8;
  GreenShift := 8; // bitshift. Direction: from least to most significant
  BluePrec := 8;
//  BlueShift:=0;
//  AlphaPrec:=0;
//  MaskBitsPerPixel:=0;
end;

procedure TRawImageDescription.Init_BPP24_B8G8R8_M1_BIO_TTB(AWidth, AHeight: integer);
{ pf24bit:

 Format=ricfRGBA HasPalette=false Depth=24 PaletteColorCount=0
 BitOrder=riboBitsInOrder ByteOrder=DefaultByteOrder
 LineOrder=riloTopToBottom
 BitsPerPixel=24 LineEnd=rileDWordBoundary
 RedPrec=8 RedShift=16 GreenPrec=8 GreenShift=8 BluePrec=8 BlueShift=0
 Masked
}
begin
  // setup an artificial ScanLineImage with format RGB 24 bit, 24bit depth format
  FillChar(Self, SizeOf(Self), 0);

  Format := ricfRGBA;
  Depth := 24; // used bits per pixel
  Width := AWidth;
  Height := AHeight;
  BitOrder := riboBitsInOrder;
  ByteOrder := riboLSBFirst;
  LineOrder := riloTopToBottom;
  BitsPerPixel := 24; // bits per pixel. can be greater than Depth.
  LineEnd := rileDWordBoundary;
  RedPrec := 8; // red precision. bits for red
  RedShift := 16;
  GreenPrec := 8;
  GreenShift := 8; // bitshift. Direction: from least to most significant
  BluePrec := 8;
//  BlueShift := 0;
//  AlphaPrec := 0;
  MaskBitsPerPixel := 1;
  MaskBitOrder := riboBitsInOrder;
//  MaskShift := 0;        // the shift (=position) of the mask bit
  MaskLineEnd := rileWordBoundary;
end;

procedure TRawImageDescription.Init_BPP32_B8G8R8_BIO_TTB(AWidth, AHeight: integer);
{ pf32bit:

 Format=ricfRGBA HasPalette=false Depth=24 PaletteColorCount=0
 BitOrder=riboBitsInOrder ByteOrder=DefaultByteOrder
 LineOrder=riloTopToBottom
 BitsPerPixel=32 LineEnd=rileDWordBoundary
 RedPrec=8 RedShift=16 GreenPrec=8 GreenShift=8 BluePrec=8 BlueShift=0
 No alpha
 No mask
}
begin
  // setup an artificial ScanLineImage with format RGB 24 bit, 32bit depth format
  FillChar(Self, SizeOf(Self), 0);

  Format := ricfRGBA;
  Depth := 24; // used bits per pixel
  Width := AWidth;
  Height := AHeight;
  BitOrder := riboBitsInOrder;
  ByteOrder := riboLSBFirst;
  LineOrder := riloTopToBottom;
  BitsPerPixel := 32; // bits per pixel. can be greater than Depth.
  LineEnd := rileDWordBoundary;
  RedPrec := 8; // red precision. bits for red
  RedShift := 16;
  GreenPrec := 8;
  GreenShift := 8; // bitshift. Direction: from least to most signifikant
  BluePrec := 8;
//  BlueShift := 0;
//  AlphaPrec := 0;
//  MaskBitsPerPixel:=0;
end;

procedure TRawImageDescription.Init_BPP32_B8G8R8_M1_BIO_TTB(AWidth, AHeight: integer);
{ pf32bit:

 Format=ricfRGBA HasPalette=false Depth=24 PaletteColorCount=0
 BitOrder=riboBitsInOrder ByteOrder=DefaultByteOrder
 LineOrder=riloTopToBottom
 BitsPerPixel=32 LineEnd=rileDWordBoundary
 RedPrec=8 RedShift=16 GreenPrec=8 GreenShift=8 BluePrec=8 BlueShift=0
 no alpha
 with mask
}
begin
  // setup an artificial ScanLineImage with format RGB 24 bit, 32bit depth format
  FillChar(Self, SizeOf(Self), 0);

  Format := ricfRGBA;
  Depth := 24; // used bits per pixel
  Width := AWidth;
  Height := AHeight;
  BitOrder := riboBitsInOrder;
  ByteOrder := riboLSBFirst;
  LineOrder := riloTopToBottom;
  BitsPerPixel := 32; // bits per pixel. can be greater than Depth.
  LineEnd := rileDWordBoundary;
  RedPrec := 8; // red precision. bits for red
  RedShift := 16;
  GreenPrec := 8;
  GreenShift := 8; // bitshift. Direction: from least to most signifikant
  BluePrec := 8;
//  BlueShift := 0;
//  AlphaPrec := 0;
  MaskBitsPerPixel := 1;
  MaskBitOrder := riboBitsInOrder;
//  MaskShift := 0;        // the shift (=position) of the mask bit
  MaskLineEnd := rileWordBoundary;
end;

function TRawImageDescription.MaskBitsPerLine: PtrUInt;
begin
  Result := GetBitsPerLine(Width, MaskBitsPerPixel, MaskLineEnd);
end;

function TRawImageDescription.MaskBytesPerLine: PtrUInt;
begin
  Result := (GetBitsPerLine(Width, MaskBitsPerPixel, MaskLineEnd) + 7) shr 3;
end;

procedure TRawImageDescription.Init_BPP32_B8G8R8A8_BIO_TTB(AWidth, AHeight: integer);
{ pf32bit:

 Format=ricfRGBA HasPalette=false Depth=32 PaletteColorCount=0
 BitOrder=riboBitsInOrder ByteOrder=DefaultByteOrder
 LineOrder=riloTopToBottom
 BitsPerPixel=32 LineEnd=rileDWordBoundary
 RedPrec=8 RedShift=16 GreenPrec=8 GreenShift=8 BluePrec=8 BlueShift=0
 alpha
 no mask
}
begin
  // setup an artificial ScanLineImage with format RGB 32 bit, 32bit depth format
  FillChar(Self, SizeOf(Self), 0);

  Format := ricfRGBA;
  Depth := 32; // used bits per pixel
  Width := AWidth;
  Height := AHeight;
  BitOrder := riboBitsInOrder;
  ByteOrder := riboLSBFirst;
  LineOrder := riloTopToBottom;
  BitsPerPixel := 32; // bits per pixel. can be greater than Depth.
  LineEnd := rileDWordBoundary;
  RedPrec := 8; // red precision. bits for red
  RedShift := 16;
  GreenPrec := 8;
  GreenShift := 8; // bitshift. Direction: from least to most signifikant
  BluePrec := 8;
  BlueShift := 0;
  AlphaPrec := 8;
  AlphaShift := 24;
//  MaskBitsPerPixel := 0;
end;

procedure TRawImageDescription.Init_BPP32_B8G8R8A8_M1_BIO_TTB(AWidth, AHeight: integer);
begin
  Init_BPP32_B8G8R8A8_BIO_TTB(AWidth, AHeight);

  MaskBitsPerPixel := 1;
  MaskBitOrder := riboBitsInOrder;
  MaskLineEnd := rileWordBoundary;
end;


function TRawImageDescription.GetDescriptionFromMask: TRawImageDescription;
begin
  Result{%H-}.Init;

  Result.Format       := ricfGray;
  Result.Width        := Width;
  Result.Height       := Height;
  Result.Depth        := 1; // per def
  Result.BitOrder     := MaskBitOrder;
  Result.ByteOrder    := DefaultByteOrder;
  Result.LineOrder    := LineOrder;
  Result.LineEnd      := MaskLineEnd;
  Result.BitsPerPixel := MaskBitsPerPixel;
  Result.RedPrec      := 1;
  Result.RedShift     := MaskShift;
end;

function TRawImageDescription.BitsPerLine: PtrUInt;
begin
  Result := GetBitsPerLine(Width, BitsPerPixel, LineEnd);
end;

function TRawImageDescription.BytesPerLine: PtrUInt;
begin
  Result := (GetBitsPerLine(Width, BitsPerPixel, LineEnd) + 7) shr 3;
end;

function TRawImageDescription.GetDescriptionFromAlpha: TRawImageDescription;
begin
  Result{%H-}.Init;

  Result.Format       := ricfGray;
  Result.Width        := Width;
  Result.Height       := Height;
  Result.Depth        := AlphaPrec;
  Result.BitOrder     := BitOrder;
  Result.ByteOrder    := ByteOrder;
  Result.LineOrder    := LineOrder;
  Result.LineEnd      := LineEnd;
  Result.BitsPerPixel := BitsPerPixel;
  Result.RedPrec      := AlphaPrec;
  Result.RedShift     := AlphaShift;
end;

procedure TRawImageDescription.GetRGBIndices(out Ridx, Gidx, Bidx, Aidx: Byte);
const
                        // riboLSBFirst, riboMSBFirst
  COMPONENT_MASK: array[TRawImageByteOrder] of Byte = (0, 3);
begin
  Ridx := (RedShift shr 3) xor COMPONENT_MASK[ByteOrder];
  Gidx := (GreenShift shr 3) xor COMPONENT_MASK[ByteOrder];
  Bidx := (BlueShift shr 3) xor COMPONENT_MASK[ByteOrder];
  Aidx := (AlphaShift shr 3) xor COMPONENT_MASK[ByteOrder];
end;

function TRawImageDescription.AsString: string;

  function EnumToString(AEnum: TRawImageColorFormat): string;
  begin
    WriteStr(Result, AEnum);
  end;

  function EnumToString(AEnum: TRawImageByteOrder): string;
  begin
    WriteStr(Result, AEnum);
  end;

  function EnumToString(AEnum: TRawImageBitOrder): string;
  begin
    WriteStr(Result, AEnum);
  end;

  function EnumToString(AEnum: TRawImageLineEnd): string;
  begin
    WriteStr(Result, AEnum);
  end;

  function EnumToString(AEnum: TRawImageLineOrder): string;
  begin
    WriteStr(Result, AEnum);
  end;

begin
  Result:=
     ' Format='+EnumToString(Format)
    +' HasPalette->'+dbgs(PaletteColorCount <> 0)
    +' HasMask->'+dbgs(PaletteColorCount <> 0)
    +' Depth='+IntToStr(Depth)
    +' Width='+IntToStr(Width)
    +' Height='+IntToStr(Height)
    +' BitOrder='+EnumToString(BitOrder)
    +' ByteOrder='+EnumToString(ByteOrder)
    +' LineOrder='+EnumToString(LineOrder)
    +' LineEnd='+EnumToString(LineEnd)
    +' BitsPerPixel='+IntToStr(BitsPerPixel)
    +' BytesPerLine->'+IntToStr(GetBytesPerLine(Width,BitsPerPixel,LineEnd))
    +' RedPrec='+IntToStr(RedPrec)
    +' RedShift='+IntToStr(RedShift)
    +' GreenPrec='+IntToStr(GreenPrec)
    +' GreenShift='+IntToStr(GreenShift)
    +' BluePrec='+IntToStr(BluePrec)
    +' BlueShift='+IntToStr(BlueShift)
    +' AlphaPrec='+IntToStr(AlphaPrec)
    +' AlphaShift='+IntToStr(AlphaShift)
    +' ~~~mask~~~'
    +' MaskBitsPerPixel='+IntToStr(MaskBitsPerPixel)
    +' MaskShift='+IntToStr(MaskShift)
    +' MaskLineEnd='+EnumToString(MaskLineEnd)
    +' MaskBitOrder='+EnumToString(MaskBitOrder)
    +' MaskBytesPerLine->'+IntToStr(GetBytesPerLine(Width,MaskBitsPerPixel,MaskLineEnd))
    +' ~~~palette~~~'
    +' PaletteColorCount='+IntToStr(PaletteColorCount)
    +' PaletteBitsPerIndex='+IntToStr(PaletteBitsPerIndex)
    +' PaletteShift='+IntToStr(PaletteShift)
    +' PaletteLineEnd='+EnumToString(PaletteLineEnd)
    +' PaletteBitOrder='+EnumToString(PaletteBitOrder)
    +' PaletteByteOrder='+EnumToString(PaletteByteOrder)
    +' PaletteBytesPerLine->'+IntToStr(GetBytesPerLine(Width,PaletteBitsPerIndex,PaletteLineEnd))
    +'';
end;

function TRawImageDescription.IsEqual(ADesc: TRawImageDescription): Boolean;
begin
// We cannot use CompareMem since some fields are depending on other fields
//  Result := CompareMem(@Self, @ADescription, SizeOf(Self));
  Result := False;
  
  if Format       <> ADesc.Format       then Exit;
  if Width        <> ADesc.Width        then Exit;
  if Height       <> ADesc.Height       then Exit;
  if Depth        <> ADesc.Depth        then Exit;
  if BitOrder     <> ADesc.BitOrder     then Exit;
  if ByteOrder    <> ADesc.ByteOrder    then Exit;
  if LineOrder    <> ADesc.LineOrder    then Exit;
  if LineEnd      <> ADesc.LineEnd      then Exit;
  if BitsPerPixel <> ADesc.BitsPerPixel then Exit;
  if RedPrec      <> ADesc.RedPrec      then Exit;
  if GreenPrec    <> ADesc.GreenPrec    then Exit;
  if BluePrec     <> ADesc.BluePrec     then Exit;
  if AlphaPrec    <> ADesc.AlphaPrec    then Exit;

  // The next values are only valid, if there is a precision
  if (RedPrec   <> 0) and (RedShift   <> ADesc.RedShift  ) then Exit;
  if Format = ricfRGBA
  then begin
    // for mono images only red is of importance
    if (GreenPrec <> 0) and (GreenShift <> ADesc.GreenShift) then Exit;
    if (BluePrec  <> 0) and (BlueShift  <> ADesc.BlueShift ) then Exit;
  end;
  if (AlphaPrec <> 0) and (AlphaShift <> ADesc.AlphaShift) then Exit;

  // The next values are only valid, if there is a mask (MaskBitsPerPixel > 0)
  if MaskBitsPerPixel <> ADesc.MaskBitsPerPixel then Exit;
  if MaskBitsPerPixel <> 0
  then begin
    if MaskShift    <> ADesc.MaskShift    then Exit;
    if MaskLineEnd  <> ADesc.MaskLineEnd  then Exit;
    if MaskBitOrder <> ADesc.MaskBitOrder then Exit;
  end;

  // The next values are only valid, if there is a palette (PaletteColorCount > 0)
  if PaletteColorCount <> ADesc.PaletteColorCount   then Exit;
  if PaletteColorCount <> 0
  then begin
    if PaletteBitsPerIndex <> ADesc.PaletteBitsPerIndex then Exit;
    if PaletteShift        <> ADesc.PaletteShift        then Exit;
    if PaletteLineEnd      <> ADesc.PaletteLineEnd      then Exit;
    if PaletteBitOrder     <> ADesc.PaletteBitOrder     then Exit;
    if PaletteByteOrder    <> ADesc.PaletteByteOrder    then Exit;
  end;
  Result := True;
end;

{ TRawImage }

function TRawImage.IsMasked(ATestPixels: Boolean): Boolean;

  function CheckMask: boolean;
  var
    Width: cardinal;
    Height: cardinal;
    UsedBitsPerLine: cardinal;
    TotalBits: Cardinal;
    TotalBitsPerLine: cardinal;
    TotalBytesPerLine: cardinal;
    UnusedBitsAtEnd: Byte;
    UnusedBytesAtEnd: Byte;
    P: PCardinal;
    LinePtr: PByte;
    x, y, xEnd: Integer;
    EndMask: Cardinal; // No mask bits should be set. The Cardinal at line end
                       // can contain some unused bits. This mask AND cardinal
                       // at line end makes the unsused bits all 0.

    procedure CreateEndMask;
    begin
      if Description.MaskBitOrder = riboBitsInOrder
      then EndMask := ($FF shr UnusedBitsAtEnd)
      else EndMask := ($FF shl UnusedBitsAtEnd) and $FF;
      // add unused bytes
      {$ifdef endian_big}
      // read in memory -> [??][eM][uu][uu]
      EndMask := ($FFFFFF00 or EndMask) shl (UnusedBytesAtEnd shl 3);
      {$else}
      // read in memory -> [uu][uu][eM][??]
      EndMask := ((EndMask shl 24) or $00FFFFFF) shr (UnusedBytesAtEnd shl 3);
      {$endif}
    end;
    
    // separate dump procs to avoid code flow cluttering
    // added here in case somone want to debug
    {$IFDEF VerboseRawImage}
    procedure DumpFull;
    begin
      DebugLn('RawImageMaskIsEmpty FullByte y=',dbgs(y),' x=',dbgs(x),' Byte=',DbgS(p^));
    end;

    procedure DumpEdge;
    begin
      DebugLn('RawImageMaskIsEmpty EdgeByte y=',dbgs(y),' x=',dbgs(x),
        ' Byte=',HexStr(Cardinal(p^),2),
        //' UnusedMask=',HexStr(Cardinal(UnusedMask),2),
        //' OR='+dbgs(p^ or UnusedMask),
        ' UnusedBitsAtEnd='+dbgs(UnusedBitsAtEnd),
        ' UsedBitsPerLine='+dbgs(UsedBitsPerLine),
        ' Width='+dbgs(Width),
        ' ARawImage.Description.MaskBitsPerPixel='+dbgs(Description.MaskBitsPerPixel));
    end;
    {$endif}

  begin
    Result := True;
    
    Width := Description.Width;
    Height := Description.Height;

    TotalBitsPerLine := GetBitsPerLine(Width, Description.MaskBitsPerPixel, Description.MaskLineEnd);
    TotalBits := Height * TotalBitsPerLine;
    if MaskSize < PtrUInt((TotalBits + 7) shr 3)
    then raise Exception.Create('RawImage_IsMasked - Invalid MaskSize');

    UsedBitsPerLine := Width * Description.MaskBitsPerPixel;
    UnusedBitsAtEnd := TotalBitsPerLine - UsedBitsPerLine;

    if UnusedBitsAtEnd = 0
    then begin
      // the next line follows the previous one, so we can compare the whole
      // memblock in one go

      P := PCardinal(Mask);
      for x := 1 to TotalBits shr 5 do
      begin
        if p^ <> 0 then Exit;
        Inc(p);
      end;

      // redefine UnusedBitsAtEnd as the bits at the end of the block
      UnusedBitsAtEnd := TotalBits and $1F;
      if UnusedBitsAtEnd <> 0
      then begin
        // check last piece
        UnusedBytesAtEnd := UnusedBitsAtEnd shr 3;
        // adjust to byte bounds
        UnusedBitsAtEnd := UnusedBitsAtEnd and 7;
        CreateEndMask;

        if p^ and EndMask <> 0 then Exit;
      end;
    end
    else begin
      // scan each line
      TotalBytesPerLine := TotalBitsPerLine shr 3;
      UnusedBytesAtEnd := UnusedBitsAtEnd shr 3;

      // Number of cardinals to check
      xEnd := (TotalBytesPerLine - UnusedBytesAtEnd) shr 2;
      
      // Adjust unused to only the last checked
      UnusedBytesAtEnd := UnusedBytesAtEnd and 3;
      UnusedBitsAtEnd := UnusedBitsAtEnd and 7;

      // create mask for the last bits
      CreateEndMask;

      LinePtr := Mask;
      for y := 0 to Height - 1 do
      begin
        p := PCardinal(LinePtr);
        for x := 0 to xEnd - 1 do
        begin
          if p^ <> 0 then Exit;
          Inc(p);
        end;
        // check last end
        if (EndMask <> 0) and (p^ and EndMask <> 0) then Exit;

        Inc(LinePtr, TotalBytesPerLine);
      end;
    end;
    Result := False;
  end;
  
begin
  Result := False;
  //DebugLn('RawImageMaskIsEmpty Quicktest: empty ',dbgs(RawImage^.Description.Width),'x',dbgs(RawImage^.Description.Height));

  // quick test
  if (Mask = nil)
  or (MaskSize = 0)
  or (Description.MaskBitsPerPixel = 0)
  or (Description.Width = 0)
  or (Description.Height = 0)
  then begin
    {$IFDEF VerboseRawImage}
    DebugLn('RawImageMaskIsEmpty Quicktest: empty');
    {$ENDIF}
    exit;
  end;

  if ATestPixels
  then begin
    Result := CheckMask;

    {$IFDEF VerboseRawImage}
    DebugLn('RawImageMaskIsEmpty Empty=',dbgs(not Result));
    {$ENDIF}
  end
  else begin
    Result := True;
    {$IFDEF VerboseRawImage}
    DebugLn('RawImageMaskIsEmpty NoPixelTest: not empty');
    {$ENDIF}
    Exit;
  end;
end;

function TRawImage.IsTransparent(ATestPixels: Boolean): Boolean;
  function CheckAlpha: Boolean;
  begin
    {$IFDEF VerboseLCLTodos}{$note TODO: implement CheckAlpha}{$ENDIF}
    Result := True;
  end;
begin
  Result :=
    (Data <> nil) and
    (DataSize <> 0) and
    (Description.AlphaPrec <> 0) and
    (Description.Width = 0) and
    (Description.Height = 0);

  if Result and ATestPixels then
    Result := CheckAlpha;
end;

function TRawImage.ReadBits(const APosition: TRawImagePosition; APrec, AShift: Byte): Word;
begin
  RawImage_ReadBits(Data, APosition, Description.BitsPerPixel, APrec, AShift, Description.BitOrder, Result);
end;

procedure TRawImage.ReadChannels(const APosition: TRawImagePosition; out ARed, AGreen, ABlue, AAlpha: Word);
var
  D: TRawImageDescription absolute Description;
begin
  case Description.Format of
    ricfRGBA: begin
      RawImage_ReadBits(Data, APosition, D.BitsPerPixel, D.RedPrec, D.RedShift, D.BitOrder, ARed);
      RawImage_ReadBits(Data, APosition, D.BitsPerPixel, D.GreenPrec, D.GreenShift, D.BitOrder, AGreen);
      RawImage_ReadBits(Data, APosition, D.BitsPerPixel, D.BluePrec, D.BlueShift, D.BitOrder, ABlue);
    end;

    ricfGray: begin
      RawImage_ReadBits(Data, APosition, D.BitsPerPixel, D.RedPrec, D.RedShift, D.BitOrder, ARed);
      AGreen := ARed;
      ABlue := ARed;
    end;

  else
    ARed := 0;
    AGreen := 0;
    ABlue := 0;
    AAlpha := 0;
    Exit;
  end;
  
  if D.AlphaPrec > 0
  then RawImage_ReadBits(Data, APosition, D.BitsPerPixel, D.AlphaPrec, D.AlphaShift, D.BitOrder, AAlpha)
  else AAlpha := High(AAlpha);
end;

procedure TRawImage.ReadMask(const APosition: TRawImagePosition; out AMask: Boolean);
var
  D: TRawImageDescription absolute Description;
  M: Word;
begin
  if (D.MaskBitsPerPixel > 0) and (Mask <> nil)
  then begin
    RawImage_ReadBits(Mask, APosition, D.MaskBitsPerPixel, 1, D.MaskShift, D.MaskBitOrder, M);
    AMask := M <> 0;
  end
  else AMask := False;
end;

procedure TRawImage.FreeData;
begin
  FreeMem(Data);
  Data := nil;
  DataSize:=0;

  FreeMem(Mask);
  Mask := nil;
  MaskSize := 0;
  
  FreeMem(Palette);
  Palette := nil;
  PaletteSize:=0;
end;

procedure TRawImage.Init;
begin
  Description.Init;
  Data := nil;
  DataSize:=0;
  Mask := nil;
  MaskSize := 0;
  Palette := nil;
  PaletteSize:=0;
end;

function TRawImage.IsEqual(AImage: TRawImage): Boolean;
begin
  //Result := CompareMem(@Self, @AImage, SizeOf(Self));
  Result :=
    Description.IsEqual(AImage.Description) and
    (DataSize = AImage.DataSize) and
    (MaskSize = AImage.MaskSize) and
    (PaletteSize = AImage.PaletteSize);

  if Result then
    Result := CompareMem(Data, AImage.Data, DataSize);
  if Result then
    Result := CompareMem(Mask, AImage.Mask, MaskSize);
  if Result then
    Result := CompareMem(Palette, AImage.Palette, PaletteSize);
end;

procedure TRawImage.ReleaseData;
begin
  Data := nil;
  DataSize := 0;
  Mask := nil;
  MaskSize := 0;
  Palette := nil;
  PaletteSize := 0;
end;

procedure TRawImage.WriteBits(const APosition: TRawImagePosition; APrec, AShift: Byte; ABits: Word);
begin
  RawImage_WriteBits(Data, APosition, Description.BitsPerPixel, APrec, AShift, Description.BitOrder, ABits);
end;

procedure TRawImage.WriteChannels(const APosition: TRawImagePosition; ARed, AGreen, ABlue, AAlpha: Word);
var
  D: TRawImageDescription absolute Description;
begin
  case D.Format of
    ricfRGBA: begin
      RawImage_WriteBits(Data, APosition, D.BitsPerPixel, D.RedPrec, D.RedShift, D.BitOrder, ARed);
      RawImage_WriteBits(Data, APosition, D.BitsPerPixel, D.GreenPrec, D.GreenShift, D.BitOrder, AGreen);
      RawImage_WriteBits(Data, APosition, D.BitsPerPixel, D.BluePrec, D.BlueShift, D.BitOrder, ABlue);
    end;

    ricfGray: begin
      RawImage_WriteBits(Data, APosition, D.BitsPerPixel, D.RedPrec, D.RedShift, D.BitOrder, ARed);
    end;
  else
    Exit;
  end;

  if D.AlphaPrec = 0 then Exit;

  RawImage_WriteBits(Data, APosition, D.BitsPerPixel, D.AlphaPrec, D.AlphaShift, D.BitOrder, AAlpha);
end;

procedure TRawImage.WriteMask(const APosition: TRawImagePosition; AMask: Boolean);
const
  M: array[Boolean] of Word = (0, $FFFF);
var
  D: TRawImageDescription absolute Description;
begin
  if Mask = nil then Exit;
  if D.MaskBitsPerPixel = 0 then Exit;
  RawImage_WriteBits(Mask, APosition, D.MaskBitsPerPixel, 1, D.MaskShift, D.MaskBitOrder, M[AMask]);
end;

procedure TRawImage.CreateData(AZeroMem: Boolean);
var
  Size: QWord;
begin
  // get current size
  if Description.Width = 0 then Exit;
  if Description.Height = 0 then Exit;

  // calculate size
  with Description do
    Size := GetBitsPerLine(Width, BitsPerPixel, LineEnd);
  Size := (Size * Description.Height) shr 3;
  
  if Size < High(DataSize)
  then DataSize := Size
  else DataSize := High(DataSize);

  ReAllocMem(Data, DataSize);

  if AZeroMem
  then FillChar(Data^, DataSize, 0);
  
  // Setup mask if needed
  if Description.MaskBitsPerPixel = 0 then Exit;
  
  // calculate mask size
  with Description do
    Size := GetBitsPerLine(Width, MaskBitsPerPixel, MaskLineEnd);
  Size := (Size * Description.Height) shr 3;

  if Size < High(MaskSize)
  then MaskSize := Size
  else MaskSize := High(MaskSize);

  ReAllocMem(Mask, MaskSize);

  if AZeroMem
  then FillChar(Mask^, MaskSize, 0);
end;

procedure TRawImage.ExtractRect(const ARect: TRect; out ADst: TRawImage);
  procedure ExtractData(AData: PByte; ADataSize: PtrUInt; ABitsPerPixel: Byte;
                        ABitOrder: TRawImageBitOrder; ALineEnd: TRawImageLineEnd;
                        ADest: PByte; ADestSize: PtrUInt);
  var
    SrcWidth, SrcHeight, SrcRight: LongInt;
    DstWidth, DstHeight: LongInt;
    x, y: Integer;
    LineOrder: TRawImageLineOrder;
    SrcLineStarts, DstLineStarts: TRawImageLineStarts;
    SrcStartPos, SrcEndPos, DstStartPos: TRawImagePosition;
    Shift0, Shift1: Byte;
    DstW1: Word;
    SrcPos: PByte;
    DstPos: PByte;
    ByteCount: PtrUInt;
  begin
    SrcWidth := Description.Width;
    DstWidth := ADst.Description.Width;
    LineOrder := Description.LineOrder;

    //DebugLn'ExtractRawImageDataRect data=',DbgS(DestData),' Size=',DestDataSize);
    if (SrcWidth = DstWidth) and (ARect.Top = 0)
    then begin
      if LineOrder = riloTopToBottom
      then // copy whole source from beginning
        System.Move(AData[0], ADest[0], ADestSize)
      else // copy remainder
        System.Move(AData[ADataSize - ADestSize], ADest[0], ADestSize);
      Exit;
    end;
    
    SrcHeight := Description.Height;
    DstHeight := ADst.Description.Height;

    // calculate line starts
    if LineOrder = riloTopToBottom
    then // we only need the first part from start
      SrcLineStarts.Init(SrcWidth, ARect.Top + DstHeight, ABitsPerPixel, ALineEnd, LineOrder)
    else
      SrcLineStarts.Init(SrcWidth, SrcHeight - ARect.Top, ABitsPerPixel, ALineEnd, LineOrder);
    DstLineStarts.Init(DstWidth, DstHeight, ABitsPerPixel, ALineEnd, LineOrder);

    // copy
    SrcRight := ARect.Left + DstWidth;
    for y := 0 to DstHeight - 1 do
    begin
      SrcStartPos := SrcLineStarts.GetPosition(ARect.Left, y + ARect.Top);
      SrcEndPos   := SrcLineStarts.GetPosition(SrcRight, y + ARect.Top);
      DstStartPos := DstLineStarts.GetPosition(0, y);
      
      //DebugLn'ExtractRawImageDataRect A y=',y,' SrcByte=',SrcLineStartPosition.Byte,' SrcBit=',SrcLineStartPosition.Bit,
      //' DestByte=',DestLineStartPosition.Byte,' DestBit=',DestLineStartPosition.Bit);

      if  (SrcStartPos.Bit = 0) and (DstStartPos.Bit = 0)
      then begin
        // copy bytes
        ByteCount := SrcEndPos.Byte - SrcStartPos.Byte;
        if SrcEndPos.Bit > 0
        then Inc(ByteCount);
          
        //DebugLn'ExtractRawImageDataRect B ByteCount=',ByteCount);
        System.Move(AData[SrcStartPos.Byte], ADest[DstStartPos.Byte], ByteCount);
      end
      else if DstStartPos.Bit = 0
      then begin
        // copy and move bits
        ByteCount := (DstWidth * ABitsPerPixel + 7) shr 3;
        SrcPos := @AData[SrcStartPos.Byte];
        DstPos := @ADest[DstStartPos.Byte];
        Shift0 := SrcStartPos.Bit;
        Shift1 := 8 - Shift0;

        if ABitOrder = riboBitsInOrder
        then begin
          // src[byte|bit]: 07 06 05 04 03 02 01 00 :: 17 16 15 14 13 12 11 10 :
          // imagine startbit = 3 ->
          // dst[byte|bit]: 12 11 10 07 06 05 04 03 :
          for x := 0 to ByteCount - 1 do
          begin
            DstW1 := SrcPos[0] shr Shift0;
            DstPos^ := Byte(DstW1 or (SrcPos[1] shl Shift1));
            inc(SrcPos);
            inc(DstPos);
          end;
        end
        else begin
          // src[byte|bit]: 07 06 05 04 03 02 01 00 :: 17 16 15 14 13 12 11 10 :
          // imagine startbit = 3 ->
          // dst[byte|bit]: 04 03 02 01 00 17 16 15 :
          for x := 0 to ByteCount - 1 do
          begin
            DstW1 := SrcPos[0] shl Shift0;
            DstPos^ := Byte(DstW1 or (SrcPos[1] shr Shift1));
            inc(SrcPos);
            inc(DstPos);
          end;
        end;
      end
      else begin
        break;
      end;
    end;
  end;

var
  R: TRect;
begin
  //DebugLn'ExtractRawImageRect SrcRawImage=',RawImageDescriptionAsString(@SrcRawImage^.Description),
  //  ' SrcRect=',SrcRect.Left,',',SrcRect.Top,',',SrcRect.Right,',',SrcRect.Bottom);

  // copy description
  ADst.Description := Description;
  ADst.ReleaseData;

  // get intersection
  IntersectRect(R, Rect(0, 0, Description.Width, Description.Height), ARect);
  ADst.Description.Width := R.Right - R.Left;
  ADst.Description.Height := R.Bottom - R.Top;
  if (ADst.Description.Width <= 0)
  or (ADst.Description.Height <= 0)
  then begin
    ADst.Description.Width := 0;
    ADst.Description.Height := 0;
    Exit;
  end;
  
  if Data = nil then Exit;
  if DataSize = 0 then Exit;
    
  // allocate some space
  ADst.CreateData(False);

  // extract rectangle from Data
  ExtractData(Data, DataSize, Description.BitsPerPixel, Description.BitOrder,
              Description.LineEnd, ADst.Data, ADst.DataSize);

  // extract rectangle from MAsk

  if Description.MaskBitsPerPixel = 0 then Exit;
  if Mask = nil then Exit;
  if MaskSize = 0 then Exit;


  //DebugLn'ExtractRawImageRect Mask SrcRawImage=',RawImageDescriptionAsString(@SrcMaskDesc));
  ExtractData(Mask, MaskSize, Description.MaskBitsPerPixel, Description.MaskBitOrder,
              Description.MaskLineEnd, ADst.Mask, ADst.MaskSize);
end;

function TRawImage.GetLineStart(ALine: Cardinal): PByte;
begin
  Result := Data;
  if Result = nil then Exit;
  if ALine = 0 then Exit;
  Inc(Result, ALine * Description.BytesPerLine);
end;

procedure TRawImage.PerformEffect(const ADrawEffect: TGraphicsDrawEffect;
  CreateNewData: Boolean; FreeOldData: boolean);
  
  function CheckDescription: Boolean;
  begin
    Result :=
      (Description.Format = ricfRGBA) and
      (Description.PaletteColorCount = 0) and
      (Description.MaskBitsPerPixel = 0) and
      (Description.Depth = 32) and
      (Description.BitOrder = riboBitsInOrder) and
      (Description.ByteOrder = riboMSBFirst) and
      (Description.LineOrder = riloTopToBottom) and
      (Description.BitsPerPixel = 32) and
      (Description.RedPrec = 8) and
      (Description.RedShift = 8) and
      (Description.GreenPrec = 8) and
      (Description.GreenShift = 16) and
      (Description.BluePrec = 8) and
      (Description.BlueShift = 24) and
      (Description.AlphaPrec = 8) and
      (Description.AlphaShift = 0);
  end;
  
const
  Glow = 68;
  Shadow = 48;
  GlowColorMultiplier = (256 - Glow) / 256;
  ShadowColorMultiplier = (256 - Shadow) / 256;
// 1 Bit color weights. Total weight = 1000
   R_Weight: Word = $00DE;
   G_Weight: Word = $02C3;
   B_Weight: Word = $0047;
   H_Threshold = $D5; // threshold of highlight ($D5 is value from experiments. $80 is standard)

var
  AData: PRGBAQuad;
  P: Pointer;
  i, j: integer;
begin
  // check here for Description. Only RGBA data can be processed here.
  if not CheckDescription then
    Exit;

  if CreateNewData then
  begin
    GetMem(AData, DataSize);
    Move(Data^, AData^, DataSize);
    P := AData;
  end
  else begin
    P := Data;
    AData := P;
  end;

  case ADrawEffect of
    gdeNormal: ;
    gdeDisabled:
      begin
        for i := 0 to Description.Height - 1 do
          for j := 0 to Description.Width - 1 do
          begin
            with AData^ do
            begin
              Red := (Red + Green + Blue) div 3;
              Green := Red;
              Blue := Red;
            end;
            inc(AData);
          end;
      end;
    gdeHighlighted:
      begin
        for i := 0 to Description.Height - 1 do
          for j := 0 to Description.Width - 1 do
          begin
            with AData^ do
            begin
              Red := Round(Glow + Red * GlowColorMultiplier);
              Green := Round(Glow + Green * GlowColorMultiplier);
              Blue := Round(Glow + Blue * GlowColorMultiplier);
            end;
            inc(AData);
          end;
      end;
    gdeShadowed:
      begin
        for i := 0 to Description.Height - 1 do
          for j := 0 to Description.Width - 1 do
          begin
            with AData^ do
            begin
              Red := Round(Red * ShadowColorMultiplier);
              Green := Round(Green * ShadowColorMultiplier);
              Blue := Round(Blue * ShadowColorMultiplier);
            end;
            inc(AData);
          end;
      end;
    gde1Bit:
      begin
        for i := 0 to Description.Height - 1 do
          for j := 0 to Description.Width - 1 do
          begin
            with AData^ do
            begin
              // color should be either black or none
              Alpha := ord
                (
                  ((R_Weight * Red + G_Weight * Green + B_Weight * Blue) < H_Threshold * 1000) and
                  (Alpha >= $80)
                ) * $FF;
              if Alpha = $FF then
              begin
                Red := 00;
                Green := 00;
                Blue := 00;
              end;
            end;
            inc(AData);
          end;
      end;
  end;

  if FreeOldData then
    ReAllocMem(Data,0);
  Data := P;
end;

{ TRawImageLineStarts }

function TRawImageLineStarts.GetPosition(x, y: cardinal): TRawImagePosition;
var
  BitOffset: Cardinal;
begin
  if FLineOrder = riloBottomToTop then
    y := FHeight - y - 1;
  Result := Positions[y];
  BitOffset := x * FBitsPerPixel + Result.Bit;
  Result.Bit := BitOffset and 7;
  Inc(Result.Byte, BitOffset shr 3);
end;

procedure TRawImageLineStarts.Init(AWidth, AHeight: cardinal; ABitsPerPixel: Byte; ALineEnd: TRawImageLineEnd; ALineOrder: TRawImageLineOrder);
var
  PixelCount: cardinal;
  BitsPerLine: cardinal;
  CurLine: cardinal;
  BytesPerLine: cardinal;
  ExtraBitsPerLine: Byte;
  CurBitOffset: Byte;
  LoopBit: Byte;
  LoopByte: PtrUInt;
begin
  FWidth := AWidth;
  FHeight := AHeight;
  FBitsPerPixel := ABitsPerPixel;
  FLineEnd := ALineEnd;
  FLineOrder := ALineOrder;
  
  // get current size
  PixelCount := AWidth * AHeight;
  if PixelCount = 0 then exit;

  // calculate BitsPerLine, BytesPerLine and ExtraBitsPerLine
  BitsPerLine := GetBitsPerLine(AWidth, ABitsPerPixel, ALineEnd);
  BytesPerLine := BitsPerLine shr 3;
  ExtraBitsPerLine := BitsPerLine and 7;

  // create line start array
  SetLength(Positions, AHeight);

  Positions[0].Byte := 0;
  Positions[0].Bit := 0;
  LoopBit := 0;
  LoopByte := 0;
  for CurLine := 1 to AHeight-1 do
  begin
    CurBitOffset := LoopBit + ExtraBitsPerLine;
    LoopByte := LoopByte + BytesPerLine + (CurBitOffset shr 3);
    LoopBit := CurBitOffset and 7;
    Positions[CurLine].Byte := LoopByte;
    Positions[CurLine].Bit := LoopBit;
  end;
end;

//------------------------------------------------------------------------------
procedure InternalInit;
var
  Prec: Integer;
  HighValue: word;
  Bits: word;
  CurShift, DShift: Integer;
begin
  for Prec := 0 to 15 do
  begin
    for HighValue := 0 to 7 do
    begin
      // HighValue represents the three highest bits
      // For example:
      //   Prec=5 and the read value is %10110
      //   => HighValue=%101
      // copy the HighValue till all missing bits are set
      // For example:
      //   Prec=5, HighValue=%110
      // => MissingBits[5,6]:=%0000011011011011
      //   because 00000 110 110 110 11
      MissingBits[Prec, HighValue] := 0;
      if Prec = 0 then Continue;

      if Prec>=3 then begin
        DShift := 3;
        Bits := HighValue;
      end else begin
        DShift := Prec;
        Bits := HighValue shr (3-Prec);
      end;
      
      CurShift := 16 - Prec;
      while CurShift > 0 do
      begin
        //DebugLn(['InternalInit CurShift=',CurShift,' DShift=',DShift]);
        if CurShift >= DShift then
          MissingBits[Prec, HighValue] :=
            word(MissingBits[Prec, HighValue] or (Bits shl (CurShift - DShift)))
        else
          MissingBits[Prec, HighValue] :=
            word(MissingBits[Prec, HighValue] or (Bits shr (DShift - CurShift)));
        Dec(CurShift, DShift);
      end;
    end;
  end;
end;

initialization
  InternalInit;

end.
