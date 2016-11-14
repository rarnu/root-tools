{  $Id: intfgraphics.pas 48555 2015-03-31 17:10:38Z juha $  }
{
 /***************************************************************************
                              intfgraphics.pp
                              ---------------

 ***************************************************************************/

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Author: Mattias Gaertner

  Abstract:
    Classes and functions for easy handling of raw images (interface images).
}

//=== ct9999 MOD for CodeTyphon Studio ===========================

unit IntfGraphics;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpImage, FPReadBMP, FPWriteBMP, BMPComn, FPCAdds,
  AvgLvlTree, LCLType, Math,
  LCLProc, GraphType, FPReadPNG, FPWritePNG, FPReadTiff, FPWriteTiff, FPTiffCmn,
  IcnsTypes;

type
  { TLazIntfImage }
  { This descendent of TFPCustomImage stores its image data as raw images and
    is therefore able to directly interchange images with the LCL interfaces.

    Usage examples:

    1. Loading a .xpm file into a TBitmap:

      var
        BmpHnd,MaskHnd: HBitmap;
        Bitmap1: TBitmap;
        IntfImg1: TLazIntfImage;
        Reader: TLazReaderXPM;
      begin
        // create a bitmap (or use an existing one)
        Bitmap1:=TBitmap.Create;
        // create the raw image for the screenformat you want
        IntfImg1:=TLazIntfImage.Create(0,0,[riqfRGB, riqfAlpha, riqfMask]);
        // create the XPM reader
        Reader:=TLazReaderXPM.Create;
        // load the image
        IntfImg1.LoadFromFile('filename.xpm',Reader);
        // create the bitmap handles
        IntfImg1.CreateBitmap(BmpHnd,MaskHnd);
        // apply handles to the Bitmap1
        Bitmap1.Handle:=BmpHnd;
        Bitmap1.MaskHandle:=MaskHnd;
        // clean up
        Reader.Free;
        IntfImg1.Free;
        // do something with the Bitmap1
        ...
      end;


    2. Saving a TBitmap to a .xpm file:

      var
        BmpHnd,MaskHnd: HBitmap;
        Bitmap1: TBitmap;
        IntfImg1: TLazIntfImage;
        Writer: TLazWriterXPM;
      begin
        ...
        // create the raw image
        IntfImg1:=TLazIntfImage.Create(0,0,[]);
        // load the raw image from the bitmap handles
        IntfImg1.LoadFromBitmap(Bitmap1.Handle,Bitmap1.MaskHandle);
        // create the XPM writer
        Writer:=TLazWriterXPM.Create;
        // save image to file
        IntfImg1.SaveToFile('filename.xpm',Writer);
        // clean up
        Writer.Free;
        IntfImg1.Free;
        ...
      end;
    }

  TLazIntfImageGetPixelProc = procedure(x, y: integer; out Color: TFPColor) of object;
  TLazIntfImageSetPixelProc = procedure(x, y: integer; const Color: TFPColor) of object;

  TOnReadRawImageBits = procedure(TheData: PByte;
    const Position: TRawImagePosition;
    Prec, Shift: cardinal; var Bits: word);

  TOnWriteRawImageBits = procedure(TheData: PByte;
    const Position: TRawImagePosition;
    Prec, Shift: cardinal; Bits: word);


  { TLazIntfImage }

  TLazIntfImage = class(TFPCustomImage)
  private
    FRawImage: TRawImage;
    FLineStarts: PRawImageLineStarts;
    FMaskLineStarts: PRawImageLineStarts;
    FMaskSet: Boolean; // Set when at least one maskpixel is set
    FUpdateCount: integer;
    fCreateAllDataNeeded: boolean;
    FGetSetColorFunctionsUpdateNeeded: boolean;
    FReadRawImageBits: TOnReadRawImageBits;
    FWriteRawImageBits: TOnWriteRawImageBits;
    FMaskReadRawImageBits: TOnReadRawImageBits;
    FMaskWriteRawImageBits: TOnWriteRawImageBits;
    FDataOwner: Boolean;
    function GetMasked(x, y: integer): Boolean;
    function GetTColors(x, y: integer): TGraphicsColor;

    procedure InternalSetSize(AWidth, AHeight: integer);

    procedure SetMasked(x, y: integer; const AValue: Boolean);
    procedure SetTColors(x, y: integer; const AValue: TGraphicsColor);
  protected
    FGetInternalColorProc: TLazIntfImageGetPixelProc;
    FSetInternalColorProc: TLazIntfImageSetPixelProc;
    procedure SetUsePalette (Value: boolean); override;
    procedure SetInternalColor(x, y: integer; const Value: TFPColor); override;
    function  GetInternalColor(x, y: integer): TFPColor; override;
    procedure SetInternalPixel (x,y:integer; Value:integer); override;
    function  GetInternalPixel (x,y:integer) : integer; override;
    procedure FreeData; virtual;
    procedure SetDataDescription(const ADescription: TRawImageDescription); virtual;
    procedure ChooseGetSetColorFunctions; virtual;
    procedure ChooseRawBitsProc(BitsPerPixel: cardinal;
                                ByteOrder: TRawImageByteOrder;
                                BitOrder: TRawImageBitOrder;
                                out ProcReadRawImageBits: TOnReadRawImageBits;
                                out ProcWriteRawImageBits: TOnWriteRawImageBits);
    // get color functions
    procedure GetColor_Generic(x, y: integer; out Value: TFPColor);
    procedure GetColor_RGBA_NoPalette(x, y: integer; out Value: TFPColor);
    procedure GetColor_RGB_NoPalette(x, y: integer; out Value: TFPColor);
    procedure GetColor_Gray_NoPalette(x, y: integer; out Value: TFPColor);
    procedure GetColor_GrayAlpha_NoPalette(x, y: integer; out Value: TFPColor);
    procedure GetColor_NULL(x, y: integer; out Value: TFPColor);
    // 32 bpp alpha
    procedure GetColor_BPP32_A8R8G8B8_BIO_TTB(x, y: integer; out Value: TFPColor);
    procedure GetColor_BPP32_A8B8G8R8_BIO_TTB(x, y: integer; out Value: TFPColor);
    procedure GetColor_BPP32_A8G8R8B8_BIO_TTB(x, y: integer; out Value: TFPColor);
    procedure GetColor_BPP32_A8G8B8R8_BIO_TTB(x, y: integer; out Value: TFPColor);
    procedure GetColor_BPP32_A8R8B8G8_BIO_TTB(x, y: integer; out Value: TFPColor);
    procedure GetColor_BPP32_A8B8R8G8_BIO_TTB(x, y: integer; out Value: TFPColor);
    procedure GetColor_BPP32_B8G8R8A8_BIO_TTB(x, y: integer; out Value: TFPColor);
    procedure GetColor_BPP32_R8G8B8A8_BIO_TTB(x, y: integer; out Value: TFPColor);
    procedure GetColor_BPP32_G8B8R8A8_BIO_TTB(x, y: integer; out Value: TFPColor);
    procedure GetColor_BPP32_G8R8B8A8_BIO_TTB(x, y: integer; out Value: TFPColor);
    procedure GetColor_BPP32_B8R8G8A8_BIO_TTB(x, y: integer; out Value: TFPColor);
    procedure GetColor_BPP32_R8B8G8A8_BIO_TTB(x, y: integer; out Value: TFPColor);
    // 32 bpp no alpha
    procedure GetColor_BPP32_X8R8G8B8_BIO_TTB(x, y: integer; out Value: TFPColor);
    procedure GetColor_BPP32_X8B8G8R8_BIO_TTB(x, y: integer; out Value: TFPColor);
    procedure GetColor_BPP32_X8G8R8B8_BIO_TTB(x, y: integer; out Value: TFPColor);
    procedure GetColor_BPP32_X8G8B8R8_BIO_TTB(x, y: integer; out Value: TFPColor);
    procedure GetColor_BPP32_X8R8B8G8_BIO_TTB(x, y: integer; out Value: TFPColor);
    procedure GetColor_BPP32_X8B8R8G8_BIO_TTB(x, y: integer; out Value: TFPColor);
    procedure GetColor_BPP32_B8G8R8X8_BIO_TTB(x, y: integer; out Value: TFPColor);
    procedure GetColor_BPP32_R8G8B8X8_BIO_TTB(x, y: integer; out Value: TFPColor);
    procedure GetColor_BPP32_G8B8R8X8_BIO_TTB(x, y: integer; out Value: TFPColor);
    procedure GetColor_BPP32_G8R8B8X8_BIO_TTB(x, y: integer; out Value: TFPColor);
    procedure GetColor_BPP32_B8R8G8X8_BIO_TTB(x, y: integer; out Value: TFPColor);
    procedure GetColor_BPP32_R8B8G8X8_BIO_TTB(x, y: integer; out Value: TFPColor);
    // 24 bpp
    procedure GetColor_BPP24_B8G8R8_BIO_TTB(x, y: integer; out Value: TFPColor);
    procedure GetColor_BPP24_R8G8B8_BIO_TTB(x, y: integer; out Value: TFPColor);
    procedure GetColor_BPP24_G8B8R8_BIO_TTB(x, y: integer; out Value: TFPColor);
    procedure GetColor_BPP24_G8R8B8_BIO_TTB(x, y: integer; out Value: TFPColor);
    procedure GetColor_BPP24_B8R8G8_BIO_TTB(x, y: integer; out Value: TFPColor);
    procedure GetColor_BPP24_R8B8G8_BIO_TTB(x, y: integer; out Value: TFPColor);

    procedure GetMask_Generic(x, y: integer; out AValue: Boolean);

    // set color functions
    procedure SetColor_Generic(x, y: integer; const Value: TFPColor);
    procedure SetColor_RGBA_NoPalette(x, y: integer; const Value: TFPColor);
    procedure SetColor_RGB_NoPalette(x, y: integer; const Value: TFPColor);
    procedure SetColor_Gray_NoPalette(x, y: integer; const Value: TFPColor);
    procedure SetColor_GrayAlpha_NoPalette(x, y: integer; const Value: TFPColor);
    procedure SetColor_NULL(x, y: integer; const Value: TFPColor);
    // 32 bpp alpha
    procedure SetColor_BPP32_A8R8G8B8_BIO_TTB(x, y: integer; const Value: TFPColor);
    procedure SetColor_BPP32_A8B8G8R8_BIO_TTB(x, y: integer; const Value: TFPColor);
    procedure SetColor_BPP32_A8G8R8B8_BIO_TTB(x, y: integer; const Value: TFPColor);
    procedure SetColor_BPP32_A8G8B8R8_BIO_TTB(x, y: integer; const Value: TFPColor);
    procedure SetColor_BPP32_A8R8B8G8_BIO_TTB(x, y: integer; const Value: TFPColor);
    procedure SetColor_BPP32_A8B8R8G8_BIO_TTB(x, y: integer; const Value: TFPColor);
    procedure SetColor_BPP32_B8G8R8A8_BIO_TTB(x, y: integer; const Value: TFPColor);
    procedure SetColor_BPP32_R8G8B8A8_BIO_TTB(x, y: integer; const Value: TFPColor);
    procedure SetColor_BPP32_G8B8R8A8_BIO_TTB(x, y: integer; const Value: TFPColor);
    procedure SetColor_BPP32_G8R8B8A8_BIO_TTB(x, y: integer; const Value: TFPColor);
    procedure SetColor_BPP32_B8R8G8A8_BIO_TTB(x, y: integer; const Value: TFPColor);
    procedure SetColor_BPP32_R8B8G8A8_BIO_TTB(x, y: integer; const Value: TFPColor);
    // 32 bpp no alpha
    procedure SetColor_BPP32_X8R8G8B8_BIO_TTB(x, y: integer; const Value: TFPColor);
    procedure SetColor_BPP32_X8B8G8R8_BIO_TTB(x, y: integer; const Value: TFPColor);
    procedure SetColor_BPP32_X8G8R8B8_BIO_TTB(x, y: integer; const Value: TFPColor);
    procedure SetColor_BPP32_X8G8B8R8_BIO_TTB(x, y: integer; const Value: TFPColor);
    procedure SetColor_BPP32_X8R8B8G8_BIO_TTB(x, y: integer; const Value: TFPColor);
    procedure SetColor_BPP32_X8B8R8G8_BIO_TTB(x, y: integer; const Value: TFPColor);
    procedure SetColor_BPP32_B8G8R8X8_BIO_TTB(x, y: integer; const Value: TFPColor);
    procedure SetColor_BPP32_R8G8B8X8_BIO_TTB(x, y: integer; const Value: TFPColor);
    procedure SetColor_BPP32_G8B8R8X8_BIO_TTB(x, y: integer; const Value: TFPColor);
    procedure SetColor_BPP32_G8R8B8X8_BIO_TTB(x, y: integer; const Value: TFPColor);
    procedure SetColor_BPP32_B8R8G8X8_BIO_TTB(x, y: integer; const Value: TFPColor);
    procedure SetColor_BPP32_R8B8G8X8_BIO_TTB(x, y: integer; const Value: TFPColor);
    // 24 bpp
    procedure SetColor_BPP24_B8G8R8_BIO_TTB(x, y: integer; const Value: TFPColor);
    procedure SetColor_BPP24_R8G8B8_BIO_TTB(x, y: integer; const Value: TFPColor);
    procedure SetColor_BPP24_G8B8R8_BIO_TTB(x, y: integer; const Value: TFPColor);
    procedure SetColor_BPP24_G8R8B8_BIO_TTB(x, y: integer; const Value: TFPColor);
    procedure SetColor_BPP24_B8R8G8_BIO_TTB(x, y: integer; const Value: TFPColor);
    procedure SetColor_BPP24_R8B8G8_BIO_TTB(x, y: integer; const Value: TFPColor);

    procedure SetMask_Generic(x, y: integer; const AValue: Boolean);
  public
    constructor Create(AWidth, AHeight: integer); override;
    constructor Create(AWidth, AHeight: integer; AFlags: TRawImageQueryFlags);
    constructor Create(ARawImage: TRawImage; ADataOwner: Boolean);
    constructor CreateCompatible(IntfImg: TLazIntfImage; AWidth, AHeight: integer);
    destructor Destroy; override;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure SetSize(AWidth, AHeight: integer); override;
    function CheckDescription(const ADescription: TRawImageDescription;
                              ExceptionOnError: boolean): boolean; virtual;
    procedure LoadFromDevice(DC: HDC); virtual;
    procedure LoadFromBitmap(ABitmap, AMaskBitmap: HBitmap; AWidth: integer = -1; AHeight: integer = -1); virtual;
    procedure CreateBitmaps(out ABitmap, AMask: HBitmap; ASkipMask: boolean = False); virtual;
    procedure SetRawImage(const ARawImage: TRawImage; ADataOwner: Boolean = True); virtual;
    procedure GetRawImage(out ARawImage: TRawImage; ATransferOwnership: Boolean = False); virtual;
    procedure FillPixels(const Color: TFPColor); virtual;
    procedure CopyPixels(ASource: TFPCustomImage; XDst: Integer = 0; YDst: Integer = 0;
                         AlphaMask: Boolean = False; AlphaTreshold: Word = 0); virtual;
    procedure AlphaBlend(ASource, ASourceAlpha: TLazIntfImage; const ADestX, ADestY: Integer);
    procedure AlphaFromMask(AKeepAlpha: Boolean = True);
    procedure Mask(const AColor: TFPColor; AKeepOldMask: Boolean = False);
    procedure GetXYDataPosition(x, y: integer; out Position: TRawImagePosition);
    procedure GetXYMaskPosition(x, y: integer; out Position: TRawImagePosition);
    function  GetDataLineStart(y: integer): Pointer;// similar to Delphi TBitmap.ScanLine. Only works with lines aligned to whole bytes.
    procedure CreateData; virtual;
    function  HasTransparency: boolean; virtual;
    function  HasMask: boolean; virtual;
    procedure SetDataDescriptionKeepData(const ADescription: TRawImageDescription);
  public
    property PixelData: PByte read FRawImage.Data;
    property MaskData: PByte read FRawImage.Mask;
    property DataDescription: TRawImageDescription read FRawImage.Description
                                                   write SetDataDescription;
    property TColors[x,y: integer]: TGraphicsColor read GetTColors write SetTColors;
    property Masked[x,y:integer]: Boolean read GetMasked write SetMasked;
  end;
  

  { TLazIntfImageMask }
  
  TLazIntfImageMask = class(TFPCustomImage)
  private
    FImage: TLazIntfImage;
  protected
    procedure SetInternalColor(x, y: integer; const Value: TFPColor); override;
    function GetInternalColor(x, y: integer): TFPColor; override;
    procedure SetInternalPixel (x,y:integer; Value:integer); override;
    function GetInternalPixel (x,y:integer) : integer; override;
  public
    constructor CreateWithImage(TheImage: TLazIntfImage); virtual;
    property Image: TLazIntfImage read FImage;
  end;


  { TLazAVLPalette }
  { This descendent of TFPPalette uses an AVL tree for speed up. }

  TLazAVLPalette = class(TFPPalette)
  protected
    FAVLPalette: TAvgLvlTree; // tree of PLazAVLPaletteEntry 'color to index'
    FAVLNodes: PAvgLvlTreeNode;// 'index to node' array
    procedure SetCount(NewCount: integer); override;
    procedure SetColor(Index: integer; const NewColor: TFPColor); override;
    function CompareEntries(Index1, Index2: integer): integer;
    function CompareColorWithEntries(const AColor: TFPColor;
                                     Index: integer): integer;
    procedure EnlargeData; override;
  public
    destructor Destroy; override;
    function IndexOf(const AColor: TFPColor): integer; override;
    function Add(const NewColor: TFPColor): integer; override;
    procedure CheckConsistency; virtual;
  end;


  { TArrayNodesTree }

  PArrayNode = ^TArrayNode;
  TArrayNode = class
  public
    Parent: TArrayNode;
    Value: integer;
    Children: PArrayNode;
    StartValue: integer;
    Capacity: integer;
    Data: Pointer;
    constructor Create;
    destructor Destroy; override;
    procedure DeleteChilds;
    procedure UnbindFromParent;
    procedure CreateChildNode(ChildValue: integer);
    function GetChildNode(ChildValue: integer;
                          CreateIfNotExists: boolean): TArrayNode;
    procedure Expand(ValueToInclude: integer);
    function FindPrevSibling: TArrayNode;
    function FindNextSibling: TArrayNode;
    function FindNextUTF8: TArrayNode;
    function FindPrev: TArrayNode;
    function FindFirstChild: TArrayNode;
    function FindLastChild: TArrayNode;
    function FindLastSubChild: TArrayNode;
    function FindFirstSibling: TArrayNode;
    function FindLastSibling: TArrayNode;
    procedure ConsistencyCheck;
  end;

  TArrayNodesTree = class
  public
    Root: TArrayNode;
    function FindNode(Path: PInteger; Count: integer): TArrayNode;
    function FindData(Path: PInteger; Count: integer): Pointer;
    function SetNode(Path: PInteger; Count: integer;
                     Data: Pointer): TArrayNode;
    procedure Delete(Node: TArrayNode);
    procedure Clear;
    constructor Create;
    destructor Destroy; override;
    procedure ConsistencyCheck;
  end;
  
  
  { ILazImageReader }
  { Extension to TFPCustomImageReader to initialize a TRawImgeDescription based
    on the image to be read
  }
  
  ILazImageReader = interface
    ['{DD8B14DE-4E97-4816-8B40-DD6C4D8CCD1B}']
    function GetUpdateDescription: Boolean;
    procedure SetUpdateDescription(AValue: Boolean);

    property UpdateDescription: Boolean read GetUpdateDescription write SetUpdateDescription;
  end;

  { ILazImageWriter }
  { Extension to TFPCustomImageWriter to initialize the writer based
    on the intfimage data. To be able to write different formats, the writer
    should initialize itself
  }
  ILazImageWriter = interface
    ['{DFE8D2A0-E318-45CE-87DE-9C6F1F1928E5}']
    procedure Initialize(AImage: TLazIntfImage);
    procedure Finalize;
  end;


  { TLazReaderXPM }
  { This is a FPImage reader for xpm images. }

  TLazReaderXPM = class(TFPCustomImageReader, ILazImageReader)
  private
    FWidth: Integer;
    FHeight: Integer;
    FColorCount: Integer;
    FCharsPerPixel: Integer;
    FXHot: Integer;
    FYHot: Integer;
    FPixelToColorTree: TArrayNodesTree;
    FContinue: Boolean;
    FUpdateDescription: Boolean; // If set, update rawimagedescription
  public
    function  GetUpdateDescription: Boolean;
    procedure SetUpdateDescription(AValue: Boolean);
    function QueryInterface(constref iid: TGuid; out obj): LongInt; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    function _AddRef: LongInt; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    function _Release: LongInt; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
  protected
    procedure ClearPixelToColorTree;
    procedure InternalRead(Str: TStream; Img: TFPCustomImage); override;
    function  InternalCheck(Str: TStream): boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    property UpdateDescription: Boolean read GetUpdateDescription write SetUpdateDescription;
  end;


  { TLazWriterXPM }
  { This is a FPImage writer for xpm images. }

  TLazWriterXPM = class(TFPCustomImageWriter)
  private
    FNibblesPerSample: word;
    FRightShiftSample: cardinal;
    FContinue: Boolean;
    procedure SetNibblesPerSample(const AValue: word);
  protected
    procedure InternalWrite(Str: TStream; Img: TFPCustomImage); override;
  public
    constructor Create; override;
    property NibblesPerSample: word read FNibblesPerSample
                                    write SetNibblesPerSample;
  end;
  
  
  { TLazReaderDIB }
  { This is an imroved FPImage reader for dib images. }

  TLazReaderMaskMode = (
    lrmmNone,  // no mask is generated
    lrmmAuto,  // a mask is generated based on the first pixel read (*)
    lrmmColor  // a mask is generated based on the given color (*)
  );
  // (*) Note: when reading images with an alpha channel and the alpha channel
  //           has no influence on the mask (unless the maskcolor is transparent)

  TLazReaderDIBEncoding = (
    lrdeRGB,
    lrdeRLE,
    lrdeBitfield,
    lrdeJpeg,     // for completion, don't know if they exist
    lrdePng,      // for completion, don't know if they exist
    lrdeHuffman   // for completion, don't know if they exist
  );

  TLazReaderDIBInfo = record
    Width: Cardinal;
    Height: Cardinal;
    BitCount: Byte;
    Encoding: TLazReaderDIBEncoding;
    PaletteCount: Word;
    UpsideDown: Boolean;
    PixelMasks: packed record
      R, G, B, A: LongWord;
    end;
    MaskShift: record
      R, G, B, A: Byte;
    end;
    MaskSize: record
      R, G, B, A: Byte;
    end;
  end;

  { TLazReaderDIB }

  TLazReaderDIB = class (TFPCustomImageReader, ILazImageReader)
  private
    FImage: TLazIntfImage;
  
    FMaskMode: TLazReaderMaskMode;
    FMaskColor: TFPColor; // color which should be interpreted as masked
    FMaskIndex: Integer;  // for palette based images, index of the color which should be interpreted as masked

    FReadSize: Integer;          // Size (in bytes) of 1 scanline.
    FDIBinfo: TLazReaderDIBInfo; // Info about the bitmap as read from the stream
    FPalette: array of TFPColor; // Buffer with Palette entries.
    FLineBuf: PByte;             // Buffer for 1 scanline. Can be Byte, Word, TColorRGB or TColorRGBA
    FUpdateDescription: Boolean; // If set, update rawimagedescription
    FContinue: Boolean;          // for progress support
    FIgnoreAlpha: Boolean;       // if alpha-channel is declared but does not exists (all values = 0)

    function BitfieldsToFPColor(const AColor: Cardinal): TFPcolor;
    function RGBToFPColor(const AColor: TColorRGBA): TFPcolor;
    function RGBToFPColor(const AColor: TColorRGB): TFPcolor;
    function RGBToFPColor(const AColor: Word): TFPcolor;

  public
    function  GetUpdateDescription: Boolean;
    procedure SetUpdateDescription(AValue: Boolean);
    function QueryInterface(constref iid: TGuid; out obj): LongInt; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    function _AddRef: LongInt; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    function _Release: LongInt; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
  protected
    procedure InitLineBuf;
    procedure FreeLineBuf;


    procedure ReadScanLine(Row: Integer); virtual;
    procedure WriteScanLine(Row: Cardinal); virtual;
    // required by TFPCustomImageReader
    procedure InternalRead(Stream: TStream; Img: TFPCustomImage); override;
    procedure InternalReadHead; virtual;
    procedure InternalReadBody; virtual;
    function  InternalCheck(Stream: TStream) : boolean; override;
    
    property ReadSize: Integer read FReadSize;
    property LineBuf: PByte read FLineBuf;
    property Info: TLazReaderDIBInfo read FDIBInfo;
  public
    constructor Create; override;
    destructor Destroy; override;
    property MaskColor: TFPColor read FMaskColor write FMaskColor;
    property MaskMode: TLazReaderMaskMode read FMaskMode write FMaskMode;
    property UpdateDescription: Boolean read GetUpdateDescription write SetUpdateDescription;
  end;
  
  { TLazReaderBMP }

  TLazReaderBMP = class(TLazReaderDIB)
  private
    FDataOffset: Int64; // some bitmaps can specify the data offset
  protected
    function  InternalCheck(Stream: TStream) : boolean; override;
    procedure InternalReadHead; override;
  end;

  { TLazWriterBMP }

  TLazWriterBMP = class(TFPWriterBMP, ILazImageWriter)
  public

    function QueryInterface(constref iid: TGuid; out obj): LongInt; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    function _AddRef: LongInt; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    function _Release: LongInt; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};

  public
    procedure Initialize(AImage: TLazIntfImage);
    procedure Finalize;
  end;


  { TLazReaderIconDIB }
  { This is a FPImage reader for a single DIB from an icon file }
  TLazReaderIconDIB = class (TLazReaderDIB)
  protected
    procedure InternalRead(Stream: TStream; Img: TFPCustomImage); override;
  end;



  { TLazReaderPNG }

  TLazReaderPNG = class(TFPReaderPNG, ILazImageReader)
  private
    FAlphaPalette: Boolean;
    FUpdateDescription: Boolean;
  public
    function  GetUpdateDescription: Boolean;
    procedure SetUpdateDescription(AValue: Boolean);
    function QueryInterface(constref iid: TGuid; out obj): LongInt; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    function _AddRef: LongInt; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    function _Release: LongInt; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
  protected
    procedure DoDecompress; override;
    procedure HandleAlpha; override;
    procedure InternalRead(Str:TStream; Img:TFPCustomImage); override;
  public
    property UpdateDescription: Boolean read GetUpdateDescription write SetUpdateDescription;
  end;

  { TLazWriterPNG }

  TLazWriterPNG = class(TFPWriterPNG, ILazImageWriter)
  public
    function QueryInterface(constref iid: TGuid; out obj): LongInt; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    function _AddRef: LongInt; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    function _Release: LongInt; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
  public
    procedure Initialize(AImage: TLazIntfImage);
    procedure Finalize;
  end;

  { TLazReaderTiff }

const
  LazTiffExtraPrefix = 'LazTiff';
  LazTiffHostComputer = LazTiffExtraPrefix + 'HostComputer';
  LazTiffMake = LazTiffExtraPrefix + 'Make';
  LazTiffModel = LazTiffExtraPrefix + 'Model';
  LazTiffSoftware = LazTiffExtraPrefix + 'Software';

type
  {$IF FPC_FULLVERSION<20601}
  {$DEFINE OldTiffCreateImageHook}
  {$ENDIF}

  TLazReaderTiff = class(TFPReaderTiff, ILazImageReader)
  private
    FUpdateDescription: Boolean;
    {$IFDEF OldTiffCreateImageHook}
    // the OnCreateImage event is "abused" to update the description after the
    // format and before the image is read
    FOrgEvent: TTiffCreateCompatibleImgEvent;
    procedure CreateImageHook(Sender: TFPReaderTiff; var NewImage: TFPCustomImage);
    procedure DoCreateImage(ImgFileDir: TTiffIDF);
    {$ELSE}
  protected
    procedure DoCreateImage(ImgFileDir: TTiffIFD); override;
    {$ENDIF}
  public
    function  GetUpdateDescription: Boolean;
    procedure SetUpdateDescription(AValue: Boolean);

    function QueryInterface(constref iid: TGuid; out obj): LongInt; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    function _AddRef: LongInt; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    function _Release: LongInt; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};

  protected
    procedure InternalRead(Str:TStream; Img:TFPCustomImage); override;
  public
    property UpdateDescription: Boolean read GetUpdateDescription write SetUpdateDescription;
  end;

  { TLazWriterTiff }

  TLazWriterTiff = class(TFPWriterTiff, ILazImageWriter)
  public

    function QueryInterface(constref iid: TGuid; out obj): LongInt; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    function _AddRef: LongInt; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    function _Release: LongInt; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};

  protected
    procedure InternalWrite(Stream: TStream; Img: TFPCustomImage); override;
  public
    procedure Initialize(AImage: TLazIntfImage);
    procedure Finalize;
  end;


  { TLazReaderIcnsPart }

  TLazReaderIcnsPart = class(TFPCustomImageReader, ILazImageReader)
  private
    FUpdateDescription: Boolean;
    FPalette: TFPPalette;
    FImage: TLazIntfImage;
    FData: PByte;
    FCalcSize: Integer;
    FDataSize: Integer;
    FIconType: TicnsIconType;
    FIconInfo: TicnsIconInfo;
  protected
    function  InternalCheck(Str:TStream): boolean; override;
    procedure InternalRead(Stream: TStream; Img: TFPCustomImage); override;
    procedure SetupRead(AWidth, AHeight, ADepth: Integer; IsMask: Boolean);
    function Create256ColorPalette: TFPPalette;
    procedure DoReadRaw;
    procedure DoReadRLE;
    procedure DoReadJpeg2000;
    procedure DoReadMask;
  public
    function GetUpdateDescription: Boolean;
    procedure SetUpdateDescription(AValue: Boolean);
    function QueryInterface(constref iid: TGuid; out obj): LongInt; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    function _AddRef: LongInt; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    function _Release: LongInt; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
  public
    constructor Create; override;
    destructor Destroy; override;
    property UpdateDescription: Boolean read GetUpdateDescription write SetUpdateDescription;
    property IconType: TicnsIconType read FIconType;
    property DataSize: Integer read FDataSize;
  end;

// extra Rawimage utility functions

function QueryDescription(AFlags: TRawImageQueryFlags; AWidth: Integer = -1; AHeight: integer = -1): TRawImageDescription;
procedure QueryDescription(var ADesc: TRawImageDescription; AFlags: TRawImageQueryFlags; AWidth: Integer = -1; AHeight: integer = -1);
function GetDescriptionFromDevice(ADC: HDC; AWidth: Integer = -1; AHeight: integer = -1): TRawImageDescription;
function GetDescriptionFromBitmap(ABitmap: HBitmap; AWidth: Integer = -1; AHeight: integer = -1): TRawImageDescription;
function AddAlphaToDescription(var ADesc: TRawImageDescription; APrec: Byte): Boolean;


procedure DefaultReaderDescription(AWidth, AHeight: Integer; ADepth: Byte; out ADesc: TRawImageDescription);


function ReadCompleteStreamToString(Str: TStream; StartSize: integer): string;
procedure ReadCompleteStreamToStream(SrcStream, DestStream: TStream;
                                     StartSize: integer);
                                     
function dbgs(const FPColor: TFPColor): string; overload;

implementation

uses
  Graphics, LCLIntf;
  
type
  PFPColorBytes = ^TFPColorBytes;
  TFPColorBytes = record
    {$ifdef ENDIAN_LITTLE}
    Rl, Rh, Gl, Gh, Bl, Bh, Al, Ah: Byte;
    {$else}
    Rh, Rl, Gh, Gl, Bh, Bl, Ah, Al: Byte;
    {$endif}
  end;
  
  PFourBytes = ^TFourBytes;
  TFourBytes = record
    B0, B1, B2, B3: Byte;
  end;
  

var
  IsSpaceChar, IsNumberChar, IsHexNumberChar: array[char] of Boolean;

function ReadCompleteStreamToString(Str: TStream; StartSize: integer): string;
var
  NewLength: Integer;
  ReadLen: Integer;
begin
  if (Str is TMemoryStream) or (Str is TFileStream) or (Str is TStringStream)
  then begin
    // read as one block
    SetLength(Result,Str.Size-Str.Position);
    if Result<>'' then
      Str.Read(Result[1],length(Result));
  end else begin
    // read exponential
    if StartSize=0 then StartSize:=1024;
    SetLength(Result,StartSize);
    NewLength:=0;
    repeat
      ReadLen:=Str.Read(Result[NewLength+1],length(Result)-NewLength);
      inc(NewLength,ReadLen);
      if NewLength<length(Result) then break;
      SetLength(Result,length(Result)*2);
    until false;
    SetLength(Result,NewLength);
  end;
end;

procedure ReadCompleteStreamToStream(SrcStream, DestStream: TStream;
                                     StartSize: integer);
var
  NewLength: Integer;
  ReadLen: Integer;
  Buffer: string;
begin
  if (SrcStream is TMemoryStream) or (SrcStream is TFileStream)
  or (SrcStream is TStringStream)
  then begin
    // read as one block
    if DestStream is TMemoryStream then
      TMemoryStream(DestStream).SetSize(DestStream.Size
                                        +(SrcStream.Size-SrcStream.Position));
    DestStream.CopyFrom(SrcStream,SrcStream.Size-SrcStream.Position);
  end else begin
    // read exponential
    if StartSize<=0 then StartSize:=1024;
    SetLength(Buffer,StartSize);
    NewLength:=0;
    repeat
      ReadLen:=SrcStream.Read(Buffer[NewLength+1],length(Buffer)-NewLength);
      inc(NewLength,ReadLen);
      if NewLength<length(Buffer) then break;
      SetLength(Buffer,length(Buffer)*2);
    until false;
    if NewLength>0 then
      DestStream.Write(Buffer[1],NewLength);
  end;
end;

function dbgs(const FPColor: TFPColor): string;
begin
  Result:='r='+hexStr(FPColor.Red,4)+',g='+hexStr(FPColor.green,4)
        +',b='+hexStr(FPColor.blue,4)+',a='+hexStr(FPColor.alpha,4);
end;

function QueryDescription(AFlags: TRawImageQueryFlags; AWidth: Integer = -1; AHeight: integer = -1): TRawImageDescription;
begin
  Exclude(AFlags, riqfUpdate);
  Result{%H-}.Init;
  QueryDescription(Result, AFlags, AWidth, AHeight);
end;

procedure QueryDescription(var ADesc: TRawImageDescription; AFlags: TRawImageQueryFlags; AWidth: Integer = -1; AHeight: integer = -1);
begin
  if RawImage_QueryDescription(AFlags, ADesc)
  then begin
    if AWidth <> -1 then ADesc.Width := AWidth;
    if AHeight <> -1 then ADesc.Height := AHeight;
  end
  else begin
    if not (riqfUpdate in AFlags) then ADesc.Init;
  end;
end;

function GetDescriptionFromDevice(ADC: HDC; AWidth, AHeight: integer): TRawImageDescription;
begin
  Result{%H-}.Init;
  if not RawImage_DescriptionFromDevice(ADC, Result) then Exit;
  if AWidth <> -1 then Result.Width := AWidth;
  if AHeight <> -1 then Result.Height := AHeight;
end;

function GetDescriptionFromBitmap(ABitmap: HBitmap; AWidth: Integer = -1; AHeight: integer = -1): TRawImageDescription;
begin
  Result{%H-}.Init;
  if not RawImage_DescriptionFromBitmap(ABitmap, Result) then Exit;
  if AWidth <> -1 then Result.Width := AWidth;
  if AHeight <> -1 then Result.Height := AHeight;
end;

function AddAlphaToDescription(var ADesc: TRawImageDescription; APrec: Byte): Boolean;
  function CreateBitMask(AShift, APrec: Byte): Cardinal; inline;
  begin
    Result := ($FFFFFFFF shr (32 - APrec)) shl AShift;
  end;
var
  Mask: Cardinal;
begin
  Result:=false;
  if ADesc.AlphaPrec >= APrec then Exit;
  if ADesc.BitsPerPixel <> 32 then Exit;
  if ADesc.Depth <> 24 then Exit;

  Mask := CreateBitMask(ADesc.RedShift, ADesc.RedPrec)
       or CreateBitMask(ADesc.GreenShift, ADesc.GreenPrec)
       or CreateBitMask(ADesc.BlueShift, ADesc.BluePrec);

  if (Mask and $FF = 0)
  then begin
    ADesc.AlphaShift := 0;
    Result := True;
  end
  else
    if (Mask and $FF000000 = 0)
    then begin
      ADesc.AlphaShift := 24;
      Result := True;
    end;
  if Result
  then begin
    ADesc.AlphaPrec := APrec;
    ADesc.Depth := 32;
  end;
end;

procedure CheckAlphaDescription(AImage: TFPCustomImage);
var
  Desc: TRawImageDescription;
begin
  if not (AImage is TLazIntfImage) then Exit;

  Desc := TLazIntfImage(AImage).DataDescription;
  if Desc.AlphaPrec >= 8 then Exit;

  if not AddAlphaToDescription(Desc, 8)
  then begin
    Desc.Init_BPP32_B8G8R8A8_BIO_TTB(Desc.Width, Desc.Height);
    // copy mask description
    with TLazIntfImage(AImage).DataDescription do
    begin
      Desc.MaskBitsPerPixel := MaskBitsPerPixel;
      Desc.MaskShift := MaskShift;
      Desc.MaskLineEnd := MaskLineEnd;
      Desc.MaskBitOrder := MaskBitOrder;
    end;
  end;

  TLazIntfImage(AImage).DataDescription := Desc;
end;

procedure DefaultReaderDescription(AWidth, AHeight: Integer; ADepth: Byte; out ADesc: TRawImageDescription);
begin
  // Default description, assume 24bit for palettebased
  // Maybe when RawImage palette is supported, other descriptions need to be adjusted.

  ADesc.Init_BPP24_B8G8R8_M1_BIO_TTB(AWidth, AHeight);

  case ADepth of
    1: begin
      ADesc.Depth := 1;
      ADesc.BitsPerPixel := 1;
      ADesc.Format := ricfGray;
      ADesc.LineEnd := rileWordBoundary;
      ADesc.RedPrec := 1;
      ADesc.RedShift := 0;
      ADesc.GreenPrec := 1;
      ADesc.GreenShift := 0;
      ADesc.BluePrec := 1;
      ADesc.BlueShift := 0;
    end;
    2..4: begin
//      ADesc.Depth := 4;
//      ADesc.BitsPerPixel := 4;
    end;
    5..8: begin
//      ADesc.Depth := 8;
//      ADesc.BitsPerPixel := 8;
    end;
    9..15: begin
      ADesc.Depth := 15;
      ADesc.BitsPerPixel := 16;
      ADesc.RedPrec := 5;
      ADesc.RedShift := 10;
      ADesc.GreenPrec := 5;
      ADesc.GreenShift := 5;
      ADesc.BluePrec := 5;
      ADesc.BlueShift := 0;
    end;
    16: begin
      ADesc.Depth := 16;
      ADesc.BitsPerPixel := 16;
      ADesc.RedPrec := 5;
      ADesc.RedShift := 10;
      ADesc.GreenPrec := 6;
      ADesc.GreenShift := 5;
      ADesc.BluePrec := 5;
      ADesc.BlueShift := 0;
    end;
    17..24: begin
      // already default
    end;
    25..32 : ADesc.Init_BPP32_B8G8R8A8_BIO_TTB(AWidth,AHeight);
  else
    ADesc.Depth := 32;
    ADesc.BitsPerPixel := 32;
    ADesc.AlphaPrec := 8;
    ADesc.AlphaShift := 24;
  end;
end;

// ReadRawImageBits_* routines are called multiple times, once for each channel
// Therefore Shift means the Shift in the raw image of the channel
// TheData points to beginning of the image data
// Position is the position in bytes to the start of the pixel in TheData
// Prec is the precision of the channel
// Bits is the value of the channel, which is the output


procedure ReadRawImageBits_1_2_4_BIO(TheData: PByte;
  const Position: TRawImagePosition;
  Prec, Shift: cardinal;
  var Bits: word);
var
  P: PByte;
  PrecMask: Cardinal;
  OneByte: Byte;
begin
  PrecMask:=(Cardinal(1) shl Prec)-1;
  P:=@(TheData[Position.Byte]);

  OneByte:=P^;
  Bits:=Word(cardinal(OneByte shr (Shift+Position.Bit)) and PrecMask);

  if Prec<16 then begin
    // add missing bits
    Bits:=(Bits shl (16-Prec));
    Bits:=Bits or MissingBits[Prec,Bits shr 13];
  end;
end;

procedure ReadRawImageBits_1_2_4_BNIO(TheData: PByte;
  const Position: TRawImagePosition;
  Prec, Shift: cardinal;
  var Bits: word);
var
  P: PByte;
  PrecMask: Cardinal;
  OneByte: Byte;
begin
  PrecMask:=(Cardinal(1) shl Prec)-1;
  P:=@(TheData[Position.Byte]);

  OneByte:=P^;
  Bits:=Word(cardinal(OneByte shr (Shift+7-Position.Bit)) and PrecMask);

  if Prec<16 then begin
    // add missing bits
    Bits:=(Bits shl (16-Prec));
    Bits:=Bits or MissingBits[Prec,Bits shr 13];
  end;
end;

procedure ReadRawImageBits_8(TheData: PByte;
  const Position: TRawImagePosition;
  Prec, Shift: cardinal;
  var Bits: word);
var
  P: PByte;
  PrecMask: Cardinal;
  OneByte: Byte;
begin
  PrecMask:=(Cardinal(1) shl Prec)-1;
  P:=@(TheData[Position.Byte]);

  OneByte:=P^;
  Bits:=Word(cardinal(OneByte shr Shift) and PrecMask);

  if Prec<16 then begin
    // add missing bits
    Bits:=(Bits shl (16-Prec));
    Bits:=Bits or MissingBits[Prec,Bits shr 13];
  end;
end;

procedure ReadRawImageBits_16(TheData: PByte;
  const Position: TRawImagePosition;
  Prec, Shift: cardinal;
  var Bits: word);
var
  P: PByte;
  PrecMask: Cardinal;
  TwoBytes: Word;
begin
  PrecMask:=(Cardinal(1) shl Prec)-1;
  P:=@(TheData[Position.Byte]);

  TwoBytes:=PWord(P)^;
  Bits:=Word(cardinal(TwoBytes shr Shift) and PrecMask);

  if Prec<16 then begin
    // add missing bits
    Bits:=(Bits shl (16-Prec));
    Bits:=Bits or MissingBits[Prec,Bits shr 13];
  end;
end;

procedure ReadRawImageBits_ReversedBytes_16(TheData: PByte;
  const Position: TRawImagePosition;
  Prec, Shift: cardinal;
  var Bits: word);
var
  P: PByte;
  PrecMask: Cardinal;
  TwoBytes: Word;
begin
  PrecMask:=(Cardinal(1) shl Prec)-1;
  P:=@(TheData[Position.Byte]);

  TwoBytes:=PWord(P)^;
  TwoBytes:=(TwoBytes shr 8) or ((TwoBytes and $ff) shl 8); // switch byte order
  Bits:=Word(cardinal(TwoBytes shr Shift) and PrecMask);

  if Prec<16 then begin
    // add missing bits
    Bits:=(Bits shl (16-Prec));
    Bits:=Bits or MissingBits[Prec,Bits shr 13];
  end;
end;

procedure ReadRawImageBits_24(TheData: PByte;
  const Position: TRawImagePosition;
  Prec, Shift: cardinal;
  var Bits: word);
var
  P: PByte;
  PrecMask: Cardinal;
  FourBytes: Cardinal;
begin
  PrecMask:=(Cardinal(1) shl Prec)-1;
  P:=@(TheData[Position.Byte]);

  {$ifdef Endian_Little}
  FourBytes:=DWord(PWord(P)^) or (DWord((P+2)^) shl 16);
  {$else}
  FourBytes:=(DWord(PWord(P)^) shl 8) or DWord((P+2)^);
  {$endif}
  Bits:=Word(cardinal(FourBytes shr Shift) and PrecMask);

  if Prec<16 then begin
    // add missing bits
    Bits:=(Bits shl (16-Prec));
    Bits:=Bits or MissingBits[Prec,Bits shr 13];
  end;
end;

procedure ReadRawImageBits_ReversedBytes_24(TheData: PByte;
  const Position: TRawImagePosition;
  Prec, Shift: cardinal;
  var Bits: word);
var
  P: PByte;
  PrecMask: Cardinal;
  FourBytes: Cardinal;
begin
  PrecMask:=(Cardinal(1) shl Prec)-1;
  P:=@(TheData[Position.Byte]);

  {$ifdef Endian_Little}
  FourBytes:=(DWord(PWord(P)^) shl 8) or DWord((P+2)^);
  {$else}
  FourBytes:=DWord(PWord(P)^) or (DWord((P+2)^) shl 16);
  {$endif}

  Bits:=Word(cardinal(FourBytes shr Shift) and PrecMask);

  if Prec<16 then begin
    // add missing bits
    Bits:=(Bits shl (16-Prec));
    Bits:=Bits or MissingBits[Prec,Bits shr 13];
  end;
end;

procedure ReadRawImageBits_32(TheData: PByte;
  const Position: TRawImagePosition;
  Prec, Shift: cardinal;
  var Bits: word);
var
  P: PByte;
  PrecMask: Cardinal;
  FourBytes: Cardinal;
begin
  PrecMask:=(Cardinal(1) shl Prec)-1;
  P:=@(TheData[Position.Byte]);

  FourBytes:=PDWord(P)^;
  Bits:=Word(cardinal(FourBytes shr Shift) and PrecMask);

  if Prec<16 then begin
    // add missing bits
    Bits:=(Bits shl (16-Prec));
    Bits:=Bits or MissingBits[Prec,Bits shr 13];
  end;
end;

procedure ReadRawImageBits_ReversedBytes_32(TheData: PByte;
  const Position: TRawImagePosition;
  Prec, Shift: cardinal;
  var Bits: word);
var
  P: PByte;
  PrecMask: Cardinal;
  FourBytes: Cardinal;
begin
  PrecMask:=(Cardinal(1) shl Prec)-1;
  P:=@(TheData[Position.Byte]);

  FourBytes:=PDWord(P)^;

  // switch byte order
  FourBytes:=(FourBytes shr 24) or ((FourBytes shr 8) and $FF00)
             or ((FourBytes and $ff00) shl 8) or ((FourBytes and $ff) shl 24);

  Bits:=Word(cardinal(FourBytes shr Shift) and PrecMask);

  if Prec<16 then begin
    // add missing bits
    Bits:=(Bits shl (16-Prec));
    Bits:=Bits or MissingBits[Prec,Bits shr 13];
  end;
end;

procedure ReadRawImageBits_48(TheData: PByte;
  const Position: TRawImagePosition;
  Prec, Shift: cardinal;
  var Bits: word);
var
  P: PByte;
  PrecMask: Cardinal;
  EightBytes: QWord;
begin
  PrecMask:=(Cardinal(1) shl Prec)-1;
  P:=@(TheData[Position.Byte]);

  {$ifdef Endian_Little}
  EightBytes:=QWord(PDWord(P)^) or (QWord(PWord(P+4)^) shl 32);
  {$else}
  EightBytes:=(QWord(PDWord(P)^) shl 16) or QWord(PWord(P+4)^);
  {$endif}
  Bits:=Word(cardinal(EightBytes shr Shift) and PrecMask);

  if Prec<16 then begin
    // add missing bits
    Bits:=(Bits shl (16-Prec));
    Bits:=Bits or MissingBits[Prec,Bits shr 13];
  end;
end;

procedure ReadRawImageBits_ReversedBytes_48(TheData: PByte;
  const Position: TRawImagePosition;
  Prec, Shift: cardinal;
  var Bits: word);
var
  P: PByte;
  PrecMask: Cardinal;
  EightBytes: QWord;
begin
  PrecMask:=(Cardinal(1) shl Prec)-1;
  P:=@(TheData[Position.Byte]);

  {$ifdef Endian_Little}
  EightBytes:=(QWord(PDWord(P)^) shl 16) or QWord(PWord(P+4)^);
  {$else}
  EightBytes:=QWord(PDWord(P)^) or (QWord(PWord(P+4)^) shl 32);
  {$endif}
  Bits:=Word(cardinal(EightBytes shr Shift) and PrecMask);

  if Prec<16 then begin
    // add missing bits
    Bits:=(Bits shl (16-Prec));
    Bits:=Bits or MissingBits[Prec,Bits shr 13];
  end;
end;

procedure ReadRawImageBits_64(TheData: PByte;
  const Position: TRawImagePosition;
  Prec, Shift: cardinal;
  var Bits: word);
var
  P: PByte;
  PrecMask: Cardinal;
  EightBytes: QWord;
begin
  PrecMask:=(Cardinal(1) shl Prec)-1;
  P:=@(TheData[Position.Byte]);

  EightBytes:=PQWord(P)^;
  Bits:=Word(Cardinal(EightBytes shr Shift) and PrecMask);

  if Prec<16 then begin
    // add missing bits
    Bits:=(Bits shl (16-Prec));
    Bits:=Bits or MissingBits[Prec,Bits shr 13];
  end;
end;

procedure ReadRawImageBits_ReversedBytes_64(TheData: PByte;
  const Position: TRawImagePosition;
  Prec, Shift: cardinal;
  var Bits: word);
var
  P: PByte;
  PrecMask: Cardinal;
  EightBytes: QWord;
begin
  PrecMask:=(Cardinal(1) shl Prec)-1;
  P:=@(TheData[Position.Byte]);

  EightBytes:=PQWord(P)^;

  // switch byte order
  EightBytes:=swapendian(EightBytes);

  Bits:=Word(Cardinal(EightBytes shr Shift) and PrecMask);

  if Prec<16 then begin
    // add missing bits
    Bits:=(Bits shl (16-Prec));
    Bits:=Bits or MissingBits[Prec,Bits shr 13];
  end;
end;

procedure WriteRawImageBits_1_2_4_BIO(TheData: PByte;
  const Position: TRawImagePosition;
  Prec, Shift: cardinal; Bits: word);
var
  P: PByte;
  PrecMask: Cardinal;
  OneByte: Byte;
  ShiftLeft: Integer;
begin
  P:=@(TheData[Position.Byte]);
  PrecMask:=(Cardinal(1) shl Prec)-1;
  Bits:=Bits shr (16-Prec);

  OneByte:=P^;
  ShiftLeft:=Shift+Position.Bit;
  PrecMask:=not (PrecMask shl ShiftLeft);
  OneByte:=OneByte and PrecMask; // clear old
  OneByte:=OneByte or (Bits shl ShiftLeft); // set new
  P^:=OneByte;
end;

procedure WriteRawImageBits_1_2_4_BNIO(TheData: PByte;
  const Position: TRawImagePosition;
  Prec, Shift: cardinal; Bits: word);
var
  P: PByte;
  PrecMask: Cardinal;
  OneByte: Byte;
  ShiftLeft: Integer;
begin
  P:=@(TheData[Position.Byte]);
  PrecMask:=(Cardinal(1) shl Prec)-1;
  Bits:=Bits shr (16-Prec);

  OneByte:=P^;
  ShiftLeft:=Shift+7-Position.Bit;
  PrecMask:=not (PrecMask shl ShiftLeft);
  OneByte:=OneByte and PrecMask; // clear old
  OneByte:=OneByte or (Bits shl ShiftLeft); // set new
  P^:=OneByte;
end;

procedure WriteRawImageBits_8(TheData: PByte;
  const Position: TRawImagePosition;
  Prec, Shift: cardinal; Bits: word);
var
  P: PByte;
  PrecMask: Cardinal;
  OneByte: Byte;
begin
  P:=@(TheData[Position.Byte]);
  PrecMask:=(Cardinal(1) shl Prec)-1;
  Bits:=Bits shr (16-Prec);

  OneByte:=P^;
  PrecMask:=not (PrecMask shl Shift);
  OneByte:=OneByte and PrecMask; // clear old
  OneByte:=OneByte or (Bits shl Shift); // set new
  P^:=OneByte;
end;

procedure WriteRawImageBits_16(TheData: PByte;
  const Position: TRawImagePosition;
  Prec, Shift: cardinal; Bits: word);
var
  P: PByte;
  PrecMask: Cardinal;
  TwoBytes: Word;
begin
  P:=@(TheData[Position.Byte]);
  PrecMask:=(Cardinal(1) shl Prec)-1;
  Bits:=Bits shr (16-Prec);

  TwoBytes:=PWord(P)^;
  PrecMask:=not (PrecMask shl Shift);
  TwoBytes:=TwoBytes and PrecMask; // clear old
  TwoBytes:=TwoBytes or (Bits shl Shift); // set new
  PWord(P)^:=TwoBytes;
end;

procedure WriteRawImageBits_ReversedBytes_16(TheData: PByte;
  const Position: TRawImagePosition;
  Prec, Shift: cardinal; Bits: word);
var
  P: PByte;
  PrecMask: Cardinal;
  TwoBytes: Word;
begin
  P:=@(TheData[Position.Byte]);
  PrecMask:=(Cardinal(1) shl Prec)-1;
  Bits:=Bits shr (16-Prec);

  TwoBytes:=PWord(P)^;
  TwoBytes:=(TwoBytes shr 8) or ((TwoBytes and $ff) shl 8); // switch byte order
  PrecMask:=not (PrecMask shl Shift);
  TwoBytes:=TwoBytes and PrecMask; // clear old
  TwoBytes:=TwoBytes or (Bits shl Shift); // set new
  TwoBytes:=(TwoBytes shr 8) or ((TwoBytes and $ff) shl 8); // switch byte order
  PWord(P)^:=TwoBytes;
end;

procedure WriteRawImageBits_24(TheData: PByte;
  const Position: TRawImagePosition;
  Prec, Shift: cardinal; Bits: word);
var
  P: PByte;
  PrecMask: Cardinal;
  FourBytes: Cardinal;
begin
  P:=@(TheData[Position.Byte]);
  PrecMask:=(Cardinal(1) shl Prec)-1;
  Bits:=Bits shr (16-Prec);

{$ifdef Endian_Little}
  FourBytes:=DWord(PWord(P)^) or (DWord((P+2)^) shl 16);
{$else}
  FourBytes:=(DWord(PWord(P)^) shl 8) or DWord((P+2)^);
{$endif}
  
  PrecMask:=not (PrecMask shl Shift);
  FourBytes:=FourBytes and PrecMask; // clear old
  FourBytes:=FourBytes or cardinal(Bits shl Shift); // set new

{$ifdef Endian_little}
  PWord(P)^ := Word(FourBytes);
  P[2] := Byte(FourBytes shr 16);
{$else}  
  PWord(P)^ := Word(FourBytes shr 8);
  P[2] := Byte(FourBytes);
{$endif}
end;

procedure WriteRawImageBits_ReversedBytes_24(TheData: PByte;
  const Position: TRawImagePosition;
  Prec, Shift: cardinal; Bits: word);
var
  P: PByte;
  PrecMask: Cardinal;
  FourBytes: Cardinal;
begin
  P:=@(TheData[Position.Byte]);
  PrecMask:=(Cardinal(1) shl Prec)-1;
  Bits:=Bits shr (16-Prec);

{$ifdef Endian_Little}
  FourBytes:=(DWord(PWord(P)^) shl 8) or DWord(P^);
{$else}
  FourBytes:=DWord(PWord(P)^) or (DWord((P+2)^) shl 16);
{$endif}
  
  PrecMask:=not (PrecMask shl Shift);
  FourBytes:=FourBytes and PrecMask; // clear old
  FourBytes:=FourBytes or cardinal(Bits shl Shift); // set new

{$ifdef Endian_little}
  PWord(P)^ := Word(FourBytes shr 8);
  P^ := Byte(FourBytes);
{$else}  
  PWord(P)^ := Word(FourBytes);
  (P+2)^ := Byte(FourBytes shr 16);
{$endif}
end;

procedure WriteRawImageBits_32(TheData: PByte;
  const Position: TRawImagePosition;
  Prec, Shift: cardinal; Bits: word);
var
  P: PByte;
  PrecMask: Cardinal;
  FourBytes: Cardinal;
begin
  if Prec=16
  then begin
    // fast update
    P:=@(TheData[Position.Byte]);
    inc(P,2-Shift shr 3);
    PWORD(P)^:=Bits;
    Exit;
  end;

  P:=@(TheData[Position.Byte]);
  PrecMask:=(Cardinal(1) shl Prec)-1;
  Bits:=Bits shr (16-Prec);

  FourBytes:=PDWord(P)^;
  PrecMask:=not (PrecMask shl Shift);
  FourBytes:=FourBytes and PrecMask; // clear old
  FourBytes:=FourBytes or cardinal(Bits shl Shift); // set new
  PDWord(P)^:=FourBytes;
end;

procedure WriteRawImageBits_ReversedBytes_32(TheData: PByte;
  const Position: TRawImagePosition;
  Prec, Shift: cardinal; Bits: word);
var
  P: PByte;
  PrecMask: Cardinal;
  FourBytes: Cardinal;
begin
  P:=@(TheData[Position.Byte]);
  PrecMask:=(Cardinal(1) shl Prec)-1;
  Bits:=Bits shr (16-Prec);

  FourBytes:=PDWord(P)^;

  // switch byte order
  FourBytes:=(FourBytes shr 24) or ((FourBytes shr 8) and $FF00)
             or ((FourBytes and $ff00) shl 8) or ((FourBytes and $ff) shl 24);

  PrecMask:=not (PrecMask shl Shift);
  FourBytes:=FourBytes and PrecMask; // clear old
  FourBytes:=FourBytes or cardinal(Bits shl Shift); // set new

  // switch byte order
  FourBytes:=(FourBytes shr 24) or ((FourBytes shr 8) and $FF00)
             or ((FourBytes and $ff00) shl 8) or ((FourBytes and $ff) shl 24);
  PDWord(P)^:=FourBytes;
end;

procedure WriteRawImageBits_48(TheData: PByte;
  const Position: TRawImagePosition;
  Prec, Shift: cardinal; Bits: word);
var
  P: PByte;
  PrecMask: QWord;
  EightBytes: QWord;
begin
  P:=@(TheData[Position.Byte]);
  PrecMask:=(QWord(1) shl Prec)-1;
  Bits:=Bits shr (16-Prec);

{$ifdef Endian_Little}
  EightBytes:=QWord(PDWord(P)^) or (QWord(PWord(P+4)^) shl 32);
{$else}
  EightBytes:=(QWord(PDWord(P)^) shl 16) or QWord(PWord(P+4)^);
{$endif}

  PrecMask:=not (PrecMask shl Shift);
  EightBytes:=EightBytes and PrecMask; // clear old
  EightBytes:=EightBytes or QWord(Bits) shl Shift; // set new

{$ifdef Endian_little}
  PDWord(P)^ := DWord(EightBytes);
  PWord(P+4)^ := Word(EightBytes shr 32);
{$else}
  PDWord(P)^ := DWord(EightBytes shr 16);
  PWord(P+4)^ := Word(EightBytes);
{$endif}
end;

procedure WriteRawImageBits_ReversedBytes_48(TheData: PByte;
  const Position: TRawImagePosition;
  Prec, Shift: cardinal; Bits: word);
var
  P: PByte;
  PrecMask: QWord;
  EightBytes: QWord;
begin
  P:=@(TheData[Position.Byte]);
  PrecMask:=(QWord(1) shl Prec)-1;
  Bits:=Bits shr (16-Prec);

{$ifdef Endian_Little}
  EightBytes:=(QWord(PDWord(P)^) shl 16) or QWord(PWord(P+4)^);
{$else}
  EightBytes:=QWord(PDWord(P)^) or (QWord(PWord(P+4)^) shl 32);
{$endif}

  PrecMask:=not (PrecMask shl Shift);
  EightBytes:=EightBytes and PrecMask; // clear old
  EightBytes:=EightBytes or QWord(Bits) shl Shift; // set new

{$ifdef Endian_little}
  PDWord(P)^ := DWord(EightBytes shr 16);
  PWord(P+4)^ := Word(EightBytes);
{$else}
  PDWord(P)^ := DWord(EightBytes);
  PWord(P+4)^ := Word(EightBytes shr 32);
{$endif}
end;

procedure WriteRawImageBits_64(TheData: PByte;
  const Position: TRawImagePosition;
  Prec, Shift: cardinal; Bits: word);
var
  P: PByte;
  PrecMask: QWord;
  EightBytes: QWord;
begin
//--------------------- ct9999
  if Prec=16
  then begin
    // fast update
    P:=@(TheData[Position.Byte]);
    inc(P,6-Shift shr 3);
    PWORD(P)^:=Bits;
    Exit;
  end;
//--------------------  

  P:=@(TheData[Position.Byte]);
  PrecMask:=(Qword(1) shl Prec)-1;
  Bits:=Bits shr (16-Prec);

  EightBytes:=PQWord(P)^;
  PrecMask:=not (PrecMask shl Shift);
  EightBytes:=EightBytes and PrecMask; // clear old
  EightBytes:=EightBytes or QWord(Bits) shl Shift; // set new
  PQWord(P)^:=EightBytes;
end;

procedure WriteRawImageBits_ReversedBytes_64(TheData: PByte;
  const Position: TRawImagePosition;
  Prec, Shift: cardinal; Bits: word);
var
  P: PByte;
  PrecMask: QWord;
  EightBytes: QWord;
begin
  P:=@(TheData[Position.Byte]);
  PrecMask:=(QWord(1) shl Prec)-1;
  Bits:=Bits shr (16-Prec);

  EightBytes:=PQWord(P)^;

  // switch byte order
  EightBytes:=swapendian(EightBytes);

  PrecMask:=not (PrecMask shl Shift);
  EightBytes:=EightBytes and PrecMask; // clear old
  EightBytes:=EightBytes or QWord(Bits) shl Shift; // set new

  // switch byte order
  EightBytes:=swapendian(EightBytes);
  PQWord(P)^:=EightBytes;
end;


procedure ReadRawImageBits_NULL(TheData: PByte;
  const Position: TRawImagePosition;
  Prec, Shift: cardinal;
  var Bits: word);
begin
  Bits:=0;

  if Prec<16 then begin
    // add missing bits
    Bits:=(Bits shl (16-Prec));
    Bits:=Bits or MissingBits[Prec,Bits shr 13];
  end;
end;

procedure WriteRawImageBits_NULL(TheData: PByte;
  const Position: TRawImagePosition;
  Prec, Shift: cardinal; Bits: word);
begin
end;

{ TLazIntfImage }

procedure TLazIntfImage.SetDataDescription(const ADescription: TRawImageDescription);
begin
  if CompareMem(@FRawImage.Description, @ADescription, SizeOf(TRawImageDescription))
  then Exit;

  CheckDescription(ADescription, True);
  BeginUpdate;
  try
    FreeData;
    FRawImage.Description := ADescription;
    ChooseGetSetColorFunctions;
    InternalSetSize(ADescription.Width, ADescription.Height);
    CreateData;
  finally
    EndUpdate;
  end;
end;

procedure TLazIntfImage.ChooseRawBitsProc(BitsPerPixel: cardinal;
  ByteOrder: TRawImageByteOrder; BitOrder: TRawImageBitOrder;
  out ProcReadRawImageBits: TOnReadRawImageBits;
  out ProcWriteRawImageBits: TOnWriteRawImageBits);
begin
  case BitsPerPixel of

  1,2,4:
  begin
    if BitOrder = riboBitsInOrder then
    begin
      ProcReadRawImageBits  := @ReadRawImageBits_1_2_4_BIO;
      ProcWriteRawImageBits := @WriteRawImageBits_1_2_4_BIO;
    end else begin
      ProcReadRawImageBits  := @ReadRawImageBits_1_2_4_BNIO;
      ProcWriteRawImageBits := @WriteRawImageBits_1_2_4_BNIO;
    end;
  end;

  8:
  begin
    ProcReadRawImageBits  := @ReadRawImageBits_8;
    ProcWriteRawImageBits := @WriteRawImageBits_8;
  end;

  16:
  begin
    if DefaultByteOrder=ByteOrder then begin
      ProcReadRawImageBits  := @ReadRawImageBits_16;
      ProcWriteRawImageBits := @WriteRawImageBits_16;
    end else begin
      ProcReadRawImageBits  := @ReadRawImageBits_ReversedBytes_16;
      ProcWriteRawImageBits := @WriteRawImageBits_ReversedBytes_16;
    end;
  end;

  24:
  begin
    if DefaultByteOrder=ByteOrder then begin
      ProcReadRawImageBits  := @ReadRawImageBits_24;
      ProcWriteRawImageBits := @WriteRawImageBits_24;
    end else begin
      ProcReadRawImageBits  := @ReadRawImageBits_ReversedBytes_24;
      ProcWriteRawImageBits := @WriteRawImageBits_ReversedBytes_24;
    end;
  end;

  32:
  begin
    if DefaultByteOrder=ByteOrder then begin
      ProcReadRawImageBits  := @ReadRawImageBits_32;
      ProcWriteRawImageBits := @WriteRawImageBits_32;
    end else begin
      ProcReadRawImageBits  := @ReadRawImageBits_ReversedBytes_32;
      ProcWriteRawImageBits := @WriteRawImageBits_ReversedBytes_32;
    end;
  end;

  48:
  begin
    if DefaultByteOrder=ByteOrder then begin
      ProcReadRawImageBits  := @ReadRawImageBits_48;
      ProcWriteRawImageBits := @WriteRawImageBits_48;
    end else begin
      ProcReadRawImageBits  := @ReadRawImageBits_ReversedBytes_48;
      ProcWriteRawImageBits := @WriteRawImageBits_ReversedBytes_48;
    end;
  end;

  64:
  begin
    if DefaultByteOrder=ByteOrder then begin
      ProcReadRawImageBits  := @ReadRawImageBits_64;
      ProcWriteRawImageBits := @WriteRawImageBits_64;
    end else begin
      ProcReadRawImageBits  := @ReadRawImageBits_ReversedBytes_64;
      ProcWriteRawImageBits := @WriteRawImageBits_ReversedBytes_64;
    end;
  end;

  else

    ProcReadRawImageBits  := @ReadRawImageBits_NULL;
    ProcWriteRawImageBits := @WriteRawImageBits_NULL;
  end;
end;

procedure TLazIntfImage.ChooseGetSetColorFunctions;

  function ChooseRGBA_32Bpp: Boolean;
  var
    Positions: Byte;
  begin
    Result := False;
    with FRawImage.Description do
    begin
      if Depth <> 32 then Exit;
      if BitsPerPixel <> 32 then Exit;
      if LineOrder <> riloTopToBottom then Exit;
      if AlphaPrec <> 8 then Exit;
      if RedPrec <> 8 then Exit;
      if GreenPrec <> 8 then Exit;
      if BluePrec <> 8 then Exit;
      if AlphaShift and 7 <> 0 then Exit;
      if RedShift and 7 <> 0 then Exit;
      if GreenShift and 7 <> 0 then Exit;
      if BlueShift and 7 <> 0 then Exit;

      Positions := (((AlphaShift shr 3) and 3) shl 6
                or ((RedShift shr 3) and 3) shl 4
                or ((GreenShift shr 3) and 3) shl 2
                or ((BlueShift shr 3) and 3)) and $FF;

      if ByteOrder = riboMSBFirst
      then Positions := not Positions; // reverse positions
    end;
    
    // the locations of A,R,G,B are now coded in 2 bits each: AARRGGBB
    // the 2-bit value (0..3) represents the location of the channel,
    // counting from left
    case Positions of
      {AARRGGBB}
      %00011011: begin
        FGetInternalColorProc := @GetColor_BPP32_A8R8G8B8_BIO_TTB;
        FSetInternalColorProc := @SetColor_BPP32_A8R8G8B8_BIO_TTB;
      end;
      {AARRGGBB}
      %00111001: begin
        FGetInternalColorProc := @GetColor_BPP32_A8B8G8R8_BIO_TTB;
        FSetInternalColorProc := @SetColor_BPP32_A8B8G8R8_BIO_TTB;
      end;
      {AARRGGBB}
      %00100111: begin
        FGetInternalColorProc := @GetColor_BPP32_A8G8R8B8_BIO_TTB;
        FSetInternalColorProc := @SetColor_BPP32_A8G8R8B8_BIO_TTB;
      end;
      {AARRGGBB}
      %00110110: begin
        FGetInternalColorProc := @GetColor_BPP32_A8G8B8R8_BIO_TTB;
        FSetInternalColorProc := @SetColor_BPP32_A8G8B8R8_BIO_TTB;
      end;
      {AARRGGBB}
      %00011110: begin
        FGetInternalColorProc := @GetColor_BPP32_A8R8B8G8_BIO_TTB;
        FSetInternalColorProc := @SetColor_BPP32_A8R8B8G8_BIO_TTB;
      end;
      {AARRGGBB}
      %00101101: begin
        FGetInternalColorProc := @GetColor_BPP32_A8B8R8G8_BIO_TTB;
        FSetInternalColorProc := @SetColor_BPP32_A8B8R8G8_BIO_TTB;
      end;
      {AARRGGBB}
      %11100100: begin
        FGetInternalColorProc := @GetColor_BPP32_B8G8R8A8_BIO_TTB;
        FSetInternalColorProc := @SetColor_BPP32_B8G8R8A8_BIO_TTB;
      end;
      {AARRGGBB}
      %11000110: begin
        FGetInternalColorProc := @GetColor_BPP32_R8G8B8A8_BIO_TTB;
        FSetInternalColorProc := @SetColor_BPP32_R8G8B8A8_BIO_TTB;
      end;
      {AARRGGBB}
      %11100001: begin
        FGetInternalColorProc := @GetColor_BPP32_G8B8R8A8_BIO_TTB;
        FSetInternalColorProc := @SetColor_BPP32_G8B8R8A8_BIO_TTB;
      end;
      {AARRGGBB}
      %11010010: begin
        FGetInternalColorProc := @GetColor_BPP32_G8R8B8A8_BIO_TTB;
        FSetInternalColorProc := @SetColor_BPP32_G8R8B8A8_BIO_TTB;
      end;
      {AARRGGBB}
      %11011000: begin
        FGetInternalColorProc := @GetColor_BPP32_B8R8G8A8_BIO_TTB;
        FSetInternalColorProc := @SetColor_BPP32_B8R8G8A8_BIO_TTB;
      end;
      {AARRGGBB}
      %11001001: begin
        FGetInternalColorProc := @GetColor_BPP32_R8B8G8A8_BIO_TTB;
        FSetInternalColorProc := @SetColor_BPP32_R8B8G8A8_BIO_TTB;
      end;
    else
      Exit;
    end;
    Result := True;
  end;

  function ChooseRGB_32Bpp: Boolean;
  var
    Positions: Byte;
  begin
    Result := False;
    with FRawImage.Description do
    begin
      if Depth <> 24 then Exit;
      if BitsPerPixel <> 32 then Exit;
      if LineOrder <> riloTopToBottom then Exit;
      if RedPrec <> 8 then Exit;
      if GreenPrec <> 8 then Exit;
      if BluePrec <> 8 then Exit;
      if RedShift and 7 <> 0 then Exit;
      if GreenShift and 7 <> 0 then Exit;
      if BlueShift and 7 <> 0 then Exit;

      Positions := (((RedShift shr 3) and 3) shl 4
                or ((GreenShift shr 3) and 3) shl 2
                or ((BlueShift shr 3) and 3)) and $FF;

      if ByteOrder = riboMSBFirst
      then Positions := not Positions and %00111111; // reverse positions
    end;

    // the locations of R,G,B are now coded in 2 bits each: xxRRBBGG
    // the 2-bit value (0..3) represents the location of the channel,
    // counting from left
    case Positions of
      {xxRRGGBB}
      %00011011: begin
        FGetInternalColorProc := @GetColor_BPP32_X8R8G8B8_BIO_TTB;
        FSetInternalColorProc := @SetColor_BPP32_X8R8G8B8_BIO_TTB;
      end;
      {xxRRGGBB}
      %00111001: begin
        FGetInternalColorProc := @GetColor_BPP32_X8B8G8R8_BIO_TTB;
        FSetInternalColorProc := @SetColor_BPP32_X8B8G8R8_BIO_TTB;
      end;
      {xxRRGGBB}
      %00100111: begin
        FGetInternalColorProc := @GetColor_BPP32_X8G8R8B8_BIO_TTB;
        FSetInternalColorProc := @SetColor_BPP32_X8G8R8B8_BIO_TTB;
      end;
      {xxRRGGBB}
      %00110110: begin
        FGetInternalColorProc := @GetColor_BPP32_X8G8B8R8_BIO_TTB;
        FSetInternalColorProc := @SetColor_BPP32_X8G8B8R8_BIO_TTB;
      end;
      {xxRRGGBB}
      %00011110: begin
        FGetInternalColorProc := @GetColor_BPP32_X8R8B8G8_BIO_TTB;
        FSetInternalColorProc := @SetColor_BPP32_X8R8B8G8_BIO_TTB;
      end;
      {xxRRGGBB}
      %00101101: begin
        FGetInternalColorProc := @GetColor_BPP32_X8B8R8G8_BIO_TTB;
        FSetInternalColorProc := @SetColor_BPP32_X8B8R8G8_BIO_TTB;
      end;
      {xxRRGGBB}
      %00100100: begin
        FGetInternalColorProc := @GetColor_BPP32_B8G8R8X8_BIO_TTB;
        FSetInternalColorProc := @SetColor_BPP32_B8G8R8X8_BIO_TTB;
      end;
      {xxRRGGBB}
      %00000110: begin
        FGetInternalColorProc := @GetColor_BPP32_R8G8B8X8_BIO_TTB;
        FSetInternalColorProc := @SetColor_BPP32_R8G8B8X8_BIO_TTB;
      end;
      {xxRRGGBB}
      %00100001: begin
        FGetInternalColorProc := @GetColor_BPP32_G8B8R8X8_BIO_TTB;
        FSetInternalColorProc := @SetColor_BPP32_G8B8R8X8_BIO_TTB;
      end;
      {xxRRGGBB}
      %00010010: begin
        FGetInternalColorProc := @GetColor_BPP32_G8R8B8X8_BIO_TTB;
        FSetInternalColorProc := @SetColor_BPP32_G8R8B8X8_BIO_TTB;
      end;
      {xxRRGGBB}
      %00011000: begin
        FGetInternalColorProc := @GetColor_BPP32_B8R8G8X8_BIO_TTB;
        FSetInternalColorProc := @SetColor_BPP32_B8R8G8X8_BIO_TTB;
      end;
      {xxRRGGBB}
      %00001001: begin
        FGetInternalColorProc := @GetColor_BPP32_R8B8G8X8_BIO_TTB;
        FSetInternalColorProc := @SetColor_BPP32_R8B8G8X8_BIO_TTB;
      end;
    else
      Exit;
    end;
    Result := True;
  end;

  function ChooseRGB_24Bpp: Boolean;
  var
    Positions: Byte;
  begin
    Result := False;
    with FRawImage.Description do
    begin
      if Depth <> 24 then Exit;
      if BitsPerPixel <> 24 then Exit;
      if LineOrder <> riloTopToBottom then Exit;
      if RedPrec <> 8 then Exit;
      if GreenPrec <> 8 then Exit;
      if BluePrec <> 8 then Exit;
      if RedShift and 7 <> 0 then Exit;
      if GreenShift and 7 <> 0 then Exit;
      if BlueShift and 7 <> 0 then Exit;

      if ByteOrder = riboMSBFirst
      then
        Positions := ((2-((RedShift   shr 3) and 3)) shl 4
                  or (2-((GreenShift shr 3) and 3)) shl 2
                  or (2-((BlueShift  shr 3) and 3))) and $FF
      else
        Positions := (((RedShift   shr 3) and 3) shl 4
                  or ((GreenShift shr 3) and 3) shl 2
                  or ((BlueShift  shr 3) and 3)) and $FF;
    end;


    // the locations of R,G,B are now coded in 2 bits each: xxRRBBGG
    // the 2-bit value (0..3) represents the location of the channel,
    // counting from left
    case Positions of
      {xxRRGGBB}
      %00100100: begin
        FGetInternalColorProc := @GetColor_BPP24_B8G8R8_BIO_TTB;
        FSetInternalColorProc := @SetColor_BPP24_B8G8R8_BIO_TTB;
      end;
      {xxRRGGBB}
      %00000110: begin
        FGetInternalColorProc := @GetColor_BPP24_R8G8B8_BIO_TTB;
        FSetInternalColorProc := @SetColor_BPP24_R8G8B8_BIO_TTB;
      end;
      {xxRRGGBB}
      %00100001: begin
        FGetInternalColorProc := @GetColor_BPP24_G8B8R8_BIO_TTB;
        FSetInternalColorProc := @SetColor_BPP24_G8B8R8_BIO_TTB;
      end;
      {xxRRGGBB}
      %00010010: begin
        FGetInternalColorProc := @GetColor_BPP24_G8R8B8_BIO_TTB;
        FSetInternalColorProc := @SetColor_BPP24_G8R8B8_BIO_TTB;
      end;
      {xxRRGGBB}
      %00011000: begin
        FGetInternalColorProc := @GetColor_BPP24_B8R8G8_BIO_TTB;
        FSetInternalColorProc := @SetColor_BPP24_B8R8G8_BIO_TTB;
      end;
      {xxRRGGBB}
      %00001001: begin
        FGetInternalColorProc := @GetColor_BPP24_R8B8G8_BIO_TTB;
        FSetInternalColorProc := @SetColor_BPP24_R8B8G8_BIO_TTB;
      end;
    else
      Exit;
    end;
    Result := True;
  end;
  
  procedure ChooseRGBAFunctions;
  begin
    with FRawImage.Description do
    begin
      ChooseRawBitsProc(BitsPerPixel, ByteOrder, BitOrder,
                        FReadRawImageBits, FWriteRawImageBits);

      if AlphaPrec > 0
      then begin
        FGetInternalColorProc := @GetColor_RGBA_NoPalette;
        FSetInternalColorProc := @SetColor_RGBA_NoPalette;
      end
      else begin
        FGetInternalColorProc := @GetColor_RGB_NoPalette;
        FSetInternalColorProc := @SetColor_RGB_NoPalette;
      end;
    end;
  end;

begin
  // Default: use the generic functions, that can handle all kinds of RawImages
  FGetInternalColorProc := @GetColor_Generic;
  FSetInternalColorProc := @SetColor_Generic;
  
  if FUpdateCount > 0
  then begin
    FGetSetColorFunctionsUpdateNeeded := true;
    Exit;
  end;
  FGetSetColorFunctionsUpdateNeeded := false;

  with FRawImage.Description do
  begin
    if MaskBitsPerPixel > 0
    then ChooseRawBitsProc(MaskBitsPerPixel, ByteOrder, MaskBitOrder,
                           FMaskReadRawImageBits, FMaskWriteRawImageBits);

    if PaletteColorCount = 0
    then begin
      case Format of
        ricfRGBA: begin
          if not (ChooseRGBA_32Bpp or ChooseRGB_32Bpp or ChooseRGB_24Bpp)
          then ChooseRGBAFunctions;
        end;
        ricfGray: begin
          ChooseRawBitsProc(BitsPerPixel,
                            ByteOrder,
                            BitOrder,
                            FReadRawImageBits, FWriteRawImageBits);

          if AlphaPrec = 0
          then begin
            FGetInternalColorProc := @GetColor_Gray_NoPalette;
            FSetInternalColorProc := @SetColor_Gray_NoPalette;
          end
          else begin
            FGetInternalColorProc := @GetColor_GrayAlpha_NoPalette;
            FSetInternalColorProc := @SetColor_GrayAlpha_NoPalette;
          end;
        end;
      end;
    end
    else begin

    end;
  end;
end;

procedure TLazIntfImage.GetColor_Generic(x, y: integer; out Value: TFPColor);
var
  Position: TRawImagePosition;
begin
  GetXYDataPosition(x,y,Position);

  if FRawImage.Description.PaletteColorCount = 0
  then begin
    FRawimage.ReadChannels(Position, Value.Red, Value.Green, Value.Blue, Value.Alpha);
  end
  else begin
    // ToDo: read index, then palette
    Value.Red:=0;
    Value.Green:=0;
    Value.Blue:=0;
    Value.Alpha:=0;
  end;
end;

procedure TLazIntfImage.GetMask_Generic(x, y: integer; out AValue: Boolean);
var
  Position: TRawImagePosition;
begin
  if FRawImage.Description.MaskBitsPerPixel = 0
  then begin
    Avalue := False;
  end
  else begin
    GetXYMaskPosition(x,y,Position);
    FRawimage.ReadMask(Position, AValue);
  end;
end;

procedure TLazIntfImage.SetColor_Generic(x, y: integer; const Value: TFPColor);
var
  Position: TRawImagePosition;
begin
  GetXYDataPosition(x,y,Position);

  if FRawImage.Description.PaletteColorCount = 0
  then begin
    FRawImage.WriteChannels(Position, Value.Red, Value.Green, Value.Blue, Value.Alpha);
  end
  else begin
    // ToDo: Palette
  end;
end;

procedure TLazIntfImage.SetMask_Generic(x, y: integer; const AValue: Boolean);
var
  Position: TRawImagePosition;
begin
  if FRawImage.Description.MaskBitsPerPixel = 0 then Exit;
  
  GetXYMaskPosition(x,y,Position);
  FRawImage.WriteMask(Position, AValue);
end;


procedure TLazIntfImage.GetColor_RGBA_NoPalette(x, y: integer; out Value: TFPColor);
var
  Position: TRawImagePosition;
begin
  GetXYDataPosition(x,y,Position);
  with FRawImage.Description do
  begin
    FReadRawImageBits(FRawImage.Data, Position, RedPrec, RedShift, Value.Red);
    FReadRawImageBits(FRawImage.Data, Position, GreenPrec, GreenShift, Value.Green);
    FReadRawImageBits(FRawImage.Data, Position, BluePrec, BlueShift, Value.Blue);
    FReadRawImageBits(FRawImage.Data, Position, AlphaPrec, AlphaShift, Value.Alpha);
  end;
end;

procedure TLazIntfImage.GetColor_RGB_NoPalette(x, y: integer; out Value: TFPColor);
var
  Position: TRawImagePosition;
begin
  GetXYDataPosition(x,y,Position);
  with FRawImage.Description do
  begin
    FReadRawImageBits(FRawImage.Data, Position, RedPrec, RedShift, Value.Red);
    FReadRawImageBits(FRawImage.Data, Position, GreenPrec, GreenShift, Value.Green);
    FReadRawImageBits(FRawImage.Data, Position, BluePrec, BlueShift, Value.Blue);
  end;
  // no alpha -> set opaque
  Value.Alpha:=high(Value.Alpha);
end;

procedure TLazIntfImage.GetColor_Gray_NoPalette(x, y: integer; out Value: TFPColor);
var
  Position: TRawImagePosition;
begin
  GetXYDataPosition(x,y,Position);
  with FRawImage.Description do
    FReadRawImageBits(FRawImage.Data, Position, RedPrec, RedShift, Value.Red);
  Value.Green := Value.Red;
  Value.Blue := Value.Red;
  // no alpha -> set opaque
  Value.Alpha:=high(Value.Alpha);
end;

procedure TLazIntfImage.GetColor_GrayAlpha_NoPalette(x, y: integer; out Value: TFPColor);
var
  Position: TRawImagePosition;
begin
  GetXYDataPosition(x,y,Position);
  with FRawImage.Description do
  begin
    FReadRawImageBits(FRawImage.Data, Position, RedPrec, RedShift, Value.Red);
    FReadRawImageBits(FRawImage.Data, Position, AlphaPrec, AlphaShift, Value.Alpha);
  end;
  Value.Green := Value.Red;
  Value.Blue := Value.Red;
end;

procedure TLazIntfImage.GetColor_BPP32_A8B8G8R8_BIO_TTB(x, y: integer; out Value: TFPColor);
var
  VBytes: TFPColorBytes absolute Value;
begin
  with PFourBytes(FRawImage.Data+FLineStarts^.Positions[y].Byte+(x shl 2))^ do
  begin
    VBytes.Ah := B0;
    VBytes.Al := B0;
    VBytes.Bh := B1;
    VBytes.Bl := B1;
    VBytes.Rh := B2;
    VBytes.Rl := B2;
    VBytes.Gh := B3;
    VBytes.Gl := B3;
  end;
end;

procedure TLazIntfImage.GetColor_BPP32_A8B8R8G8_BIO_TTB(x, y: integer; out Value: TFPColor);
var
  VBytes: TFPColorBytes absolute Value;
begin
  with PFourBytes(FRawImage.Data+FLineStarts^.Positions[y].Byte+(x shl 2))^ do
  begin
    VBytes.Ah := B0;
    VBytes.Al := B0;
    VBytes.Bh := B1;
    VBytes.Bl := B1;
    VBytes.Rh := B2;
    VBytes.Rl := B2;
    VBytes.Gh := B3;
    VBytes.Gl := B3;
  end;
end;

procedure TLazIntfImage.GetColor_BPP32_A8G8B8R8_BIO_TTB(x, y: integer; out Value: TFPColor);
var
  VBytes: TFPColorBytes absolute Value;
begin
  with PFourBytes(FRawImage.Data+FLineStarts^.Positions[y].Byte+(x shl 2))^ do
  begin
    VBytes.Ah := B0;
    VBytes.Al := B0;
    VBytes.Gh := B1;
    VBytes.Gl := B1;
    VBytes.Bh := B2;
    VBytes.Bl := B2;
    VBytes.Rh := B3;
    VBytes.Rl := B3;
  end;
end;

procedure TLazIntfImage.GetColor_BPP32_A8G8R8B8_BIO_TTB(x, y: integer; out Value: TFPColor);
var
  VBytes: TFPColorBytes absolute Value;
begin
  with PFourBytes(FRawImage.Data+FLineStarts^.Positions[y].Byte+(x shl 2))^ do
  begin
    VBytes.Ah := B0;
    VBytes.Al := B0;
    VBytes.Gh := B1;
    VBytes.Gl := B1;
    VBytes.Rh := B2;
    VBytes.Rl := B2;
    VBytes.Bh := B3;
    VBytes.Bl := B3;
  end;
end;

procedure TLazIntfImage.GetColor_BPP32_A8R8B8G8_BIO_TTB(x, y: integer; out Value: TFPColor);
var
  VBytes: TFPColorBytes absolute Value;
begin
  with PFourBytes(FRawImage.Data+FLineStarts^.Positions[y].Byte+(x shl 2))^ do
  begin
    VBytes.Ah := B0;
    VBytes.Al := B0;
    VBytes.Rh := B1;
    VBytes.Rl := B1;
    VBytes.Bh := B2;
    VBytes.Bl := B2;
    VBytes.Gh := B3;
    VBytes.Gl := B3;
  end;
end;

procedure TLazIntfImage.GetColor_BPP32_A8R8G8B8_BIO_TTB(x, y: integer; out Value: TFPColor);
// Format=ricfRGBA HasPalette=false Depth=32 PaletteColorCount=0
// BitOrder=riboBitsInOrder LineOrder=riloTopToBottom
// BitsPerPixel=32
var
  VBytes: TFPColorBytes absolute Value;
begin
  with PFourBytes(FRawImage.Data+FLineStarts^.Positions[y].Byte+(x shl 2))^ do
  begin
    VBytes.Ah := B0;
    VBytes.Al := B0;
    VBytes.Rh := B1;
    VBytes.Rl := B1;
    VBytes.Gh := B2;
    VBytes.Gl := B2;
    VBytes.Bh := B3;
    VBytes.Bl := B3;
  end;
end;

procedure TLazIntfImage.GetColor_BPP32_B8G8R8A8_BIO_TTB(x, y: integer; out Value: TFPColor);
// Format=ricfRGBA HasPalette=false Depth=32 PaletteColorCount=0
// BitOrder=riboBitsInOrder LineOrder=riloTopToBottom
// BitsPerPixel=32
var
  VBytes: TFPColorBytes absolute Value;
begin
  with PFourBytes(FRawImage.Data+FLineStarts^.Positions[y].Byte+(x shl 2))^ do
  begin
    VBytes.Bh := B0;
    VBytes.Bl := B0;
    VBytes.Gh := B1;
    VBytes.Gl := B1;
    VBytes.Rh := B2;
    VBytes.Rl := B2;
    VBytes.Ah := B3;
    VBytes.Al := B3;
  end;
end;

procedure TLazIntfImage.GetColor_BPP32_B8R8G8A8_BIO_TTB(x, y: integer; out Value: TFPColor);
// Format=ricfRGBA HasPalette=false Depth=32 PaletteColorCount=0
// BitOrder=riboBitsInOrder LineOrder=riloTopToBottom
// BitsPerPixel=32
var
  VBytes: TFPColorBytes absolute Value;
begin
  with PFourBytes(FRawImage.Data+FLineStarts^.Positions[y].Byte+(x shl 2))^ do
  begin
    VBytes.Bh := B0;
    VBytes.Bl := B0;
    VBytes.Rh := B1;
    VBytes.Rl := B1;
    VBytes.Gh := B2;
    VBytes.Gl := B2;
    VBytes.Ah := B3;
    VBytes.Al := B3;
  end;
end;

procedure TLazIntfImage.GetColor_BPP32_G8B8R8A8_BIO_TTB(x, y: integer; out Value: TFPColor);
// Format=ricfRGBA HasPalette=false Depth=32 PaletteColorCount=0
// BitOrder=riboBitsInOrder LineOrder=riloTopToBottom
// BitsPerPixel=32
var
  VBytes: TFPColorBytes absolute Value;
begin
  with PFourBytes(FRawImage.Data+FLineStarts^.Positions[y].Byte+(x shl 2))^ do
  begin
    VBytes.Gh := B0;
    VBytes.Gl := B0;
    VBytes.Bh := B1;
    VBytes.Bl := B1;
    VBytes.Rh := B2;
    VBytes.Rl := B2;
    VBytes.Ah := B3;
    VBytes.Al := B3;
  end;
end;

procedure TLazIntfImage.GetColor_BPP32_G8R8B8A8_BIO_TTB(x, y: integer; out Value: TFPColor);
// Format=ricfRGBA HasPalette=false Depth=32 PaletteColorCount=0
// BitOrder=riboBitsInOrder LineOrder=riloTopToBottom
// BitsPerPixel=32
var
  VBytes: TFPColorBytes absolute Value;
begin
  with PFourBytes(FRawImage.Data+FLineStarts^.Positions[y].Byte+(x shl 2))^ do
  begin
    VBytes.Gh := B0;
    VBytes.Gl := B0;
    VBytes.Rh := B1;
    VBytes.Rl := B1;
    VBytes.Bh := B2;
    VBytes.Bl := B2;
    VBytes.Ah := B3;
    VBytes.Al := B3;
  end;
end;

procedure TLazIntfImage.GetColor_BPP32_R8B8G8A8_BIO_TTB(x, y: integer; out Value: TFPColor);
// Format=ricfRGBA HasPalette=false Depth=32 PaletteColorCount=0
// BitOrder=riboBitsInOrder LineOrder=riloTopToBottom
// BitsPerPixel=32
var
  VBytes: TFPColorBytes absolute Value;
begin
  with PFourBytes(FRawImage.Data+FLineStarts^.Positions[y].Byte+(x shl 2))^ do
  begin
    VBytes.Rh := B0;
    VBytes.Rl := B0;
    VBytes.Bh := B1;
    VBytes.Bl := B1;
    VBytes.Gh := B2;
    VBytes.Gl := B2;
    VBytes.Ah := B3;
    VBytes.Al := B3;
  end;
end;

procedure TLazIntfImage.GetColor_BPP32_R8G8B8A8_BIO_TTB(x, y: integer; out Value: TFPColor);
// Format=ricfRGBA HasPalette=false Depth=32 PaletteColorCount=0
// BitOrder=riboBitsInOrder LineOrder=riloTopToBottom
// BitsPerPixel=32
var
  VBytes: TFPColorBytes absolute Value;
begin
  with PFourBytes(FRawImage.Data+FLineStarts^.Positions[y].Byte+(x shl 2))^ do
  begin
    VBytes.Rh := B0;
    VBytes.Rl := B0;
    VBytes.Gh := B1;
    VBytes.Gl := B1;
    VBytes.Bh := B2;
    VBytes.Bl := B2;
    VBytes.Ah := B3;
    VBytes.Al := B3;
  end;
end;

procedure TLazIntfImage.GetColor_BPP32_X8B8G8R8_BIO_TTB(x, y: integer; out Value: TFPColor);
// Format=ricfRGBA HasPalette=false Depth=32 PaletteColorCount=0
// BitOrder=riboBitsInOrder LineOrder=riloTopToBottom
// BitsPerPixel=32
var
  VBytes: TFPColorBytes absolute Value;
begin
  with PFourBytes(FRawImage.Data+FLineStarts^.Positions[y].Byte+(x shl 2))^ do
  begin
    VBytes.Bh := B1;
    VBytes.Bl := B1;
    VBytes.Rh := B2;
    VBytes.Rl := B2;
    VBytes.Gh := B3;
    VBytes.Gl := B3;
  end;
  // no alpha -> set opaque
  Value.Alpha:=high(Value.Alpha);
end;

procedure TLazIntfImage.GetColor_BPP32_X8B8R8G8_BIO_TTB(x, y: integer; out Value: TFPColor);
// Format=ricfRGBA HasPalette=false Depth=32 PaletteColorCount=0
// BitOrder=riboBitsInOrder LineOrder=riloTopToBottom
// BitsPerPixel=32
var
  VBytes: TFPColorBytes absolute Value;
begin
  with PFourBytes(FRawImage.Data+FLineStarts^.Positions[y].Byte+(x shl 2))^ do
  begin
    VBytes.Bh := B1;
    VBytes.Bl := B1;
    VBytes.Rh := B2;
    VBytes.Rl := B2;
    VBytes.Gh := B3;
    VBytes.Gl := B3;
  end;
  // no alpha -> set opaque
  Value.Alpha:=high(Value.Alpha);
end;

procedure TLazIntfImage.GetColor_BPP32_X8G8B8R8_BIO_TTB(x, y: integer; out Value: TFPColor);
// Format=ricfRGBA HasPalette=false Depth=32 PaletteColorCount=0
// BitOrder=riboBitsInOrder LineOrder=riloTopToBottom
// BitsPerPixel=32
var
  VBytes: TFPColorBytes absolute Value;
begin
  with PFourBytes(FRawImage.Data+FLineStarts^.Positions[y].Byte+(x shl 2))^ do
  begin
    VBytes.Gh := B1;
    VBytes.Gl := B1;
    VBytes.Bh := B2;
    VBytes.Bl := B2;
    VBytes.Rh := B3;
    VBytes.Rl := B3;
  end;
  // no alpha -> set opaque
  Value.Alpha:=high(Value.Alpha);
end;

procedure TLazIntfImage.GetColor_BPP32_X8G8R8B8_BIO_TTB(x, y: integer; out Value: TFPColor);
// Format=ricfRGBA HasPalette=false Depth=32 PaletteColorCount=0
// BitOrder=riboBitsInOrder LineOrder=riloTopToBottom
// BitsPerPixel=32
var
  VBytes: TFPColorBytes absolute Value;
begin
  with PFourBytes(FRawImage.Data+FLineStarts^.Positions[y].Byte+(x shl 2))^ do
  begin
    VBytes.Gh := B1;
    VBytes.Gl := B1;
    VBytes.Rh := B2;
    VBytes.Rl := B2;
    VBytes.Bh := B3;
    VBytes.Bl := B3;
  end;
  // no alpha -> set opaque
  Value.Alpha:=high(Value.Alpha);
end;

procedure TLazIntfImage.GetColor_BPP32_X8R8B8G8_BIO_TTB(x, y: integer; out Value: TFPColor);
// Format=ricfRGBA HasPalette=false Depth=32 PaletteColorCount=0
// BitOrder=riboBitsInOrder LineOrder=riloTopToBottom
// BitsPerPixel=32
var
  VBytes: TFPColorBytes absolute Value;
begin
  with PFourBytes(FRawImage.Data+FLineStarts^.Positions[y].Byte+(x shl 2))^ do
  begin
    VBytes.Rh := B1;
    VBytes.Rl := B1;
    VBytes.Bh := B2;
    VBytes.Bl := B2;
    VBytes.Gh := B3;
    VBytes.Gl := B3;
  end;
  // no alpha -> set opaque
  Value.Alpha:=high(Value.Alpha);
end;

procedure TLazIntfImage.GetColor_BPP32_X8R8G8B8_BIO_TTB(x, y: integer; out Value: TFPColor);
// Format=ricfRGBA HasPalette=false Depth=32 PaletteColorCount=0
// BitOrder=riboBitsInOrder LineOrder=riloTopToBottom
// BitsPerPixel=32
var
  VBytes: TFPColorBytes absolute Value;
begin
  with PFourBytes(FRawImage.Data+FLineStarts^.Positions[y].Byte+(x shl 2))^ do
  begin
    VBytes.Rh := B1;
    VBytes.Rl := B1;
    VBytes.Gh := B2;
    VBytes.Gl := B2;
    VBytes.Bh := B3;
    VBytes.Bl := B3;
  end;
  // no alpha -> set opaque
  Value.Alpha:=high(Value.Alpha);
end;

procedure TLazIntfImage.GetColor_BPP32_B8G8R8X8_BIO_TTB(x, y: integer; out Value: TFPColor);
// Format=ricfRGBA HasPalette=false Depth=32 PaletteColorCount=0
// BitOrder=riboBitsInOrder LineOrder=riloTopToBottom
// BitsPerPixel=32
var
  VBytes: TFPColorBytes absolute Value;
begin
  with PFourBytes(FRawImage.Data+FLineStarts^.Positions[y].Byte+(x shl 2))^ do
  begin
    VBytes.Bh := B0;
    VBytes.Bl := B0;
    VBytes.Gh := B1;
    VBytes.Gl := B1;
    VBytes.Rh := B2;
    VBytes.Rl := B2;
  end;
  // no alpha -> set opaque
  Value.Alpha:=high(Value.Alpha);
end;

procedure TLazIntfImage.GetColor_BPP32_B8R8G8X8_BIO_TTB(x, y: integer; out Value: TFPColor);
// Format=ricfRGBA HasPalette=false Depth=32 PaletteColorCount=0
// BitOrder=riboBitsInOrder LineOrder=riloTopToBottom
// BitsPerPixel=32
var
  VBytes: TFPColorBytes absolute Value;
begin
  with PFourBytes(FRawImage.Data+FLineStarts^.Positions[y].Byte+(x shl 2))^ do
  begin
    VBytes.Bh := B0;
    VBytes.Bl := B0;
    VBytes.Rh := B1;
    VBytes.Rl := B1;
    VBytes.Gh := B2;
    VBytes.Gl := B2;
  end;
  // no alpha -> set opaque
  Value.Alpha:=high(Value.Alpha);
end;

procedure TLazIntfImage.GetColor_BPP32_G8B8R8X8_BIO_TTB(x, y: integer; out Value: TFPColor);
// Format=ricfRGBA HasPalette=false Depth=32 PaletteColorCount=0
// BitOrder=riboBitsInOrder LineOrder=riloTopToBottom
// BitsPerPixel=32
var
  VBytes: TFPColorBytes absolute Value;
begin
  with PFourBytes(FRawImage.Data+FLineStarts^.Positions[y].Byte+(x shl 2))^ do
  begin
    VBytes.Gh := B0;
    VBytes.Gl := B0;
    VBytes.Bh := B1;
    VBytes.Bl := B1;
    VBytes.Rh := B2;
    VBytes.Rl := B2;
  end;
  // no alpha -> set opaque
  Value.Alpha:=high(Value.Alpha);
end;

procedure TLazIntfImage.GetColor_BPP32_G8R8B8X8_BIO_TTB(x, y: integer; out Value: TFPColor);
// Format=ricfRGBA HasPalette=false Depth=32 PaletteColorCount=0
// BitOrder=riboBitsInOrder LineOrder=riloTopToBottom
// BitsPerPixel=32
var
  VBytes: TFPColorBytes absolute Value;
begin
  with PFourBytes(FRawImage.Data+FLineStarts^.Positions[y].Byte+(x shl 2))^ do
  begin
    VBytes.Gh := B0;
    VBytes.Gl := B0;
    VBytes.Rh := B1;
    VBytes.Rl := B1;
    VBytes.Bh := B2;
    VBytes.Bl := B2;
  end;
  // no alpha -> set opaque
  Value.Alpha:=high(Value.Alpha);
end;

procedure TLazIntfImage.GetColor_BPP32_R8B8G8X8_BIO_TTB(x, y: integer; out Value: TFPColor);
// Format=ricfRGBA HasPalette=false Depth=32 PaletteColorCount=0
// BitOrder=riboBitsInOrder LineOrder=riloTopToBottom
// BitsPerPixel=32
var
  VBytes: TFPColorBytes absolute Value;
begin
  with PFourBytes(FRawImage.Data+FLineStarts^.Positions[y].Byte+(x shl 2))^ do
  begin
    VBytes.Rh := B0;
    VBytes.Rl := B0;
    VBytes.Bh := B1;
    VBytes.Bl := B1;
    VBytes.Gh := B2;
    VBytes.Gl := B2;
  end;
  // no alpha -> set opaque
  Value.Alpha:=high(Value.Alpha);
end;

procedure TLazIntfImage.GetColor_BPP32_R8G8B8X8_BIO_TTB(x, y: integer; out Value: TFPColor);
// Format=ricfRGBA HasPalette=false Depth=32 PaletteColorCount=0
// BitOrder=riboBitsInOrder LineOrder=riloTopToBottom
// BitsPerPixel=32
var
  VBytes: TFPColorBytes absolute Value;
begin
  with PFourBytes(FRawImage.Data+FLineStarts^.Positions[y].Byte+(x shl 2))^ do
  begin
    VBytes.Rh := B0;
    VBytes.Rl := B0;
    VBytes.Gh := B1;
    VBytes.Gl := B1;
    VBytes.Bh := B2;
    VBytes.Bl := B2;
  end;
  // no alpha -> set opaque
  Value.Alpha:=high(Value.Alpha);
end;

procedure TLazIntfImage.GetColor_BPP24_B8G8R8_BIO_TTB(x, y: integer; out Value: TFPColor);
// Format=ricfRGBA HasPalette=false Depth=24 PaletteColorCount=0
// BitOrder=riboBitsInOrder LineOrder=riloTopToBottom
// BitsPerPixel=24
var
  VBytes: TFPColorBytes absolute Value;
begin
  with PFourBytes(FRawImage.Data+FLineStarts^.Positions[y].Byte+(x * 3))^ do
  begin
    VBytes.Bh := B0;
    VBytes.Bl := B0;
    VBytes.Gh := B1;
    VBytes.Gl := B1;
    VBytes.Rh := B2;
    VBytes.Rl := B2;
  end;
  // no alpha -> set opaque
  Value.Alpha:=high(Value.Alpha);
end;

procedure TLazIntfImage.GetColor_BPP24_B8R8G8_BIO_TTB(x, y: integer; out Value: TFPColor);
// Format=ricfRGBA HasPalette=false Depth=24 PaletteColorCount=0
// BitOrder=riboBitsInOrder LineOrder=riloTopToBottom
// BitsPerPixel=24
var
  VBytes: TFPColorBytes absolute Value;
begin
  with PFourBytes(FRawImage.Data+FLineStarts^.Positions[y].Byte+(x * 3))^ do
  begin
    VBytes.Bh := B0;
    VBytes.Bl := B0;
    VBytes.Rh := B1;
    VBytes.Rl := B1;
    VBytes.Gh := B2;
    VBytes.Gl := B2;
  end;
  // no alpha -> set opaque
  Value.Alpha:=high(Value.Alpha);
end;

procedure TLazIntfImage.GetColor_BPP24_G8B8R8_BIO_TTB(x, y: integer; out Value: TFPColor);
// Format=ricfRGBA HasPalette=false Depth=24 PaletteColorCount=0
// BitOrder=riboBitsInOrder LineOrder=riloTopToBottom
// BitsPerPixel=24
var
  VBytes: TFPColorBytes absolute Value;
begin
  with PFourBytes(FRawImage.Data+FLineStarts^.Positions[y].Byte+(x * 3))^ do
  begin
    VBytes.Gh := B0;
    VBytes.Gl := B0;
    VBytes.Bh := B1;
    VBytes.Bl := B1;
    VBytes.Rh := B2;
    VBytes.Rl := B2;
  end;
  // no alpha -> set opaque
  Value.Alpha:=high(Value.Alpha);
end;

procedure TLazIntfImage.GetColor_BPP24_G8R8B8_BIO_TTB(x, y: integer; out Value: TFPColor);
// Format=ricfRGBA HasPalette=false Depth=24 PaletteColorCount=0
// BitOrder=riboBitsInOrder LineOrder=riloTopToBottom
// BitsPerPixel=24
var
  VBytes: TFPColorBytes absolute Value;
begin
  with PFourBytes(FRawImage.Data+FLineStarts^.Positions[y].Byte+(x * 3))^ do
  begin
    VBytes.Gh := B0;
    VBytes.Gl := B0;
    VBytes.Rh := B1;
    VBytes.Rl := B1;
    VBytes.Bh := B2;
    VBytes.Bl := B2;
  end;
  // no alpha -> set opaque
  Value.Alpha:=high(Value.Alpha);
end;

procedure TLazIntfImage.GetColor_BPP24_R8B8G8_BIO_TTB(x, y: integer; out Value: TFPColor);
// Format=ricfRGBA HasPalette=false Depth=24 PaletteColorCount=0
// BitOrder=riboBitsInOrder LineOrder=riloTopToBottom
// BitsPerPixel=24
var
  VBytes: TFPColorBytes absolute Value;
begin
  with PFourBytes(FRawImage.Data+FLineStarts^.Positions[y].Byte+(x * 3))^ do
  begin
    VBytes.Rh := B0;
    VBytes.Rl := B0;
    VBytes.Bh := B1;
    VBytes.Bl := B1;
    VBytes.Gh := B2;
    VBytes.Gl := B2;
  end;
  // no alpha -> set opaque
  Value.Alpha:=high(Value.Alpha);
end;

procedure TLazIntfImage.GetColor_BPP24_R8G8B8_BIO_TTB(x, y: integer; out Value: TFPColor);
// Format=ricfRGBA HasPalette=false Depth=24 PaletteColorCount=0
// BitOrder=riboBitsInOrder LineOrder=riloTopToBottom
// BitsPerPixel=24
var
  VBytes: TFPColorBytes absolute Value;
begin
  with PFourBytes(FRawImage.Data+FLineStarts^.Positions[y].Byte+(x * 3))^ do
  begin
    VBytes.Rh := B0;
    VBytes.Rl := B0;
    VBytes.Gh := B1;
    VBytes.Gl := B1;
    VBytes.Bh := B2;
    VBytes.Bl := B2;
  end;
  // no alpha -> set opaque
  Value.Alpha:=high(Value.Alpha);
end;

procedure TLazIntfImage.GetColor_NULL(x, y: integer; out Value: TFPColor);
//var
//  Position: TRawImagePosition;
begin
//  GetXYDataPosition(x,y,Position);
  Value.Red:=0;
  Value.Green:=0;
  Value.Blue:=0;
  Value.Alpha:=0;
end;

procedure TLazIntfImage.SetColor_RGBA_NoPalette(x, y: integer; const Value: TFPColor);
var
  Position: TRawImagePosition;
begin
  GetXYDataPosition(x,y,Position);
  with FRawImage.Description do
  begin
    FWriteRawImageBits(FRawImage.Data, Position, RedPrec, RedShift, Value.Red);
    FWriteRawImageBits(FRawImage.Data, Position, GreenPrec, GreenShift, Value.Green);
    FWriteRawImageBits(FRawImage.Data, Position, BluePrec, BlueShift, Value.Blue);
    FWriteRawImageBits(FRawImage.Data, Position, AlphaPrec, AlphaShift, Value.Alpha)
  end;
end;

procedure TLazIntfImage.SetColor_RGB_NoPalette(x, y: integer; const Value: TFPColor);
var
  Position: TRawImagePosition;
begin
  GetXYDataPosition(x,y,Position);
  with FRawImage.Description do
  begin
    FWriteRawImageBits(FRawImage.Data, Position, RedPrec, RedShift, Value.Red);
    FWriteRawImageBits(FRawImage.Data, Position, GreenPrec, GreenShift, Value.Green);
    FWriteRawImageBits(FRawImage.Data, Position, BluePrec, BlueShift, Value.Blue);
  end;
  // no alpha -> ignore
end;

procedure TLazIntfImage.SetColor_Gray_NoPalette(x, y: integer; const Value: TFPColor);
var
  Position: TRawImagePosition;
begin
  GetXYDataPosition(x,y,Position);
  with FRawImage.Description do
    FWriteRawImageBits(FRawImage.Data, Position, RedPrec, RedShift, Value.Red);
end;

procedure TLazIntfImage.SetColor_GrayAlpha_NoPalette(x, y: integer; const Value: TFPColor);
var
  Position: TRawImagePosition;
begin
  GetXYDataPosition(x,y,Position);
  with FRawImage.Description do
  begin
    FWriteRawImageBits(FRawImage.Data, Position, RedPrec, RedShift, Value.Red);
    FWriteRawImageBits(FRawImage.Data, Position, AlphaPrec, AlphaShift, Value.Alpha)
  end;
end;


procedure TLazIntfImage.SetColor_NULL(x, y: integer; const Value: TFPColor);
begin
  // NULL, not implemented
end;

procedure TLazIntfImage.SetColor_BPP32_A8R8G8B8_BIO_TTB(x, y: integer; const Value: TFPColor);
// Format=ricfRGBA HasPalette=false Depth=32 PaletteColorCount=0
// BitOrder=riboBitsInOrder LineOrder=riloTopToBottom
// BitsPerPixel=32
var
  VBytes: TFPColorBytes absolute Value;
begin
  with PFourBytes(FRawImage.Data+FLineStarts^.Positions[y].Byte+(x shl 2))^ do
  begin
    B0 := VBytes.Ah;
    B1 := VBytes.Rh;
    B2 := VBytes.Gh;
    B3 := VBytes.Bh;
  end;
end;

procedure TLazIntfImage.SetColor_BPP32_A8B8G8R8_BIO_TTB(x, y: integer; const Value: TFPColor);
// Format=ricfRGBA HasPalette=false Depth=32 PaletteColorCount=0
// BitOrder=riboBitsInOrder LineOrder=riloTopToBottom
// BitsPerPixel=32
var
  VBytes: TFPColorBytes absolute Value;
begin
  with PFourBytes(FRawImage.Data+FLineStarts^.Positions[y].Byte+(x shl 2))^ do
  begin
    B0 := VBytes.Ah;
    B1 := VBytes.Bh;
    B2 := VBytes.Gh;
    B3 := VBytes.Rh;
  end;
end;

procedure TLazIntfImage.SetColor_BPP32_A8B8R8G8_BIO_TTB(x, y: integer; const Value: TFPColor);
// Format=ricfRGBA HasPalette=false Depth=32 PaletteColorCount=0
// BitOrder=riboBitsInOrder LineOrder=riloTopToBottom
// BitsPerPixel=32
var
  VBytes: TFPColorBytes absolute Value;
begin
  with PFourBytes(FRawImage.Data+FLineStarts^.Positions[y].Byte+(x shl 2))^ do
  begin
    B0 := VBytes.Ah;
    B1 := VBytes.Bh;
    B2 := VBytes.Rh;
    B3 := VBytes.Gh;
  end;
end;

procedure TLazIntfImage.SetColor_BPP32_A8G8B8R8_BIO_TTB(x, y: integer; const Value: TFPColor);
// Format=ricfRGBA HasPalette=false Depth=32 PaletteColorCount=0
// BitOrder=riboBitsInOrder LineOrder=riloTopToBottom
// BitsPerPixel=32
var
  VBytes: TFPColorBytes absolute Value;
begin
  with PFourBytes(FRawImage.Data+FLineStarts^.Positions[y].Byte+(x shl 2))^ do
  begin
    B0 := VBytes.Ah;
    B1 := VBytes.Gh;
    B2 := VBytes.Bh;
    B3 := VBytes.Rh;
  end;
end;

procedure TLazIntfImage.SetColor_BPP32_A8G8R8B8_BIO_TTB(x, y: integer; const Value: TFPColor);
// Format=ricfRGBA HasPalette=false Depth=32 PaletteColorCount=0
// BitOrder=riboBitsInOrder LineOrder=riloTopToBottom
// BitsPerPixel=32
var
  VBytes: TFPColorBytes absolute Value;
begin
  with PFourBytes(FRawImage.Data+FLineStarts^.Positions[y].Byte+(x shl 2))^ do
  begin
    B0 := VBytes.Ah;
    B1 := VBytes.Gh;
    B2 := VBytes.Rh;
    B3 := VBytes.Bh;
  end;
end;

procedure TLazIntfImage.SetColor_BPP32_A8R8B8G8_BIO_TTB(x, y: integer; const Value: TFPColor);
// Format=ricfRGBA HasPalette=false Depth=32 PaletteColorCount=0
// BitOrder=riboBitsInOrder LineOrder=riloTopToBottom
// BitsPerPixel=32
var
  VBytes: TFPColorBytes absolute Value;
begin
  with PFourBytes(FRawImage.Data+FLineStarts^.Positions[y].Byte+(x shl 2))^ do
  begin
    B0 := VBytes.Ah;
    B1 := VBytes.Rh;
    B2 := VBytes.Bh;
    B3 := VBytes.Gh;
  end;
end;

procedure TLazIntfImage.SetColor_BPP32_B8G8R8A8_BIO_TTB(x, y: integer; const Value: TFPColor);
// Format=ricfRGBA HasPalette=false Depth=32 PaletteColorCount=0
// BitOrder=riboBitsInOrder LineOrder=riloTopToBottom
// BitsPerPixel=32
var
  VBytes: TFPColorBytes absolute Value;
begin
  with PFourBytes(FRawImage.Data+FLineStarts^.Positions[y].Byte+(x shl 2))^ do
  begin
    B0 := VBytes.Bh;
    B1 := VBytes.Gh;
    B2 := VBytes.Rh;
    B3 := VBytes.Ah;
  end;
end;

procedure TLazIntfImage.SetColor_BPP32_B8R8G8A8_BIO_TTB(x, y: integer; const Value: TFPColor);
// Format=ricfRGBA HasPalette=false Depth=32 PaletteColorCount=0
// BitOrder=riboBitsInOrder LineOrder=riloTopToBottom
// BitsPerPixel=32
var
  VBytes: TFPColorBytes absolute Value;
begin
  with PFourBytes(FRawImage.Data+FLineStarts^.Positions[y].Byte+(x shl 2))^ do
  begin
    B0 := VBytes.Bh;
    B1 := VBytes.Rh;
    B2 := VBytes.Gh;
    B3 := VBytes.Ah;
  end;
end;

procedure TLazIntfImage.SetColor_BPP32_G8B8R8A8_BIO_TTB(x, y: integer; const Value: TFPColor);
// Format=ricfRGBA HasPalette=false Depth=32 PaletteColorCount=0
// BitOrder=riboBitsInOrder LineOrder=riloTopToBottom
// BitsPerPixel=32
var
  VBytes: TFPColorBytes absolute Value;
begin
  with PFourBytes(FRawImage.Data+FLineStarts^.Positions[y].Byte+(x shl 2))^ do
  begin
    B0 := VBytes.Gh;
    B1 := VBytes.Bh;
    B2 := VBytes.Rh;
    B3 := VBytes.Ah;
  end;
end;

procedure TLazIntfImage.SetColor_BPP32_G8R8B8A8_BIO_TTB(x, y: integer; const Value: TFPColor);
// Format=ricfRGBA HasPalette=false Depth=32 PaletteColorCount=0
// BitOrder=riboBitsInOrder LineOrder=riloTopToBottom
// BitsPerPixel=32
var
  VBytes: TFPColorBytes absolute Value;
begin
  with PFourBytes(FRawImage.Data+FLineStarts^.Positions[y].Byte+(x shl 2))^ do
  begin
    B0 := VBytes.Gh;
    B1 := VBytes.Rh;
    B2 := VBytes.Bh;
    B3 := VBytes.Ah;
  end;
end;

procedure TLazIntfImage.SetColor_BPP32_R8B8G8A8_BIO_TTB(x, y: integer; const Value: TFPColor);
// Format=ricfRGBA HasPalette=false Depth=32 PaletteColorCount=0
// BitOrder=riboBitsInOrder LineOrder=riloTopToBottom
// BitsPerPixel=32
var
  VBytes: TFPColorBytes absolute Value;
begin
  with PFourBytes(FRawImage.Data+FLineStarts^.Positions[y].Byte+(x shl 2))^ do
  begin
    B0 := VBytes.Rh;
    B1 := VBytes.Bh;
    B2 := VBytes.Gh;
    B3 := VBytes.Ah;
  end;
end;

procedure TLazIntfImage.SetColor_BPP32_R8G8B8A8_BIO_TTB(x, y: integer; const Value: TFPColor);
// Format=ricfRGBA HasPalette=false Depth=32 PaletteColorCount=0
// BitOrder=riboBitsInOrder LineOrder=riloTopToBottom
// BitsPerPixel=32
var
  VBytes: TFPColorBytes absolute Value;
begin
  with PFourBytes(FRawImage.Data+FLineStarts^.Positions[y].Byte+(x shl 2))^ do
  begin
    B0 := VBytes.Rh;
    B1 := VBytes.Gh;
    B2 := VBytes.Bh;
    B3 := VBytes.Ah;
  end;
end;

procedure TLazIntfImage.SetColor_BPP32_X8R8G8B8_BIO_TTB(x, y: integer; const Value: TFPColor);
// Format=ricfRGBA HasPalette=false Depth=32 PaletteColorCount=0
// BitOrder=riboBitsInOrder LineOrder=riloTopToBottom
// BitsPerPixel=32
var
  VBytes: TFPColorBytes absolute Value;
begin
  with PFourBytes(FRawImage.Data+FLineStarts^.Positions[y].Byte+(x shl 2))^ do
  begin
    B1 := VBytes.Rh;
    B2 := VBytes.Gh;
    B3 := VBytes.Bh;
  end;
end;

procedure TLazIntfImage.SetColor_BPP32_X8B8G8R8_BIO_TTB(x, y: integer; const Value: TFPColor);
// Format=ricfRGBA HasPalette=false Depth=32 PaletteColorCount=0
// BitOrder=riboBitsInOrder LineOrder=riloTopToBottom
// BitsPerPixel=32
var
  VBytes: TFPColorBytes absolute Value;
begin
  with PFourBytes(FRawImage.Data+FLineStarts^.Positions[y].Byte+(x shl 2))^ do
  begin
    B1 := VBytes.Bh;
    B2 := VBytes.Gh;
    B3 := VBytes.Rh;
  end;
end;

procedure TLazIntfImage.SetColor_BPP32_X8B8R8G8_BIO_TTB(x, y: integer; const Value: TFPColor);
// Format=ricfRGBA HasPalette=false Depth=32 PaletteColorCount=0
// BitOrder=riboBitsInOrder LineOrder=riloTopToBottom
// BitsPerPixel=32
var
  VBytes: TFPColorBytes absolute Value;
begin
  with PFourBytes(FRawImage.Data+FLineStarts^.Positions[y].Byte+(x shl 2))^ do
  begin
    B1 := VBytes.Bh;
    B2 := VBytes.Rh;
    B3 := VBytes.Gh;
  end;
end;

procedure TLazIntfImage.SetColor_BPP32_X8G8B8R8_BIO_TTB(x, y: integer; const Value: TFPColor);
// Format=ricfRGBA HasPalette=false Depth=32 PaletteColorCount=0
// BitOrder=riboBitsInOrder LineOrder=riloTopToBottom
// BitsPerPixel=32
var
  VBytes: TFPColorBytes absolute Value;
begin
  with PFourBytes(FRawImage.Data+FLineStarts^.Positions[y].Byte+(x shl 2))^ do
  begin
    B1 := VBytes.Gh;
    B2 := VBytes.Bh;
    B3 := VBytes.Rh;
  end;
end;

procedure TLazIntfImage.SetColor_BPP32_X8G8R8B8_BIO_TTB(x, y: integer; const Value: TFPColor);
// Format=ricfRGBA HasPalette=false Depth=32 PaletteColorCount=0
// BitOrder=riboBitsInOrder LineOrder=riloTopToBottom
// BitsPerPixel=32
var
  VBytes: TFPColorBytes absolute Value;
begin
  with PFourBytes(FRawImage.Data+FLineStarts^.Positions[y].Byte+(x shl 2))^ do
  begin
    B1 := VBytes.Gh;
    B2 := VBytes.Rh;
    B3 := VBytes.Bh;
  end;
end;

procedure TLazIntfImage.SetColor_BPP32_X8R8B8G8_BIO_TTB(x, y: integer; const Value: TFPColor);
// Format=ricfRGBA HasPalette=false Depth=32 PaletteColorCount=0
// BitOrder=riboBitsInOrder LineOrder=riloTopToBottom
// BitsPerPixel=32
var
  VBytes: TFPColorBytes absolute Value;
begin
  with PFourBytes(FRawImage.Data+FLineStarts^.Positions[y].Byte+(x shl 2))^ do
  begin
    B1 := VBytes.Rh;
    B2 := VBytes.Bh;
    B3 := VBytes.Gh;
  end;
end;

procedure TLazIntfImage.SetColor_BPP32_B8G8R8X8_BIO_TTB(x, y: integer; const Value: TFPColor);
// Format=ricfRGBA HasPalette=false Depth=32 PaletteColorCount=0
// BitOrder=riboBitsInOrder LineOrder=riloTopToBottom
// BitsPerPixel=32
var
  VBytes: TFPColorBytes absolute Value;
begin
  with PFourBytes(FRawImage.Data+FLineStarts^.Positions[y].Byte+(x shl 2))^ do
  begin
    B0 := VBytes.Bh;
    B1 := VBytes.Gh;
    B2 := VBytes.Rh;
  end;
end;

procedure TLazIntfImage.SetColor_BPP32_B8R8G8X8_BIO_TTB(x, y: integer; const Value: TFPColor);
// Format=ricfRGBA HasPalette=false Depth=32 PaletteColorCount=0
// BitOrder=riboBitsInOrder LineOrder=riloTopToBottom
// BitsPerPixel=32
var
  VBytes: TFPColorBytes absolute Value;
begin
  with PFourBytes(FRawImage.Data+FLineStarts^.Positions[y].Byte+(x shl 2))^ do
  begin
    B0 := VBytes.Bh;
    B1 := VBytes.Rh;
    B2 := VBytes.Gh;
  end;
end;

procedure TLazIntfImage.SetColor_BPP32_G8B8R8X8_BIO_TTB(x, y: integer; const Value: TFPColor);
// Format=ricfRGBA HasPalette=false Depth=32 PaletteColorCount=0
// BitOrder=riboBitsInOrder LineOrder=riloTopToBottom
// BitsPerPixel=32
var
  VBytes: TFPColorBytes absolute Value;
begin
  with PFourBytes(FRawImage.Data+FLineStarts^.Positions[y].Byte+(x shl 2))^ do
  begin
    B0 := VBytes.Gh;
    B1 := VBytes.Bh;
    B2 := VBytes.Rh;
  end;
end;

procedure TLazIntfImage.SetColor_BPP32_G8R8B8X8_BIO_TTB(x, y: integer; const Value: TFPColor);
// Format=ricfRGBA HasPalette=false Depth=32 PaletteColorCount=0
// BitOrder=riboBitsInOrder LineOrder=riloTopToBottom
// BitsPerPixel=32
var
  VBytes: TFPColorBytes absolute Value;
begin
  with PFourBytes(FRawImage.Data+FLineStarts^.Positions[y].Byte+(x shl 2))^ do
  begin
    B0 := VBytes.Gh;
    B1 := VBytes.Rh;
    B2 := VBytes.Bh;
  end;
end;

procedure TLazIntfImage.SetColor_BPP32_R8B8G8X8_BIO_TTB(x, y: integer; const Value: TFPColor);
// Format=ricfRGBA HasPalette=false Depth=32 PaletteColorCount=0
// BitOrder=riboBitsInOrder LineOrder=riloTopToBottom
// BitsPerPixel=32
var
  VBytes: TFPColorBytes absolute Value;
begin
  with PFourBytes(FRawImage.Data+FLineStarts^.Positions[y].Byte+(x shl 2))^ do
  begin
    B0 := VBytes.Rh;
    B1 := VBytes.Bh;
    B2 := VBytes.Gh;
  end;
end;

procedure TLazIntfImage.SetColor_BPP32_R8G8B8X8_BIO_TTB(x, y: integer; const Value: TFPColor);
// Format=ricfRGBA HasPalette=false Depth=32 PaletteColorCount=0
// BitOrder=riboBitsInOrder LineOrder=riloTopToBottom
// BitsPerPixel=32
var
  VBytes: TFPColorBytes absolute Value;
begin
  with PFourBytes(FRawImage.Data+FLineStarts^.Positions[y].Byte+(x shl 2))^ do
  begin
    B0 := VBytes.Rh;
    B1 := VBytes.Gh;
    B2 := VBytes.Bh;
  end;
end;

procedure TLazIntfImage.SetColor_BPP24_B8G8R8_BIO_TTB(x, y: integer; const Value: TFPColor);
// Format=ricfRGBA HasPalette=false Depth=32 PaletteColorCount=0
// BitOrder=riboBitsInOrder LineOrder=riloTopToBottom
// BitsPerPixel=32
var
  VBytes: TFPColorBytes absolute Value;
begin
  with PFourBytes(FRawImage.Data+FLineStarts^.Positions[y].Byte+(x * 3))^ do
  begin
    B0 := VBytes.Bh;
    B1 := VBytes.Gh;
    B2 := VBytes.Rh;
  end;
end;

procedure TLazIntfImage.SetColor_BPP24_B8R8G8_BIO_TTB(x, y: integer; const Value: TFPColor);
// Format=ricfRGBA HasPalette=false Depth=32 PaletteColorCount=0
// BitOrder=riboBitsInOrder LineOrder=riloTopToBottom
// BitsPerPixel=32
var
  VBytes: TFPColorBytes absolute Value;
begin
  with PFourBytes(FRawImage.Data+FLineStarts^.Positions[y].Byte+(x * 3))^ do
  begin
    B0 := VBytes.Bh;
    B1 := VBytes.Rh;
    B2 := VBytes.Gh;
  end;
end;

procedure TLazIntfImage.SetColor_BPP24_G8B8R8_BIO_TTB(x, y: integer; const Value: TFPColor);
// Format=ricfRGBA HasPalette=false Depth=32 PaletteColorCount=0
// BitOrder=riboBitsInOrder LineOrder=riloTopToBottom
// BitsPerPixel=32
var
  VBytes: TFPColorBytes absolute Value;
begin
  with PFourBytes(FRawImage.Data+FLineStarts^.Positions[y].Byte+(x * 3))^ do
  begin
    B0 := VBytes.Gh;
    B1 := VBytes.Bh;
    B2 := VBytes.Rh;
  end;
end;

procedure TLazIntfImage.SetColor_BPP24_G8R8B8_BIO_TTB(x, y: integer; const Value: TFPColor);
// Format=ricfRGBA HasPalette=false Depth=32 PaletteColorCount=0
// BitOrder=riboBitsInOrder LineOrder=riloTopToBottom
// BitsPerPixel=32
var
  VBytes: TFPColorBytes absolute Value;
begin
  with PFourBytes(FRawImage.Data+FLineStarts^.Positions[y].Byte+(x * 3))^ do
  begin
    B0 := VBytes.Gh;
    B1 := VBytes.Rh;
    B2 := VBytes.Bh;
  end;
end;

procedure TLazIntfImage.SetColor_BPP24_R8B8G8_BIO_TTB(x, y: integer; const Value: TFPColor);
// Format=ricfRGBA HasPalette=false Depth=32 PaletteColorCount=0
// BitOrder=riboBitsInOrder LineOrder=riloTopToBottom
// BitsPerPixel=32
var
  VBytes: TFPColorBytes absolute Value;
begin
  with PFourBytes(FRawImage.Data+FLineStarts^.Positions[y].Byte+(x * 3))^ do
  begin
    B0 := VBytes.Rh;
    B1 := VBytes.Bh;
    B2 := VBytes.Gh;
  end;
end;

procedure TLazIntfImage.SetColor_BPP24_R8G8B8_BIO_TTB(x, y: integer; const Value: TFPColor);
// Format=ricfRGBA HasPalette=false Depth=32 PaletteColorCount=0
// BitOrder=riboBitsInOrder LineOrder=riloTopToBottom
// BitsPerPixel=32
var
  VBytes: TFPColorBytes absolute Value;
begin
  with PFourBytes(FRawImage.Data+FLineStarts^.Positions[y].Byte+(x * 3))^ do
  begin
    B0 := VBytes.Rh;
    B1 := VBytes.Gh;
    B2 := VBytes.Bh;
  end;
end;

function TLazIntfImage.GetTColors(x, y: integer): TGraphicsColor;
begin
  Result:=FPColorToTColor(Colors[x,y]);
end;

procedure TLazIntfImage.SetTColors(x, y: integer; const AValue: TGraphicsColor);
begin
  Colors[x,y]:=TColorToFPColor(AValue);
end;

procedure TLazIntfImage.SetUsePalette(Value: boolean);
begin
  inherited // we can SetUsePalette(False);  // Can't handle palettes at the moment
end;

procedure TLazIntfImage.SetInternalColor(x, y: integer; const Value: TFPColor);
begin
  {if (x=0) and (y=0) then begin
    // a common bug in the readers is that Alpha is reversed
    DebugLn('TLazIntfImage.SetInternalColor ',x,',',y,' ',Value.Red,',',Value.Green,',',Value.Blue,',',Value.Alpha);
    if Value.Alpha<>alphaOpaque then
      RaiseGDBException('');
  end;}
  FSetInternalColorProc(x,y,Value);
  {if y=Height-1 then
    DebugLn(['TLazIntfImage.SetInternalColor x=',x,' y=',y,' ',dbgs(Value),' ',dbgs(GetInternalColor(x,y))]);}
end;

function TLazIntfImage.GetInternalColor(x, y: integer): TFPColor;
begin
  FGetInternalColorProc(x,y,Result);
end;

procedure TLazIntfImage.SetInternalPixel(x, y: integer; Value: integer);
begin
  if Palette = nil then Exit;
  if FRawImage.Description.PaletteColorCount = 0
  then begin
    // Non palettebased image so set the color
    SetInternalColor(x, y, Palette.Color[Value]);
  end
  else begin
    // TODO: Setting of palette colors
  end;
end;

procedure TLazIntfImage.SetMasked(x, y: integer; const AValue: Boolean);
begin
//  CheckIndex(x,y);
//-------------------------------- ct9999
SetMask_Generic(x,y,AValue);                              
FMaskSet:=(FMaskSet or AValue){and((self.Width and 3)=0)};
//--------------------------------
end;

function TLazIntfImage.GetInternalPixel(x, y: integer): integer;
begin
  if Palette = nil then Exit(0);

  if FRawImage.Description.PaletteColorCount = 0
  then begin
    // Non palettebased image so lookup the color
    Result := Palette.IndexOf(GetInternalColor(x, y));
  end
  else begin
    // TODO: Setting of palette colors
    Result := 0;
  end;
end;

function TLazIntfImage.GetMasked(x, y: integer): Boolean;
begin
//  CheckIndex (x,y);
  GetMask_Generic(x, y, Result);
end;

procedure TLazIntfImage.FreeData;
begin
  //DebugLn(Format('[TLazIntfImage.FreeData] Self=%x Data=%x', [PtrUInt(Self), PtrUInt(FRawImage.Data)]));
  if FDataOwner
  then ReallocMem(FRawImage.Data, 0)
  else FRawImage.Data := nil;
  FRawImage.DataSize := 0;

  if FLineStarts <> nil then Dispose(FLineStarts);
  FLineStarts := nil;
  
  if FDataOwner and (FRawImage.Mask <> nil)
  then ReallocMem(FRawImage.Mask, 0)
  else FRawImage.Mask := nil;
  FRawImage.MaskSize := 0;

  if FMaskLineStarts <> nil then Dispose(FMaskLineStarts);
  FMaskLineStarts := nil;
  FMaskSet := False;
  
  if FDataOwner and (FRawImage.Palette <> nil)
  then ReallocMem(FRawImage.Palette, 0)
  else FRawImage.Palette := nil;
  FRawImage.PaletteSize := 0;
  
  // old RawImage data has been cleared/destroyed => so new data must be owned by us
  FDataOwner := True;
end;

procedure TLazIntfImage.CreateData;
begin
  if FUpdateCount > 0
  then begin
    FCreateAllDataNeeded := True;
    Exit;
  end;
  FCreateAllDataNeeded := False;

  FreeData;

  New(FLineStarts);
  FLineStarts^.Init(Width, Height, FRawImage.Description.BitsPerPixel, FRawImage.Description.LineEnd, FRawImage.Description.LineOrder);
  New(FMaskLineStarts);
  FMaskLineStarts^.Init(Width, Height, FRawImage.Description.MaskBitsPerPixel, FRawImage.Description.MaskLineEnd, FRawImage.Description.LineOrder);

  FRawImage.CreateData(False);
end;

function TLazIntfImage.HasTransparency: boolean;
begin
  Result := FMaskSet or FRawImage.IsTransparent(True);
end;

function TLazIntfImage.HasMask: boolean;
begin
  Result := FMaskSet;
end;

procedure TLazIntfImage.SetDataDescriptionKeepData(
  const ADescription: TRawImageDescription);
begin
  FRawImage.Description:=ADescription;
end;

constructor TLazIntfImage.Create(AWidth, AHeight: integer);
begin
  Create(AWidth, AHeight, []);
end;

constructor TLazIntfImage.Create(AWidth, AHeight: integer; AFlags: TRawImageQueryFlags);
begin
  FDataOwner := True;
  FGetInternalColorProc := @GetColor_NULL;
  FSetInternalColorProc := @SetColor_NULL;
  inherited Create(AWidth, AHeight);

  if AFlags <> []
  then begin
    QueryDescription(FRawImage.Description, AFlags, AWidth, AHeight);
    ChooseGetSetColorFunctions;
  end;
end;

constructor TLazIntfImage.Create(ARawImage: TRawImage; ADataOwner: Boolean);
var
  Desc: TRawImageDescription absolute ARawImage.Description;
begin
  BeginUpdate;
  FRawImage := ARawImage;
  Create(Desc.Width, Desc.Height, []);
  FDataOwner := ADataOwner;
  FCreateAllDataNeeded := False;
  EndUpdate;
  New(FLineStarts);
  FLineStarts^.Init(Width, Height, Desc.BitsPerPixel, Desc.LineEnd, Desc.LineOrder);
  New(FMaskLineStarts);
  FMaskLineStarts^.Init(Width, Height, Desc.MaskBitsPerPixel, Desc.MaskLineEnd, Desc.LineOrder);
  ChooseGetSetColorFunctions;
end;

constructor TLazIntfImage.CreateCompatible(IntfImg: TLazIntfImage; AWidth,
  AHeight: integer);
var
  Desc: TRawImageDescription;
begin
  Create(0,0);
  Desc:=IntfImg.DataDescription;
  Desc.Width:=AWidth;
  Desc.Height:=AHeight;
  DataDescription:=Desc;
end;

destructor TLazIntfImage.Destroy;
begin
  FreeData;
  inherited Destroy;
end;

procedure TLazIntfImage.AlphaFromMask(AKeepAlpha: Boolean);
var
  x, y, xStop, yStop: Integer;
  Color: TFPColor;
begin
  if FRawImage.Mask = nil then Exit;
  if FRawImage.MaskSize = 0 then Exit;

  xStop := Width - 1;
  yStop := Height - 1;

  if AKeepAlpha
  then begin
    for y:=0 to yStop do
      for x:=0 to xStop do
      begin
        if not Masked[x,y] then Continue;
        Color := Colors[x,y];
        Color.alpha := Low(Color.alpha);
        Colors[x,y] := Color;
      end;
  end
  else begin
    for y:=0 to yStop do
      for x:=0 to xStop do
      begin
        Color := Colors[x,y];
        if Masked[x,y]
        then Color.alpha := Low(Color.alpha)
        else Color.alpha := High(Color.alpha);
        Colors[x,y] := Color;
      end;
  end;
end;

procedure TLazIntfImage.Mask(const AColor: TFPColor; AKeepOldMask: Boolean = False);
var
  x, y: Integer;
begin
  if AKeepOldMask then
    for y := 0 to Height - 1 do
      for x := 0 to Width - 1 do
        Masked[x,y] := Masked[x,y] or (Colors[x,y] = AColor)
  else
    for y := 0 to Height - 1 do
      for x := 0 to Width - 1 do
        Masked[x,y] := Colors[x,y] = AColor;
end;

procedure TLazIntfImage.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TLazIntfImage.EndUpdate;
begin
  if FUpdateCount = 0 then Exit;
  Dec(FUpdateCount);
  if FUpdateCount > 0 then Exit;

  if FCreateAllDataNeeded then
    CreateData;
  if FGetSetColorFunctionsUpdateNeeded then
    ChooseGetSetColorFunctions;
end;

//-------------------------------- ct9999
procedure TLazIntfImage.InternalSetSize(AWidth, AHeight: integer);
  procedure Error;
  begin raise FPImageException.Create('Invalid Size');end;
begin
if (AWidth<>Width)or(AHeight<>Height) then
   begin
   if (AWidth>=0)and(AHeight>=0) then
      begin
      inherited SetSize(AWidth,AHeight);
      FRawImage.Description.Width:=Width;
      FRawImage.Description.Height:=Height;
      end else Error;
   end;
end;
//--------------------------------

procedure TLazIntfImage.SetSize(AWidth, AHeight: integer);
begin
  InternalSetSize(AWidth, AHeight);
  CreateData;
end;

//-------------------------------- ct9999
function TLazIntfImage.CheckDescription(const ADescription: TRawImageDescription; ExceptionOnError: boolean): boolean;
  procedure DoError(const Msg: string);
  begin
  if ExceptionOnError then Raise FPImageException.Create(Msg);

  end;
begin
// check format
if (ADescription.Format in [low(TRawImageColorFormat)..high(TRawImageColorFormat)]) then Result:=true else
   begin
   DoError('Invalid Raw Image Description Format');
   Result:=false;
   end;
end;
//--------------------------------

procedure TLazIntfImage.GetXYDataPosition(x, y: integer; out Position: TRawImagePosition);
begin
  Position := FLineStarts^.GetPosition(x, y);
end;

procedure TLazIntfImage.GetXYMaskPosition(x, y: integer; out Position: TRawImagePosition);
begin
  Position := FMaskLineStarts^.GetPosition(x, y);
end;

function TLazIntfImage.GetDataLineStart(y: integer): Pointer;
begin
  if FRawimage.Description.LineOrder = riloBottomToTop then
    y:=Height-y;
  Result := FRawImage.Data+FLineStarts^.Positions[y].Byte;
end;

//-------------------------------- ct9999
procedure TLazIntfImage.LoadFromDevice(DC: HDC);
var RawImage: TRawImage;
    DeviceSize: TPoint;
begin
GetDeviceSize(DC,DeviceSize);
if RawImage_FromDevice(RawImage,DC,Rect(0,0,DeviceSize.X,DeviceSize.Y)) then SetRawImage(RawImage) else
   raise FPImageException.Create('Failed to get raw image from device');
end;

procedure TLazIntfImage.LoadFromBitmap(ABitmap, AMaskBitmap: HBitmap;
  AWidth: integer; AHeight: integer);
var R: TRect;
    RawImage: TRawImage;
    Desc: TRawImageDescription;
begin
if RawImage_DescriptionFromBitmap(ABitmap,Desc) then
   begin
   if AWidth<0 then AWidth:=Desc.Width;
   if AHeight<0 then AHeight:=Desc.Height;
   R := Rect(0,0,AWidth,AHeight);
   if RawImage_FromBitmap(RawImage,ABitmap,AMaskBitmap,@R) then SetRawImage(RawImage) else
      raise FPImageException.Create('Failed to get raw image from bitmap');
   end else raise FPImageException.Create('Failed to get raw image description from bitmap');
end;
//--------------------------------

procedure TLazIntfImage.CreateBitmaps(out ABitmap, AMask: HBitmap; ASkipMask: boolean);
begin
  if not RawImage_CreateBitmaps(FRawImage, ABitmap, AMask, ASkipMask)
  then raise FPImageException.Create('Failed to create handles');
end;

procedure TLazIntfImage.SetRawImage(const ARawImage: TRawImage; ADataOwner: Boolean);
var
  Desc: TRawImageDescription absolute ARawImage.Description;
begin
  if FRawImage.IsEqual(ARawImage) then Exit;

  BeginUpdate;
  try
    FreeData;
    FRawImage := ARawImage;
    FDataOwner := ADataOwner;
    SetSize(Desc.Width, Desc.Height);
    FCreateAllDataNeeded := False;
    New(FLineStarts);
    FLineStarts^.Init(Width, Height, Desc.BitsPerPixel, Desc.LineEnd, Desc.LineOrder);
    New(FMaskLineStarts);
    FMaskLineStarts^.Init(Width, Height, Desc.MaskBitsPerPixel, Desc.MaskLineEnd, Desc.LineOrder);
    ChooseGetSetColorFunctions;
  finally
    EndUpdate;
  end;
end;

procedure TLazIntfImage.GetRawImage(out ARawImage: TRawImage; ATransferOwnership: Boolean);
begin
  ARawImage := FRawImage;
  if ATransferOwnership
  then FDataOwner := False;
end;

procedure TLazIntfImage.FillPixels(const Color: TFPColor);
var
  ColorChar: char;
  ColorWord: Word;
  Cnt: Integer;
  i: Integer;
  ColorDWord: Cardinal;
  y: Integer;
  x: Integer;
begin
  if (Width=0) or (Height=0) or (FRawImage.Data=nil) then exit;

  case FRawImage.Description.BitsPerPixel of

  8:
    begin
      SetInternalColor(0,0,Color);
      ColorChar:=Char(FRawImage.Data[0]);
      FillChar(FRawImage.Data^,FRawImage.DataSize,ColorChar);
    end;

  16:
    begin
      SetInternalColor(0,0,Color);
      ColorWord:=PWord(FRawImage.Data)[0];
      Cnt:=FRawImage.DataSize div 2;
      for i:=0 to Cnt-1 do
        PWord(FRawImage.Data)[i]:=ColorWord;
    end;

  32:
    begin
      SetInternalColor(0,0,Color);
      ColorDWord:=PDWord(FRawImage.Data)[0];
      Cnt:=FRawImage.DataSize div 4;
      for i:=0 to Cnt-1 do
        PDWord(FRawImage.Data)[i]:=ColorDWord;
    end;

  else
    for y:=0 to Height-1 do
      for x:=0 to Width-1 do
        SetInternalColor(x,y,Color);
  end;

  // ToDo: mask
end;

{
  Merges an image to a canvas using alpha blend acording to a separate image
  containing the alpha channel. White pixels in the alpha channel will correspond
  to the source image pixel being fully drawn, grey ones are merged and
  black ones ignored.

  If ASourceAlpha = nil then it will utilize the alpha channel from ASource
}
procedure TLazIntfImage.AlphaBlend(ASource, ASourceAlpha: TLazIntfImage;
  const ADestX, ADestY: Integer);
var
  x, y, CurX, CurY: Integer;
  MaskValue, InvMaskValue: Word;
  CurColor: TFPColor;
  lDrawWidth, lDrawHeight: Integer;
begin
  // Take care not to draw outside the destination area
  lDrawWidth := Min(Self.Width - ADestX, ASource.Width);
  lDrawHeight := Min(Self.Height - ADestY, ASource.Height);
  for y := 0 to lDrawHeight - 1 do
  begin
    for x := 0 to lDrawWidth - 1 do
    begin
      CurX := ADestX + x;
      CurY := ADestY + y;

      // Never draw outside the destination
      if (CurX < 0) or (CurY < 0) then Continue;

      if ASourceAlpha <> nil then
        MaskValue := ASourceAlpha.Colors[x, y].alpha
      else
        MaskValue := ASource.Colors[x, y].alpha;

      InvMaskValue := $FFFF - MaskValue;

      if MaskValue = $FFFF then
      begin
        Self.Colors[CurX, CurY] := ASource.Colors[x, y];
      end
      else if MaskValue > $00 then
      begin
        CurColor := Self.Colors[CurX, CurY];

        CurColor.Red := Round(
          CurColor.Red * InvMaskValue / $FFFF +
          ASource.Colors[x, y].Red * MaskValue / $FFFF);

        CurColor.Green := Round(
          CurColor.Green * InvMaskValue / $FFFF +
          ASource.Colors[x, y].Green * MaskValue / $FFFF);

        CurColor.Blue := Round(
          CurColor.Blue * InvMaskValue / $FFFF +
          ASource.Colors[x, y].Blue * MaskValue / $FFFF);

        Self.Colors[CurX, CurY] := CurColor;
      end;
    end;
  end;
end;

//-------------------------------- ct9999
procedure TLazIntfImage.CopyPixels(ASource: TFPCustomImage; XDst: Integer; YDst: Integer; AlphaMask: Boolean; AlphaTreshold: Word);
var SrcImg: TLazIntfImage absolute ASource;
    SrcHasMask, DstHasMask: Boolean;
    x, y, xStop, yStop: Integer;
    c: TFPColor;
begin
{ if (Src.Width<>Width) or (Src.Height<>Height) then SetSize(Src.Width,Src.Height); }
if (ASource is TLazIntfImage) and FRawImage.Description.IsEqual(SrcImg.FRawImage.Description) and (XDst =  0) and (YDst = 0) then
   begin
   // same description -> copy
   if FRawImage.Data <> nil then System.Move(SrcImg.FRawImage.Data^,FRawImage.Data^,FRawImage.DataSize);
   if FRawImage.Mask <> nil then System.Move(SrcImg.FRawImage.Mask^,FRawImage.Mask^,FRawImage.MaskSize);
   end else
   begin
   // copy pixels
   xStop:=ASource.Width;
   if xStop>Width-XDst then xStop:=Width-XDst;
   Dec(xStop);
   yStop:=ASource.Height;
   if yStop>Height-YDst then yStop:=Height-YDst;
   Dec(yStop);
   if ASource is TLazIntfImage then SrcHasMask:=(SrcImg.FRawImage.Description.MaskBitsPerPixel>0) else
      SrcHasMask:=False;
   DstHasMask:=(FRawImage.Description.MaskBitsPerPixel>0);
   if DstHasMask and (ASource is TLazIntfImage) then
      begin
      for y:=0 to yStop do
          for x:=0 to xStop do Masked[x+XDst,y+YDst]:=(SrcHasMask and SrcImg.Masked[x,y]);
      end;
   for y:=0 to yStop do
       for x:=0 to xStop do
           begin
           c := ASource.Colors[x,y];
           if not DstHasMask and SrcHasMask and (c.alpha = $FFFF) then // copy mask to alpha channel
              if SrcImg.Masked[x,y] then c.alpha:=0;
           Colors[x+XDst,y+YDst] := c;
           if AlphaMask and (c.alpha<AlphaTreshold) then Masked[x+XDst,y+YDst]:=True;
           end;
   end;
end;
//--------------------------------

{ TLazReaderXPM }

type
  TXPMPixelToColorEntry = record
    Color: TFPColor;
  end;
  PXPMPixelToColorEntry = ^TXPMPixelToColorEntry;

procedure TLazReaderXPM.ClearPixelToColorTree;
var
  Entry: PXPMPixelToColorEntry;
  ArrNode: TArrayNode;
begin
  if FPixelToColorTree<>nil then begin
    ArrNode:=FPixelToColorTree.Root;
    while ArrNode<>nil do begin
      Entry:=PXPMPixelToColorEntry(ArrNode.Data);
      if Entry<>nil then begin
        //DebugLn('TLazReaderXPM.ClearPixelToColorTree A ',DbgS(ArrNode),' ',DbgS(Entry));
        Dispose(Entry);
      end;
      ArrNode:=ArrNode.FindNextUTF8;
    end;
    FPixelToColorTree.Free;
    FPixelToColorTree:=nil;
  end;
end;

procedure TLazReaderXPM.InternalRead(Str: TStream; Img: TFPCustomImage);
type
  TSrcLine = record
    StartPos: integer;
    EndPos: integer;
  end;

var
  SrcPos: integer;
  Src: String;
  SrcLen: Integer;
  CurLineNumber, LastLineStart: integer;
  HasAlpha: Boolean;

  procedure RaiseXPMReadError(const Msg: string; ReadPos: integer);
  var
    CurColumn: Integer;
  begin
    CurColumn:=ReadPos-LastLineStart+1;
    raise Exception.Create(Msg
                           +' in xpm stream at line '+IntToStr(CurLineNumber)
                           +' column '+IntToStr(CurColumn));
  end;

  // read next string constant "" and skip comments
  function ReadNextLine(var Line: TSrcLine;
    ExceptionOnNotFound: Boolean): boolean;
  begin
    while SrcPos<=SrcLen do begin
      case Src[SrcPos] of

      #10,#13:
        begin
          // count linenumbers for nicer error output
          inc(SrcPos);
          inc(CurLineNumber);
          if (SrcPos<=SrcLen) and (Src[SrcPos] in [#10,#13])
          and (Src[SrcPos]<>Src[SrcPos-1]) then
            inc(SrcPos);
          LastLineStart:=SrcPos;
        end;

      '/':
        begin
          if (SrcPos<SrcLen) and (Src[SrcPos+1]='*') then begin
            // this is a C comment
            // -> skip comment
            inc(SrcPos,2);
            while (SrcPos<SrcLen) do begin
              if (Src[SrcPos]='*') and (Src[SrcPos+1]='/') then begin
                // comment end found
                inc(SrcPos,2);
                break;
              end;
              inc(SrcPos);
            end;
          end else
            RaiseXPMReadError('syntax error',SrcPos);
        end;

      '"':
        begin
          // start of a string constant
          inc(SrcPos);
          Line.StartPos:=SrcPos;
          while (SrcPos<SrcLen) do begin
            if (Src[SrcPos]='"') and (Src[SrcPos-1]<>'\') then begin
              // string end found
              Line.EndPos:=SrcPos;
              //DebugLn('  ',copy(Src,Line.StartPos-1,Line.EndPos-Line.StartPos+2));
              inc(SrcPos);
              Result:=true;
              exit;
            end;
            inc(SrcPos);
          end;
        end;

      else
        inc(SrcPos);
      end;
    end;
    Result:=false;
    if ExceptionOnNotFound then
      Raise Exception.Create('Unexpected end of xpm stream');
  end;

  function ReadNumber(var ReadPos: integer;
    ExceptionOnNotFound: Boolean): integer;
  begin
    // skip spaces
    while IsSpaceChar[Src[ReadPos]] do inc(ReadPos);
    // read number
    Result:=0;
    if IsNumberChar[Src[ReadPos]] then begin
      repeat
        Result:=Result*10+ord(Src[ReadPos])-Ord('0');
        inc(ReadPos);
      until not IsNumberChar[Src[ReadPos]];
    end else if ExceptionOnNotFound then
      RaiseXPMReadError('number expected',ReadPos);
  end;

  procedure ReadHeader;
  var
    FirstLine: TSrcLine;
  begin
    ReadNextLine(FirstLine,true);
    FWidth:=ReadNumber(FirstLine.StartPos,true);
    FHeight:=ReadNumber(FirstLine.StartPos,true);
    FColorCount:=ReadNumber(FirstLine.StartPos,true);
    FCharsPerPixel:=ReadNumber(FirstLine.StartPos,true);
    fXHot:=ReadNumber(FirstLine.StartPos,false);
    fYHot:=ReadNumber(FirstLine.StartPos,fXHot<>0);
    //DebugLn('ReadHeader A Width=',FWidth,' Height=',FHeight,' ColorCount=',FColorCount,' CharsPerPixel=',FCharsPerPixel);
    // ToDo: parse XPMExt tag
  end;

  function HexToColor(HexStart, HexEnd: integer): TFPColor;

    procedure ReadHexNumber(var StartPos: integer; Len: integer;
      var Number: word);
    var
      c: Char;
      i: Integer;
    begin
      Number:=0;
      for i:=1 to 4 do begin
        Number:=Number shl 4;
        if i<=Len then begin
          c:=Src[StartPos];
          case c of
          '0'..'9': inc(Number,ord(c)-ord('0'));
          'A'..'F': inc(Number,ord(c)-ord('A')+10);
          'a'..'f': inc(Number,ord(c)-ord('a')+10);
          end;
          inc(StartPos);
        end;
      end;
      // fill missing bits
      case Len of
      1: Number:=Number or (Number shr 4) or (Number shr 8) or (Number shr 12);
      2: Number:=Number or (Number shr 8);
      3: Number:=Number or (Number shr 12);
      end;
    end;

  var
    HexLen: Integer;
    SampleLen: Integer;
    SampleStart: Integer;
  begin
    HexLen:=HexEnd-HexStart;
    case HexLen of
    3: SampleLen:=1;
    6: SampleLen:=2;
    9: SampleLen:=3;
    12:SampleLen:=4;
    else
      RaiseXPMReadError('hexnumber expected',HexStart);
    end;
    SampleStart:=HexStart;
    ReadHexNumber(SampleStart,SampleLen,Result.Red);
    ReadHexNumber(SampleStart,SampleLen,Result.Green);
    ReadHexNumber(SampleStart,SampleLen,Result.Blue);
    Result.Alpha:=alphaOpaque;
  end;

  function TextToColor(TextStart, TextEnd: integer): TFPColor;
  var
    s: String;
  begin
    s := lowercase(copy(Src,TextStart,TextEnd-TextStart));
    if s = 'transparent' then
      Result := FPImage.colTransparent
    else if s = 'none' then
      Result := FPImage.colTransparent
    else if s = 'black' then
      result := FPImage.colBlack
    else if s = 'blue' then
      Result := FPImage.colBlue
    else if s = 'green' then
      Result := FPImage.colGreen
    else if s = 'cyan' then
      Result := FPImage.colCyan
    else if s = 'red' then
      Result := FPImage.colRed
    else if s = 'magenta' then
      Result := FPImage.colMagenta
    else if s = 'yellow' then
      Result := FPImage.colYellow
    else if s = 'white' then
      Result := FPImage.colWhite
    else if s = 'gray' then
      Result := FPImage.colGray
    else if s = 'lightgray' then
      Result := FPImage.colLtGray
    else if (s = 'darkgray') or (s='grey') then
      Result := FPImage.colDKGray
    else if s = 'darkblue' then
      Result := FPImage.colDkBlue
    else if s = 'darkgreen' then
      Result := FPImage.colDkGreen
    else if s = 'darkcyan' then
      Result := FPImage.colDkCyan
    else if s = 'darkred' then
      Result := FPImage.colDkRed
    else if s = 'darkmagenta' then
      Result := FPImage.colDkMagenta
    else if s = 'darkyellow' then
      Result := FPImage.colDkYellow
    else if s = 'maroon' then
      Result := FPImage.colMaroon
    else if s = 'lightgreen' then
      Result := FPImage.colLtGreen
    else if s = 'olive' then
      Result := FPImage.colOlive
    else if s = 'navy' then
      Result := FPImage.colNavy
    else if s = 'purple' then
      Result := FPImage.colPurple
    else if s = 'teal' then
      Result := FPImage.colTeal
    else if s = 'silver' then
      Result := FPImage.colSilver
    else if s = 'lime' then
      Result := FPImage.colLime
    else if s = 'fuchsia' then
      Result := FPImage.colFuchsia
    else if s = 'aqua' then
      Result := FPImage.colAqua
    else
      Result := FPImage.colTransparent;
  end;

  procedure AddColor(PixelStart: integer; const AColor: TFPColor;
    IntArray: PInteger);
  var
    NewEntry: PXPMPixelToColorEntry;
    i: Integer;
  begin
    {DebugLn('TLazReaderXPM.InternalRead.AddColor A "',DbgStr(copy(Src,PixelStart,FCharsPerPixel)),'"=',
      DbgS(AColor.Red),',',
      DbgS(AColor.Green),',',
      DbgS(AColor.Blue),',',
      DbgS(AColor.Alpha));}
    New(NewEntry);
    NewEntry^.Color:=AColor;
    // add entry to Array Tree
    if FPixelToColorTree=nil then
      FPixelToColorTree:=TArrayNodesTree.Create;
    for i:=0 to FCharsPerPixel-1 do
      IntArray[i]:=ord(Src[PixelStart+i]);
    FPixelToColorTree.SetNode(IntArray,FCharsPerPixel,NewEntry);
    //if FPixelToColorTree.FindData(IntArray,FCharsPerPixel)<>NewEntry then RaiseGDBException('');
  end;

  procedure ReadPalette(IntArray: PInteger);
  var
    i: Integer;
    Line: TSrcLine;
    ReadPos: Integer;
    ColorStart: Integer;
    ColorEnd: Integer;
    NewColor: TFPColor;
    PixelStart: Integer;
  begin
  for i:=1 to FColorCount do
      begin
      ReadNextLine(Line,true);
      ReadPos:=Line.StartPos;
      // read pixel string
      PixelStart:=ReadPos;
      inc(ReadPos,FCharsPerPixel);
      // skip spaces
      while IsSpaceChar[Src[ReadPos]] do inc(ReadPos);
      // read 'c' (sometimes the 'c' is an 's')
      if (Src[ReadPos] in ['c','s']) then
         begin
         inc(ReadPos);
         // skip spaces
         while IsSpaceChar[Src[ReadPos]] do inc(ReadPos);
         // read color string
         ColorStart:=ReadPos;
         if Src[ReadPos]='#' then
            begin
            inc(ColorStart);
            // read as hexnumber
            repeat
            inc(ReadPos);
            until not (IsHexNumberChar[Src[ReadPos]]);
            ColorEnd:=ReadPos;
            NewColor:=HexToColor(ColorStart,ColorEnd);
            end else
            begin
            // read as text
            repeat
            inc(ReadPos);
            until not (Src[ReadPos] in ['A'..'Z','a'..'z']);
            ColorEnd:=ReadPos;
            NewColor:=TextToColor(ColorStart,ColorEnd);
            end;
         AddColor(PixelStart,NewColor,IntArray);
         HasAlpha := HasAlpha or (NewColor.alpha<>alphaOpaque);
         end else RaiseXPMReadError('"c" expected',ReadPos);
      end;
  end;

  procedure ReadPixels(IntArray: PInteger);
  var x,y,i,ReadPos: Integer;
      Line: TSrcLine;
      CurColor: TFPColor;
      CurEntry: PXPMPixelToColorEntry;
  begin
  Img.SetSize(FWidth, FHeight);
  for y:=0 to FHeight-1 do
      begin
      if not FContinue then Exit;
      ReadNextLine(Line,true);
      ReadPos:=Line.StartPos;
      if Line.EndPos-Line.StartPos>=FCharsPerPixel*FWidth then
         begin
         for x:=0 to FWidth-1 do
             begin
             //DebugLn('ReadPixels x=',dbgs(x),' y=',dbgs(y),' color="',DbgStr(copy(Src,ReadPos,FCharsPerPixel)),'"');
             for i:=0 to FCharsPerPixel-1 do
                 begin
                 IntArray[i]:=ord(Src[ReadPos]);
                 inc(ReadPos);
                 end;
             CurEntry:=PXPMPixelToColorEntry(FPixelToColorTree.FindData(IntArray,FCharsPerPixel));
             if CurEntry<>nil then CurColor:=CurEntry^.Color else RaiseXPMReadError('invalid color',ReadPos-FCharsPerPixel);
             {if CurEntry2<>CurEntry then begin
              DebugLn('x=',x,' y=',y,' Pixel=',Entry^.Pixel,
              ' RefPixel=',CurEntry^.Pixel,
              ' Color=',
              DbgS(CurColor.Red),',',
              DbgS(CurColor.Green),',',
              DbgS(CurColor.Blue),',',
              DbgS(CurColor.Alpha));
              DebugLn('Entry2: Pixel=',CurEntry2^.Pixel,
              ' RefPixel=',CurEntry2^.Pixel,
              ' Color=',
              DbgS(CurEntry2^.Color.Red),',',
              DbgS(CurEntry2^.Color.Green),',',
              DbgS(CurEntry2^.Color.Blue),',',
              DbgS(CurEntry2^.Color.Alpha));
              end;}
              {DebugLn('x=',x,' y=',y,' Pixel=',Entry^.Pixel,
               ' RefPixel=',PXPMPixelToColorEntry(Node.Data)^.Pixel,
               ' Color=',
               DbgS(CurColor.Red),',',
               DbgS(CurColor.Green),',',
               DbgS(CurColor.Blue),',',
               DbgS(CurColor.Alpha));}
             Img.Colors[x,y]:=CurColor;
             end;
         Progress(psRunning,trunc(100*(Y+1)/FHeight),False,Rect(0,0,FWidth-1,y),'reading XPM pixels',FContinue);
         end else RaiseXPMReadError('line too short',ReadPos);
      end;
  end;
  
var
  IntArray: array of Integer;
  Desc: TRawImageDescription;
begin
  FContinue := True;
  Progress(psStarting, 0, False, Rect(0,0,0,0), '', FContinue);

  ClearPixelToColorTree;
  Src:=ReadCompleteStreamToString(Str,1024);
  SrcLen:=length(Src);
  SrcPos:=1;
  CurLineNumber:=1;
  LastLineStart:=1;
  ReadHeader;
  
  SetLength(IntArray, FCharsPerPixel+1);

  HasAlpha := False;
  ReadPalette(@IntArray[0]);

  if FUpdateDescription and (theImage is TLazIntfImage)
  then begin
    if HasAlpha
    then DefaultReaderDescription(FWidth, FHeight, 32, Desc)
    else DefaultReaderDescription(FWidth, FHeight, 24, Desc);
//  MWE: keep mask ?
//    if FMaskMode = lrmmNone
//    then Desc.MaskBitsPerPixel := 0;
    TLazIntfImage(theImage).DataDescription := Desc;
  end
  else begin
    if HasAlpha
    then CheckAlphaDescription(TheImage);
  end;
  //FPixelToColorTree.ConsistencyCheck;
  ReadPixels(@IntArray[0]);

  Progress(psEnding, 100, false, Rect(0,0,0,0), '', FContinue);
end;

function TLazReaderXPM.QueryInterface(constref iid: TGuid; out obj): longint; {$IFDEF WINDOWs}stdcall{$ELSE}cdecl{$ENDIF};
begin
  if GetInterface(iid, obj)
  then Result := S_OK
  else Result := E_NOINTERFACE;
end;

procedure TLazReaderXPM.SetUpdateDescription(AValue: Boolean);
begin
  FUpdateDescription := AValue;
end;

function TLazReaderXPM._AddRef: LongInt; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
begin
  Result := -1;
end;

function TLazReaderXPM._Release: LongInt; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
begin
  Result := -1;
end;

function TLazReaderXPM.InternalCheck(Str: TStream): boolean;
var s : string[9];
    l : integer;
begin
  try
    l := str.Read (s[1],9);
    s[0] := char(l);
    if l <> 9 then
      result := False
    else
      result := (s = '/* XPM */');
  except
    result := false;
  end;
end;

constructor TLazReaderXPM.Create;
begin
  inherited Create;
end;

destructor TLazReaderXPM.Destroy;
begin
  ClearPixelToColorTree;
  inherited Destroy;
end;

function TLazReaderXPM.GetUpdateDescription: Boolean;
begin
  Result := FUpdateDescription;
end;

{ TLazAVLPalette }

type
  TLazAVLPaletteEntry = record
    Palette: TLazAVLPalette;
    Index: integer;
  end;
  PLazAVLPaletteEntry = ^TLazAVLPaletteEntry;

function CompareLazAVLPaletteEntries(Entry1, Entry2: PLazAVLPaletteEntry): integer;
begin
  Result := Entry1^.Palette.CompareEntries(Entry1^.Index, Entry2^.Index);
end;

function ComparePFPColorAndLazAVLPalEntry(PColor: PFPColor; Entry: PLazAVLPaletteEntry): integer;
begin
  Result := Entry^.Palette.CompareColorWithEntries(PColor^, Entry^.Index);
end;

procedure TLazAVLPalette.SetCount(NewCount: integer);
var
  NewAVLPalEntry: PLazAVLPaletteEntry;
  AVLNode: TAvgLvlTreeNode;
  CurAVLPalEntry: PLazAVLPaletteEntry;
  Index: Integer;
begin
  if FCount=NewCount then exit;
  // remove unused colors from 'color to index' tree
  if FAVLPalette<>nil then begin
    for Index:=FCount-1 downto NewCount do begin
      AVLNode:=FAVLNodes[Index];
      CurAVLPalEntry:=PLazAVLPaletteEntry(AVLNode.Data);
      FAVLPalette.Delete(AVLNode);
      FAVLNodes[Index]:=nil;
      Dispose(CurAVLPalEntry);
    end;
  end;
  inherited SetCount(NewCount);
  // create tree if not already done
  if (FAVLPalette=nil) and (FCount>0) then
    FAVLPalette:=TAvgLvlTree.Create(TListSortCompare(@CompareLazAVLPaletteEntries));
  if FAVLPalette=nil then exit;
  // add new colors to 'color to index' tree and 'index to node' array
  while FAVLPalette.Count<FCount do begin
    Index:=FAVLPalette.Count;
    New(NewAVLPalEntry);
    NewAVLPalEntry^.Palette:=Self;
    NewAVLPalEntry^.Index:=Index;
    FAVLNodes[Index]:=FAVLPalette.Add(NewAVLPalEntry);
  end;
end;

procedure TLazAVLPalette.SetColor(Index: integer; const NewColor: TFPColor);
var
  Node: TAvgLvlTreeNode;
  Entry: PLazAVLPaletteEntry;
begin
  if Index=FCount then
    Add(NewColor)
  else begin
    CheckIndex(Index);
    if FData^[Index]=NewColor then exit;
    // remove node from tree
    Node:=FAVLNodes[Index];
    Entry:=PLazAVLPaletteEntry(Node.Data);
    FAVLPalette.Delete(Node);
    // change color
    FData^[index] := NewColor;
    // add node
    FAVLNodes[Index]:=FAVLPalette.Add(Entry);
  end;
end;

destructor TLazAVLPalette.Destroy;
begin
  SetCount(0);
  FAVLPalette.Free;
  FAVLPalette:=nil;
  if FCapacity>0 then
    FreeMem(FAVLNodes);
  inherited Destroy;
end;

function TLazAVLPalette.IndexOf(const AColor: TFPColor): integer;
var
  Node: TAvgLvlTreeNode;
begin
  if FAVLPalette<>nil then
    Node:=FAVLPalette.FindKey(@AColor,TListSortCompare(@ComparePFPColorAndLazAVLPalEntry))
  else
    Node:=nil;
  if Node<>nil then
    Result:=PLazAVLPaletteEntry(Node.Data)^.Index
  else
    Result:=Add(AColor);
end;

function TLazAVLPalette.Add(const NewColor: TFPColor): integer;
begin
  Result:=FCount;
  if FCount=FCapacity then EnlargeData;
  SetCount(FCount+1);
  SetColor(Result,NewColor);
end;

function TLazAVLPalette.CompareEntries(Index1, Index2: integer): integer;
begin
  Result:=CompareColors(FData^[Index1],FData^[Index2]);
end;

function TLazAVLPalette.CompareColorWithEntries(const AColor: TFPColor;
  Index: integer): integer;
begin
  Result:=CompareColors(AColor,FData^[Index]);
end;

procedure TLazAVLPalette.EnlargeData;
var
  NewCapacity: Integer;
begin
  if FCapacity<16 then
    NewCapacity:=32
  else if FCapacity<64 then
    NewCapacity:=128
  else
    NewCapacity:=FCapacity*2;
  ReallocMem(FData,SizeOf(TFPColor)*NewCapacity);
  ReallocMem(FAVLNodes,SizeOf(Pointer)*NewCapacity);
  FCapacity:=NewCapacity;
end;

procedure TLazAVLPalette.CheckConsistency;
var Node: TAvgLvlTreeNode;
    Entry: PLazAVLPaletteEntry;
    i: Integer;
begin
if FAVLPalette<>nil then
   begin
   FAVLPalette.ConsistencyCheck;

   end;
if FAVLNodes<>nil then
   begin
   for i:=0 to FCapacity-1 do
       begin
       Node:=FAVLNodes[i];
       if i>=FCount then continue;
       if Node<>nil then
          begin
          Entry:=PLazAVLPaletteEntry(Node.Data);
          if Entry<>nil then
             begin
             if Entry^.Index=i then
                begin

                end;
             end;
          end;
       end;
   end;
end;

{ TLazWriterXPM }

const
  DefXPMPalChars = '.,-*abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'
                  +'0123456789@#;:=+%$()[]';

procedure TLazWriterXPM.SetNibblesPerSample(const AValue: word);
begin
  if FNibblesPerSample=AValue then exit;
  FNibblesPerSample:=AValue;
  if FNibblesPerSample>4 then FNibblesPerSample:=4;
  FRightShiftSample:=(4-FNibblesPerSample)*4;
end;

procedure TLazWriterXPM.InternalWrite(Str: TStream; Img: TFPCustomImage);
var
  Palette: TLazAVLPalette;
  PixelStrings: ^AnsiString;
  ColorStrings: ^AnsiString;
  CharsPerPixel: Integer;
  LineEnd: string;

  function GetColor(x,y: integer): TFPColor;
  begin
    Result:=Img.Colors[x,y];
    if (Result.Alpha>=(alphaOpaque shr 1)) then
      Result.Alpha:=alphaOpaque
    else
      Result:=colTransparent;
    Result.Red:=Result.Red shr FRightShiftSample;
    Result.Green:=Result.Green shr FRightShiftSample;
    Result.Blue:=Result.Blue shr FRightShiftSample;
  end;

  function SampleToHex(Sample: word): string;
  begin
    Result:=HexStr(Sample,FNibblesPerSample);
  end;

  procedure BuildPalette;
  var
    x: Integer;
    y: Integer;
    PixelStringsSize: Integer;
    i: Integer;
    Rest: Integer;
    c: char;
    CharPos: Integer;
    ColorStringsSize: Integer;
    Color: TFPColor;
  begin
    // create Palette
    Palette:=TLazAVLPalette.Create(0);
    for y:=0 to Img.Height-1 do
      for x:=0 to Img.Width-1 do
        Palette.IndexOf(GetColor(x,y));
    // calclulate CharsPerPixel
    CharsPerPixel:=0;
    i:=Palette.Count;
    while i>0 do begin
      i:=i div length(DefXPMPalChars);
      inc(CharsPerPixel);
    end;
    // create pixel strings
    PixelStringsSize:=SizeOf(Pointer)*Palette.Count;
    ReAllocMem(PixelStrings,PixelStringsSize);
    FillChar(PixelStrings^,PixelStringsSize,0);
    for i:=0 to Palette.Count-1 do begin
      SetLength(PixelStrings[i],CharsPerPixel);
      Rest:=i;
      for CharPos:=CharsPerPixel downto 1 do begin
        c:=DefXPMPalChars[(Rest mod length(DefXPMPalChars))+1];
        PixelStrings[i][CharPos]:=c;
        Rest:=Rest div length(DefXPMPalChars);
      end;
    end;
    // create color strings
    ColorStringsSize:=SizeOf(Pointer)*Palette.Count;
    ReAllocMem(ColorStrings,ColorStringsSize);
    FillChar(ColorStrings^,ColorStringsSize,0);
    for i:=0 to Palette.Count-1 do begin
      Color:=Palette[i];
      if Color.Alpha=0 then begin
        ColorStrings[i]:='None';
      end else begin
        ColorStrings[i]:='#'+SampleToHex(Color.Red)+SampleToHex(Color.Green)
                            +SampleToHex(Color.Blue);
      end;
    end;
  end;

  procedure WriteString(const s: string);
  begin
    Str.Write(s[1],length(s));
  end;

  procedure WriteHeader;
  var
    s: String;
  begin
    s:='/* XPM */'+LineEnd;
    s:=s+'static char *graphic[] = {'+LineEnd;
    s:=s+'"'+IntToStr(Img.Width)+' '+IntToStr(Img.Height)
        +' '+IntToStr(Palette.Count)+' '+IntToStr(CharsPerPixel)+'"';
    if Palette.Count>0 then s:=s+',';
    s:=s+LineEnd;
    WriteString(s);
  end;

  procedure WritePalette;
  var
    s: string;
    SrcPos: Integer;

    procedure WriteToSrc(const AddString: string);
    var
      i: Integer;
    begin
      for i:=1 to length(AddString) do begin
        s[SrcPos]:=AddString[i];
        inc(SrcPos);
      end;
    end;

  var
    PaletteLineLen: Integer;
    i: Integer;
    SrcLen: Integer;
  begin
    // calculate needed memory
    PaletteLineLen:=length('"')+CharsPerPixel+length(' c ')+length('",'+LineEnd);
    SrcLen:=0;
    for i:=0 to Palette.Count-1 do begin
      inc(SrcLen,PaletteLineLen);
      inc(SrcLen,length(ColorStrings[i]));
    end;
    // build palette source
    SetLength(s,SrcLen);
    SrcPos:=1;
    for i:=0 to Palette.Count-1 do begin
      WriteToSrc('"');
      WriteToSrc(PixelStrings[i]);
      WriteToSrc(' c ');
      WriteToSrc(ColorStrings[i]);
      WriteToSrc('",');
      WriteToSrc(LineEnd);
    end;

    WriteString(s);
  end;

  procedure WritePixels;
  var
    s: string;
    SrcPos: Integer;

    procedure WriteToSrc(const AddString: string);
    var
      i: Integer;
    begin
      for i:=1 to length(AddString) do begin
        s[SrcPos]:=AddString[i];
        inc(SrcPos);
      end;
    end;

  var
    y: Integer;
    x: Integer;
    i: Integer;
    SrcLenPerLine: Integer;
    SrcLen: Integer;
  begin
    // calculate needed memory
    SrcLenPerLine:=length('"')+CharsPerPixel*Img.Width+length('",')+length(LineEnd);
    SrcLen:=Img.Height*SrcLenPerLine;
    // build palette source
    SetLength(s,SrcLen);
    SrcPos:=1;
    for y:=0 to Img.Height-1 do
    begin
      WriteToSrc('"');
      for x:=0 to Img.Width-1 do
      begin
        i := Palette.IndexOf(GetColor(x,y));
        WriteToSrc(PixelStrings[i]);
      end;
      Progress(psRunning, trunc(100.0 * ((y + 1) / Img.Height)),
           False, Rect(0,0,Img.Width-1,y), 'writing XPM pixels', FContinue);
      if y<Img.Height-1 then
        WriteToSrc('",'+LineEnd)
      else
        WriteToSrc('"}'+LineEnd);
    end;

    WriteString(s);
  end;

var
  i: Integer;
begin
  FContinue := True;
  Progress(psStarting, 0, False, Rect(0,0,0,0), '', FContinue);

  Palette:=nil;
  PixelStrings:=nil;
  ColorStrings:=nil;
  LineEnd:=#10;
  try
    BuildPalette;
    WriteHeader;
    WritePalette;
    WritePixels;
  finally
    if PixelStrings<>nil then begin
      for i:=0 to Palette.Count-1 do begin
        PixelStrings[i]:='';
        ColorStrings[i]:='';
      end;
      ReAllocMem(PixelStrings,0);
      ReAllocMem(ColorStrings,0);
    end;
    Palette.Free;
  end;
  Progress(psEnding, 100, false, Rect(0,0,0,0), '', FContinue);
end;

constructor TLazWriterXPM.Create;
begin
  inherited Create;
  FNibblesPerSample:=2;
  FRightShiftSample:=8;
end;

{ TArrayNode }

constructor TArrayNode.Create;
begin
  //DebugLn('TArrayNode.Create ',Capacity,' Self=',DbgS(Self));
end;

destructor TArrayNode.Destroy;
begin
  DeleteChilds;
  UnbindFromParent;
  inherited Destroy;
end;

procedure TArrayNode.DeleteChilds;
var
  i: Integer;
begin
  if Children<>nil then begin
    for i:=0 to Capacity-1 do
      Children[i].Free;
    FreeMem(Children);
    Children:=nil;
    Capacity:=0;
  end;
end;

procedure TArrayNode.UnbindFromParent;
begin
  if Parent<>nil then begin
    Parent.Children[Value-Parent.StartValue]:=nil;
    Parent:=nil;
  end;
end;

procedure TArrayNode.CreateChildNode(ChildValue: integer);
var
  NewNode: TArrayNode;
  Index: Integer;
begin
  NewNode:=TArrayNode.Create;
  NewNode.Value:=ChildValue;
  NewNode.Parent:=Self;
  Index:=ChildValue-StartValue;
  Children[Index]:=NewNode;
end;

function TArrayNode.GetChildNode(ChildValue: integer; CreateIfNotExists: boolean): TArrayNode;
var Index:Integer;
begin
Index:=ChildValue-StartValue;
if (Index<0)or(Index>=Capacity) then
   begin
   // out of range
   if not CreateIfNotExists then exit(nil);
   Expand(ChildValue);
   Index:=ChildValue-StartValue;
   end;
Result:=Children[Index];
if (Result=nil) and CreateIfNotExists then
   begin
   CreateChildNode(ChildValue);
   Result:=Children[Index];
   end;
end;

procedure TArrayNode.Expand(ValueToInclude: integer);
var
  Index: Integer;
  NewChilds: PArrayNode;
  NewSize: Integer;
  i: Integer;
  NewStartValue: Integer;
  NewCapacity: Integer;
  OldSize: Integer;
begin
  //DebugLn('TArrayNode.Expand A ',ValueToInclude,' Capacity=',Capacity,' StartValue=',StartValue);
  if Children=nil then begin
    NewStartValue:=ValueToInclude;
    NewCapacity:=4;
  end else begin
    Index:=ValueToInclude-StartValue;
    if (Index>=0) and (Index<Capacity) then exit;
    NewStartValue:=StartValue;
    NewCapacity:=Capacity;
    if NewStartValue>ValueToInclude then begin
      inc(NewCapacity,NewStartValue-ValueToInclude);
      NewStartValue:=ValueToInclude;
    end else begin
      Index:=ValueToInclude-NewStartValue;
      if Index>=NewCapacity then
        NewCapacity:=Index+1;
    end;
    // make NewCapacity a power of 2
    for i:=1 to 30 do begin
      if (1 shl i)>=NewCapacity then begin
        NewCapacity:=1 shl i;
        break;
      end;
    end;
  end;
  NewSize:=SizeOf(Pointer)*NewCapacity;
  GetMem(NewChilds,NewSize);
  FillChar(NewChilds^,NewSize,0);
  if Children<>nil then begin
    OldSize:=SizeOf(Pointer)*Capacity;
    System.Move(Children^,NewChilds[StartValue-NewStartValue],OldSize);
    FreeMem(Children);
  end;
  Children:=NewChilds;
  StartValue:=NewStartValue;
  Capacity:=NewCapacity;
end;

function TArrayNode.FindPrevSibling: TArrayNode;
var i: Integer;
begin
if Parent<>nil then
   for i:=Value-Parent.StartValue-1 downto 0 do
       if Parent.Children[i]<>nil then exit(Parent.Children[i]);
Result:=nil;
end;

function TArrayNode.FindNextSibling: TArrayNode;
var i: Integer;
begin
if Parent<>nil then
   begin
   for i:=Value-Parent.StartValue+1 to Parent.Capacity-1 do
       if Parent.Children[i]<>nil then exit(Parent.Children[i]);
   end;
Result:=nil;
end;

function TArrayNode.FindNextUTF8: TArrayNode;
var
  SiblingNode: TArrayNode;
begin
  Result:=FindFirstChild;
  if Result<>nil then exit;
  SiblingNode:=Self;
  while SiblingNode<>nil do begin
    Result:=SiblingNode.FindNextSibling;
    if Result<>nil then exit;
    SiblingNode:=SiblingNode.Parent;
  end;
end;

function TArrayNode.FindPrev: TArrayNode;
begin
Result:=FindPrevSibling;
if Result=nil then Result:=Parent else Result:=Result.FindLastSubChild;
end;

function TArrayNode.FindFirstChild: TArrayNode;
var i: Integer;
begin
if Capacity<>0 then
   For i:=0 to Capacity-1 do
       if Children[i]<>nil then exit(Children[i]);
Result:=nil;
end;

function TArrayNode.FindLastChild: TArrayNode;
var i: Integer;
begin
for i:=Capacity-1 downto 0 do
    if Children[i]<>nil then exit(Children[i]);
Result:=nil;
end;

function TArrayNode.FindLastSubChild: TArrayNode;
var
  ANode: TArrayNode;
begin
  ANode:=Self;
  while ANode<>nil do begin
    Result:=ANode;
    ANode:=ANode.FindLastChild;
  end;
end;

function TArrayNode.FindFirstSibling: TArrayNode;
begin
  if Parent=nil then
    Result:=nil
  else
    Result:=Parent.FindFirstChild;
end;

function TArrayNode.FindLastSibling: TArrayNode;
begin
  if Parent=nil then
    Result:=nil
  else
    Result:=Parent.FindLastChild;
end;

procedure TArrayNode.ConsistencyCheck;
    procedure R(const Msg: string);
    begin end;

var i: Integer;
    ChildNode: TArrayNode;
begin
if Children<>nil then
   begin
   if Capacity>0 then
      begin
      for i:=0 to Capacity-1 do
          begin
          ChildNode:=Children[i];
          if ChildNode<>nil then
             begin
             if ChildNode.Value<>i+StartValue then R('Value wrong');
             if ChildNode.Parent<>Self then R('Parent wrong');
             ChildNode.ConsistencyCheck;
             end;
          end;
      end else R('Capacity too small');
   end else
   begin
   if Capacity<>0 then R('Capacity wrong');
   end;
end;

{ TArrayNodesTree }

function TArrayNodesTree.FindNode(Path: PInteger; Count: integer
  ): TArrayNode;
var
  i: Integer;
begin
  Result:=Root;
  i:=0;
  while (Result<>nil) and (i<Count) do begin
    Result:=Result.GetChildNode(Path[i],false);
    inc(i);
  end;
end;

function TArrayNodesTree.FindData(Path: PInteger; Count: integer): Pointer;
var
  ANode: TArrayNode;
begin
  ANode:=FindNode(Path,Count);
  if ANode<>nil then
    Result:=ANode.Data
  else
    Result:=nil;
end;

function TArrayNodesTree.SetNode(Path: PInteger; Count: integer;
  Data: Pointer): TArrayNode;
var
  i: Integer;
begin
  if Root=nil then
    Root:=TArrayNode.Create;
  Result:=Root;
  for i:=0 to Count-1 do begin
    //DebugLn('TArrayNodesTree.SetNode A ',DbgS(Result));
    Result:=Result.GetChildNode(Path[i],true);
  end;
  Result.Data:=Data;
end;

procedure TArrayNodesTree.Delete(Node: TArrayNode);
begin
  if Node=nil then exit;
  if Node=Root then Root:=nil;
  Node.Free;
end;

procedure TArrayNodesTree.Clear;
begin
  Delete(Root);
end;

constructor TArrayNodesTree.Create;
begin

end;

destructor TArrayNodesTree.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TArrayNodesTree.ConsistencyCheck;
begin
  if Root<>nil then
    Root.ConsistencyCheck;
end;

{ TLazReaderBMP }

function TLazReaderBMP.InternalCheck(Stream: TStream): boolean;
var
  BFH: TBitMapFileHeader;
  offbits: DWORD;
begin
  Stream.Read(BFH, SizeOf(BFH));
  Result := BFH.bfType = LEtoN(BMmagic); // Just check magic number

  { Store the data offset. BFH is poorly aligned (dictated by the .bmp file
    format), which can cause problems for architectures such as SPARC and some
    ARM implementations which have strict alignment requirements. That is why
    the code below uses an intermediate variable, rather than a direct call to
    LEtoN(BFH.bfOffBits) which will try to pass a misaligned parameter.        }
  if Result and (BFH.bfOffBits <> 0)
  then begin
    offbits := BFH.bfOffBits;
    FDataOffset := Stream.Position + LEtoN(offbits) - SizeOf(BFH)
  end
end;

procedure TLazReaderBMP.InternalReadHead;
begin
  inherited InternalReadHead;
  if FDataOffset <> 0
  then TheStream.Position := FDataOffset;
end;

{ TLazWriterBMP }

procedure TLazWriterBMP.Finalize;
begin
end;

procedure TLazWriterBMP.Initialize(AImage: TLazIntfImage);
begin
  // set BPP
  // we can also look at PixelFormat, but it can be inexact
  BitsPerPixel := AImage.DataDescription.Depth;
end;

function TLazWriterBMP.QueryInterface(constref iid: TGuid; out obj): longint; {$IFDEF WINDOWs}stdcall{$ELSE}cdecl{$ENDIF};
begin
  if GetInterface(iid, obj)
  then Result := S_OK
  else Result := E_NOINTERFACE;
end;

function TLazWriterBMP._AddRef: LongInt; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
begin
  Result := -1;
end;

function TLazWriterBMP._Release: LongInt; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
begin
  Result := -1;
end;

{ TLazReaderDIB }

procedure TLazReaderDIB.InitLineBuf;
begin
  FreeLineBuf;

  if Info.BitCount < 8
  then FReadSize := ((Info.BitCount * Info.Width + 31) shr 5) shl 2
  else FReadSize := (((Info.BitCount shr 3) * Info.Width + 3) shr 2) shl 2;

  // allocate 3 bytes more so we can always use a cardinal to read (in case of bitfields)
  GetMem(FLineBuf, FReadSize+3);
end;

procedure TLazReaderDIB.FreeLineBuf;
begin
  FreeMem(FLineBuf);
  FLineBuf := nil;
end;

function TLazReaderDIB.GetUpdateDescription: Boolean;
begin
  Result := FUpdateDescription;
end;

procedure TLazReaderDIB.ReadScanLine(Row: Integer);
  procedure DoRLE4;
  var
    Head: array[0..1] of Byte;
    Value, NibbleCount, ByteCount: Byte;
    WriteNibble: Boolean;       // Set when only lower nibble needs to be written
    BufPtr, DstPtr: PByte;
    Buf: array[0..127] of Byte; // temp buffer to read nibbles
  begin
    DstPtr := @LineBuf[0];
    WriteNibble := False;
    while True do
    begin
      TheStream.Read(Head[0], 2);
      NibbleCount := Head[0];

      if NibbleCount > 0 then
      begin
        if WriteNibble
        then begin
          // low nibble needs to be written
          // swap pixels so that they are in order after this nibble
          Value := (Head[1] shl 4) or (Head[1] shr 4);
          DstPtr^ := (DstPtr^ and $F0) or (Value and $0F);
          Inc(DstPtr);
          // we have written one
          Dec(NibbleCount);
        end
        else begin
          Value := Head[1];
        end;
        ByteCount := (NibbleCount + 1) div 2;
        FillChar(DstPtr^, ByteCount , Value);
        // if we have written an odd number of nibbles we still have to write one
        WriteNibble := NibbleCount and 1 = 1;
        Inc(DstPtr, ByteCount);
        // correct DstPtr if we still need to write a nibble
        if WriteNibble then Dec(DstPtr);
      end
      else begin
        NibbleCount := Head[1];
        case NibbleCount of
          0, 1: break;       // End of scanline or end of bitmap
          2: raise FPImageException.Create('RLE code #2 is not supported');
        else
          ByteCount := (NibbleCount + 1) div 2;

          if WriteNibble
          then begin
            // we cannot read directly into destination, so use temp buf
            TheStream.Read(Buf[0], ByteCount);
            BufPtr := @Buf[0];
            repeat
              DstPtr^ := (DstPtr^ and $F0) or (BufPtr^ shr 4);
              Inc(DstPtr);
              Dec(NibbleCount);
              if NibbleCount = 0
              then begin
                // if we have written both nibbles
                WriteNibble := False;
                Break;
              end;
              DstPtr^ := (BufPtr^ shl 4);
              Inc(BufPtr);
              Dec(NibbleCount);
            until NibbleCount = 0;
          end
          else begin
            TheStream.Read(DstPtr^, ByteCount);
            // if we have written an odd number of nibbles we still have to write one
            WriteNibble := NibbleCount and 1 = 1;
            Inc(DstPtr, ByteCount);
            // correct DstPtr if we still need to write a nibble
            if WriteNibble then Dec(DstPtr);
          end;

          // keep stream at word boundary
          if ByteCount and 1 = 1
          then TheStream.Seek(1, soCurrent);
        end;
      end;

    end
  end;

  procedure DoRLE8;
  var
    Head: array[0..1] of Byte;
    Value, Count: Byte;
    DstPtr: PByte;
  begin
    DstPtr := @LineBuf[0];
    while True do
    begin
      TheStream.Read(Head[0], 2);
      Count := Head[0];
      if Count > 0
      then begin
        Value := Head[1];
        FillChar(DstPtr^, Count, Value);
      end
      else begin
        Count := Head[1];
        case Count of
          0, 1: break;       // End of scanline or end of bitmap
          2: raise FPImageException.Create('RLE code #2 is not supported');
        else
          TheStream.Read(DstPtr^, Count);
          // keep stream at word boundary
          if Count and 1 = 1
          then TheStream.Seek(1, soCurrent);
        end;
      end;

      Inc(DstPtr, Count);
    end
  end;
begin
  // Add here support for compressed lines. The 'readsize' is the same in the end.

  // MWE: Note: when doing so, keep in mind that the bufer is expected to be in Little Endian.
  // for better performance, the conversion is done when writeing the buffer.

  if Info.Encoding = lrdeRLE
  then begin
    case Info.BitCount of
      4: DoRLE4;
      8: DoRLE8;
     //24: DoRLE24;
    end;
  end
  else begin
    TheStream.Read(LineBuf[0], ReadSize);
  end;
end;

function TLazReaderDIB.BitfieldsToFPColor(const AColor: Cardinal): TFPcolor;
var
  V: Word;
begin
  //--- red ---
  V := ((AColor and Info.PixelMasks.R) shl (32 - Info.MaskShift.R - Info.MaskSize.R)) shr 16;
  Result.Red := V;
  repeat
    V := V shr Info.MaskSize.R;
    Result.Red := Result.Red or V;
  until V = 0;

  //--- green ---
  V := ((AColor and Info.PixelMasks.G) shl (32 - Info.MaskShift.G - Info.MaskSize.G)) shr 16;
  Result.Green := V;
  repeat
    V := V shr Info.MaskSize.G;
    Result.Green := Result.Green or V;
  until V = 0;

  //--- blue ---
  V := ((AColor and Info.PixelMasks.B) shl (32 - Info.MaskShift.B - Info.MaskSize.B)) shr 16;
  Result.Blue := V;
  repeat
    V := V shr Info.MaskSize.B;
    Result.Blue := Result.Blue or V;
  until V = 0;

  //--- alpha ---
  if Info.MaskSize.A = 0
  then begin
    Result.Alpha := AlphaOpaque;
  end
  else begin
    V := ((AColor and Info.PixelMasks.A) shl (32 - Info.MaskShift.A - Info.MaskSize.A)) shr 16;
    Result.Alpha := V;
    repeat
      V := V shr Info.MaskSize.A;
      Result.Alpha := Result.Alpha or V;
    until V = 0;
  end;
end;

function TLazReaderDIB.RGBToFPColor(const AColor: TColorRGB): TFPcolor;
var
  RBytes: TFPColorBytes absolute Result;
begin
  RBytes.Bh := AColor.B;
  RBytes.Bl := AColor.B;
  RBytes.Gh := AColor.G;
  RBytes.Gl := AColor.G;
  RBytes.Rh := AColor.R;
  RBytes.Rl := AColor.R;
  Result.Alpha := AlphaOpaque;
end;

function TLazReaderDIB.RGBToFPColor(const AColor: TColorRGBA): TFPcolor;
var
  RBytes: TFPColorBytes absolute Result;
begin
  RBytes.Bh := AColor.B;
  RBytes.Bl := AColor.B;
  RBytes.Gh := AColor.G;
  RBytes.Gl := AColor.G;
  RBytes.Rh := AColor.R;
  RBytes.Rl := AColor.R;
  if Info.MaskSize.A = 0
  then Result.Alpha := AlphaOpaque
  else begin
    RBytes.Ah := AColor.A;
    RBytes.Al := AColor.A;
  end;
end;

function TLazReaderDIB.RGBToFPColor(const AColor: Word): TFPcolor;
var
  V1, V2: Cardinal;
begin
  // 5 bit for red  -> 16 bit for TFPColor
  V1 := (AColor shl 1) and $F800;     // 15..11
  V2 := V1;
  V1 := V1 shr 5;                  // 10..6
  V2 := V2 or V1;
  V1 := V1 shr 5;                  // 5..1
  V2 := V2 or V1;
  V1 := V1 shr 5;                  // 0
  Result.Red := Word(V2 or V1);
  // 5 bit for red  -> 16 bit for TFPColor
  V1 := (AColor shl 6) and $F800;     // 15..11
  V2 := V1;
  V1 := V1 shr 5;                  // 10..6
  V2 := V2 or V1;
  V1 := V1 shr 5;                  // 5..1
  V2 := V2 or V1;
  V1 := V1 shr 5;                  // 0
  Result.Green := Word(V2 or V1);
  // 5 bit for blue -> 16 bit for TFPColor
  V1 := (AColor shl 11) and $F800;    // 15..11
  V2 := V1;
  V1 := V1 shr 5;
  V2 := V2 or V1;                  // 10..6
  V1 := V1 shr 5;
  V2 := V2 or V1;                  // 5..1
  V1 := V1 shr 5;
  Result.Blue := Word(V2 or V1);   // 0
  // opaque, no mask
  Result.Alpha:=alphaOpaque;
end;

procedure TLazReaderDIB.SetUpdateDescription(AValue: Boolean);
begin
  FUpdateDescription := AValue;
end;

procedure TLazReaderDIB.WriteScanLine(Row: Cardinal);
// using cardinals generates compacter code
var
  Column: Cardinal;
  Color: TFPColor;
  Index: Byte;
begin
  if FMaskMode = lrmmNone
  then begin
    case Info.BitCount of
     1 :
       for Column := 0 to TheImage.Width - 1 do
         TheImage.colors[Column,Row] := FPalette[Ord(LineBuf[Column div 8] and ($80 shr (Column and 7)) <> 0)];
     4 :
       for Column := 0 to TheImage.Width - 1 do
         TheImage.colors[Column,Row] := FPalette[(LineBuf[Column div 2] shr (((not Column) and 1)*4)) and $0f];
     8 :
       for Column := 0 to TheImage.Width - 1 do
         TheImage.colors[Column,Row] := FPalette[LineBuf[Column]];
    else
      if Info.Encoding = lrdeBitfield
      then begin
        // always cast to cardinal without conversion
        // this way the value will have the same order as the bitfields
        case Info.BitCount of
          16:
            for Column := 0 to TheImage.Width - 1 do
              TheImage.colors[Column,Row] := BitfieldsToFPColor(PCardinal(@PWord(LineBuf)[Column])^);
          24:
            for Column := 0 to TheImage.Width - 1 do
              TheImage.colors[Column,Row] := BitfieldsToFPColor(PCardinal(@PColorRGB(LineBuf)[Column])^);
          32:
            for Column := 0 to TheImage.Width - 1 do
            begin
              Color := BitfieldsToFPColor(PCardinal(@PColorRGBA(LineBuf)[Column])^);
              TheImage.colors[Column,Row] := Color;
              FIgnoreAlpha := FIgnoreAlpha and (Color.alpha = alphaTransparent);
            end;
        end;
      end
      else begin
        case Info.BitCount of
          16:
            for Column := 0 to TheImage.Width - 1 do
              TheImage.colors[Column,Row] := RGBToFPColor({$ifdef FPC_BIG_ENDIAN}LeToN{$endif}(PWord(LineBuf)[Column]));
          24:
            for Column := 0 to TheImage.Width - 1 do
              TheImage.colors[Column,Row] := RGBToFPColor(PColorRGB(LineBuf)[Column]);
          32:
            for Column := 0 to TheImage.Width - 1 do
            begin
              Color := RGBToFPColor(PColorRGBA(LineBuf)[Column]);
              TheImage.colors[Column,Row] := Color;
              FIgnoreAlpha := FIgnoreAlpha and (Color.alpha = alphaTransparent);
            end;
        end;
      end;
    end;
  end
  else begin
    case Info.BitCount of
     1 :
       for Column := 0 to TheImage.Width - 1 do
       begin
         Index := Ord(LineBuf[Column div 8] and ($80 shr (Column and 7)) <> 0);
         FImage.colors[Column,Row] := FPalette[Index];
         FImage.Masked[Column,Row] := Index = FMaskIndex;
       end;
     4 :
       for Column := 0 to TheImage.Width - 1 do
       begin
         Index := (LineBuf[Column div 2] shr (((not Column) and 1)*4)) and $0f;
         FImage.colors[Column,Row] := FPalette[Index];
         FImage.Masked[Column,Row] := Index = FMaskIndex;
       end;
     8 :
       for Column := 0 to TheImage.Width - 1 do
       begin
         Index := LineBuf[Column];
         FImage.colors[Column,Row] := FPalette[Index];
         FImage.Masked[Column,Row] := Index = FMaskIndex;
       end;
    else
      if Info.Encoding = lrdeBitfield
      then begin
        // always cast to cardinal without conversion
        // this way the value will have the same order as the bitfields
        case Info.BitCount of
         16:
           for Column := 0 to TheImage.Width - 1 do
           begin
             Color := BitfieldsToFPColor(PCardinal(@PWord(LineBuf)[Column])^);
             FImage.colors[Column,Row] := Color;
             FImage.Masked[Column,Row] := Color = FMaskColor;
           end;
         24:
           for Column := 0 to TheImage.Width - 1 do
           begin
             Color := BitfieldsToFPColor(PCardinal(@PColorRGB(LineBuf)[Column])^);
             FImage.colors[Column,Row] := Color;
             FImage.Masked[Column,Row] := Color = FMaskColor;
           end;
         32:
           for Column := 0 to TheImage.Width - 1 do
           begin
             Color := BitfieldsToFPColor(PCardinal(@PColorRGBA(LineBuf)[Column])^);
             FImage.colors[Column,Row] := Color;
             FImage.Masked[Column,Row] := Color = FMaskColor;
             FIgnoreAlpha := FIgnoreAlpha and (Color.alpha = alphaTransparent);
           end;
        end;
      end
      else begin
        case Info.BitCount of
         16:
           for Column := 0 to TheImage.Width - 1 do
           begin
             Color := RGBToFPColor({$ifdef FPC_BIG_ENDIAN}LeToN{$endif}(PWord(LineBuf)[Column]));
             FImage.colors[Column,Row] := Color;
             FImage.Masked[Column,Row] := Color = FMaskColor;
           end;
         24:
           for Column := 0 to TheImage.Width - 1 do
           begin
             Color := RGBToFPColor(PColorRGB(LineBuf)[Column]);
             FImage.colors[Column,Row] := Color;
             FImage.Masked[Column,Row] := Color = FMaskColor;
           end;
         32:
           for Column := 0 to TheImage.Width - 1 do
           begin
             Color := RGBToFPColor(PColorRGBA(LineBuf)[Column]);
             FImage.colors[Column,Row] := Color;
             FImage.Masked[Column,Row] := Color = FMaskColor;
             FIgnoreAlpha := FIgnoreAlpha and (Color.alpha = alphaTransparent);
           end;
        end;
      end;
    end;
  end;
end;

function TLazReaderDIB._AddRef: LongInt; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
begin
  Result := -1;
end;

function TLazReaderDIB._Release: LongInt; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
begin
  Result := -1;
end;

procedure TLazReaderDIB.InternalRead(Stream: TStream; Img: TFPCustomImage);
var
  Desc: TRawImageDescription;
  Depth: Byte;
begin
  FContinue := True;
  Progress(psStarting, 0, False, Rect(0,0,0,0), '', FContinue);
  FImage := TheImage as TLazIntfImage;
  FIgnoreAlpha := True;
  Depth := 0;
  InternalReadHead;
  
  if FUpdateDescription
  then begin
    if (Info.BitCount = 32) and (Info.MaskSize.A = 0)
    then Depth := 24
    else Depth := Info.BitCount;
    DefaultReaderDescription(Info.Width, Info.Height, Depth, Desc);
    FImage.DataDescription := Desc;
  end;

  InternalReadBody;

  // if there is no alpha in real (all alpha values = 0) then update the description
  if FUpdateDescription and FIgnoreAlpha and (Depth = 32) then
  begin
    Desc.AlphaPrec:=0;
    FImage.SetDataDescriptionKeepData(Desc);
  end;

  Progress(psEnding, 100, false, Rect(0,0,0,0), '', FContinue);
end;

procedure TLazReaderDIB.InternalReadHead;
const
  SUnknownCompression = 'Bitmap with unknown compression (%d)';
  SUnsupportedCompression = 'Bitmap with unsupported compression (%s)';
  SWrongCombination = 'Bitmap with wrong combination of bit count (%d) and compression (%s)';
  SUnsupportedPixelMask = 'Bitmap with non-standard pixel masks not supported';

  SEncoding: array[TLazReaderDIBEncoding] of string = (
    'RGB',
    'RLE',
    'Bitfield',
    'Jpeg',
    'Png',
    'Huffman'
  );

  function ValidCompression: Boolean;
  begin
    case Info.BitCount of
      1:   Result := FDibInfo.Encoding in [lrdeRGB, lrdeHuffman];
      4,8: Result := FDibInfo.Encoding in [lrdeRGB, lrdeRLE];
      16:  Result := FDibInfo.Encoding in [lrdeRGB, lrdeBitfield];
      24:  Result := FDibInfo.Encoding in [lrdeRGB, lrdeBitfield, lrdeRLE];
      32:  Result := FDibInfo.Encoding in [lrdeRGB, lrdeBitfield];
    else
      raise FPImageException.CreateFmt('Wrong bitmap bit count: %d', [Info.BitCount]);
    end;
  end;

  procedure GetMaskShiftSize(AMask: LongWord; var AShift, ASize: Byte);
  begin
    AShift := 0;
    repeat
      if (AMask and 1) <> 0 then Break;
      AMask := AMask shr 1;
      Inc(AShift);
    until AShift >= 32;

    ASize := 0;
    repeat
      if (AMask and 1) = 0 then Break;
      AMask := AMask shr 1;
      Inc(ASize);
    until AShift + ASize >= 32;
  end;

  procedure ReadPalette(APaletteIsOS2: Boolean);
  var
    ColorSize: Byte;
    C: TColorRGBA;
    n, len, maxlen: Integer;
  begin
    SetLength(FPalette, 0);
    if Info.PaletteCount = 0 then Exit;

    if APaletteIsOS2
    then ColorSize := 3
    else ColorSize := 4;

    if FDibInfo.BitCount > 8
    then begin
      // Bitmaps can have a color table stored in the palette entries,
      // skip them, since we don't use it
      TheStream.Seek(Info.PaletteCount * ColorSize, soCurrent);
      Exit;
    end;

    maxlen := 1 shl Info.BitCount;
    if Info.PaletteCount <= maxlen
    then len := maxlen
    else len := Info.PaletteCount; // more colors ???

    SetLength(FPalette, len);

    for n := 0 to Info.PaletteCount - 1 do
    begin
      TheStream.Read(C, ColorSize);
      C.A := $FF; //palette has no alpha
      FPalette[n] := RGBToFPColor(C);
    end;

    // fill remaining with black color, so we don't have to check for out of index values
    for n := Info.PaletteCount to maxlen - 1 do
      FPalette[n] := colBlack;
  end;

var
  BIH: TBitmapInfoHeader;
  BCH: TBitmapCoreHeader;
  H: Integer;
  StreamStart: Int64;
begin
  StreamStart := theStream.Position;
  TheStream.Read(BIH.biSize,SizeOf(BIH.biSize));
  {$IFDEF FPC_BIG_ENDIAN}
  BIH.biSize := LEtoN(BIH.biSize);
  {$ENDIF}

  if BIH.biSize = 12
  then begin
    // OS2 V1 header
    TheStream.Read(BCH.bcWidth, BIH.biSize - SizeOf(BIH.biSize));

    FDibInfo.Width := LEtoN(BCH.bcWidth);
    FDibInfo.Height := LEtoN(BCH.bcHeight);
    FDibInfo.BitCount := LEtoN(BCH.bcBitCount);
    FDibInfo.Encoding := lrdeRGB;
    FDibInfo.UpsideDown := True;

    if FDibInfo.BitCount > 8
    then FDibInfo.PaletteCount := 0
    else FDibInfo.PaletteCount := 1 shl FDibInfo.BitCount;
  end
  else begin
    // Windows Vx header or OSX V2, all start with BitmapInfoHeader
    TheStream.Read(BIH.biWidth, SizeOf(BIH) - SizeOf(BIH.biSize));

    FDibInfo.Width := LEtoN(BIH.biWidth);
    H := LEtoN(BIH.biHeight);
    // by default bitmaps are stored upside down
    if H >= 0
    then begin
      FDibInfo.UpsideDown := True;
      FDibInfo.Height := H;
    end
    else begin
      FDibInfo.UpsideDown := False;
      FDibInfo.Height := -H;
    end;

    FDibInfo.BitCount := LEtoN(BIH.biBitCount);
    case LEtoN(BIH.biCompression) of
      BI_RGB        : FDibInfo.Encoding := lrdeRGB;
      4, {BCA_RLE24}
      BI_RLE8,
      BI_RLE4       : FDibInfo.Encoding := lrdeRLE;
      {BCA_HUFFMAN1D, }
      BI_BITFIELDS  : begin
        // OS2 can use huffman encoding for mono bitmaps
        // bitfields only work for 16 and 32
        if FDibInfo.BitCount = 1
        then FDibInfo.Encoding := lrdeHuffman
        else FDibInfo.Encoding := lrdeBitfield;
      end;
    else
      raise FPImageException.CreateFmt(SUnknownCompression, [LEtoN(BIH.biCompression)]);
    end;

    if not (FDibInfo.Encoding in [lrdeRGB, lrdeRLE, lrdeBitfield])
    then raise FPImageException.CreateFmt(SUnsupportedCompression, [SEncoding[FDibInfo.Encoding]]);

    FDibInfo.PaletteCount := LEtoN(BIH.biClrUsed);
    if  (FDibInfo.PaletteCount = 0)
    and (FDibInfo.BitCount <= 8)
    then FDibInfo.PaletteCount := 1 shl FDibInfo.BitCount;
  end;

  if not ValidCompression
  then raise FPImageException.CreateFmt(SWrongCombination, [FDibInfo.BitCount, SEncoding[FDibInfo.Encoding]]);

  if BIH.biSize >= 108
  then begin
    // at least a V4 header -> has alpha mask, which is always valid (read other masks too)
    TheStream.Read(FDibInfo.PixelMasks, 4 * SizeOf(FDibInfo.PixelMasks.R));
    GetMaskShiftSize(FDibInfo.PixelMasks.A, FDibInfo.MaskShift.A, FDibInfo.MaskSize.A);
  end
  else begin
    // officially no alpha support, but that breaks older LCL compatebility
    // so add it
    if Info.BitCount = 32
    then begin
      {$ifdef ENDIAN_BIG}
      FDibInfo.PixelMasks.A := $000000FF;
      {$else}
      FDibInfo.PixelMasks.A := $FF000000;
      {$endif}
      GetMaskShiftSize(FDibInfo.PixelMasks.A, FDibInfo.MaskShift.A, FDibInfo.MaskSize.A);
    end
    else begin
      FDibInfo.PixelMasks.A := 0;
      FDibInfo.MaskShift.A := 0;
      FDibInfo.MaskSize.A := 0;
    end;
  end;

  if Info.Encoding = lrdeBitfield
  then begin
    if BIH.biSize < 108
    then begin
      // not read yet
      TheStream.Read(FDibInfo.PixelMasks, 3 * SizeOf(FDibInfo.PixelMasks.R));
      // check if added mask is valid
      if (Info.PixelMasks.R or Info.PixelMasks.G or Info.PixelMasks.B) and Info.PixelMasks.A <> 0
      then begin
        // Alpha mask overlaps others
        FDibInfo.PixelMasks.A := 0;
        FDibInfo.MaskShift.A := 0;
        FDibInfo.MaskSize.A := 0;
      end;
    end;
    GetMaskShiftSize(FDibInfo.PixelMasks.R, FDibInfo.MaskShift.R, FDibInfo.MaskSize.R);
    GetMaskShiftSize(FDibInfo.PixelMasks.G, FDibInfo.MaskShift.G, FDibInfo.MaskSize.G);
    GetMaskShiftSize(FDibInfo.PixelMasks.B, FDibInfo.MaskShift.B, FDibInfo.MaskSize.B);

    TheStream.Seek(StreamStart + BIH.biSize, soBeginning);
  end
  else begin
    TheStream.Seek(StreamStart + BIH.biSize, soBeginning);
    ReadPalette(BIH.biSize = 12);
  end;

  if Info.MaskSize.A <> 0 {Info.BitCount = 32}
  then CheckAlphaDescription(TheImage);
end;

function TLazReaderDIB.QueryInterface(constref iid: TGuid; out obj): longint; {$IFDEF WINDOWs}stdcall{$ELSE}cdecl{$ENDIF};
begin
  if GetInterface(iid, obj)
  then Result := S_OK
  else Result := E_NOINTERFACE;
end;

procedure TLazReaderDIB.InternalReadBody;


  procedure SaveTransparentColor;
  begin
    if FMaskMode <> lrmmAuto then Exit;

    // define transparent color: 1-8 use palette, 15-24 use fixed color
    case Info.BitCount of
      1: FMaskIndex := (LineBuf[0] shr 7) and 1;
      4: FMaskIndex := (LineBuf[0] shr 4) and $f;
      8: FMaskIndex := LineBuf[0];
    else
      FMaskIndex := -1;
      if Info.Encoding = lrdeBitfield
      then begin
        FMaskColor := BitfieldsToFPColor(PCardinal(LineBuf)[0]);
        Exit;
      end;

      case Info.BitCount of
        16: FMaskColor := RGBToFPColor({$ifdef FPC_BIG_ENDIAN}LeToN{$endif}(PWord(LineBuf)[0]));
        24: FMaskColor := RGBToFPColor(PColorRGB(LineBuf)[0]);
        32: FMaskColor := RGBToFPColor(PColorRGBA(LineBuf)[0]);
      end;

      Exit;
    end;
    if FMaskIndex <> -1
    then FMaskColor := FPalette[FMaskIndex];
  end;
  
  procedure UpdateProgress(Row: Integer); inline;
  begin
    Progress(psRunning, trunc(100.0 * ((TheImage.Height - Row) / TheImage.Height)),
      False, Rect(0, 0, TheImage.Width - 1, TheImage.Height - 1 - Row), 'reading BMP pixels', FContinue);
  end;

var
  Row : Cardinal;
begin
  TheImage.SetSize(Info.Width, Info.Height);

  if Info.Height = 0 then Exit;
  if Info.Width = 0 then Exit;

  InitLineBuf;
  try
    if not FContinue then Exit;

    Row := Info.Height - 1;
    ReadScanLine(Row);
    SaveTransparentColor;
    
    if Info.UpsideDown
    then WriteScanLine(Row)
    else WriteScanLine(Info.Height - 1 - Row);

    UpdateProgress(Row);

    while Row > 0 do
    begin
      if not FContinue then Exit;
      Dec(Row);
      ReadScanLine(Row); // Scanline in LineBuf with Size ReadSize.

      if Info.UpsideDown
      then WriteScanLine(Row)
      else WriteScanLine(Info.Height - 1 - Row);

      UpdateProgress(Row);
    end;
  finally
    FreeLineBuf;
  end;
end;

function TLazReaderDIB.InternalCheck(Stream: TStream): boolean;
begin
  Result := True;
end;

constructor TLazReaderDIB.Create;
begin
  inherited Create;
  FMaskColor := colTransparent;
  FContinue := True;
end;

destructor TLazReaderDIB.Destroy;
begin
  FreeLineBuf;
  inherited Destroy;
end;

{ TLazIntfImageMask }

procedure TLazIntfImageMask.SetInternalColor(x, y: integer; const Value: TFPColor);
begin
  FImage.Masked[x, y] := Value.red < $8000;
end;

function TLazIntfImageMask.GetInternalColor(x, y: integer): TFPColor;
begin
  if FImage.Masked[x, y]
  then Result := FPImage.colWhite
  else Result := FPImage.colBlack;
end;

procedure TLazIntfImageMask.SetInternalPixel(x, y: integer; Value: integer);
begin
  FImage.Masked[x, y] := Value <> 0;
end;

function TLazIntfImageMask.GetInternalPixel(x, y: integer): integer;
begin
  Result := Ord(FImage.Masked[x, y]);
end;

constructor TLazIntfImageMask.CreateWithImage(TheImage: TLazIntfImage);
begin
  FImage:=TheImage;
  inherited Create(FImage.Width,FImage.Height);
end;

{ TLazReaderIconDIB }

procedure TLazReaderIconDIB.InternalRead(Stream: TStream; Img: TFPCustomImage);
var
  Desc: TRawImageDescription;
  Row, Column: Integer;
  NewColor: TFPColor;
  BufPtr: PByte;
  MaskBit: Byte;
begin
  FImage := TheImage as TLazIntfImage;
  InternalReadHead;
  
  // Height field is doubled, to (sort of) accomodate mask
  // MWE: it shoud be safer to verify the division agains the dirinfo.height
  //      anyway I haven't encountered an icon in the wild which doesn't have a mask
  FDIBinfo.Height := FDIBinfo.Height div 2;
  if FUpdateDescription
  then begin
    DefaultReaderDescription(Info.Width, Info.Height, Info.BitCount, Desc);
    FImage.DataDescription := Desc;
  end
  else Desc := FImage.DataDescription;
  InternalReadBody; { Now read standard bitmap }

  // Mask immediately follows unless bitmap was 32 bit - monchrome bitmap with no header
  // MWE: Correction, it seems that even 32bit icons can have a mask following
  // if BFI.biBitCount >= 32 then Exit;

  FDIBinfo.Encoding := lrdeRGB;
  FDIBinfo.BitCount := 1;
  InitLineBuf;
  try
    for Row := Desc.Height - 1 downto 0 do
    begin
      ReadScanLine(Row); // Scanline in LineBuf with Size ReadSize.
      BufPtr := LineBuf;
      MaskBit := $80;
      for Column:=0 to Desc.Width - 1 do
      begin
        if BufPtr^ and MaskBit = 0
        then begin
          // opaque
          FImage.Masked[Column, Row] := False;
        end
        else begin
          // transparent
          FImage.Masked[Column, Row] := True;
          // add alpha when source wasn't 32bit
          if  (Desc.AlphaPrec <> 0)
          and ((Desc.Depth < 32) or (Info.MaskSize.A = 0))
          then begin
            NewColor := FImage.Colors[Column, Row];
            NewColor.Alpha := alphaTransparent;
            FImage.Colors[Column, Row] := NewColor;
          end;
        end;
        if MaskBit = 1
        then begin
          MaskBit := $80;
          Inc(BufPtr);
        end
        else begin
          MaskBit := MaskBit shr 1;
        end;
      end;
    end;
  finally
    FreeLineBuf;
  end;
end;


{ TLazReaderPNG }

procedure TLazReaderPNG.DoDecompress;
var
  Desc: TRawImageDescription;
  IsAlpha, IsGray: Boolean;
begin
  if FUpdateDescription and (theImage is TLazIntfImage)
  then begin
    // init some default

    IsGray := Header.ColorType and 3 = 0;

    // Paul: if we have a mask in the description then we need to set it manually
    // by Masked[x, y] := Color.Alpha = AlphaTransparent, but to do that we must
    // read format ourself. fpReaders set alpha instead - they do not have Masked[].
    // So if we want true description with mask we must teach our SetInternalColor
    // method to handle Alpha if mask needed (or do it any other way). In other words
    // this is now unimplemented and we'll get randomly masked image.
    // As a temporary solution I'm enable alpha description if transparent color
    // is present. This is indicated by UseTransparent property.
    // When we will handle Mask in SetInternalColor please remove UseTransparent
    // from the IsAlpha assignment.
    
    IsAlpha := (Header.ColorType and 4 <> 0) or FAlphaPalette or UseTransparent;
    
    if not IsAlpha and UseTransparent
    then Desc.Init_BPP32_B8G8R8A8_M1_BIO_TTB(Header.Width, Header.height)
    else Desc.Init_BPP32_B8G8R8A8_BIO_TTB(Header.Width, Header.height);

    if IsGray
    then Desc.Format := ricfGray;
    if not IsAlpha
    then Desc.AlphaPrec := 0;

    // check palette
    if (Header.ColorType and 1 <> 0)
    then begin
      // todo: palette
    end
    else begin
      // no palette, adjust description
      if IsGray
      then begin
        Desc.RedPrec := Header.BitDepth;
        Desc.RedShift := 0;
        if IsAlpha
        then begin
          Desc.BitsPerPixel := 2 * Header.BitDepth;
          Desc.AlphaPrec := Header.BitDepth;
          Desc.AlphaShift := Header.BitDepth;
        end
        else begin
          Desc.BitsPerPixel := Header.BitDepth;
        end;
        Desc.Depth := Desc.BitsPerPixel;
      end
      else begin
        if IsAlpha
        then Desc.Depth := 4 * Header.BitDepth
        else Desc.Depth := 3 * Header.BitDepth
      end;

      case Header.BitDepth of
        1,2,4: begin
          // only gray
        end;
        8: begin
          // no change
        end;
        16: begin
          if not IsGray then begin
            Desc.BitsPerPixel := Desc.Depth;
            Desc.RedPrec := 16;
            Desc.RedShift := Desc.RedShift * 2;
            Desc.GreenPrec := 16;
            Desc.GreenShift := Desc.GreenShift * 2;
            Desc.BluePrec := 16;
            Desc.BlueShift := Desc.BlueShift * 2;
            Desc.AlphaPrec := Desc.AlphaPrec * 2; // might be zero
            Desc.AlphaShift := Desc.AlphaShift * 2;
          end;
        end;
      end;
    end;

    TLazIntfImage(theImage).DataDescription := Desc;
  end;

  inherited DoDecompress;
end;

function TLazReaderPNG.GetUpdateDescription: Boolean;
begin
  Result := FUpdateDescription;
end;

procedure TLazReaderPNG.HandleAlpha;
begin
  inherited HandleAlpha;
  FAlphaPalette := Header.ColorType = 3;
end;

procedure TLazReaderPNG.InternalRead(Str: TStream; Img: TFPCustomImage);
begin
  FAlphaPalette := False;
  inherited InternalRead(Str, Img);
end;

function TLazReaderPNG.QueryInterface(constref iid: TGuid; out obj): longint; {$IFDEF WINDOWs}stdcall{$ELSE}cdecl{$ENDIF};
begin
  if GetInterface(iid, obj)
  then Result := S_OK
  else Result := E_NOINTERFACE;
end;

procedure TLazReaderPNG.SetUpdateDescription(AValue: Boolean);
begin
  FUpdateDescription := AValue;
end;

function TLazReaderPNG._AddRef: LongInt; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
begin
  Result := -1;
end;

function TLazReaderPNG._Release: LongInt; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
begin
  Result := -1;
end;

{ TLazWriterPNG }

procedure TLazWriterPNG.Finalize;
begin
end;

procedure TLazWriterPNG.Initialize(AImage: TLazIntfImage);
begin
  UseAlpha := AImage.DataDescription.AlphaPrec <> 0;
  GrayScale := AImage.DataDescription.Format = ricfGray;
  Indexed := AImage.DataDescription.Depth <= 8;
  WordSized := (AImage.DataDescription.RedPrec > 8)
            or (AImage.DataDescription.GreenPrec > 8)
            or (AImage.DataDescription.BluePrec > 8);
end;

function TLazWriterPNG.QueryInterface(constref iid: TGuid; out obj): longint; {$IFDEF WINDOWs}stdcall{$ELSE}cdecl{$ENDIF};
begin
  if GetInterface(iid, obj)
  then Result := S_OK
  else Result := E_NOINTERFACE;
end;

function TLazWriterPNG._AddRef: LongInt; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
begin
  Result := -1;
end;

function TLazWriterPNG._Release: LongInt; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
begin
  Result := -1;
end;

{ TLazReaderTiff }

{$IFDEF OldTiffCreateImageHook}
procedure TLazReaderTiff.CreateImageHook(Sender: TFPReaderTiff; var NewImage: TFPCustomImage);
begin
  if Assigned(FOrgEvent) then FOrgEvent(Sender, NewImage);
  FirstImg.Img:=NewImage;
  DoCreateImage(FirstImg);
end;
{$ENDIF}

procedure TLazReaderTiff.DoCreateImage(
  ImgFileDir: {$IFDEF OldTiffCreateImageHook}TTiffIDF{$ELSE}TTiffIFD{$ENDIF});
var
  Desc: TRawImageDescription;
  IsAlpha, IsGray: Boolean;
begin
  {$IFNDEF OldTiffCreateImageHook}
  inherited;
  {$ENDIF}

  if not FUpdateDescription then Exit;
  if not (theImage is TLazIntfImage) then Exit;

  // init some default

  IsGray := ImgFileDir.PhotoMetricInterpretation in [0, 1];
  IsAlpha := ImgFileDir.AlphaBits <> 0;

  if IsAlpha
  then Desc.Init_BPP32_B8G8R8A8_BIO_TTB(ImgFileDir.ImageWidth, ImgFileDir.ImageHeight)
  else Desc.Init_BPP24_B8G8R8_BIO_TTB(ImgFileDir.ImageWidth, ImgFileDir.ImageHeight);

  if IsGray
  then Desc.Format := ricfGray;

  // check mask
  if ImgFileDir.PhotoMetricInterpretation = 4
  then begin
    // todo: mask
  end
  else
  // check palette
  if ImgFileDir.PhotoMetricInterpretation = 3
  then begin
    // todo: palette
  end
  else begin
    // no palette, adjust description
    if IsGray
    then begin
      Desc.RedPrec := ImgFileDir.GrayBits;
      Desc.RedShift := 0;
      if IsAlpha
      then begin
        Desc.Depth := ImgFileDir.GrayBits + ImgFileDir.AlphaBits;
        Desc.AlphaPrec := ImgFileDir.AlphaBits;
        Desc.AlphaShift := ImgFileDir.GrayBits;
      end
      else begin
        Desc.Depth := ImgFileDir.GrayBits;
        Desc.BitsPerPixel := ImgFileDir.GrayBits;
      end;
    end
    else begin
      Desc.Depth := ImgFileDir.RedBits + ImgFileDir.GreenBits + ImgFileDir.BlueBits + ImgFileDir.AlphaBits;
      if Desc.Depth > 32
      then begin
        // switch to 64bit description
        Desc.BitsPerPixel := Desc.BitsPerPixel * 2;
        Desc.RedPrec := 16;
        Desc.RedShift := Desc.RedShift * 2;
        Desc.GreenPrec := 16;
        Desc.GreenShift := Desc.GreenShift * 2;
        Desc.BluePrec := 16;
        Desc.BlueShift := Desc.BlueShift * 2;
        Desc.AlphaPrec := Desc.AlphaPrec * 2; // might be zero
        Desc.AlphaShift := Desc.AlphaShift * 2;
      end;
    end;
  end;

  TLazIntfImage(theImage).DataDescription := Desc;
end;

function TLazReaderTiff.GetUpdateDescription: Boolean;
begin
  Result := FUpdateDescription;
end;

procedure TLazReaderTiff.InternalRead(Str: TStream; Img: TFPCustomImage);
begin
  {$IFDEF OldTiffCreateImageHook}
  FOrgEvent := OnCreateImage;
  OnCreateImage := @CreateImageHook;
  inherited InternalRead(Str, Img);
  OnCreateImage := FOrgEvent;
  FOrgEvent := nil;
  {$ELSE}
  inherited InternalRead(Str, Img);
  {$ENDIF}
end;

function TLazReaderTiff.QueryInterface(constref iid: TGuid; out obj): longint; {$IFDEF WINDOWs}stdcall{$ELSE}cdecl{$ENDIF};
begin
  if GetInterface(iid, obj)
  then Result := S_OK
  else Result := E_NOINTERFACE;
end;

procedure TLazReaderTiff.SetUpdateDescription(AValue: Boolean);
begin
  FUpdateDescription := AValue;
end;

function TLazReaderTiff._AddRef: LongInt; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
begin
  Result := -1;
end;

function TLazReaderTiff._Release: LongInt; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
begin
  Result := -1;
end;

{ TLazWriterTiff }

procedure TLazWriterTiff.Finalize;
begin
end;

procedure TLazWriterTiff.Initialize(AImage: TLazIntfImage);
begin
{ct9999} AImage.Extra[LazTiffSoftware] := 'TLazWriterTiff - Typhon LCL';
end;

procedure TLazWriterTiff.InternalWrite(Stream: TStream; Img: TFPCustomImage);
var
  S: String;
begin
  AddImage(Img);

  //add additional elements

  S := Img.Extra[LazTiffHostComputer];
  if S <> '' then AddEntryString(316, S);
  S := Img.Extra[LazTiffMake];
  if S <> '' then AddEntryString(271, S);
  S := Img.Extra[LazTiffModel];
  if S <> '' then AddEntryString(272, S);
  S := Img.Extra[LazTiffSoftware];
  if S <> '' then AddEntryString(305, S);

  SaveToStream(Stream);
end;

function TLazWriterTiff.QueryInterface(constref iid: TGuid; out obj): longint; {$IFDEF WINDOWs}stdcall{$ELSE}cdecl{$ENDIF};
begin
  if GetInterface(iid, obj)
  then Result := S_OK
  else Result := E_NOINTERFACE;
end;

function TLazWriterTiff._AddRef: LongInt; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
begin
  Result := -1;
end;

function TLazWriterTiff._Release: LongInt; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
begin
  Result := -1;
end;

{ TLazReaderIcnsPart }

function TLazReaderIcnsPart.InternalCheck(Str: TStream): boolean;
begin
  // todo: write check code
  Result := True;
end;

procedure TLazReaderIcnsPart.InternalRead(Stream: TStream; Img: TFPCustomImage);
var
  Desc: TRawImageDescription;
  Element: TIconFamilyElement;
  IsMask: Boolean;
begin
  FImage := TheImage as TLazIntfImage;

  Stream.Read(Element, SizeOf(Element));
  Element.elementSize := BEtoN(Element.elementSize);
  FIconType := GetIcnsIconType(Element.elementType);
  FIconInfo := icnsIconTypeInfo[FIconType];
  IsMask := FIconType in icnsMaskTypes;

  if UpdateDescription
  then begin
    if IsMask then
    begin
      if FIconInfo.Depth = 1 then
        DefaultReaderDescription(FIconInfo.Width, FIconInfo.Height, FIconInfo.Depth, Desc)
      else
        DefaultReaderDescription(FIconInfo.Width, FIconInfo.Height, 32, Desc);
    end
    else
      DefaultReaderDescription(FIconInfo.Width, FIconInfo.Height, FIconInfo.Depth, Desc);
    if (Desc.BitsPerPixel = 32) then
      Desc.MaskBitsPerPixel := 0;
    FImage.DataDescription := Desc;
  end
  else Desc := FImage.DataDescription;

  SetupRead(FIconInfo.Width, FIconInfo.Height, FIconInfo.Depth, IsMask);

  FDataSize := Element.elementSize - SizeOf(Element);

  GetMem(FData, FDataSize);
  try
    Stream.Read(FData^, FDataSize);
    if FIconType in icnsWithAlpha then
      DoReadJpeg2000
    else
    if IsMask then
      DoReadMask
    else
    if FIconType in icnsRGB then
      DoReadRLE
    else
      DoReadRaw;
  finally
    FreeMem(FData);
    FData := nil;
  end;
end;

function TLazReaderIcnsPart.QueryInterface(constref iid: TGuid; out obj): longint; {$IFDEF WINDOWs}stdcall{$ELSE}cdecl{$ENDIF};
begin
  if GetInterface(iid, obj)
  then Result := S_OK
  else Result := E_NOINTERFACE;
end;

function TLazReaderIcnsPart._AddRef: LongInt; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
begin
  Result := -1;
end;

function TLazReaderIcnsPart._Release: LongInt; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
begin
  Result := -1;
end;

function TLazReaderIcnsPart.GetUpdateDescription: Boolean;
begin
  Result := FUpdateDescription;
end;

procedure TLazReaderIcnsPart.SetUpdateDescription(AValue: Boolean);
begin
  FUpdateDescription := AValue;
end;

procedure TLazReaderIcnsPart.SetupRead(AWidth, AHeight, ADepth: Integer; IsMask: Boolean);
begin
  if FData <> nil then
    FreeMem(FData);
  FreeAndNil(FPalette);
  if not IsMask then
    case ADepth of
      4: FPalette := CreateVGAPalette;
      8: FPalette := Create256ColorPalette;
    end;

  FCalcSize := ((AWidth * AHeight * ADepth) shr 3);
  TheImage.SetSize(AWidth, AHeight);
end;

procedure TLazReaderIcnsPart.DoReadRaw;
var
  Row, Column: Integer;
  shift: byte;
  b: PByte;
begin
  // only 4 and 8 are stored as raw image format
  case FIconInfo.Depth of
    4 :
      begin
        b := FData;
        shift := 4;
        for Row := 0 to FIconInfo.Height - 1 do
          for Column := 0 to FIconInfo.Width - 1 do
          begin
            FImage.colors[Column, Row] := FPalette[(b^ shr shift) mod 16];
            if shift = 0 then
            begin
              shift := 4;
              inc(b);
            end
            else
              shift := 0;
          end;
      end;
    8 :
      begin
        b := FData;
        for Row := 0 to FIconInfo.Height - 1 do
          for Column := 0 to FIconInfo.Width - 1 do
          begin
            FImage.colors[Column, Row] := FPalette[b^];
            inc(b);
          end;
      end;
  end;
end;

procedure TLazReaderIcnsPart.DoReadRLE;
var
  ADecompData: PDWord;
  ARGBAData: PRGBAQuad;
  Component, Shift: Byte;
  PixelCount, j, l: Integer;
  RepeatValue: DWord;
  SourcePtr: PByte;
  DestPtr: PDWord;
begin
  // only 24 bit RGB is RLE encoded the same way as TIFF or TGA RLE
  // data is encoded channel by channel:
  // high bit = 0 => length = low 0..6 bits + 1; read length times next value
  // high bit = 1 => length = value - 125      ; read one value and repeat length times

  ADecompData := AllocMem(FCalcSize);
  DestPtr := ADecompData;

  if FIconType = iitThumbnail32BitData
  then SourcePtr := @FData[4]
  else SourcePtr := FData;

  PixelCount := FIconInfo.Height * FIconInfo.Width;

  for Component := 0 to 2 do
  begin
    DestPtr := ADecompData;
    Shift := (2 - Component) * 8;
    while DestPtr - ADecompData < PixelCount do
    begin
      l := SourcePtr^;
      inc(SourcePtr);
      if (l and $80) = 0 then // high bit = 0
      begin
        for j := 0 to l do
        begin
          DestPtr^ := DestPtr^ or (DWord(SourcePtr^) shl Shift);
          inc(SourcePtr);
          inc(DestPtr);
        end;
      end
      else
      begin                   // high bit = 1
        l := l - 126;
        RepeatValue := DWord(SourcePtr^) shl Shift;
        inc(SourcePtr);
        for j := 0 to l do
        begin
          DestPtr^ := DestPtr^ or RepeatValue;
          inc(DestPtr);
        end;
      end;
    end;
  end;

  ARGBAData := PRGBAQuad(ADecompData);
  for l := 0 to FIconInfo.Height - 1 do
    for j := 0 to FIconInfo.Width - 1 do
    begin
      FImage.Colors[j, l] :=
        FPColor(ARGBAData^.Red shl 8 or ARGBAData^.Red,
                ARGBAData^.Green shl 8 or ARGBAData^.Green,
                ARGBAData^.Blue shl 8 or ARGBAData^.Blue,
                alphaOpaque);
      inc(ARGBAData);
    end;
  FreeMem(ADecompData);
end;

procedure TLazReaderIcnsPart.DoReadJpeg2000;
begin
  // TODO: according to some research in the web we need to read jpeg 2000 data
end;

procedure TLazReaderIcnsPart.DoReadMask;
var
  Row, Column: Integer;
  shift: byte;
  b: PByte;
begin
  case FIconInfo.Depth of
    1:
      begin
        // actually here is stored 2 1-bit images, but we will get only first
        shift := 7;
        b := FData;
        for Row := 0 to FIconInfo.Height - 1 do
        begin
          for Column := 0 to FIconInfo.Width - 1 do
          begin
            FImage.colors[Column, Row] := FPColor(0, 0, 0);
            FImage.Masked[Column, Row] := (b^ shr shift) mod 2 = 0;
            if shift = 0 then
            begin
              shift := 7;
              inc(b);
            end
            else
              dec(shift);
          end;
        end;
      end;
    8:
      begin
        b := FData;
        for Row := 0 to FIconInfo.Height - 1 do
          for Column := 0 to FIconInfo.Width - 1 do
          begin
            FImage.colors[Column, Row] := FPColor(0, 0, 0, (b^ shl 8) or b^);
            inc(b);
          end;
      end;
  end;
end;

function TLazReaderIcnsPart.Create256ColorPalette: TFPPalette;
const
  CHANNELVAL: array[0..15] of Word = (
    $FFFF, $CCCC, $9999, $6666, $3333, $0000,
    $EEEE, $DDDD, $BBBB, $AAAA, $8888,
    $7777, $5555, $4444, $2222, $1111
  );

var
  rIdx, gIdx, bIdx: byte;
  PalIdx: Byte;
begin
  Result := TFPPalette.Create(256);
  PalIdx := 0;
  for rIdx := 0 to 5 do
  begin
    for gIdx := 0 to 5 do
    begin
      for bIdx := 0 to 5 do
      begin
        Result[PalIdx] := FPColor(CHANNELVAL[rIdx], CHANNELVAL[gIdx], CHANNELVAL[bIdx]);
        Inc(PalIdx);
      end;
    end;
  end;
  for rIdx := 6 to 15 do
  begin
    Result[PalIdx] := FPColor(CHANNELVAL[rIdx], 0, 0);
    Inc(PalIdx);
  end;
  for gIdx := 6 to 15 do
  begin
    Result[PalIdx] := FPColor(0, CHANNELVAL[gIdx], 0);
    Inc(PalIdx);
  end;
  for bIdx := 6 to 15 do
  begin
    Result[PalIdx] := FPColor(0, 0, CHANNELVAL[bIdx]);
    Inc(PalIdx);
  end;
  for rIdx := 6 to 15 do
  begin
    Result[PalIdx] := FPColor(CHANNELVAL[rIdx], CHANNELVAL[rIdx], CHANNELVAL[rIdx]);
    Inc(PalIdx);
  end;
  Result[PalIdx] := FPColor(0, 0, 0);
end;

constructor TLazReaderIcnsPart.Create;
begin
  inherited Create;
  FData := nil;
  FPalette := nil;
  FCalcSize := 0;
  FIconType := iitNone;
end;

destructor TLazReaderIcnsPart.Destroy;
begin
  FPalette.Free;
  FreeMem(FData);
  inherited Destroy;
end;

//------------------------------------------------------------------------------
procedure InternalInit;
var
  c: Char;
begin
  for c:=Low(char) to High(char) do begin
    IsSpaceChar[c]:=c in [' ',#9,#10,#13];
    IsNumberChar[c]:=c in ['0'..'9'];
    IsHexNumberChar[c]:=c in ['0'..'9','A'..'F','a'..'f'];
  end;
end;

initialization
  InternalInit;

end.
