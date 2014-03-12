      {******************************************************************}
      { GDI+ Class (C++ Builder compatible)                              }
      {                                                                  }
      { Modified by Eugene Kryukov                                       }
      {                                                                  }
      { Latest change can be found at:                                   }
      {   http://www.ksdev.com/mplfiles/                                 }
      {                                                                  }
      {******************************************************************}
      { GDI+ Class                                                       }
      {                                                                  }
      { home page : http://www.progdigy.com                              }
      { email     : hgourvest@progdigy.com                               }
      {                                                                  }
      { The contents of this file are used with permission, subject to   }
      { the Mozilla Public License Version 1.1 (the "License"); you may  }
      { not use this file except in compliance with the License. You may }
      { obtain a copy of the License at                                  }
      { http://www.mozilla.org/MPL/MPL-1.1.html                          }
      {                                                                  }
      { Software distributed under the License is distributed on an      }
      { "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or   }
      { implied. See the License for the specific language governing     }
      { rights and limitations under the License.                        }
      {                                                                  }
      { *****************************************************************}

type
  INT16   = type Smallint;
  UINT16  = type Word;
  PUINT16 = ^UINT16;
  UINT32  = type Cardinal;
  TSingleDynArray = array of Single;

const WINGDIPDLL = 'gdiplus.dll';

//----------------------------------------------------------------------------
// Memory Allocation APIs
//----------------------------------------------------------------------------

var
{.$EXTERNALSYM GdipAlloc}
GdipAlloc: function(size: ULONG): pointer; stdcall;
{.$EXTERNALSYM GdipFree}
GdipFree: procedure(ptr: pointer); stdcall;  

type
  TGdiplusBase = class
  public
    class function NewInstance: TObject; override;
    procedure FreeInstance; override;
  end;

//--------------------------------------------------------------------------
// Default bezier flattening tolerance in device pixels.
//--------------------------------------------------------------------------

const
  {.$EXTERNALSYM FlatnessDefault}
  FlatnessDefault = 0.25;

//--------------------------------------------------------------------------
// Graphics and Container State cookies
//--------------------------------------------------------------------------
type
  {.$EXTERNALSYM GraphicsState}
  GraphicsState     = UINT;
  {.$EXTERNALSYM GraphicsContainer}
  GraphicsContainer = UINT;

//--------------------------------------------------------------------------
// Fill mode constants
//--------------------------------------------------------------------------

  {.$EXTERNALSYM FillMode}
  FillMode = (
    FillModeAlternate,        // 0
    FillModeWinding           // 1
  );
  TFillMode = FillMode;

//--------------------------------------------------------------------------
// Quality mode constants
//--------------------------------------------------------------------------

{$IFDEF DELPHI6_UP}
  {.$EXTERNALSYM QualityMode}
  QualityMode = (
    QualityModeInvalid   = -1,
    QualityModeDefault   =  0,
    QualityModeLow       =  1, // Best performance
    QualityModeHigh      =  2  // Best rendering quality
  );
  TQualityMode = QualityMode;
{$ELSE}
  {.$EXTERNALSYM QualityMode}
  QualityMode = Integer;
  const
    QualityModeInvalid   = -1;
    QualityModeDefault   =  0;
    QualityModeLow       =  1; // Best performance
    QualityModeHigh      =  2; // Best rendering quality
{$ENDIF}

//--------------------------------------------------------------------------
// Alpha Compositing mode constants
//--------------------------------------------------------------------------
type
  {.$EXTERNALSYM CompositingMode}
  CompositingMode = (
    CompositingModeSourceOver,    // 0
    CompositingModeSourceCopy     // 1
  );
  TCompositingMode = CompositingMode;

//--------------------------------------------------------------------------
// Alpha Compositing quality constants
//--------------------------------------------------------------------------
{$IFDEF DELPHI6_UP}
  {.$EXTERNALSYM CompositingQuality}
  CompositingQuality = (
    CompositingQualityInvalid          = ord(QualityModeInvalid),
    CompositingQualityDefault          = ord(QualityModeDefault),
    CompositingQualityHighSpeed        = ord(QualityModeLow),
    CompositingQualityHighQuality      = ord(QualityModeHigh),
    CompositingQualityGammaCorrected,
    CompositingQualityAssumeLinear
  );
  TCompositingQuality = CompositingQuality;
{$ELSE}
  {.$EXTERNALSYM CompositingQuality}
  CompositingQuality = Integer;
  const
    CompositingQualityInvalid          = QualityModeInvalid;
    CompositingQualityDefault          = QualityModeDefault;
    CompositingQualityHighSpeed        = QualityModeLow;
    CompositingQualityHighQuality      = QualityModeHigh;
    CompositingQualityGammaCorrected   = 3;
    CompositingQualityAssumeLinear     = 4;

type
  TCompositingQuality = CompositingQuality;
{$ENDIF}

//--------------------------------------------------------------------------
// Unit constants
//--------------------------------------------------------------------------

 // {.$EXTERNALSYM Unit}
  Unit_ = (
    UnitWorld,      // 0 -- World coordinate (non-physical unit)
    UnitDisplay,    // 1 -- Variable -- for PageTransform only
    UnitPixel,      // 2 -- Each unit is one device pixel.
    UnitPoint,      // 3 -- Each unit is a printer's point, or 1/72 inch.
    UnitInch,       // 4 -- Each unit is 1 inch.
    UnitDocument,   // 5 -- Each unit is 1/300 inch.
    UnitMillimeter  // 6 -- Each unit is 1 millimeter.
  );
  TUnit = Unit_;

//--------------------------------------------------------------------------
// MetafileFrameUnit
//
// The frameRect for creating a metafile can be specified in any of these
// units.  There is an extra frame unit value (MetafileFrameUnitGdi) so
// that units can be supplied in the same units that GDI expects for
// frame rects -- these units are in .01 (1/100ths) millimeter units
// as defined by GDI.
//--------------------------------------------------------------------------
{$IFDEF DELPHI6_UP}
  {.$EXTERNALSYM MetafileFrameUnit}
  MetafileFrameUnit = (
    MetafileFrameUnitPixel      = ord(UnitPixel),
    MetafileFrameUnitPoint      = ord(UnitPoint),
    MetafileFrameUnitInch       = ord(UnitInch),
    MetafileFrameUnitDocument   = ord(UnitDocument),
    MetafileFrameUnitMillimeter = ord(UnitMillimeter),
    MetafileFrameUnitGdi        // GDI compatible .01 MM units
  );
  TMetafileFrameUnit = MetafileFrameUnit;
{$ELSE}
  {.$EXTERNALSYM MetafileFrameUnit}
  MetafileFrameUnit = Integer;
  const
    MetafileFrameUnitPixel      = 2;
    MetafileFrameUnitPoint      = 3;
    MetafileFrameUnitInch       = 4;
    MetafileFrameUnitDocument   = 5;
    MetafileFrameUnitMillimeter = 6;
    MetafileFrameUnitGdi        = 7; // GDI compatible .01 MM units

type
  TMetafileFrameUnit = MetafileFrameUnit;
{$ENDIF}
//--------------------------------------------------------------------------
// Coordinate space identifiers
//--------------------------------------------------------------------------

  {.$EXTERNALSYM CoordinateSpace}
  CoordinateSpace = (
    CoordinateSpaceWorld,     // 0
    CoordinateSpacePage,      // 1
    CoordinateSpaceDevice     // 2
  );
  TCoordinateSpace = CoordinateSpace;

//--------------------------------------------------------------------------
// Various wrap modes for brushes
//--------------------------------------------------------------------------

  {.$EXTERNALSYM WrapMode}
  WrapMode = (
    WrapModeTile,        // 0
    WrapModeTileFlipX,   // 1
    WrapModeTileFlipY,   // 2
    WrapModeTileFlipXY,  // 3
    WrapModeClamp        // 4
  );
  TWrapMode = WrapMode;

//--------------------------------------------------------------------------
// Various hatch styles
//--------------------------------------------------------------------------

  {.$EXTERNALSYM HatchStyle}
  HatchStyle = (
    HatchStyleHorizontal,                  // = 0,
    HatchStyleVertical,                    // = 1,
    HatchStyleForwardDiagonal,             // = 2,
    HatchStyleBackwardDiagonal,            // = 3,
    HatchStyleCross,                       // = 4,
    HatchStyleDiagonalCross,               // = 5,
    HatchStyle05Percent,                   // = 6,
    HatchStyle10Percent,                   // = 7,
    HatchStyle20Percent,                   // = 8,
    HatchStyle25Percent,                   // = 9,
    HatchStyle30Percent,                   // = 10,
    HatchStyle40Percent,                   // = 11,
    HatchStyle50Percent,                   // = 12,
    HatchStyle60Percent,                   // = 13,
    HatchStyle70Percent,                   // = 14,
    HatchStyle75Percent,                   // = 15,
    HatchStyle80Percent,                   // = 16,
    HatchStyle90Percent,                   // = 17,
    HatchStyleLightDownwardDiagonal,       // = 18,
    HatchStyleLightUpwardDiagonal,         // = 19,
    HatchStyleDarkDownwardDiagonal,        // = 20,
    HatchStyleDarkUpwardDiagonal,          // = 21,
    HatchStyleWideDownwardDiagonal,        // = 22,
    HatchStyleWideUpwardDiagonal,          // = 23,
    HatchStyleLightVertical,               // = 24,
    HatchStyleLightHorizontal,             // = 25,
    HatchStyleNarrowVertical,              // = 26,
    HatchStyleNarrowHorizontal,            // = 27,
    HatchStyleDarkVertical,                // = 28,
    HatchStyleDarkHorizontal,              // = 29,
    HatchStyleDashedDownwardDiagonal,      // = 30,
    HatchStyleDashedUpwardDiagonal,        // = 31,
    HatchStyleDashedHorizontal,            // = 32,
    HatchStyleDashedVertical,              // = 33,
    HatchStyleSmallConfetti,               // = 34,
    HatchStyleLargeConfetti,               // = 35,
    HatchStyleZigZag,                      // = 36,
    HatchStyleWave,                        // = 37,
    HatchStyleDiagonalBrick,               // = 38,
    HatchStyleHorizontalBrick,             // = 39,
    HatchStyleWeave,                       // = 40,
    HatchStylePlaid,                       // = 41,
    HatchStyleDivot,                       // = 42,
    HatchStyleDottedGrid,                  // = 43,
    HatchStyleDottedDiamond,               // = 44,
    HatchStyleShingle,                     // = 45,
    HatchStyleTrellis,                     // = 46,
    HatchStyleSphere,                      // = 47,
    HatchStyleSmallGrid,                   // = 48,
    HatchStyleSmallCheckerBoard,           // = 49,
    HatchStyleLargeCheckerBoard,           // = 50,
    HatchStyleOutlinedDiamond,             // = 51,
    HatchStyleSolidDiamond,                // = 52,

    HatchStyleTotal                        // = 53,
  );

  const
    HatchStyleLargeGrid = HatchStyleCross; // 4
    HatchStyleMin       = HatchStyleHorizontal;
    HatchStyleMax       = HatchStyleSolidDiamond;

type
  THatchStyle = HatchStyle;

//--------------------------------------------------------------------------
// Dash style constants
//--------------------------------------------------------------------------

  {.$EXTERNALSYM DashStyle}
  DashStyle = (
    DashStyleSolid,          // 0
    DashStyleDash,           // 1
    DashStyleDot,            // 2
    DashStyleDashDot,        // 3
    DashStyleDashDotDot,     // 4
    DashStyleCustom          // 5
  );
  TDashStyle = DashStyle;

//--------------------------------------------------------------------------
// Dash cap constants
//--------------------------------------------------------------------------
{$IFDEF DELPHI6_UP}
  {.$EXTERNALSYM DashCap}
  DashCap = (
    DashCapFlat             = 0,
    DashCapRound            = 2,
    DashCapTriangle         = 3
  );
  TDashCap = DashCap;
{$ELSE}
  {.$EXTERNALSYM DashCap}
  DashCap = Integer;
  const
    DashCapFlat             = 0;
    DashCapRound            = 2;
    DashCapTriangle         = 3;

type
  TDashCap = DashCap;
{$ENDIF}

//--------------------------------------------------------------------------
// Line cap constants (only the lowest 8 bits are used).
//--------------------------------------------------------------------------
{$IFDEF DELPHI6_UP}
  {.$EXTERNALSYM LineCap}
  LineCap = (
    LineCapFlat             = 0,
    LineCapSquare           = 1,
    LineCapRound            = 2,
    LineCapTriangle         = 3,

    LineCapNoAnchor         = $10, // corresponds to flat cap
    LineCapSquareAnchor     = $11, // corresponds to square cap
    LineCapRoundAnchor      = $12, // corresponds to round cap
    LineCapDiamondAnchor    = $13, // corresponds to triangle cap
    LineCapArrowAnchor      = $14, // no correspondence

    LineCapCustom           = $ff, // custom cap

    LineCapAnchorMask       = $f0  // mask to check for anchor or not.
  );
  TLineCap = LineCap;
{$ELSE}
  {.$EXTERNALSYM LineCap}
  LineCap = Integer;
  const
    LineCapFlat             = 0;
    LineCapSquare           = 1;
    LineCapRound            = 2;
    LineCapTriangle         = 3;

    LineCapNoAnchor         = $10; // corresponds to flat cap
    LineCapSquareAnchor     = $11; // corresponds to square cap
    LineCapRoundAnchor      = $12; // corresponds to round cap
    LineCapDiamondAnchor    = $13; // corresponds to triangle cap
    LineCapArrowAnchor      = $14; // no correspondence

    LineCapCustom           = $ff; // custom cap

    LineCapAnchorMask       = $f0; // mask to check for anchor or not.

type
  TLineCap = LineCap;
{$ENDIF}

//--------------------------------------------------------------------------
// Custom Line cap type constants
//--------------------------------------------------------------------------

  {.$EXTERNALSYM CustomLineCapType}
  CustomLineCapType = (
    CustomLineCapTypeDefault,
    CustomLineCapTypeAdjustableArrow
  );
  TCustomLineCapType = CustomLineCapType;

//--------------------------------------------------------------------------
// Line join constants
//--------------------------------------------------------------------------

  {.$EXTERNALSYM LineJoin}
  LineJoin = (
    LineJoinMiter,
    LineJoinBevel,
    LineJoinRound,
    LineJoinMiterClipped
  );
  TLineJoin = LineJoin;

//--------------------------------------------------------------------------
// Path point types (only the lowest 8 bits are used.)
//  The lowest 3 bits are interpreted as point type
//  The higher 5 bits are reserved for flags.
//--------------------------------------------------------------------------

{$IFDEF DELPHI6_UP}
  {$Z1}
  {.$EXTERNALSYM PathPointType}
  PathPointType = (
    PathPointTypeStart           = $00, // move
    PathPointTypeLine            = $01, // line
    PathPointTypeBezier          = $03, // default Bezier (= cubic Bezier)
    PathPointTypePathTypeMask    = $07, // type mask (lowest 3 bits).
    PathPointTypeDashMode        = $10, // currently in dash mode.
    PathPointTypePathMarker      = $20, // a marker for the path.
    PathPointTypeCloseSubpath    = $80, // closed flag

    // Path types used for advanced path.
    PathPointTypeBezier3         = $03  // cubic Bezier
  );
  TPathPointType = PathPointType;
  {$Z4}
{$ELSE}
  {.$EXTERNALSYM PathPointType}
  PathPointType = Byte;
  const
    PathPointTypeStart          : Byte = $00; // move
    PathPointTypeLine           : Byte = $01; // line
    PathPointTypeBezier         : Byte = $03; // default Bezier (= cubic Bezier)
    PathPointTypePathTypeMask   : Byte = $07; // type mask (lowest 3 bits).
    PathPointTypeDashMode       : Byte = $10; // currently in dash mode.
    PathPointTypePathMarker     : Byte = $20; // a marker for the path.
    PathPointTypeCloseSubpath   : Byte = $80; // closed flag

    // Path types used for advanced path.
    PathPointTypeBezier3        : Byte = $03;  // cubic Bezier

type
  TPathPointType = PathPointType;
{$ENDIF}

//--------------------------------------------------------------------------
// WarpMode constants
//--------------------------------------------------------------------------

  {.$EXTERNALSYM WarpMode}
  WarpMode = (
    WarpModePerspective,    // 0
    WarpModeBilinear        // 1
  );
  TWarpMode = WarpMode;

//--------------------------------------------------------------------------
// LineGradient Mode
//--------------------------------------------------------------------------

  {.$EXTERNALSYM LinearGradientMode}
  LinearGradientMode = (
    LinearGradientModeHorizontal,         // 0
    LinearGradientModeVertical,           // 1
    LinearGradientModeForwardDiagonal,    // 2
    LinearGradientModeBackwardDiagonal    // 3
  );
  TLinearGradientMode = LinearGradientMode;

//--------------------------------------------------------------------------
// Region Comine Modes
//--------------------------------------------------------------------------

  {.$EXTERNALSYM CombineMode}
  CombineMode = (
    CombineModeReplace,     // 0
    CombineModeIntersect,   // 1
    CombineModeUnion,       // 2
    CombineModeXor,         // 3
    CombineModeExclude,     // 4
    CombineModeComplement   // 5 (Exclude From)
  );
  TCombineMode = CombineMode;

//--------------------------------------------------------------------------
 // Image types
//--------------------------------------------------------------------------

  {.$EXTERNALSYM ImageType}
  ImageType = (
    ImageTypeUnknown,   // 0
    ImageTypeBitmap,    // 1
    ImageTypeMetafile   // 2
  );
  TImageType = ImageType;

//--------------------------------------------------------------------------
// Interpolation modes
//--------------------------------------------------------------------------
{$IFDEF DELPHI6_UP}
  {.$EXTERNALSYM InterpolationMode}
  InterpolationMode = (
    InterpolationModeInvalid          = ord(QualityModeInvalid),
    InterpolationModeDefault          = ord(QualityModeDefault),
    InterpolationModeLowQuality       = ord(QualityModeLow),
    InterpolationModeHighQuality      = ord(QualityModeHigh),
    InterpolationModeBilinear,
    InterpolationModeBicubic,
    InterpolationModeNearestNeighbor,
    InterpolationModeHighQualityBilinear,
    InterpolationModeHighQualityBicubic
  );
  TInterpolationMode = InterpolationMode;
{$ELSE}
  {.$EXTERNALSYM InterpolationMode}
  InterpolationMode = Integer;
  const
    InterpolationModeInvalid             = QualityModeInvalid;
    InterpolationModeDefault             = QualityModeDefault;
    InterpolationModeLowQuality          = QualityModeLow;
    InterpolationModeHighQuality         = QualityModeHigh;
    InterpolationModeBilinear            = 3;
    InterpolationModeBicubic             = 4;
    InterpolationModeNearestNeighbor     = 5;
    InterpolationModeHighQualityBilinear = 6;
    InterpolationModeHighQualityBicubic  = 7;

type
  TInterpolationMode = InterpolationMode;
{$ENDIF}

//--------------------------------------------------------------------------
// Pen types
//--------------------------------------------------------------------------

  {.$EXTERNALSYM PenAlignment}
  PenAlignment = (
    PenAlignmentCenter,
    PenAlignmentInset
  );
  TPenAlignment = PenAlignment;

//--------------------------------------------------------------------------
// Brush types
//--------------------------------------------------------------------------

  {.$EXTERNALSYM BrushType}
  BrushType = (
   BrushTypeSolidColor,
   BrushTypeHatchFill,
   BrushTypeTextureFill,
   BrushTypePathGradient,
   BrushTypeLinearGradient 
  );
  TBrushType = BrushType;

//--------------------------------------------------------------------------
// Pen's Fill types
//--------------------------------------------------------------------------
{$IFDEF DELPHI6_UP}
  {.$EXTERNALSYM PenType}
  PenType = (
   PenTypeSolidColor       =  ord(BrushTypeSolidColor),
   PenTypeHatchFill        =  ord(BrushTypeHatchFill),
   PenTypeTextureFill      =  ord(BrushTypeTextureFill),
   PenTypePathGradient     =  ord(BrushTypePathGradient),
   PenTypeLinearGradient   =  ord(BrushTypeLinearGradient),
   PenTypeUnknown          = -1
  );
  TPenType = PenType;
{$ELSE}
  {.$EXTERNALSYM PenType}
  PenType = Integer;
  const
    PenTypeSolidColor       =  0;
    PenTypeHatchFill        =  1;
    PenTypeTextureFill      =  2;
    PenTypePathGradient     =  3;
    PenTypeLinearGradient   =  4;
    PenTypeUnknown          = -1;

type
  TPenType = PenType;
{$ENDIF}

//--------------------------------------------------------------------------
// Matrix Order
//--------------------------------------------------------------------------

  {.$EXTERNALSYM MatrixOrder}
  MatrixOrder = (
    MatrixOrderPrepend,
    MatrixOrderAppend
  );
  TMatrixOrder = MatrixOrder;

//--------------------------------------------------------------------------
// Generic font families
//--------------------------------------------------------------------------

  {.$EXTERNALSYM GenericFontFamily}
  GenericFontFamily = (
    GenericFontFamilySerif,
    GenericFontFamilySansSerif,
    GenericFontFamilyMonospace
  );
  TGenericFontFamily = GenericFontFamily;

//--------------------------------------------------------------------------
// FontStyle: face types and common styles
//--------------------------------------------------------------------------
type
  {.$EXTERNALSYM FontStyle}
  FontStyle = Integer;
  const
    FontStyleRegular    = Integer(0);
    FontStyleBold       = Integer(1);
    FontStyleItalic     = Integer(2);
    FontStyleBoldItalic = Integer(3);
    FontStyleUnderline  = Integer(4);
    FontStyleStrikeout  = Integer(8);
  Type
  TFontStyle = FontStyle;

//---------------------------------------------------------------------------
// Smoothing Mode
//---------------------------------------------------------------------------
{$IFDEF DELPHI6_UP}
  {.$EXTERNALSYM SmoothingMode}
  SmoothingMode = (
    SmoothingModeInvalid     = ord(QualityModeInvalid),
    SmoothingModeDefault     = ord(QualityModeDefault),
    SmoothingModeHighSpeed   = ord(QualityModeLow),
    SmoothingModeHighQuality = ord(QualityModeHigh),
    SmoothingModeNone,
    SmoothingModeAntiAlias
  );
  TSmoothingMode = SmoothingMode;
{$ELSE}
  {.$EXTERNALSYM SmoothingMode}
  SmoothingMode = Integer;
  const
    SmoothingModeInvalid     = QualityModeInvalid;
    SmoothingModeDefault     = QualityModeDefault;
    SmoothingModeHighSpeed   = QualityModeLow;
    SmoothingModeHighQuality = QualityModeHigh;
    SmoothingModeNone        = 3;
    SmoothingModeAntiAlias   = 4;

type
  TSmoothingMode = SmoothingMode;
{$ENDIF}

//---------------------------------------------------------------------------
// Pixel Format Mode
//---------------------------------------------------------------------------
{$IFDEF DELPHI6_UP}
  {.$EXTERNALSYM PixelOffsetMode}
  PixelOffsetMode = (
    PixelOffsetModeInvalid     = Ord(QualityModeInvalid),
    PixelOffsetModeDefault     = Ord(QualityModeDefault),
    PixelOffsetModeHighSpeed   = Ord(QualityModeLow),
    PixelOffsetModeHighQuality = Ord(QualityModeHigh),
    PixelOffsetModeNone,    // No pixel offset
    PixelOffsetModeHalf     // Offset by -0.5, -0.5 for fast anti-alias perf
  );
  TPixelOffsetMode = PixelOffsetMode;
{$ELSE}
  {.$EXTERNALSYM PixelOffsetMode}
  PixelOffsetMode = Integer;
  const
    PixelOffsetModeInvalid     = QualityModeInvalid;
    PixelOffsetModeDefault     = QualityModeDefault;
    PixelOffsetModeHighSpeed   = QualityModeLow;
    PixelOffsetModeHighQuality = QualityModeHigh;
    PixelOffsetModeNone        = 3;    // No pixel offset
    PixelOffsetModeHalf        = 4;    // Offset by -0.5, -0.5 for fast anti-alias perf

type
  TPixelOffsetMode = PixelOffsetMode;
{$ENDIF}

//---------------------------------------------------------------------------
// Text Rendering Hint
//---------------------------------------------------------------------------

  {.$EXTERNALSYM TextRenderingHint}
  TextRenderingHint = (
    TextRenderingHintSystemDefault,                // Glyph with system default rendering hint
    TextRenderingHintSingleBitPerPixelGridFit,     // Glyph bitmap with hinting
    TextRenderingHintSingleBitPerPixel,            // Glyph bitmap without hinting
    TextRenderingHintAntiAliasGridFit,             // Glyph anti-alias bitmap with hinting
    TextRenderingHintAntiAlias,                    // Glyph anti-alias bitmap without hinting
    TextRenderingHintClearTypeGridFit              // Glyph CT bitmap with hinting
  );
  TTextRenderingHint = TextRenderingHint;

//---------------------------------------------------------------------------
// Metafile Types
//---------------------------------------------------------------------------

  {.$EXTERNALSYM MetafileType}
  MetafileType = (
    MetafileTypeInvalid,            // Invalid metafile
    MetafileTypeWmf,                // Standard WMF
    MetafileTypeWmfPlaceable,       // Placeable WMF
    MetafileTypeEmf,                // EMF (not EMF+)
    MetafileTypeEmfPlusOnly,        // EMF+ without dual, down-level records
    MetafileTypeEmfPlusDual         // EMF+ with dual, down-level records
  );
  TMetafileType = MetafileType;

//---------------------------------------------------------------------------
// Specifies the type of EMF to record
//---------------------------------------------------------------------------
{$IFDEF DELPHI6_UP}
  {.$EXTERNALSYM EmfType}
  EmfType = (
    EmfTypeEmfOnly     = Ord(MetafileTypeEmf),          // no EMF+, only EMF
    EmfTypeEmfPlusOnly = Ord(MetafileTypeEmfPlusOnly),  // no EMF, only EMF+
    EmfTypeEmfPlusDual = Ord(MetafileTypeEmfPlusDual)   // both EMF+ and EMF
  );
  TEmfType = EmfType;
{$ELSE}
  {.$EXTERNALSYM EmfType}
  EmfType = Integer;
  const
    EmfTypeEmfOnly     = Ord(MetafileTypeEmf);          // no EMF+, only EMF
    EmfTypeEmfPlusOnly = Ord(MetafileTypeEmfPlusOnly);  // no EMF, only EMF+
    EmfTypeEmfPlusDual = Ord(MetafileTypeEmfPlusDual);   // both EMF+ and EMF

type
  TEmfType = EmfType;
{$ENDIF}

//---------------------------------------------------------------------------
// EMF+ Persistent object types
//---------------------------------------------------------------------------

  {.$EXTERNALSYM ObjectType}
  ObjectType = (
    ObjectTypeInvalid,
    ObjectTypeBrush,
    ObjectTypePen,
    ObjectTypePath,
    ObjectTypeRegion,
    ObjectTypeImage,
    ObjectTypeFont,
    ObjectTypeStringFormat,
    ObjectTypeImageAttributes,
    ObjectTypeCustomLineCap
  );
  TObjectType = ObjectType;

const
  ObjectTypeMax = ObjectTypeCustomLineCap;
  ObjectTypeMin = ObjectTypeBrush;

//---------------------------------------------------------------------------
// EMF+ Records
//---------------------------------------------------------------------------

  // We have to change the WMF record numbers so that they don't conflict with
  // the EMF and EMF+ record numbers.

const
  GDIP_EMFPLUS_RECORD_BASE      = $00004000;
  {.$EXTERNALSYM GDIP_EMFPLUS_RECORD_BASE}
  GDIP_WMF_RECORD_BASE          = $00010000;
  {.$EXTERNALSYM GDIP_WMF_RECORD_BASE}

{$IFDEF DELPHI6_UP}
type
  {.$EXTERNALSYM EmfPlusRecordType}
  EmfPlusRecordType = (
   // Since we have to enumerate GDI records right along with GDI+ records,
   // We list all the GDI records here so that they can be part of the
   // same enumeration type which is used in the enumeration callback.

    WmfRecordTypeSetBkColor              = (META_SETBKCOLOR or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSetBkMode               = (META_SETBKMODE or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSetMapMode              = (META_SETMAPMODE or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSetROP2                 = (META_SETROP2 or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSetRelAbs               = (META_SETRELABS or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSetPolyFillMode         = (META_SETPOLYFILLMODE or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSetStretchBltMode       = (META_SETSTRETCHBLTMODE or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSetTextCharExtra        = (META_SETTEXTCHAREXTRA or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSetTextColor            = (META_SETTEXTCOLOR or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSetTextJustification    = (META_SETTEXTJUSTIFICATION or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSetWindowOrg            = (META_SETWINDOWORG or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSetWindowExt            = (META_SETWINDOWEXT or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSetViewportOrg          = (META_SETVIEWPORTORG or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSetViewportExt          = (META_SETVIEWPORTEXT or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeOffsetWindowOrg         = (META_OFFSETWINDOWORG or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeScaleWindowExt          = (META_SCALEWINDOWEXT or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeOffsetViewportOrg       = (META_OFFSETVIEWPORTORG or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeScaleViewportExt        = (META_SCALEVIEWPORTEXT or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeLineTo                  = (META_LINETO or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeMoveTo                  = (META_MOVETO or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeExcludeClipRect         = (META_EXCLUDECLIPRECT or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeIntersectClipRect       = (META_INTERSECTCLIPRECT or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeArc                     = (META_ARC or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeEllipse                 = (META_ELLIPSE or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeFloodFill               = (META_FLOODFILL or GDIP_WMF_RECORD_BASE),
    WmfRecordTypePie                     = (META_PIE or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeRectangle               = (META_RECTANGLE or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeRoundRect               = (META_ROUNDRECT or GDIP_WMF_RECORD_BASE),
    WmfRecordTypePatBlt                  = (META_PATBLT or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSaveDC                  = (META_SAVEDC or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSetPixel                = (META_SETPIXEL or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeOffsetClipRgn           = (META_OFFSETCLIPRGN or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeTextOut                 = (META_TEXTOUT or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeBitBlt                  = (META_BITBLT or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeStretchBlt              = (META_STRETCHBLT or GDIP_WMF_RECORD_BASE),
    WmfRecordTypePolygon                 = (META_POLYGON or GDIP_WMF_RECORD_BASE),
    WmfRecordTypePolyline                = (META_POLYLINE or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeEscape                  = (META_ESCAPE or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeRestoreDC               = (META_RESTOREDC or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeFillRegion              = (META_FILLREGION or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeFrameRegion             = (META_FRAMEREGION or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeInvertRegion            = (META_INVERTREGION or GDIP_WMF_RECORD_BASE),
    WmfRecordTypePaintRegion             = (META_PAINTREGION or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSelectClipRegion        = (META_SELECTCLIPREGION or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSelectObject            = (META_SELECTOBJECT or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSetTextAlign            = (META_SETTEXTALIGN or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeDrawText                = ($062F or GDIP_WMF_RECORD_BASE),  // META_DRAWTEXT
    WmfRecordTypeChord                   = (META_CHORD or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSetMapperFlags          = (META_SETMAPPERFLAGS or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeExtTextOut              = (META_EXTTEXTOUT or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSetDIBToDev             = (META_SETDIBTODEV or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSelectPalette           = (META_SELECTPALETTE or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeRealizePalette          = (META_REALIZEPALETTE or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeAnimatePalette          = (META_ANIMATEPALETTE or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSetPalEntries           = (META_SETPALENTRIES or GDIP_WMF_RECORD_BASE),
    WmfRecordTypePolyPolygon             = (META_POLYPOLYGON or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeResizePalette           = (META_RESIZEPALETTE or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeDIBBitBlt               = (META_DIBBITBLT or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeDIBStretchBlt           = (META_DIBSTRETCHBLT or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeDIBCreatePatternBrush   = (META_DIBCREATEPATTERNBRUSH or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeStretchDIB              = (META_STRETCHDIB or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeExtFloodFill            = (META_EXTFLOODFILL or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSetLayout               = ($0149 or GDIP_WMF_RECORD_BASE),  // META_SETLAYOUT
    WmfRecordTypeResetDC                 = ($014C or GDIP_WMF_RECORD_BASE),  // META_RESETDC
    WmfRecordTypeStartDoc                = ($014D or GDIP_WMF_RECORD_BASE),  // META_STARTDOC
    WmfRecordTypeStartPage               = ($004F or GDIP_WMF_RECORD_BASE),  // META_STARTPAGE
    WmfRecordTypeEndPage                 = ($0050 or GDIP_WMF_RECORD_BASE),  // META_ENDPAGE
    WmfRecordTypeAbortDoc                = ($0052 or GDIP_WMF_RECORD_BASE),  // META_ABORTDOC
    WmfRecordTypeEndDoc                  = ($005E or GDIP_WMF_RECORD_BASE),  // META_ENDDOC
    WmfRecordTypeDeleteObject            = (META_DELETEOBJECT or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeCreatePalette           = (META_CREATEPALETTE or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeCreateBrush             = ($00F8 or GDIP_WMF_RECORD_BASE),  // META_CREATEBRUSH
    WmfRecordTypeCreatePatternBrush      = (META_CREATEPATTERNBRUSH or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeCreatePenIndirect       = (META_CREATEPENINDIRECT or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeCreateFontIndirect      = (META_CREATEFONTINDIRECT or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeCreateBrushIndirect     = (META_CREATEBRUSHINDIRECT or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeCreateBitmapIndirect    = ($02FD or GDIP_WMF_RECORD_BASE),  // META_CREATEBITMAPINDIRECT
    WmfRecordTypeCreateBitmap            = ($06FE or GDIP_WMF_RECORD_BASE),  // META_CREATEBITMAP
    WmfRecordTypeCreateRegion            = (META_CREATEREGION or GDIP_WMF_RECORD_BASE),

    EmfRecordTypeHeader                  = EMR_HEADER,
    EmfRecordTypePolyBezier              = EMR_POLYBEZIER,
    EmfRecordTypePolygon                 = EMR_POLYGON,
    EmfRecordTypePolyline                = EMR_POLYLINE,
    EmfRecordTypePolyBezierTo            = EMR_POLYBEZIERTO,
    EmfRecordTypePolyLineTo              = EMR_POLYLINETO,
    EmfRecordTypePolyPolyline            = EMR_POLYPOLYLINE,
    EmfRecordTypePolyPolygon             = EMR_POLYPOLYGON,
    EmfRecordTypeSetWindowExtEx          = EMR_SETWINDOWEXTEX,
    EmfRecordTypeSetWindowOrgEx          = EMR_SETWINDOWORGEX,
    EmfRecordTypeSetViewportExtEx        = EMR_SETVIEWPORTEXTEX,
    EmfRecordTypeSetViewportOrgEx        = EMR_SETVIEWPORTORGEX,
    EmfRecordTypeSetBrushOrgEx           = EMR_SETBRUSHORGEX,
    EmfRecordTypeEOF                     = EMR_EOF,
    EmfRecordTypeSetPixelV               = EMR_SETPIXELV,
    EmfRecordTypeSetMapperFlags          = EMR_SETMAPPERFLAGS,
    EmfRecordTypeSetMapMode              = EMR_SETMAPMODE,
    EmfRecordTypeSetBkMode               = EMR_SETBKMODE,
    EmfRecordTypeSetPolyFillMode         = EMR_SETPOLYFILLMODE,
    EmfRecordTypeSetROP2                 = EMR_SETROP2,
    EmfRecordTypeSetStretchBltMode       = EMR_SETSTRETCHBLTMODE,
    EmfRecordTypeSetTextAlign            = EMR_SETTEXTALIGN,
    EmfRecordTypeSetColorAdjustment      = EMR_SETCOLORADJUSTMENT,
    EmfRecordTypeSetTextColor            = EMR_SETTEXTCOLOR,
    EmfRecordTypeSetBkColor              = EMR_SETBKCOLOR,
    EmfRecordTypeOffsetClipRgn           = EMR_OFFSETCLIPRGN,
    EmfRecordTypeMoveToEx                = EMR_MOVETOEX,
    EmfRecordTypeSetMetaRgn              = EMR_SETMETARGN,
    EmfRecordTypeExcludeClipRect         = EMR_EXCLUDECLIPRECT,
    EmfRecordTypeIntersectClipRect       = EMR_INTERSECTCLIPRECT,
    EmfRecordTypeScaleViewportExtEx      = EMR_SCALEVIEWPORTEXTEX,
    EmfRecordTypeScaleWindowExtEx        = EMR_SCALEWINDOWEXTEX,
    EmfRecordTypeSaveDC                  = EMR_SAVEDC,
    EmfRecordTypeRestoreDC               = EMR_RESTOREDC,
    EmfRecordTypeSetWorldTransform       = EMR_SETWORLDTRANSFORM,
    EmfRecordTypeModifyWorldTransform    = EMR_MODIFYWORLDTRANSFORM,
    EmfRecordTypeSelectObject            = EMR_SELECTOBJECT,
    EmfRecordTypeCreatePen               = EMR_CREATEPEN,
    EmfRecordTypeCreateBrushIndirect     = EMR_CREATEBRUSHINDIRECT,
    EmfRecordTypeDeleteObject            = EMR_DELETEOBJECT,
    EmfRecordTypeAngleArc                = EMR_ANGLEARC,
    EmfRecordTypeEllipse                 = EMR_ELLIPSE,
    EmfRecordTypeRectangle               = EMR_RECTANGLE,
    EmfRecordTypeRoundRect               = EMR_ROUNDRECT,
    EmfRecordTypeArc                     = EMR_ARC,
    EmfRecordTypeChord                   = EMR_CHORD,
    EmfRecordTypePie                     = EMR_PIE,
    EmfRecordTypeSelectPalette           = EMR_SELECTPALETTE,
    EmfRecordTypeCreatePalette           = EMR_CREATEPALETTE,
    EmfRecordTypeSetPaletteEntries       = EMR_SETPALETTEENTRIES,
    EmfRecordTypeResizePalette           = EMR_RESIZEPALETTE,
    EmfRecordTypeRealizePalette          = EMR_REALIZEPALETTE,
    EmfRecordTypeExtFloodFill            = EMR_EXTFLOODFILL,
    EmfRecordTypeLineTo                  = EMR_LINETO,
    EmfRecordTypeArcTo                   = EMR_ARCTO,
    EmfRecordTypePolyDraw                = EMR_POLYDRAW,
    EmfRecordTypeSetArcDirection         = EMR_SETARCDIRECTION,
    EmfRecordTypeSetMiterLimit           = EMR_SETMITERLIMIT,
    EmfRecordTypeBeginPath               = EMR_BEGINPATH,
    EmfRecordTypeEndPath                 = EMR_ENDPATH,
    EmfRecordTypeCloseFigure             = EMR_CLOSEFIGURE,
    EmfRecordTypeFillPath                = EMR_FILLPATH,
    EmfRecordTypeStrokeAndFillPath       = EMR_STROKEANDFILLPATH,
    EmfRecordTypeStrokePath              = EMR_STROKEPATH,
    EmfRecordTypeFlattenPath             = EMR_FLATTENPATH,
    EmfRecordTypeWidenPath               = EMR_WIDENPATH,
    EmfRecordTypeSelectClipPath          = EMR_SELECTCLIPPATH,
    EmfRecordTypeAbortPath               = EMR_ABORTPATH,
    EmfRecordTypeReserved_069            = 69,  // Not Used
    EmfRecordTypeGdiComment              = EMR_GDICOMMENT,
    EmfRecordTypeFillRgn                 = EMR_FILLRGN,
    EmfRecordTypeFrameRgn                = EMR_FRAMERGN,
    EmfRecordTypeInvertRgn               = EMR_INVERTRGN,
    EmfRecordTypePaintRgn                = EMR_PAINTRGN,
    EmfRecordTypeExtSelectClipRgn        = EMR_EXTSELECTCLIPRGN,
    EmfRecordTypeBitBlt                  = EMR_BITBLT,
    EmfRecordTypeStretchBlt              = EMR_STRETCHBLT,
    EmfRecordTypeMaskBlt                 = EMR_MASKBLT,
    EmfRecordTypePlgBlt                  = EMR_PLGBLT,
    EmfRecordTypeSetDIBitsToDevice       = EMR_SETDIBITSTODEVICE,
    EmfRecordTypeStretchDIBits           = EMR_STRETCHDIBITS,
    EmfRecordTypeExtCreateFontIndirect   = EMR_EXTCREATEFONTINDIRECTW,
    EmfRecordTypeExtTextOutA             = EMR_EXTTEXTOUTA,
    EmfRecordTypeExtTextOutW             = EMR_EXTTEXTOUTW,
    EmfRecordTypePolyBezier16            = EMR_POLYBEZIER16,
    EmfRecordTypePolygon16               = EMR_POLYGON16,
    EmfRecordTypePolyline16              = EMR_POLYLINE16,
    EmfRecordTypePolyBezierTo16          = EMR_POLYBEZIERTO16,
    EmfRecordTypePolylineTo16            = EMR_POLYLINETO16,
    EmfRecordTypePolyPolyline16          = EMR_POLYPOLYLINE16,
    EmfRecordTypePolyPolygon16           = EMR_POLYPOLYGON16,
    EmfRecordTypePolyDraw16              = EMR_POLYDRAW16,
    EmfRecordTypeCreateMonoBrush         = EMR_CREATEMONOBRUSH,
    EmfRecordTypeCreateDIBPatternBrushPt = EMR_CREATEDIBPATTERNBRUSHPT,
    EmfRecordTypeExtCreatePen            = EMR_EXTCREATEPEN,
    EmfRecordTypePolyTextOutA            = EMR_POLYTEXTOUTA,
    EmfRecordTypePolyTextOutW            = EMR_POLYTEXTOUTW,
    EmfRecordTypeSetICMMode              = 98,  // EMR_SETICMMODE,
    EmfRecordTypeCreateColorSpace        = 99,  // EMR_CREATECOLORSPACE,
    EmfRecordTypeSetColorSpace           = 100, // EMR_SETCOLORSPACE,
    EmfRecordTypeDeleteColorSpace        = 101, // EMR_DELETECOLORSPACE,
    EmfRecordTypeGLSRecord               = 102, // EMR_GLSRECORD,
    EmfRecordTypeGLSBoundedRecord        = 103, // EMR_GLSBOUNDEDRECORD,
    EmfRecordTypePixelFormat             = 104, // EMR_PIXELFORMAT,
    EmfRecordTypeDrawEscape              = 105, // EMR_RESERVED_105,
    EmfRecordTypeExtEscape               = 106, // EMR_RESERVED_106,
    EmfRecordTypeStartDoc                = 107, // EMR_RESERVED_107,
    EmfRecordTypeSmallTextOut            = 108, // EMR_RESERVED_108,
    EmfRecordTypeForceUFIMapping         = 109, // EMR_RESERVED_109,
    EmfRecordTypeNamedEscape             = 110, // EMR_RESERVED_110,
    EmfRecordTypeColorCorrectPalette     = 111, // EMR_COLORCORRECTPALETTE,
    EmfRecordTypeSetICMProfileA          = 112, // EMR_SETICMPROFILEA,
    EmfRecordTypeSetICMProfileW          = 113, // EMR_SETICMPROFILEW,
    EmfRecordTypeAlphaBlend              = 114, // EMR_ALPHABLEND,
    EmfRecordTypeSetLayout               = 115, // EMR_SETLAYOUT,
    EmfRecordTypeTransparentBlt          = 116, // EMR_TRANSPARENTBLT,
    EmfRecordTypeReserved_117            = 117, // Not Used
    EmfRecordTypeGradientFill            = 118, // EMR_GRADIENTFILL,
    EmfRecordTypeSetLinkedUFIs           = 119, // EMR_RESERVED_119,
    EmfRecordTypeSetTextJustification    = 120, // EMR_RESERVED_120,
    EmfRecordTypeColorMatchToTargetW     = 121, // EMR_COLORMATCHTOTARGETW,
    EmfRecordTypeCreateColorSpaceW       = 122, // EMR_CREATECOLORSPACEW,
    EmfRecordTypeMax                     = 122,
    EmfRecordTypeMin                     = 1,

    // That is the END of the GDI EMF records.

    // Now we start the list of EMF+ records.  We leave quite
    // a bit of room here for the addition of any new GDI
    // records that may be added later.

    EmfPlusRecordTypeInvalid = GDIP_EMFPLUS_RECORD_BASE,
    EmfPlusRecordTypeHeader,
    EmfPlusRecordTypeEndOfFile,

    EmfPlusRecordTypeComment,

    EmfPlusRecordTypeGetDC,

    EmfPlusRecordTypeMultiFormatStart,
    EmfPlusRecordTypeMultiFormatSection,
    EmfPlusRecordTypeMultiFormatEnd,

    // For all persistent objects

    EmfPlusRecordTypeObject,

    // Drawing Records

    EmfPlusRecordTypeClear,
    EmfPlusRecordTypeFillRects,
    EmfPlusRecordTypeDrawRects,
    EmfPlusRecordTypeFillPolygon,
    EmfPlusRecordTypeDrawLines,
    EmfPlusRecordTypeFillEllipse,
    EmfPlusRecordTypeDrawEllipse,
    EmfPlusRecordTypeFillPie,
    EmfPlusRecordTypeDrawPie,
    EmfPlusRecordTypeDrawArc,
    EmfPlusRecordTypeFillRegion,
    EmfPlusRecordTypeFillPath,
    EmfPlusRecordTypeDrawPath,
    EmfPlusRecordTypeFillClosedCurve,
    EmfPlusRecordTypeDrawClosedCurve,
    EmfPlusRecordTypeDrawCurve,
    EmfPlusRecordTypeDrawBeziers,
    EmfPlusRecordTypeDrawImage,
    EmfPlusRecordTypeDrawImagePoints,
    EmfPlusRecordTypeDrawString,

    // Graphics State Records

    EmfPlusRecordTypeSetRenderingOrigin,
    EmfPlusRecordTypeSetAntiAliasMode,
    EmfPlusRecordTypeSetTextRenderingHint,
    EmfPlusRecordTypeSetTextContrast,
    EmfPlusRecordTypeSetInterpolationMode,
    EmfPlusRecordTypeSetPixelOffsetMode,
    EmfPlusRecordTypeSetCompositingMode,
    EmfPlusRecordTypeSetCompositingQuality,
    EmfPlusRecordTypeSave,
    EmfPlusRecordTypeRestore,
    EmfPlusRecordTypeBeginContainer,
    EmfPlusRecordTypeBeginContainerNoParams,
    EmfPlusRecordTypeEndContainer,
    EmfPlusRecordTypeSetWorldTransform,
    EmfPlusRecordTypeResetWorldTransform,
    EmfPlusRecordTypeMultiplyWorldTransform,
    EmfPlusRecordTypeTranslateWorldTransform,
    EmfPlusRecordTypeScaleWorldTransform,
    EmfPlusRecordTypeRotateWorldTransform,
    EmfPlusRecordTypeSetPageTransform,
    EmfPlusRecordTypeResetClip,
    EmfPlusRecordTypeSetClipRect,
    EmfPlusRecordTypeSetClipPath,
    EmfPlusRecordTypeSetClipRegion,
    EmfPlusRecordTypeOffsetClip,

    EmfPlusRecordTypeDrawDriverString,

    EmfPlusRecordTotal,

    EmfPlusRecordTypeMax = EmfPlusRecordTotal-1,
    EmfPlusRecordTypeMin = EmfPlusRecordTypeHeader
  );
  TEmfPlusRecordType = EmfPlusRecordType;
{$ELSE}
type
  {.$EXTERNALSYM EmfPlusRecordType}
  EmfPlusRecordType = Integer;
  // Since we have to enumerate GDI records right along with GDI+ records,
  // We list all the GDI records here so that they can be part of the
  // same enumeration type which is used in the enumeration callback.
(*  const
    WmfRecordTypeSetBkColor              = (META_SETBKCOLOR or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeSetBkMode               = (META_SETBKMODE or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeSetMapMode              = (META_SETMAPMODE or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeSetROP2                 = (META_SETROP2 or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeSetRelAbs               = (META_SETRELABS or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeSetPolyFillMode         = (META_SETPOLYFILLMODE or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeSetStretchBltMode       = (META_SETSTRETCHBLTMODE or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeSetTextCharExtra        = (META_SETTEXTCHAREXTRA or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeSetTextColor            = (META_SETTEXTCOLOR or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeSetTextJustification    = (META_SETTEXTJUSTIFICATION or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeSetWindowOrg            = (META_SETWINDOWORG or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeSetWindowExt            = (META_SETWINDOWEXT or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeSetViewportOrg          = (META_SETVIEWPORTORG or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeSetViewportExt          = (META_SETVIEWPORTEXT or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeOffsetWindowOrg         = (META_OFFSETWINDOWORG or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeScaleWindowExt          = (META_SCALEWINDOWEXT or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeOffsetViewportOrg       = (META_OFFSETVIEWPORTORG or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeScaleViewportExt        = (META_SCALEVIEWPORTEXT or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeLineTo                  = (META_LINETO or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeMoveTo                  = (META_MOVETO or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeExcludeClipRect         = (META_EXCLUDECLIPRECT or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeIntersectClipRect       = (META_INTERSECTCLIPRECT or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeArc                     = (META_ARC or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeEllipse                 = (META_ELLIPSE or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeFloodFill               = (META_FLOODFILL or GDIP_WMF_RECORD_BASE);
    WmfRecordTypePie                     = (META_PIE or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeRectangle               = (META_RECTANGLE or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeRoundRect               = (META_ROUNDRECT or GDIP_WMF_RECORD_BASE);
    WmfRecordTypePatBlt                  = (META_PATBLT or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeSaveDC                  = (META_SAVEDC or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeSetPixel                = (META_SETPIXEL or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeOffsetClipRgn           = (META_OFFSETCLIPRGN or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeTextOut                 = (META_TEXTOUT or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeBitBlt                  = (META_BITBLT or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeStretchBlt              = (META_STRETCHBLT or GDIP_WMF_RECORD_BASE);
    WmfRecordTypePolygon                 = (META_POLYGON or GDIP_WMF_RECORD_BASE);
    WmfRecordTypePolyline                = (META_POLYLINE or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeEscape                  = (META_ESCAPE or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeRestoreDC               = (META_RESTOREDC or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeFillRegion              = (META_FILLREGION or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeFrameRegion             = (META_FRAMEREGION or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeInvertRegion            = (META_INVERTREGION or GDIP_WMF_RECORD_BASE);
    WmfRecordTypePaintRegion             = (META_PAINTREGION or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeSelectClipRegion        = (META_SELECTCLIPREGION or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeSelectObject            = (META_SELECTOBJECT or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeSetTextAlign            = (META_SETTEXTALIGN or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeDrawText                = ($062F or GDIP_WMF_RECORD_BASE);  // META_DRAWTEXT
    WmfRecordTypeChord                   = (META_CHORD or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeSetMapperFlags          = (META_SETMAPPERFLAGS or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeExtTextOut              = (META_EXTTEXTOUT or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeSetDIBToDev             = (META_SETDIBTODEV or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeSelectPalette           = (META_SELECTPALETTE or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeRealizePalette          = (META_REALIZEPALETTE or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeAnimatePalette          = (META_ANIMATEPALETTE or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeSetPalEntries           = (META_SETPALENTRIES or GDIP_WMF_RECORD_BASE);
    WmfRecordTypePolyPolygon             = (META_POLYPOLYGON or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeResizePalette           = (META_RESIZEPALETTE or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeDIBBitBlt               = (META_DIBBITBLT or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeDIBStretchBlt           = (META_DIBSTRETCHBLT or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeDIBCreatePatternBrush   = (META_DIBCREATEPATTERNBRUSH or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeStretchDIB              = (META_STRETCHDIB or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeExtFloodFill            = (META_EXTFLOODFILL or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeSetLayout               = ($0149 or GDIP_WMF_RECORD_BASE);  // META_SETLAYOUT
    WmfRecordTypeResetDC                 = ($014C or GDIP_WMF_RECORD_BASE);  // META_RESETDC
    WmfRecordTypeStartDoc                = ($014D or GDIP_WMF_RECORD_BASE);  // META_STARTDOC
    WmfRecordTypeStartPage               = ($004F or GDIP_WMF_RECORD_BASE);  // META_STARTPAGE
    WmfRecordTypeEndPage                 = ($0050 or GDIP_WMF_RECORD_BASE);  // META_ENDPAGE
    WmfRecordTypeAbortDoc                = ($0052 or GDIP_WMF_RECORD_BASE);  // META_ABORTDOC
    WmfRecordTypeEndDoc                  = ($005E or GDIP_WMF_RECORD_BASE);  // META_ENDDOC
    WmfRecordTypeDeleteObject            = (META_DELETEOBJECT or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeCreatePalette           = (META_CREATEPALETTE or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeCreateBrush             = ($00F8 or GDIP_WMF_RECORD_BASE);  // META_CREATEBRUSH
    WmfRecordTypeCreatePatternBrush      = (META_CREATEPATTERNBRUSH or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeCreatePenIndirect       = (META_CREATEPENINDIRECT or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeCreateFontIndirect      = (META_CREATEFONTINDIRECT or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeCreateBrushIndirect     = (META_CREATEBRUSHINDIRECT or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeCreateBitmapIndirect    = ($02FD or GDIP_WMF_RECORD_BASE);  // META_CREATEBITMAPINDIRECT
    WmfRecordTypeCreateBitmap            = ($06FE or GDIP_WMF_RECORD_BASE);  // META_CREATEBITMAP
    WmfRecordTypeCreateRegion            = (META_CREATEREGION or GDIP_WMF_RECORD_BASE);

    EmfRecordTypeHeader                  = EMR_HEADER;
    EmfRecordTypePolyBezier              = EMR_POLYBEZIER;
    EmfRecordTypePolygon                 = EMR_POLYGON;
    EmfRecordTypePolyline                = EMR_POLYLINE;
    EmfRecordTypePolyBezierTo            = EMR_POLYBEZIERTO;
    EmfRecordTypePolyLineTo              = EMR_POLYLINETO;
    EmfRecordTypePolyPolyline            = EMR_POLYPOLYLINE;
    EmfRecordTypePolyPolygon             = EMR_POLYPOLYGON;
    EmfRecordTypeSetWindowExtEx          = EMR_SETWINDOWEXTEX;
    EmfRecordTypeSetWindowOrgEx          = EMR_SETWINDOWORGEX;
    EmfRecordTypeSetViewportExtEx        = EMR_SETVIEWPORTEXTEX;
    EmfRecordTypeSetViewportOrgEx        = EMR_SETVIEWPORTORGEX;
    EmfRecordTypeSetBrushOrgEx           = EMR_SETBRUSHORGEX;
    EmfRecordTypeEOF                     = EMR_EOF;
    EmfRecordTypeSetPixelV               = EMR_SETPIXELV;
    EmfRecordTypeSetMapperFlags          = EMR_SETMAPPERFLAGS;
    EmfRecordTypeSetMapMode              = EMR_SETMAPMODE;
    EmfRecordTypeSetBkMode               = EMR_SETBKMODE;
    EmfRecordTypeSetPolyFillMode         = EMR_SETPOLYFILLMODE;
    EmfRecordTypeSetROP2                 = EMR_SETROP2;
    EmfRecordTypeSetStretchBltMode       = EMR_SETSTRETCHBLTMODE;
    EmfRecordTypeSetTextAlign            = EMR_SETTEXTALIGN;
    EmfRecordTypeSetColorAdjustment      = EMR_SETCOLORADJUSTMENT;
    EmfRecordTypeSetTextColor            = EMR_SETTEXTCOLOR;
    EmfRecordTypeSetBkColor              = EMR_SETBKCOLOR;
    EmfRecordTypeOffsetClipRgn           = EMR_OFFSETCLIPRGN;
    EmfRecordTypeMoveToEx                = EMR_MOVETOEX;
    EmfRecordTypeSetMetaRgn              = EMR_SETMETARGN;
    EmfRecordTypeExcludeClipRect         = EMR_EXCLUDECLIPRECT;
    EmfRecordTypeIntersectClipRect       = EMR_INTERSECTCLIPRECT;
    EmfRecordTypeScaleViewportExtEx      = EMR_SCALEVIEWPORTEXTEX;
    EmfRecordTypeScaleWindowExtEx        = EMR_SCALEWINDOWEXTEX;
    EmfRecordTypeSaveDC                  = EMR_SAVEDC;
    EmfRecordTypeRestoreDC               = EMR_RESTOREDC;
    EmfRecordTypeSetWorldTransform       = EMR_SETWORLDTRANSFORM;
    EmfRecordTypeModifyWorldTransform    = EMR_MODIFYWORLDTRANSFORM;
    EmfRecordTypeSelectObject            = EMR_SELECTOBJECT;
    EmfRecordTypeCreatePen               = EMR_CREATEPEN;
    EmfRecordTypeCreateBrushIndirect     = EMR_CREATEBRUSHINDIRECT;
    EmfRecordTypeDeleteObject            = EMR_DELETEOBJECT;
    EmfRecordTypeAngleArc                = EMR_ANGLEARC;
    EmfRecordTypeEllipse                 = EMR_ELLIPSE;
    EmfRecordTypeRectangle               = EMR_RECTANGLE;
    EmfRecordTypeRoundRect               = EMR_ROUNDRECT;
    EmfRecordTypeArc                     = EMR_ARC;
    EmfRecordTypeChord                   = EMR_CHORD;
    EmfRecordTypePie                     = EMR_PIE;
    EmfRecordTypeSelectPalette           = EMR_SELECTPALETTE;
    EmfRecordTypeCreatePalette           = EMR_CREATEPALETTE;
    EmfRecordTypeSetPaletteEntries       = EMR_SETPALETTEENTRIES;
    EmfRecordTypeResizePalette           = EMR_RESIZEPALETTE;
    EmfRecordTypeRealizePalette          = EMR_REALIZEPALETTE;
    EmfRecordTypeExtFloodFill            = EMR_EXTFLOODFILL;
    EmfRecordTypeLineTo                  = EMR_LINETO;
    EmfRecordTypeArcTo                   = EMR_ARCTO;
    EmfRecordTypePolyDraw                = EMR_POLYDRAW;
    EmfRecordTypeSetArcDirection         = EMR_SETARCDIRECTION;
    EmfRecordTypeSetMiterLimit           = EMR_SETMITERLIMIT;
    EmfRecordTypeBeginPath               = EMR_BEGINPATH;
    EmfRecordTypeEndPath                 = EMR_ENDPATH;
    EmfRecordTypeCloseFigure             = EMR_CLOSEFIGURE;
    EmfRecordTypeFillPath                = EMR_FILLPATH;
    EmfRecordTypeStrokeAndFillPath       = EMR_STROKEANDFILLPATH;
    EmfRecordTypeStrokePath              = EMR_STROKEPATH;
    EmfRecordTypeFlattenPath             = EMR_FLATTENPATH;
    EmfRecordTypeWidenPath               = EMR_WIDENPATH;
    EmfRecordTypeSelectClipPath          = EMR_SELECTCLIPPATH;
    EmfRecordTypeAbortPath               = EMR_ABORTPATH;
    EmfRecordTypeReserved_069            = 69;  // Not Used
    EmfRecordTypeGdiComment              = EMR_GDICOMMENT;
    EmfRecordTypeFillRgn                 = EMR_FILLRGN;
    EmfRecordTypeFrameRgn                = EMR_FRAMERGN;
    EmfRecordTypeInvertRgn               = EMR_INVERTRGN;
    EmfRecordTypePaintRgn                = EMR_PAINTRGN;
    EmfRecordTypeExtSelectClipRgn        = EMR_EXTSELECTCLIPRGN;
    EmfRecordTypeBitBlt                  = EMR_BITBLT;
    EmfRecordTypeStretchBlt              = EMR_STRETCHBLT;
    EmfRecordTypeMaskBlt                 = EMR_MASKBLT;
    EmfRecordTypePlgBlt                  = EMR_PLGBLT;
    EmfRecordTypeSetDIBitsToDevice       = EMR_SETDIBITSTODEVICE;
    EmfRecordTypeStretchDIBits           = EMR_STRETCHDIBITS;
    EmfRecordTypeExtCreateFontIndirect   = EMR_EXTCREATEFONTINDIRECTW;
    EmfRecordTypeExtTextOutA             = EMR_EXTTEXTOUTA;
    EmfRecordTypeExtTextOutW             = EMR_EXTTEXTOUTW;
    EmfRecordTypePolyBezier16            = EMR_POLYBEZIER16;
    EmfRecordTypePolygon16               = EMR_POLYGON16;
    EmfRecordTypePolyline16              = EMR_POLYLINE16;
    EmfRecordTypePolyBezierTo16          = EMR_POLYBEZIERTO16;
    EmfRecordTypePolylineTo16            = EMR_POLYLINETO16;
    EmfRecordTypePolyPolyline16          = EMR_POLYPOLYLINE16;
    EmfRecordTypePolyPolygon16           = EMR_POLYPOLYGON16;
    EmfRecordTypePolyDraw16              = EMR_POLYDRAW16;
    EmfRecordTypeCreateMonoBrush         = EMR_CREATEMONOBRUSH;
    EmfRecordTypeCreateDIBPatternBrushPt = EMR_CREATEDIBPATTERNBRUSHPT;
    EmfRecordTypeExtCreatePen            = EMR_EXTCREATEPEN;
    EmfRecordTypePolyTextOutA            = EMR_POLYTEXTOUTA;
    EmfRecordTypePolyTextOutW            = EMR_POLYTEXTOUTW;
    EmfRecordTypeSetICMMode              = 98;  // EMR_SETICMMODE,
    EmfRecordTypeCreateColorSpace        = 99;  // EMR_CREATECOLORSPACE,
    EmfRecordTypeSetColorSpace           = 100; // EMR_SETCOLORSPACE,
    EmfRecordTypeDeleteColorSpace        = 101; // EMR_DELETECOLORSPACE,
    EmfRecordTypeGLSRecord               = 102; // EMR_GLSRECORD,
    EmfRecordTypeGLSBoundedRecord        = 103; // EMR_GLSBOUNDEDRECORD,
    EmfRecordTypePixelFormat             = 104; // EMR_PIXELFORMAT,
    EmfRecordTypeDrawEscape              = 105; // EMR_RESERVED_105,
    EmfRecordTypeExtEscape               = 106; // EMR_RESERVED_106,
    EmfRecordTypeStartDoc                = 107; // EMR_RESERVED_107,
    EmfRecordTypeSmallTextOut            = 108; // EMR_RESERVED_108,
    EmfRecordTypeForceUFIMapping         = 109; // EMR_RESERVED_109,
    EmfRecordTypeNamedEscape             = 110; // EMR_RESERVED_110,
    EmfRecordTypeColorCorrectPalette     = 111; // EMR_COLORCORRECTPALETTE,
    EmfRecordTypeSetICMProfileA          = 112; // EMR_SETICMPROFILEA,
    EmfRecordTypeSetICMProfileW          = 113; // EMR_SETICMPROFILEW,
    EmfRecordTypeAlphaBlend              = 114; // EMR_ALPHABLEND,
    EmfRecordTypeSetLayout               = 115; // EMR_SETLAYOUT,
    EmfRecordTypeTransparentBlt          = 116; // EMR_TRANSPARENTBLT,
    EmfRecordTypeReserved_117            = 117; // Not Used
    EmfRecordTypeGradientFill            = 118; // EMR_GRADIENTFILL,
    EmfRecordTypeSetLinkedUFIs           = 119; // EMR_RESERVED_119,
    EmfRecordTypeSetTextJustification    = 120; // EMR_RESERVED_120,
    EmfRecordTypeColorMatchToTargetW     = 121; // EMR_COLORMATCHTOTARGETW,
    EmfRecordTypeCreateColorSpaceW       = 122; // EMR_CREATECOLORSPACEW,
    EmfRecordTypeMax                     = 122;
    EmfRecordTypeMin                     = 1;

    // That is the END of the GDI EMF records.

    // Now we start the list of EMF+ records.  We leave quite
    // a bit of room here for the addition of any new GDI
    // records that may be added later.

    EmfPlusRecordTypeInvalid   = GDIP_EMFPLUS_RECORD_BASE;
    EmfPlusRecordTypeHeader    = GDIP_EMFPLUS_RECORD_BASE + 1;
    EmfPlusRecordTypeEndOfFile = GDIP_EMFPLUS_RECORD_BASE + 2;

    EmfPlusRecordTypeComment   = GDIP_EMFPLUS_RECORD_BASE + 3;

    EmfPlusRecordTypeGetDC     = GDIP_EMFPLUS_RECORD_BASE + 4;

    EmfPlusRecordTypeMultiFormatStart   = GDIP_EMFPLUS_RECORD_BASE + 5;
    EmfPlusRecordTypeMultiFormatSection = GDIP_EMFPLUS_RECORD_BASE + 6;
    EmfPlusRecordTypeMultiFormatEnd     = GDIP_EMFPLUS_RECORD_BASE + 7;

    // For all persistent objects

    EmfPlusRecordTypeObject = GDIP_EMFPLUS_RECORD_BASE + 8;

    // Drawing Records

    EmfPlusRecordTypeClear           = GDIP_EMFPLUS_RECORD_BASE + 9;
    EmfPlusRecordTypeFillRects       = GDIP_EMFPLUS_RECORD_BASE + 10;
    EmfPlusRecordTypeDrawRects       = GDIP_EMFPLUS_RECORD_BASE + 11;
    EmfPlusRecordTypeFillPolygon     = GDIP_EMFPLUS_RECORD_BASE + 12;
    EmfPlusRecordTypeDrawLines       = GDIP_EMFPLUS_RECORD_BASE + 13;
    EmfPlusRecordTypeFillEllipse     = GDIP_EMFPLUS_RECORD_BASE + 14;
    EmfPlusRecordTypeDrawEllipse     = GDIP_EMFPLUS_RECORD_BASE + 15;
    EmfPlusRecordTypeFillPie         = GDIP_EMFPLUS_RECORD_BASE + 16;
    EmfPlusRecordTypeDrawPie         = GDIP_EMFPLUS_RECORD_BASE + 17;
    EmfPlusRecordTypeDrawArc         = GDIP_EMFPLUS_RECORD_BASE + 18;
    EmfPlusRecordTypeFillRegion      = GDIP_EMFPLUS_RECORD_BASE + 19;
    EmfPlusRecordTypeFillPath        = GDIP_EMFPLUS_RECORD_BASE + 20;
    EmfPlusRecordTypeDrawPath        = GDIP_EMFPLUS_RECORD_BASE + 21;
    EmfPlusRecordTypeFillClosedCurve = GDIP_EMFPLUS_RECORD_BASE + 22;
    EmfPlusRecordTypeDrawClosedCurve = GDIP_EMFPLUS_RECORD_BASE + 23;
    EmfPlusRecordTypeDrawCurve       = GDIP_EMFPLUS_RECORD_BASE + 24;
    EmfPlusRecordTypeDrawBeziers     = GDIP_EMFPLUS_RECORD_BASE + 25;
    EmfPlusRecordTypeDrawImage       = GDIP_EMFPLUS_RECORD_BASE + 26;
    EmfPlusRecordTypeDrawImagePoints = GDIP_EMFPLUS_RECORD_BASE + 27;
    EmfPlusRecordTypeDrawString      = GDIP_EMFPLUS_RECORD_BASE + 28;

    // Graphics State Records

    EmfPlusRecordTypeSetRenderingOrigin      = GDIP_EMFPLUS_RECORD_BASE + 29;
    EmfPlusRecordTypeSetAntiAliasMode        = GDIP_EMFPLUS_RECORD_BASE + 30;
    EmfPlusRecordTypeSetTextRenderingHint    = GDIP_EMFPLUS_RECORD_BASE + 31;
    EmfPlusRecordTypeSetTextContrast         = GDIP_EMFPLUS_RECORD_BASE + 32;
    EmfPlusRecordTypeSetInterpolationMode    = GDIP_EMFPLUS_RECORD_BASE + 33;
    EmfPlusRecordTypeSetPixelOffsetMode      = GDIP_EMFPLUS_RECORD_BASE + 34;
    EmfPlusRecordTypeSetCompositingMode      = GDIP_EMFPLUS_RECORD_BASE + 35;
    EmfPlusRecordTypeSetCompositingQuality   = GDIP_EMFPLUS_RECORD_BASE + 36;
    EmfPlusRecordTypeSave                    = GDIP_EMFPLUS_RECORD_BASE + 37;
    EmfPlusRecordTypeRestore                 = GDIP_EMFPLUS_RECORD_BASE + 38;
    EmfPlusRecordTypeBeginContainer          = GDIP_EMFPLUS_RECORD_BASE + 39;
    EmfPlusRecordTypeBeginContainerNoParams  = GDIP_EMFPLUS_RECORD_BASE + 40;
    EmfPlusRecordTypeEndContainer            = GDIP_EMFPLUS_RECORD_BASE + 41;
    EmfPlusRecordTypeSetWorldTransform       = GDIP_EMFPLUS_RECORD_BASE + 42;
    EmfPlusRecordTypeResetWorldTransform     = GDIP_EMFPLUS_RECORD_BASE + 43;
    EmfPlusRecordTypeMultiplyWorldTransform  = GDIP_EMFPLUS_RECORD_BASE + 44;
    EmfPlusRecordTypeTranslateWorldTransform = GDIP_EMFPLUS_RECORD_BASE + 45;
    EmfPlusRecordTypeScaleWorldTransform     = GDIP_EMFPLUS_RECORD_BASE + 46;
    EmfPlusRecordTypeRotateWorldTransform    = GDIP_EMFPLUS_RECORD_BASE + 47;
    EmfPlusRecordTypeSetPageTransform        = GDIP_EMFPLUS_RECORD_BASE + 48;
    EmfPlusRecordTypeResetClip               = GDIP_EMFPLUS_RECORD_BASE + 49;
    EmfPlusRecordTypeSetClipRect             = GDIP_EMFPLUS_RECORD_BASE + 50;
    EmfPlusRecordTypeSetClipPath             = GDIP_EMFPLUS_RECORD_BASE + 51;
    EmfPlusRecordTypeSetClipRegion           = GDIP_EMFPLUS_RECORD_BASE + 52;
    EmfPlusRecordTypeOffsetClip              = GDIP_EMFPLUS_RECORD_BASE + 53;

    EmfPlusRecordTypeDrawDriverString        = GDIP_EMFPLUS_RECORD_BASE + 54;

    EmfPlusRecordTotal                       = GDIP_EMFPLUS_RECORD_BASE + 55;

    EmfPlusRecordTypeMax = EmfPlusRecordTotal-1;
    EmfPlusRecordTypeMin = EmfPlusRecordTypeHeader;*)

type
  TEmfPlusRecordType = EmfPlusRecordType;
{$ENDIF}
//---------------------------------------------------------------------------
// StringFormatFlags
//---------------------------------------------------------------------------

//---------------------------------------------------------------------------
// String format flags
//
//  DirectionRightToLeft          - For horizontal text, the reading order is
//                                  right to left. This value is called
//                                  the base embedding level by the Unicode
//                                  bidirectional engine.
//                                  For vertical text, columns are read from
//                                  right to left.
//                                  By default, horizontal or vertical text is
//                                  read from left to right.
//
//  DirectionVertical             - Individual lines of text are vertical. In
//                                  each line, characters progress from top to
//                                  bottom.
//                                  By default, lines of text are horizontal,
//                                  each new line below the previous line.
//
//  NoFitBlackBox                 - Allows parts of glyphs to overhang the
//                                  bounding rectangle.
//                                  By default glyphs are first aligned
//                                  inside the margines, then any glyphs which
//                                  still overhang the bounding box are
//                                  repositioned to avoid any overhang.
//                                  For example when an italic
//                                  lower case letter f in a font such as
//                                  Garamond is aligned at the far left of a
//                                  rectangle, the lower part of the f will
//                                  reach slightly further left than the left
//                                  edge of the rectangle. Setting this flag
//                                  will ensure the character aligns visually
//                                  with the lines above and below, but may
//                                  cause some pixels outside the formatting
//                                  rectangle to be clipped or painted.
//
//  DisplayFormatControl          - Causes control characters such as the
//                                  left-to-right mark to be shown in the
//                                  output with a representative glyph.
//
//  NoFontFallback                - Disables fallback to alternate fonts for
//                                  characters not supported in the requested
//                                  font. Any missing characters will be
//                                  be displayed with the fonts missing glyph,
//                                  usually an open square.
//
//  NoWrap                        - Disables wrapping of text between lines
//                                  when formatting within a rectangle.
//                                  NoWrap is implied when a point is passed
//                                  instead of a rectangle, or when the
//                                  specified rectangle has a zero line length.
//
//  NoClip                        - By default text is clipped to the
//                                  formatting rectangle. Setting NoClip
//                                  allows overhanging pixels to affect the
//                                  device outside the formatting rectangle.
//                                  Pixels at the end of the line may be
//                                  affected if the glyphs overhang their
//                                  cells, and either the NoFitBlackBox flag
//                                  has been set, or the glyph extends to far
//                                  to be fitted.
//                                  Pixels above/before the first line or
//                                  below/after the last line may be affected
//                                  if the glyphs extend beyond their cell
//                                  ascent / descent. This can occur rarely
//                                  with unusual diacritic mark combinations.

//---------------------------------------------------------------------------

  {.$EXTERNALSYM StringFormatFlags}
  StringFormatFlags = Integer;
  const
    StringFormatFlagsDirectionRightToLeft        = $00000001;
    StringFormatFlagsDirectionVertical           = $00000002;
    StringFormatFlagsNoFitBlackBox               = $00000004;
    StringFormatFlagsDisplayFormatControl        = $00000020;
    StringFormatFlagsNoFontFallback              = $00000400;
    StringFormatFlagsMeasureTrailingSpaces       = $00000800;
    StringFormatFlagsNoWrap                      = $00001000;
    StringFormatFlagsLineLimit                   = $00002000;

    StringFormatFlagsNoClip                      = $00004000;

Type
  TStringFormatFlags = StringFormatFlags;

//---------------------------------------------------------------------------
// StringTrimming
//---------------------------------------------------------------------------

  {.$EXTERNALSYM StringTrimming}
  StringTrimming = (
    StringTrimmingNone,
    StringTrimmingCharacter,
    StringTrimmingWord,
    StringTrimmingEllipsisCharacter,
    StringTrimmingEllipsisWord,
    StringTrimmingEllipsisPath
  );
  TStringTrimming = StringTrimming;

//---------------------------------------------------------------------------
// National language digit substitution
//---------------------------------------------------------------------------

  {.$EXTERNALSYM StringDigitSubstitute}
  StringDigitSubstitute = (
    StringDigitSubstituteUser,          // As NLS setting
    StringDigitSubstituteNone,
    StringDigitSubstituteNational,
    StringDigitSubstituteTraditional
  );
  TStringDigitSubstitute = StringDigitSubstitute;
  PStringDigitSubstitute = ^TStringDigitSubstitute;

//---------------------------------------------------------------------------
// Hotkey prefix interpretation
//---------------------------------------------------------------------------

  {.$EXTERNALSYM HotkeyPrefix}
  HotkeyPrefix = (
    HotkeyPrefixNone,
    HotkeyPrefixShow,
    HotkeyPrefixHide
  );
  THotkeyPrefix = HotkeyPrefix;

//---------------------------------------------------------------------------
// String alignment flags
//---------------------------------------------------------------------------

  {.$EXTERNALSYM StringAlignment}
  StringAlignment = (
    // Left edge for left-to-right text,
    // right for right-to-left text,
    // and top for vertical
    StringAlignmentNear,
    StringAlignmentCenter,
    StringAlignmentFar
  );
  TStringAlignment = StringAlignment;

//---------------------------------------------------------------------------
// DriverStringOptions
//---------------------------------------------------------------------------

  {.$EXTERNALSYM DriverStringOptions}
  DriverStringOptions = Integer;
  const
    DriverStringOptionsCmapLookup             = 1;
    DriverStringOptionsVertical               = 2;
    DriverStringOptionsRealizedAdvance        = 4;
    DriverStringOptionsLimitSubpixel          = 8;

type
  TDriverStringOptions = DriverStringOptions;

//---------------------------------------------------------------------------
// Flush Intention flags
//---------------------------------------------------------------------------

  {.$EXTERNALSYM FlushIntention}
  FlushIntention = (
    FlushIntentionFlush,  // Flush all batched rendering operations
    FlushIntentionSync    // Flush all batched rendering operations
                          // and wait for them to complete
  );
  TFlushIntention = FlushIntention;

//---------------------------------------------------------------------------
// Image encoder parameter related types
//---------------------------------------------------------------------------

  {.$EXTERNALSYM EncoderParameterValueType}
  EncoderParameterValueType = Integer;
  const
    EncoderParameterValueTypeByte          : Integer = 1;    // 8-bit unsigned int
    EncoderParameterValueTypeASCII         : Integer = 2;    // 8-bit byte containing one 7-bit ASCII
                                                             // code. NULL terminated.
    EncoderParameterValueTypeShort         : Integer = 3;    // 16-bit unsigned int
    EncoderParameterValueTypeLong          : Integer = 4;    // 32-bit unsigned int
    EncoderParameterValueTypeRational      : Integer = 5;    // Two Longs. The first Long is the
                                                             // numerator, the second Long expresses the
                                                             // denomintor.
    EncoderParameterValueTypeLongRange     : Integer = 6;    // Two longs which specify a range of
                                                             // integer values. The first Long specifies
                                                             // the lower end and the second one
                                                             // specifies the higher end. All values
                                                             // are inclusive at both ends
    EncoderParameterValueTypeUndefined     : Integer = 7;    // 8-bit byte that can take any value
                                                             // depending on field definition
    EncoderParameterValueTypeRationalRange : Integer = 8;    // Two Rationals. The first Rational
                                                             // specifies the lower end and the second
                                                             // specifies the higher end. All values
                                                             // are inclusive at both ends
type
  TEncoderParameterValueType = EncoderParameterValueType;

//---------------------------------------------------------------------------
// Image encoder value types
//---------------------------------------------------------------------------

  {.$EXTERNALSYM EncoderValue}
  EncoderValue = (
    EncoderValueColorTypeCMYK,
    EncoderValueColorTypeYCCK,
    EncoderValueCompressionLZW,
    EncoderValueCompressionCCITT3,
    EncoderValueCompressionCCITT4,
    EncoderValueCompressionRle,
    EncoderValueCompressionNone,
    EncoderValueScanMethodInterlaced,
    EncoderValueScanMethodNonInterlaced,
    EncoderValueVersionGif87,
    EncoderValueVersionGif89,
    EncoderValueRenderProgressive,
    EncoderValueRenderNonProgressive,
    EncoderValueTransformRotate90,
    EncoderValueTransformRotate180,
    EncoderValueTransformRotate270,
    EncoderValueTransformFlipHorizontal,
    EncoderValueTransformFlipVertical,
    EncoderValueMultiFrame,
    EncoderValueLastFrame,
    EncoderValueFlush,
    EncoderValueFrameDimensionTime,
    EncoderValueFrameDimensionResolution,
    EncoderValueFrameDimensionPage
  );
  TEncoderValue = EncoderValue;

//---------------------------------------------------------------------------
// Conversion of Emf To WMF Bits flags
//---------------------------------------------------------------------------
{$IFDEF DELPHI6_UP}
  {.$EXTERNALSYM EmfToWmfBitsFlags}
  EmfToWmfBitsFlags = (
    EmfToWmfBitsFlagsDefault          = $00000000,
    EmfToWmfBitsFlagsEmbedEmf         = $00000001,
    EmfToWmfBitsFlagsIncludePlaceable = $00000002,
    EmfToWmfBitsFlagsNoXORClip        = $00000004
  );
  TEmfToWmfBitsFlags = EmfToWmfBitsFlags;
{$ELSE}
  {.$EXTERNALSYM EmfToWmfBitsFlags}
  EmfToWmfBitsFlags = Integer;
  const
    EmfToWmfBitsFlagsDefault          = $00000000;
    EmfToWmfBitsFlagsEmbedEmf         = $00000001;
    EmfToWmfBitsFlagsIncludePlaceable = $00000002;
    EmfToWmfBitsFlagsNoXORClip        = $00000004;
    
type
  TEmfToWmfBitsFlags = EmfToWmfBitsFlags;
{$ENDIF}

//--------------------------------------------------------------------------
// Callback functions
//--------------------------------------------------------------------------

  {.$EXTERNALSYM ImageAbort}
  ImageAbort = function: BOOL; stdcall;
  {.$EXTERNALSYM DrawImageAbort}
  DrawImageAbort         = ImageAbort;
  {.$EXTERNALSYM GetThumbnailImageAbort}
  GetThumbnailImageAbort = ImageAbort;


  // Callback for EnumerateMetafile methods.  The parameters are:

  //      recordType      WMF, EMF, or EMF+ record type
  //      flags           (always 0 for WMF/EMF records)
  //      dataSize        size of the record data (in bytes), or 0 if no data
  //      data            pointer to the record data, or NULL if no data
  //      callbackData    pointer to callbackData, if any

  // This method can then call Metafile::PlayRecord to play the
  // record that was just enumerated.  If this method  returns
  // FALSE, the enumeration process is aborted.  Otherwise, it continues.

  {.$EXTERNALSYM EnumerateMetafileProc}
  EnumerateMetafileProc = function(recordType: EmfPlusRecordType; flags: UINT;
    dataSize: UINT; data: PBYTE; callbackData: pointer): BOOL; stdcall;

//--------------------------------------------------------------------------
// Primitive data types
//
// NOTE:
//  Types already defined in standard header files:
//      INT8
//      UINT8
//      INT16
//      UINT16
//      INT32
//      UINT32
//      INT64
//      UINT64
//
//  Avoid using the following types:
//      LONG - use INT
//      ULONG - use UINT
//      DWORD - use UINT32
//--------------------------------------------------------------------------

const
  { from float.h }
  FLT_MAX =  3.402823466e+38; // max value
  FLT_MIN =  1.175494351e-38; // min positive value

  REAL_MAX           = FLT_MAX;
  {.$EXTERNALSYM REAL_MAX}
  REAL_MIN           = FLT_MIN;
  {.$EXTERNALSYM REAL_MIN}
  REAL_TOLERANCE     = (FLT_MIN * 100);
  {.$EXTERNALSYM REAL_TOLERANCE}
  REAL_EPSILON       = 1.192092896e-07;        // FLT_EPSILON
  {.$EXTERNALSYM REAL_EPSILON}

//--------------------------------------------------------------------------
// Status return values from GDI+ methods
//--------------------------------------------------------------------------
type
  {.$EXTERNALSYM Status}
  Status = (
    Ok,
    GenericError,
    InvalidParameter,
    OutOfMemory,
    ObjectBusy,
    InsufficientBuffer,
    NotImplemented,
    Win32Error,
    WrongState,
    Aborted,
    FileNotFound,
    ValueOverflow,
    AccessDenied,
    UnknownImageFormat,
    FontFamilyNotFound,
    FontStyleNotFound,
    NotTrueTypeFont,
    UnsupportedGdiplusVersion,
    GdiplusNotInitialized,
    PropertyNotFound,
    PropertyNotSupported
  );
  TStatus = Status;

//--------------------------------------------------------------------------
// Represents a dimension in a 2D coordinate system (floating-point coordinates)
//--------------------------------------------------------------------------

type
  PGPSizeF = ^TGPSizeF;
  TGPSizeF = packed record
    Width  : Single;
    Height : Single;
  end;

//--------------------------------------------------------------------------
// Represents a dimension in a 2D coordinate system (integer coordinates)
//--------------------------------------------------------------------------

type
  PGPSize = ^TGPSize;
  TGPSize = packed record
    Width  : Integer;
    Height : Integer;
  end;

//--------------------------------------------------------------------------
// Represents a location in a 2D coordinate system (floating-point coordinates)
//--------------------------------------------------------------------------

type
  PGPPointF = ^TGPPointF;
  TGPPointF = packed record
    X : Single;
    Y : Single;
  end;
  TPointFDynArray = array of TGPPointF;

//--------------------------------------------------------------------------
// Represents a location in a 2D coordinate system (integer coordinates)
//--------------------------------------------------------------------------

type
  PGPPoint = ^TGPPoint;
  TGPPoint = packed record
    X : Integer;
    Y : Integer;
  end;
  TPointDynArray = array of TGPPoint;

//--------------------------------------------------------------------------
// Represents a rectangle in a 2D coordinate system (floating-point coordinates)
//--------------------------------------------------------------------------

type
  PGPRectF = ^TGPRectF;
  TGPRectF = packed record
    X     : Single;
    Y     : Single;
    Width : Single;
    Height: Single;
  end;
  TRectFDynArray = array of TGPRectF;

type
  PGPRect = ^TGPRect;
  TGPRect = packed record
    X     : Integer;
    Y     : Integer;
    Width : Integer;
    Height: Integer;
  end;
  TRectDynArray = array of TGPRect;

type
  TPathData = packed class
  public
    Count  : Integer;
    Points : PGPPointF;
    Types  : PBYTE;
    constructor Create;
    destructor destroy; override;
  end;

  PCharacterRange = ^TCharacterRange;
  TCharacterRange = packed record
    First  : Integer;
    Length : Integer;
  end;

(**************************************************************************
*
*   GDI+ Startup and Shutdown APIs
*
**************************************************************************)
type
  {.$EXTERNALSYM DebugEventLevel}
  DebugEventLevel = (
    DebugEventLevelFatal,
    DebugEventLevelWarning
  );
  TDebugEventLevel = DebugEventLevel;

  // Callback function that GDI+ can call, on debug builds, for assertions
  // and warnings.

  {.$EXTERNALSYM DebugEventProc}
  DebugEventProc = procedure(level: DebugEventLevel; message: PChar); stdcall;

  // Notification functions which the user must call appropriately if
  // "SuppressBackgroundThread" (below) is set.

  {.$EXTERNALSYM NotificationHookProc}
  NotificationHookProc = function(out token: ULONG): Status; stdcall;
  {.$EXTERNALSYM NotificationUnhookProc}
  NotificationUnhookProc = procedure(token: ULONG); stdcall;

  // Input structure for GdiplusStartup

  {.$EXTERNALSYM GdiplusStartupInput}
  GdiplusStartupInput = packed record
    GdiplusVersion          : Cardinal;       // Must be 1
    DebugEventCallback      : DebugEventProc; // Ignored on free builds
    SuppressBackgroundThread: BOOL;           // FALSE unless you're prepared to call
                                              // the hook/unhook functions properly
    SuppressExternalCodecs  : BOOL;           // FALSE unless you want GDI+ only to use
  end;                                        // its internal image codecs.
  TGdiplusStartupInput = GdiplusStartupInput;
  PGdiplusStartupInput = ^TGdiplusStartupInput;

  // Output structure for GdiplusStartup()

  {.$EXTERNALSYM GdiplusStartupOutput}
  GdiplusStartupOutput = packed record
    // The following 2 fields are NULL if SuppressBackgroundThread is FALSE.
    // Otherwise, they are functions which must be called appropriately to
    // replace the background thread.
    //
    // These should be called on the application's main message loop - i.e.
    // a message loop which is active for the lifetime of GDI+.
    // "NotificationHook" should be called before starting the loop,
    // and "NotificationUnhook" should be called after the loop ends.

    NotificationHook  : NotificationHookProc;
    NotificationUnhook: NotificationUnhookProc;
  end;
  TGdiplusStartupOutput = GdiplusStartupOutput;
  PGdiplusStartupOutput = ^TGdiplusStartupOutput;

  // GDI+ initialization. Must not be called from DllMain - can cause deadlock.
  //
  // Must be called before GDI+ API's or constructors are used.
  //
  // token  - may not be NULL - accepts a token to be passed in the corresponding
  //          GdiplusShutdown call.
  // input  - may not be NULL
  // output - may be NULL only if input->SuppressBackgroundThread is FALSE.

var
  {.$EXTERNALSYM GdiplusStartup}
GdiplusStartup: function(out token: ULONG; input: PGdiplusStartupInput;
   output: PGdiplusStartupOutput): Status; stdcall;

  // GDI+ termination. Must be called before GDI+ is unloaded.
  // Must not be called from DllMain - can cause deadlock.
  //
  // GDI+ API's may not be called after GdiplusShutdown. Pay careful attention
  // to GDI+ object destructors.

  {.$EXTERNALSYM GdiplusShutdown}
GdiplusShutdown: procedure(token: ULONG); stdcall;  


type
  PARGB  = ^ARGB;
  ARGB   = DWORD;
  {.$EXTERNALSYM ARGB}
  ARGB64 = Int64;
  {.$EXTERNALSYM ARGB64}

const
  ALPHA_SHIFT = 24;
  {.$EXTERNALSYM ALPHA_SHIFT}
  RED_SHIFT   = 16;
  {.$EXTERNALSYM RED_SHIFT}
  GREEN_SHIFT = 8;
  {.$EXTERNALSYM GREEN_SHIFT}
  BLUE_SHIFT  = 0;
  {.$EXTERNALSYM BLUE_SHIFT}
  ALPHA_MASK  = (ARGB($ff) shl ALPHA_SHIFT);
  {.$EXTERNALSYM ALPHA_MASK}

  // In-memory pixel data formats:
  // bits 0-7 = format index
  // bits 8-15 = pixel size (in bits)
  // bits 16-23 = flags
  // bits 24-31 = reserved

type
  PixelFormat = Integer;
  {.$EXTERNALSYM PixelFormat}
  TPixelFormat = PixelFormat;

const
  PixelFormatIndexed     = $00010000; // Indexes into a palette
  {.$EXTERNALSYM PixelFormatIndexed}
  PixelFormatGDI         = $00020000; // Is a GDI-supported format
  {.$EXTERNALSYM PixelFormatGDI}
  PixelFormatAlpha       = $00040000; // Has an alpha component
  {.$EXTERNALSYM PixelFormatAlpha}
  PixelFormatPAlpha      = $00080000; // Pre-multiplied alpha
  {.$EXTERNALSYM PixelFormatPAlpha}
  PixelFormatExtended    = $00100000; // Extended color 16 bits/channel
  {.$EXTERNALSYM PixelFormatExtended}
  PixelFormatCanonical   = $00200000;
  {.$EXTERNALSYM PixelFormatCanonical}

  PixelFormatUndefined      = 0;
  {.$EXTERNALSYM PixelFormatUndefined}
  PixelFormatDontCare       = 0;
  {.$EXTERNALSYM PixelFormatDontCare}

  PixelFormat1bppIndexed    = (1  or ( 1 shl 8) or PixelFormatIndexed or PixelFormatGDI);
  {.$EXTERNALSYM PixelFormat1bppIndexed}
  PixelFormat4bppIndexed    = (2  or ( 4 shl 8) or PixelFormatIndexed or PixelFormatGDI);
  {.$EXTERNALSYM PixelFormat4bppIndexed}
  PixelFormat8bppIndexed    = (3  or ( 8 shl 8) or PixelFormatIndexed or PixelFormatGDI);
  {.$EXTERNALSYM PixelFormat8bppIndexed}
  PixelFormat16bppGrayScale = (4  or (16 shl 8) or PixelFormatExtended);
  {.$EXTERNALSYM PixelFormat16bppGrayScale}
  PixelFormat16bppRGB555    = (5  or (16 shl 8) or PixelFormatGDI);
  {.$EXTERNALSYM PixelFormat16bppRGB555}
  PixelFormat16bppRGB565    = (6  or (16 shl 8) or PixelFormatGDI);
  {.$EXTERNALSYM PixelFormat16bppRGB565}
  PixelFormat16bppARGB1555  = (7  or (16 shl 8) or PixelFormatAlpha or PixelFormatGDI);
  {.$EXTERNALSYM PixelFormat16bppARGB1555}
  PixelFormat24bppRGB       = (8  or (24 shl 8) or PixelFormatGDI);
  {.$EXTERNALSYM PixelFormat24bppRGB}
  PixelFormat32bppRGB       = (9  or (32 shl 8) or PixelFormatGDI);
  {.$EXTERNALSYM PixelFormat32bppRGB}
  PixelFormat32bppARGB      = (10 or (32 shl 8) or PixelFormatAlpha or PixelFormatGDI or PixelFormatCanonical);
  {.$EXTERNALSYM PixelFormat32bppARGB}
  PixelFormat32bppPARGB     = (11 or (32 shl 8) or PixelFormatAlpha or PixelFormatPAlpha or PixelFormatGDI);
  {.$EXTERNALSYM PixelFormat32bppPARGB}
  PixelFormat48bppRGB       = (12 or (48 shl 8) or PixelFormatExtended);
  {.$EXTERNALSYM PixelFormat48bppRGB}
  PixelFormat64bppARGB      = (13 or (64 shl 8) or PixelFormatAlpha  or PixelFormatCanonical or PixelFormatExtended);
  {.$EXTERNALSYM PixelFormat64bppARGB}
  PixelFormat64bppPARGB     = (14 or (64 shl 8) or PixelFormatAlpha  or PixelFormatPAlpha or PixelFormatExtended);
  {.$EXTERNALSYM PixelFormat64bppPARGB}
  PixelFormatMax            = 15;
  {.$EXTERNALSYM PixelFormatMax}

//--------------------------------------------------------------------------
// Determine if the Pixel Format is Canonical format:
//   PixelFormat32bppARGB
//   PixelFormat32bppPARGB
//   PixelFormat64bppARGB
//   PixelFormat64bppPARGB
//--------------------------------------------------------------------------

{$IFDEF DELPHI6_UP}
type
  {.$EXTERNALSYM PaletteFlags}
  PaletteFlags = (
    PaletteFlagsHasAlpha    = $0001,
    PaletteFlagsGrayScale   = $0002,
    PaletteFlagsHalftone    = $0004
  );
  TPaletteFlags = PaletteFlags;
{$ELSE}
type
  {.$EXTERNALSYM PaletteFlags}
  PaletteFlags = Integer;
  const
    PaletteFlagsHasAlpha    = $0001;
    PaletteFlagsGrayScale   = $0002;
    PaletteFlagsHalftone    = $0004;

type
  TPaletteFlags = PaletteFlags;
{$ENDIF}

  {.$EXTERNALSYM ColorPalette}
  ColorPalette = packed record
    Flags  : UINT ;                 // Palette flags
    Count  : UINT ;                 // Number of color entries
    Entries: array [0..0] of ARGB ; // Palette color entries
  end;

  TColorPalette = ColorPalette;
  PColorPalette = ^TColorPalette;

//----------------------------------------------------------------------------
// Color mode
//----------------------------------------------------------------------------

  {.$EXTERNALSYM ColorMode}
  ColorMode = (
    ColorModeARGB32,
    ColorModeARGB64
  );
  TColorMode = ColorMode;

//----------------------------------------------------------------------------
// Color Channel flags 
//----------------------------------------------------------------------------

  {.$EXTERNALSYM ColorChannelFlags}
  ColorChannelFlags = (
    ColorChannelFlagsC,
    ColorChannelFlagsM,
    ColorChannelFlagsY,
    ColorChannelFlagsK,
    ColorChannelFlagsLast
  );
  TColorChannelFlags = ColorChannelFlags;

//----------------------------------------------------------------------------
// Color
//----------------------------------------------------------------------------

  // Common color constants
const
  aclAliceBlue            = $FFF0F8FF;
  aclAntiqueWhite         = $FFFAEBD7;
  aclAqua                 = $FF00FFFF;
  aclAquamarine           = $FF7FFFD4;
  aclAzure                = $FFF0FFFF;
  aclBeige                = $FFF5F5DC;
  aclBisque               = $FFFFE4C4;
  aclBlack                = $FF000000;
  aclBlanchedAlmond       = $FFFFEBCD;
  aclBlue                 = $FF0000FF;
  aclBlueViolet           = $FF8A2BE2;
  aclBrown                = $FFA52A2A;
  aclBurlyWood            = $FFDEB887;
  aclCadetBlue            = $FF5F9EA0;
  aclChartreuse           = $FF7FFF00;
  aclChocolate            = $FFD2691E;
  aclCoral                = $FFFF7F50;
  aclCornflowerBlue       = $FF6495ED;
  aclCornsilk             = $FFFFF8DC;
  aclCrimson              = $FFDC143C;
  aclCyan                 = $FF00FFFF;
  aclDarkBlue             = $FF00008B;
  aclDarkCyan             = $FF008B8B;
  aclDarkGoldenrod        = $FFB8860B;
  aclDarkGray             = $FFA9A9A9;
  aclDarkGreen            = $FF006400;
  aclDarkKhaki            = $FFBDB76B;
  aclDarkMagenta          = $FF8B008B;
  aclDarkOliveGreen       = $FF556B2F;
  aclDarkOrange           = $FFFF8C00;
  aclDarkOrchid           = $FF9932CC;
  aclDarkRed              = $FF8B0000;
  aclDarkSalmon           = $FFE9967A;
  aclDarkSeaGreen         = $FF8FBC8B;
  aclDarkSlateBlue        = $FF483D8B;
  aclDarkSlateGray        = $FF2F4F4F;
  aclDarkTurquoise        = $FF00CED1;
  aclDarkViolet           = $FF9400D3;
  aclDeepPink             = $FFFF1493;
  aclDeepSkyBlue          = $FF00BFFF;
  aclDimGray              = $FF696969;
  aclDodgerBlue           = $FF1E90FF;
  aclFirebrick            = $FFB22222;
  aclFloralWhite          = $FFFFFAF0;
  aclForestGreen          = $FF228B22;
  aclFuchsia              = $FFFF00FF;
  aclGainsboro            = $FFDCDCDC;
  aclGhostWhite           = $FFF8F8FF;
  aclGold                 = $FFFFD700;
  aclGoldenrod            = $FFDAA520;
  aclGray                 = $FF808080;
  aclGreen                = $FF008000;
  aclGreenYellow          = $FFADFF2F;
  aclHoneydew             = $FFF0FFF0;
  aclHotPink              = $FFFF69B4;
  aclIndianRed            = $FFCD5C5C;
  aclIndigo               = $FF4B0082;
  aclIvory                = $FFFFFFF0;
  aclKhaki                = $FFF0E68C;
  aclLavender             = $FFE6E6FA;
  aclLavenderBlush        = $FFFFF0F5;
  aclLawnGreen            = $FF7CFC00;
  aclLemonChiffon         = $FFFFFACD;
  aclLightBlue            = $FFADD8E6;
  aclLightCoral           = $FFF08080;
  aclLightCyan            = $FFE0FFFF;
  aclLightGoldenrodYellow = $FFFAFAD2;
  aclLightGray            = $FFD3D3D3;
  aclLightGreen           = $FF90EE90;
  aclLightPink            = $FFFFB6C1;
  aclLightSalmon          = $FFFFA07A;
  aclLightSeaGreen        = $FF20B2AA;
  aclLightSkyBlue         = $FF87CEFA;
  aclLightSlateGray       = $FF778899;
  aclLightSteelBlue       = $FFB0C4DE;
  aclLightYellow          = $FFFFFFE0;
  aclLime                 = $FF00FF00;
  aclLimeGreen            = $FF32CD32;
  aclLinen                = $FFFAF0E6;
  aclMagenta              = $FFFF00FF;
  aclMaroon               = $FF800000;
  aclMediumAquamarine     = $FF66CDAA;
  aclMediumBlue           = $FF0000CD;
  aclMediumOrchid         = $FFBA55D3;
  aclMediumPurple         = $FF9370DB;
  aclMediumSeaGreen       = $FF3CB371;
  aclMediumSlateBlue      = $FF7B68EE;
  aclMediumSpringGreen    = $FF00FA9A;
  aclMediumTurquoise      = $FF48D1CC;
  aclMediumVioletRed      = $FFC71585;
  aclMidnightBlue         = $FF191970;
  aclMintCream            = $FFF5FFFA;
  aclMistyRose            = $FFFFE4E1;
  aclMoccasin             = $FFFFE4B5;
  aclNavajoWhite          = $FFFFDEAD;
  aclNavy                 = $FF000080;
  aclOldLace              = $FFFDF5E6;
  aclOlive                = $FF808000;
  aclOliveDrab            = $FF6B8E23;
  aclOrange               = $FFFFA500;
  aclOrangeRed            = $FFFF4500;
  aclOrchid               = $FFDA70D6;
  aclPaleGoldenrod        = $FFEEE8AA;
  aclPaleGreen            = $FF98FB98;
  aclPaleTurquoise        = $FFAFEEEE;
  aclPaleVioletRed        = $FFDB7093;
  aclPapayaWhip           = $FFFFEFD5;
  aclPeachPuff            = $FFFFDAB9;
  aclPeru                 = $FFCD853F;
  aclPink                 = $FFFFC0CB;
  aclPlum                 = $FFDDA0DD;
  aclPowderBlue           = $FFB0E0E6;
  aclPurple               = $FF800080;
  aclRed                  = $FFFF0000;
  aclRosyBrown            = $FFBC8F8F;
  aclRoyalBlue            = $FF4169E1;
  aclSaddleBrown          = $FF8B4513;
  aclSalmon               = $FFFA8072;
  aclSandyBrown           = $FFF4A460;
  aclSeaGreen             = $FF2E8B57;
  aclSeaShell             = $FFFFF5EE;
  aclSienna               = $FFA0522D;
  aclSilver               = $FFC0C0C0;
  aclSkyBlue              = $FF87CEEB;
  aclSlateBlue            = $FF6A5ACD;
  aclSlateGray            = $FF708090;
  aclSnow                 = $FFFFFAFA;
  aclSpringGreen          = $FF00FF7F;
  aclSteelBlue            = $FF4682B4;
  aclTan                  = $FFD2B48C;
  aclTeal                 = $FF008080;
  aclThistle              = $FFD8BFD8;
  aclTomato               = $FFFF6347;
  aclTransparent          = $00FFFFFF;
  aclTurquoise            = $FF40E0D0;
  aclViolet               = $FFEE82EE;
  aclWheat                = $FFF5DEB3;
  aclWhite                = $FFFFFFFF;
  aclWhiteSmoke           = $FFF5F5F5;
  aclYellow               = $FFFFFF00;
  aclYellowGreen          = $FF9ACD32;

  // Shift count and bit mask for A, R, G, B components
  AlphaShift  = 24;
  {.$EXTERNALSYM AlphaShift}
  RedShift    = 16;
  {.$EXTERNALSYM RedShift}
  GreenShift  = 8;
  {.$EXTERNALSYM GreenShift}
  BlueShift   = 0;
  {.$EXTERNALSYM BlueShift}

  AlphaMask   = $ff000000;
  {.$EXTERNALSYM AlphaMask}
  RedMask     = $00ff0000;
  {.$EXTERNALSYM RedMask}
  GreenMask   = $0000ff00;
  {.$EXTERNALSYM GreenMask}
  BlueMask    = $000000ff;
  {.$EXTERNALSYM BlueMask}


type
{  TGPColor = class
  protected
     Argb: ARGB;
  public
    constructor Create; overload;
    constructor Create(r, g, b: Byte); overload;
    constructor Create(a, r, g, b: Byte); overload;
    constructor Create(Value: ARGB); overload;
    function GetAlpha: BYTE;
    function GetA: BYTE;
    function GetRed: BYTE;
    function GetR: BYTE;
    function GetGreen: Byte;
    function GetG: Byte;
    function GetBlue: Byte;
    function GetB: Byte;
    function GetValue: ARGB;
    procedure SetValue(Value: ARGB);
    procedure SetFromCOLORREF(rgb: COLORREF);
    function ToCOLORREF: COLORREF;
    function MakeARGB(a, r, g, b: Byte): ARGB;
  end;  }

  PGPColor = ^TGPColor;
  TGPColor = ARGB;
  TColorDynArray = array of TGPColor;

type
  RECTL = Windows.TRect;
  SIZEL = Windows.TSize;

  {.$EXTERNALSYM ENHMETAHEADER3}
  ENHMETAHEADER3 = packed record
    iType          : DWORD;  // Record type EMR_HEADER
    nSize          : DWORD;  // Record size in bytes.  This may be greater
                             // than the sizeof(ENHMETAHEADER).
    rclBounds      : RECTL;  // Inclusive-inclusive bounds in device units
    rclFrame       : RECTL;  // Inclusive-inclusive Picture Frame .01mm unit
    dSignature     : DWORD;  // Signature.  Must be ENHMETA_SIGNATURE.
    nVersion       : DWORD;  // Version number
    nBytes         : DWORD;  // Size of the metafile in bytes
    nRecords       : DWORD;  // Number of records in the metafile
    nHandles       : WORD;   // Number of handles in the handle table
                             // Handle index zero is reserved.
    sReserved      : WORD;   // Reserved.  Must be zero.
    nDescription   : DWORD;  // Number of chars in the unicode desc string
                             // This is 0 if there is no description string
    offDescription : DWORD;  // Offset to the metafile description record.
                             // This is 0 if there is no description string
    nPalEntries    : DWORD;  // Number of entries in the metafile palette.
    szlDevice      : SIZEL;  // Size of the reference device in pels
    szlMillimeters : SIZEL;  // Size of the reference device in millimeters
  end;
  TENHMETAHEADER3 = ENHMETAHEADER3;
  PENHMETAHEADER3 = ^TENHMETAHEADER3;

  {.$EXTERNALSYM PWMFRect16}
  PWMFRect16 = packed record
    Left   : INT16;
    Top    : INT16;
    Right  : INT16;
    Bottom : INT16;
  end;
  TPWMFRect16 = PWMFRect16;
  PPWMFRect16 = ^TPWMFRect16;

  {.$EXTERNALSYM WmfPlaceableFileHeader}
  WmfPlaceableFileHeader = packed record
    Key         : UINT32;      // GDIP_WMF_PLACEABLEKEY
    Hmf         : INT16;       // Metafile HANDLE number (always 0)
    BoundingBox : PWMFRect16;  // Coordinates in metafile units
    Inch        : INT16;       // Number of metafile units per inch
    Reserved    : UINT32;      // Reserved (always 0)
    Checksum    : INT16;       // Checksum value for previous 10 WORDs
  end;
  TWmfPlaceableFileHeader = WmfPlaceableFileHeader;
  PWmfPlaceableFileHeader = ^TWmfPlaceableFileHeader;

const
  GDIP_EMFPLUSFLAGS_DISPLAY      = $00000001;
  {.$EXTERNALSYM GDIP_EMFPLUSFLAGS_DISPLAY}

type
  TMetafileHeader = packed class
  public
    Type_        : TMetafileType;
    Size         : UINT;           // Size of the metafile (in bytes)
    Version      : UINT;           // EMF+, EMF, or WMF version
    EmfPlusFlags : UINT;
    DpiX         : Single;
    DpiY         : Single;
    X            : Integer;        // Bounds in device units
    Y            : Integer;
    Width        : Integer;
    Height       : Integer;
    WmfHeader: TMETAHEADER;
    EmfHeader: TENHMETAHEADER3;
    EmfPlusHeaderSize : Integer; // size of the EMF+ header in file
    LogicalDpiX       : Integer; // Logical Dpi of reference Hdc
    LogicalDpiY       : Integer; // usually valid only for EMF+
  public
    property GetType: TMetafileType read Type_;
    property GetMetafileSize: UINT read Size;
    // If IsEmfPlus, this is the EMF+ version; else it is the WMF or EMF ver
    property GetVersion: UINT read Version;
     // Get the EMF+ flags associated with the metafile
    property GetEmfPlusFlags: UINT read EmfPlusFlags;
    property GetDpiX: Single read DpiX;
    property GetDpiY: Single read DpiY;
    procedure GetBounds(out Rect: TGPRect);
    // Is it any type of WMF (standard or Placeable Metafile)?
    function IsWmf: BOOL;
    // Is this an Placeable Metafile?
    function IsWmfPlaceable: BOOL;
    // Is this an EMF (not an EMF+)?
    function IsEmf: BOOL;
    // Is this an EMF or EMF+ file?
    function IsEmfOrEmfPlus: BOOL;
    // Is this an EMF+ file?
    function IsEmfPlus: BOOL;
    // Is this an EMF+ dual (has dual, down-level records) file?
    function IsEmfPlusDual: BOOL;
    // Is this an EMF+ only (no dual records) file?
    function IsEmfPlusOnly: BOOL;
    // If it's an EMF+ file, was it recorded against a display Hdc?
    function IsDisplay: BOOL;
    // Get the WMF header of the metafile (if it is a WMF)
    function GetWmfHeader: PMetaHeader;
    // Get the EMF header of the metafile (if it is an EMF)
    function GetEmfHeader: PENHMETAHEADER3;
  end;

//---------------------------------------------------------------------------
// Image file format identifiers
//---------------------------------------------------------------------------

const
  ImageFormatUndefined : TGUID = '{b96b3ca9-0728-11d3-9d7b-0000f81ef32e}';
  {.$EXTERNALSYM ImageFormatUndefined}
  ImageFormatMemoryBMP : TGUID = '{b96b3caa-0728-11d3-9d7b-0000f81ef32e}';
  {.$EXTERNALSYM ImageFormatMemoryBMP}
  ImageFormatBMP       : TGUID = '{b96b3cab-0728-11d3-9d7b-0000f81ef32e}';
  {.$EXTERNALSYM ImageFormatBMP}
  ImageFormatEMF       : TGUID = '{b96b3cac-0728-11d3-9d7b-0000f81ef32e}';
  {.$EXTERNALSYM ImageFormatEMF}
  ImageFormatWMF       : TGUID = '{b96b3cad-0728-11d3-9d7b-0000f81ef32e}';
  {.$EXTERNALSYM ImageFormatWMF}
  ImageFormatJPEG      : TGUID = '{b96b3cae-0728-11d3-9d7b-0000f81ef32e}';
  {.$EXTERNALSYM ImageFormatJPEG}
  ImageFormatPNG       : TGUID = '{b96b3caf-0728-11d3-9d7b-0000f81ef32e}';
  {.$EXTERNALSYM ImageFormatPNG}
  ImageFormatGIF       : TGUID = '{b96b3cb0-0728-11d3-9d7b-0000f81ef32e}';
  {.$EXTERNALSYM ImageFormatGIF}
  ImageFormatTIFF      : TGUID = '{b96b3cb1-0728-11d3-9d7b-0000f81ef32e}';
  {.$EXTERNALSYM ImageFormatTIFF}
  ImageFormatEXIF      : TGUID = '{b96b3cb2-0728-11d3-9d7b-0000f81ef32e}';
  {.$EXTERNALSYM ImageFormatEXIF}
  ImageFormatIcon      : TGUID = '{b96b3cb5-0728-11d3-9d7b-0000f81ef32e}';
  {.$EXTERNALSYM ImageFormatIcon}

//---------------------------------------------------------------------------
// Predefined multi-frame dimension IDs
//---------------------------------------------------------------------------

  FrameDimensionTime       : TGUID = '{6aedbd6d-3fb5-418a-83a6-7f45229dc872}';
  {.$EXTERNALSYM FrameDimensionTime}
  FrameDimensionResolution : TGUID = '{84236f7b-3bd3-428f-8dab-4ea1439ca315}';
  {.$EXTERNALSYM FrameDimensionResolution}
  FrameDimensionPage       : TGUID = '{7462dc86-6180-4c7e-8e3f-ee7333a7a483}';
  {.$EXTERNALSYM FrameDimensionPage}

//---------------------------------------------------------------------------
// Property sets
//---------------------------------------------------------------------------

  FormatIDImageInformation : TGUID = '{e5836cbe-5eef-4f1d-acde-ae4c43b608ce}';
  {.$EXTERNALSYM FormatIDImageInformation}
  FormatIDJpegAppHeaders   : TGUID = '{1c4afdcd-6177-43cf-abc7-5f51af39ee85}';
  {.$EXTERNALSYM FormatIDJpegAppHeaders}

//---------------------------------------------------------------------------
// Encoder parameter sets
//---------------------------------------------------------------------------

  EncoderCompression      : TGUID = '{e09d739d-ccd4-44ee-8eba-3fbf8be4fc58}';
  {.$EXTERNALSYM EncoderCompression}
  EncoderColorDepth       : TGUID = '{66087055-ad66-4c7c-9a18-38a2310b8337}';
  {.$EXTERNALSYM EncoderColorDepth}
  EncoderScanMethod       : TGUID = '{3a4e2661-3109-4e56-8536-42c156e7dcfa}';
  {.$EXTERNALSYM EncoderScanMethod}
  EncoderVersion          : TGUID = '{24d18c76-814a-41a4-bf53-1c219cccf797}';
  {.$EXTERNALSYM EncoderVersion}
  EncoderRenderMethod     : TGUID = '{6d42c53a-229a-4825-8bb7-5c99e2b9a8b8}';
  {.$EXTERNALSYM EncoderRenderMethod}
  EncoderQuality          : TGUID = '{1d5be4b5-fa4a-452d-9cdd-5db35105e7eb}';
  {.$EXTERNALSYM EncoderQuality}
  EncoderTransformation   : TGUID = '{8d0eb2d1-a58e-4ea8-aa14-108074b7b6f9}';
  {.$EXTERNALSYM EncoderTransformation}
  EncoderLuminanceTable   : TGUID = '{edb33bce-0266-4a77-b904-27216099e717}';
  {.$EXTERNALSYM EncoderLuminanceTable}
  EncoderChrominanceTable : TGUID = '{f2e455dc-09b3-4316-8260-676ada32481c}';
  {.$EXTERNALSYM EncoderChrominanceTable}
  EncoderSaveFlag         : TGUID = '{292266fc-ac40-47bf-8cfc-a85b89a655de}';
  {.$EXTERNALSYM EncoderSaveFlag}

  CodecIImageBytes : TGUID = '{025d1823-6c7d-447b-bbdb-a3cbc3dfa2fc}';
  {.$EXTERNALSYM CodecIImageBytes}

type
  {.$EXTERNALSYM IImageBytes}
  IImageBytes = Interface(IUnknown)
    ['{025D1823-6C7D-447B-BBDB-A3CBC3DFA2FC}']
    // Return total number of bytes in the IStream
    function CountBytes(out pcb: UINT): HRESULT; stdcall;
    // Locks "cb" bytes, starting from "ulOffset" in the stream, and returns the
    // pointer to the beginning of the locked memory chunk in "ppvBytes"
    function LockBytes(cb: UINT; ulOffset: ULONG; out ppvBytes: pointer): HRESULT; stdcall;
    // Unlocks "cb" bytes, pointed by "pvBytes", starting from "ulOffset" in the
    // stream
    function UnlockBytes(pvBytes: pointer; cb: UINT; ulOffset: ULONG): HRESULT; stdcall;
  end;

//--------------------------------------------------------------------------
// ImageCodecInfo structure
//--------------------------------------------------------------------------

  {.$EXTERNALSYM ImageCodecInfo}
  ImageCodecInfo = packed record
    Clsid             : TGUID;
    FormatID          : TGUID;
    CodecName         : PWCHAR;
    DllName           : PWCHAR;
    FormatDescription : PWCHAR;
    FilenameExtension : PWCHAR;
    MimeType          : PWCHAR;
    Flags             : DWORD;
    Version           : DWORD;
    SigCount          : DWORD;
    SigSize           : DWORD;
    SigPattern        : PBYTE;
    SigMask           : PBYTE;
  end;
  TImageCodecInfo = ImageCodecInfo;
  PImageCodecInfo = ^TImageCodecInfo;

//--------------------------------------------------------------------------
// Information flags about image codecs
//--------------------------------------------------------------------------
{$IFDEF DELPHI6_UP}
  {.$EXTERNALSYM ImageCodecFlags}
  ImageCodecFlags = (
    ImageCodecFlagsEncoder            = $00000001,
    ImageCodecFlagsDecoder            = $00000002,
    ImageCodecFlagsSupportBitmap      = $00000004,
    ImageCodecFlagsSupportVector      = $00000008,
    ImageCodecFlagsSeekableEncode     = $00000010,
    ImageCodecFlagsBlockingDecode     = $00000020,

    ImageCodecFlagsBuiltin            = $00010000,
    ImageCodecFlagsSystem             = $00020000,
    ImageCodecFlagsUser               = $00040000
  );
  TImageCodecFlags = ImageCodecFlags;
{$ELSE}
  {.$EXTERNALSYM ImageCodecFlags}
  ImageCodecFlags = Integer;
  const
    ImageCodecFlagsEncoder            = $00000001;
    ImageCodecFlagsDecoder            = $00000002;
    ImageCodecFlagsSupportBitmap      = $00000004;
    ImageCodecFlagsSupportVector      = $00000008;
    ImageCodecFlagsSeekableEncode     = $00000010;
    ImageCodecFlagsBlockingDecode     = $00000020;

    ImageCodecFlagsBuiltin            = $00010000;
    ImageCodecFlagsSystem             = $00020000;
    ImageCodecFlagsUser               = $00040000;

type
  TImageCodecFlags = ImageCodecFlags;
{$ENDIF}
//---------------------------------------------------------------------------
// Access modes used when calling Image::LockBits
//---------------------------------------------------------------------------

  {.$EXTERNALSYM ImageLockMode}
  ImageLockMode = Integer;
  const
    ImageLockModeRead         = $0001;
    ImageLockModeWrite        = $0002;
    ImageLockModeUserInputBuf = $0004;
type
  TImageLockMode = ImageLockMode;

//---------------------------------------------------------------------------
// Information about image pixel data
//---------------------------------------------------------------------------

  {.$EXTERNALSYM BitmapData}
  BitmapData = packed record
    Width       : UINT;
    Height      : UINT;
    Stride      : Integer;
    PixelFormat : PixelFormat;
    Scan0       : Pointer;
    Reserved    : UINT;
  end;
  TBitmapData = BitmapData;
  PBitmapData = ^TBitmapData;

//---------------------------------------------------------------------------
// Image flags
//---------------------------------------------------------------------------
{$IFDEF DELPHI6_UP}
  {.$EXTERNALSYM ImageFlags}
  ImageFlags = (
    ImageFlagsNone                = 0,

    // Low-word: shared with SINKFLAG_x

    ImageFlagsScalable            = $0001,
    ImageFlagsHasAlpha            = $0002,
    ImageFlagsHasTranslucent      = $0004,
    ImageFlagsPartiallyScalable   = $0008,

    // Low-word: color space definition

    ImageFlagsColorSpaceRGB       = $0010,
    ImageFlagsColorSpaceCMYK      = $0020,
    ImageFlagsColorSpaceGRAY      = $0040,
    ImageFlagsColorSpaceYCBCR     = $0080,
    ImageFlagsColorSpaceYCCK      = $0100,

    // Low-word: image size info

    ImageFlagsHasRealDPI          = $1000,
    ImageFlagsHasRealPixelSize    = $2000,

    // High-word

    ImageFlagsReadOnly            = $00010000,
    ImageFlagsCaching             = $00020000
  );
  TImageFlags = ImageFlags;
{$ELSE}
  {.$EXTERNALSYM ImageFlags}
  ImageFlags = Integer;
  const
    ImageFlagsNone                = 0;

    // Low-word: shared with SINKFLAG_x

    ImageFlagsScalable            = $0001;
    ImageFlagsHasAlpha            = $0002;
    ImageFlagsHasTranslucent      = $0004;
    ImageFlagsPartiallyScalable   = $0008;

    // Low-word: color space definition

    ImageFlagsColorSpaceRGB       = $0010;
    ImageFlagsColorSpaceCMYK      = $0020;
    ImageFlagsColorSpaceGRAY      = $0040;
    ImageFlagsColorSpaceYCBCR     = $0080;
    ImageFlagsColorSpaceYCCK      = $0100;

    // Low-word: image size info

    ImageFlagsHasRealDPI          = $1000;
    ImageFlagsHasRealPixelSize    = $2000;

    // High-word

    ImageFlagsReadOnly            = $00010000;
    ImageFlagsCaching             = $00020000;

type
  TImageFlags = ImageFlags;
{$ENDIF}


{$IFDEF DELPHI6_UP}
  {.$EXTERNALSYM RotateFlipType}
  RotateFlipType = (
    RotateNoneFlipNone = 0,
    Rotate90FlipNone   = 1,
    Rotate180FlipNone  = 2,
    Rotate270FlipNone  = 3,

    RotateNoneFlipX    = 4,
    Rotate90FlipX      = 5,
    Rotate180FlipX     = 6,
    Rotate270FlipX     = 7,

    RotateNoneFlipY    = Rotate180FlipX,
    Rotate90FlipY      = Rotate270FlipX,
    Rotate180FlipY     = RotateNoneFlipX,
    Rotate270FlipY     = Rotate90FlipX,

    RotateNoneFlipXY   = Rotate180FlipNone,
    Rotate90FlipXY     = Rotate270FlipNone,
    Rotate180FlipXY    = RotateNoneFlipNone,
    Rotate270FlipXY    = Rotate90FlipNone
  );
  TRotateFlipType = RotateFlipType;
{$ELSE}
  {.$EXTERNALSYM RotateFlipType}
  RotateFlipType = (
    RotateNoneFlipNone, // = 0,
    Rotate90FlipNone,   // = 1,
    Rotate180FlipNone,  // = 2,
    Rotate270FlipNone,  // = 3,

    RotateNoneFlipX,    // = 4,
    Rotate90FlipX,      // = 5,
    Rotate180FlipX,     // = 6,
    Rotate270FlipX      // = 7,
  );
  const
    RotateNoneFlipY    = Rotate180FlipX;
    Rotate90FlipY      = Rotate270FlipX;
    Rotate180FlipY     = RotateNoneFlipX;
    Rotate270FlipY     = Rotate90FlipX;

    RotateNoneFlipXY   = Rotate180FlipNone;
    Rotate90FlipXY     = Rotate270FlipNone;
    Rotate180FlipXY    = RotateNoneFlipNone;
    Rotate270FlipXY    = Rotate90FlipNone;

type
  TRotateFlipType = RotateFlipType;
{$ENDIF}

//---------------------------------------------------------------------------
// Encoder Parameter structure
//---------------------------------------------------------------------------

  {.$EXTERNALSYM EncoderParameter}
  EncoderParameter = packed record
    Guid           : TGUID;   // GUID of the parameter
    NumberOfValues : ULONG;   // Number of the parameter values
    Type_          : ULONG;   // Value type, like ValueTypeLONG  etc.
    Value          : Pointer; // A pointer to the parameter values
  end;
  TEncoderParameter = EncoderParameter;
  PEncoderParameter = ^TEncoderParameter;

//---------------------------------------------------------------------------
// Encoder Parameters structure
//---------------------------------------------------------------------------

  {.$EXTERNALSYM EncoderParameters}
  EncoderParameters = packed record
    Count     : UINT;               // Number of parameters in this structure
    Parameter : array[0..10] of TEncoderParameter;  // Parameter values
  end;
  TEncoderParameters = EncoderParameters;
  PEncoderParameters = ^TEncoderParameters;

//---------------------------------------------------------------------------
// Property Item
//---------------------------------------------------------------------------

  {.$EXTERNALSYM PropertyItem}
  PropertyItem = record // NOT PACKED !!
    id       : PROPID;  // ID of this property
    length   : ULONG;   // Length of the property value, in bytes
    type_    : WORD;    // Type of the value, as one of TAG_TYPE_XXX
    value    : Pointer; // property value
  end;
  TPropertyItem = PropertyItem;
  PPropertyItem = ^TPropertyItem;

//---------------------------------------------------------------------------
// Image property types
//---------------------------------------------------------------------------

const
  PropertyTagTypeByte      : Integer =  1;
  {.$EXTERNALSYM PropertyTagTypeByte}
  PropertyTagTypeASCII     : Integer =  2;
  {.$EXTERNALSYM PropertyTagTypeASCII}
  PropertyTagTypeShort     : Integer =  3;
  {.$EXTERNALSYM PropertyTagTypeShort}
  PropertyTagTypeLong      : Integer =  4;
  {.$EXTERNALSYM PropertyTagTypeLong}
  PropertyTagTypeRational  : Integer =  5;
  {.$EXTERNALSYM PropertyTagTypeRational}
  PropertyTagTypeUndefined : Integer =  7;
  {.$EXTERNALSYM PropertyTagTypeUndefined}
  PropertyTagTypeSLONG     : Integer =  9;
  {.$EXTERNALSYM PropertyTagTypeSLONG}
  PropertyTagTypeSRational : Integer = 10;
  {.$EXTERNALSYM PropertyTagTypeSRational}

//---------------------------------------------------------------------------
// Image property ID tags
//---------------------------------------------------------------------------

  PropertyTagExifIFD            = $8769;
  {.$EXTERNALSYM PropertyTagExifIFD}
  PropertyTagGpsIFD             = $8825;
  {.$EXTERNALSYM PropertyTagGpsIFD}

  PropertyTagNewSubfileType     = $00FE;
  {.$EXTERNALSYM PropertyTagNewSubfileType}
  PropertyTagSubfileType        = $00FF;
  {.$EXTERNALSYM PropertyTagSubfileType}
  PropertyTagImageWidth         = $0100;
  {.$EXTERNALSYM PropertyTagImageWidth}
  PropertyTagImageHeight        = $0101;
  {.$EXTERNALSYM PropertyTagImageHeight}
  PropertyTagBitsPerSample      = $0102;
  {.$EXTERNALSYM PropertyTagBitsPerSample}
  PropertyTagCompression        = $0103;
  {.$EXTERNALSYM PropertyTagCompression}
  PropertyTagPhotometricInterp  = $0106;
  {.$EXTERNALSYM PropertyTagPhotometricInterp}
  PropertyTagThreshHolding      = $0107;
  {.$EXTERNALSYM PropertyTagThreshHolding}
  PropertyTagCellWidth          = $0108;
  {.$EXTERNALSYM PropertyTagCellWidth}
  PropertyTagCellHeight         = $0109;
  {.$EXTERNALSYM PropertyTagCellHeight}
  PropertyTagFillOrder          = $010A;
  {.$EXTERNALSYM PropertyTagFillOrder}
  PropertyTagDocumentName       = $010D;
  {.$EXTERNALSYM PropertyTagDocumentName}
  PropertyTagImageDescription   = $010E;
  {.$EXTERNALSYM PropertyTagImageDescription}
  PropertyTagEquipMake          = $010F;
  {.$EXTERNALSYM PropertyTagEquipMake}
  PropertyTagEquipModel         = $0110;
  {.$EXTERNALSYM PropertyTagEquipModel}
  PropertyTagStripOffsets       = $0111;
  {.$EXTERNALSYM PropertyTagStripOffsets}
  PropertyTagOrientation        = $0112;
  {.$EXTERNALSYM PropertyTagOrientation}
  PropertyTagSamplesPerPixel    = $0115;
  {.$EXTERNALSYM PropertyTagSamplesPerPixel}
  PropertyTagRowsPerStrip       = $0116;
  {.$EXTERNALSYM PropertyTagRowsPerStrip}
  PropertyTagStripBytesCount    = $0117;
  {.$EXTERNALSYM PropertyTagStripBytesCount}
  PropertyTagMinSampleValue     = $0118;
  {.$EXTERNALSYM PropertyTagMinSampleValue}
  PropertyTagMaxSampleValue     = $0119;
  {.$EXTERNALSYM PropertyTagMaxSampleValue}
  PropertyTagXResolution        = $011A;   // Image resolution in width direction
  {.$EXTERNALSYM PropertyTagXResolution}
  PropertyTagYResolution        = $011B;   // Image resolution in height direction
  {.$EXTERNALSYM PropertyTagYResolution}
  PropertyTagPlanarConfig       = $011C;   // Image data arrangement
  {.$EXTERNALSYM PropertyTagPlanarConfig}
  PropertyTagPageName           = $011D;
  {.$EXTERNALSYM PropertyTagPageName}
  PropertyTagXPosition          = $011E;
  {.$EXTERNALSYM PropertyTagXPosition}
  PropertyTagYPosition          = $011F;
  {.$EXTERNALSYM PropertyTagYPosition}
  PropertyTagFreeOffset         = $0120;
  {.$EXTERNALSYM PropertyTagFreeOffset}
  PropertyTagFreeByteCounts     = $0121;
  {.$EXTERNALSYM PropertyTagFreeByteCounts}
  PropertyTagGrayResponseUnit   = $0122;
  {.$EXTERNALSYM PropertyTagGrayResponseUnit}
  PropertyTagGrayResponseCurve  = $0123;
  {.$EXTERNALSYM PropertyTagGrayResponseCurve}
  PropertyTagT4Option           = $0124;
  {.$EXTERNALSYM PropertyTagT4Option}
  PropertyTagT6Option           = $0125;
  {.$EXTERNALSYM PropertyTagT6Option}
  PropertyTagResolutionUnit     = $0128;   // Unit of X and Y resolution
  {.$EXTERNALSYM PropertyTagResolutionUnit}
  PropertyTagPageNumber         = $0129;
  {.$EXTERNALSYM PropertyTagPageNumber}
  PropertyTagTransferFuncition  = $012D;
  {.$EXTERNALSYM PropertyTagTransferFuncition}
  PropertyTagSoftwareUsed       = $0131;
  {.$EXTERNALSYM PropertyTagSoftwareUsed}
  PropertyTagDateTime           = $0132;
  {.$EXTERNALSYM PropertyTagDateTime}
  PropertyTagArtist             = $013B;
  {.$EXTERNALSYM PropertyTagArtist}
  PropertyTagHostComputer       = $013C;
  {.$EXTERNALSYM PropertyTagHostComputer}
  PropertyTagPredictor          = $013D;
  {.$EXTERNALSYM PropertyTagPredictor}
  PropertyTagWhitePoint         = $013E;
  {.$EXTERNALSYM PropertyTagWhitePoint}
  PropertyTagPrimaryChromaticities = $013F;
  {.$EXTERNALSYM PropertyTagPrimaryChromaticities}
  PropertyTagColorMap           = $0140;
  {.$EXTERNALSYM PropertyTagColorMap}
  PropertyTagHalftoneHints      = $0141;
  {.$EXTERNALSYM PropertyTagHalftoneHints}
  PropertyTagTileWidth          = $0142;
  {.$EXTERNALSYM PropertyTagTileWidth}
  PropertyTagTileLength         = $0143;
  {.$EXTERNALSYM PropertyTagTileLength}
  PropertyTagTileOffset         = $0144;
  {.$EXTERNALSYM PropertyTagTileOffset}
  PropertyTagTileByteCounts     = $0145;
  {.$EXTERNALSYM PropertyTagTileByteCounts}
  PropertyTagInkSet             = $014C;
  {.$EXTERNALSYM PropertyTagInkSet}
  PropertyTagInkNames           = $014D;
  {.$EXTERNALSYM PropertyTagInkNames}
  PropertyTagNumberOfInks       = $014E;
  {.$EXTERNALSYM PropertyTagNumberOfInks}
  PropertyTagDotRange           = $0150;
  {.$EXTERNALSYM PropertyTagDotRange}
  PropertyTagTargetPrinter      = $0151;
  {.$EXTERNALSYM PropertyTagTargetPrinter}
  PropertyTagExtraSamples       = $0152;
  {.$EXTERNALSYM PropertyTagExtraSamples}
  PropertyTagSampleFormat       = $0153;
  {.$EXTERNALSYM PropertyTagSampleFormat}
  PropertyTagSMinSampleValue    = $0154;
  {.$EXTERNALSYM PropertyTagSMinSampleValue}
  PropertyTagSMaxSampleValue    = $0155;
  {.$EXTERNALSYM PropertyTagSMaxSampleValue}
  PropertyTagTransferRange      = $0156;
  {.$EXTERNALSYM PropertyTagTransferRange}

  PropertyTagJPEGProc               = $0200;
  {.$EXTERNALSYM PropertyTagJPEGProc}
  PropertyTagJPEGInterFormat        = $0201;
  {.$EXTERNALSYM PropertyTagJPEGInterFormat}
  PropertyTagJPEGInterLength        = $0202;
  {.$EXTERNALSYM PropertyTagJPEGInterLength}
  PropertyTagJPEGRestartInterval    = $0203;
  {.$EXTERNALSYM PropertyTagJPEGRestartInterval}
  PropertyTagJPEGLosslessPredictors = $0205;
  {.$EXTERNALSYM PropertyTagJPEGLosslessPredictors}
  PropertyTagJPEGPointTransforms    = $0206;
  {.$EXTERNALSYM PropertyTagJPEGPointTransforms}
  PropertyTagJPEGQTables            = $0207;
  {.$EXTERNALSYM PropertyTagJPEGQTables}
  PropertyTagJPEGDCTables           = $0208;
  {.$EXTERNALSYM PropertyTagJPEGDCTables}
  PropertyTagJPEGACTables           = $0209;
  {.$EXTERNALSYM PropertyTagJPEGACTables}

  PropertyTagYCbCrCoefficients  = $0211;
  {.$EXTERNALSYM PropertyTagYCbCrCoefficients}
  PropertyTagYCbCrSubsampling   = $0212;
  {.$EXTERNALSYM PropertyTagYCbCrSubsampling}
  PropertyTagYCbCrPositioning   = $0213;
  {.$EXTERNALSYM PropertyTagYCbCrPositioning}
  PropertyTagREFBlackWhite      = $0214;
  {.$EXTERNALSYM PropertyTagREFBlackWhite}

  PropertyTagICCProfile         = $8773;   // This TAG is defined by ICC
  {.$EXTERNALSYM PropertyTagICCProfile}
                                           // for embedded ICC in TIFF
  PropertyTagGamma                = $0301;
  {.$EXTERNALSYM PropertyTagGamma}
  PropertyTagICCProfileDescriptor = $0302;
  {.$EXTERNALSYM PropertyTagICCProfileDescriptor}
  PropertyTagSRGBRenderingIntent  = $0303;
  {.$EXTERNALSYM PropertyTagSRGBRenderingIntent}

  PropertyTagImageTitle         = $0320;
  {.$EXTERNALSYM PropertyTagImageTitle}
  PropertyTagCopyright          = $8298;
  {.$EXTERNALSYM PropertyTagCopyright}

// Extra TAGs (Like Adobe Image Information tags etc.)

  PropertyTagResolutionXUnit           = $5001;
  {.$EXTERNALSYM PropertyTagResolutionXUnit}
  PropertyTagResolutionYUnit           = $5002;
  {.$EXTERNALSYM PropertyTagResolutionYUnit}
  PropertyTagResolutionXLengthUnit     = $5003;
  {.$EXTERNALSYM PropertyTagResolutionXLengthUnit}
  PropertyTagResolutionYLengthUnit     = $5004;
  {.$EXTERNALSYM PropertyTagResolutionYLengthUnit}
  PropertyTagPrintFlags                = $5005;
  {.$EXTERNALSYM PropertyTagPrintFlags}
  PropertyTagPrintFlagsVersion         = $5006;
  {.$EXTERNALSYM PropertyTagPrintFlagsVersion}
  PropertyTagPrintFlagsCrop            = $5007;
  {.$EXTERNALSYM PropertyTagPrintFlagsCrop}
  PropertyTagPrintFlagsBleedWidth      = $5008;
  {.$EXTERNALSYM PropertyTagPrintFlagsBleedWidth}
  PropertyTagPrintFlagsBleedWidthScale = $5009;
  {.$EXTERNALSYM PropertyTagPrintFlagsBleedWidthScale}
  PropertyTagHalftoneLPI               = $500A;
  {.$EXTERNALSYM PropertyTagHalftoneLPI}
  PropertyTagHalftoneLPIUnit           = $500B;
  {.$EXTERNALSYM PropertyTagHalftoneLPIUnit}
  PropertyTagHalftoneDegree            = $500C;
  {.$EXTERNALSYM PropertyTagHalftoneDegree}
  PropertyTagHalftoneShape             = $500D;
  {.$EXTERNALSYM PropertyTagHalftoneShape}
  PropertyTagHalftoneMisc              = $500E;
  {.$EXTERNALSYM PropertyTagHalftoneMisc}
  PropertyTagHalftoneScreen            = $500F;
  {.$EXTERNALSYM PropertyTagHalftoneScreen}
  PropertyTagJPEGQuality               = $5010;
  {.$EXTERNALSYM PropertyTagJPEGQuality}
  PropertyTagGridSize                  = $5011;
  {.$EXTERNALSYM PropertyTagGridSize}
  PropertyTagThumbnailFormat           = $5012;  // 1 = JPEG, 0 = RAW RGB
  {.$EXTERNALSYM PropertyTagThumbnailFormat}
  PropertyTagThumbnailWidth            = $5013;
  {.$EXTERNALSYM PropertyTagThumbnailWidth}
  PropertyTagThumbnailHeight           = $5014;
  {.$EXTERNALSYM PropertyTagThumbnailHeight}
  PropertyTagThumbnailColorDepth       = $5015;
  {.$EXTERNALSYM PropertyTagThumbnailColorDepth}
  PropertyTagThumbnailPlanes           = $5016;
  {.$EXTERNALSYM PropertyTagThumbnailPlanes}
  PropertyTagThumbnailRawBytes         = $5017;
  {.$EXTERNALSYM PropertyTagThumbnailRawBytes}
  PropertyTagThumbnailSize             = $5018;
  {.$EXTERNALSYM PropertyTagThumbnailSize}
  PropertyTagThumbnailCompressedSize   = $5019;
  {.$EXTERNALSYM PropertyTagThumbnailCompressedSize}
  PropertyTagColorTransferFunction     = $501A;
  {.$EXTERNALSYM PropertyTagColorTransferFunction}
  PropertyTagThumbnailData             = $501B;    // RAW thumbnail bits in
  {.$EXTERNALSYM PropertyTagThumbnailData}
                                                   // JPEG format or RGB format
                                                   // depends on
                                                   // PropertyTagThumbnailFormat

  // Thumbnail related TAGs

  PropertyTagThumbnailImageWidth        = $5020;   // Thumbnail width
  {.$EXTERNALSYM PropertyTagThumbnailImageWidth}
  PropertyTagThumbnailImageHeight       = $5021;   // Thumbnail height
  {.$EXTERNALSYM PropertyTagThumbnailImageHeight}
  PropertyTagThumbnailBitsPerSample     = $5022;   // Number of bits per
  {.$EXTERNALSYM PropertyTagThumbnailBitsPerSample}
                                                   // component
  PropertyTagThumbnailCompression       = $5023;   // Compression Scheme
  {.$EXTERNALSYM PropertyTagThumbnailCompression}
  PropertyTagThumbnailPhotometricInterp = $5024;   // Pixel composition
  {.$EXTERNALSYM PropertyTagThumbnailPhotometricInterp}
  PropertyTagThumbnailImageDescription  = $5025;   // Image Tile
  {.$EXTERNALSYM PropertyTagThumbnailImageDescription}
  PropertyTagThumbnailEquipMake         = $5026;   // Manufacturer of Image
  {.$EXTERNALSYM PropertyTagThumbnailEquipMake}
                                                   // Input equipment
  PropertyTagThumbnailEquipModel        = $5027;   // Model of Image input
  {.$EXTERNALSYM PropertyTagThumbnailEquipModel}
                                                   // equipment
  PropertyTagThumbnailStripOffsets    = $5028;  // Image data location
  {.$EXTERNALSYM PropertyTagThumbnailStripOffsets}
  PropertyTagThumbnailOrientation     = $5029;  // Orientation of image
  {.$EXTERNALSYM PropertyTagThumbnailOrientation}
  PropertyTagThumbnailSamplesPerPixel = $502A;  // Number of components
  {.$EXTERNALSYM PropertyTagThumbnailSamplesPerPixel}
  PropertyTagThumbnailRowsPerStrip    = $502B;  // Number of rows per strip
  {.$EXTERNALSYM PropertyTagThumbnailRowsPerStrip}
  PropertyTagThumbnailStripBytesCount = $502C;  // Bytes per compressed
  {.$EXTERNALSYM PropertyTagThumbnailStripBytesCount}
                                                // strip
  PropertyTagThumbnailResolutionX     = $502D;  // Resolution in width
  {.$EXTERNALSYM PropertyTagThumbnailResolutionX}
                                                // direction
  PropertyTagThumbnailResolutionY     = $502E;  // Resolution in height
  {.$EXTERNALSYM PropertyTagThumbnailResolutionY}
                                                // direction
  PropertyTagThumbnailPlanarConfig    = $502F;  // Image data arrangement
  {.$EXTERNALSYM PropertyTagThumbnailPlanarConfig}
  PropertyTagThumbnailResolutionUnit  = $5030;  // Unit of X and Y
  {.$EXTERNALSYM PropertyTagThumbnailResolutionUnit}
                                                // Resolution
  PropertyTagThumbnailTransferFunction = $5031;  // Transfer function
  {.$EXTERNALSYM PropertyTagThumbnailTransferFunction}
  PropertyTagThumbnailSoftwareUsed     = $5032;  // Software used
  {.$EXTERNALSYM PropertyTagThumbnailSoftwareUsed}
  PropertyTagThumbnailDateTime         = $5033;  // File change date and
  {.$EXTERNALSYM PropertyTagThumbnailDateTime}
                                                 // time
  PropertyTagThumbnailArtist          = $5034;  // Person who created the
  {.$EXTERNALSYM PropertyTagThumbnailArtist}
                                                // image
  PropertyTagThumbnailWhitePoint      = $5035;  // White point chromaticity
  {.$EXTERNALSYM PropertyTagThumbnailWhitePoint}
  PropertyTagThumbnailPrimaryChromaticities = $5036;
  {.$EXTERNALSYM PropertyTagThumbnailPrimaryChromaticities}
                                                    // Chromaticities of
                                                    // primaries
  PropertyTagThumbnailYCbCrCoefficients = $5037; // Color space transforma-
  {.$EXTERNALSYM PropertyTagThumbnailYCbCrCoefficients}
                                                 // tion coefficients
  PropertyTagThumbnailYCbCrSubsampling = $5038;  // Subsampling ratio of Y
  {.$EXTERNALSYM PropertyTagThumbnailYCbCrSubsampling}
                                                 // to C
  PropertyTagThumbnailYCbCrPositioning = $5039;  // Y and C position
  {.$EXTERNALSYM PropertyTagThumbnailYCbCrPositioning}
  PropertyTagThumbnailRefBlackWhite    = $503A;  // Pair of black and white
  {.$EXTERNALSYM PropertyTagThumbnailRefBlackWhite}
                                                 // reference values
  PropertyTagThumbnailCopyRight       = $503B;   // CopyRight holder
  {.$EXTERNALSYM PropertyTagThumbnailCopyRight}

  PropertyTagLuminanceTable           = $5090;
  {.$EXTERNALSYM PropertyTagLuminanceTable}
  PropertyTagChrominanceTable         = $5091;
  {.$EXTERNALSYM PropertyTagChrominanceTable}

  PropertyTagFrameDelay               = $5100;
  {.$EXTERNALSYM PropertyTagFrameDelay}
  PropertyTagLoopCount                = $5101;
  {.$EXTERNALSYM PropertyTagLoopCount}

  PropertyTagPixelUnit         = $5110;  // Unit specifier for pixel/unit
  {.$EXTERNALSYM PropertyTagPixelUnit}
  PropertyTagPixelPerUnitX     = $5111;  // Pixels per unit in X
  {.$EXTERNALSYM PropertyTagPixelPerUnitX}
  PropertyTagPixelPerUnitY     = $5112;  // Pixels per unit in Y
  {.$EXTERNALSYM PropertyTagPixelPerUnitY}
  PropertyTagPaletteHistogram  = $5113;  // Palette histogram
  {.$EXTERNALSYM PropertyTagPaletteHistogram}

  // EXIF specific tag

  PropertyTagExifExposureTime  = $829A;
  {.$EXTERNALSYM PropertyTagExifExposureTime}
  PropertyTagExifFNumber       = $829D;
  {.$EXTERNALSYM PropertyTagExifFNumber}

  PropertyTagExifExposureProg  = $8822;
  {.$EXTERNALSYM PropertyTagExifExposureProg}
  PropertyTagExifSpectralSense = $8824;
  {.$EXTERNALSYM PropertyTagExifSpectralSense}
  PropertyTagExifISOSpeed      = $8827;
  {.$EXTERNALSYM PropertyTagExifISOSpeed}
  PropertyTagExifOECF          = $8828;
  {.$EXTERNALSYM PropertyTagExifOECF}

  PropertyTagExifVer           = $9000;
  {.$EXTERNALSYM PropertyTagExifVer}
  PropertyTagExifDTOrig        = $9003; // Date & time of original
  {.$EXTERNALSYM PropertyTagExifDTOrig}
  PropertyTagExifDTDigitized   = $9004; // Date & time of digital data generation
  {.$EXTERNALSYM PropertyTagExifDTDigitized}

  PropertyTagExifCompConfig    = $9101;
  {.$EXTERNALSYM PropertyTagExifCompConfig}
  PropertyTagExifCompBPP       = $9102;
  {.$EXTERNALSYM PropertyTagExifCompBPP}

  PropertyTagExifShutterSpeed  = $9201;
  {.$EXTERNALSYM PropertyTagExifShutterSpeed}
  PropertyTagExifAperture      = $9202;
  {.$EXTERNALSYM PropertyTagExifAperture}
  PropertyTagExifBrightness    = $9203;
  {.$EXTERNALSYM PropertyTagExifBrightness}
  PropertyTagExifExposureBias  = $9204;
  {.$EXTERNALSYM PropertyTagExifExposureBias}
  PropertyTagExifMaxAperture   = $9205;
  {.$EXTERNALSYM PropertyTagExifMaxAperture}
  PropertyTagExifSubjectDist   = $9206;
  {.$EXTERNALSYM PropertyTagExifSubjectDist}
  PropertyTagExifMeteringMode  = $9207;
  {.$EXTERNALSYM PropertyTagExifMeteringMode}
  PropertyTagExifLightSource   = $9208;
  {.$EXTERNALSYM PropertyTagExifLightSource}
  PropertyTagExifFlash         = $9209;
  {.$EXTERNALSYM PropertyTagExifFlash}
  PropertyTagExifFocalLength   = $920A;
  {.$EXTERNALSYM PropertyTagExifFocalLength}
  PropertyTagExifMakerNote     = $927C;
  {.$EXTERNALSYM PropertyTagExifMakerNote}
  PropertyTagExifUserComment   = $9286;
  {.$EXTERNALSYM PropertyTagExifUserComment}
  PropertyTagExifDTSubsec      = $9290;  // Date & Time subseconds
  {.$EXTERNALSYM PropertyTagExifDTSubsec}
  PropertyTagExifDTOrigSS      = $9291;  // Date & Time original subseconds
  {.$EXTERNALSYM PropertyTagExifDTOrigSS}
  PropertyTagExifDTDigSS       = $9292;  // Date & TIme digitized subseconds
  {.$EXTERNALSYM PropertyTagExifDTDigSS}

  PropertyTagExifFPXVer        = $A000;
  {.$EXTERNALSYM PropertyTagExifFPXVer}
  PropertyTagExifColorSpace    = $A001;
  {.$EXTERNALSYM PropertyTagExifColorSpace}
  PropertyTagExifPixXDim       = $A002;
  {.$EXTERNALSYM PropertyTagExifPixXDim}
  PropertyTagExifPixYDim       = $A003;
  {.$EXTERNALSYM PropertyTagExifPixYDim}
  PropertyTagExifRelatedWav    = $A004;  // related sound file
  {.$EXTERNALSYM PropertyTagExifRelatedWav}
  PropertyTagExifInterop       = $A005;
  {.$EXTERNALSYM PropertyTagExifInterop}
  PropertyTagExifFlashEnergy   = $A20B;
  {.$EXTERNALSYM PropertyTagExifFlashEnergy}
  PropertyTagExifSpatialFR     = $A20C;  // Spatial Frequency Response
  {.$EXTERNALSYM PropertyTagExifSpatialFR}
  PropertyTagExifFocalXRes     = $A20E;  // Focal Plane X Resolution
  {.$EXTERNALSYM PropertyTagExifFocalXRes}
  PropertyTagExifFocalYRes     = $A20F;  // Focal Plane Y Resolution
  {.$EXTERNALSYM PropertyTagExifFocalYRes}
  PropertyTagExifFocalResUnit  = $A210;  // Focal Plane Resolution Unit
  {.$EXTERNALSYM PropertyTagExifFocalResUnit}
  PropertyTagExifSubjectLoc    = $A214;
  {.$EXTERNALSYM PropertyTagExifSubjectLoc}
  PropertyTagExifExposureIndex = $A215;
  {.$EXTERNALSYM PropertyTagExifExposureIndex}
  PropertyTagExifSensingMethod = $A217;
  {.$EXTERNALSYM PropertyTagExifSensingMethod}
  PropertyTagExifFileSource    = $A300;
  {.$EXTERNALSYM PropertyTagExifFileSource}
  PropertyTagExifSceneType     = $A301;
  {.$EXTERNALSYM PropertyTagExifSceneType}
  PropertyTagExifCfaPattern    = $A302;
  {.$EXTERNALSYM PropertyTagExifCfaPattern}

  PropertyTagGpsVer            = $0000;
  {.$EXTERNALSYM PropertyTagGpsVer}
  PropertyTagGpsLatitudeRef    = $0001;
  {.$EXTERNALSYM PropertyTagGpsLatitudeRef}
  PropertyTagGpsLatitude       = $0002;
  {.$EXTERNALSYM PropertyTagGpsLatitude}
  PropertyTagGpsLongitudeRef   = $0003;
  {.$EXTERNALSYM PropertyTagGpsLongitudeRef}
  PropertyTagGpsLongitude      = $0004;
  {.$EXTERNALSYM PropertyTagGpsLongitude}
  PropertyTagGpsAltitudeRef    = $0005;
  {.$EXTERNALSYM PropertyTagGpsAltitudeRef}
  PropertyTagGpsAltitude       = $0006;
  {.$EXTERNALSYM PropertyTagGpsAltitude}
  PropertyTagGpsGpsTime        = $0007;
  {.$EXTERNALSYM PropertyTagGpsGpsTime}
  PropertyTagGpsGpsSatellites  = $0008;
  {.$EXTERNALSYM PropertyTagGpsGpsSatellites}
  PropertyTagGpsGpsStatus      = $0009;
  {.$EXTERNALSYM PropertyTagGpsGpsStatus}
  PropertyTagGpsGpsMeasureMode = $00A;
  {.$EXTERNALSYM PropertyTagGpsGpsMeasureMode}
  PropertyTagGpsGpsDop         = $000B;  // Measurement precision
  {.$EXTERNALSYM PropertyTagGpsGpsDop}
  PropertyTagGpsSpeedRef       = $000C;
  {.$EXTERNALSYM PropertyTagGpsSpeedRef}
  PropertyTagGpsSpeed          = $000D;
  {.$EXTERNALSYM PropertyTagGpsSpeed}
  PropertyTagGpsTrackRef       = $000E;
  {.$EXTERNALSYM PropertyTagGpsTrackRef}
  PropertyTagGpsTrack          = $000F;
  {.$EXTERNALSYM PropertyTagGpsTrack}
  PropertyTagGpsImgDirRef      = $0010;
  {.$EXTERNALSYM PropertyTagGpsImgDirRef}
  PropertyTagGpsImgDir         = $0011;
  {.$EXTERNALSYM PropertyTagGpsImgDir}
  PropertyTagGpsMapDatum       = $0012;
  {.$EXTERNALSYM PropertyTagGpsMapDatum}
  PropertyTagGpsDestLatRef     = $0013;
  {.$EXTERNALSYM PropertyTagGpsDestLatRef}
  PropertyTagGpsDestLat        = $0014;
  {.$EXTERNALSYM PropertyTagGpsDestLat}
  PropertyTagGpsDestLongRef    = $0015;
  {.$EXTERNALSYM PropertyTagGpsDestLongRef}
  PropertyTagGpsDestLong       = $0016;
  {.$EXTERNALSYM PropertyTagGpsDestLong}
  PropertyTagGpsDestBearRef    = $0017;
  {.$EXTERNALSYM PropertyTagGpsDestBearRef}
  PropertyTagGpsDestBear       = $0018;
  {.$EXTERNALSYM PropertyTagGpsDestBear}
  PropertyTagGpsDestDistRef    = $0019;
  {.$EXTERNALSYM PropertyTagGpsDestDistRef}
  PropertyTagGpsDestDist       = $001A;
  {.$EXTERNALSYM PropertyTagGpsDestDist}

//----------------------------------------------------------------------------
// Color matrix
//----------------------------------------------------------------------------

type
  {.$EXTERNALSYM ColorMatrix}
  ColorMatrix = packed array[0..4, 0..4] of Single;
  TColorMatrix = ColorMatrix;
  PColorMatrix = ^TColorMatrix;

//----------------------------------------------------------------------------
// Color Matrix flags
//----------------------------------------------------------------------------

  {.$EXTERNALSYM ColorMatrixFlags}
  ColorMatrixFlags = (
    ColorMatrixFlagsDefault,
    ColorMatrixFlagsSkipGrays,
    ColorMatrixFlagsAltGray
  );
  TColorMatrixFlags = ColorMatrixFlags;

//----------------------------------------------------------------------------
// Color Adjust Type
//----------------------------------------------------------------------------

  {.$EXTERNALSYM ColorAdjustType}
  ColorAdjustType = (
    ColorAdjustTypeDefault,
    ColorAdjustTypeBitmap,
    ColorAdjustTypeBrush,
    ColorAdjustTypePen,
    ColorAdjustTypeText,
    ColorAdjustTypeCount,
    ColorAdjustTypeAny      // Reserved
  );
  TColorAdjustType = ColorAdjustType;

//----------------------------------------------------------------------------
// Color Map
//----------------------------------------------------------------------------

  {.$EXTERNALSYM ColorMap}
  ColorMap = packed record
    oldColor: TGPColor;
    newColor: TGPColor;
  end;
  TColorMap = ColorMap;
  PColorMap = ^TColorMap;

//---------------------------------------------------------------------------
// Private GDI+ classes for internal type checking
//---------------------------------------------------------------------------

  GpGraphics = Pointer;

  GpBrush = Pointer;
  GpTexture = Pointer;
  GpSolidFill = Pointer;
  GpLineGradient = Pointer;
  GpPathGradient = Pointer;
  GpHatch =  Pointer;

  GpPen = Pointer;
  GpCustomLineCap = Pointer;
  GpAdjustableArrowCap = Pointer;

  GpImage = Pointer;
  GpBitmap = Pointer;
  GpMetafile = Pointer;
  GpImageAttributes = Pointer;

  GpPath = Pointer;
  GpRegion = Pointer;
  GpPathIterator = Pointer;

  GpFontFamily = Pointer;
  GpFont = Pointer;
  GpStringFormat = Pointer;
  GpFontCollection = Pointer;
  GpCachedBitmap = Pointer;

  GpStatus          = TStatus;
  GpFillMode        = TFillMode;
  GpWrapMode        = TWrapMode;
  GpUnit            = TUnit;
  GpCoordinateSpace = TCoordinateSpace;
  GpPointF          = PGPPointF;
  GpPoint           = PGPPoint;
  GpRectF           = PGPRectF;
  GpRect            = PGPRect;
  GpSizeF           = PGPSizeF;
  GpHatchStyle      = THatchStyle;
  GpDashStyle       = TDashStyle;
  GpLineCap         = TLineCap;
  GpDashCap         = TDashCap;

  GpPenAlignment    = TPenAlignment;

  GpLineJoin        = TLineJoin;
  GpPenType         = TPenType;

  GpMatrix          = Pointer; 
  GpBrushType       = TBrushType;
  GpMatrixOrder     = TMatrixOrder;
  GpFlushIntention  = TFlushIntention;
  GpPathData        = TPathData;

var

GdipCreatePath: function(brushMode: GPFILLMODE;
    out path: GPPATH): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreatePath}

GdipCreatePath2: function(v1: GPPOINTF; v2: PBYTE; v3: Integer; v4: GPFILLMODE;  
    out path: GPPATH): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreatePath2}

GdipCreatePath2I: function(v1: GPPOINT; v2: PBYTE; v3: Integer; v4: GPFILLMODE;  
    out path: GPPATH): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreatePath2I}

GdipClonePath: function(path: GPPATH;  
    out clonePath: GPPATH): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipClonePath}

GdipDeletePath: function(path: GPPATH): GPSTATUS; stdcall;  
  {.$EXTERNALSYM GdipDeletePath}

GdipResetPath: function(path: GPPATH): GPSTATUS; stdcall;  
  {.$EXTERNALSYM GdipResetPath}

GdipGetPointCount: function(path: GPPATH;
    out count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetPointCount}

GdipGetPathTypes: function(path: GPPATH; types: PBYTE;  
    count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetPathTypes}

GdipGetPathPoints: function(v1: GPPATH; points: GPPOINTF;  
    count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetPathPoints}

GdipGetPathPointsI: function(v1: GPPATH; points: GPPOINT;  
             count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetPathPointsI}

GdipGetPathFillMode: function(path: GPPATH;  
    var fillmode: GPFILLMODE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetPathFillMode}

GdipSetPathFillMode: function(path: GPPATH;
    fillmode: GPFILLMODE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetPathFillMode}

GdipGetPathData: function(path: GPPATH;  
    pathData: Pointer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetPathData}

GdipStartPathFigure: function(path: GPPATH): GPSTATUS; stdcall;  
  {.$EXTERNALSYM GdipStartPathFigure}

GdipClosePathFigure: function(path: GPPATH): GPSTATUS; stdcall;  
  {.$EXTERNALSYM GdipClosePathFigure}

GdipClosePathFigures: function(path: GPPATH): GPSTATUS; stdcall;  
  {.$EXTERNALSYM GdipClosePathFigures}

GdipSetPathMarker: function(path: GPPATH): GPSTATUS; stdcall;  
  {.$EXTERNALSYM GdipSetPathMarker}

GdipClearPathMarkers: function(path: GPPATH): GPSTATUS; stdcall;  
  {.$EXTERNALSYM GdipClearPathMarkers}

GdipReversePath: function(path: GPPATH): GPSTATUS; stdcall;  
  {.$EXTERNALSYM GdipReversePath}

GdipGetPathLastPoint: function(path: GPPATH;  
    lastPoint: GPPOINTF): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetPathLastPoint}

GdipAddPathLine: function(path: GPPATH;  
    x1, y1, x2, y2: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipAddPathLine}

GdipAddPathLine2: function(path: GPPATH; points: GPPOINTF;
    count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipAddPathLine2}

GdipAddPathArc: function(path: GPPATH; x, y, width, height, startAngle,  
    sweepAngle: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipAddPathArc}

GdipAddPathBezier: function(path: GPPATH;
    x1, y1, x2, y2, x3, y3, x4, y4: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipAddPathBezier}

GdipAddPathBeziers: function(path: GPPATH; points: GPPOINTF;  
    count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipAddPathBeziers}

GdipAddPathCurve: function(path: GPPATH; points: GPPOINTF;  
    count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipAddPathCurve}

GdipAddPathCurve2: function(path: GPPATH; points: GPPOINTF; count: Integer;  
    tension: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipAddPathCurve2}

GdipAddPathCurve3: function(path: GPPATH; points: GPPOINTF; count: Integer;  
    offset: Integer; numberOfSegments: Integer;
    tension: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipAddPathCurve3}

GdipAddPathClosedCurve: function(path: GPPATH; points: GPPOINTF;  
    count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipAddPathClosedCurve}

GdipAddPathClosedCurve2: function(path: GPPATH; points: GPPOINTF;  
    count: Integer; tension: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipAddPathClosedCurve2}

GdipAddPathRectangle: function(path: GPPATH; x: Single; y: Single;  
    width: Single; height: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipAddPathRectangle}

GdipAddPathRectangles: function(path: GPPATH; rects: GPRECTF;  
    count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipAddPathRectangles}

GdipAddPathEllipse: function(path: GPPATH;  x: Single; y: Single;  
    width: Single; height: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipAddPathEllipse}

GdipAddPathPie: function(path: GPPATH; x: Single; y: Single; width: Single;  
    height: Single; startAngle: Single; sweepAngle: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipAddPathPie}

GdipAddPathPolygon: function(path: GPPATH; points: GPPOINTF;  
    count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipAddPathPolygon}

GdipAddPathPath: function(path: GPPATH; addingPath: GPPATH;
    connect: Bool): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipAddPathPath}

GdipAddPathString: function(path: GPPATH; string_: PWCHAR; length: Integer;  
    family: GPFONTFAMILY; style: Integer; emSize: Single; layoutRect: PGPRectF;
    format: GPSTRINGFORMAT): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipAddPathString}

GdipAddPathStringI: function(path: GPPATH; string_: PWCHAR; length: Integer;  
    family: GPFONTFAMILY; style: Integer; emSize: Single; layoutRect: PGPRect;
    format: GPSTRINGFORMAT): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipAddPathStringI}

GdipAddPathLineI: function(path: GPPATH; x1: Integer; y1: Integer; x2: Integer;  
    y2: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipAddPathLineI}

GdipAddPathLine2I: function(path: GPPATH; points: GPPOINT;  
    count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipAddPathLine2I}

GdipAddPathArcI: function(path: GPPATH; x: Integer; y: Integer; width: Integer;  
    height: Integer; startAngle: Single; sweepAngle: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipAddPathArcI}

GdipAddPathBezierI: function(path: GPPATH; x1: Integer; y1: Integer;  
    x2: Integer; y2: Integer; x3: Integer; y3: Integer; x4: Integer;
    y4: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipAddPathBezierI}

GdipAddPathBeziersI: function(path: GPPATH; points: GPPOINT;  
    count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipAddPathBeziersI}

GdipAddPathCurveI: function(path: GPPATH; points: GPPOINT;
    count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipAddPathCurveI}

GdipAddPathCurve2I: function(path: GPPATH; points: GPPOINT; count: Integer;  
    tension: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipAddPathCurve2I}

GdipAddPathCurve3I: function(path: GPPATH; points: GPPOINT; count: Integer;  
    offset: Integer; numberOfSegments: Integer;
    tension: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipAddPathCurve3I}

GdipAddPathClosedCurveI: function(path: GPPATH; points: GPPOINT;  
    count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipAddPathClosedCurveI}

GdipAddPathClosedCurve2I: function(path: GPPATH; points: GPPOINT;  
    count: Integer; tension: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipAddPathClosedCurve2I}

GdipAddPathRectangleI: function(path: GPPATH; x: Integer; y: Integer;  
    width: Integer; height: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipAddPathRectangleI}

GdipAddPathRectanglesI: function(path: GPPATH; rects: GPRECT;  
    count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipAddPathRectanglesI}

GdipAddPathEllipseI: function(path: GPPATH; x: Integer; y: Integer;  
    width: Integer; height: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipAddPathEllipseI}

GdipAddPathPieI: function(path: GPPATH; x: Integer; y: Integer; width: Integer;  
    height: Integer; startAngle: Single; sweepAngle: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipAddPathPieI}

GdipAddPathPolygonI: function(path: GPPATH; points: GPPOINT;  
    count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipAddPathPolygonI}

GdipFlattenPath: function(path: GPPATH; matrix: GPMATRIX;  
    flatness: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipFlattenPath}

GdipWindingModeOutline: function(path: GPPATH; matrix: GPMATRIX;  
    flatness: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipWindingModeOutline}

GdipWidenPath: function(nativePath: GPPATH; pen: GPPEN; matrix: GPMATRIX;  
    flatness: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipWidenPath}

GdipWarpPath: function(path: GPPATH; matrix: GPMATRIX; points: GPPOINTF;  
    count: Integer; srcx: Single; srcy: Single; srcwidth: Single;
    srcheight: Single; warpMode: WARPMODE; flatness: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipWarpPath}

GdipTransformPath: function(path: GPPATH; matrix: GPMATRIX): GPSTATUS; stdcall;  
  {.$EXTERNALSYM GdipTransformPath}

GdipGetPathWorldBounds: function(path: GPPATH; bounds: GPRECTF;  
    matrix: GPMATRIX; pen: GPPEN): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetPathWorldBounds}

GdipGetPathWorldBoundsI: function(path: GPPATH; bounds: GPRECT;  
    matrix: GPMATRIX; pen: GPPEN): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetPathWorldBoundsI}

GdipIsVisiblePathPoint: function(path: GPPATH; x: Single; y: Single;  
    graphics: GPGRAPHICS; out result: Bool): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipIsVisiblePathPoint}

GdipIsVisiblePathPointI: function(path: GPPATH; x: Integer; y: Integer;  
    graphics: GPGRAPHICS; out result: Bool): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipIsVisiblePathPointI}

GdipIsOutlineVisiblePathPoint: function(path: GPPATH; x: Single; y: Single;  
    pen: GPPEN; graphics: GPGRAPHICS; out result: Bool): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipIsOutlineVisiblePathPoint}

GdipIsOutlineVisiblePathPointI: function(path: GPPATH; x: Integer; y: Integer;  
    pen: GPPEN; graphics: GPGRAPHICS; out result: Bool): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipIsOutlineVisiblePathPointI}

//----------------------------------------------------------------------------
// PathIterator APIs 
//----------------------------------------------------------------------------

GdipCreatePathIter: function(out iterator: GPPATHITERATOR;  
    path: GPPATH): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreatePathIter}

GdipDeletePathIter: function(iterator: GPPATHITERATOR): GPSTATUS; stdcall;  
  {.$EXTERNALSYM GdipDeletePathIter}

GdipPathIterNextSubpath: function(iterator: GPPATHITERATOR;  
    var resultCount: Integer; var startIndex: Integer; var endIndex: Integer;
    out isClosed: Bool): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipPathIterNextSubpath}

GdipPathIterNextSubpathPath: function(iterator: GPPATHITERATOR;  
    var resultCount: Integer; path: GPPATH;
    out isClosed: Bool): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipPathIterNextSubpathPath}

GdipPathIterNextPathType: function(iterator: GPPATHITERATOR;  
    var resultCount: Integer; pathType: PBYTE; var startIndex: Integer;
    var endIndex: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipPathIterNextPathType}

GdipPathIterNextMarker: function(iterator: GPPATHITERATOR;  
    var resultCount: Integer; var startIndex: Integer;
    var endIndex: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipPathIterNextMarker}

GdipPathIterNextMarkerPath: function(iterator: GPPATHITERATOR;  
    var resultCount: Integer; path: GPPATH): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipPathIterNextMarkerPath}

GdipPathIterGetCount: function(iterator: GPPATHITERATOR;  
    out count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipPathIterGetCount}

GdipPathIterGetSubpathCount: function(iterator: GPPATHITERATOR;
    out count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipPathIterGetSubpathCount}

GdipPathIterIsValid: function(iterator: GPPATHITERATOR;  
    out valid: Bool): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipPathIterIsValid}

GdipPathIterHasCurve: function(iterator: GPPATHITERATOR;  
    out hasCurve: Bool): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipPathIterHasCurve}

GdipPathIterRewind: function(iterator: GPPATHITERATOR): GPSTATUS; stdcall;  
  {.$EXTERNALSYM GdipPathIterRewind}

GdipPathIterEnumerate: function(iterator: GPPATHITERATOR;  
    var resultCount: Integer; points: GPPOINTF; types: PBYTE;
    count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipPathIterEnumerate}

GdipPathIterCopyData: function(iterator: GPPATHITERATOR;  
    var resultCount: Integer; points: GPPOINTF; types: PBYTE;
    startIndex: Integer; endIndex: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipPathIterCopyData}

//----------------------------------------------------------------------------
// Matrix APIs
//----------------------------------------------------------------------------

GdipCreateMatrix: function(out matrix: GPMATRIX): GPSTATUS; stdcall;  
  {.$EXTERNALSYM GdipCreateMatrix}

GdipCreateMatrix2: function(m11: Single; m12: Single; m21: Single; m22: Single;  
    dx: Single; dy: Single; out matrix: GPMATRIX): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreateMatrix2}

GdipCreateMatrix3: function(rect: GPRECTF; dstplg: GPPOINTF;  
    out matrix: GPMATRIX): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreateMatrix3}

GdipCreateMatrix3I: function(rect: GPRECT; dstplg: GPPOINT;  
    out matrix: GPMATRIX): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreateMatrix3I}

GdipCloneMatrix: function(matrix: GPMATRIX;
    out cloneMatrix: GPMATRIX): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCloneMatrix}

GdipDeleteMatrix: function(matrix: GPMATRIX): GPSTATUS; stdcall;  
  {.$EXTERNALSYM GdipDeleteMatrix}

GdipSetMatrixElements: function(matrix: GPMATRIX; m11: Single; m12: Single;  
    m21: Single; m22: Single; dx: Single; dy: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetMatrixElements}

GdipMultiplyMatrix: function(matrix: GPMATRIX; matrix2: GPMATRIX;  
    order: GPMATRIXORDER): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipMultiplyMatrix}

GdipTranslateMatrix: function(matrix: GPMATRIX; offsetX: Single;  
    offsetY: Single; order: GPMATRIXORDER): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipTranslateMatrix}

GdipScaleMatrix: function(matrix: GPMATRIX; scaleX: Single; scaleY: Single;  
    order: GPMATRIXORDER): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipScaleMatrix}

GdipRotateMatrix: function(matrix: GPMATRIX; angle: Single;  
    order: GPMATRIXORDER): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipRotateMatrix}

GdipShearMatrix: function(matrix: GPMATRIX; shearX: Single; shearY: Single;  
    order: GPMATRIXORDER): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipShearMatrix}

GdipInvertMatrix: function(matrix: GPMATRIX): GPSTATUS; stdcall;  
  {.$EXTERNALSYM GdipInvertMatrix}

GdipTransformMatrixPoints: function(matrix: GPMATRIX; pts: GPPOINTF;  
    count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipTransformMatrixPoints}

GdipTransformMatrixPointsI: function(matrix: GPMATRIX; pts: GPPOINT;  
    count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipTransformMatrixPointsI}

GdipVectorTransformMatrixPoints: function(matrix: GPMATRIX; pts: GPPOINTF;
    count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipVectorTransformMatrixPoints}

GdipVectorTransformMatrixPointsI: function(matrix: GPMATRIX; pts: GPPOINT;  
    count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipVectorTransformMatrixPointsI}

GdipGetMatrixElements: function(matrix: GPMATRIX;  
    matrixOut: PSingle): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetMatrixElements}

GdipIsMatrixInvertible: function(matrix: GPMATRIX;  
    out result: Bool): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipIsMatrixInvertible}

GdipIsMatrixIdentity: function(matrix: GPMATRIX;  
    out result: Bool): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipIsMatrixIdentity}

GdipIsMatrixEqual: function(matrix: GPMATRIX; matrix2: GPMATRIX;  
    out result: Bool): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipIsMatrixEqual}

//----------------------------------------------------------------------------
// Region APIs
//----------------------------------------------------------------------------

GdipCreateRegion: function(out region: GPREGION): GPSTATUS; stdcall;  
  {.$EXTERNALSYM GdipCreateRegion}

GdipCreateRegionRect: function(rect: GPRECTF;  
    out region: GPREGION): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreateRegionRect}

GdipCreateRegionRectI: function(rect: GPRECT;  
    out region: GPREGION): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreateRegionRectI}

GdipCreateRegionPath: function(path: GPPATH;  
    out region: GPREGION): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreateRegionPath}

GdipCreateRegionRgnData: function(regionData: PBYTE; size: Integer;  
    out region: GPREGION): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreateRegionRgnData}

GdipCreateRegionHrgn: function(hRgn: HRGN;  
    out region: GPREGION): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreateRegionHrgn}

GdipCloneRegion: function(region: GPREGION;  
    out cloneRegion: GPREGION): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCloneRegion}

GdipDeleteRegion: function(region: GPREGION): GPSTATUS; stdcall;  
  {.$EXTERNALSYM GdipDeleteRegion}

GdipSetInfinite: function(region: GPREGION): GPSTATUS; stdcall;  
  {.$EXTERNALSYM GdipSetInfinite}

GdipSetEmpty: function(region: GPREGION): GPSTATUS; stdcall;  
  {.$EXTERNALSYM GdipSetEmpty}

GdipCombineRegionRect: function(region: GPREGION; rect: GPRECTF;  
    combineMode: COMBINEMODE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCombineRegionRect}

GdipCombineRegionRectI: function(region: GPREGION; rect: GPRECT;  
    combineMode: COMBINEMODE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCombineRegionRectI}

GdipCombineRegionPath: function(region: GPREGION; path: GPPATH;  
    combineMode: COMBINEMODE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCombineRegionPath}

GdipCombineRegionRegion: function(region: GPREGION; region2: GPREGION;
    combineMode: COMBINEMODE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCombineRegionRegion}

GdipTranslateRegion: function(region: GPREGION; dx: Single;  
    dy: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipTranslateRegion}

GdipTranslateRegionI: function(region: GPREGION; dx: Integer;
    dy: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipTranslateRegionI}

GdipTransformRegion: function(region: GPREGION;  
    matrix: GPMATRIX): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipTransformRegion}

GdipGetRegionBounds: function(region: GPREGION; graphics: GPGRAPHICS;  
    rect: GPRECTF): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetRegionBounds}

GdipGetRegionBoundsI: function(region: GPREGION; graphics: GPGRAPHICS;  
    rect: GPRECT): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetRegionBoundsI}

GdipGetRegionHRgn: function(region: GPREGION; graphics: GPGRAPHICS;  
    out hRgn: HRGN): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetRegionHRgn}

GdipIsEmptyRegion: function(region: GPREGION; graphics: GPGRAPHICS;  
    out result: Bool): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipIsEmptyRegion}

GdipIsInfiniteRegion: function(region: GPREGION; graphics: GPGRAPHICS;  
    out result: Bool): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipIsInfiniteRegion}

GdipIsEqualRegion: function(region: GPREGION; region2: GPREGION;  
    graphics: GPGRAPHICS; out result: Bool): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipIsEqualRegion}

GdipGetRegionDataSize: function(region: GPREGION;  
    out bufferSize: UINT): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetRegionDataSize}

GdipGetRegionData: function(region: GPREGION; buffer: PBYTE;  
    bufferSize: UINT; sizeFilled: PUINT): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetRegionData}

GdipIsVisibleRegionPoint: function(region: GPREGION; x: Single; y: Single;  
    graphics: GPGRAPHICS; out result: Bool): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipIsVisibleRegionPoint}

GdipIsVisibleRegionPointI: function(region: GPREGION; x: Integer; y: Integer;  
    graphics: GPGRAPHICS; out result: Bool): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipIsVisibleRegionPointI}

GdipIsVisibleRegionRect: function(region: GPREGION; x: Single; y: Single;  
    width: Single; height: Single; graphics: GPGRAPHICS;
    out result: Bool): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipIsVisibleRegionRect}

GdipIsVisibleRegionRectI: function(region: GPREGION; x: Integer; y: Integer;  
    width: Integer; height: Integer; graphics: GPGRAPHICS;
    out result: Bool): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipIsVisibleRegionRectI}

GdipGetRegionScansCount: function(region: GPREGION; out count: UINT;  
    matrix: GPMATRIX): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetRegionScansCount}

GdipGetRegionScans: function(region: GPREGION; rects: GPRECTF;  
    out count: Integer; matrix: GPMATRIX): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetRegionScans}

GdipGetRegionScansI: function(region: GPREGION; rects: GPRECT;  
    out count: Integer; matrix: GPMATRIX): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetRegionScansI}

//----------------------------------------------------------------------------
// Brush APIs
//----------------------------------------------------------------------------

GdipCloneBrush: function(brush: GPBRUSH;  
    out cloneBrush: GPBRUSH): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCloneBrush}

GdipDeleteBrush: function(brush: GPBRUSH): GPSTATUS; stdcall;  
  {.$EXTERNALSYM GdipDeleteBrush}

GdipGetBrushType: function(brush: GPBRUSH;  
    out type_: GPBRUSHTYPE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetBrushType}

//----------------------------------------------------------------------------
// HatchBrush APIs
//----------------------------------------------------------------------------

GdipCreateHatchBrush: function(hatchstyle: Integer; forecol: ARGB;  
    backcol: ARGB; out brush: GPHATCH): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreateHatchBrush}

GdipGetHatchStyle: function(brush: GPHATCH;  
    out hatchstyle: GPHATCHSTYLE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetHatchStyle}

GdipGetHatchForegroundColor: function(brush: GPHATCH;  
    out forecol: ARGB): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetHatchForegroundColor}

GdipGetHatchBackgroundColor: function(brush: GPHATCH;  
    out backcol: ARGB): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetHatchBackgroundColor}

//----------------------------------------------------------------------------
// TextureBrush APIs
//----------------------------------------------------------------------------


GdipCreateTexture: function(image: GPIMAGE; wrapmode: GPWRAPMODE;  
    var texture: GPTEXTURE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreateTexture}

GdipCreateTexture2: function(image: GPIMAGE; wrapmode: GPWRAPMODE;  
    x: Single; y: Single; width: Single; height: Single;
    out texture: GPTEXTURE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreateTexture2}

GdipCreateTextureIA: function(image: GPIMAGE;  
    imageAttributes: GPIMAGEATTRIBUTES; x: Single; y: Single; width: Single;
    height: Single; out texture: GPTEXTURE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreateTextureIA}

GdipCreateTexture2I: function(image: GPIMAGE; wrapmode: GPWRAPMODE; x: Integer;  
    y: Integer; width: Integer; height: Integer;
    out texture: GPTEXTURE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreateTexture2I}

GdipCreateTextureIAI: function(image: GPIMAGE;  
    imageAttributes: GPIMAGEATTRIBUTES; x: Integer; y: Integer; width: Integer;
    height: Integer; out texture: GPTEXTURE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreateTextureIAI}

GdipGetTextureTransform: function(brush: GPTEXTURE;  
    matrix: GPMATRIX): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetTextureTransform}

GdipSetTextureTransform: function(brush: GPTEXTURE;  
    matrix: GPMATRIX): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetTextureTransform}

GdipResetTextureTransform: function(brush: GPTEXTURE): GPSTATUS; stdcall;  
  {.$EXTERNALSYM GdipResetTextureTransform}

GdipMultiplyTextureTransform: function(brush: GPTEXTURE; matrix: GPMATRIX;  
    order: GPMATRIXORDER): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipMultiplyTextureTransform}

GdipTranslateTextureTransform: function(brush: GPTEXTURE; dx: Single;  
    dy: Single; order: GPMATRIXORDER): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipTranslateTextureTransform}

GdipScaleTextureTransform: function(brush: GPTEXTURE; sx: Single; sy: Single;  
    order: GPMATRIXORDER): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipScaleTextureTransform}

GdipRotateTextureTransform: function(brush: GPTEXTURE; angle: Single;  
    order: GPMATRIXORDER): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipRotateTextureTransform}

GdipSetTextureWrapMode: function(brush: GPTEXTURE;  
    wrapmode: GPWRAPMODE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetTextureWrapMode}

GdipGetTextureWrapMode: function(brush: GPTEXTURE;  
    var wrapmode: GPWRAPMODE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetTextureWrapMode}

GdipGetTextureImage: function(brush: GPTEXTURE;  
    out image: GPIMAGE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetTextureImage}

//----------------------------------------------------------------------------
// SolidBrush APIs
//----------------------------------------------------------------------------

GdipCreateSolidFill: function(color: ARGB;  
    out brush: GPSOLIDFILL): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreateSolidFill}

GdipSetSolidFillColor: function(brush: GPSOLIDFILL;
    color: ARGB): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetSolidFillColor}

GdipGetSolidFillColor: function(brush: GPSOLIDFILL;  
    out color: ARGB): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetSolidFillColor}

//----------------------------------------------------------------------------
// LineBrush APIs
//----------------------------------------------------------------------------

GdipCreateLineBrush: function(point1: GPPOINTF; point2: GPPOINTF; color1: ARGB;  
    color2: ARGB; wrapMode: GPWRAPMODE;
    out lineGradient: GPLINEGRADIENT): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreateLineBrush}

GdipCreateLineBrushI: function(point1: GPPOINT; point2: GPPOINT; color1: ARGB;  
    color2: ARGB; wrapMode: GPWRAPMODE;
    out lineGradient: GPLINEGRADIENT): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreateLineBrushI}

GdipCreateLineBrushFromRect: function(rect: GPRECTF; color1: ARGB;  
    color2: ARGB; mode: LINEARGRADIENTMODE; wrapMode: GPWRAPMODE;
    out lineGradient: GPLINEGRADIENT): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreateLineBrushFromRect}

GdipCreateLineBrushFromRectI: function(rect: GPRECT; color1: ARGB;
    color2: ARGB; mode: LINEARGRADIENTMODE; wrapMode: GPWRAPMODE;
    out lineGradient: GPLINEGRADIENT): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreateLineBrushFromRectI}

GdipCreateLineBrushFromRectWithAngle: function(rect: GPRECTF; color1: ARGB;  
    color2: ARGB; angle: Single; isAngleScalable: Bool; wrapMode: GPWRAPMODE;
    out lineGradient: GPLINEGRADIENT): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreateLineBrushFromRectWithAngle}

GdipCreateLineBrushFromRectWithAngleI: function(rect: GPRECT; color1: ARGB;  
    color2: ARGB; angle: Single; isAngleScalable: Bool; wrapMode: GPWRAPMODE;
    out lineGradient: GPLINEGRADIENT): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreateLineBrushFromRectWithAngleI}

GdipSetLineColors: function(brush: GPLINEGRADIENT; color1: ARGB;  
    color2: ARGB): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetLineColors}

GdipGetLineColors: function(brush: GPLINEGRADIENT;  
    colors: PARGB): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetLineColors}

GdipGetLineRect: function(brush: GPLINEGRADIENT;  
    rect: GPRECTF): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetLineRect}

GdipGetLineRectI: function(brush: GPLINEGRADIENT;
    rect: GPRECT): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetLineRectI}

GdipSetLineGammaCorrection: function(brush: GPLINEGRADIENT;  
    useGammaCorrection: Bool): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetLineGammaCorrection}

GdipGetLineGammaCorrection: function(brush: GPLINEGRADIENT;  
    out useGammaCorrection: Bool): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetLineGammaCorrection}

GdipGetLineBlendCount: function(brush: GPLINEGRADIENT;  
    out count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetLineBlendCount}

GdipGetLineBlend: function(brush: GPLINEGRADIENT; blend: PSingle;  
    positions: PSingle; count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetLineBlend}

GdipSetLineBlend: function(brush: GPLINEGRADIENT; blend: PSingle;  
    positions: PSingle; count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetLineBlend}

GdipGetLinePresetBlendCount: function(brush: GPLINEGRADIENT;  
    out count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetLinePresetBlendCount}

GdipGetLinePresetBlend: function(brush: GPLINEGRADIENT; blend: PARGB;  
    positions: PSingle; count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetLinePresetBlend}

GdipSetLinePresetBlend: function(brush: GPLINEGRADIENT; blend: PARGB;  
    positions: PSingle; count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetLinePresetBlend}

GdipSetLineSigmaBlend: function(brush: GPLINEGRADIENT; focus: Single;  
    scale: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetLineSigmaBlend}

GdipSetLineLinearBlend: function(brush: GPLINEGRADIENT; focus: Single;  
    scale: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetLineLinearBlend}

GdipSetLineWrapMode: function(brush: GPLINEGRADIENT;  
    wrapmode: GPWRAPMODE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetLineWrapMode}

GdipGetLineWrapMode: function(brush: GPLINEGRADIENT;  
    out wrapmode: GPWRAPMODE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetLineWrapMode}

GdipGetLineTransform: function(brush: GPLINEGRADIENT;  
    matrix: GPMATRIX): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetLineTransform}

GdipSetLineTransform: function(brush: GPLINEGRADIENT;  
    matrix: GPMATRIX): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetLineTransform}

GdipResetLineTransform: function(brush: GPLINEGRADIENT): GPSTATUS; stdcall;  
  {.$EXTERNALSYM GdipResetLineTransform}

GdipMultiplyLineTransform: function(brush: GPLINEGRADIENT; matrix: GPMATRIX;  
    order: GPMATRIXORDER): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipMultiplyLineTransform}

GdipTranslateLineTransform: function(brush: GPLINEGRADIENT; dx: Single;  
    dy: Single; order: GPMATRIXORDER): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipTranslateLineTransform}

GdipScaleLineTransform: function(brush: GPLINEGRADIENT; sx: Single; sy: Single;  
    order: GPMATRIXORDER): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipScaleLineTransform}

GdipRotateLineTransform: function(brush: GPLINEGRADIENT; angle: Single;  
    order: GPMATRIXORDER): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipRotateLineTransform}

//----------------------------------------------------------------------------
// PathGradientBrush APIs
//----------------------------------------------------------------------------

GdipCreatePathGradient: function(points: GPPOINTF; count: Integer;  
    wrapMode: GPWRAPMODE; out polyGradient: GPPATHGRADIENT): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreatePathGradient}

GdipCreatePathGradientI: function(points: GPPOINT; count: Integer;  
    wrapMode: GPWRAPMODE; out polyGradient: GPPATHGRADIENT): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreatePathGradientI}

GdipCreatePathGradientFromPath: function(path: GPPATH;  
    out polyGradient: GPPATHGRADIENT): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreatePathGradientFromPath}

GdipGetPathGradientCenterColor: function(brush: GPPATHGRADIENT;  
    out colors: ARGB): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetPathGradientCenterColor}

GdipSetPathGradientCenterColor: function(brush: GPPATHGRADIENT;  
    colors: ARGB): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetPathGradientCenterColor}

GdipGetPathGradientSurroundColorsWithCount: function(brush: GPPATHGRADIENT;  
    color: PARGB; var count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetPathGradientSurroundColorsWithCount}

GdipSetPathGradientSurroundColorsWithCount: function(brush: GPPATHGRADIENT;  
    color: PARGB; var count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetPathGradientSurroundColorsWithCount}

GdipGetPathGradientPath: function(brush: GPPATHGRADIENT;  
    path: GPPATH): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetPathGradientPath}

GdipSetPathGradientPath: function(brush: GPPATHGRADIENT;  
    path: GPPATH): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetPathGradientPath}

GdipGetPathGradientCenterPoint: function(brush: GPPATHGRADIENT;  
    points: GPPOINTF): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetPathGradientCenterPoint}

GdipGetPathGradientCenterPointI: function(brush: GPPATHGRADIENT;  
    points: GPPOINT): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetPathGradientCenterPointI}

GdipSetPathGradientCenterPoint: function(brush: GPPATHGRADIENT;  
    points: GPPOINTF): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetPathGradientCenterPoint}

GdipSetPathGradientCenterPointI: function(brush: GPPATHGRADIENT;  
    points: GPPOINT): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetPathGradientCenterPointI}

GdipGetPathGradientRect: function(brush: GPPATHGRADIENT;
    rect: GPRECTF): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetPathGradientRect}

GdipGetPathGradientRectI: function(brush: GPPATHGRADIENT;  
    rect: GPRECT): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetPathGradientRectI}

GdipGetPathGradientPointCount: function(brush: GPPATHGRADIENT;
    var count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetPathGradientPointCount}

GdipGetPathGradientSurroundColorCount: function(brush: GPPATHGRADIENT;  
    var count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetPathGradientSurroundColorCount}

GdipSetPathGradientGammaCorrection: function(brush: GPPATHGRADIENT;  
    useGammaCorrection: Bool): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetPathGradientGammaCorrection}

GdipGetPathGradientGammaCorrection: function(brush: GPPATHGRADIENT;  
    var useGammaCorrection: Bool): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetPathGradientGammaCorrection}

GdipGetPathGradientBlendCount: function(brush: GPPATHGRADIENT;  
    var count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetPathGradientBlendCount}

GdipGetPathGradientBlend: function(brush: GPPATHGRADIENT;  
    blend: PSingle; positions: PSingle; count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetPathGradientBlend}

GdipSetPathGradientBlend: function(brush: GPPATHGRADIENT;  
    blend: PSingle; positions: PSingle; count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetPathGradientBlend}

GdipGetPathGradientPresetBlendCount: function(brush: GPPATHGRADIENT;  
    var count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetPathGradientPresetBlendCount}

GdipGetPathGradientPresetBlend: function(brush: GPPATHGRADIENT;  
    blend: PARGB; positions: PSingle; count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetPathGradientPresetBlend}

GdipSetPathGradientPresetBlend: function(brush: GPPATHGRADIENT;  
    blend: PARGB; positions: PSingle; count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetPathGradientPresetBlend}

GdipSetPathGradientSigmaBlend: function(brush: GPPATHGRADIENT;  
    focus: Single; scale: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetPathGradientSigmaBlend}

GdipSetPathGradientLinearBlend: function(brush: GPPATHGRADIENT;  
    focus: Single; scale: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetPathGradientLinearBlend}

GdipGetPathGradientWrapMode: function(brush: GPPATHGRADIENT;  
    var wrapmode: GPWRAPMODE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetPathGradientWrapMode}

GdipSetPathGradientWrapMode: function(brush: GPPATHGRADIENT;  
    wrapmode: GPWRAPMODE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetPathGradientWrapMode}

GdipGetPathGradientTransform: function(brush: GPPATHGRADIENT;  
    matrix: GPMATRIX): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetPathGradientTransform}

GdipSetPathGradientTransform: function(brush: GPPATHGRADIENT;  
    matrix: GPMATRIX): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetPathGradientTransform}

GdipResetPathGradientTransform: function(  
    brush: GPPATHGRADIENT): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipResetPathGradientTransform}

GdipMultiplyPathGradientTransform: function(brush: GPPATHGRADIENT;  
    matrix: GPMATRIX; order: GPMATRIXORDER): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipMultiplyPathGradientTransform}

GdipTranslatePathGradientTransform: function(brush: GPPATHGRADIENT;  
    dx: Single; dy: Single; order: GPMATRIXORDER): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipTranslatePathGradientTransform}

GdipScalePathGradientTransform: function(brush: GPPATHGRADIENT;  
    sx: Single; sy: Single; order: GPMATRIXORDER): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipScalePathGradientTransform}

GdipRotatePathGradientTransform: function(brush: GPPATHGRADIENT;  
    angle: Single; order: GPMATRIXORDER): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipRotatePathGradientTransform}

GdipGetPathGradientFocusScales: function(brush: GPPATHGRADIENT;
    var xScale: Single; var yScale: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetPathGradientFocusScales}

GdipSetPathGradientFocusScales: function(brush: GPPATHGRADIENT;  
    xScale: Single; yScale: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetPathGradientFocusScales}

//----------------------------------------------------------------------------
// Pen APIs
//----------------------------------------------------------------------------

GdipCreatePen1: function(color: ARGB; width: Single; unit_: GPUNIT;  
    out pen: GPPEN): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreatePen1}

GdipCreatePen2: function(brush: GPBRUSH; width: Single; unit_: GPUNIT;
    out pen: GPPEN): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreatePen2}

GdipClonePen: function(pen: GPPEN; out clonepen: GPPEN): GPSTATUS; stdcall;  
  {.$EXTERNALSYM GdipClonePen}

GdipDeletePen: function(pen: GPPEN): GPSTATUS; stdcall;  
  {.$EXTERNALSYM GdipDeletePen}

GdipSetPenWidth: function(pen: GPPEN; width: Single): GPSTATUS; stdcall;  
  {.$EXTERNALSYM GdipSetPenWidth}

GdipGetPenWidth: function(pen: GPPEN; out width: Single): GPSTATUS; stdcall;  
  {.$EXTERNALSYM GdipGetPenWidth}

GdipSetPenUnit: function(pen: GPPEN; unit_: GPUNIT): GPSTATUS; stdcall;  
  {.$EXTERNALSYM GdipSetPenUnit}

GdipGetPenUnit: function(pen: GPPEN; var unit_: GPUNIT): GPSTATUS; stdcall;  
  {.$EXTERNALSYM GdipGetPenUnit}

GdipSetPenLineCap197819: function(pen: GPPEN; startCap: GPLINECAP;  
    endCap: GPLINECAP; dashCap: GPDASHCAP): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetPenLineCap197819}

GdipSetPenStartCap: function(pen: GPPEN;
    startCap: GPLINECAP): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetPenStartCap}

GdipSetPenEndCap: function(pen: GPPEN; endCap: GPLINECAP): GPSTATUS; stdcall;  
  {.$EXTERNALSYM GdipSetPenEndCap}

GdipSetPenDashCap197819: function(pen: GPPEN;  
    dashCap: GPDASHCAP): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetPenDashCap197819}

GdipGetPenStartCap: function(pen: GPPEN;  
    out startCap: GPLINECAP): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetPenStartCap}

GdipGetPenEndCap: function(pen: GPPEN;  
    out endCap: GPLINECAP): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetPenEndCap}

GdipGetPenDashCap197819: function(pen: GPPEN;  
    out dashCap: GPDASHCAP): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetPenDashCap197819}

GdipSetPenLineJoin: function(pen: GPPEN;  
    lineJoin: GPLINEJOIN): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetPenLineJoin}

GdipGetPenLineJoin: function(pen: GPPEN;  
    var lineJoin: GPLINEJOIN): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetPenLineJoin}

GdipSetPenCustomStartCap: function(pen: GPPEN;  
    customCap: GPCUSTOMLINECAP): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetPenCustomStartCap}

GdipGetPenCustomStartCap: function(pen: GPPEN;  
    out customCap: GPCUSTOMLINECAP): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetPenCustomStartCap}

GdipSetPenCustomEndCap: function(pen: GPPEN;  
    customCap: GPCUSTOMLINECAP): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetPenCustomEndCap}

GdipGetPenCustomEndCap: function(pen: GPPEN;  
    out customCap: GPCUSTOMLINECAP): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetPenCustomEndCap}

GdipSetPenMiterLimit: function(pen: GPPEN;  
    miterLimit: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetPenMiterLimit}

GdipGetPenMiterLimit: function(pen: GPPEN;  
    out miterLimit: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetPenMiterLimit}

GdipSetPenMode: function(pen: GPPEN;
    penMode: GPPENALIGNMENT): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetPenMode}

GdipGetPenMode: function(pen: GPPEN;  
    var penMode: GPPENALIGNMENT): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetPenMode}

GdipSetPenTransform: function(pen: GPPEN;  
    matrix: GPMATRIX): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetPenTransform}

GdipGetPenTransform: function(pen: GPPEN;  
    matrix: GPMATRIX): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetPenTransform}

GdipResetPenTransform: function(pen: GPPEN): GPSTATUS; stdcall;  
  {.$EXTERNALSYM GdipResetPenTransform}

GdipMultiplyPenTransform: function(pen: GPPEN; matrix: GPMATRIX;  
    order: GPMATRIXORDER): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipMultiplyPenTransform}

GdipTranslatePenTransform: function(pen: GPPEN; dx: Single; dy: Single;  
    order: GPMATRIXORDER): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipTranslatePenTransform}

GdipScalePenTransform: function(pen: GPPEN; sx: Single; sy: Single;
    order: GPMATRIXORDER): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipScalePenTransform}

GdipRotatePenTransform: function(pen: GPPEN; angle: Single;  
    order: GPMATRIXORDER): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipRotatePenTransform}

GdipSetPenColor: function(pen: GPPEN; argb: ARGB): GPSTATUS; stdcall;  
  {.$EXTERNALSYM GdipSetPenColor}

GdipGetPenColor: function(pen: GPPEN; out argb: ARGB): GPSTATUS; stdcall;  
  {.$EXTERNALSYM GdipGetPenColor}

GdipSetPenBrushFill: function(pen: GPPEN; brush: GPBRUSH): GPSTATUS; stdcall;  
  {.$EXTERNALSYM GdipSetPenBrushFill}

GdipGetPenBrushFill: function(pen: GPPEN;  
    out brush: GPBRUSH): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetPenBrushFill}

GdipGetPenFillType: function(pen: GPPEN;  
    out type_: GPPENTYPE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetPenFillType}

GdipGetPenDashStyle: function(pen: GPPEN;  
    out dashstyle: GPDASHSTYLE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetPenDashStyle}

GdipSetPenDashStyle: function(pen: GPPEN;  
    dashstyle: GPDASHSTYLE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetPenDashStyle}

GdipGetPenDashOffset: function(pen: GPPEN;  
    out offset: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetPenDashOffset}

GdipSetPenDashOffset: function(pen: GPPEN; offset: Single): GPSTATUS; stdcall;  
  {.$EXTERNALSYM GdipSetPenDashOffset}

GdipGetPenDashCount: function(pen: GPPEN;  
    var count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetPenDashCount}

GdipSetPenDashArray: function(pen: GPPEN; dash: PSingle;
    count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetPenDashArray}

GdipGetPenDashArray: function(pen: GPPEN; dash: PSingle;  
    count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetPenDashArray}

GdipGetPenCompoundCount: function(pen: GPPEN;  
    out count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetPenCompoundCount}

GdipSetPenCompoundArray: function(pen: GPPEN; dash: PSingle;  
    count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetPenCompoundArray}

GdipGetPenCompoundArray: function(pen: GPPEN; dash: PSingle;  
    count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetPenCompoundArray}

//----------------------------------------------------------------------------
// CustomLineCap APIs
//----------------------------------------------------------------------------

GdipCreateCustomLineCap: function(fillPath: GPPATH; strokePath: GPPATH;  
    baseCap: GPLINECAP; baseInset: Single;
    out customCap: GPCUSTOMLINECAP): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreateCustomLineCap}

GdipDeleteCustomLineCap: function(  
    customCap: GPCUSTOMLINECAP): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipDeleteCustomLineCap}

GdipCloneCustomLineCap: function(customCap: GPCUSTOMLINECAP;  
    out clonedCap: GPCUSTOMLINECAP): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCloneCustomLineCap}

GdipGetCustomLineCapType: function(customCap: GPCUSTOMLINECAP;
    var capType: CUSTOMLINECAPTYPE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetCustomLineCapType}

GdipSetCustomLineCapStrokeCaps: function(customCap: GPCUSTOMLINECAP;  
    startCap: GPLINECAP; endCap: GPLINECAP): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetCustomLineCapStrokeCaps}

GdipGetCustomLineCapStrokeCaps: function(customCap: GPCUSTOMLINECAP;  
    var startCap: GPLINECAP; var endCap: GPLINECAP): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetCustomLineCapStrokeCaps}

GdipSetCustomLineCapStrokeJoin: function(customCap: GPCUSTOMLINECAP;  
  lineJoin: GPLINEJOIN): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetCustomLineCapStrokeJoin}

GdipGetCustomLineCapStrokeJoin: function(customCap: GPCUSTOMLINECAP;  
  var lineJoin: GPLINEJOIN): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetCustomLineCapStrokeJoin}

GdipSetCustomLineCapBaseCap: function(customCap: GPCUSTOMLINECAP;  
  baseCap: GPLINECAP): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetCustomLineCapBaseCap}

GdipGetCustomLineCapBaseCap: function(customCap: GPCUSTOMLINECAP;  
  var baseCap: GPLINECAP): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetCustomLineCapBaseCap}

GdipSetCustomLineCapBaseInset: function(customCap: GPCUSTOMLINECAP;  
  inset: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetCustomLineCapBaseInset}

GdipGetCustomLineCapBaseInset: function(customCap: GPCUSTOMLINECAP;  
  var inset: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetCustomLineCapBaseInset}

GdipSetCustomLineCapWidthScale: function(customCap: GPCUSTOMLINECAP;  
  widthScale: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetCustomLineCapWidthScale}

GdipGetCustomLineCapWidthScale: function(customCap: GPCUSTOMLINECAP;  
  var widthScale: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetCustomLineCapWidthScale}

//----------------------------------------------------------------------------
// AdjustableArrowCap APIs
//----------------------------------------------------------------------------

GdipCreateAdjustableArrowCap: function(height: Single;  
  width: Single;
  isFilled: Bool;
  out cap: GPADJUSTABLEARROWCAP): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreateAdjustableArrowCap}

GdipSetAdjustableArrowCapHeight: function(cap: GPADJUSTABLEARROWCAP;
  height: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetAdjustableArrowCapHeight}

GdipGetAdjustableArrowCapHeight: function(cap: GPADJUSTABLEARROWCAP;  
  var height: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetAdjustableArrowCapHeight}

GdipSetAdjustableArrowCapWidth: function(cap: GPADJUSTABLEARROWCAP;  
  width: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetAdjustableArrowCapWidth}

GdipGetAdjustableArrowCapWidth: function(cap: GPADJUSTABLEARROWCAP;  
  var width: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetAdjustableArrowCapWidth}

GdipSetAdjustableArrowCapMiddleInset: function(cap: GPADJUSTABLEARROWCAP;  
  middleInset: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetAdjustableArrowCapMiddleInset}

GdipGetAdjustableArrowCapMiddleInset: function(cap: GPADJUSTABLEARROWCAP;  
  var middleInset: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetAdjustableArrowCapMiddleInset}

GdipSetAdjustableArrowCapFillState: function(cap: GPADJUSTABLEARROWCAP;  
  fillState: Bool): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetAdjustableArrowCapFillState}

GdipGetAdjustableArrowCapFillState: function(cap: GPADJUSTABLEARROWCAP;  
  var fillState: Bool): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetAdjustableArrowCapFillState}

//---------------------------------------------------------------------------- 
// Image APIs
//----------------------------------------------------------------------------

GdipLoadImageFromStream: function(stream: ISTREAM;  
  out image: GPIMAGE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipLoadImageFromStream}

GdipLoadImageFromFile: function(filename: PWCHAR;  
  out image: GPIMAGE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipLoadImageFromFile}

GdipLoadImageFromStreamICM: function(stream: ISTREAM;  
  out image: GPIMAGE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipLoadImageFromStreamICM}

GdipLoadImageFromFileICM: function(filename: PWCHAR;  
  out image: GPIMAGE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipLoadImageFromFileICM}

GdipCloneImage: function(image: GPIMAGE;  
  out cloneImage: GPIMAGE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCloneImage}

GdipDisposeImage: function(image: GPIMAGE): GPSTATUS; stdcall;  
  {.$EXTERNALSYM GdipDisposeImage}

GdipSaveImageToFile: function(image: GPIMAGE;  
  filename: PWCHAR;
  clsidEncoder: PGUID;
  encoderParams: PENCODERPARAMETERS): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSaveImageToFile}

GdipSaveImageToStream: function(image: GPIMAGE;  
  stream: ISTREAM;
  clsidEncoder: PGUID;
  encoderParams: PENCODERPARAMETERS): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSaveImageToStream}

GdipSaveAdd: function(image: GPIMAGE;  
  encoderParams: PENCODERPARAMETERS): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSaveAdd}

GdipSaveAddImage: function(image: GPIMAGE;  
  newImage: GPIMAGE;
  encoderParams: PENCODERPARAMETERS): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSaveAddImage}

GdipGetImageGraphicsContext: function(image: GPIMAGE;  
  out graphics: GPGRAPHICS): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetImageGraphicsContext}

GdipGetImageBounds: function(image: GPIMAGE;  
  srcRect: GPRECTF;
  var srcUnit: GPUNIT): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetImageBounds}

GdipGetImageDimension: function(image: GPIMAGE;  
  var width: Single;
  var height: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetImageDimension}

GdipGetImageType: function(image: GPIMAGE;  
  var type_: IMAGETYPE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetImageType}

GdipGetImageWidth: function(image: GPIMAGE;  
  var width: UINT): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetImageWidth}

GdipGetImageHeight: function(image: GPIMAGE;  
  var height: UINT): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetImageHeight}

GdipGetImageHorizontalResolution: function(image: GPIMAGE;  
  var resolution: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetImageHorizontalResolution}

GdipGetImageVerticalResolution: function(image: GPIMAGE;  
  var resolution: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetImageVerticalResolution}

GdipGetImageFlags: function(image: GPIMAGE;  
  var flags: UINT): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetImageFlags}

GdipGetImageRawFormat: function(image: GPIMAGE;  
  format: PGUID): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetImageRawFormat}

GdipGetImagePixelFormat: function(image: GPIMAGE;  
  out format: TPIXELFORMAT): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetImagePixelFormat}

GdipGetImageThumbnail: function(image: GPIMAGE; thumbWidth: UINT;  
    thumbHeight: UINT; out thumbImage: GPIMAGE;
    callback: GETTHUMBNAILIMAGEABORT; callbackData: Pointer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetImageThumbnail}

GdipGetEncoderParameterListSize: function(image: GPIMAGE;  
    clsidEncoder: PGUID; out size: UINT): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetEncoderParameterListSize}

GdipGetEncoderParameterList: function(image: GPIMAGE; clsidEncoder: PGUID;
    size: UINT; buffer: PENCODERPARAMETERS): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetEncoderParameterList}

GdipImageGetFrameDimensionsCount: function(image: GPIMAGE;  
    var count: UINT): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipImageGetFrameDimensionsCount}

GdipImageGetFrameDimensionsList: function(image: GPIMAGE; dimensionIDs: PGUID;  
    count: UINT): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipImageGetFrameDimensionsList}

GdipImageGetFrameCount: function(image: GPIMAGE; dimensionID: PGUID;  
    var count: UINT): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipImageGetFrameCount}

GdipImageSelectActiveFrame: function(image: GPIMAGE; dimensionID: PGUID;  
    frameIndex: UINT): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipImageSelectActiveFrame}

GdipImageRotateFlip: function(image: GPIMAGE;  
    rfType: ROTATEFLIPTYPE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipImageRotateFlip}

GdipGetImagePalette: function(image: GPIMAGE; palette: PCOLORPALETTE;  
    size: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetImagePalette}

GdipSetImagePalette: function(image: GPIMAGE;  
    palette: PCOLORPALETTE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetImagePalette}

GdipGetImagePaletteSize: function(image: GPIMAGE;  
    var size: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetImagePaletteSize}

GdipGetPropertyCount: function(image: GPIMAGE;  
    var numOfProperty: UINT): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetPropertyCount}

GdipGetPropertyIdList: function(image: GPIMAGE; numOfProperty: UINT;  
    list: PPROPID): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetPropertyIdList}

GdipGetPropertyItemSize: function(image: GPIMAGE; propId: PROPID;  
    var size: UINT): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetPropertyItemSize}

GdipGetPropertyItem: function(image: GPIMAGE; propId: PROPID; propSize: UINT;  
    buffer: PPROPERTYITEM): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetPropertyItem}

GdipGetPropertySize: function(image: GPIMAGE; var totalBufferSize: UINT;  
    var numProperties: UINT): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetPropertySize}

GdipGetAllPropertyItems: function(image: GPIMAGE; totalBufferSize: UINT;  
    numProperties: UINT; allItems: PPROPERTYITEM): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetAllPropertyItems}

GdipRemovePropertyItem: function(image: GPIMAGE;  
    propId: PROPID): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipRemovePropertyItem}

GdipSetPropertyItem: function(image: GPIMAGE;  
    item: PPROPERTYITEM): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetPropertyItem}

GdipImageForceValidation: function(image: GPIMAGE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipImageForceValidation}

//---------------------------------------------------------------------------- 
// Bitmap APIs
//----------------------------------------------------------------------------

GdipCreateBitmapFromStream: function(stream: ISTREAM;  
    out bitmap: GPBITMAP): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreateBitmapFromStream}

GdipCreateBitmapFromFile: function(filename: PWCHAR;  
    out bitmap: GPBITMAP): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreateBitmapFromFile}

GdipCreateBitmapFromStreamICM: function(stream: ISTREAM;  
    out bitmap: GPBITMAP): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreateBitmapFromStreamICM}

GdipCreateBitmapFromFileICM: function(filename: PWCHAR;  
    var bitmap: GPBITMAP): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreateBitmapFromFileICM}

GdipCreateBitmapFromScan0: function(width: Integer; height: Integer;  
    stride: Integer; format: PIXELFORMAT; scan0: PBYTE;
    out bitmap: GPBITMAP): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreateBitmapFromScan0}

GdipCreateBitmapFromGraphics: function(width: Integer; height: Integer;  
    target: GPGRAPHICS; out bitmap: GPBITMAP): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreateBitmapFromGraphics}

GdipCreateBitmapFromGdiDib: function(gdiBitmapInfo: PBitmapInfo;  
    gdiBitmapData: Pointer; out bitmap: GPBITMAP): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreateBitmapFromGdiDib}

GdipCreateBitmapFromHBITMAP: function(hbm: HBITMAP; hpal: HPALETTE;  
    out bitmap: GPBITMAP): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreateBitmapFromHBITMAP}

GdipCreateHBITMAPFromBitmap: function(bitmap: GPBITMAP; out hbmReturn: HBITMAP;
    background: ARGB): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreateHBITMAPFromBitmap}

GdipCreateBitmapFromHICON: function(hicon: HICON;  
    out bitmap: GPBITMAP): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreateBitmapFromHICON}

GdipCreateHICONFromBitmap: function(bitmap: GPBITMAP;  
    out hbmReturn: HICON): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreateHICONFromBitmap}

GdipCreateBitmapFromResource: function(hInstance: HMODULE;  
    lpBitmapName: PWCHAR; out bitmap: GPBITMAP): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreateBitmapFromResource}

GdipCloneBitmapArea: function(x: Single; y: Single; width: Single;  
    height: Single; format: PIXELFORMAT; srcBitmap: GPBITMAP;
    out dstBitmap: GPBITMAP): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCloneBitmapArea}

GdipCloneBitmapAreaI: function(x: Integer; y: Integer; width: Integer;  
    height: Integer; format: PIXELFORMAT; srcBitmap: GPBITMAP;
    out dstBitmap: GPBITMAP): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCloneBitmapAreaI}

GdipBitmapLockBits: function(bitmap: GPBITMAP; rect: GPRECT; flags: UINT;  
    format: PIXELFORMAT; lockedBitmapData: PBITMAPDATA): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipBitmapLockBits}

GdipBitmapUnlockBits: function(bitmap: GPBITMAP;  
    lockedBitmapData: PBITMAPDATA): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipBitmapUnlockBits}

GdipBitmapGetPixel: function(bitmap: GPBITMAP; x: Integer; y: Integer;  
    var color: ARGB): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipBitmapGetPixel}

GdipBitmapSetPixel: function(bitmap: GPBITMAP; x: Integer; y: Integer;  
    color: ARGB): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipBitmapSetPixel}

GdipBitmapSetResolution: function(bitmap: GPBITMAP; xdpi: Single;  
    ydpi: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipBitmapSetResolution}

//----------------------------------------------------------------------------
// ImageAttributes APIs
//----------------------------------------------------------------------------

GdipCreateImageAttributes: function(  
    out imageattr: GPIMAGEATTRIBUTES): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreateImageAttributes}

GdipCloneImageAttributes: function(imageattr: GPIMAGEATTRIBUTES;
    out cloneImageattr: GPIMAGEATTRIBUTES): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCloneImageAttributes}

GdipDisposeImageAttributes: function(  
    imageattr: GPIMAGEATTRIBUTES): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipDisposeImageAttributes}

GdipSetImageAttributesToIdentity: function(imageattr: GPIMAGEATTRIBUTES;  
    type_: COLORADJUSTTYPE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetImageAttributesToIdentity}

GdipResetImageAttributes: function(imageattr: GPIMAGEATTRIBUTES;  
    type_: COLORADJUSTTYPE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipResetImageAttributes}

GdipSetImageAttributesColorMatrix: function(imageattr: GPIMAGEATTRIBUTES;  
    type_: COLORADJUSTTYPE; enableFlag: Bool; colorMatrix: PCOLORMATRIX;
    grayMatrix: PCOLORMATRIX; flags: COLORMATRIXFLAGS): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetImageAttributesColorMatrix}

GdipSetImageAttributesThreshold: function(imageattr: GPIMAGEATTRIBUTES;  
    type_: COLORADJUSTTYPE; enableFlag: Bool;
    threshold: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetImageAttributesThreshold}

GdipSetImageAttributesGamma: function(imageattr: GPIMAGEATTRIBUTES;  
    type_: COLORADJUSTTYPE; enableFlag: Bool; gamma: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetImageAttributesGamma}

GdipSetImageAttributesNoOp: function(imageattr: GPIMAGEATTRIBUTES;  
  type_: COLORADJUSTTYPE; enableFlag: Bool): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetImageAttributesNoOp}

GdipSetImageAttributesColorKeys: function(imageattr: GPIMAGEATTRIBUTES;  
    type_: COLORADJUSTTYPE; enableFlag: Bool; colorLow: ARGB;
    colorHigh: ARGB): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetImageAttributesColorKeys}

GdipSetImageAttributesOutputChannel: function(imageattr: GPIMAGEATTRIBUTES;  
    type_: COLORADJUSTTYPE; enableFlag: Bool;
    channelFlags: COLORCHANNELFLAGS): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetImageAttributesOutputChannel}

GdipSetImageAttributesOutputChannelColorProfile: function(imageattr: GPIMAGEATTRIBUTES;  
    type_: COLORADJUSTTYPE; enableFlag: Bool;
    colorProfileFilename: PWCHAR): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetImageAttributesOutputChannelColorProfile}

GdipSetImageAttributesRemapTable: function(imageattr: GPIMAGEATTRIBUTES;  
    type_: COLORADJUSTTYPE; enableFlag: Bool; mapSize: UINT;
    map: PCOLORMAP): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetImageAttributesRemapTable}

GdipSetImageAttributesWrapMode: function(imageAttr: GPIMAGEATTRIBUTES;
    wrap: WRAPMODE; argb: ARGB; clamp: Bool): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetImageAttributesWrapMode}

GdipSetImageAttributesICMMode: function(imageAttr: GPIMAGEATTRIBUTES;  
    on_: Bool): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetImageAttributesICMMode}

GdipGetImageAttributesAdjustedPalette: function(imageAttr: GPIMAGEATTRIBUTES;  
    colorPalette: PCOLORPALETTE;
    colorAdjustType: COLORADJUSTTYPE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetImageAttributesAdjustedPalette}

//----------------------------------------------------------------------------
// Graphics APIs
//----------------------------------------------------------------------------

GdipFlush: function(graphics: GPGRAPHICS;  
    intention: GPFLUSHINTENTION): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipFlush}

GdipCreateFromHDC: function(hdc: HDC;  
    out graphics: GPGRAPHICS): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreateFromHDC}

GdipCreateFromHDC2: function(hdc: HDC; hDevice: THandle;  
    out graphics: GPGRAPHICS): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreateFromHDC2}

GdipCreateFromHWND: function(hwnd: HWND;  
    out graphics: GPGRAPHICS): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreateFromHWND}

GdipCreateFromHWNDICM: function(hwnd: HWND;  
    out graphics: GPGRAPHICS): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreateFromHWNDICM}

GdipDeleteGraphics: function(graphics: GPGRAPHICS): GPSTATUS; stdcall;  
  {.$EXTERNALSYM GdipDeleteGraphics}

GdipGetDC: function(graphics: GPGRAPHICS; var hdc: HDC): GPSTATUS; stdcall;  
  {.$EXTERNALSYM GdipGetDC}

GdipReleaseDC: function(graphics: GPGRAPHICS; hdc: HDC): GPSTATUS; stdcall;  
  {.$EXTERNALSYM GdipReleaseDC}

GdipSetCompositingMode: function(graphics: GPGRAPHICS;  
    compositingMode: COMPOSITINGMODE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetCompositingMode}

GdipGetCompositingMode: function(graphics: GPGRAPHICS;  
    var compositingMode: COMPOSITINGMODE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetCompositingMode}

GdipSetRenderingOrigin: function(graphics: GPGRAPHICS; x: Integer;
    y: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetRenderingOrigin}

GdipGetRenderingOrigin: function(graphics: GPGRAPHICS; var x: Integer;  
    var y: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetRenderingOrigin}

GdipSetCompositingQuality: function(graphics: GPGRAPHICS;
    compositingQuality: COMPOSITINGQUALITY): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetCompositingQuality}

GdipGetCompositingQuality: function(graphics: GPGRAPHICS;  
    var compositingQuality: COMPOSITINGQUALITY): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetCompositingQuality}

GdipSetSmoothingMode: function(graphics: GPGRAPHICS;  
    smoothingMode: SMOOTHINGMODE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetSmoothingMode}

GdipGetSmoothingMode: function(graphics: GPGRAPHICS;  
    var smoothingMode: SMOOTHINGMODE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetSmoothingMode}

GdipSetPixelOffsetMode: function(graphics: GPGRAPHICS;  
    pixelOffsetMode: PIXELOFFSETMODE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetPixelOffsetMode}

GdipGetPixelOffsetMode: function(graphics: GPGRAPHICS;  
    var pixelOffsetMode: PIXELOFFSETMODE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetPixelOffsetMode}

GdipSetTextRenderingHint: function(graphics: GPGRAPHICS;  
    mode: TEXTRENDERINGHINT): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetTextRenderingHint}

GdipGetTextRenderingHint: function(graphics: GPGRAPHICS;  
    var mode: TEXTRENDERINGHINT): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetTextRenderingHint}

GdipSetTextContrast: function(graphics: GPGRAPHICS;  
    contrast: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetTextContrast}

GdipGetTextContrast: function(graphics: GPGRAPHICS;  
    var contrast: UINT): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetTextContrast}

GdipSetInterpolationMode: function(graphics: GPGRAPHICS;  
    interpolationMode: INTERPOLATIONMODE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetInterpolationMode}

GdipGetInterpolationMode: function(graphics: GPGRAPHICS;  
    var interpolationMode: INTERPOLATIONMODE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetInterpolationMode}

GdipSetWorldTransform: function(graphics: GPGRAPHICS;  
    matrix: GPMATRIX): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetWorldTransform}

GdipResetWorldTransform: function(graphics: GPGRAPHICS): GPSTATUS; stdcall;  
  {.$EXTERNALSYM GdipResetWorldTransform}

GdipMultiplyWorldTransform: function(graphics: GPGRAPHICS; matrix: GPMATRIX;  
    order: GPMATRIXORDER): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipMultiplyWorldTransform}

GdipTranslateWorldTransform: function(graphics: GPGRAPHICS; dx: Single;  
    dy: Single; order: GPMATRIXORDER): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipTranslateWorldTransform}

GdipScaleWorldTransform: function(graphics: GPGRAPHICS; sx: Single; sy: Single;  
    order: GPMATRIXORDER): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipScaleWorldTransform}

GdipRotateWorldTransform: function(graphics: GPGRAPHICS; angle: Single;  
    order: GPMATRIXORDER): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipRotateWorldTransform}

GdipGetWorldTransform: function(graphics: GPGRAPHICS;  
    matrix: GPMATRIX): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetWorldTransform}

GdipResetPageTransform: function(graphics: GPGRAPHICS): GPSTATUS; stdcall;  
  {.$EXTERNALSYM GdipResetPageTransform}

GdipGetPageUnit: function(graphics: GPGRAPHICS;  
    var unit_: GPUNIT): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetPageUnit}

GdipGetPageScale: function(graphics: GPGRAPHICS;  
    var scale: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetPageScale}

GdipSetPageUnit: function(graphics: GPGRAPHICS;  
    unit_: GPUNIT): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetPageUnit}

GdipSetPageScale: function(graphics: GPGRAPHICS;  
    scale: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetPageScale}

GdipGetDpiX: function(graphics: GPGRAPHICS;  
    var dpi: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetDpiX}

GdipGetDpiY: function(graphics: GPGRAPHICS;  
    var dpi: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetDpiY}

GdipTransformPoints: function(graphics: GPGRAPHICS;  
    destSpace: GPCOORDINATESPACE; srcSpace: GPCOORDINATESPACE;
    points: GPPOINTF; count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipTransformPoints}

GdipTransformPointsI: function(graphics: GPGRAPHICS;  
    destSpace: GPCOORDINATESPACE; srcSpace: GPCOORDINATESPACE;
    points: GPPOINT; count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipTransformPointsI}

GdipGetNearestColor: function(graphics: GPGRAPHICS;  
    argb: PARGB): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetNearestColor}

// Creates the Win9x Halftone Palette (even on NT) with correct Desktop colors

GdipCreateHalftonePalette: function: HPALETTE; stdcall;
  {.$EXTERNALSYM GdipCreateHalftonePalette}

GdipDrawLine: function(graphics: GPGRAPHICS; pen: GPPEN; x1: Single;  
    y1: Single; x2: Single; y2: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipDrawLine}

GdipDrawLineI: function(graphics: GPGRAPHICS; pen: GPPEN; x1: Integer;  
    y1: Integer; x2: Integer; y2: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipDrawLineI}

GdipDrawLines: function(graphics: GPGRAPHICS; pen: GPPEN; points: GPPOINTF;  
    count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipDrawLines}

GdipDrawLinesI: function(graphics: GPGRAPHICS; pen: GPPEN; points: GPPOINT;  
    count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipDrawLinesI}

GdipDrawArc: function(graphics: GPGRAPHICS; pen: GPPEN; x: Single; y: Single;  
    width: Single; height: Single; startAngle: Single;
    sweepAngle: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipDrawArc}

GdipDrawArcI: function(graphics: GPGRAPHICS; pen: GPPEN; x: Integer;  
    y: Integer; width: Integer; height: Integer; startAngle: Single;
    sweepAngle: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipDrawArcI}

GdipDrawBezier: function(graphics: GPGRAPHICS; pen: GPPEN; x1: Single;  
    y1: Single; x2: Single; y2: Single; x3: Single; y3: Single; x4: Single;
    y4: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipDrawBezier}

GdipDrawBezierI: function(graphics: GPGRAPHICS; pen: GPPEN; x1: Integer;  
    y1: Integer; x2: Integer; y2: Integer; x3: Integer; y3: Integer;
    x4: Integer; y4: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipDrawBezierI}

GdipDrawBeziers: function(graphics: GPGRAPHICS; pen: GPPEN; points: GPPOINTF;  
    count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipDrawBeziers}

GdipDrawBeziersI: function(graphics: GPGRAPHICS; pen: GPPEN; points: GPPOINT;  
    count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipDrawBeziersI}

GdipDrawRectangle: function(graphics: GPGRAPHICS; pen: GPPEN; x: Single;  
    y: Single; width: Single; height: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipDrawRectangle}

GdipDrawRectangleI: function(graphics: GPGRAPHICS; pen: GPPEN; x: Integer;  
    y: Integer; width: Integer; height: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipDrawRectangleI}

GdipDrawRectangles: function(graphics: GPGRAPHICS; pen: GPPEN; rects: GPRECTF;  
    count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipDrawRectangles}

GdipDrawRectanglesI: function(graphics: GPGRAPHICS; pen: GPPEN; rects: GPRECT;  
    count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipDrawRectanglesI}

GdipDrawEllipse: function(graphics: GPGRAPHICS; pen: GPPEN; x: Single;
    y: Single; width: Single; height: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipDrawEllipse}

GdipDrawEllipseI: function(graphics: GPGRAPHICS; pen: GPPEN; x: Integer;  
    y: Integer; width: Integer; height: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipDrawEllipseI}

GdipDrawPie: function(graphics: GPGRAPHICS; pen: GPPEN; x: Single; y: Single;  
    width: Single;  height: Single; startAngle: Single;
    sweepAngle: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipDrawPie}

GdipDrawPieI: function(graphics: GPGRAPHICS; pen: GPPEN; x: Integer;  
    y: Integer; width: Integer; height: Integer; startAngle: Single;
    sweepAngle: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipDrawPieI}

GdipDrawPolygon: function(graphics: GPGRAPHICS; pen: GPPEN; points: GPPOINTF;  
    count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipDrawPolygon}

GdipDrawPolygonI: function(graphics: GPGRAPHICS; pen: GPPEN; points: GPPOINT;  
    count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipDrawPolygonI}

GdipDrawPath: function(graphics: GPGRAPHICS; pen: GPPEN;  
    path: GPPATH): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipDrawPath}

GdipDrawCurve: function(graphics: GPGRAPHICS; pen: GPPEN; points: GPPOINTF;  
    count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipDrawCurve}

GdipDrawCurveI: function(graphics: GPGRAPHICS; pen: GPPEN; points: GPPOINT;  
    count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipDrawCurveI}

GdipDrawCurve2: function(graphics: GPGRAPHICS; pen: GPPEN; points: GPPOINTF;  
    count: Integer; tension: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipDrawCurve2}

GdipDrawCurve2I: function(graphics: GPGRAPHICS; pen: GPPEN; points: GPPOINT;  
    count: Integer; tension: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipDrawCurve2I}

GdipDrawCurve3: function(graphics: GPGRAPHICS; pen: GPPEN; points: GPPOINTF;  
    count: Integer; offset: Integer; numberOfSegments: Integer;
    tension: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipDrawCurve3}

GdipDrawCurve3I: function(graphics: GPGRAPHICS; pen: GPPEN; points: GPPOINT;  
    count: Integer; offset: Integer; numberOfSegments: Integer;
    tension: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipDrawCurve3I}

GdipDrawClosedCurve: function(graphics: GPGRAPHICS; pen: GPPEN;  
    points: GPPOINTF; count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipDrawClosedCurve}

GdipDrawClosedCurveI: function(graphics: GPGRAPHICS; pen: GPPEN;  
    points: GPPOINT; count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipDrawClosedCurveI}

GdipDrawClosedCurve2: function(graphics: GPGRAPHICS; pen: GPPEN;  
    points: GPPOINTF; count: Integer; tension: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipDrawClosedCurve2}

GdipDrawClosedCurve2I: function(graphics: GPGRAPHICS; pen: GPPEN;  
    points: GPPOINT; count: Integer; tension: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipDrawClosedCurve2I}

GdipGraphicsClear: function(graphics: GPGRAPHICS;  
    color: ARGB): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGraphicsClear}

GdipFillRectangle: function(graphics: GPGRAPHICS; brush: GPBRUSH; x: Single;  
    y: Single; width: Single; height: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipFillRectangle}

GdipFillRectangleI: function(graphics: GPGRAPHICS; brush: GPBRUSH; x: Integer;  
    y: Integer; width: Integer; height: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipFillRectangleI}

GdipFillRectangles: function(graphics: GPGRAPHICS; brush: GPBRUSH;  
    rects: GPRECTF; count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipFillRectangles}

GdipFillRectanglesI: function(graphics: GPGRAPHICS; brush: GPBRUSH;  
    rects: GPRECT; count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipFillRectanglesI}

GdipFillPolygon: function(graphics: GPGRAPHICS; brush: GPBRUSH;  
    points: GPPOINTF; count: Integer; fillMode: GPFILLMODE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipFillPolygon}

GdipFillPolygonI: function(graphics: GPGRAPHICS; brush: GPBRUSH;  
    points: GPPOINT; count: Integer; fillMode: GPFILLMODE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipFillPolygonI}

GdipFillPolygon2: function(graphics: GPGRAPHICS; brush: GPBRUSH;  
    points: GPPOINTF; count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipFillPolygon2}

GdipFillPolygon2I: function(graphics: GPGRAPHICS; brush: GPBRUSH;  
    points: GPPOINT; count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipFillPolygon2I}

GdipFillEllipse: function(graphics: GPGRAPHICS; brush: GPBRUSH; x: Single;
    y: Single; width: Single; height: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipFillEllipse}

GdipFillEllipseI: function(graphics: GPGRAPHICS; brush: GPBRUSH; x: Integer;  
    y: Integer; width: Integer; height: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipFillEllipseI}

GdipFillPie: function(graphics: GPGRAPHICS; brush: GPBRUSH; x: Single;  
    y: Single; width: Single; height: Single; startAngle: Single;
    sweepAngle: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipFillPie}

GdipFillPieI: function(graphics: GPGRAPHICS; brush: GPBRUSH; x: Integer;  
    y: Integer; width: Integer; height: Integer; startAngle: Single;
    sweepAngle: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipFillPieI}

GdipFillPath: function(graphics: GPGRAPHICS; brush: GPBRUSH;  
    path: GPPATH): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipFillPath}

GdipFillClosedCurve: function(graphics: GPGRAPHICS; brush: GPBRUSH;  
    points: GPPOINTF; count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipFillClosedCurve}

GdipFillClosedCurveI: function(graphics: GPGRAPHICS; brush: GPBRUSH;  
    points: GPPOINT; count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipFillClosedCurveI}

GdipFillClosedCurve2: function(graphics: GPGRAPHICS; brush: GPBRUSH;  
    points: GPPOINTF; count: Integer; tension: Single;
    fillMode: GPFILLMODE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipFillClosedCurve2}

GdipFillClosedCurve2I: function(graphics: GPGRAPHICS; brush: GPBRUSH;  
    points: GPPOINT; count: Integer; tension: Single;
    fillMode: GPFILLMODE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipFillClosedCurve2I}

GdipFillRegion: function(graphics: GPGRAPHICS; brush: GPBRUSH;  
    region: GPREGION): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipFillRegion}

GdipDrawImage: function(graphics: GPGRAPHICS; image: GPIMAGE; x: Single;  
    y: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipDrawImage}

GdipDrawImageI: function(graphics: GPGRAPHICS; image: GPIMAGE; x: Integer;  
    y: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipDrawImageI}

GdipDrawImageRect: function(graphics: GPGRAPHICS; image: GPIMAGE; x: Single;  
    y: Single; width: Single; height: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipDrawImageRect}

GdipDrawImageRectI: function(graphics: GPGRAPHICS; image: GPIMAGE; x: Integer;  
    y: Integer; width: Integer; height: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipDrawImageRectI}

GdipDrawImagePoints: function(graphics: GPGRAPHICS; image: GPIMAGE;  
    dstpoints: GPPOINTF; count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipDrawImagePoints}

GdipDrawImagePointsI: function(graphics: GPGRAPHICS; image: GPIMAGE;  
    dstpoints: GPPOINT; count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipDrawImagePointsI}

GdipDrawImagePointRect: function(graphics: GPGRAPHICS; image: GPIMAGE;  
    x: Single; y: Single; srcx: Single; srcy: Single; srcwidth: Single;
    srcheight: Single; srcUnit: GPUNIT): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipDrawImagePointRect}

GdipDrawImagePointRectI: function(graphics: GPGRAPHICS; image: GPIMAGE;  
    x: Integer; y: Integer; srcx: Integer; srcy: Integer; srcwidth: Integer;
    srcheight: Integer; srcUnit: GPUNIT): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipDrawImagePointRectI}

GdipDrawImageRectRect: function(graphics: GPGRAPHICS; image: GPIMAGE;  
    dstx: Single; dsty: Single; dstwidth: Single; dstheight: Single;
    srcx: Single; srcy: Single; srcwidth: Single; srcheight: Single;
    srcUnit: GPUNIT; imageAttributes: GPIMAGEATTRIBUTES;
    callback: DRAWIMAGEABORT; callbackData: Pointer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipDrawImageRectRect}

GdipDrawImageRectRectI: function(graphics: GPGRAPHICS; image: GPIMAGE;  
    dstx: Integer; dsty: Integer; dstwidth: Integer; dstheight: Integer;
    srcx: Integer; srcy: Integer; srcwidth: Integer; srcheight: Integer;
    srcUnit: GPUNIT; imageAttributes: GPIMAGEATTRIBUTES;
    callback: DRAWIMAGEABORT; callbackData: Pointer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipDrawImageRectRectI}

GdipDrawImagePointsRect: function(graphics: GPGRAPHICS; image: GPIMAGE;  
    points: GPPOINTF; count: Integer; srcx: Single; srcy: Single;
    srcwidth: Single; srcheight: Single; srcUnit: GPUNIT;
    imageAttributes: GPIMAGEATTRIBUTES; callback: DRAWIMAGEABORT;
    callbackData: Pointer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipDrawImagePointsRect}

GdipDrawImagePointsRectI: function(graphics: GPGRAPHICS; image: GPIMAGE;  
    points: GPPOINT; count: Integer; srcx: Integer; srcy: Integer;
    srcwidth: Integer; srcheight: Integer; srcUnit: GPUNIT;
    imageAttributes: GPIMAGEATTRIBUTES; callback: DRAWIMAGEABORT;
    callbackData: Pointer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipDrawImagePointsRectI}

GdipEnumerateMetafileDestPoint: function(graphics: GPGRAPHICS;  
    metafile: GPMETAFILE; destPoint: PGPPointF; callback: ENUMERATEMETAFILEPROC;
    callbackData: Pointer;
    imageAttributes: GPIMAGEATTRIBUTES): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipEnumerateMetafileDestPoint}

GdipEnumerateMetafileDestPointI: function(graphics: GPGRAPHICS;  
    metafile: GPMETAFILE; destPoint: PGPPoint; callback: ENUMERATEMETAFILEPROC;
    callbackData: Pointer;
    imageAttributes: GPIMAGEATTRIBUTES): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipEnumerateMetafileDestPointI}

GdipEnumerateMetafileDestRect: function(graphics: GPGRAPHICS;  
    metafile: GPMETAFILE; destRect: PGPRectF; callback: ENUMERATEMETAFILEPROC;
    callbackData: Pointer;
    imageAttributes: GPIMAGEATTRIBUTES): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipEnumerateMetafileDestRect}

GdipEnumerateMetafileDestRectI: function(graphics: GPGRAPHICS;  
    metafile: GPMETAFILE; destRect: PGPRect; callback: ENUMERATEMETAFILEPROC;
    callbackData: Pointer;
    imageAttributes: GPIMAGEATTRIBUTES): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipEnumerateMetafileDestRectI}

GdipEnumerateMetafileDestPoints: function(graphics: GPGRAPHICS;  
    metafile: GPMETAFILE; destPoints: PGPPointF; count: Integer;
    callback: ENUMERATEMETAFILEPROC; callbackData: Pointer;
    imageAttributes: GPIMAGEATTRIBUTES): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipEnumerateMetafileDestPoints}

GdipEnumerateMetafileDestPointsI: function(graphics: GPGRAPHICS;  
    metafile: GPMETAFILE; destPoints: PGPPoint; count: Integer;
    callback: ENUMERATEMETAFILEPROC; callbackData: Pointer;
    imageAttributes: GPIMAGEATTRIBUTES): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipEnumerateMetafileDestPointsI}

GdipEnumerateMetafileSrcRectDestPoint: function(graphics: GPGRAPHICS;  
    metafile: GPMETAFILE; destPoint: PGPPointF; srcRect: PGPRectF; srcUnit: TUNIT;
    callback: ENUMERATEMETAFILEPROC; callbackData: Pointer;
    imageAttributes: GPIMAGEATTRIBUTES): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipEnumerateMetafileSrcRectDestPoint}

GdipEnumerateMetafileSrcRectDestPointI: function(graphics: GPGRAPHICS;  
    metafile: GPMETAFILE; destPoint: PGPPoint; srcRect: PGPRect; srcUnit: TUNIT;
    callback: ENUMERATEMETAFILEPROC; callbackData: Pointer;
    imageAttributes: GPIMAGEATTRIBUTES): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipEnumerateMetafileSrcRectDestPointI}

GdipEnumerateMetafileSrcRectDestRect: function(graphics: GPGRAPHICS;  
    metafile: GPMETAFILE; destRect: PGPRectF; srcRect: PGPRectF; srcUnit: TUNIT;
    callback: ENUMERATEMETAFILEPROC; callbackData: Pointer;
    imageAttributes: GPIMAGEATTRIBUTES): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipEnumerateMetafileSrcRectDestRect}

GdipEnumerateMetafileSrcRectDestRectI: function(graphics: GPGRAPHICS;  
    metafile: GPMETAFILE; destRect: PGPRect; srcRect: PGPRect; srcUnit: TUNIT;
    callback: ENUMERATEMETAFILEPROC; callbackData: Pointer;
    imageAttributes: GPIMAGEATTRIBUTES): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipEnumerateMetafileSrcRectDestRectI}

GdipEnumerateMetafileSrcRectDestPoints: function(graphics: GPGRAPHICS;  
    metafile: GPMETAFILE; destPoints: PGPPointF; count: Integer; srcRect: PGPRectF;
    srcUnit: TUNIT; callback: ENUMERATEMETAFILEPROC; callbackData: Pointer;
    imageAttributes: GPIMAGEATTRIBUTES): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipEnumerateMetafileSrcRectDestPoints}

GdipEnumerateMetafileSrcRectDestPointsI: function(graphics: GPGRAPHICS;  
    metafile: GPMETAFILE; destPoints: PGPPoint; count: Integer; srcRect: PGPRect;
    srcUnit: TUNIT; callback: ENUMERATEMETAFILEPROC; callbackData: Pointer;
    imageAttributes: GPIMAGEATTRIBUTES): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipEnumerateMetafileSrcRectDestPointsI}

GdipPlayMetafileRecord: function(metafile: GPMETAFILE;  
    recordType: EMFPLUSRECORDTYPE; flags: UINT; dataSize: UINT;
    data: PBYTE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipPlayMetafileRecord}

GdipSetClipGraphics: function(graphics: GPGRAPHICS; srcgraphics: GPGRAPHICS;  
    combineMode: COMBINEMODE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetClipGraphics}

GdipSetClipRect: function(graphics: GPGRAPHICS; x: Single; y: Single;  
    width: Single; height: Single; combineMode: COMBINEMODE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetClipRect}

GdipSetClipRectI: function(graphics: GPGRAPHICS; x: Integer; y: Integer;  
    width: Integer; height: Integer;
    combineMode: COMBINEMODE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetClipRectI}

GdipSetClipPath: function(graphics: GPGRAPHICS; path: GPPATH;  
    combineMode: COMBINEMODE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetClipPath}

GdipSetClipRegion: function(graphics: GPGRAPHICS; region: GPREGION;  
    combineMode: COMBINEMODE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetClipRegion}

GdipSetClipHrgn: function(graphics: GPGRAPHICS; hRgn: HRGN;  
    combineMode: COMBINEMODE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetClipHrgn}

GdipResetClip: function(graphics: GPGRAPHICS): GPSTATUS; stdcall;  
  {.$EXTERNALSYM GdipResetClip}

GdipTranslateClip: function(graphics: GPGRAPHICS; dx: Single;  
    dy: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipTranslateClip}

GdipTranslateClipI: function(graphics: GPGRAPHICS; dx: Integer;
    dy: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipTranslateClipI}

GdipGetClip: function(graphics: GPGRAPHICS;  
    region: GPREGION): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetClip}

GdipGetClipBounds: function(graphics: GPGRAPHICS;  
    rect: GPRECTF): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetClipBounds}

GdipGetClipBoundsI: function(graphics: GPGRAPHICS;  
    rect: GPRECT): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetClipBoundsI}

GdipIsClipEmpty: function(graphics: GPGRAPHICS;  
    result: PBool): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipIsClipEmpty}

GdipGetVisibleClipBounds: function(graphics: GPGRAPHICS;  
    rect: GPRECTF): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetVisibleClipBounds}

GdipGetVisibleClipBoundsI: function(graphics: GPGRAPHICS;  
    rect: GPRECT): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetVisibleClipBoundsI}

GdipIsVisibleClipEmpty: function(graphics: GPGRAPHICS;
    var result: Bool): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipIsVisibleClipEmpty}

GdipIsVisiblePoint: function(graphics: GPGRAPHICS; x: Single; y: Single;  
    var result: Bool): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipIsVisiblePoint}

GdipIsVisiblePointI: function(graphics: GPGRAPHICS; x: Integer; y: Integer;  
    var result: Bool): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipIsVisiblePointI}

GdipIsVisibleRect: function(graphics: GPGRAPHICS; x: Single; y: Single;  
    width: Single; height: Single; var result: Bool): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipIsVisibleRect}

GdipIsVisibleRectI: function(graphics: GPGRAPHICS; x: Integer; y: Integer;  
    width: Integer; height: Integer; var result: Bool): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipIsVisibleRectI}

GdipSaveGraphics: function(graphics: GPGRAPHICS;  
    var state: GRAPHICSSTATE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSaveGraphics}

GdipRestoreGraphics: function(graphics: GPGRAPHICS;  
    state: GRAPHICSSTATE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipRestoreGraphics}

GdipBeginContainer: function(graphics: GPGRAPHICS; dstrect: GPRECTF;  
    srcrect: GPRECTF; unit_: GPUNIT;
    var state: GRAPHICSCONTAINER): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipBeginContainer}

GdipBeginContainerI: function(graphics: GPGRAPHICS; dstrect: GPRECT;  
    srcrect: GPRECT; unit_: GPUNIT;
    var state: GRAPHICSCONTAINER): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipBeginContainerI}

GdipBeginContainer2: function(graphics: GPGRAPHICS;  
    var state: GRAPHICSCONTAINER): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipBeginContainer2}

GdipEndContainer: function(graphics: GPGRAPHICS;  
    state: GRAPHICSCONTAINER): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipEndContainer}

GdipGetMetafileHeaderFromWmf: function(hWmf: HMETAFILE;  
    wmfPlaceableFileHeader: PWMFPLACEABLEFILEHEADER;
    header: Pointer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetMetafileHeaderFromWmf}

GdipGetMetafileHeaderFromEmf: function(hEmf: HENHMETAFILE;  
    header: Pointer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetMetafileHeaderFromEmf}

GdipGetMetafileHeaderFromFile: function(filename: PWCHAR;  
    header: Pointer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetMetafileHeaderFromFile}

GdipGetMetafileHeaderFromStream: function(stream: ISTREAM;  
    header: Pointer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetMetafileHeaderFromStream}

GdipGetMetafileHeaderFromMetafile: function(metafile: GPMETAFILE;  
    header: Pointer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetMetafileHeaderFromMetafile}

GdipGetHemfFromMetafile: function(metafile: GPMETAFILE;  
    var hEmf: HENHMETAFILE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetHemfFromMetafile}

GdipCreateStreamOnFile: function(filename: PWCHAR; access: UINT;  
    out stream: ISTREAM): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreateStreamOnFile}

GdipCreateMetafileFromWmf: function(hWmf: HMETAFILE; deleteWmf: Bool;  
    wmfPlaceableFileHeader: PWMFPLACEABLEFILEHEADER;
    out metafile: GPMETAFILE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreateMetafileFromWmf}

GdipCreateMetafileFromEmf: function(hEmf: HENHMETAFILE; deleteEmf: Bool;  
    out metafile: GPMETAFILE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreateMetafileFromEmf}

GdipCreateMetafileFromFile: function(file_: PWCHAR;  
    out metafile: GPMETAFILE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreateMetafileFromFile}

GdipCreateMetafileFromWmfFile: function(file_: PWCHAR;  
    wmfPlaceableFileHeader: PWMFPLACEABLEFILEHEADER;
    out metafile: GPMETAFILE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreateMetafileFromWmfFile}

GdipCreateMetafileFromStream: function(stream: ISTREAM;  
    out metafile: GPMETAFILE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreateMetafileFromStream}

GdipRecordMetafile: function(referenceHdc: HDC; type_: EMFTYPE;  
    frameRect: GPRECTF; frameUnit: METAFILEFRAMEUNIT;
    description: PWCHAR; out metafile: GPMETAFILE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipRecordMetafile}

GdipRecordMetafileI: function(referenceHdc: HDC; type_: EMFTYPE;  
    frameRect: GPRECT; frameUnit: METAFILEFRAMEUNIT; description: PWCHAR;
    out metafile: GPMETAFILE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipRecordMetafileI}

GdipRecordMetafileFileName: function(fileName: PWCHAR; referenceHdc: HDC;  
    type_: EMFTYPE; frameRect: GPRECTF; frameUnit: METAFILEFRAMEUNIT;
    description: PWCHAR; out metafile: GPMETAFILE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipRecordMetafileFileName}

GdipRecordMetafileFileNameI: function(fileName: PWCHAR; referenceHdc: HDC;  
    type_: EMFTYPE; frameRect: GPRECT; frameUnit: METAFILEFRAMEUNIT;
    description: PWCHAR; out metafile: GPMETAFILE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipRecordMetafileFileNameI}

GdipRecordMetafileStream: function(stream: ISTREAM; referenceHdc: HDC;  
    type_: EMFTYPE; frameRect: GPRECTF; frameUnit: METAFILEFRAMEUNIT;
    description: PWCHAR; out metafile: GPMETAFILE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipRecordMetafileStream}

GdipRecordMetafileStreamI: function(stream: ISTREAM; referenceHdc: HDC;  
    type_: EMFTYPE; frameRect: GPRECT; frameUnit: METAFILEFRAMEUNIT;
    description: PWCHAR; out metafile: GPMETAFILE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipRecordMetafileStreamI}

GdipSetMetafileDownLevelRasterizationLimit: function(metafile: GPMETAFILE;  
    metafileRasterizationLimitDpi: UINT): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetMetafileDownLevelRasterizationLimit}

GdipGetMetafileDownLevelRasterizationLimit: function(metafile: GPMETAFILE;  
    var metafileRasterizationLimitDpi: UINT): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetMetafileDownLevelRasterizationLimit}

GdipGetImageDecodersSize: function(out numDecoders: UINT;
    out size: UINT): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetImageDecodersSize}

GdipGetImageDecoders: function(numDecoders: UINT; size: UINT;  
    decoders: PIMAGECODECINFO): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetImageDecoders}

GdipGetImageEncodersSize: function(out numEncoders: UINT;  
    out size: UINT): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetImageEncodersSize}

GdipGetImageEncoders: function(numEncoders: UINT; size: UINT;  
    encoders: PIMAGECODECINFO): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetImageEncoders}

GdipComment: function(graphics: GPGRAPHICS; sizeData: UINT;  
    data: PBYTE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipComment}

//----------------------------------------------------------------------------
// FontFamily APIs
//----------------------------------------------------------------------------

GdipCreateFontFamilyFromName: function(name: PWCHAR;  
    fontCollection: GPFONTCOLLECTION;
    out FontFamily: GPFONTFAMILY): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreateFontFamilyFromName}

GdipDeleteFontFamily: function(FontFamily: GPFONTFAMILY): GPSTATUS; stdcall;  
  {.$EXTERNALSYM GdipDeleteFontFamily}

GdipCloneFontFamily: function(FontFamily: GPFONTFAMILY;  
    out clonedFontFamily: GPFONTFAMILY): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCloneFontFamily}

GdipGetGenericFontFamilySansSerif: function(  
    out nativeFamily: GPFONTFAMILY): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetGenericFontFamilySansSerif}

GdipGetGenericFontFamilySerif: function(  
    out nativeFamily: GPFONTFAMILY): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetGenericFontFamilySerif}

GdipGetGenericFontFamilyMonospace: function(  
    out nativeFamily: GPFONTFAMILY): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetGenericFontFamilyMonospace}

GdipGetFamilyName: function(family: GPFONTFAMILY; name: PWideChar;  
    language: LANGID): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetFamilyName}

GdipIsStyleAvailable: function(family: GPFONTFAMILY; style: Integer;  
    var IsStyleAvailable: Bool): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipIsStyleAvailable}

GdipFontCollectionEnumerable: function(fontCollection: GPFONTCOLLECTION;  
    graphics: GPGRAPHICS; var numFound: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipFontCollectionEnumerable}

GdipFontCollectionEnumerate: function(fontCollection: GPFONTCOLLECTION;  
    numSought: Integer; gpfamilies: array of GPFONTFAMILY;
    var numFound: Integer; graphics: GPGRAPHICS): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipFontCollectionEnumerate}

GdipGetEmHeight: function(family: GPFONTFAMILY; style: Integer;  
    out EmHeight: UINT16): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetEmHeight}

GdipGetCellAscent: function(family: GPFONTFAMILY; style: Integer;  
    var CellAscent: UINT16): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetCellAscent}

GdipGetCellDescent: function(family: GPFONTFAMILY; style: Integer;  
    var CellDescent: UINT16): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetCellDescent}

GdipGetLineSpacing: function(family: GPFONTFAMILY; style: Integer;  
    var LineSpacing: UINT16): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetLineSpacing}

//----------------------------------------------------------------------------
// Font APIs
//----------------------------------------------------------------------------

GdipCreateFontFromDC: function(hdc: HDC; out font: GPFONT): GPSTATUS; stdcall;  
  {.$EXTERNALSYM GdipCreateFontFromDC}

GdipCreateFontFromLogfontA: function(hdc: HDC; logfont: PLOGFONTA;  
    out font: GPFONT): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreateFontFromLogfontA}

GdipCreateFontFromLogfontW: function(hdc: HDC; logfont: PLOGFONTW;  
    out font: GPFONT): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreateFontFromLogfontW}

GdipCreateFont: function(fontFamily: GPFONTFAMILY; emSize: Single;  
    style: Integer; unit_: Integer; out font: GPFONT): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreateFont}

GdipCloneFont: function(font: GPFONT;  
    out cloneFont: GPFONT): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCloneFont}

GdipDeleteFont: function(font: GPFONT): GPSTATUS; stdcall;  
  {.$EXTERNALSYM GdipDeleteFont}

GdipGetFamily: function(font: GPFONT;  
    out family: GPFONTFAMILY): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetFamily}

GdipGetFontStyle: function(font: GPFONT;  
    var style: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetFontStyle}

GdipGetFontSize: function(font: GPFONT; var size: Single): GPSTATUS; stdcall;  
  {.$EXTERNALSYM GdipGetFontSize}

GdipGetFontUnit: function(font: GPFONT; var unit_: TUNIT): GPSTATUS; stdcall;  
  {.$EXTERNALSYM GdipGetFontUnit}

GdipGetFontHeight: function(font: GPFONT; graphics: GPGRAPHICS;  
    var height: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetFontHeight}

GdipGetFontHeightGivenDPI: function(font: GPFONT; dpi: Single;  
    var height: Single): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetFontHeightGivenDPI}

{$IFNDEF FPC}
GdipGetLogFontA: function(font: GPFONT; graphics: GPGRAPHICS;  
    var logfontA: LOGFONTA): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetLogFontA}
{$ENDIF}

GdipGetLogFontW: function(font: GPFONT; graphics: GPGRAPHICS;  
    var logfontW: LOGFONTW): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetLogFontW}

GdipNewInstalledFontCollection: function(  
    out fontCollection: GPFONTCOLLECTION): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipNewInstalledFontCollection}

GdipNewPrivateFontCollection: function(  
    out fontCollection: GPFONTCOLLECTION): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipNewPrivateFontCollection}

GdipDeletePrivateFontCollection: function(  
    out fontCollection: GPFONTCOLLECTION): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipDeletePrivateFontCollection}

GdipGetFontCollectionFamilyCount: function(fontCollection: GPFONTCOLLECTION;  
    var numFound: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetFontCollectionFamilyCount}

GdipGetFontCollectionFamilyList: function(fontCollection: GPFONTCOLLECTION;  
    numSought: Integer; gpfamilies: GPFONTFAMILY;
    var numFound: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetFontCollectionFamilyList}

GdipPrivateAddFontFile: function(fontCollection: GPFONTCOLLECTION;  
    filename: PWCHAR): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipPrivateAddFontFile}

GdipPrivateAddMemoryFont: function(fontCollection: GPFONTCOLLECTION;
    memory: Pointer; length: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipPrivateAddMemoryFont}

//----------------------------------------------------------------------------
// Text APIs
//----------------------------------------------------------------------------

GdipDrawString: function(graphics: GPGRAPHICS; string_: PWCHAR;  
    length: Integer; font: GPFONT; layoutRect: PGPRectF;
    stringFormat: GPSTRINGFORMAT; brush: GPBRUSH): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipDrawString}

GdipMeasureString: function(graphics: GPGRAPHICS; string_: PWCHAR;  
    length: Integer; font: GPFONT; layoutRect: PGPRectF;
    stringFormat: GPSTRINGFORMAT; boundingBox: PGPRectF;
    codepointsFitted: PInteger; linesFilled: PInteger): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipMeasureString}

GdipMeasureCharacterRanges: function(graphics: GPGRAPHICS; string_: PWCHAR;  
    length: Integer; font: GPFONT; layoutRect: PGPRectF;
    stringFormat: GPSTRINGFORMAT; regionCount: Integer;
    const regions: GPREGION): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipMeasureCharacterRanges}

GdipDrawDriverString: function(graphics: GPGRAPHICS; const text: PUINT16;  
    length: Integer; const font: GPFONT; const brush: GPBRUSH;
    const positions: PGPPointF; flags: Integer;
    const matrix: GPMATRIX): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipDrawDriverString}

GdipMeasureDriverString: function(graphics: GPGRAPHICS; text: PUINT16;  
    length: Integer; font: GPFONT; positions: PGPPointF; flags: Integer;
    matrix: GPMATRIX; boundingBox: PGPRectF): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipMeasureDriverString}

//----------------------------------------------------------------------------
// String format APIs
//----------------------------------------------------------------------------

GdipCreateStringFormat: function(formatAttributes: Integer; language: LANGID;  
    out format: GPSTRINGFORMAT): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreateStringFormat}

GdipStringFormatGetGenericDefault: function(  
    out format: GPSTRINGFORMAT): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipStringFormatGetGenericDefault}

GdipStringFormatGetGenericTypographic: function(  
    out format: GPSTRINGFORMAT): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipStringFormatGetGenericTypographic}

GdipDeleteStringFormat: function(format: GPSTRINGFORMAT): GPSTATUS; stdcall;  
  {.$EXTERNALSYM GdipDeleteStringFormat}

GdipCloneStringFormat: function(format: GPSTRINGFORMAT;  
    out newFormat: GPSTRINGFORMAT): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCloneStringFormat}

GdipSetStringFormatFlags: function(format: GPSTRINGFORMAT;  
    flags: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetStringFormatFlags}

GdipGetStringFormatFlags: function(format: GPSTRINGFORMAT;  
    out flags: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetStringFormatFlags}

GdipSetStringFormatAlign: function(format: GPSTRINGFORMAT;  
    align: STRINGALIGNMENT): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetStringFormatAlign}

GdipGetStringFormatAlign: function(format: GPSTRINGFORMAT;  
    out align: STRINGALIGNMENT): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetStringFormatAlign}

GdipSetStringFormatLineAlign: function(format: GPSTRINGFORMAT;  
    align: STRINGALIGNMENT): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetStringFormatLineAlign}

GdipGetStringFormatLineAlign: function(format: GPSTRINGFORMAT;  
    out align: STRINGALIGNMENT): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetStringFormatLineAlign}

GdipSetStringFormatTrimming: function(format: GPSTRINGFORMAT;  
    trimming: STRINGTRIMMING): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetStringFormatTrimming}

GdipGetStringFormatTrimming: function(format: GPSTRINGFORMAT;  
    out trimming: STRINGTRIMMING): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetStringFormatTrimming}

GdipSetStringFormatHotkeyPrefix: function(format: GPSTRINGFORMAT;  
    hotkeyPrefix: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetStringFormatHotkeyPrefix}

GdipGetStringFormatHotkeyPrefix: function(format: GPSTRINGFORMAT;  
    out hotkeyPrefix: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetStringFormatHotkeyPrefix}

GdipSetStringFormatTabStops: function(format: GPSTRINGFORMAT;  
    firstTabOffset: Single; count: Integer;
    tabStops: PSingle): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetStringFormatTabStops}

GdipGetStringFormatTabStops: function(format: GPSTRINGFORMAT;  
    count: Integer; firstTabOffset: PSingle;
    tabStops: PSingle): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetStringFormatTabStops}

GdipGetStringFormatTabStopCount: function(format: GPSTRINGFORMAT;  
    out count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetStringFormatTabStopCount}

GdipSetStringFormatDigitSubstitution: function(format: GPSTRINGFORMAT;  
    language: LANGID;
    substitute: STRINGDIGITSUBSTITUTE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetStringFormatDigitSubstitution}

GdipGetStringFormatDigitSubstitution: function(format: GPSTRINGFORMAT;  
    language: PUINT; substitute: PSTRINGDIGITSUBSTITUTE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetStringFormatDigitSubstitution}

GdipGetStringFormatMeasurableCharacterRangeCount: function(format: GPSTRINGFORMAT;  
    out count: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipGetStringFormatMeasurableCharacterRangeCount}

GdipSetStringFormatMeasurableCharacterRanges: function(format: GPSTRINGFORMAT;
    rangeCount: Integer; ranges: PCHARACTERRANGE): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipSetStringFormatMeasurableCharacterRanges}

//----------------------------------------------------------------------------
// Cached Bitmap APIs
//----------------------------------------------------------------------------

GdipCreateCachedBitmap: function(bitmap: GPBITMAP; graphics: GPGRAPHICS;  
    out cachedBitmap: GPCACHEDBITMAP): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipCreateCachedBitmap}

GdipDeleteCachedBitmap: function(  
    cachedBitmap: GPCACHEDBITMAP): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipDeleteCachedBitmap}

GdipDrawCachedBitmap: function(graphics: GPGRAPHICS;  
    cachedBitmap: GPCACHEDBITMAP; x: Integer;
    y: Integer): GPSTATUS; stdcall;
  {.$EXTERNALSYM GdipDrawCachedBitmap}

GdipEmfToWmfBits: function(hemf: HENHMETAFILE; cbData16: UINT; pData16: PBYTE;  
    iMapMode: Integer; eFlags: Integer): UINT; stdcall;
  {.$EXTERNALSYM GdipEmfToWmfBits}

var
  GDIPlusFileName: string;
  GdipLibrary: THandle = 0;

procedure LoadGdiplus;
begin
  GDIPlusFileName := 'gdiplus.dll';
  if FileExists(ExtractFilePath(ParamStr(0)) + GDIPlusFileName) then
    GdipLibrary := LoadLibrary(PChar(ExtractFilePath(ParamStr(0)) + GDIPlusFileName))
  else
    if FileExists(ExtractFilePath(ParamStr(0)) + '..\' + GDIPlusFileName) then
      GdipLibrary := LoadLibrary(PChar(ExtractFilePath(ParamStr(0)) + '..\' + GDIPlusFileName))
    else
      if FileExists(ExtractFilePath(ParamStr(0)) + '..\projects\bpl\' + GDIPlusFileName) then
        GdipLibrary := LoadLibrary(PChar(ExtractFilePath(ParamStr(0)) + '..\projects\bpl\' + GDIPlusFileName))
      else
      if FileExists(ExtractFilePath(ParamStr(0)) + '..\..\' + GDIPlusFileName) then
        GdipLibrary := LoadLibrary(PChar(ExtractFilePath(ParamStr(0)) + '..\..\' + GDIPlusFileName))
      else
        GdipLibrary := LoadLibrary(PChar(GDIPlusFileName));

  if GdipLibrary > 0 then
  begin
    GdipAlloc := GetProcAddress(GdipLibrary, 'GdipAlloc');
    GdipFree := GetProcAddress(GdipLibrary, 'GdipFree');
    GdiplusStartup := GetProcAddress(GdipLibrary, 'GdiplusStartup');
    GdiplusShutdown := GetProcAddress(GdipLibrary, 'GdiplusShutdown');

    GdipCreatePath := GetProcAddress(GdipLibrary, 'GdipCreatePath');
    GdipCreatePath2 := GetProcAddress(GdipLibrary, 'GdipCreatePath2');
    GdipCreatePath2I := GetProcAddress(GdipLibrary, 'GdipCreatePath2I');
    GdipClonePath := GetProcAddress(GdipLibrary, 'GdipClonePath');
    GdipDeletePath := GetProcAddress(GdipLibrary, 'GdipDeletePath');
    GdipResetPath := GetProcAddress(GdipLibrary, 'GdipResetPath');
    GdipGetPointCount := GetProcAddress(GdipLibrary, 'GdipGetPointCount');
    GdipGetPathTypes := GetProcAddress(GdipLibrary, 'GdipGetPathTypes');
    GdipGetPathPoints := GetProcAddress(GdipLibrary, 'GdipGetPathPoints');
    GdipGetPathPointsI := GetProcAddress(GdipLibrary, 'GdipGetPathPointsI');
    GdipGetPathFillMode := GetProcAddress(GdipLibrary, 'GdipGetPathFillMode');
    GdipSetPathFillMode := GetProcAddress(GdipLibrary, 'GdipSetPathFillMode');
    GdipGetPathData := GetProcAddress(GdipLibrary, 'GdipGetPathData');
    GdipStartPathFigure := GetProcAddress(GdipLibrary, 'GdipStartPathFigure');
    GdipClosePathFigure := GetProcAddress(GdipLibrary, 'GdipClosePathFigure');
    GdipClosePathFigures := GetProcAddress(GdipLibrary, 'GdipClosePathFigures');
    GdipSetPathMarker := GetProcAddress(GdipLibrary, 'GdipSetPathMarker');
    GdipClearPathMarkers := GetProcAddress(GdipLibrary, 'GdipClearPathMarkers');
    GdipReversePath := GetProcAddress(GdipLibrary, 'GdipReversePath');
    GdipGetPathLastPoint := GetProcAddress(GdipLibrary, 'GdipGetPathLastPoint');
    GdipAddPathLine := GetProcAddress(GdipLibrary, 'GdipAddPathLine');
    GdipAddPathLine2 := GetProcAddress(GdipLibrary, 'GdipAddPathLine2');
    GdipAddPathArc := GetProcAddress(GdipLibrary, 'GdipAddPathArc');
    GdipAddPathBezier := GetProcAddress(GdipLibrary, 'GdipAddPathBezier');
    GdipAddPathBeziers := GetProcAddress(GdipLibrary, 'GdipAddPathBeziers');
    GdipAddPathCurve := GetProcAddress(GdipLibrary, 'GdipAddPathCurve');
    GdipAddPathCurve2 := GetProcAddress(GdipLibrary, 'GdipAddPathCurve2');
    GdipAddPathCurve3 := GetProcAddress(GdipLibrary, 'GdipAddPathCurve3');
    GdipAddPathClosedCurve := GetProcAddress(GdipLibrary, 'GdipAddPathClosedCurve');
    GdipAddPathClosedCurve2 := GetProcAddress(GdipLibrary, 'GdipAddPathClosedCurve2');
    GdipAddPathRectangle := GetProcAddress(GdipLibrary, 'GdipAddPathRectangle');
    GdipAddPathRectangles := GetProcAddress(GdipLibrary, 'GdipAddPathRectangles');
    GdipAddPathEllipse := GetProcAddress(GdipLibrary, 'GdipAddPathEllipse');
    GdipAddPathPie := GetProcAddress(GdipLibrary, 'GdipAddPathPie');
    GdipAddPathPolygon := GetProcAddress(GdipLibrary, 'GdipAddPathPolygon');
    GdipAddPathPath := GetProcAddress(GdipLibrary, 'GdipAddPathPath');
    GdipAddPathString := GetProcAddress(GdipLibrary, 'GdipAddPathString');
    GdipAddPathStringI := GetProcAddress(GdipLibrary, 'GdipAddPathStringI');
    GdipAddPathLineI := GetProcAddress(GdipLibrary, 'GdipAddPathLineI');
    GdipAddPathLine2I := GetProcAddress(GdipLibrary, 'GdipAddPathLine2I');
    GdipAddPathArcI := GetProcAddress(GdipLibrary, 'GdipAddPathArcI');
    GdipAddPathBezierI := GetProcAddress(GdipLibrary, 'GdipAddPathBezierI');
    GdipAddPathBeziersI := GetProcAddress(GdipLibrary, 'GdipAddPathBeziersI');
    GdipAddPathCurveI := GetProcAddress(GdipLibrary, 'GdipAddPathCurveI');
    GdipAddPathCurve2I := GetProcAddress(GdipLibrary, 'GdipAddPathCurve2I');
    GdipAddPathCurve3I := GetProcAddress(GdipLibrary, 'GdipAddPathCurve3I');
    GdipAddPathClosedCurveI := GetProcAddress(GdipLibrary, 'GdipAddPathClosedCurveI');
    GdipAddPathClosedCurve2I := GetProcAddress(GdipLibrary, 'GdipAddPathClosedCurve2I');
    GdipAddPathRectangleI := GetProcAddress(GdipLibrary, 'GdipAddPathRectangleI');
    GdipAddPathRectanglesI := GetProcAddress(GdipLibrary, 'GdipAddPathRectanglesI');
    GdipAddPathEllipseI := GetProcAddress(GdipLibrary, 'GdipAddPathEllipseI');
    GdipAddPathPieI := GetProcAddress(GdipLibrary, 'GdipAddPathPieI');
    GdipAddPathPolygonI := GetProcAddress(GdipLibrary, 'GdipAddPathPolygonI');
    GdipFlattenPath := GetProcAddress(GdipLibrary, 'GdipFlattenPath');
    GdipWindingModeOutline := GetProcAddress(GdipLibrary, 'GdipWindingModeOutline');
    GdipWidenPath := GetProcAddress(GdipLibrary, 'GdipWidenPath');
    GdipWarpPath := GetProcAddress(GdipLibrary, 'GdipWarpPath');
    GdipTransformPath := GetProcAddress(GdipLibrary, 'GdipTransformPath');
    GdipGetPathWorldBounds := GetProcAddress(GdipLibrary, 'GdipGetPathWorldBounds');
    GdipGetPathWorldBoundsI := GetProcAddress(GdipLibrary, 'GdipGetPathWorldBoundsI');
    GdipIsVisiblePathPoint := GetProcAddress(GdipLibrary, 'GdipIsVisiblePathPoint');
    GdipIsVisiblePathPointI := GetProcAddress(GdipLibrary, 'GdipIsVisiblePathPointI');
    GdipIsOutlineVisiblePathPoint := GetProcAddress(GdipLibrary, 'GdipIsOutlineVisiblePathPoint');
    GdipIsOutlineVisiblePathPointI := GetProcAddress(GdipLibrary, 'GdipIsOutlineVisiblePathPointI');
    GdipCreatePathIter := GetProcAddress(GdipLibrary, 'GdipCreatePathIter');
    GdipDeletePathIter := GetProcAddress(GdipLibrary, 'GdipDeletePathIter');
    GdipPathIterNextSubpath := GetProcAddress(GdipLibrary, 'GdipPathIterNextSubpath');
    GdipPathIterNextSubpathPath := GetProcAddress(GdipLibrary, 'GdipPathIterNextSubpathPath');
    GdipPathIterNextPathType := GetProcAddress(GdipLibrary, 'GdipPathIterNextPathType');
    GdipPathIterNextMarker := GetProcAddress(GdipLibrary, 'GdipPathIterNextMarker');
    GdipPathIterNextMarkerPath := GetProcAddress(GdipLibrary, 'GdipPathIterNextMarkerPath');
    GdipPathIterGetCount := GetProcAddress(GdipLibrary, 'GdipPathIterGetCount');
    GdipPathIterGetSubpathCount := GetProcAddress(GdipLibrary, 'GdipPathIterGetSubpathCount');
    GdipPathIterIsValid := GetProcAddress(GdipLibrary, 'GdipPathIterIsValid');
    GdipPathIterHasCurve := GetProcAddress(GdipLibrary, 'GdipPathIterHasCurve');
    GdipPathIterRewind := GetProcAddress(GdipLibrary, 'GdipPathIterRewind');
    GdipPathIterEnumerate := GetProcAddress(GdipLibrary, 'GdipPathIterEnumerate');
    GdipPathIterCopyData := GetProcAddress(GdipLibrary, 'GdipPathIterCopyData');
    GdipCreateMatrix := GetProcAddress(GdipLibrary, 'GdipCreateMatrix');
    GdipCreateMatrix2 := GetProcAddress(GdipLibrary, 'GdipCreateMatrix2');
    GdipCreateMatrix3 := GetProcAddress(GdipLibrary, 'GdipCreateMatrix3');
    GdipCreateMatrix3I := GetProcAddress(GdipLibrary, 'GdipCreateMatrix3I');
    GdipCloneMatrix := GetProcAddress(GdipLibrary, 'GdipCloneMatrix');
    GdipDeleteMatrix := GetProcAddress(GdipLibrary, 'GdipDeleteMatrix');
    GdipSetMatrixElements := GetProcAddress(GdipLibrary, 'GdipSetMatrixElements');
    GdipMultiplyMatrix := GetProcAddress(GdipLibrary, 'GdipMultiplyMatrix');
    GdipTranslateMatrix := GetProcAddress(GdipLibrary, 'GdipTranslateMatrix');
    GdipScaleMatrix := GetProcAddress(GdipLibrary, 'GdipScaleMatrix');
    GdipRotateMatrix := GetProcAddress(GdipLibrary, 'GdipRotateMatrix');
    GdipShearMatrix := GetProcAddress(GdipLibrary, 'GdipShearMatrix');
    GdipInvertMatrix := GetProcAddress(GdipLibrary, 'GdipInvertMatrix');
    GdipTransformMatrixPoints := GetProcAddress(GdipLibrary, 'GdipTransformMatrixPoints');
    GdipTransformMatrixPointsI := GetProcAddress(GdipLibrary, 'GdipTransformMatrixPointsI');
    GdipVectorTransformMatrixPoints := GetProcAddress(GdipLibrary, 'GdipVectorTransformMatrixPoints');
    GdipVectorTransformMatrixPointsI := GetProcAddress(GdipLibrary, 'GdipVectorTransformMatrixPointsI');
    GdipGetMatrixElements := GetProcAddress(GdipLibrary, 'GdipGetMatrixElements');
    GdipIsMatrixInvertible := GetProcAddress(GdipLibrary, 'GdipIsMatrixInvertible');
    GdipIsMatrixIdentity := GetProcAddress(GdipLibrary, 'GdipIsMatrixIdentity');
    GdipIsMatrixEqual := GetProcAddress(GdipLibrary, 'GdipIsMatrixEqual');
    GdipCreateRegion := GetProcAddress(GdipLibrary, 'GdipCreateRegion');
    GdipCreateRegionRect := GetProcAddress(GdipLibrary, 'GdipCreateRegionRect');
    GdipCreateRegionRectI := GetProcAddress(GdipLibrary, 'GdipCreateRegionRectI');
    GdipCreateRegionPath := GetProcAddress(GdipLibrary, 'GdipCreateRegionPath');
    GdipCreateRegionRgnData := GetProcAddress(GdipLibrary, 'GdipCreateRegionRgnData');
    GdipCreateRegionHrgn := GetProcAddress(GdipLibrary, 'GdipCreateRegionHrgn');
    GdipCloneRegion := GetProcAddress(GdipLibrary, 'GdipCloneRegion');
    GdipDeleteRegion := GetProcAddress(GdipLibrary, 'GdipDeleteRegion');
    GdipSetInfinite := GetProcAddress(GdipLibrary, 'GdipSetInfinite');
    GdipSetEmpty := GetProcAddress(GdipLibrary, 'GdipSetEmpty');
    GdipCombineRegionRect := GetProcAddress(GdipLibrary, 'GdipCombineRegionRect');
    GdipCombineRegionRectI := GetProcAddress(GdipLibrary, 'GdipCombineRegionRectI');
    GdipCombineRegionPath := GetProcAddress(GdipLibrary, 'GdipCombineRegionPath');
    GdipCombineRegionRegion := GetProcAddress(GdipLibrary, 'GdipCombineRegionRegion');
    GdipTranslateRegion := GetProcAddress(GdipLibrary, 'GdipTranslateRegion');
    GdipTranslateRegionI := GetProcAddress(GdipLibrary, 'GdipTranslateRegionI');
    GdipTransformRegion := GetProcAddress(GdipLibrary, 'GdipTransformRegion');
    GdipGetRegionBounds := GetProcAddress(GdipLibrary, 'GdipGetRegionBounds');
    GdipGetRegionBoundsI := GetProcAddress(GdipLibrary, 'GdipGetRegionBoundsI');
    GdipGetRegionHRgn := GetProcAddress(GdipLibrary, 'GdipGetRegionHRgn');
    GdipIsEmptyRegion := GetProcAddress(GdipLibrary, 'GdipIsEmptyRegion');
    GdipIsInfiniteRegion := GetProcAddress(GdipLibrary, 'GdipIsInfiniteRegion');
    GdipIsEqualRegion := GetProcAddress(GdipLibrary, 'GdipIsEqualRegion');
    GdipGetRegionDataSize := GetProcAddress(GdipLibrary, 'GdipGetRegionDataSize');
    GdipGetRegionData := GetProcAddress(GdipLibrary, 'GdipGetRegionData');
    GdipIsVisibleRegionPoint := GetProcAddress(GdipLibrary, 'GdipIsVisibleRegionPoint');
    GdipIsVisibleRegionPointI := GetProcAddress(GdipLibrary, 'GdipIsVisibleRegionPointI');
    GdipIsVisibleRegionRect := GetProcAddress(GdipLibrary, 'GdipIsVisibleRegionRect');
    GdipIsVisibleRegionRectI := GetProcAddress(GdipLibrary, 'GdipIsVisibleRegionRectI');
    GdipGetRegionScansCount := GetProcAddress(GdipLibrary, 'GdipGetRegionScansCount');
    GdipGetRegionScans := GetProcAddress(GdipLibrary, 'GdipGetRegionScans');
    GdipGetRegionScansI := GetProcAddress(GdipLibrary, 'GdipGetRegionScansI');
    GdipCloneBrush := GetProcAddress(GdipLibrary, 'GdipCloneBrush');
    GdipDeleteBrush := GetProcAddress(GdipLibrary, 'GdipDeleteBrush');
    GdipGetBrushType := GetProcAddress(GdipLibrary, 'GdipGetBrushType');
    GdipCreateHatchBrush := GetProcAddress(GdipLibrary, 'GdipCreateHatchBrush');
    GdipGetHatchStyle := GetProcAddress(GdipLibrary, 'GdipGetHatchStyle');
    GdipGetHatchForegroundColor := GetProcAddress(GdipLibrary, 'GdipGetHatchForegroundColor');
    GdipGetHatchBackgroundColor := GetProcAddress(GdipLibrary, 'GdipGetHatchBackgroundColor');
    GdipCreateTexture := GetProcAddress(GdipLibrary, 'GdipCreateTexture');
    GdipCreateTexture2 := GetProcAddress(GdipLibrary, 'GdipCreateTexture2');
    GdipCreateTextureIA := GetProcAddress(GdipLibrary, 'GdipCreateTextureIA');
    GdipCreateTexture2I := GetProcAddress(GdipLibrary, 'GdipCreateTexture2I');
    GdipCreateTextureIAI := GetProcAddress(GdipLibrary, 'GdipCreateTextureIAI');
    GdipGetTextureTransform := GetProcAddress(GdipLibrary, 'GdipGetTextureTransform');
    GdipSetTextureTransform := GetProcAddress(GdipLibrary, 'GdipSetTextureTransform');
    GdipResetTextureTransform := GetProcAddress(GdipLibrary, 'GdipResetTextureTransform');
    GdipMultiplyTextureTransform := GetProcAddress(GdipLibrary, 'GdipMultiplyTextureTransform');
    GdipTranslateTextureTransform := GetProcAddress(GdipLibrary, 'GdipTranslateTextureTransform');
    GdipScaleTextureTransform := GetProcAddress(GdipLibrary, 'GdipScaleTextureTransform');
    GdipRotateTextureTransform := GetProcAddress(GdipLibrary, 'GdipRotateTextureTransform');
    GdipSetTextureWrapMode := GetProcAddress(GdipLibrary, 'GdipSetTextureWrapMode');
    GdipGetTextureWrapMode := GetProcAddress(GdipLibrary, 'GdipGetTextureWrapMode');
    GdipGetTextureImage := GetProcAddress(GdipLibrary, 'GdipGetTextureImage');
    GdipCreateSolidFill := GetProcAddress(GdipLibrary, 'GdipCreateSolidFill');
    GdipSetSolidFillColor := GetProcAddress(GdipLibrary, 'GdipSetSolidFillColor');
    GdipGetSolidFillColor := GetProcAddress(GdipLibrary, 'GdipGetSolidFillColor');
    GdipCreateLineBrush := GetProcAddress(GdipLibrary, 'GdipCreateLineBrush');
    GdipCreateLineBrushI := GetProcAddress(GdipLibrary, 'GdipCreateLineBrushI');
    GdipCreateLineBrushFromRect := GetProcAddress(GdipLibrary, 'GdipCreateLineBrushFromRect');
    GdipCreateLineBrushFromRectI := GetProcAddress(GdipLibrary, 'GdipCreateLineBrushFromRectI');
    GdipCreateLineBrushFromRectWithAngle := GetProcAddress(GdipLibrary, 'GdipCreateLineBrushFromRectWithAngle');
    GdipCreateLineBrushFromRectWithAngleI := GetProcAddress(GdipLibrary, 'GdipCreateLineBrushFromRectWithAngleI');
    GdipSetLineColors := GetProcAddress(GdipLibrary, 'GdipSetLineColors');
    GdipGetLineColors := GetProcAddress(GdipLibrary, 'GdipGetLineColors');
    GdipGetLineRect := GetProcAddress(GdipLibrary, 'GdipGetLineRect');
    GdipGetLineRectI := GetProcAddress(GdipLibrary, 'GdipGetLineRectI');
    GdipSetLineGammaCorrection := GetProcAddress(GdipLibrary, 'GdipSetLineGammaCorrection');
    GdipGetLineGammaCorrection := GetProcAddress(GdipLibrary, 'GdipGetLineGammaCorrection');
    GdipGetLineBlendCount := GetProcAddress(GdipLibrary, 'GdipGetLineBlendCount');
    GdipGetLineBlend := GetProcAddress(GdipLibrary, 'GdipGetLineBlend');
    GdipSetLineBlend := GetProcAddress(GdipLibrary, 'GdipSetLineBlend');
    GdipGetLinePresetBlendCount := GetProcAddress(GdipLibrary, 'GdipGetLinePresetBlendCount');
    GdipGetLinePresetBlend := GetProcAddress(GdipLibrary, 'GdipGetLinePresetBlend');
    GdipSetLinePresetBlend := GetProcAddress(GdipLibrary, 'GdipSetLinePresetBlend');
    GdipSetLineSigmaBlend := GetProcAddress(GdipLibrary, 'GdipSetLineSigmaBlend');
    GdipSetLineLinearBlend := GetProcAddress(GdipLibrary, 'GdipSetLineLinearBlend');
    GdipSetLineWrapMode := GetProcAddress(GdipLibrary, 'GdipSetLineWrapMode');
    GdipGetLineWrapMode := GetProcAddress(GdipLibrary, 'GdipGetLineWrapMode');
    GdipGetLineTransform := GetProcAddress(GdipLibrary, 'GdipGetLineTransform');
    GdipSetLineTransform := GetProcAddress(GdipLibrary, 'GdipSetLineTransform');
    GdipResetLineTransform := GetProcAddress(GdipLibrary, 'GdipResetLineTransform');
    GdipMultiplyLineTransform := GetProcAddress(GdipLibrary, 'GdipMultiplyLineTransform');
    GdipTranslateLineTransform := GetProcAddress(GdipLibrary, 'GdipTranslateLineTransform');
    GdipScaleLineTransform := GetProcAddress(GdipLibrary, 'GdipScaleLineTransform');
    GdipRotateLineTransform := GetProcAddress(GdipLibrary, 'GdipRotateLineTransform');
    GdipCreatePathGradient := GetProcAddress(GdipLibrary, 'GdipCreatePathGradient');
    GdipCreatePathGradientI := GetProcAddress(GdipLibrary, 'GdipCreatePathGradientI');
    GdipCreatePathGradientFromPath := GetProcAddress(GdipLibrary, 'GdipCreatePathGradientFromPath');
    GdipGetPathGradientCenterColor := GetProcAddress(GdipLibrary, 'GdipGetPathGradientCenterColor');
    GdipSetPathGradientCenterColor := GetProcAddress(GdipLibrary, 'GdipSetPathGradientCenterColor');
    GdipGetPathGradientSurroundColorsWithCount := GetProcAddress(GdipLibrary, 'GdipGetPathGradientSurroundColorsWithCount');
    GdipSetPathGradientSurroundColorsWithCount := GetProcAddress(GdipLibrary, 'GdipSetPathGradientSurroundColorsWithCount');
    GdipGetPathGradientPath := GetProcAddress(GdipLibrary, 'GdipGetPathGradientPath');
    GdipSetPathGradientPath := GetProcAddress(GdipLibrary, 'GdipSetPathGradientPath');
    GdipGetPathGradientCenterPoint := GetProcAddress(GdipLibrary, 'GdipGetPathGradientCenterPoint');
    GdipGetPathGradientCenterPointI := GetProcAddress(GdipLibrary, 'GdipGetPathGradientCenterPointI');
    GdipSetPathGradientCenterPoint := GetProcAddress(GdipLibrary, 'GdipSetPathGradientCenterPoint');
    GdipSetPathGradientCenterPointI := GetProcAddress(GdipLibrary, 'GdipSetPathGradientCenterPointI');
    GdipGetPathGradientRect := GetProcAddress(GdipLibrary, 'GdipGetPathGradientRect');
    GdipGetPathGradientRectI := GetProcAddress(GdipLibrary, 'GdipGetPathGradientRectI');
    GdipGetPathGradientPointCount := GetProcAddress(GdipLibrary, 'GdipGetPathGradientPointCount');
    GdipGetPathGradientSurroundColorCount := GetProcAddress(GdipLibrary, 'GdipGetPathGradientSurroundColorCount');
    GdipSetPathGradientGammaCorrection := GetProcAddress(GdipLibrary, 'GdipSetPathGradientGammaCorrection');
    GdipGetPathGradientGammaCorrection := GetProcAddress(GdipLibrary, 'GdipGetPathGradientGammaCorrection');
    GdipGetPathGradientBlendCount := GetProcAddress(GdipLibrary, 'GdipGetPathGradientBlendCount');
    GdipGetPathGradientBlend := GetProcAddress(GdipLibrary, 'GdipGetPathGradientBlend');
    GdipSetPathGradientBlend := GetProcAddress(GdipLibrary, 'GdipSetPathGradientBlend');
    GdipGetPathGradientPresetBlendCount := GetProcAddress(GdipLibrary, 'GdipGetPathGradientPresetBlendCount');
    GdipGetPathGradientPresetBlend := GetProcAddress(GdipLibrary, 'GdipGetPathGradientPresetBlend');
    GdipSetPathGradientPresetBlend := GetProcAddress(GdipLibrary, 'GdipSetPathGradientPresetBlend');
    GdipSetPathGradientSigmaBlend := GetProcAddress(GdipLibrary, 'GdipSetPathGradientSigmaBlend');
    GdipSetPathGradientLinearBlend := GetProcAddress(GdipLibrary, 'GdipSetPathGradientLinearBlend');
    GdipGetPathGradientWrapMode := GetProcAddress(GdipLibrary, 'GdipGetPathGradientWrapMode');
    GdipSetPathGradientWrapMode := GetProcAddress(GdipLibrary, 'GdipSetPathGradientWrapMode');
    GdipGetPathGradientTransform := GetProcAddress(GdipLibrary, 'GdipGetPathGradientTransform');
    GdipSetPathGradientTransform := GetProcAddress(GdipLibrary, 'GdipSetPathGradientTransform');
    GdipResetPathGradientTransform := GetProcAddress(GdipLibrary, 'GdipResetPathGradientTransform');
    GdipMultiplyPathGradientTransform := GetProcAddress(GdipLibrary, 'GdipMultiplyPathGradientTransform');
    GdipTranslatePathGradientTransform := GetProcAddress(GdipLibrary, 'GdipTranslatePathGradientTransform');
    GdipScalePathGradientTransform := GetProcAddress(GdipLibrary, 'GdipScalePathGradientTransform');
    GdipRotatePathGradientTransform := GetProcAddress(GdipLibrary, 'GdipRotatePathGradientTransform');
    GdipGetPathGradientFocusScales := GetProcAddress(GdipLibrary, 'GdipGetPathGradientFocusScales');
    GdipSetPathGradientFocusScales := GetProcAddress(GdipLibrary, 'GdipSetPathGradientFocusScales');
    GdipCreatePen1 := GetProcAddress(GdipLibrary, 'GdipCreatePen1');
    GdipCreatePen2 := GetProcAddress(GdipLibrary, 'GdipCreatePen2');
    GdipClonePen := GetProcAddress(GdipLibrary, 'GdipClonePen');
    GdipDeletePen := GetProcAddress(GdipLibrary, 'GdipDeletePen');
    GdipSetPenWidth := GetProcAddress(GdipLibrary, 'GdipSetPenWidth');
    GdipGetPenWidth := GetProcAddress(GdipLibrary, 'GdipGetPenWidth');
    GdipSetPenUnit := GetProcAddress(GdipLibrary, 'GdipSetPenUnit');
    GdipGetPenUnit := GetProcAddress(GdipLibrary, 'GdipGetPenUnit');
    GdipSetPenLineCap197819 := GetProcAddress(GdipLibrary, 'GdipSetPenLineCap197819');
    GdipSetPenStartCap := GetProcAddress(GdipLibrary, 'GdipSetPenStartCap');
    GdipSetPenEndCap := GetProcAddress(GdipLibrary, 'GdipSetPenEndCap');
    GdipSetPenDashCap197819 := GetProcAddress(GdipLibrary, 'GdipSetPenDashCap197819');
    GdipGetPenStartCap := GetProcAddress(GdipLibrary, 'GdipGetPenStartCap');
    GdipGetPenEndCap := GetProcAddress(GdipLibrary, 'GdipGetPenEndCap');
    GdipGetPenDashCap197819 := GetProcAddress(GdipLibrary, 'GdipGetPenDashCap197819');
    GdipSetPenLineJoin := GetProcAddress(GdipLibrary, 'GdipSetPenLineJoin');
    GdipGetPenLineJoin := GetProcAddress(GdipLibrary, 'GdipGetPenLineJoin');
    GdipSetPenCustomStartCap := GetProcAddress(GdipLibrary, 'GdipSetPenCustomStartCap');
    GdipGetPenCustomStartCap := GetProcAddress(GdipLibrary, 'GdipGetPenCustomStartCap');
    GdipSetPenCustomEndCap := GetProcAddress(GdipLibrary, 'GdipSetPenCustomEndCap');
    GdipGetPenCustomEndCap := GetProcAddress(GdipLibrary, 'GdipGetPenCustomEndCap');
    GdipSetPenMiterLimit := GetProcAddress(GdipLibrary, 'GdipSetPenMiterLimit');
    GdipGetPenMiterLimit := GetProcAddress(GdipLibrary, 'GdipGetPenMiterLimit');
    GdipSetPenMode := GetProcAddress(GdipLibrary, 'GdipSetPenMode');
    GdipGetPenMode := GetProcAddress(GdipLibrary, 'GdipGetPenMode');
    GdipSetPenTransform := GetProcAddress(GdipLibrary, 'GdipSetPenTransform');
    GdipGetPenTransform := GetProcAddress(GdipLibrary, 'GdipGetPenTransform');
    GdipResetPenTransform := GetProcAddress(GdipLibrary, 'GdipResetPenTransform');
    GdipMultiplyPenTransform := GetProcAddress(GdipLibrary, 'GdipMultiplyPenTransform');
    GdipTranslatePenTransform := GetProcAddress(GdipLibrary, 'GdipTranslatePenTransform');
    GdipScalePenTransform := GetProcAddress(GdipLibrary, 'GdipScalePenTransform');
    GdipRotatePenTransform := GetProcAddress(GdipLibrary, 'GdipRotatePenTransform');
    GdipSetPenColor := GetProcAddress(GdipLibrary, 'GdipSetPenColor');
    GdipGetPenColor := GetProcAddress(GdipLibrary, 'GdipGetPenColor');
    GdipSetPenBrushFill := GetProcAddress(GdipLibrary, 'GdipSetPenBrushFill');
    GdipGetPenBrushFill := GetProcAddress(GdipLibrary, 'GdipGetPenBrushFill');
    GdipGetPenFillType := GetProcAddress(GdipLibrary, 'GdipGetPenFillType');
    GdipGetPenDashStyle := GetProcAddress(GdipLibrary, 'GdipGetPenDashStyle');
    GdipSetPenDashStyle := GetProcAddress(GdipLibrary, 'GdipSetPenDashStyle');
    GdipGetPenDashOffset := GetProcAddress(GdipLibrary, 'GdipGetPenDashOffset');
    GdipSetPenDashOffset := GetProcAddress(GdipLibrary, 'GdipSetPenDashOffset');
    GdipGetPenDashCount := GetProcAddress(GdipLibrary, 'GdipGetPenDashCount');
    GdipSetPenDashArray := GetProcAddress(GdipLibrary, 'GdipSetPenDashArray');
    GdipGetPenDashArray := GetProcAddress(GdipLibrary, 'GdipGetPenDashArray');
    GdipGetPenCompoundCount := GetProcAddress(GdipLibrary, 'GdipGetPenCompoundCount');
    GdipSetPenCompoundArray := GetProcAddress(GdipLibrary, 'GdipSetPenCompoundArray');
    GdipGetPenCompoundArray := GetProcAddress(GdipLibrary, 'GdipGetPenCompoundArray');
    GdipCreateCustomLineCap := GetProcAddress(GdipLibrary, 'GdipCreateCustomLineCap');
    GdipDeleteCustomLineCap := GetProcAddress(GdipLibrary, 'GdipDeleteCustomLineCap');
    GdipCloneCustomLineCap := GetProcAddress(GdipLibrary, 'GdipCloneCustomLineCap');
    GdipGetCustomLineCapType := GetProcAddress(GdipLibrary, 'GdipGetCustomLineCapType');
    GdipSetCustomLineCapStrokeCaps := GetProcAddress(GdipLibrary, 'GdipSetCustomLineCapStrokeCaps');
    GdipGetCustomLineCapStrokeCaps := GetProcAddress(GdipLibrary, 'GdipGetCustomLineCapStrokeCaps');
    GdipSetCustomLineCapStrokeJoin := GetProcAddress(GdipLibrary, 'GdipSetCustomLineCapStrokeJoin');
    GdipGetCustomLineCapStrokeJoin := GetProcAddress(GdipLibrary, 'GdipGetCustomLineCapStrokeJoin');
    GdipSetCustomLineCapBaseCap := GetProcAddress(GdipLibrary, 'GdipSetCustomLineCapBaseCap');
    GdipGetCustomLineCapBaseCap := GetProcAddress(GdipLibrary, 'GdipGetCustomLineCapBaseCap');
    GdipSetCustomLineCapBaseInset := GetProcAddress(GdipLibrary, 'GdipSetCustomLineCapBaseInset');
    GdipGetCustomLineCapBaseInset := GetProcAddress(GdipLibrary, 'GdipGetCustomLineCapBaseInset');
    GdipSetCustomLineCapWidthScale := GetProcAddress(GdipLibrary, 'GdipSetCustomLineCapWidthScale');
    GdipGetCustomLineCapWidthScale := GetProcAddress(GdipLibrary, 'GdipGetCustomLineCapWidthScale');
    GdipCreateAdjustableArrowCap := GetProcAddress(GdipLibrary, 'GdipCreateAdjustableArrowCap');
    GdipSetAdjustableArrowCapHeight := GetProcAddress(GdipLibrary, 'GdipSetAdjustableArrowCapHeight');
    GdipGetAdjustableArrowCapHeight := GetProcAddress(GdipLibrary, 'GdipGetAdjustableArrowCapHeight');
    GdipSetAdjustableArrowCapWidth := GetProcAddress(GdipLibrary, 'GdipSetAdjustableArrowCapWidth');
    GdipGetAdjustableArrowCapWidth := GetProcAddress(GdipLibrary, 'GdipGetAdjustableArrowCapWidth');
    GdipSetAdjustableArrowCapMiddleInset := GetProcAddress(GdipLibrary, 'GdipSetAdjustableArrowCapMiddleInset');
    GdipGetAdjustableArrowCapMiddleInset := GetProcAddress(GdipLibrary, 'GdipGetAdjustableArrowCapMiddleInset');
    GdipSetAdjustableArrowCapFillState := GetProcAddress(GdipLibrary, 'GdipSetAdjustableArrowCapFillState');
    GdipGetAdjustableArrowCapFillState := GetProcAddress(GdipLibrary, 'GdipGetAdjustableArrowCapFillState');
    GdipLoadImageFromStream := GetProcAddress(GdipLibrary, 'GdipLoadImageFromStream');
    GdipLoadImageFromFile := GetProcAddress(GdipLibrary, 'GdipLoadImageFromFile');
    GdipLoadImageFromStreamICM := GetProcAddress(GdipLibrary, 'GdipLoadImageFromStreamICM');
    GdipLoadImageFromFileICM := GetProcAddress(GdipLibrary, 'GdipLoadImageFromFileICM');
    GdipCloneImage := GetProcAddress(GdipLibrary, 'GdipCloneImage');
    GdipDisposeImage := GetProcAddress(GdipLibrary, 'GdipDisposeImage');
    GdipSaveImageToFile := GetProcAddress(GdipLibrary, 'GdipSaveImageToFile');
    GdipSaveImageToStream := GetProcAddress(GdipLibrary, 'GdipSaveImageToStream');
    GdipSaveAdd := GetProcAddress(GdipLibrary, 'GdipSaveAdd');
    GdipSaveAddImage := GetProcAddress(GdipLibrary, 'GdipSaveAddImage');
    GdipGetImageGraphicsContext := GetProcAddress(GdipLibrary, 'GdipGetImageGraphicsContext');
    GdipGetImageBounds := GetProcAddress(GdipLibrary, 'GdipGetImageBounds');
    GdipGetImageDimension := GetProcAddress(GdipLibrary, 'GdipGetImageDimension');
    GdipGetImageType := GetProcAddress(GdipLibrary, 'GdipGetImageType');
    GdipGetImageWidth := GetProcAddress(GdipLibrary, 'GdipGetImageWidth');
    GdipGetImageHeight := GetProcAddress(GdipLibrary, 'GdipGetImageHeight');
    GdipGetImageHorizontalResolution := GetProcAddress(GdipLibrary, 'GdipGetImageHorizontalResolution');
    GdipGetImageVerticalResolution := GetProcAddress(GdipLibrary, 'GdipGetImageVerticalResolution');
    GdipGetImageFlags := GetProcAddress(GdipLibrary, 'GdipGetImageFlags');
    GdipGetImageRawFormat := GetProcAddress(GdipLibrary, 'GdipGetImageRawFormat');
    GdipGetImagePixelFormat := GetProcAddress(GdipLibrary, 'GdipGetImagePixelFormat');
    GdipGetImageThumbnail := GetProcAddress(GdipLibrary, 'GdipGetImageThumbnail');
    GdipGetEncoderParameterListSize := GetProcAddress(GdipLibrary, 'GdipGetEncoderParameterListSize');
    GdipGetEncoderParameterList := GetProcAddress(GdipLibrary, 'GdipGetEncoderParameterList');
    GdipImageGetFrameDimensionsCount := GetProcAddress(GdipLibrary, 'GdipImageGetFrameDimensionsCount');
    GdipImageGetFrameDimensionsList := GetProcAddress(GdipLibrary, 'GdipImageGetFrameDimensionsList');
    GdipImageGetFrameCount := GetProcAddress(GdipLibrary, 'GdipImageGetFrameCount');
    GdipImageSelectActiveFrame := GetProcAddress(GdipLibrary, 'GdipImageSelectActiveFrame');
    GdipImageRotateFlip := GetProcAddress(GdipLibrary, 'GdipImageRotateFlip');
    GdipGetImagePalette := GetProcAddress(GdipLibrary, 'GdipGetImagePalette');
    GdipSetImagePalette := GetProcAddress(GdipLibrary, 'GdipSetImagePalette');
    GdipGetImagePaletteSize := GetProcAddress(GdipLibrary, 'GdipGetImagePaletteSize');
    GdipGetPropertyCount := GetProcAddress(GdipLibrary, 'GdipGetPropertyCount');
    GdipGetPropertyIdList := GetProcAddress(GdipLibrary, 'GdipGetPropertyIdList');
    GdipGetPropertyItemSize := GetProcAddress(GdipLibrary, 'GdipGetPropertyItemSize');
    GdipGetPropertyItem := GetProcAddress(GdipLibrary, 'GdipGetPropertyItem');
    GdipGetPropertySize := GetProcAddress(GdipLibrary, 'GdipGetPropertySize');
    GdipGetAllPropertyItems := GetProcAddress(GdipLibrary, 'GdipGetAllPropertyItems');
    GdipRemovePropertyItem := GetProcAddress(GdipLibrary, 'GdipRemovePropertyItem');
    GdipSetPropertyItem := GetProcAddress(GdipLibrary, 'GdipSetPropertyItem');
    GdipImageForceValidation := GetProcAddress(GdipLibrary, 'GdipImageForceValidation');
    GdipCreateBitmapFromStream := GetProcAddress(GdipLibrary, 'GdipCreateBitmapFromStream');
    GdipCreateBitmapFromFile := GetProcAddress(GdipLibrary, 'GdipCreateBitmapFromFile');
    GdipCreateBitmapFromStreamICM := GetProcAddress(GdipLibrary, 'GdipCreateBitmapFromStreamICM');
    GdipCreateBitmapFromFileICM := GetProcAddress(GdipLibrary, 'GdipCreateBitmapFromFileICM');
    GdipCreateBitmapFromScan0 := GetProcAddress(GdipLibrary, 'GdipCreateBitmapFromScan0');
    GdipCreateBitmapFromGraphics := GetProcAddress(GdipLibrary, 'GdipCreateBitmapFromGraphics');
    GdipCreateBitmapFromGdiDib := GetProcAddress(GdipLibrary, 'GdipCreateBitmapFromGdiDib');
    GdipCreateBitmapFromHBITMAP := GetProcAddress(GdipLibrary, 'GdipCreateBitmapFromHBITMAP');
    GdipCreateHBITMAPFromBitmap := GetProcAddress(GdipLibrary, 'GdipCreateHBITMAPFromBitmap');
    GdipCreateBitmapFromHICON := GetProcAddress(GdipLibrary, 'GdipCreateBitmapFromHICON');
    GdipCreateHICONFromBitmap := GetProcAddress(GdipLibrary, 'GdipCreateHICONFromBitmap');
    GdipCreateBitmapFromResource := GetProcAddress(GdipLibrary, 'GdipCreateBitmapFromResource');
    GdipCloneBitmapArea := GetProcAddress(GdipLibrary, 'GdipCloneBitmapArea');
    GdipCloneBitmapAreaI := GetProcAddress(GdipLibrary, 'GdipCloneBitmapAreaI');
    GdipBitmapLockBits := GetProcAddress(GdipLibrary, 'GdipBitmapLockBits');
    GdipBitmapUnlockBits := GetProcAddress(GdipLibrary, 'GdipBitmapUnlockBits');
    GdipBitmapGetPixel := GetProcAddress(GdipLibrary, 'GdipBitmapGetPixel');
    GdipBitmapSetPixel := GetProcAddress(GdipLibrary, 'GdipBitmapSetPixel');
    GdipBitmapSetResolution := GetProcAddress(GdipLibrary, 'GdipBitmapSetResolution');
    GdipCreateImageAttributes := GetProcAddress(GdipLibrary, 'GdipCreateImageAttributes');
    GdipCloneImageAttributes := GetProcAddress(GdipLibrary, 'GdipCloneImageAttributes');
    GdipDisposeImageAttributes := GetProcAddress(GdipLibrary, 'GdipDisposeImageAttributes');
    GdipSetImageAttributesToIdentity := GetProcAddress(GdipLibrary, 'GdipSetImageAttributesToIdentity');
    GdipResetImageAttributes := GetProcAddress(GdipLibrary, 'GdipResetImageAttributes');
    GdipSetImageAttributesColorMatrix := GetProcAddress(GdipLibrary, 'GdipSetImageAttributesColorMatrix');
    GdipSetImageAttributesThreshold := GetProcAddress(GdipLibrary, 'GdipSetImageAttributesThreshold');
    GdipSetImageAttributesGamma := GetProcAddress(GdipLibrary, 'GdipSetImageAttributesGamma');
    GdipSetImageAttributesNoOp := GetProcAddress(GdipLibrary, 'GdipSetImageAttributesNoOp');
    GdipSetImageAttributesColorKeys := GetProcAddress(GdipLibrary, 'GdipSetImageAttributesColorKeys');
    GdipSetImageAttributesOutputChannel := GetProcAddress(GdipLibrary, 'GdipSetImageAttributesOutputChannel');
    GdipSetImageAttributesOutputChannelColorProfile := GetProcAddress(GdipLibrary, 'GdipSetImageAttributesOutputChannelColorProfile');
    GdipSetImageAttributesRemapTable := GetProcAddress(GdipLibrary, 'GdipSetImageAttributesRemapTable');
    GdipSetImageAttributesWrapMode := GetProcAddress(GdipLibrary, 'GdipSetImageAttributesWrapMode');
    GdipSetImageAttributesICMMode := GetProcAddress(GdipLibrary, 'GdipSetImageAttributesICMMode');
    GdipGetImageAttributesAdjustedPalette := GetProcAddress(GdipLibrary, 'GdipGetImageAttributesAdjustedPalette');
    GdipFlush := GetProcAddress(GdipLibrary, 'GdipFlush');
    GdipCreateFromHDC := GetProcAddress(GdipLibrary, 'GdipCreateFromHDC');
    GdipCreateFromHDC2 := GetProcAddress(GdipLibrary, 'GdipCreateFromHDC2');
    GdipCreateFromHWND := GetProcAddress(GdipLibrary, 'GdipCreateFromHWND');
    GdipCreateFromHWNDICM := GetProcAddress(GdipLibrary, 'GdipCreateFromHWNDICM');
    GdipDeleteGraphics := GetProcAddress(GdipLibrary, 'GdipDeleteGraphics');
    GdipGetDC := GetProcAddress(GdipLibrary, 'GdipGetDC');
    GdipReleaseDC := GetProcAddress(GdipLibrary, 'GdipReleaseDC');
    GdipSetCompositingMode := GetProcAddress(GdipLibrary, 'GdipSetCompositingMode');
    GdipGetCompositingMode := GetProcAddress(GdipLibrary, 'GdipGetCompositingMode');
    GdipSetRenderingOrigin := GetProcAddress(GdipLibrary, 'GdipSetRenderingOrigin');
    GdipGetRenderingOrigin := GetProcAddress(GdipLibrary, 'GdipGetRenderingOrigin');
    GdipSetCompositingQuality := GetProcAddress(GdipLibrary, 'GdipSetCompositingQuality');
    GdipGetCompositingQuality := GetProcAddress(GdipLibrary, 'GdipGetCompositingQuality');
    GdipSetSmoothingMode := GetProcAddress(GdipLibrary, 'GdipSetSmoothingMode');
    GdipGetSmoothingMode := GetProcAddress(GdipLibrary, 'GdipGetSmoothingMode');
    GdipSetPixelOffsetMode := GetProcAddress(GdipLibrary, 'GdipSetPixelOffsetMode');
    GdipGetPixelOffsetMode := GetProcAddress(GdipLibrary, 'GdipGetPixelOffsetMode');
    GdipSetTextRenderingHint := GetProcAddress(GdipLibrary, 'GdipSetTextRenderingHint');
    GdipGetTextRenderingHint := GetProcAddress(GdipLibrary, 'GdipGetTextRenderingHint');
    GdipSetTextContrast := GetProcAddress(GdipLibrary, 'GdipSetTextContrast');
    GdipGetTextContrast := GetProcAddress(GdipLibrary, 'GdipGetTextContrast');
    GdipSetInterpolationMode := GetProcAddress(GdipLibrary, 'GdipSetInterpolationMode');
    GdipGetInterpolationMode := GetProcAddress(GdipLibrary, 'GdipGetInterpolationMode');
    GdipSetWorldTransform := GetProcAddress(GdipLibrary, 'GdipSetWorldTransform');
    GdipResetWorldTransform := GetProcAddress(GdipLibrary, 'GdipResetWorldTransform');
    GdipMultiplyWorldTransform := GetProcAddress(GdipLibrary, 'GdipMultiplyWorldTransform');
    GdipTranslateWorldTransform := GetProcAddress(GdipLibrary, 'GdipTranslateWorldTransform');
    GdipScaleWorldTransform := GetProcAddress(GdipLibrary, 'GdipScaleWorldTransform');
    GdipRotateWorldTransform := GetProcAddress(GdipLibrary, 'GdipRotateWorldTransform');
    GdipGetWorldTransform := GetProcAddress(GdipLibrary, 'GdipGetWorldTransform');
    GdipResetPageTransform := GetProcAddress(GdipLibrary, 'GdipResetPageTransform');
    GdipGetPageUnit := GetProcAddress(GdipLibrary, 'GdipGetPageUnit');
    GdipGetPageScale := GetProcAddress(GdipLibrary, 'GdipGetPageScale');
    GdipSetPageUnit := GetProcAddress(GdipLibrary, 'GdipSetPageUnit');
    GdipSetPageScale := GetProcAddress(GdipLibrary, 'GdipSetPageScale');
    GdipGetDpiX := GetProcAddress(GdipLibrary, 'GdipGetDpiX');
    GdipGetDpiY := GetProcAddress(GdipLibrary, 'GdipGetDpiY');
    GdipTransformPoints := GetProcAddress(GdipLibrary, 'GdipTransformPoints');
    GdipTransformPointsI := GetProcAddress(GdipLibrary, 'GdipTransformPointsI');
    GdipGetNearestColor := GetProcAddress(GdipLibrary, 'GdipGetNearestColor');
    GdipCreateHalftonePalette := GetProcAddress(GdipLibrary, 'GdipCreateHalftonePalette');
    GdipDrawLine := GetProcAddress(GdipLibrary, 'GdipDrawLine');
    GdipDrawLineI := GetProcAddress(GdipLibrary, 'GdipDrawLineI');
    GdipDrawLines := GetProcAddress(GdipLibrary, 'GdipDrawLines');
    GdipDrawLinesI := GetProcAddress(GdipLibrary, 'GdipDrawLinesI');
    GdipDrawArc := GetProcAddress(GdipLibrary, 'GdipDrawArc');
    GdipDrawArcI := GetProcAddress(GdipLibrary, 'GdipDrawArcI');
    GdipDrawBezier := GetProcAddress(GdipLibrary, 'GdipDrawBezier');
    GdipDrawBezierI := GetProcAddress(GdipLibrary, 'GdipDrawBezierI');
    GdipDrawBeziers := GetProcAddress(GdipLibrary, 'GdipDrawBeziers');
    GdipDrawBeziersI := GetProcAddress(GdipLibrary, 'GdipDrawBeziersI');
    GdipDrawRectangle := GetProcAddress(GdipLibrary, 'GdipDrawRectangle');
    GdipDrawRectangleI := GetProcAddress(GdipLibrary, 'GdipDrawRectangleI');
    GdipDrawRectangles := GetProcAddress(GdipLibrary, 'GdipDrawRectangles');
    GdipDrawRectanglesI := GetProcAddress(GdipLibrary, 'GdipDrawRectanglesI');
    GdipDrawEllipse := GetProcAddress(GdipLibrary, 'GdipDrawEllipse');
    GdipDrawEllipseI := GetProcAddress(GdipLibrary, 'GdipDrawEllipseI');
    GdipDrawPie := GetProcAddress(GdipLibrary, 'GdipDrawPie');
    GdipDrawPieI := GetProcAddress(GdipLibrary, 'GdipDrawPieI');
    GdipDrawPolygon := GetProcAddress(GdipLibrary, 'GdipDrawPolygon');
    GdipDrawPolygonI := GetProcAddress(GdipLibrary, 'GdipDrawPolygonI');
    GdipDrawPath := GetProcAddress(GdipLibrary, 'GdipDrawPath');
    GdipDrawCurve := GetProcAddress(GdipLibrary, 'GdipDrawCurve');
    GdipDrawCurveI := GetProcAddress(GdipLibrary, 'GdipDrawCurveI');
    GdipDrawCurve2 := GetProcAddress(GdipLibrary, 'GdipDrawCurve2');
    GdipDrawCurve2I := GetProcAddress(GdipLibrary, 'GdipDrawCurve2I');
    GdipDrawCurve3 := GetProcAddress(GdipLibrary, 'GdipDrawCurve3');
    GdipDrawCurve3I := GetProcAddress(GdipLibrary, 'GdipDrawCurve3I');
    GdipDrawClosedCurve := GetProcAddress(GdipLibrary, 'GdipDrawClosedCurve');
    GdipDrawClosedCurveI := GetProcAddress(GdipLibrary, 'GdipDrawClosedCurveI');
    GdipDrawClosedCurve2 := GetProcAddress(GdipLibrary, 'GdipDrawClosedCurve2');
    GdipDrawClosedCurve2I := GetProcAddress(GdipLibrary, 'GdipDrawClosedCurve2I');
    GdipGraphicsClear := GetProcAddress(GdipLibrary, 'GdipGraphicsClear');
    GdipFillRectangle := GetProcAddress(GdipLibrary, 'GdipFillRectangle');
    GdipFillRectangleI := GetProcAddress(GdipLibrary, 'GdipFillRectangleI');
    GdipFillRectangles := GetProcAddress(GdipLibrary, 'GdipFillRectangles');
    GdipFillRectanglesI := GetProcAddress(GdipLibrary, 'GdipFillRectanglesI');
    GdipFillPolygon := GetProcAddress(GdipLibrary, 'GdipFillPolygon');
    GdipFillPolygonI := GetProcAddress(GdipLibrary, 'GdipFillPolygonI');
    GdipFillPolygon2 := GetProcAddress(GdipLibrary, 'GdipFillPolygon2');
    GdipFillPolygon2I := GetProcAddress(GdipLibrary, 'GdipFillPolygon2I');
    GdipFillEllipse := GetProcAddress(GdipLibrary, 'GdipFillEllipse');
    GdipFillEllipseI := GetProcAddress(GdipLibrary, 'GdipFillEllipseI');
    GdipFillPie := GetProcAddress(GdipLibrary, 'GdipFillPie');
    GdipFillPieI := GetProcAddress(GdipLibrary, 'GdipFillPieI');
    GdipFillPath := GetProcAddress(GdipLibrary, 'GdipFillPath');
    GdipFillClosedCurve := GetProcAddress(GdipLibrary, 'GdipFillClosedCurve');
    GdipFillClosedCurveI := GetProcAddress(GdipLibrary, 'GdipFillClosedCurveI');
    GdipFillClosedCurve2 := GetProcAddress(GdipLibrary, 'GdipFillClosedCurve2');
    GdipFillClosedCurve2I := GetProcAddress(GdipLibrary, 'GdipFillClosedCurve2I');
    GdipFillRegion := GetProcAddress(GdipLibrary, 'GdipFillRegion');
    GdipDrawImage := GetProcAddress(GdipLibrary, 'GdipDrawImage');
    GdipDrawImageI := GetProcAddress(GdipLibrary, 'GdipDrawImageI');
    GdipDrawImageRect := GetProcAddress(GdipLibrary, 'GdipDrawImageRect');
    GdipDrawImageRectI := GetProcAddress(GdipLibrary, 'GdipDrawImageRectI');
    GdipDrawImagePoints := GetProcAddress(GdipLibrary, 'GdipDrawImagePoints');
    GdipDrawImagePointsI := GetProcAddress(GdipLibrary, 'GdipDrawImagePointsI');
    GdipDrawImagePointRect := GetProcAddress(GdipLibrary, 'GdipDrawImagePointRect');
    GdipDrawImagePointRectI := GetProcAddress(GdipLibrary, 'GdipDrawImagePointRectI');
    GdipDrawImageRectRect := GetProcAddress(GdipLibrary, 'GdipDrawImageRectRect');
    GdipDrawImageRectRectI := GetProcAddress(GdipLibrary, 'GdipDrawImageRectRectI');
    GdipDrawImagePointsRect := GetProcAddress(GdipLibrary, 'GdipDrawImagePointsRect');
    GdipDrawImagePointsRectI := GetProcAddress(GdipLibrary, 'GdipDrawImagePointsRectI');
    GdipEnumerateMetafileDestPoint := GetProcAddress(GdipLibrary, 'GdipEnumerateMetafileDestPoint');
    GdipEnumerateMetafileDestPointI := GetProcAddress(GdipLibrary, 'GdipEnumerateMetafileDestPointI');
    GdipEnumerateMetafileDestRect := GetProcAddress(GdipLibrary, 'GdipEnumerateMetafileDestRect');
    GdipEnumerateMetafileDestRectI := GetProcAddress(GdipLibrary, 'GdipEnumerateMetafileDestRectI');
    GdipEnumerateMetafileDestPoints := GetProcAddress(GdipLibrary, 'GdipEnumerateMetafileDestPoints');
    GdipEnumerateMetafileDestPointsI := GetProcAddress(GdipLibrary, 'GdipEnumerateMetafileDestPointsI');
    GdipEnumerateMetafileSrcRectDestPoint := GetProcAddress(GdipLibrary, 'GdipEnumerateMetafileSrcRectDestPoint');
    GdipEnumerateMetafileSrcRectDestPointI := GetProcAddress(GdipLibrary, 'GdipEnumerateMetafileSrcRectDestPointI');
    GdipEnumerateMetafileSrcRectDestRect := GetProcAddress(GdipLibrary, 'GdipEnumerateMetafileSrcRectDestRect');
    GdipEnumerateMetafileSrcRectDestRectI := GetProcAddress(GdipLibrary, 'GdipEnumerateMetafileSrcRectDestRectI');
    GdipEnumerateMetafileSrcRectDestPoints := GetProcAddress(GdipLibrary, 'GdipEnumerateMetafileSrcRectDestPoints');
    GdipEnumerateMetafileSrcRectDestPointsI := GetProcAddress(GdipLibrary, 'GdipEnumerateMetafileSrcRectDestPointsI');
    GdipPlayMetafileRecord := GetProcAddress(GdipLibrary, 'GdipPlayMetafileRecord');
    GdipSetClipGraphics := GetProcAddress(GdipLibrary, 'GdipSetClipGraphics');
    GdipSetClipRect := GetProcAddress(GdipLibrary, 'GdipSetClipRect');
    GdipSetClipRectI := GetProcAddress(GdipLibrary, 'GdipSetClipRectI');
    GdipSetClipPath := GetProcAddress(GdipLibrary, 'GdipSetClipPath');
    GdipSetClipRegion := GetProcAddress(GdipLibrary, 'GdipSetClipRegion');
    GdipSetClipHrgn := GetProcAddress(GdipLibrary, 'GdipSetClipHrgn');
    GdipResetClip := GetProcAddress(GdipLibrary, 'GdipResetClip');
    GdipTranslateClip := GetProcAddress(GdipLibrary, 'GdipTranslateClip');
    GdipTranslateClipI := GetProcAddress(GdipLibrary, 'GdipTranslateClipI');
    GdipGetClip := GetProcAddress(GdipLibrary, 'GdipGetClip');
    GdipGetClipBounds := GetProcAddress(GdipLibrary, 'GdipGetClipBounds');
    GdipGetClipBoundsI := GetProcAddress(GdipLibrary, 'GdipGetClipBoundsI');
    GdipIsClipEmpty := GetProcAddress(GdipLibrary, 'GdipIsClipEmpty');
    GdipGetVisibleClipBounds := GetProcAddress(GdipLibrary, 'GdipGetVisibleClipBounds');
    GdipGetVisibleClipBoundsI := GetProcAddress(GdipLibrary, 'GdipGetVisibleClipBoundsI');
    GdipIsVisibleClipEmpty := GetProcAddress(GdipLibrary, 'GdipIsVisibleClipEmpty');
    GdipIsVisiblePoint := GetProcAddress(GdipLibrary, 'GdipIsVisiblePoint');
    GdipIsVisiblePointI := GetProcAddress(GdipLibrary, 'GdipIsVisiblePointI');
    GdipIsVisibleRect := GetProcAddress(GdipLibrary, 'GdipIsVisibleRect');
    GdipIsVisibleRectI := GetProcAddress(GdipLibrary, 'GdipIsVisibleRectI');
    GdipSaveGraphics := GetProcAddress(GdipLibrary, 'GdipSaveGraphics');
    GdipRestoreGraphics := GetProcAddress(GdipLibrary, 'GdipRestoreGraphics');
    GdipBeginContainer := GetProcAddress(GdipLibrary, 'GdipBeginContainer');
    GdipBeginContainerI := GetProcAddress(GdipLibrary, 'GdipBeginContainerI');
    GdipBeginContainer2 := GetProcAddress(GdipLibrary, 'GdipBeginContainer2');
    GdipEndContainer := GetProcAddress(GdipLibrary, 'GdipEndContainer');
    GdipGetMetafileHeaderFromWmf := GetProcAddress(GdipLibrary, 'GdipGetMetafileHeaderFromWmf');
    GdipGetMetafileHeaderFromEmf := GetProcAddress(GdipLibrary, 'GdipGetMetafileHeaderFromEmf');
    GdipGetMetafileHeaderFromFile := GetProcAddress(GdipLibrary, 'GdipGetMetafileHeaderFromFile');
    GdipGetMetafileHeaderFromStream := GetProcAddress(GdipLibrary, 'GdipGetMetafileHeaderFromStream');
    GdipGetMetafileHeaderFromMetafile := GetProcAddress(GdipLibrary, 'GdipGetMetafileHeaderFromMetafile');
    GdipGetHemfFromMetafile := GetProcAddress(GdipLibrary, 'GdipGetHemfFromMetafile');
    GdipCreateStreamOnFile := GetProcAddress(GdipLibrary, 'GdipCreateStreamOnFile');
    GdipCreateMetafileFromWmf := GetProcAddress(GdipLibrary, 'GdipCreateMetafileFromWmf');
    GdipCreateMetafileFromEmf := GetProcAddress(GdipLibrary, 'GdipCreateMetafileFromEmf');
    GdipCreateMetafileFromFile := GetProcAddress(GdipLibrary, 'GdipCreateMetafileFromFile');
    GdipCreateMetafileFromWmfFile := GetProcAddress(GdipLibrary, 'GdipCreateMetafileFromWmfFile');
    GdipCreateMetafileFromStream := GetProcAddress(GdipLibrary, 'GdipCreateMetafileFromStream');
    GdipRecordMetafile := GetProcAddress(GdipLibrary, 'GdipRecordMetafile');
    GdipRecordMetafileI := GetProcAddress(GdipLibrary, 'GdipRecordMetafileI');
    GdipRecordMetafileFileName := GetProcAddress(GdipLibrary, 'GdipRecordMetafileFileName');
    GdipRecordMetafileFileNameI := GetProcAddress(GdipLibrary, 'GdipRecordMetafileFileNameI');
    GdipRecordMetafileStream := GetProcAddress(GdipLibrary, 'GdipRecordMetafileStream');
    GdipRecordMetafileStreamI := GetProcAddress(GdipLibrary, 'GdipRecordMetafileStreamI');
    GdipSetMetafileDownLevelRasterizationLimit := GetProcAddress(GdipLibrary, 'GdipSetMetafileDownLevelRasterizationLimit');
    GdipGetMetafileDownLevelRasterizationLimit := GetProcAddress(GdipLibrary, 'GdipGetMetafileDownLevelRasterizationLimit');
    GdipGetImageDecodersSize := GetProcAddress(GdipLibrary, 'GdipGetImageDecodersSize');
    GdipGetImageDecoders := GetProcAddress(GdipLibrary, 'GdipGetImageDecoders');
    GdipGetImageEncodersSize := GetProcAddress(GdipLibrary, 'GdipGetImageEncodersSize');
    GdipGetImageEncoders := GetProcAddress(GdipLibrary, 'GdipGetImageEncoders');
    GdipComment := GetProcAddress(GdipLibrary, 'GdipComment');
    GdipCreateFontFamilyFromName := GetProcAddress(GdipLibrary, 'GdipCreateFontFamilyFromName');
    GdipDeleteFontFamily := GetProcAddress(GdipLibrary, 'GdipDeleteFontFamily');
    GdipCloneFontFamily := GetProcAddress(GdipLibrary, 'GdipCloneFontFamily');
    GdipGetGenericFontFamilySansSerif := GetProcAddress(GdipLibrary, 'GdipGetGenericFontFamilySansSerif');
    GdipGetGenericFontFamilySerif := GetProcAddress(GdipLibrary, 'GdipGetGenericFontFamilySerif');
    GdipGetGenericFontFamilyMonospace := GetProcAddress(GdipLibrary, 'GdipGetGenericFontFamilyMonospace');
    GdipGetFamilyName := GetProcAddress(GdipLibrary, 'GdipGetFamilyName');
    GdipIsStyleAvailable := GetProcAddress(GdipLibrary, 'GdipIsStyleAvailable');
    GdipFontCollectionEnumerable := GetProcAddress(GdipLibrary, 'GdipFontCollectionEnumerable');
    GdipFontCollectionEnumerate := GetProcAddress(GdipLibrary, 'GdipFontCollectionEnumerate');
    GdipGetEmHeight := GetProcAddress(GdipLibrary, 'GdipGetEmHeight');
    GdipGetCellAscent := GetProcAddress(GdipLibrary, 'GdipGetCellAscent');
    GdipGetCellDescent := GetProcAddress(GdipLibrary, 'GdipGetCellDescent');
    GdipGetLineSpacing := GetProcAddress(GdipLibrary, 'GdipGetLineSpacing');
    GdipCreateFontFromDC := GetProcAddress(GdipLibrary, 'GdipCreateFontFromDC');
    GdipCreateFontFromLogfontA := GetProcAddress(GdipLibrary, 'GdipCreateFontFromLogfontA');
    GdipCreateFontFromLogfontW := GetProcAddress(GdipLibrary, 'GdipCreateFontFromLogfontW');
    GdipCreateFont := GetProcAddress(GdipLibrary, 'GdipCreateFont');
    GdipCloneFont := GetProcAddress(GdipLibrary, 'GdipCloneFont');
    GdipDeleteFont := GetProcAddress(GdipLibrary, 'GdipDeleteFont');
    GdipGetFamily := GetProcAddress(GdipLibrary, 'GdipGetFamily');
    GdipGetFontStyle := GetProcAddress(GdipLibrary, 'GdipGetFontStyle');
    GdipGetFontSize := GetProcAddress(GdipLibrary, 'GdipGetFontSize');
    GdipGetFontUnit := GetProcAddress(GdipLibrary, 'GdipGetFontUnit');
    GdipGetFontHeight := GetProcAddress(GdipLibrary, 'GdipGetFontHeight');
    GdipGetFontHeightGivenDPI := GetProcAddress(GdipLibrary, 'GdipGetFontHeightGivenDPI');
    {$IFNDEF FPC}
    GdipGetLogFontA := GetProcAddress(GdipLibrary, 'GdipGetLogFontA');
    {$ENDIF}
    GdipGetLogFontW := GetProcAddress(GdipLibrary, 'GdipGetLogFontW');
    GdipNewInstalledFontCollection := GetProcAddress(GdipLibrary, 'GdipNewInstalledFontCollection');
    GdipNewPrivateFontCollection := GetProcAddress(GdipLibrary, 'GdipNewPrivateFontCollection');
    GdipDeletePrivateFontCollection := GetProcAddress(GdipLibrary, 'GdipDeletePrivateFontCollection');
    GdipGetFontCollectionFamilyCount := GetProcAddress(GdipLibrary, 'GdipGetFontCollectionFamilyCount');
    GdipGetFontCollectionFamilyList := GetProcAddress(GdipLibrary, 'GdipGetFontCollectionFamilyList');
    GdipPrivateAddFontFile := GetProcAddress(GdipLibrary, 'GdipPrivateAddFontFile');
    GdipPrivateAddMemoryFont := GetProcAddress(GdipLibrary, 'GdipPrivateAddMemoryFont');
    GdipDrawString := GetProcAddress(GdipLibrary, 'GdipDrawString');
    GdipMeasureString := GetProcAddress(GdipLibrary, 'GdipMeasureString');
    GdipMeasureCharacterRanges := GetProcAddress(GdipLibrary, 'GdipMeasureCharacterRanges');
    GdipDrawDriverString := GetProcAddress(GdipLibrary, 'GdipDrawDriverString');
    GdipMeasureDriverString := GetProcAddress(GdipLibrary, 'GdipMeasureDriverString');
    GdipCreateStringFormat := GetProcAddress(GdipLibrary, 'GdipCreateStringFormat');
    GdipStringFormatGetGenericDefault := GetProcAddress(GdipLibrary, 'GdipStringFormatGetGenericDefault');
    GdipStringFormatGetGenericTypographic := GetProcAddress(GdipLibrary, 'GdipStringFormatGetGenericTypographic');
    GdipDeleteStringFormat := GetProcAddress(GdipLibrary, 'GdipDeleteStringFormat');
    GdipCloneStringFormat := GetProcAddress(GdipLibrary, 'GdipCloneStringFormat');
    GdipSetStringFormatFlags := GetProcAddress(GdipLibrary, 'GdipSetStringFormatFlags');
    GdipGetStringFormatFlags := GetProcAddress(GdipLibrary, 'GdipGetStringFormatFlags');
    GdipSetStringFormatAlign := GetProcAddress(GdipLibrary, 'GdipSetStringFormatAlign');
    GdipGetStringFormatAlign := GetProcAddress(GdipLibrary, 'GdipGetStringFormatAlign');
    GdipSetStringFormatLineAlign := GetProcAddress(GdipLibrary, 'GdipSetStringFormatLineAlign');
    GdipGetStringFormatLineAlign := GetProcAddress(GdipLibrary, 'GdipGetStringFormatLineAlign');
    GdipSetStringFormatTrimming := GetProcAddress(GdipLibrary, 'GdipSetStringFormatTrimming');
    GdipGetStringFormatTrimming := GetProcAddress(GdipLibrary, 'GdipGetStringFormatTrimming');
    GdipSetStringFormatHotkeyPrefix := GetProcAddress(GdipLibrary, 'GdipSetStringFormatHotkeyPrefix');
    GdipGetStringFormatHotkeyPrefix := GetProcAddress(GdipLibrary, 'GdipGetStringFormatHotkeyPrefix');
    GdipSetStringFormatTabStops := GetProcAddress(GdipLibrary, 'GdipSetStringFormatTabStops');
    GdipGetStringFormatTabStops := GetProcAddress(GdipLibrary, 'GdipGetStringFormatTabStops');
    GdipGetStringFormatTabStopCount := GetProcAddress(GdipLibrary, 'GdipGetStringFormatTabStopCount');
    GdipSetStringFormatDigitSubstitution := GetProcAddress(GdipLibrary, 'GdipSetStringFormatDigitSubstitution');
    GdipGetStringFormatDigitSubstitution := GetProcAddress(GdipLibrary, 'GdipGetStringFormatDigitSubstitution');
    GdipGetStringFormatMeasurableCharacterRangeCount := GetProcAddress(GdipLibrary, 'GdipGetStringFormatMeasurableCharacterRangeCount');
    GdipSetStringFormatMeasurableCharacterRanges := GetProcAddress(GdipLibrary, 'GdipSetStringFormatMeasurableCharacterRanges');
    GdipCreateCachedBitmap := GetProcAddress(GdipLibrary, 'GdipCreateCachedBitmap');
    GdipDeleteCachedBitmap := GetProcAddress(GdipLibrary, 'GdipDeleteCachedBitmap');
    GdipDrawCachedBitmap := GetProcAddress(GdipLibrary, 'GdipDrawCachedBitmap');
    GdipEmfToWmfBits := GetProcAddress(GdipLibrary, 'GdipEmfToWmfBits');
  end;
end;

procedure FreeGdiplus;
begin
  if (GdipLibrary <> 0) then
  begin
    FreeLibrary(GdipLibrary);
    GdipLibrary := 0;

    GdipAlloc := nil;
    GdipFree := nil;
    GdiplusStartup := nil;
    GdiplusShutdown := nil;

    GdipCreatePath := nil;
    GdipCreatePath2 := nil;
    GdipCreatePath2I := nil;
    GdipClonePath := nil;
    GdipDeletePath := nil;
    GdipResetPath := nil;
    GdipGetPointCount := nil;
    GdipGetPathTypes := nil;
    GdipGetPathPoints := nil;
    GdipGetPathPointsI := nil;
    GdipGetPathFillMode := nil;
    GdipSetPathFillMode := nil;
    GdipGetPathData := nil;
    GdipStartPathFigure := nil;
    GdipClosePathFigure := nil;
    GdipClosePathFigures := nil;
    GdipSetPathMarker := nil;
    GdipClearPathMarkers := nil;
    GdipReversePath := nil;
    GdipGetPathLastPoint := nil;
    GdipAddPathLine := nil;
    GdipAddPathLine2 := nil;
    GdipAddPathArc := nil;
    GdipAddPathBezier := nil;
    GdipAddPathBeziers := nil;
    GdipAddPathCurve := nil;
    GdipAddPathCurve2 := nil;
    GdipAddPathCurve3 := nil;
    GdipAddPathClosedCurve := nil;
    GdipAddPathClosedCurve2 := nil;
    GdipAddPathRectangle := nil;
    GdipAddPathRectangles := nil;
    GdipAddPathEllipse := nil;
    GdipAddPathPie := nil;
    GdipAddPathPolygon := nil;
    GdipAddPathPath := nil;
    GdipAddPathString := nil;
    GdipAddPathStringI := nil;
    GdipAddPathLineI := nil;
    GdipAddPathLine2I := nil;
    GdipAddPathArcI := nil;
    GdipAddPathBezierI := nil;
    GdipAddPathBeziersI := nil;
    GdipAddPathCurveI := nil;
    GdipAddPathCurve2I := nil;
    GdipAddPathCurve3I := nil;
    GdipAddPathClosedCurveI := nil;
    GdipAddPathClosedCurve2I := nil;
    GdipAddPathRectangleI := nil;
    GdipAddPathRectanglesI := nil;
    GdipAddPathEllipseI := nil;
    GdipAddPathPieI := nil;
    GdipAddPathPolygonI := nil;
    GdipFlattenPath := nil;
    GdipWindingModeOutline := nil;
    GdipWidenPath := nil;
    GdipWarpPath := nil;
    GdipTransformPath := nil;
    GdipGetPathWorldBounds := nil;
    GdipGetPathWorldBoundsI := nil;
    GdipIsVisiblePathPoint := nil;
    GdipIsVisiblePathPointI := nil;
    GdipIsOutlineVisiblePathPoint := nil;
    GdipIsOutlineVisiblePathPointI := nil;
    GdipCreatePathIter := nil;
    GdipDeletePathIter := nil;
    GdipPathIterNextSubpath := nil;
    GdipPathIterNextSubpathPath := nil;
    GdipPathIterNextPathType := nil;
    GdipPathIterNextMarker := nil;
    GdipPathIterNextMarkerPath := nil;
    GdipPathIterGetCount := nil;
    GdipPathIterGetSubpathCount := nil;
    GdipPathIterIsValid := nil;
    GdipPathIterHasCurve := nil;
    GdipPathIterRewind := nil;
    GdipPathIterEnumerate := nil;
    GdipPathIterCopyData := nil;
    GdipCreateMatrix := nil;
    GdipCreateMatrix2 := nil;
    GdipCreateMatrix3 := nil;
    GdipCreateMatrix3I := nil;
    GdipCloneMatrix := nil;
    GdipDeleteMatrix := nil;
    GdipSetMatrixElements := nil;
    GdipMultiplyMatrix := nil;
    GdipTranslateMatrix := nil;
    GdipScaleMatrix := nil;
    GdipRotateMatrix := nil;
    GdipShearMatrix := nil;
    GdipInvertMatrix := nil;
    GdipTransformMatrixPoints := nil;
    GdipTransformMatrixPointsI := nil;
    GdipVectorTransformMatrixPoints := nil;
    GdipVectorTransformMatrixPointsI := nil;
    GdipGetMatrixElements := nil;
    GdipIsMatrixInvertible := nil;
    GdipIsMatrixIdentity := nil;
    GdipIsMatrixEqual := nil;
    GdipCreateRegion := nil;
    GdipCreateRegionRect := nil;
    GdipCreateRegionRectI := nil;
    GdipCreateRegionPath := nil;
    GdipCreateRegionRgnData := nil;
    GdipCreateRegionHrgn := nil;
    GdipCloneRegion := nil;
    GdipDeleteRegion := nil;
    GdipSetInfinite := nil;
    GdipSetEmpty := nil;
    GdipCombineRegionRect := nil;
    GdipCombineRegionRectI := nil;
    GdipCombineRegionPath := nil;
    GdipCombineRegionRegion := nil;
    GdipTranslateRegion := nil;
    GdipTranslateRegionI := nil;
    GdipTransformRegion := nil;
    GdipGetRegionBounds := nil;
    GdipGetRegionBoundsI := nil;
    GdipGetRegionHRgn := nil;
    GdipIsEmptyRegion := nil;
    GdipIsInfiniteRegion := nil;
    GdipIsEqualRegion := nil;
    GdipGetRegionDataSize := nil;
    GdipGetRegionData := nil;
    GdipIsVisibleRegionPoint := nil;
    GdipIsVisibleRegionPointI := nil;
    GdipIsVisibleRegionRect := nil;
    GdipIsVisibleRegionRectI := nil;
    GdipGetRegionScansCount := nil;
    GdipGetRegionScans := nil;
    GdipGetRegionScansI := nil;
    GdipCloneBrush := nil;
    GdipDeleteBrush := nil;
    GdipGetBrushType := nil;
    GdipCreateHatchBrush := nil;
    GdipGetHatchStyle := nil;
    GdipGetHatchForegroundColor := nil;
    GdipGetHatchBackgroundColor := nil;
    GdipCreateTexture := nil;
    GdipCreateTexture2 := nil;
    GdipCreateTextureIA := nil;
    GdipCreateTexture2I := nil;
    GdipCreateTextureIAI := nil;
    GdipGetTextureTransform := nil;
    GdipSetTextureTransform := nil;
    GdipResetTextureTransform := nil;
    GdipMultiplyTextureTransform := nil;
    GdipTranslateTextureTransform := nil;
    GdipScaleTextureTransform := nil;
    GdipRotateTextureTransform := nil;
    GdipSetTextureWrapMode := nil;
    GdipGetTextureWrapMode := nil;
    GdipGetTextureImage := nil;
    GdipCreateSolidFill := nil;
    GdipSetSolidFillColor := nil;
    GdipGetSolidFillColor := nil;
    GdipCreateLineBrush := nil;
    GdipCreateLineBrushI := nil;
    GdipCreateLineBrushFromRect := nil;
    GdipCreateLineBrushFromRectI := nil;
    GdipCreateLineBrushFromRectWithAngle := nil;
    GdipCreateLineBrushFromRectWithAngleI := nil;
    GdipSetLineColors := nil;
    GdipGetLineColors := nil;
    GdipGetLineRect := nil;
    GdipGetLineRectI := nil;
    GdipSetLineGammaCorrection := nil;
    GdipGetLineGammaCorrection := nil;
    GdipGetLineBlendCount := nil;
    GdipGetLineBlend := nil;
    GdipSetLineBlend := nil;
    GdipGetLinePresetBlendCount := nil;
    GdipGetLinePresetBlend := nil;
    GdipSetLinePresetBlend := nil;
    GdipSetLineSigmaBlend := nil;
    GdipSetLineLinearBlend := nil;
    GdipSetLineWrapMode := nil;
    GdipGetLineWrapMode := nil;
    GdipGetLineTransform := nil;
    GdipSetLineTransform := nil;
    GdipResetLineTransform := nil;
    GdipMultiplyLineTransform := nil;
    GdipTranslateLineTransform := nil;
    GdipScaleLineTransform := nil;
    GdipRotateLineTransform := nil;
    GdipCreatePathGradient := nil;
    GdipCreatePathGradientI := nil;
    GdipCreatePathGradientFromPath := nil;
    GdipGetPathGradientCenterColor := nil;
    GdipSetPathGradientCenterColor := nil;
    GdipGetPathGradientSurroundColorsWithCount := nil;
    GdipSetPathGradientSurroundColorsWithCount := nil;
    GdipGetPathGradientPath := nil;
    GdipSetPathGradientPath := nil;
    GdipGetPathGradientCenterPoint := nil;
    GdipGetPathGradientCenterPointI := nil;
    GdipSetPathGradientCenterPoint := nil;
    GdipSetPathGradientCenterPointI := nil;
    GdipGetPathGradientRect := nil;
    GdipGetPathGradientRectI := nil;
    GdipGetPathGradientPointCount := nil;
    GdipGetPathGradientSurroundColorCount := nil;
    GdipSetPathGradientGammaCorrection := nil;
    GdipGetPathGradientGammaCorrection := nil;
    GdipGetPathGradientBlendCount := nil;
    GdipGetPathGradientBlend := nil;
    GdipSetPathGradientBlend := nil;
    GdipGetPathGradientPresetBlendCount := nil;
    GdipGetPathGradientPresetBlend := nil;
    GdipSetPathGradientPresetBlend := nil;
    GdipSetPathGradientSigmaBlend := nil;
    GdipSetPathGradientLinearBlend := nil;
    GdipGetPathGradientWrapMode := nil;
    GdipSetPathGradientWrapMode := nil;
    GdipGetPathGradientTransform := nil;
    GdipSetPathGradientTransform := nil;
    GdipResetPathGradientTransform := nil;
    GdipMultiplyPathGradientTransform := nil;
    GdipTranslatePathGradientTransform := nil;
    GdipScalePathGradientTransform := nil;
    GdipRotatePathGradientTransform := nil;
    GdipGetPathGradientFocusScales := nil;
    GdipSetPathGradientFocusScales := nil;
    GdipCreatePen1 := nil;
    GdipCreatePen2 := nil;
    GdipClonePen := nil;
    GdipDeletePen := nil;
    GdipSetPenWidth := nil;
    GdipGetPenWidth := nil;
    GdipSetPenUnit := nil;
    GdipGetPenUnit := nil;
    GdipSetPenLineCap197819 := nil;
    GdipSetPenStartCap := nil;
    GdipSetPenEndCap := nil;
    GdipSetPenDashCap197819 := nil;
    GdipGetPenStartCap := nil;
    GdipGetPenEndCap := nil;
    GdipGetPenDashCap197819 := nil;
    GdipSetPenLineJoin := nil;
    GdipGetPenLineJoin := nil;
    GdipSetPenCustomStartCap := nil;
    GdipGetPenCustomStartCap := nil;
    GdipSetPenCustomEndCap := nil;
    GdipGetPenCustomEndCap := nil;
    GdipSetPenMiterLimit := nil;
    GdipGetPenMiterLimit := nil;
    GdipSetPenMode := nil;
    GdipGetPenMode := nil;
    GdipSetPenTransform := nil;
    GdipGetPenTransform := nil;
    GdipResetPenTransform := nil;
    GdipMultiplyPenTransform := nil;
    GdipTranslatePenTransform := nil;
    GdipScalePenTransform := nil;
    GdipRotatePenTransform := nil;
    GdipSetPenColor := nil;
    GdipGetPenColor := nil;
    GdipSetPenBrushFill := nil;
    GdipGetPenBrushFill := nil;
    GdipGetPenFillType := nil;
    GdipGetPenDashStyle := nil;
    GdipSetPenDashStyle := nil;
    GdipGetPenDashOffset := nil;
    GdipSetPenDashOffset := nil;
    GdipGetPenDashCount := nil;
    GdipSetPenDashArray := nil;
    GdipGetPenDashArray := nil;
    GdipGetPenCompoundCount := nil;
    GdipSetPenCompoundArray := nil;
    GdipGetPenCompoundArray := nil;
    GdipCreateCustomLineCap := nil;
    GdipDeleteCustomLineCap := nil;
    GdipCloneCustomLineCap := nil;
    GdipGetCustomLineCapType := nil;
    GdipSetCustomLineCapStrokeCaps := nil;
    GdipGetCustomLineCapStrokeCaps := nil;
    GdipSetCustomLineCapStrokeJoin := nil;
    GdipGetCustomLineCapStrokeJoin := nil;
    GdipSetCustomLineCapBaseCap := nil;
    GdipGetCustomLineCapBaseCap := nil;
    GdipSetCustomLineCapBaseInset := nil;
    GdipGetCustomLineCapBaseInset := nil;
    GdipSetCustomLineCapWidthScale := nil;
    GdipGetCustomLineCapWidthScale := nil;
    GdipCreateAdjustableArrowCap := nil;
    GdipSetAdjustableArrowCapHeight := nil;
    GdipGetAdjustableArrowCapHeight := nil;
    GdipSetAdjustableArrowCapWidth := nil;
    GdipGetAdjustableArrowCapWidth := nil;
    GdipSetAdjustableArrowCapMiddleInset := nil;
    GdipGetAdjustableArrowCapMiddleInset := nil;
    GdipSetAdjustableArrowCapFillState := nil;
    GdipGetAdjustableArrowCapFillState := nil;
    GdipLoadImageFromStream := nil;
    GdipLoadImageFromFile := nil;
    GdipLoadImageFromStreamICM := nil;
    GdipLoadImageFromFileICM := nil;
    GdipCloneImage := nil;
    GdipDisposeImage := nil;
    GdipSaveImageToFile := nil;
    GdipSaveImageToStream := nil;
    GdipSaveAdd := nil;
    GdipSaveAddImage := nil;
    GdipGetImageGraphicsContext := nil;
    GdipGetImageBounds := nil;
    GdipGetImageDimension := nil;
    GdipGetImageType := nil;
    GdipGetImageWidth := nil;
    GdipGetImageHeight := nil;
    GdipGetImageHorizontalResolution := nil;
    GdipGetImageVerticalResolution := nil;
    GdipGetImageFlags := nil;
    GdipGetImageRawFormat := nil;
    GdipGetImagePixelFormat := nil;
    GdipGetImageThumbnail := nil;
    GdipGetEncoderParameterListSize := nil;
    GdipGetEncoderParameterList := nil;
    GdipImageGetFrameDimensionsCount := nil;
    GdipImageGetFrameDimensionsList := nil;
    GdipImageGetFrameCount := nil;
    GdipImageSelectActiveFrame := nil;
    GdipImageRotateFlip := nil;
    GdipGetImagePalette := nil;
    GdipSetImagePalette := nil;
    GdipGetImagePaletteSize := nil;
    GdipGetPropertyCount := nil;
    GdipGetPropertyIdList := nil;
    GdipGetPropertyItemSize := nil;
    GdipGetPropertyItem := nil;
    GdipGetPropertySize := nil;
    GdipGetAllPropertyItems := nil;
    GdipRemovePropertyItem := nil;
    GdipSetPropertyItem := nil;
    GdipImageForceValidation := nil;
    GdipCreateBitmapFromStream := nil;
    GdipCreateBitmapFromFile := nil;
    GdipCreateBitmapFromStreamICM := nil;
    GdipCreateBitmapFromFileICM := nil;
    GdipCreateBitmapFromScan0 := nil;
    GdipCreateBitmapFromGraphics := nil;
    GdipCreateBitmapFromGdiDib := nil;
    GdipCreateBitmapFromHBITMAP := nil;
    GdipCreateHBITMAPFromBitmap := nil;
    GdipCreateBitmapFromHICON := nil;
    GdipCreateHICONFromBitmap := nil;
    GdipCreateBitmapFromResource := nil;
    GdipCloneBitmapArea := nil;
    GdipCloneBitmapAreaI := nil;
    GdipBitmapLockBits := nil;
    GdipBitmapUnlockBits := nil;
    GdipBitmapGetPixel := nil;
    GdipBitmapSetPixel := nil;
    GdipBitmapSetResolution := nil;
    GdipCreateImageAttributes := nil;
    GdipCloneImageAttributes := nil;
    GdipDisposeImageAttributes := nil;
    GdipSetImageAttributesToIdentity := nil;
    GdipResetImageAttributes := nil;
    GdipSetImageAttributesColorMatrix := nil;
    GdipSetImageAttributesThreshold := nil;
    GdipSetImageAttributesGamma := nil;
    GdipSetImageAttributesNoOp := nil;
    GdipSetImageAttributesColorKeys := nil;
    GdipSetImageAttributesOutputChannel := nil;
    GdipSetImageAttributesOutputChannelColorProfile := nil;
    GdipSetImageAttributesRemapTable := nil;
    GdipSetImageAttributesWrapMode := nil;
    GdipSetImageAttributesICMMode := nil;
    GdipGetImageAttributesAdjustedPalette := nil;
    GdipFlush := nil;
    GdipCreateFromHDC := nil;
    GdipCreateFromHDC2 := nil;
    GdipCreateFromHWND := nil;
    GdipCreateFromHWNDICM := nil;
    GdipDeleteGraphics := nil;
    GdipGetDC := nil;
    GdipReleaseDC := nil;
    GdipSetCompositingMode := nil;
    GdipGetCompositingMode := nil;
    GdipSetRenderingOrigin := nil;
    GdipGetRenderingOrigin := nil;
    GdipSetCompositingQuality := nil;
    GdipGetCompositingQuality := nil;
    GdipSetSmoothingMode := nil;
    GdipGetSmoothingMode := nil;
    GdipSetPixelOffsetMode := nil;
    GdipGetPixelOffsetMode := nil;
    GdipSetTextRenderingHint := nil;
    GdipGetTextRenderingHint := nil;
    GdipSetTextContrast := nil;
    GdipGetTextContrast := nil;
    GdipSetInterpolationMode := nil;
    GdipGetInterpolationMode := nil;
    GdipSetWorldTransform := nil;
    GdipResetWorldTransform := nil;
    GdipMultiplyWorldTransform := nil;
    GdipTranslateWorldTransform := nil;
    GdipScaleWorldTransform := nil;
    GdipRotateWorldTransform := nil;
    GdipGetWorldTransform := nil;
    GdipResetPageTransform := nil;
    GdipGetPageUnit := nil;
    GdipGetPageScale := nil;
    GdipSetPageUnit := nil;
    GdipSetPageScale := nil;
    GdipGetDpiX := nil;
    GdipGetDpiY := nil;
    GdipTransformPoints := nil;
    GdipTransformPointsI := nil;
    GdipGetNearestColor := nil;
    GdipCreateHalftonePalette := nil;
    GdipDrawLine := nil;
    GdipDrawLineI := nil;
    GdipDrawLines := nil;
    GdipDrawLinesI := nil;
    GdipDrawArc := nil;
    GdipDrawArcI := nil;
    GdipDrawBezier := nil;
    GdipDrawBezierI := nil;
    GdipDrawBeziers := nil;
    GdipDrawBeziersI := nil;
    GdipDrawRectangle := nil;
    GdipDrawRectangleI := nil;
    GdipDrawRectangles := nil;
    GdipDrawRectanglesI := nil;
    GdipDrawEllipse := nil;
    GdipDrawEllipseI := nil;
    GdipDrawPie := nil;
    GdipDrawPieI := nil;
    GdipDrawPolygon := nil;
    GdipDrawPolygonI := nil;
    GdipDrawPath := nil;
    GdipDrawCurve := nil;
    GdipDrawCurveI := nil;
    GdipDrawCurve2 := nil;
    GdipDrawCurve2I := nil;
    GdipDrawCurve3 := nil;
    GdipDrawCurve3I := nil;
    GdipDrawClosedCurve := nil;
    GdipDrawClosedCurveI := nil;
    GdipDrawClosedCurve2 := nil;
    GdipDrawClosedCurve2I := nil;
    GdipGraphicsClear := nil;
    GdipFillRectangle := nil;
    GdipFillRectangleI := nil;
    GdipFillRectangles := nil;
    GdipFillRectanglesI := nil;
    GdipFillPolygon := nil;
    GdipFillPolygonI := nil;
    GdipFillPolygon2 := nil;
    GdipFillPolygon2I := nil;
    GdipFillEllipse := nil;
    GdipFillEllipseI := nil;
    GdipFillPie := nil;
    GdipFillPieI := nil;
    GdipFillPath := nil;
    GdipFillClosedCurve := nil;
    GdipFillClosedCurveI := nil;
    GdipFillClosedCurve2 := nil;
    GdipFillClosedCurve2I := nil;
    GdipFillRegion := nil;
    GdipDrawImage := nil;
    GdipDrawImageI := nil;
    GdipDrawImageRect := nil;
    GdipDrawImageRectI := nil;
    GdipDrawImagePoints := nil;
    GdipDrawImagePointsI := nil;
    GdipDrawImagePointRect := nil;
    GdipDrawImagePointRectI := nil;
    GdipDrawImageRectRect := nil;
    GdipDrawImageRectRectI := nil;
    GdipDrawImagePointsRect := nil;
    GdipDrawImagePointsRectI := nil;
    GdipEnumerateMetafileDestPoint := nil;
    GdipEnumerateMetafileDestPointI := nil;
    GdipEnumerateMetafileDestRect := nil;
    GdipEnumerateMetafileDestRectI := nil;
    GdipEnumerateMetafileDestPoints := nil;
    GdipEnumerateMetafileDestPointsI := nil;
    GdipEnumerateMetafileSrcRectDestPoint := nil;
    GdipEnumerateMetafileSrcRectDestPointI := nil;
    GdipEnumerateMetafileSrcRectDestRect := nil;
    GdipEnumerateMetafileSrcRectDestRectI := nil;
    GdipEnumerateMetafileSrcRectDestPoints := nil;
    GdipEnumerateMetafileSrcRectDestPointsI := nil;
    GdipPlayMetafileRecord := nil;
    GdipSetClipGraphics := nil;
    GdipSetClipRect := nil;
    GdipSetClipRectI := nil;
    GdipSetClipPath := nil;
    GdipSetClipRegion := nil;
    GdipSetClipHrgn := nil;
    GdipResetClip := nil;
    GdipTranslateClip := nil;
    GdipTranslateClipI := nil;
    GdipGetClip := nil;
    GdipGetClipBounds := nil;
    GdipGetClipBoundsI := nil;
    GdipIsClipEmpty := nil;
    GdipGetVisibleClipBounds := nil;
    GdipGetVisibleClipBoundsI := nil;
    GdipIsVisibleClipEmpty := nil;
    GdipIsVisiblePoint := nil;
    GdipIsVisiblePointI := nil;
    GdipIsVisibleRect := nil;
    GdipIsVisibleRectI := nil;
    GdipSaveGraphics := nil;
    GdipRestoreGraphics := nil;
    GdipBeginContainer := nil;
    GdipBeginContainerI := nil;
    GdipBeginContainer2 := nil;
    GdipEndContainer := nil;
    GdipGetMetafileHeaderFromWmf := nil;
    GdipGetMetafileHeaderFromEmf := nil;
    GdipGetMetafileHeaderFromFile := nil;
    GdipGetMetafileHeaderFromStream := nil;
    GdipGetMetafileHeaderFromMetafile := nil;
    GdipGetHemfFromMetafile := nil;
    GdipCreateStreamOnFile := nil;
    GdipCreateMetafileFromWmf := nil;
    GdipCreateMetafileFromEmf := nil;
    GdipCreateMetafileFromFile := nil;
    GdipCreateMetafileFromWmfFile := nil;
    GdipCreateMetafileFromStream := nil;
    GdipRecordMetafile := nil;
    GdipRecordMetafileI := nil;
    GdipRecordMetafileFileName := nil;
    GdipRecordMetafileFileNameI := nil;
    GdipRecordMetafileStream := nil;
    GdipRecordMetafileStreamI := nil;
    GdipSetMetafileDownLevelRasterizationLimit := nil;
    GdipGetMetafileDownLevelRasterizationLimit := nil;
    GdipGetImageDecodersSize := nil;
    GdipGetImageDecoders := nil;
    GdipGetImageEncodersSize := nil;
    GdipGetImageEncoders := nil;
    GdipComment := nil;
    GdipCreateFontFamilyFromName := nil;
    GdipDeleteFontFamily := nil;
    GdipCloneFontFamily := nil;
    GdipGetGenericFontFamilySansSerif := nil;
    GdipGetGenericFontFamilySerif := nil;
    GdipGetGenericFontFamilyMonospace := nil;
    GdipGetFamilyName := nil;
    GdipIsStyleAvailable := nil;
    GdipFontCollectionEnumerable := nil;
    GdipFontCollectionEnumerate := nil;
    GdipGetEmHeight := nil;
    GdipGetCellAscent := nil;
    GdipGetCellDescent := nil;
    GdipGetLineSpacing := nil;
    GdipCreateFontFromDC := nil;
    GdipCreateFontFromLogfontA := nil;
    GdipCreateFontFromLogfontW := nil;
    GdipCreateFont := nil;
    GdipCloneFont := nil;
    GdipDeleteFont := nil;
    GdipGetFamily := nil;
    GdipGetFontStyle := nil;
    GdipGetFontSize := nil;
    GdipGetFontUnit := nil;
    GdipGetFontHeight := nil;
    GdipGetFontHeightGivenDPI := nil;
    {$IFNDEF FPC}
    GdipGetLogFontA := nil;
    {$ENDIF}
    GdipGetLogFontW := nil;
    GdipNewInstalledFontCollection := nil;
    GdipNewPrivateFontCollection := nil;
    GdipDeletePrivateFontCollection := nil;
    GdipGetFontCollectionFamilyCount := nil;
    GdipGetFontCollectionFamilyList := nil;
    GdipPrivateAddFontFile := nil;
    GdipPrivateAddMemoryFont := nil;
    GdipDrawString := nil;
    GdipMeasureString := nil;
    GdipMeasureCharacterRanges := nil;
    GdipDrawDriverString := nil;
    GdipMeasureDriverString := nil;
    GdipCreateStringFormat := nil;
    GdipStringFormatGetGenericDefault := nil;
    GdipStringFormatGetGenericTypographic := nil;
    GdipDeleteStringFormat := nil;
    GdipCloneStringFormat := nil;
    GdipSetStringFormatFlags := nil;
    GdipGetStringFormatFlags := nil;
    GdipSetStringFormatAlign := nil;
    GdipGetStringFormatAlign := nil;
    GdipSetStringFormatLineAlign := nil;
    GdipGetStringFormatLineAlign := nil;
    GdipSetStringFormatTrimming := nil;
    GdipGetStringFormatTrimming := nil;
    GdipSetStringFormatHotkeyPrefix := nil;
    GdipGetStringFormatHotkeyPrefix := nil;
    GdipSetStringFormatTabStops := nil;
    GdipGetStringFormatTabStops := nil;
    GdipGetStringFormatTabStopCount := nil;
    GdipSetStringFormatDigitSubstitution := nil;
    GdipGetStringFormatDigitSubstitution := nil;
    GdipGetStringFormatMeasurableCharacterRangeCount := nil;
    GdipSetStringFormatMeasurableCharacterRanges := nil;
    GdipCreateCachedBitmap := nil;
    GdipDeleteCachedBitmap := nil;
    GdipDrawCachedBitmap := nil;
    GdipEmfToWmfBits := nil;
  end;
end;

// -----------------------------------------------------------------------------
// TGdiplusBase class
// -----------------------------------------------------------------------------

  class function TGdiplusBase.NewInstance: TObject;
  begin
    Result := InitInstance(GdipAlloc(ULONG(instanceSize)));
  end;

  procedure TGdiplusBase.FreeInstance;
  begin
    CleanupInstance;
    GdipFree(Self);
  end;

// -----------------------------------------------------------------------------
// macros
// -----------------------------------------------------------------------------

function ObjectTypeIsValid(type_: ObjectType): BOOL;
begin
  result :=  ((type_ >= ObjectTypeMin) and (type_ <= ObjectTypeMax));
end;

function GDIP_WMF_RECORD_TO_EMFPLUS(n: integer): Integer;
begin
  result := (n or GDIP_WMF_RECORD_BASE);
end;

function GDIP_EMFPLUS_RECORD_TO_WMF(n: integer): Integer;
begin
  result := n and (not GDIP_WMF_RECORD_BASE);
end;

function GDIP_IS_WMF_RECORDTYPE(n: integer): BOOL;
begin
  result := ((n and GDIP_WMF_RECORD_BASE) <> 0);
end;


//--------------------------------------------------------------------------
// TGPPoint Util
//--------------------------------------------------------------------------

  function MakePoint(X, Y: Integer): TGPPoint; overload;
  begin
    result.X := X;
    result.Y := Y;
  end;

  function MakePoint(X, Y: Single): TGPPointF; overload;
  begin
    Result.X := X;
    result.Y := Y;
  end;

//--------------------------------------------------------------------------
// TGPSize Util
//--------------------------------------------------------------------------

  function MakeSize(Width, Height: Single): TGPSizeF; overload;
  begin
    result.Width := Width;
    result.Height := Height;
  end;

  function MakeSize(Width, Height: Integer): TGPSize; overload;
  begin
    result.Width := Width;
    result.Height := Height;
  end;

//--------------------------------------------------------------------------
// TCharacterRange Util
//--------------------------------------------------------------------------

  function MakeCharacterRange(First, Length: Integer): TCharacterRange;
  begin
    result.First  := First;
    result.Length := Length;
  end;

// -----------------------------------------------------------------------------
// RectF class
// -----------------------------------------------------------------------------

  function MakeRect(x, y, width, height: Single): TGPRectF; overload;
  begin
    Result.X      := x;
    Result.Y      := y;
    Result.Width  := width;
    Result.Height := height;
  end;

  function MakeRect(location: TGPPointF; size: TGPSizeF): TGPRectF; overload;
  begin
    Result.X      := location.X;
    Result.Y      := location.Y;
    Result.Width  := size.Width;
    Result.Height := size.Height;
  end;

// -----------------------------------------------------------------------------
// Rect class
// -----------------------------------------------------------------------------

  function MakeRect(x, y, width, height: Integer): TGPRect; overload;
  begin
    Result.X      := x;
    Result.Y      := y;
    Result.Width  := width;
    Result.Height := height;
  end;

  function MakeRect(location: TGPPoint; size: TGPSize): TGPRect; overload;
  begin
    Result.X      := location.X;
    Result.Y      := location.Y;
    Result.Width  := size.Width;
    Result.Height := size.Height;
  end;

  function MakeRect(const Rect: TRect): TGPRect; overload;
  begin
    Result.X := rect.Left;
    Result.Y := Rect.Top;
    Result.Width := Rect.Right-Rect.Left;
    Result.Height:= Rect.Bottom-Rect.Top;
  end;

// -----------------------------------------------------------------------------
// PathData class
// -----------------------------------------------------------------------------

  constructor TPathData.Create;
  begin
    Count := 0;
    Points := nil;
    Types := nil;
  end;

  destructor TPathData.destroy;
  begin
    if assigned(Points) then freemem(Points);
    if assigned(Types) then freemem(Types);
  end;


function GetPixelFormatSize(pixfmt: PixelFormat): UINT;
begin
  result := (pixfmt shr 8) and $ff;
end;

function IsIndexedPixelFormat(pixfmt: PixelFormat): BOOL;
begin
  result := (pixfmt and PixelFormatIndexed) <> 0;
end;

function IsAlphaPixelFormat(pixfmt: PixelFormat): BOOL;
begin
  result := (pixfmt and PixelFormatAlpha) <> 0;
end;

function IsExtendedPixelFormat(pixfmt: PixelFormat): BOOL;
begin
  result := (pixfmt and PixelFormatExtended) <> 0;
end;

function IsCanonicalPixelFormat(pixfmt: PixelFormat): BOOL;
begin
  result := (pixfmt and PixelFormatCanonical) <> 0;
end;

// -----------------------------------------------------------------------------
// Color class
// -----------------------------------------------------------------------------

{  constructor TGPColor.Create;
  begin
    Argb := DWORD(Black);
  end;

  // Construct an opaque Color object with
  // the specified Red, Green, Blue values.
  //
  // Color values are not premultiplied.

  constructor TGPColor.Create(r, g, b: Byte);
  begin
    Argb := MakeARGB(255, r, g, b);
  end;

  constructor TGPColor.Create(a, r, g, b: Byte);
  begin
    Argb := MakeARGB(a, r, g, b);
  end;

  constructor TGPColor.Create(Value: ARGB);
  begin
    Argb := Value;
  end;

  function TGPColor.GetAlpha: BYTE;
  begin
    result := BYTE(Argb shr AlphaShift);
  end;

  function TGPColor.GetA: BYTE;
  begin
    result := GetAlpha;
  end;

  function TGPColor.GetRed: BYTE;
  begin
    result := BYTE(Argb shr RedShift);
  end;

  function TGPColor.GetR: BYTE;
  begin
    result := GetRed;
  end;

  function TGPColor.GetGreen: Byte;
  begin
    result := BYTE(Argb shr GreenShift);
  end;

  function TGPColor.GetG: Byte;
  begin
    result := GetGreen;
  end;

  function TGPColor.GetBlue: Byte;
  begin
    result := BYTE(Argb shr BlueShift);
  end;

  function TGPColor.GetB: Byte;
  begin
    result := GetBlue;
  end;

  function TGPColor.GetValue: ARGB;
  begin
    result := Argb;
  end;

  procedure TGPColor.SetValue(Value: ARGB);
  begin
    Argb := Value;
  end;

  procedure TGPColor.SetFromCOLORREF(rgb: COLORREF);
  begin
    Argb := MakeARGB(255, GetRValue(rgb), GetGValue(rgb), GetBValue(rgb));
  end;

  function TGPColor.ToCOLORREF: COLORREF;
  begin
    result := RGB(GetRed, GetGreen, GetBlue);
  end;

  function TGPColor.MakeARGB(a, r, g, b: Byte): ARGB;
  begin
    result := ((DWORD(b) shl  BlueShift) or
               (DWORD(g) shl GreenShift) or
               (DWORD(r) shl   RedShift) or
               (DWORD(a) shl AlphaShift));
  end;  }

  function MakeColor(a, r, g, b: Byte): ARGB; overload;
  begin
    result := ((DWORD(b) shl  BlueShift) or
               (DWORD(g) shl GreenShift) or
               (DWORD(r) shl   RedShift) or
               (DWORD(a) shl AlphaShift));
  end;

  function MakeColor(r, g, b: Byte): ARGB; overload;
  begin
    result := MakeColor(255, r, g, b);
  end;

  function GetAlpha(color: ARGB): BYTE;
  begin
    result := BYTE(color shr AlphaShift);
  end;

  function GetRed(color: ARGB): BYTE;
  begin
    result := BYTE(color shr RedShift);
  end;

  function GetGreen(color: ARGB): BYTE;
  begin
    result := BYTE(color shr GreenShift);
  end;

  function GetBlue(color: ARGB): BYTE;
  begin
    result := BYTE(color shr BlueShift);
  end;

  function ColorRefToARGB(rgb: COLORREF): ARGB;
  begin
    result := MakeColor(255, GetRValue(rgb), GetGValue(rgb), GetBValue(rgb));
  end;

  function ARGBToColorRef(Color: ARGB): COLORREF;
  begin
    result := RGB(GetRed(Color), GetGreen(Color), GetBlue(Color));
  end;


// -----------------------------------------------------------------------------
// MetafileHeader class
// -----------------------------------------------------------------------------

  procedure TMetafileHeader.GetBounds(out Rect: TGPRect);
  begin
    rect.X      := X;
    rect.Y      := Y;
    rect.Width  := Width;
    rect.Height := Height;
  end;

  function TMetafileHeader.IsWmf: BOOL;
  begin
    result :=  ((Type_ = MetafileTypeWmf) or (Type_ = MetafileTypeWmfPlaceable));
  end;

  function TMetafileHeader.IsWmfPlaceable: BOOL;
  begin
    result := (Type_ = MetafileTypeWmfPlaceable);
  end;

  function TMetafileHeader.IsEmf: BOOL;
  begin
    result := (Type_ = MetafileTypeEmf);
  end;

  function TMetafileHeader.IsEmfOrEmfPlus: BOOL;
  begin
    result := (Type_ >= MetafileTypeEmf);
  end;

  function TMetafileHeader.IsEmfPlus: BOOL;
  begin
    result := (Type_ >= MetafileTypeEmfPlusOnly)
  end;

  function TMetafileHeader.IsEmfPlusDual: BOOL;
  begin
    result := (Type_ = MetafileTypeEmfPlusDual)
  end;

  function TMetafileHeader.IsEmfPlusOnly: BOOL;
  begin
    result := (Type_ = MetafileTypeEmfPlusOnly)
  end;

  function TMetafileHeader.IsDisplay: BOOL;
  begin
    result := (IsEmfPlus and ((EmfPlusFlags and GDIP_EMFPLUSFLAGS_DISPLAY) <> 0));
  end;

  function TMetafileHeader.GetWmfHeader: PMetaHeader;
  begin
    if IsWmf then result :=  @WmfHeader
             else result := nil;
  end;

  function TMetafileHeader.GetEmfHeader: PENHMETAHEADER3;
  begin
    if IsEmfOrEmfPlus then result := @EmfHeader
                      else result := nil;
  end;


//---------------------------------------------------------------------------
// GDI+ classes for forward reference
//---------------------------------------------------------------------------

type
  TGPGraphics = class;
  TGPPen = class;
  TGPBrush = class;
  TGPMatrix = class;
  TGPBitmap = class;
  TGPMetafile = class;
  TGPFontFamily = class;
  TGPGraphicsPath = class;
  TGPRegion = class;
  TGPImage = class;
  TGPHatchBrush = class;
  TGPSolidBrush = class;
  TGPLinearGradientBrush = class;
  TGPPathGradientBrush = class;
  TGPFont = class;
  TGPFontCollection = class;
  TGPInstalledFontCollection = class;
  TGPPrivateFontCollection = class;
  TGPImageAttributes = class;
  TGPCachedBitmap = class;

  TGPRegion = class(TGdiplusBase)
  protected
    nativeRegion: GpRegion;
    lastResult: TStatus;
    function SetStatus(status: TStatus): TStatus;
    procedure SetNativeRegion(nativeRegion: GpRegion);
    constructor Create(nativeRegion: GpRegion); reintroduce; overload;
  public
    constructor Create; reintroduce; overload;
    constructor Create(rect: TGPRectF); reintroduce; overload;
    constructor Create(rect: TGPRect); reintroduce; overload;
    constructor Create(path: TGPGraphicsPath); reintroduce; overload;
    constructor Create(regionData: PBYTE; size: Integer); reintroduce; overload;
    {$IFNDEF KS_CBUILDER}
    constructor Create(hRgn: HRGN); reintroduce; overload;
    {$ENDIF}
    function FromHRGN(hRgn: HRGN): TGPRegion;
    destructor Destroy; override;
    function Clone: TGPRegion;
    function MakeInfinite: TStatus;
    function MakeEmpty: TStatus;
    function GetDataSize: UINT;
    // buffer     - where to put the data
    // bufferSize - how big the buffer is (should be at least as big as GetDataSize())
    // sizeFilled - if not NULL, this is an OUT param that says how many bytes
    //              of data were written to the buffer.
    function GetData(buffer: PBYTE; bufferSize: UINT;
       sizeFilled: PUINT = nil): TStatus;
    function Intersect(const rect: TGPRect): TStatus; overload;
    function Intersect(const rect: TGPRectF): TStatus; overload;
    function Intersect(path: TGPGraphicsPath): TStatus; overload;
    function Intersect(region: TGPRegion): TStatus; overload;
    function Union(const rect: TGPRect): TStatus; overload;
    function Union(const rect: TGPRectF): TStatus; overload;
    function Union(path: TGPGraphicsPath): TStatus; overload;
    function Union(region: TGPRegion): TStatus; overload;
    function Xor_(const rect: TGPRect): TStatus; overload;
    function Xor_(const rect: TGPRectF): TStatus; overload;
    function Xor_(path: TGPGraphicsPath): TStatus; overload;
    function Xor_(region: TGPRegion): TStatus; overload;
    function Exclude(const rect: TGPRect): TStatus; overload;
    function Exclude(const rect: TGPRectF): TStatus; overload;
    function Exclude(path: TGPGraphicsPath): TStatus; overload;
    function Exclude(region: TGPRegion): TStatus; overload;
    function Complement(const rect: TGPRect): TStatus; overload;
    function Complement(const rect: TGPRectF): TStatus; overload;
    function Complement(path: TGPGraphicsPath): TStatus; overload;
    function Complement(region: TGPRegion): TStatus; overload;
    function Translate(dx, dy: Single): TStatus; overload;
    function Translate(dx, dy: Integer): TStatus; overload;
    function Transform(matrix: TGPMatrix): TStatus;
    function GetBounds(out rect: TGPRect; g: TGPGraphics): TStatus; overload;
    function GetBounds(out rect: TGPRectF; g: TGPGraphics): TStatus; overload;
    function GetHRGN(g: TGPGraphics): HRGN;
    function IsEmpty(g: TGPGraphics): BOOL;
    function IsInfinite(g: TGPGraphics): BOOL ;
    function IsVisible(x, y: Integer; g: TGPGraphics = nil): BOOL; overload;
    function IsVisible(const point: TGPPoint; g: TGPGraphics = nil): BOOL; overload;
    function IsVisible(x, y: Single; g: TGPGraphics = nil): BOOL; overload;
    function IsVisible(const point: TGPPointF; g: TGPGraphics = nil): BOOL; overload;
    function IsVisible(x, y, width, height: Integer; g: TGPGraphics): BOOL; overload;
    function IsVisible(const rect: TGPRect; g: TGPGraphics = nil): BOOL; overload;
    function IsVisible(x, y, width, height: Single; g: TGPGraphics = nil): BOOL; overload;
    function IsVisible(const rect: TGPRectF; g: TGPGraphics = nil): BOOL; overload;
    function Equals(region: TGPRegion; g: TGPGraphics): BOOL;
    function GetRegionScansCount(matrix: TGPMatrix): UINT;
    function GetRegionScans(matrix: TGPMatrix ;rects: PGPRectF; out count: Integer): TStatus; overload;
    function GetRegionScans(matrix: TGPMatrix; rects: PGPRect; out count: Integer): TStatus; overload;
    function GetLastStatus: TStatus;
  end;

//--------------------------------------------------------------------------
// FontFamily
//--------------------------------------------------------------------------

  TGPFontFamily = class(TGdiplusBase)
  protected
    nativeFamily: GpFontFamily;
    lastResult: TStatus;
    function SetStatus(status: TStatus): TStatus;
    constructor Create(nativeOrig: GpFontFamily;
      status: TStatus); reintroduce; overload;
  public
    constructor Create; reintroduce; overload;
    constructor Create(name: WideString; fontCollection: TGPFontCollection = nil); reintroduce; overload;
    destructor Destroy; override;
    class function GenericSansSerif: TGPFontFamily;
    class function GenericSerif: TGPFontFamily;
    class function GenericMonospace: TGPFontFamily;
    function GetFamilyName(out name: String; language: LANGID = 0): TStatus;
    function Clone: TGPFontFamily;
    function IsAvailable: BOOL;
    function IsStyleAvailable(style: Integer): BOOL;
    function GetEmHeight(style: Integer): UINT16;
    function GetCellAscent(style: Integer): UINT16;
    function GetCellDescent(style: Integer): UINT16;
    function GetLineSpacing(style: Integer): UINT16;
    function GetLastStatus: TStatus;
  end;

//--------------------------------------------------------------------------
// Font Collection
//--------------------------------------------------------------------------

  TGPFontCollection = class(TGdiplusBase)
  protected
    nativeFontCollection: GpFontCollection;
    lastResult: TStatus;
    function SetStatus(status: TStatus): TStatus;
  public
    constructor Create;
    destructor Destroy; override;
    function GetFamilyCount: Integer;
    function GetFamilies(numSought: Integer; out gpfamilies: array of TGPFontFamily;
      out numFound: Integer): TStatus;
    function GetLastStatus: TStatus;
  end;

  TGPInstalledFontCollection = class(TGPFontCollection)
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  end;

  TGPPrivateFontCollection = class(TGPFontCollection)
  public
    constructor Create; reintroduce;
    destructor destroy; override;
    function AddFontFile(filename: WideString): TStatus;
    function AddMemoryFont(memory: Pointer; length: Integer): TStatus;
  end;

//--------------------------------------------------------------------------
// TFont
//--------------------------------------------------------------------------

  TGPFont = class(TGdiplusBase)
  protected
    nativeFont: GpFont;
    lastResult: TStatus;
    procedure SetNativeFont(Font: GpFont);
    function SetStatus(status: TStatus): TStatus;
    constructor Create(font: GpFont; status: TStatus); overload;
  public
    constructor Create(hdc: HDC); reintroduce; overload;
    constructor Create(hdc: HDC; logfont: PLogFontA); reintroduce; overload;
    constructor Create(hdc: HDC; logfont: PLogFontW); reintroduce; overload;
    constructor Create(hdc: HDC; hfont: HFONT); reintroduce; overload;
    constructor Create(family: TGPFontFamily; emSize: Single;
      style: TFontStyle = FontStyleRegular;
      unit_: TUnit = UnitPoint); reintroduce; overload;
    constructor Create(familyName: WideString; emSize: Single;
      style: TFontStyle = FontStyleRegular; unit_: TUnit = UnitPoint;
      fontCollection: TGPFontCollection = nil); reintroduce; overload;
    function GetLogFontA(g: TGPGraphics; out logfontA: TLogFontA): TStatus;
    function GetLogFontW(g: TGPGraphics; out logfontW: TLogFontW): TStatus;
    function Clone: TGPFont;
    destructor Destroy; override;
    function IsAvailable: BOOL;
    function GetStyle: Integer;
    function GetSize: Single;
    function GetUnit: TUnit;
    function GetLastStatus: TStatus;
    function GetHeight(graphics: TGPGraphics): Single; overload;
    function GetHeight(dpi: Single): Single; overload;
    function GetFamily(family: TGPFontFamily): TStatus;
  end;

//--------------------------------------------------------------------------
// Abstract base class for Image and Metafile
//--------------------------------------------------------------------------

  TGPImage = class(TGdiplusBase)
  protected
    nativeImage: GpImage;
    lastResult: TStatus;
    loadStatus: TStatus;
    procedure SetNativeImage(nativeImage: GpImage);
    function SetStatus(status: TStatus): TStatus;
    constructor Create(nativeImage: GpImage; status: TStatus); reintroduce; overload;
  public
    constructor Create(filename: WideString; useEmbeddedColorManagement: BOOL = FALSE); reintroduce; overload;
    constructor Create(stream: IStream; useEmbeddedColorManagement: BOOL  = FALSE); reintroduce; overload;
    function FromFile(filename: WideString; useEmbeddedColorManagement: BOOL = FALSE): TGPImage;
    function FromStream(stream: IStream; useEmbeddedColorManagement: BOOL = FALSE): TGPImage;
    destructor destroy; override;
    function Clone: TGPImage;
    function Save(filename: WideString; const clsidEncoder: TGUID;
      encoderParams: PEncoderParameters = nil): TStatus; overload;
    function Save(stream: IStream; const clsidEncoder: TGUID;
      encoderParams: PEncoderParameters  = nil): TStatus; overload;
    function SaveAdd(encoderParams: PEncoderParameters): TStatus; overload;
    function SaveAdd(newImage: TGPImage; encoderParams: PEncoderParameters): TStatus; overload;
    function GetType: TImageType;
    function GetPhysicalDimension(out size: TGPSizeF): TStatus;
    function GetBounds(out srcRect: TGPRectF; out srcUnit: TUnit): TStatus;
    function GetWidth: UINT;
    function GetHeight: UINT;
    function GetHorizontalResolution: Single;
    function GetVerticalResolution: Single;
    function GetFlags: UINT;
    function GetRawFormat(out format: TGUID): TStatus;
    function GetPixelFormat: TPixelFormat;
    function GetPaletteSize: Integer;
    function GetPalette(palette: PColorPalette; size: Integer): TStatus;
    function SetPalette(palette: PColorPalette): TStatus;
    function GetThumbnailImage(thumbWidth, thumbHeight: UINT;
      callback: GetThumbnailImageAbort = nil; callbackData: pointer = nil): TGPImage;
    function GetFrameDimensionsCount: UINT;
    function GetFrameDimensionsList(dimensionIDs: PGUID; count: UINT): TStatus;
    function GetFrameCount(const dimensionID: TGUID): UINT;
    function SelectActiveFrame(const dimensionID: TGUID; frameIndex: UINT): TStatus;
    function RotateFlip(rotateFlipType: TRotateFlipType): TStatus;
    function GetPropertyCount: UINT;
    function GetPropertyIdList(numOfProperty: UINT; list: PPropID): TStatus;
    function GetPropertyItemSize(propId: PROPID): UINT;
    function GetPropertyItem(propId: PROPID; propSize: UINT; buffer: PPropertyItem): TStatus;
    function GetPropertySize(out totalBufferSize, numProperties : UINT): TStatus;
    function GetAllPropertyItems(totalBufferSize, numProperties: UINT;
      allItems: PPROPERTYITEM): TStatus;
    function RemovePropertyItem(propId: TPROPID): TStatus;
    function SetPropertyItem(const item: TPropertyItem): TStatus;
    function GetEncoderParameterListSize(const clsidEncoder: TGUID): UINT;
    function GetEncoderParameterList(const clsidEncoder: TGUID; size: UINT;
      buffer: PEncoderParameters): TStatus;
    function GetLastStatus: TStatus;
  end;

  TGPBitmap = class(TGPImage)
  protected
    constructor Create(nativeBitmap: GpBitmap);  reintroduce; overload;
  public
    constructor Create(filename: WideString; useEmbeddedColorManagement: BOOL = FALSE); reintroduce; overload;
    constructor Create(stream: IStream; useEmbeddedColorManagement: BOOL = FALSE); reintroduce; overload;
    function FromFile(filename: WideString; useEmbeddedColorManagement: BOOL = FALSE): TGPBitmap;
    function FromStream(stream: IStream; useEmbeddedColorManagement: BOOL = FALSE): TGPBitmap;
    constructor Create(width, height, stride: Integer; format: TPixelFormat; scan0: PBYTE); reintroduce; overload;
    constructor Create(width, height: Integer; format: TPixelFormat = PixelFormat32bppARGB); reintroduce; overload;
    constructor Create(width, height: Integer; target: TGPGraphics); reintroduce; overload;
    function Clone(rect: TGPRect; format: TPixelFormat): TGPBitmap; overload;
    function Clone(x, y, width, height: Integer; format: TPixelFormat): TGPBitmap; overload;
    function Clone(rect: TGPRectF; format: TPixelFormat): TGPBitmap; overload;
    function Clone(x, y, width, height: Single; format: TPixelFormat): TGPBitmap; overload;
    function LockBits(rect: TGPRect; flags: UINT; format: TPixelFormat; out lockedBitmapData: TBitmapData): TStatus; 
    function UnlockBits(var lockedBitmapData: TBitmapData): TStatus;
    function GetPixel(x, y: Integer; out color: TGPColor): TStatus;
    function SetPixel(x, y: Integer; color: TGPColor): TStatus;
    function SetResolution(xdpi, ydpi: Single): TStatus;
    constructor Create(var gdiBitmapInfo: TBITMAPINFO; gdiBitmapData: Pointer); reintroduce; overload;
    constructor Create(hbm: HBITMAP; hpal: HPALETTE); reintroduce; overload;
    {$IFNDEF KS_CBUILDER}
    constructor Create(hicon: HICON); reintroduce; overload;
    function FromHICON(hicon: HICON): TGPBitmap;
    {$ENDIF}
    constructor Create(hInstance: HMODULE; bitmapName: WideString); reintroduce; overload;
    function FromBITMAPINFO(var gdiBitmapInfo: TBITMAPINFO; gdiBitmapData: Pointer): TGPBitmap;
    function FromHBITMAP(hbm: HBITMAP; hpal: HPALETTE): TGPBitmap;
    function FromResource(hInstance: HMODULE; bitmapName: WideString): TGPBitmap;
    function GetHBITMAP(colorBackground: TGPColor; out hbmReturn: HBITMAP): TStatus;
    function GetHICON(out hicon: HICON): TStatus;
  end;

  TGPCustomLineCap = class(TGdiplusBase)
  protected
    nativeCap: GpCustomLineCap;
    lastResult: TStatus;
    procedure SetNativeCap(nativeCap: GpCustomLineCap);
    function SetStatus(status: TStatus): TStatus;
    constructor Create(nativeCap: GpCustomLineCap;
      status: TStatus); reintroduce; overload;
  public
    constructor Create; reintroduce; overload;
    constructor Create(fillPath, strokePath: TGPGraphicsPath;
      baseCap: TLineCap = LineCapFlat;
      baseInset: Single = 0); reintroduce; overload;
    destructor Destroy; override;
    function Clone: TGPCustomLineCap;
    function SetStrokeCap(strokeCap: TLineCap): TStatus;
    function SetStrokeCaps(startCap, endCap: TLineCap): TStatus;
    function GetStrokeCaps(out startCap, endCap: TLineCap): TStatus;
    function SetStrokeJoin(lineJoin: TLineJoin): TStatus;
    function GetStrokeJoin: TLineJoin;
    function SetBaseCap(baseCap: TLineCap): TStatus;
    function GetBaseCap: TLineCap;
    function SetBaseInset(inset: Single): TStatus;
    function GetBaseInset: Single;
    function SetWidthScale(widthScale: Single): TStatus;
    function GetWidthScale: Single;                                
    function GetLastStatus: TStatus;
  end;

  TGPCachedBitmap = class(TGdiplusBase)
  protected
    nativeCachedBitmap: GpCachedBitmap;
    lastResult: TStatus;
  public
    constructor Create(bitmap: TGPBitmap; graphics: TGPGraphics); reintroduce;
    destructor Destroy; override;
    function GetLastStatus: TStatus;
  end;

  TGPImageAttributes = class(TGdiplusBase)
  protected
    nativeImageAttr: GpImageAttributes;
    lastResult: TStatus;
    procedure SetNativeImageAttr(nativeImageAttr: GpImageAttributes);
    function SetStatus(status: TStatus): TStatus;
    constructor Create(imageAttr: GpImageAttributes;
      status: GpStatus); reintroduce; overload;
  public
    constructor Create; reintroduce; overload;
    destructor Destroy; override;
    function Clone: TGPImageAttributes;
    function SetToIdentity(type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
    function Reset(type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
    function SetColorMatrix(const colorMatrix: TColorMatrix;
      mode: TColorMatrixFlags = ColorMatrixFlagsDefault;
      type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
    function ClearColorMatrix(type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
    function SetColorMatrices(const colorMatrix: TColorMatrix; const grayMatrix: TColorMatrix;
      mode: TColorMatrixFlags  = ColorMatrixFlagsDefault;
      type_: TColorAdjustType  = ColorAdjustTypeDefault): TStatus;
    function ClearColorMatrices(Type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
    function SetThreshold(threshold: Single; type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
    function ClearThreshold(type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
    function SetGamma(gamma: Single; type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
    function ClearGamma( type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
    function SetNoOp(type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
    function ClearNoOp(Type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
    function SetColorKey(colorLow, colorHigh: TGPColor; type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
    function ClearColorKey(type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
    function SetOutputChannel(channelFlags: TColorChannelFlags; type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
    function ClearOutputChannel(type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
    function SetOutputChannelColorProfile(colorProfileFilename: WideString;
      type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
    function ClearOutputChannelColorProfile(type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
    function SetRemapTable(mapSize: Cardinal; map: PColorMap; type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
    function ClearRemapTable(type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
    function SetBrushRemapTable(mapSize: Cardinal; map: PColorMap): TStatus;
    function ClearBrushRemapTable: TStatus;
    function SetWrapMode(wrap: TWrapMode; color: TGPColor = aclBlack; clamp: BOOL = FALSE): TStatus;
    // The flags of the palette are ignored.
    function GetAdjustedPalette(colorPalette: PColorPalette; colorAdjustType: TColorAdjustType): TStatus;
    function GetLastStatus: TStatus;
  end;

  TMatrixArray = array[0..5] of Single;

  TGPMatrix = class(TGdiplusBase)
  protected
    nativeMatrix: GpMatrix;
    lastResult: GpStatus ;
    procedure SetNativeMatrix(nativeMatrix: GpMatrix);
    function SetStatus(status: GpStatus): TStatus;
    constructor Create(nativeMatrix: GpMatrix); reintroduce; overload;
  public
    // Default constructor is set to identity matrix.
    constructor Create; reintroduce; overload;
    constructor Create(m11, m12, m21, m22, dx, dy: Single); reintroduce; overload;
    constructor Create(const rect: TGPRectF; const dstplg: TGPPointF); reintroduce; overload;
    constructor Create(const rect: TGPRect; const dstplg: TGPPoint); reintroduce; overload;
    destructor Destroy; override;
    function Clone: TGPMatrix;
    function GetElements(const m: TMatrixArray): TStatus;
    function SetElements(m11, m12, m21, m22, dx, dy: Single): TStatus;
    function OffsetX: Single;
    function OffsetY: Single;
    function Reset: TStatus;
    function Multiply(matrix: TGPMatrix; order: TMatrixOrder = MatrixOrderPrepend): TStatus;                // ok
    function Translate(offsetX, offsetY: Single; order: TMatrixOrder = MatrixOrderPrepend): TStatus;      // ok
    function Scale(scaleX, scaleY: Single; order: TMatrixOrder = MatrixOrderPrepend): TStatus;            // ok
    function Rotate(angle: Single; order: TMatrixOrder = MatrixOrderPrepend): TStatus;                    // ok
    function RotateAt(angle: Single; const center: TGPPointF; order: TMatrixOrder = MatrixOrderPrepend): TStatus; // ok
    function Shear(shearX, shearY: Single; order: TMatrixOrder = MatrixOrderPrepend): TStatus;            // ok
    function Invert: TStatus;                                                                             // ok

    function TransformPoints(pts: PGPPointF; count: Integer = 1): TStatus; overload;
    function TransformPoints(pts: PGPPoint; count: Integer = 1): TStatus; overload;

    function TransformVectors(pts: PGPPointF; count: Integer = 1): TStatus; overload;
    function TransformVectors(pts: PGPPoint; count: Integer = 1): TStatus; overload;

    function IsInvertible: BOOL;
    function IsIdentity: BOOL;
    function Equals(matrix: TGPMatrix): BOOL;
    function GetLastStatus: TStatus;
  end;

  //--------------------------------------------------------------------------
  // Abstract base class for various brush types
  //--------------------------------------------------------------------------

  TGPBrush = class(TGdiplusBase)
  protected
    nativeBrush: GpBrush;
    lastResult: TStatus;
    procedure SetNativeBrush(nativeBrush: GpBrush);
    function SetStatus(status: TStatus): TStatus;
    constructor Create(nativeBrush: GpBrush; status: TStatus); overload;
  public
    constructor Create; overload;
    destructor Destroy; override;
    function Clone: TGPBrush; virtual;
    function GetType: TBrushType;
    function GetLastStatus: TStatus;
  end;

  //--------------------------------------------------------------------------
  // Solid Fill Brush Object
  //--------------------------------------------------------------------------

  TGPSolidBrush = class(TGPBrush)
  public
    constructor Create(color: TGPColor); reintroduce; overload;
    constructor Create; reintroduce; overload;
    function GetColor(out color: TGPColor): TStatus;
    function SetColor(color: TGPColor): TStatus;
  end;

  //--------------------------------------------------------------------------
  // Texture Brush Fill Object
  //--------------------------------------------------------------------------

  TGPTextureBrush = class(TGPBrush)
  public
    constructor Create(image: TGPImage; wrapMode: TWrapMode = WrapModeTile); reintroduce; overload;
    constructor Create(image: TGPImage; wrapMode: TWrapMode; dstRect: TGPRectF); reintroduce; overload;
    constructor Create(image: TGPImage; dstRect: TGPRectF; imageAttributes: TGPImageAttributes = nil); reintroduce; overload;
    constructor Create(image: TGPImage; dstRect: TGPRect; imageAttributes: TGPImageAttributes = nil); reintroduce; overload;
    constructor Create(image: TGPImage; wrapMode: TWrapMode; dstRect: TGPRect); reintroduce; overload;
    constructor Create(image: TGPImage; wrapMode: TWrapMode; dstX, dstY, dstWidth,
      dstHeight: Single); reintroduce; overload;
    constructor Create(image: TGPImage; wrapMode: TWrapMode; dstX, dstY, dstWidth,
      dstHeight: Integer); reintroduce; overload;
    constructor Create; reintroduce; overload;
    function SetTransform(matrix: TGPMatrix): TStatus;
    function GetTransform(matrix: TGPMatrix): TStatus;
    function ResetTransform: TStatus;
    function MultiplyTransform(matrix: TGPMatrix; order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    function TranslateTransform(dx, dy: Single; order: MatrixOrder = MatrixOrderPrepend): TStatus;
    function ScaleTransform(sx, sy: Single; order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    function RotateTransform(angle: Single; order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    function SetWrapMode(wrapMode: TWrapMode): TStatus;
    function GetWrapMode: TWrapMode;
    function GetImage: TGPImage;
  end;

  //--------------------------------------------------------------------------
  // Linear Gradient Brush Object
  //--------------------------------------------------------------------------

  TGPLinearGradientBrush = class(TGPBrush)
  public
    constructor Create; reintroduce; overload;
    constructor Create(const point1, point2: TGPPointF; color1,
      color2: TGPColor); reintroduce; overload;
    constructor Create(const point1, point2: TGPPoint; color1,
      color2: TGPColor); reintroduce; overload;
    constructor Create(rect: TGPRectF; color1, color2: TGPColor;
      mode: TLinearGradientMode); reintroduce; overload;
    constructor Create(rect: TGPRect; color1, color2: TGPColor;
      mode: TLinearGradientMode); reintroduce; overload;
    constructor Create(rect: TGPRectF; color1, color2: TGPColor; angle: Single;
      isAngleScalable: BOOL = FALSE); overload;
    constructor Create(rect: TGPRect; color1, color2: TGPColor; angle: Single;
      isAngleScalable: BOOL = FALSE); overload;
    function SetLinearColors(color1, color2: TGPColor): TStatus;
    function GetLinearColors(out color1, color2: TGPColor): TStatus;
    function GetRectangle(out rect: TGPRectF): TStatus; overload;
    function GetRectangle(out rect: TGPRect): TStatus; overload;
    function SetGammaCorrection(useGammaCorrection: BOOL): TStatus;
    function GetGammaCorrection: BOOL;
    function GetBlendCount: Integer;
    function SetBlend(blendFactors, blendPositions: PSingle; count: Integer): TStatus;
    function GetBlend(blendFactors, blendPositions: PSingle; count: Integer): TStatus;
    function GetInterpolationColorCount: Integer;
    function SetInterpolationColors(presetColors: PGPColor; blendPositions: PSingle; count: Integer): TStatus;
    function GetInterpolationColors(presetColors: PGPColor; blendPositions: PSingle; count: Integer): TStatus;
    function SetBlendBellShape(focus: Single; scale: Single = 1.0): TStatus;
    function SetBlendTriangularShape(focus: Single; scale: Single = 1.0): TStatus;
    function SetTransform(matrix: TGPMatrix): TStatus;
    function GetTransform(matrix: TGPMatrix): TStatus;
    function ResetTransform: TStatus;
    function MultiplyTransform(matrix: TGPMatrix; order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    function TranslateTransform(dx, dy: Single; order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    function ScaleTransform(sx, sy: Single; order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    function RotateTransform(angle: Single; order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    function SetWrapMode(wrapMode: TWrapMode): TStatus;
    function GetWrapMode: TWrapMode;
  end;

  //--------------------------------------------------------------------------
  // Hatch Brush Object
  //--------------------------------------------------------------------------

  TGPHatchBrush = class(TGPBrush)
  public
    constructor Create; reintroduce; overload;
    constructor Create(hatchStyle: THatchStyle; foreColor: TGPColor; backColor: TGPColor = aclBlack); reintroduce; overload; // ok
    function GetHatchStyle: THatchStyle;
    function GetForegroundColor(out color: TGPColor): TStatus;
    function GetBackgroundColor(out color: TGPColor): TStatus;
  end;

//--------------------------------------------------------------------------
// Pen class 
//--------------------------------------------------------------------------

  TGPPen = class(TGdiplusBase)
  protected
    nativePen: GpPen;
    lastResult: TStatus;
    procedure SetNativePen(nativePen: GpPen);
    function SetStatus(status: TStatus): TStatus;
    constructor Create(nativePen: GpPen; status: TStatus); reintroduce; overload;
  public
    constructor Create(color: TGPColor; width: Single = 1.0); reintroduce; overload;
    constructor Create(brush: TGPBrush; width: Single = 1.0); reintroduce; overload;
    destructor Destroy; override;
    function Clone: TGPPen;
    function SetWidth(width: Single): TStatus;
    function GetWidth: Single;
    // Set/get line caps: start, end, and dash
    // Line cap and join APIs by using LineCap and LineJoin enums.
    function SetLineCap(startCap, endCap: TLineCap; dashCap: TDashCap): TStatus;
    function SetStartCap(startCap: TLineCap): TStatus;
    function SetEndCap(endCap: TLineCap): TStatus;
    function SetDashCap(dashCap: TDashCap): TStatus;
    function GetStartCap: TLineCap;
    function GetEndCap: TLineCap;
    function GetDashCap: TDashCap;
    function SetLineJoin(lineJoin: TLineJoin): TStatus;
    function GetLineJoin: TLineJoin;
    function SetCustomStartCap(customCap: TGPCustomLineCap): TStatus;
    function GetCustomStartCap(customCap: TGPCustomLineCap): TStatus;
    function SetCustomEndCap(customCap: TGPCustomLineCap): TStatus;
    function GetCustomEndCap(customCap: TGPCustomLineCap): TStatus;
    function SetMiterLimit(miterLimit: Single): TStatus;
    function GetMiterLimit: Single;
    function SetAlignment(penAlignment: TPenAlignment): TStatus;
    function GetAlignment: TPenAlignment;
    function SetTransform(matrix: TGPMatrix): TStatus;
    function GetTransform(matrix: TGPMatrix): TStatus;
    function ResetTransform: TStatus;
    function MultiplyTransform(matrix: TGPMatrix; order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    function TranslateTransform(dx, dy: Single; order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    function ScaleTransform(sx, sy: Single; order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    function RotateTransform(angle: Single; order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    function GetPenType: TPenType;
    function SetColor(color: TGPColor): TStatus;
    function SetBrush(brush: TGPBrush): TStatus;
    function GetColor(out Color: TGPColor): TStatus;
    function GetBrush: TGPBrush;
    function GetDashStyle: TDashStyle;
    function SetDashStyle(dashStyle: TDashStyle): TStatus;
    function GetDashOffset: Single;
    function SetDashOffset(dashOffset: Single): TStatus;
    function SetDashPattern(dashArray: PSingle; count: Integer): TStatus;
    function GetDashPatternCount: Integer;
    function GetDashPattern(dashArray: PSingle; count: Integer): TStatus;
    function SetCompoundArray(compoundArray: PSingle; count: Integer): TStatus;
    function GetCompoundArrayCount: Integer;
    function GetCompoundArray(compoundArray: PSingle; count: Integer): TStatus;
    function GetLastStatus: TStatus;
  end;

  TGPStringFormat = class(TGdiplusBase)
  protected
    nativeFormat: GpStringFormat;
    lastError: TStatus;
    function SetStatus(newStatus: GpStatus): TStatus;
    procedure Assign(source: TGPStringFormat);
    constructor Create(clonedStringFormat: GpStringFormat; status: TStatus); reintroduce; overload;
  public
    constructor Create(formatFlags: Integer = 0; language: LANGID = LANG_NEUTRAL); reintroduce; overload;
    constructor Create(format: TGPStringFormat); reintroduce; overload;
    destructor Destroy; override;
    class function GenericDefault: TGPStringFormat;
    class function GenericTypographic: TGPStringFormat;
    function Clone: TGPStringFormat;
    function SetFormatFlags(flags: Integer): TStatus;
    function GetFormatFlags: Integer;
    function SetAlignment(align: TStringAlignment): TStatus;
    function GetAlignment: TStringAlignment;
    function SetLineAlignment(align: TStringAlignment): TStatus;
    function GetLineAlignment: TStringAlignment;
    function SetHotkeyPrefix(hotkeyPrefix: THotkeyPrefix): TStatus;
    function GetHotkeyPrefix: THotkeyPrefix;
    function SetTabStops(firstTabOffset: Single; count: Integer; tabStops: PSingle): TStatus;
    function GetTabStopCount: Integer;
    function GetTabStops(count: Integer; firstTabOffset, tabStops: PSingle): TStatus;
    function SetDigitSubstitution(language: LANGID; substitute: TStringDigitSubstitute): TStatus;
    function GetDigitSubstitutionLanguage: LANGID;
    function GetDigitSubstitutionMethod: TStringDigitSubstitute;
    function SetTrimming(trimming: TStringTrimming): TStatus;
    function GetTrimming: TStringTrimming;
    function SetMeasurableCharacterRanges(rangeCount: Integer; ranges: PCharacterRange): TStatus;
    function GetMeasurableCharacterRangeCount: Integer;
    function GetLastStatus: TStatus;
  end;

  TGPGraphicsPath = class(TGdiplusBase)
  protected
    nativePath: GpPath;
    lastResult: TStatus;
    procedure SetNativePath(nativePath: GpPath);
    function SetStatus(status: TStatus): TStatus;
    constructor Create(nativePath: GpPath); reintroduce; overload;
  public
    constructor Create(path: TGPGraphicsPath); reintroduce; overload;
    constructor Create(fillMode: TFillMode = FillModeAlternate); reintroduce; overload;      
    constructor Create(points: PGPPointF; types: PBYTE; count: Integer;
      fillMode: TFillMode = FillModeAlternate); reintroduce; overload;
    constructor Create(points: PGPPoint; types: PBYTE; count: Integer;
      fillMode: TFillMode = FillModeAlternate); reintroduce; overload;
    destructor destroy; override;
    function Clone: TGPGraphicsPath;
    // Reset the path object to empty (and fill mode to FillModeAlternate)
    function Reset: TStatus;
    function GetFillMode: TFillMode;
    function SetFillMode(fillmode: TFillMode): TStatus;
    function GetPathData(pathData: TPathData): TStatus;
    function StartFigure: TStatus;
    function CloseFigure: TStatus;
    function CloseAllFigures: TStatus;
    function SetMarker: TStatus;
    function ClearMarkers: TStatus;
    function Reverse: TStatus;
    function GetLastPoint(out lastPoint: TGPPointF): TStatus;

    function AddLine(const pt1, pt2: TGPPointF): TStatus; overload;
    function AddLine(x1, y1, x2, y2: Single): TStatus; overload;
    function AddLines(points: PGPPointF; count: Integer): TStatus; overload;
    function AddLine(const pt1, pt2: TGPPoint): TStatus; overload;
    function AddLine(x1, y1, x2, y2: Integer): TStatus; overload;
    function AddLines(points: PGPPoint; count: Integer): TStatus; overload;

    function AddArc(rect: TGPRectF; startAngle, sweepAngle: Single): TStatus; overload;
    function AddArc(x, y, width, height, startAngle, sweepAngle: Single): TStatus; overload;
    function AddArc(rect: TGPRect; startAngle, sweepAngle: Single): TStatus; overload;
    function AddArc(x, y, width, height: Integer; startAngle, sweepAngle: Single): TStatus; overload;

    function AddBezier(pt1, pt2, pt3, pt4: TGPPointF): TStatus; overload;
    function AddBezier(x1, y1, x2, y2, x3, y3, x4, y4: Single): TStatus; overload;
    function AddBeziers(points: PGPPointF; count: Integer): TStatus; overload;
    function AddBezier(pt1, pt2, pt3, pt4: TGPPoint): TStatus; overload;
    function AddBezier(x1, y1, x2, y2, x3, y3, x4, y4: Integer): TStatus; overload;
    function AddBeziers(points: PGPPoint; count: Integer): TStatus; overload;

    function AddCurve(points: PGPPointF; count: Integer): TStatus; overload;
    function AddCurve(points: PGPPointF; count: Integer; tension: Single): TStatus; overload;
    function AddCurve(points: PGPPointF; count, offset, numberOfSegments: Integer; tension: Single): TStatus; overload;
    function AddCurve(points: PGPPoint; count: Integer): TStatus; overload;
    function AddCurve(points: PGPPoint; count: Integer; tension: Single): TStatus; overload;
    function AddCurve(points: PGPPoint; count, offset, numberOfSegments: Integer; tension: Single): TStatus; overload;

    function AddClosedCurve(points: PGPPointF; count: Integer): TStatus; overload;
    function AddClosedCurve(points: PGPPointF; count: Integer; tension: Single): TStatus; overload;
    function AddClosedCurve(points: PGPPoint; count: Integer): TStatus; overload;
    function AddClosedCurve(points: PGPPoint; count: Integer; tension: Single): TStatus; overload;

    function AddRectangle(rect: TGPRectF): TStatus; overload;
    function AddRectangles(rects: PGPRectF; count: Integer): TStatus; overload;
    function AddRectangle(rect: TGPRect): TStatus; overload;
    function AddRectangles(rects: PGPRect; count: Integer): TStatus; overload;

    function AddEllipse(rect: TGPRectF): TStatus; overload;
    function AddEllipse(x, y, width, height: Single): TStatus; overload;
    function AddEllipse(rect: TGPRect): TStatus; overload;
    function AddEllipse(x, y, width, height: Integer): TStatus; overload;

    function AddPie(rect: TGPRectF; startAngle, sweepAngle: Single): TStatus; overload;
    function AddPie(x, y, width, height, startAngle, sweepAngle: Single): TStatus; overload;
    function AddPie(rect: TGPRect; startAngle, sweepAngle: Single): TStatus; overload;
    function AddPie(x, y, width, height: Integer; startAngle, sweepAngle: Single): TStatus; overload;

    function AddPolygon(points: PGPPointF; count: Integer): TStatus; overload;
    function AddPolygon(points: PGPPoint; count: Integer): TStatus; overload;

    function AddPath(addingPath: TGPGraphicsPath; connect: Bool): TStatus;

    function AddString(string_: WideString; length: Integer; family : TGPFontFamily;
      style  : Integer; emSize : Single; origin : TGPPointF; format : TGPStringFormat): TStatus; overload;
    function AddString(string_: WideString; length : Integer; family : TGPFontFamily;
      style  : Integer; emSize : Single; layoutRect: TGPRectF; format : TGPStringFormat): TStatus; overload;
    function AddString(string_: WideString; length : Integer; family : TGPFontFamily;
      style  : Integer; emSize : Single; origin : TGPPoint; format : TGPStringFormat): TStatus; overload;
    function AddString(string_: WideString; length : Integer; family : TGPFontFamily;
      style  : Integer; emSize : Single; layoutRect: TGPRect; format : TGPStringFormat): TStatus; overload;

    function Transform(matrix: TGPMatrix): TStatus;

    // This is not always the tightest bounds.
    function GetBounds(out bounds: TGPRectF; matrix: TGPMatrix = nil; pen: TGPPen = nil): TStatus; overload;
    function GetBounds(out bounds: TGPRect; matrix: TGPMatrix = nil; pen: TGPPen = nil): TStatus;  overload;
    // Once flattened, the resultant path is made of line segments and
    // the original path information is lost.  When matrix is NULL the
    // identity matrix is assumed.
    function Flatten(matrix: TGPMatrix = nil; flatness: Single = FlatnessDefault): TStatus;
    function Widen(pen: TGPPen; matrix: TGPMatrix = nil; flatness: Single = FlatnessDefault): TStatus;
    function Outline(matrix: TGPMatrix = nil; flatness: Single = FlatnessDefault): TStatus;
    // Once this is called, the resultant path is made of line segments and
    // the original path information is lost.  When matrix is NULL, the
    // identity matrix is assumed.
    function Warp(destPoints: PGPPointF; count: Integer; srcRect: TGPRectF;
      matrix: TGPMatrix = nil; warpMode: TWarpMode = WarpModePerspective;
      flatness: Single = FlatnessDefault): TStatus;
    function GetPointCount: Integer;
    function GetPathTypes(types: PBYTE; count: Integer): TStatus;
    function GetPathPoints(points: PGPPointF; count: Integer): TStatus; overload;
    function GetPathPoints(points: PGPPoint; count: Integer): TStatus; overload;
    function GetLastStatus: TStatus;

    function IsVisible(point: TGPPointF; g: TGPGraphics = nil): BOOL; overload;
    function IsVisible(x, y: Single; g: TGPGraphics = nil): BOOL; overload;
    function IsVisible(point: TGPPoint; g : TGPGraphics = nil): BOOL; overload;
    function IsVisible(x, y: Integer; g: TGPGraphics = nil): BOOL; overload;

    function IsOutlineVisible(point: TGPPointF; pen: TGPPen; g: TGPGraphics = nil): BOOL; overload;
    function IsOutlineVisible(x, y: Single; pen: TGPPen; g: TGPGraphics = nil): BOOL; overload;
    function IsOutlineVisible(point: TGPPoint; pen: TGPPen; g: TGPGraphics = nil): BOOL; overload;
    function IsOutlineVisible(x, y: Integer; pen: TGPPen; g: TGPGraphics = nil): BOOL; overload;
  end;

//--------------------------------------------------------------------------
// GraphisPathIterator class
//--------------------------------------------------------------------------

  TGPGraphicsPathIterator = class(TGdiplusBase)
  protected
    nativeIterator: GpPathIterator;
    lastResult    : TStatus;
    procedure SetNativeIterator(nativeIterator: GpPathIterator);
    function SetStatus(status: TStatus): TStatus;
  public
    constructor Create(path: TGPGraphicsPath); reintroduce;
    destructor Destroy; override;
    function NextSubpath(out startIndex, endIndex: Integer; out isClosed: bool): Integer; overload;
    function NextSubpath(path: TGPGraphicsPath; out isClosed: BOOL): Integer; overload;
    function NextPathType(out pathType: TPathPointType; out startIndex, endIndex: Integer): Integer;
    function NextMarker(out startIndex, endIndex: Integer): Integer; overload;
    function NextMarker(path: TGPGraphicsPath): Integer; overload;
    function GetCount: Integer;
    function GetSubpathCount: Integer;
    function HasCurve: BOOL;
    procedure Rewind;
    function Enumerate(points: PGPPointF; types: PBYTE; count: Integer): Integer;
    function CopyData(points: PGPPointF; types: PBYTE; startIndex, endIndex: Integer): Integer;
    function GetLastStatus: TStatus;
  end;

//--------------------------------------------------------------------------
// Path Gradient Brush
//--------------------------------------------------------------------------

  TGPPathGradientBrush = class(TGPBrush)
  public
    constructor Create(points: PGPPointF; count: Integer;
      wrapMode: TWrapMode = WrapModeClamp); reintroduce; overload;
    constructor Create(points: PGPPoint; count: Integer;
      wrapMode: TWrapMode = WrapModeClamp); reintroduce; overload;
    constructor Create(path: TGPGraphicsPath); reintroduce; overload;
    constructor Create; reintroduce; overload;
    function GetCenterColor(out Color: TGPColor): TStatus;
    function SetCenterColor(color: TGPColor): TStatus;
    function GetPointCount: Integer;
    function GetSurroundColorCount: Integer;
    function GetSurroundColors(colors: PARGB; var count: Integer): TStatus;
    function SetSurroundColors(colors: PARGB; var count: Integer): TStatus;
    function GetGraphicsPath(path: TGPGraphicsPath): TStatus;
    function SetGraphicsPath(path: TGPGraphicsPath): TStatus;
    function GetCenterPoint(out point: TGPPointF): TStatus; overload;
    function GetCenterPoint(out point: TGPPoint): TStatus; overload;
    function SetCenterPoint(point: TGPPointF): TStatus; overload;
    function SetCenterPoint(point: TGPPoint): TStatus; overload;
    function GetRectangle(out rect: TGPRectF): TStatus; overload;
    function GetRectangle(out rect: TGPRect): TStatus; overload;
    function SetGammaCorrection(useGammaCorrection: BOOL): TStatus; overload;
    function GetGammaCorrection: BOOL; overload;
    function GetBlendCount: Integer;
    function GetBlend(blendFactors, blendPositions: PSingle; count: Integer): TStatus;
    function SetBlend(blendFactors, blendPositions: PSingle; count: Integer): TStatus;
    function GetInterpolationColorCount: Integer;
    function SetInterpolationColors(presetColors: PARGB; blendPositions: PSingle;
      count: Integer): TStatus;
    function GetInterpolationColors(presetColors: PARGB;
      blendPositions: PSingle; count: Integer): TStatus;
    function SetBlendBellShape(focus: Single; scale: Single = 1.0): TStatus;
    function SetBlendTriangularShape(focus: Single; scale: Single = 1.0): TStatus;
    function GetTransform(matrix: TGPMatrix): TStatus;
    function SetTransform(matrix: TGPMatrix): TStatus;
    function ResetTransform: TStatus;
    function MultiplyTransform(matrix: TGPMatrix;
      order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    function TranslateTransform(dx, dy: Single;
      order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    function ScaleTransform(sx, sy: Single;
      order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    function RotateTransform(angle: Single;
      order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    function GetFocusScales(out xScale, yScale: Single): TStatus;
    function SetFocusScales(xScale, yScale: Single): TStatus;
    function GetWrapMode: TWrapMode;
    function SetWrapMode(wrapMode: TWrapMode): TStatus;
  end;

  TGPGraphics = class(TGdiplusBase)
  protected
    nativeGraphics: GpGraphics;
    lastResult: TStatus;
    procedure SetNativeGraphics(graphics: GpGraphics);
    function SetStatus(status: TStatus): TStatus;
    function GetNativeGraphics: GpGraphics;
    function GetNativePen(pen: TGPPen): GpPen;
    constructor Create(graphics: GpGraphics); reintroduce; overload;
  public
    function FromHDC(hdc: HDC): TGPGraphics; overload;
    function FromHDC(hdc: HDC; hdevice: THANDLE): TGPGraphics; overload;
    function FromHWND(hwnd: HWND; icm: BOOL = FALSE): TGPGraphics;
    function FromImage(image: TGPImage): TGPGraphics;
    constructor Create(hdc: HDC; NotUses1: integer = 0; NotUses2: integer = 0); reintroduce; overload;
    constructor Create(hdc: HDC; hdevice: THANDLE); reintroduce; overload;
    constructor Create(hwnd: HWND; icm: BOOL{ = FALSE}); reintroduce; overload;
    constructor Create(image: TGPImage); reintroduce; overload;
    destructor destroy; override;
    procedure Flush(intention: TFlushIntention = FlushIntentionFlush);
    //------------------------------------------------------------------------
    // GDI Interop methods
    //------------------------------------------------------------------------
    // Locks the graphics until ReleaseDC is called
    function GetHDC: HDC;
    procedure ReleaseHDC(hdc: HDC);
    //------------------------------------------------------------------------
    // Rendering modes
    //------------------------------------------------------------------------
    function SetRenderingOrigin(x, y: Integer): TStatus;
    function GetRenderingOrigin(out x, y: Integer): TStatus;
    function SetCompositingMode(compositingMode: TCompositingMode): TStatus;
    function GetCompositingMode: TCompositingMode;
    function SetCompositingQuality(compositingQuality: TCompositingQuality): TStatus;
    function GetCompositingQuality: TCompositingQuality;
    function SetTextRenderingHint(newMode: TTextRenderingHint): TStatus;
    function GetTextRenderingHint: TTextRenderingHint;
    function SetTextContrast(contrast: UINT): TStatus; // 0..12
    function GetTextContrast: UINT;
    function GetInterpolationMode: TInterpolationMode;
    function SetInterpolationMode(interpolationMode: TInterpolationMode): TStatus;
    function GetSmoothingMode: TSmoothingMode;
    function SetSmoothingMode(smoothingMode: TSmoothingMode): TStatus;
    function GetPixelOffsetMode: TPixelOffsetMode;
    function SetPixelOffsetMode(pixelOffsetMode: TPixelOffsetMode): TStatus;
    //------------------------------------------------------------------------
    // Manipulate current world transform
    //------------------------------------------------------------------------
    function SetTransform(matrix: TGPMatrix): TStatus;
    function ResetTransform: TStatus;
    function MultiplyTransform(matrix: TGPMatrix; order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    function TranslateTransform(dx, dy: Single; order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    function ScaleTransform(sx, sy: Single; order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    function RotateTransform(angle: Single; order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    function GetTransform(matrix: TGPMatrix): TStatus;
    function SetPageUnit(unit_: TUnit): TStatus;
    function SetPageScale(scale: Single): TStatus;
    function GetPageUnit: TUnit;
    function GetPageScale: Single;
    function GetDpiX: Single;
    function GetDpiY: Single;
    function TransformPoints(destSpace: TCoordinateSpace; srcSpace: TCoordinateSpace;
      pts: PGPPointF; count: Integer): TStatus; overload;
    function TransformPoints(destSpace: TCoordinateSpace; srcSpace: TCoordinateSpace;
      pts: PGPPoint; count: Integer): TStatus; overload;
    //------------------------------------------------------------------------
    // GetNearestColor (for <= 8bpp surfaces).  Note: Alpha is ignored.
    //------------------------------------------------------------------------
    function GetNearestColor(var color: TGPColor): TStatus;

    // DrawLine(s)
    function DrawLine(pen: TGPPen; x1, y1, x2, y2: Single): TStatus; overload;
    function DrawLine(pen: TGPPen; const pt1, pt2: TGPPointF): TStatus; overload;
    function DrawLines(pen: TGPPen; points: PGPPointF; count: Integer): TStatus; overload;
    function DrawLine(pen: TGPPen; x1, y1, x2, y2: Integer): TStatus; overload;
    function DrawLine(pen: TGPPen; const pt1, pt2: TGPPoint): TStatus; overload;
    function DrawLines(pen: TGPPen; points: PGPPoint; count: Integer): TStatus; overload;

    // DrawArc
    function DrawArc(pen: TGPPen; x, y, width, height, startAngle, sweepAngle: Single): TStatus; overload;
    function DrawArc(pen: TGPPen; const rect: TGPRectF; startAngle, sweepAngle: Single): TStatus; overload;
    function DrawArc(pen: TGPPen; x, y, width, height: Integer; startAngle, sweepAngle: Single): TStatus; overload;
    function DrawArc(pen: TGPPen; const rect: TGPRect; startAngle, sweepAngle: Single): TStatus; overload;

    // DrawBezier(s)
    function DrawBezier(pen: TGPPen; x1, y1, x2, y2, x3, y3, x4, y4: Single): TStatus; overload;
    function DrawBezier(pen: TGPPen; const pt1, pt2, pt3, pt4: TGPPointF): TStatus; overload;
    function DrawBeziers(pen: TGPPen; points: PGPPointF; count: Integer): TStatus; overload;
    function DrawBezier(pen: TGPPen; x1, y1, x2, y2, x3, y3, x4, y4: Integer): TStatus; overload;
    function DrawBezier(pen: TGPPen; const pt1, pt2, pt3, pt4: TGPPoint): TStatus; overload;
    function DrawBeziers(pen: TGPPen; points: PGPPoint; count: Integer): TStatus; overload;

    // DrawRectangle(s)
    function DrawRectangle(pen: TGPPen; const rect: TGPRectF): TStatus; overload;
    function DrawRectangle(pen: TGPPen; x, y, width, height: Single): TStatus; overload;
    function DrawRectangles(pen: TGPPen; rects: PGPRectF; count: Integer): TStatus; overload;
    function DrawRectangle(pen: TGPPen; const rect: TGPRect): TStatus; overload;
    function DrawRectangle(pen: TGPPen; x, y, width, height: Integer): TStatus; overload;
    function DrawRectangles(pen: TGPPen; rects: PGPRect; count: Integer): TStatus; overload;

    // DrawEllipse
    function DrawEllipse(pen: TGPPen; const rect: TGPRectF): TStatus; overload;
    function DrawEllipse(pen: TGPPen; x, y, width, height: Single): TStatus; overload;
    function DrawEllipse(pen: TGPPen; const rect: TGPRect): TStatus; overload;
    function DrawEllipse(pen: TGPPen; x, y, width, height: Integer): TStatus; overload;

    // DrawPie
    function DrawPie(pen: TGPPen; const rect: TGPRectF; startAngle, sweepAngle: Single): TStatus; overload;
    function DrawPie(pen: TGPPen; x, y, width, height, startAngle, sweepAngle: Single): TStatus; overload;
    function DrawPie(pen: TGPPen; const rect: TGPRect; startAngle, sweepAngle: Single): TStatus; overload;
    function DrawPie(pen: TGPPen; x, y, width, height: Integer; startAngle, sweepAngle: Single): TStatus; overload;

    // DrawPolygon
    function DrawPolygon(pen: TGPPen; points: PGPPointF; count: Integer): TStatus; overload;
    function DrawPolygon(pen: TGPPen; points: PGPPoint; count: Integer): TStatus; overload;

    // DrawPath
    function DrawPath(pen: TGPPen; path: TGPGraphicsPath): TStatus;

    // DrawCurve
    function DrawCurve(pen: TGPPen; points: PGPPointF; count: Integer): TStatus; overload;
    function DrawCurve(pen: TGPPen; points: PGPPointF; count: Integer; tension: Single): TStatus; overload;
    function DrawCurve(pen: TGPPen; points: PGPPointF; count, offset,
      numberOfSegments: Integer; tension: Single = 0.5): TStatus; overload;
    function DrawCurve(pen: TGPPen; points: PGPPoint; count: Integer): TStatus; overload;
    function DrawCurve(pen: TGPPen; points: PGPPoint; count: Integer; tension: Single): TStatus; overload;
    function DrawCurve(pen: TGPPen; points: PGPPoint; count, offset, numberOfSegments: Integer;
      tension: Single = 0.5): TStatus; overload;

    // DrawClosedCurve
    function DrawClosedCurve(pen: TGPPen; points: PGPPointF; count: Integer): TStatus; overload;
    function DrawClosedCurve(pen: TGPPen; points: PGPPointF; count: Integer; tension: Single): TStatus; overload;
    function DrawClosedCurve(pen: TGPPen; points: PGPPoint;  count: Integer): TStatus; overload;
    function DrawClosedCurve(pen: TGPPen; points: PGPPoint; count: Integer; tension: Single): TStatus; overload;

    // Clear
    function Clear(color: TGPColor): TStatus;

    // FillRectangle(s)
    function FillRectangle(brush: TGPBrush; const rect: TGPRectF): TStatus; overload;
    function FillRectangle(brush: TGPBrush; x, y, width, height: Single): TStatus; overload;
    function FillRectangles(brush: TGPBrush; rects: PGPRectF; count: Integer): TStatus; overload;
    function FillRectangle(brush: TGPBrush; const rect: TGPRect): TStatus; overload;
    function FillRectangle(brush: TGPBrush; x, y, width, height: Integer): TStatus; overload;
    function FillRectangles(brush: TGPBrush; rects: PGPRect; count: Integer): TStatus; overload;

    // FillPolygon
    function FillPolygon(brush: TGPBrush; points: PGPPointF; count: Integer): TStatus; overload;
    function FillPolygon(brush: TGPBrush; points: PGPPointF; count: Integer; fillMode: TFillMode): TStatus; overload;
    function FillPolygon(brush: TGPBrush; points: PGPPoint; count: Integer): TStatus; overload;
    function FillPolygon(brush: TGPBrush; points: PGPPoint; count: Integer; fillMode: TFillMode): TStatus; overload;

    // FillEllipse
    function FillEllipse(brush: TGPBrush; const rect: TGPRectF): TStatus; overload;
    function FillEllipse(brush: TGPBrush; x, y, width, height: Single): TStatus; overload;
    function FillEllipse(brush: TGPBrush; const rect: TGPRect): TStatus; overload;
    function FillEllipse(brush: TGPBrush; x, y, width, height: Integer): TStatus; overload;

    // FillPie
    function FillPie(brush: TGPBrush; const rect: TGPRectF; startAngle, sweepAngle: Single): TStatus; overload;
    function FillPie(brush: TGPBrush; x, y, width, height, startAngle, sweepAngle: Single): TStatus; overload;
    function FillPie(brush: TGPBrush; const rect: TGPRect; startAngle, sweepAngle: Single): TStatus; overload;
    function FillPie(brush: TGPBrush; x, y, width, height: Integer; startAngle, sweepAngle: Single): TStatus; overload;

    // FillPath
    function FillPath(brush: TGPBrush; path: TGPGraphicsPath): TStatus;

    // FillClosedCurve
    function FillClosedCurve(brush: TGPBrush; points: PGPPointF; count: Integer): TStatus; overload;
    function FillClosedCurve(brush: TGPBrush; points: PGPPointF; count: Integer;
      fillMode: TFillMode; tension: Single = 0.5 ): TStatus; overload;
    function FillClosedCurve(brush: TGPBrush; points: PGPPoint; count: Integer): TStatus; overload;
    function FillClosedCurve(brush: TGPBrush; points: PGPPoint; count: Integer;
      fillMode: TFillMode; tension: Single = 0.5): TStatus; overload;

    // FillRegion
    function FillRegion(brush: TGPBrush; region: TGPRegion): TStatus;

    // DrawString
    function DrawString(string_: WideString; length: Integer; font: TGPFont;
      const layoutRect: TGPRectF; stringFormat: TGPStringFormat; brush: TGPBrush): TStatus; overload;
    function DrawString(string_: WideString; length: Integer; font: TGPFont;
      const origin: TGPPointF; brush: TGPBrush): TStatus; overload;
    function DrawString(string_: WideString; length: Integer; font: TGPFont;
      const origin: TGPPointF; stringFormat: TGPStringFormat; brush: TGPBrush): TStatus; overload;

    // MeasureString
    function MeasureString(string_: WideString; length: Integer; font: TGPFont;
      const layoutRect: TGPRectF; stringFormat: TGPStringFormat; out boundingBox: TGPRectF;
      codepointsFitted: PInteger = nil; linesFilled: PInteger = nil): TStatus; overload;
    function MeasureString(string_: WideString; length: Integer; font: TGPFont;
      const layoutRectSize: TGPSizeF; stringFormat: TGPStringFormat; out size: TGPSizeF;
      codepointsFitted: PInteger = nil; linesFilled: PInteger = nil): TStatus; overload;
    function MeasureString(string_: WideString ; length: Integer; font: TGPFont;
      const origin: TGPPointF; stringFormat: TGPStringFormat;
      out boundingBox: TGPRectF): TStatus; overload;
    function MeasureString(string_: WideString; length: Integer; font: TGPFont;
      const layoutRect: TGPRectF; out boundingBox: TGPRectF): TStatus; overload;
    function MeasureString(string_: WideString; length: Integer; font: TGPFont;
      const origin: TGPPointF; out boundingBox: TGPRectF): TStatus; overload;

    // MeasureCharacterRanges
    function MeasureCharacterRanges(string_: WideString; length: Integer; font: TGPFont;
      const layoutRect: TGPRectF; stringFormat: TGPStringFormat; regionCount: Integer;
      const regions: array of TGPRegion): TStatus; overload;

    // DrawDriverString
    function DrawDriverString(text: PUINT16; length: Integer; font: TGPFont;
      brush: TGPBrush; positions: PGPPointF; flags: Integer; matrix: TGPMatrix): TStatus;

    // MeasureDriverString
    function MeasureDriverString(text: PUINT16; length: Integer; font: TGPFont;
       positions: PGPPointF; flags: Integer; matrix: TGPMatrix;
       out boundingBox: TGPRectF): TStatus;

    // Draw a cached bitmap on this graphics destination offset by
    // x, y. Note this will fail with WrongState if the CachedBitmap
    // native format differs from this Graphics.
    function DrawCachedBitmap(cb: TGPCachedBitmap;  x, y: Integer): TStatus;
    function DrawImage(image: TGPImage; const point: TGPPointF): TStatus; overload;
    function DrawImage(image: TGPImage; x, y: Single): TStatus; overload;
    function DrawImage(image: TGPImage; const rect: TGPRectF): TStatus; overload;
    function DrawImage(image: TGPImage; x, y, width, height: Single): TStatus; overload;
    function DrawImage(image: TGPImage; const point: TGPPoint): TStatus; overload;
    function DrawImage(image: TGPImage; x, y: Integer): TStatus; overload;
    function DrawImage(image: TGPImage; const rect: TGPRect): TStatus; overload;
    function DrawImage(image: TGPImage; x, y, width, height: Integer): TStatus; overload;

    // Affine Draw Image
    // destPoints.length = 3: rect => parallelogram
    //     destPoints[0] <=> top-left corner of the source rectangle
    //     destPoints[1] <=> top-right corner
    //     destPoints[2] <=> bottom-left corner
    // destPoints.length = 4: rect => quad
    //     destPoints[3] <=> bottom-right corner
    function DrawImage(image: TGPImage; destPoints: PGPPointF; count: Integer): TStatus; overload;
    function DrawImage(image: TGPImage; destPoints: PGPPoint; count: Integer): TStatus; overload;
    function DrawImage(image: TGPImage; x, y, srcx, srcy, srcwidth, srcheight: Single; srcUnit: TUnit): TStatus; overload;
    function DrawImage(image: TGPImage; const destRect: TGPRectF; srcx, srcy,
      srcwidth, srcheight: Single; srcUnit: TUnit;
      imageAttributes: TGPImageAttributes = nil; callback: DrawImageAbort = nil;
      callbackData: Pointer = nil): TStatus; overload;
    function DrawImage(image: TGPImage; destPoints: PGPPointF; count: Integer;
      srcx, srcy, srcwidth, srcheight: Single; srcUnit: TUnit;
      imageAttributes: TGPImageAttributes = nil; callback: DrawImageAbort = nil;
      callbackData: Pointer = nil): TStatus; overload;
    function DrawImage(image: TGPImage; x, y, srcx, srcy, srcwidth,
      srcheight: Integer; srcUnit: TUnit): TStatus; overload;
    function DrawImage(image: TGPImage; const destRect: TGPRect; srcx, srcy,
      srcwidth, srcheight: Integer; srcUnit: TUnit;
      imageAttributes: TGPImageAttributes = nil; callback: DrawImageAbort = nil;
      callbackData: Pointer = nil): TStatus; overload;
    function DrawImage(image: TGPImage; destPoints: PGPPoint;
      count, srcx, srcy, srcwidth, srcheight: Integer; srcUnit: TUnit;
      imageAttributes: TGPImageAttributes = nil; callback: DrawImageAbort = nil;
      callbackData: Pointer = nil): TStatus; overload;

    // The following methods are for playing an EMF+ to a graphics
    // via the enumeration interface.  Each record of the EMF+ is
    // sent to the callback (along with the callbackData).  Then
    // the callback can invoke the Metafile::PlayRecord method
    // to play the particular record.
    function EnumerateMetafile(metafile: TGPMetafile; const destPoint: TGPPointF;
      callback: EnumerateMetafileProc; callbackData: Pointer = nil;
      imageAttributes: TGPImageAttributes = nil): TStatus; overload;
    function EnumerateMetafile(metafile: TGPMetafile; const destPoint: TGPPoint;    
       callback: EnumerateMetafileProc; callbackData: pointer = nil;
       imageAttributes: TGPImageAttributes = nil): TStatus; overload;
    function EnumerateMetafile(metafile: TGPMetafile; const destRect: TGPRectF;
       callback: EnumerateMetafileProc; callbackData: Pointer = nil;
       imageAttributes: TGPImageAttributes = nil): TStatus; overload;
    function EnumerateMetafile(metafile: TGPMetafile; const destRect: TGPRect;
       callback: EnumerateMetafileProc; callbackData: Pointer = nil;
       imageAttributes: TGPImageAttributes = nil): TStatus; overload;
    function EnumerateMetafile(metafile: TGPMetafile; destPoints: PGPPointF;
       count: Integer; callback: EnumerateMetafileProc; callbackData: Pointer = nil;
       imageAttributes: TGPImageAttributes = nil): TStatus; overload;
    function EnumerateMetafile(metafile: TGPMetafile; destPoints: PGPPoint;
       count: Integer; callback: EnumerateMetafileProc; callbackData: Pointer = nil;
       imageAttributes: TGPImageAttributes = nil): TStatus; overload;
    function EnumerateMetafile(metafile: TGPMetafile; const destPoint: TGPPointF;
       const srcRect: TGPRectF; srcUnit: TUnit; callback: EnumerateMetafileProc;
       callbackData: pointer = nil; imageAttributes: TGPImageAttributes = nil
       ): TStatus; overload;
    function EnumerateMetafile(metafile : TGPMetafile; const destPoint : TGPPoint;
       const srcRect : TGPRect; srcUnit : TUnit; callback : EnumerateMetafileProc;
       callbackData : Pointer = nil; imageAttributes : TGPImageAttributes = nil
       ): TStatus; overload;
    function EnumerateMetafile(metafile: TGPMetafile; const destRect: TGPRectF;
       const srcRect: TGPRectF; srcUnit: TUnit; callback: EnumerateMetafileProc;
       callbackData: Pointer = nil; imageAttributes: TGPImageAttributes = nil): TStatus; overload;
    function EnumerateMetafile(metafile : TGPMetafile; const destRect, srcRect: TGPRect;
       srcUnit : TUnit; callback : EnumerateMetafileProc; callbackData : Pointer = nil;
       imageAttributes : TGPImageAttributes = nil): TStatus; overload;
    function EnumerateMetafile( metafile: TGPMetafile; destPoints: PGPPointF;
        count: Integer; const srcRect: TGPRectF; srcUnit: TUnit; callback: EnumerateMetafileProc;
        callbackData: Pointer = nil; imageAttributes: TGPImageAttributes = nil): TStatus; overload;
    function EnumerateMetafile(metafile: TGPMetafile; destPoints: PGPPoint;
        count: Integer; const srcRect: TGPRect; srcUnit: TUnit; callback: EnumerateMetafileProc;
        callbackData: Pointer = nil; imageAttributes: TGPImageAttributes = nil): TStatus; overload;

    // SetClip
    function SetClip(g: TGPGraphics; combineMode: TCombineMode = CombineModeReplace): TStatus; overload;
    function SetClip(rect: TGPRectF; combineMode: TCombineMode = CombineModeReplace): TStatus; overload;
    function SetClip(rect: TGPRect; combineMode: TCombineMode = CombineModeReplace): TStatus; overload;
    function SetClip(path: TGPGraphicsPath; combineMode: TCombineMode = CombineModeReplace): TStatus; overload;
    function SetClip(region: TGPRegion; combineMode: TCombineMode = CombineModeReplace): TStatus; overload;
    // This is different than the other SetClip methods because it assumes
    // that the HRGN is already in device units, so it doesn't transform
    // the coordinates in the HRGN.
    function SetClip(hRgn: HRGN; combineMode: TCombineMode = CombineModeReplace): TStatus; overload;

    // IntersectClip
    function IntersectClip(const rect: TGPRectF): TStatus; overload;
    function IntersectClip(const rect: TGPRect): TStatus; overload;
    function IntersectClip(region: TGPRegion): TStatus; overload;
    // ExcludeClip
    function ExcludeClip(const rect: TGPRectF): TStatus; overload;
    function ExcludeClip(const rect: TGPRect): TStatus; overload;
    function ExcludeClip(region: TGPRegion): TStatus; overload;

    function ResetClip: TStatus;

    function TranslateClip(dx, dy: Single): TStatus; overload;
    function TranslateClip(dx, dy: Integer): TStatus; overload;

    function GetClip(region: TGPRegion): TStatus;

    function GetClipBounds(out rect: TGPRectF): TStatus; overload;
    function GetClipBounds(out rect: TGPRect): TStatus; overload;

    function IsClipEmpty: Bool;

    function GetVisibleClipBounds(out rect: TGPRectF): TStatus; overload;
    function GetVisibleClipBounds(out rect: TGPRect): TStatus; overload;

    function IsVisibleClipEmpty: BOOL;

    function IsVisible(x, y: Integer): BOOL; overload;
    function IsVisible(const point: TGPPoint): BOOL; overload;
    function IsVisible(x, y, width, height: Integer): BOOL; overload;
    function IsVisible(const rect: TGPRect): BOOL; overload;
    function IsVisible(x, y: Single): BOOL; overload;
    function IsVisible(const point: TGPPointF): BOOL; overload;
    function IsVisible(x, y, width, height: Single): BOOL; overload;
    function IsVisible(const rect: TGPRectF): BOOL; overload;

    function Save: GraphicsState;
    function Restore(gstate: GraphicsState): TStatus;

    function BeginContainer(const dstrect,srcrect: TGPRectF; unit_: TUnit): GraphicsContainer; overload;
    function BeginContainer(const dstrect, srcrect: TGPRect; unit_: TUnit): GraphicsContainer; overload;
    function BeginContainer: GraphicsContainer; overload;
    function EndContainer(state: GraphicsContainer): TStatus;

    // Only valid when recording metafiles.
    function AddMetafileComment(data: PBYTE; sizeData: UINT): TStatus;

    function GetHalftonePalette: HPALETTE;                                      
    function GetLastStatus: TStatus;
  end;


  TGPAdjustableArrowCap = class(TGPCustomLineCap)
  public
    constructor Create(height, width: Single; isFilled: Bool = TRUE); 
    function SetHeight(height: Single): TStatus;
    function GetHeight: Single;
    function SetWidth(width: Single): TStatus;
    function GetWidth: Single;
    function SetMiddleInset(middleInset: Single): TStatus;
    function GetMiddleInset: Single;
    function SetFillState(isFilled: Bool): TStatus;
    function IsFilled: BOOL;
  end;

  TGPMetafile = class(TGPImage)
  public
    constructor Create(hWmf: HMETAFILE; var wmfPlaceableFileHeader: TWmfPlaceableFileHeader;
      deleteWmf: BOOL = FALSE); overload;
    constructor Create(hEmf: HENHMETAFILE; deleteEmf: BOOL = FALSE); overload;
    constructor Create(filename: WideString); overload;
    constructor Create(filename: WideString; var wmfPlaceableFileHeader: TWmfPlaceableFileHeader); overload;
    constructor Create(stream: IStream); overload;
    constructor Create(referenceHdc: HDC; type_: TEmfType = EmfTypeEmfPlusDual;
      description: PWCHAR = nil); overload;
    constructor Create(referenceHdc: HDC; frameRect: TGPRectF;
      frameUnit: TMetafileFrameUnit = MetafileFrameUnitGdi;
      type_: TEmfType = EmfTypeEmfPlusDual; description: PWCHAR = nil); overload;
    constructor Create(referenceHdc: HDC; frameRect: TGPRect;
      frameUnit: TMetafileFrameUnit = MetafileFrameUnitGdi;
      type_: TEmfType = EmfTypeEmfPlusDual; description: PWCHAR = nil); overload;
    constructor Create(fileName: WideString;referenceHdc: HDC;
      type_: TEmfType = EmfTypeEmfPlusDual; description: PWCHAR = nil); overload;
    constructor Create(fileName: WideString; referenceHdc: HDC; frameRect: TGPRectF;
      frameUnit: TMetafileFrameUnit = MetafileFrameUnitGdi;
      type_: TEmfType = EmfTypeEmfPlusDual; description: PWCHAR = nil); overload;
    constructor Create( fileName: WideString; referenceHdc: HDC; frameRect: TGPRect;
      frameUnit: TMetafileFrameUnit = MetafileFrameUnitGdi;
      type_: TEmfType = EmfTypeEmfPlusDual; description: PWCHAR = nil); overload;
    constructor Create(stream: IStream; referenceHdc: HDC;
      type_: TEmfType = EmfTypeEmfPlusDual; description: PWCHAR = nil); overload;
    constructor Create(stream: IStream; referenceHdc: HDC; frameRect: TGPRectF;
      frameUnit: TMetafileFrameUnit = MetafileFrameUnitGdi;
      type_: TEmfType = EmfTypeEmfPlusDual; description: PWCHAR = nil); overload;
    constructor Create(stream : IStream; referenceHdc : HDC; frameRect : TGPRect;
     frameUnit : TMetafileFrameUnit = MetafileFrameUnitGdi;
     type_ : TEmfType = EmfTypeEmfPlusDual; description : PWCHAR = nil); overload;
    constructor Create; reintroduce; overload;
    function GetMetafileHeader(hWmf: HMETAFILE; var wmfPlaceableFileHeader: TWmfPlaceableFileHeader;
      header: TMetafileHeader): TStatus; overload;
    function GetMetafileHeader(hEmf: HENHMETAFILE; header: TMetafileHeader): TStatus; overload;   // ok
    function GetMetafileHeader(filename: WideString; header: TMetafileHeader): TStatus; overload; // ok
    function GetMetafileHeader(stream: IStream; header: TMetafileHeader): TStatus; overload;      // ok
    function GetMetafileHeader(header: TMetafileHeader): TStatus; overload;                       // ok
    function GetHENHMETAFILE: HENHMETAFILE;
    function PlayRecord(recordType: TEmfPlusRecordType; flags, dataSize: UINT; data: PBYTE): TStatus;
    function SetDownLevelRasterizationLimit(metafileRasterizationLimitDpi: UINT): TStatus;
    function GetDownLevelRasterizationLimit: UINT;
    function EmfToWmfBits(hemf: HENHMETAFILE; cbData16: UINT; pData16: PBYTE;
      iMapMode: Integer = MM_ANISOTROPIC; eFlags: TEmfToWmfBitsFlags = EmfToWmfBitsFlagsDefault): UINT;
  end;

////////////////////////////////////////////////////////////////////////////////

var
   GenericSansSerifFontFamily : TGPFontFamily = nil;
   GenericSerifFontFamily     : TGPFontFamily = nil;
   GenericMonospaceFontFamily : TGPFontFamily = nil;

   GenericTypographicStringFormatBuffer: TGPStringFormat = nil;
   GenericDefaultStringFormatBuffer    : TGPStringFormat = nil;

   StartupInput: TGDIPlusStartupInput;
   gdiplusToken: ULONG;

////////////////////////////////////////////////////////////////////////////////

  constructor TGPImageAttributes.Create;
  begin
    nativeImageAttr := nil;
    lastResult := GdipCreateImageAttributes(nativeImageAttr);
  end;

  destructor TGPImageAttributes.Destroy;
  begin
    GdipDisposeImageAttributes(nativeImageAttr);
    inherited Destroy;
  end;

  function TGPImageAttributes.Clone: TGPImageAttributes;
  var clone: GpImageAttributes;
  begin
    SetStatus(GdipCloneImageAttributes(nativeImageAttr, clone));
    result := TGPImageAttributes.Create(clone, lastResult);
  end;

  function TGPImageAttributes.SetToIdentity(type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
  begin
    result := SetStatus(GdipSetImageAttributesToIdentity(nativeImageAttr, type_));
  end;

  function TGPImageAttributes.Reset(type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
  begin
    result := SetStatus(GdipResetImageAttributes(nativeImageAttr, type_));
  end;

  function TGPImageAttributes.SetColorMatrix(const colorMatrix: TColorMatrix;
    mode: TColorMatrixFlags = ColorMatrixFlagsDefault;
    type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
  begin
    result := SetStatus(GdipSetImageAttributesColorMatrix(nativeImageAttr,
      type_, TRUE, @colorMatrix, nil, mode));
  end;

  function TGPImageAttributes.ClearColorMatrix(type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
  begin
    result := SetStatus(GdipSetImageAttributesColorMatrix(nativeImageAttr, type_,
      FALSE, nil, nil, ColorMatrixFlagsDefault));
  end;

    
  function TGPImageAttributes.SetColorMatrices(const colorMatrix: TColorMatrix;
    const grayMatrix: TColorMatrix; mode: TColorMatrixFlags  = ColorMatrixFlagsDefault;
    type_: TColorAdjustType  = ColorAdjustTypeDefault): TStatus;
  begin
    result := SetStatus(GdipSetImageAttributesColorMatrix(nativeImageAttr, type_,
      TRUE, @colorMatrix, @grayMatrix, mode));
  end;

  function TGPImageAttributes.ClearColorMatrices(Type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
  begin
    result := SetStatus(GdipSetImageAttributesColorMatrix( nativeImageAttr,
      type_, FALSE, nil, nil, ColorMatrixFlagsDefault));
  end;

  function TGPImageAttributes.SetThreshold(threshold: Single; type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
  begin
    result := SetStatus(GdipSetImageAttributesThreshold( nativeImageAttr, type_,
      TRUE, threshold));
  end;

  function TGPImageAttributes.ClearThreshold(type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
  begin
    result := SetStatus(GdipSetImageAttributesThreshold(nativeImageAttr, type_,
      FALSE, 0.0));
  end;

  function TGPImageAttributes.SetGamma(gamma: Single; type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
  begin
    result := SetStatus(GdipSetImageAttributesGamma(nativeImageAttr, type_, TRUE, gamma));
  end;

  function TGPImageAttributes.ClearGamma(type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
  begin
    result := SetStatus(GdipSetImageAttributesGamma(nativeImageAttr, type_, FALSE, 0.0));
  end;

  function TGPImageAttributes.SetNoOp(type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
  begin
    result := SetStatus(GdipSetImageAttributesNoOp(nativeImageAttr, type_, TRUE));
  end;

  function TGPImageAttributes.ClearNoOp(Type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
  begin
    result := SetStatus(GdipSetImageAttributesNoOp( nativeImageAttr, type_, FALSE));
  end;

  function TGPImageAttributes.SetColorKey(colorLow, colorHigh: TGPColor;
    type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
  begin
    result := SetStatus(GdipSetImageAttributesColorKeys(nativeImageAttr, type_,
      TRUE, colorLow, colorHigh));
  end;

  function TGPImageAttributes.ClearColorKey(type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
  begin
    result := SetStatus(GdipSetImageAttributesColorKeys(nativeImageAttr, type_,
      FALSE, 0, 0));
  end;

  function TGPImageAttributes.SetOutputChannel(channelFlags: TColorChannelFlags;
        type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
  begin
    result := SetStatus(GdipSetImageAttributesOutputChannel(nativeImageAttr,
      type_, TRUE, channelFlags));
  end;

  function TGPImageAttributes.ClearOutputChannel(type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
  begin
    result := SetStatus(GdipSetImageAttributesOutputChannel(nativeImageAttr,
      type_, FALSE, ColorChannelFlagsLast));
  end;

  function TGPImageAttributes.SetOutputChannelColorProfile(colorProfileFilename: WideString;
    type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
  begin
    result := SetStatus(GdipSetImageAttributesOutputChannelColorProfile(nativeImageAttr,
      type_, TRUE, PWideChar(colorProfileFilename)));
  end;

  function TGPImageAttributes.ClearOutputChannelColorProfile(type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
  begin
    result := SetStatus(GdipSetImageAttributesOutputChannelColorProfile(nativeImageAttr,
      type_, FALSE, nil));
  end;

  function TGPImageAttributes.SetRemapTable(mapSize: Cardinal; map: PColorMap;
    type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
  begin
    result := SetStatus(GdipSetImageAttributesRemapTable(nativeImageAttr, type_,
      TRUE, mapSize, map));
  end;

  function TGPImageAttributes.ClearRemapTable(type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
  begin
    result := SetStatus(GdipSetImageAttributesRemapTable(nativeImageAttr, type_,
      FALSE, 0, nil));
  end;

  function TGPImageAttributes.SetBrushRemapTable(mapSize: Cardinal; map: PColorMap): TStatus;
  begin
    result := SetRemapTable(mapSize, map, ColorAdjustTypeBrush);
  end;

  function TGPImageAttributes.ClearBrushRemapTable: TStatus;
  begin
    result := ClearRemapTable(ColorAdjustTypeBrush);
  end;

  function TGPImageAttributes.SetWrapMode(wrap: TWrapMode; color: TGPColor = aclBlack;
    clamp: BOOL = FALSE): TStatus;
  begin
    result := SetStatus(GdipSetImageAttributesWrapMode(nativeImageAttr, wrap, color, clamp));
  end;

  // The flags of the palette are ignored.

  function TGPImageAttributes.GetAdjustedPalette(colorPalette: PColorPalette;
    colorAdjustType: TColorAdjustType): TStatus;
  begin
    result := SetStatus(GdipGetImageAttributesAdjustedPalette(nativeImageAttr,
      colorPalette, colorAdjustType));
  end;

  function TGPImageAttributes.GetLastStatus: TStatus;
  begin
    result := lastResult;
    lastResult := Ok;
  end;

  constructor TGPImageAttributes.Create(imageAttr: GpImageAttributes; status: TStatus);
  begin
    SetNativeImageAttr(imageAttr);
    lastResult := status;
  end;

  procedure TGPImageAttributes.SetNativeImageAttr(nativeImageAttr: GpImageAttributes);
  begin
    self.nativeImageAttr := nativeImageAttr;
  end;

  function TGPImageAttributes.SetStatus(status: TStatus): TStatus;
  begin
    if (status <> Ok) then lastResult := status;
    result := status;
  end;

(**************************************************************************\
*
*   GDI+ Matrix class
*
\**************************************************************************)

  // Default constructor is set to identity matrix.
  constructor TGPMatrix.Create;
  var matrix: GpMatrix;
  begin
    matrix := nil;
    lastResult := GdipCreateMatrix(matrix);
    SetNativeMatrix(matrix);
  end;

  constructor TGPMatrix.Create(m11, m12, m21, m22, dx, dy: Single);
  var matrix: GpMatrix;
  begin
    matrix := nil;
    lastResult := GdipCreateMatrix2(m11, m12, m21, m22, dx, dy, matrix);
    SetNativeMatrix(matrix);
  end;

  constructor TGPMatrix.Create(const rect: TGPRectF; const dstplg: TGPPointF);
  var matrix: GpMatrix;
  begin
    matrix := nil;
    lastResult := GdipCreateMatrix3(@rect, @dstplg, matrix);
    SetNativeMatrix(matrix);
  end;

  constructor TGPMatrix.Create(const rect: TGPRect; const dstplg: TGPPoint);
  var matrix: GpMatrix;
  begin
    matrix := nil;
    lastResult := GdipCreateMatrix3I(@rect, @dstplg, matrix);
    SetNativeMatrix(matrix);
  end;

  destructor TGPMatrix.Destroy;
  begin
    GdipDeleteMatrix(nativeMatrix);
  end;

  function TGPMatrix.Clone: TGPMatrix;
  var cloneMatrix: GpMatrix;
  begin
    cloneMatrix := nil;
    SetStatus(GdipCloneMatrix(nativeMatrix, cloneMatrix));
    if (lastResult <> Ok) then
    begin
      result := nil;
      exit;
    end;
    result := TGPMatrix.Create(cloneMatrix);
  end;

  function TGPMatrix.GetElements(const m: TMatrixArray): TStatus;
  begin
    result := SetStatus(GdipGetMatrixElements(nativeMatrix, @m));
  end;

  function TGPMatrix.SetElements(m11, m12, m21, m22, dx, dy: Single): TStatus;
  begin
        result := SetStatus(GdipSetMatrixElements(nativeMatrix,
                            m11, m12, m21, m22, dx, dy));
  end;

  function TGPMatrix.OffsetX: Single;
  var elements: TMatrixArray;
  begin
    if (GetElements(elements) = Ok) then
      result := elements[4]
    else
      result := 0.0;
  end;

  function TGPMatrix.OffsetY: Single;
  var elements: TMatrixArray;
  begin
    if (GetElements(elements) = Ok) then result := elements[5]
                                        else result := 0.0;
  end;

  function TGPMatrix.Reset: TStatus;
  begin
    // set identity matrix elements
    result := SetStatus(GdipSetMatrixElements(nativeMatrix, 1.0, 0.0, 0.0, 1.0,
                0.0, 0.0));
  end;

  function TGPMatrix.Multiply(matrix: TGPMatrix; order: TMatrixOrder = MatrixOrderPrepend): TStatus;
  begin
    result := SetStatus(GdipMultiplyMatrix(nativeMatrix, matrix.nativeMatrix, order));
  end;

  function TGPMatrix.Translate(offsetX, offsetY: Single; order: TMatrixOrder = MatrixOrderPrepend): TStatus;
  begin
    result := SetStatus(GdipTranslateMatrix(nativeMatrix, offsetX, offsetY, order));
  end;

  function TGPMatrix.Scale(scaleX, scaleY: Single; order: TMatrixOrder = MatrixOrderPrepend): TStatus;
  begin
    result := SetStatus(GdipScaleMatrix(nativeMatrix, scaleX, scaleY, order));
  end;

  function TGPMatrix.Rotate(angle: Single; order: TMatrixOrder = MatrixOrderPrepend): TStatus;
  begin
    result := SetStatus(GdipRotateMatrix(nativeMatrix, angle, order));
  end;

  function TGPMatrix.RotateAt(angle: Single; const center: TGPPointF; order: TMatrixOrder = MatrixOrderPrepend): TStatus;
  begin
    if(order = MatrixOrderPrepend) then
    begin
      SetStatus(GdipTranslateMatrix(nativeMatrix, center.X, center.Y, order));
      SetStatus(GdipRotateMatrix(nativeMatrix, angle, order));
      result := SetStatus(GdipTranslateMatrix(nativeMatrix, -center.X, -center.Y,
                  order));
    end
    else
    begin
      SetStatus(GdipTranslateMatrix(nativeMatrix, - center.X, - center.Y, order));
      SetStatus(GdipRotateMatrix(nativeMatrix, angle, order));
      result := SetStatus(GdipTranslateMatrix(nativeMatrix, center.X, center.Y,
                  order));
    end;
  end;

  function TGPMatrix.Shear(shearX, shearY: Single; order: TMatrixOrder = MatrixOrderPrepend): TStatus;
  begin
    result := SetStatus(GdipShearMatrix(nativeMatrix, shearX, shearY, order));
  end;

  function TGPMatrix.Invert: TStatus;
  begin
    result := SetStatus(GdipInvertMatrix(nativeMatrix));
  end;

  // float version
  function TGPMatrix.TransformPoints(pts: PGPPointF; count: Integer = 1): TStatus;
  begin
    result := SetStatus(GdipTransformMatrixPoints(nativeMatrix, pts, count));
  end;

  function TGPMatrix.TransformPoints(pts: PGPPoint; count: Integer = 1): TStatus;
  begin
    result := SetStatus(GdipTransformMatrixPointsI(nativeMatrix, pts, count));
  end;

  function TGPMatrix.TransformVectors(pts: PGPPointF; count: Integer = 1): TStatus;
  begin
    result := SetStatus(GdipVectorTransformMatrixPoints( nativeMatrix, pts, count));
  end;

  function TGPMatrix.TransformVectors(pts: PGPPoint; count: Integer = 1): TStatus;
  begin
    result := SetStatus(GdipVectorTransformMatrixPointsI(nativeMatrix, pts, count));
  end;

  function TGPMatrix.IsInvertible: BOOL;
  begin
    result := FALSE;
    SetStatus(GdipIsMatrixInvertible(nativeMatrix, result));
  end;

  function TGPMatrix.IsIdentity: BOOL;
  begin
    result := False;
    SetStatus(GdipIsMatrixIdentity(nativeMatrix, result));
  end;

  function TGPMatrix.Equals(matrix: TGPMatrix): BOOL;
  begin
    result := FALSE;
    SetStatus(GdipIsMatrixEqual(nativeMatrix, matrix.nativeMatrix, result));
  end;

  function TGPMatrix.GetLastStatus: TStatus;
  begin
    result := lastResult;
    lastResult := Ok;
  end;

  constructor TGPMatrix.Create(nativeMatrix: GpMatrix);
  begin
    lastResult := Ok;
    SetNativeMatrix(nativeMatrix);
  end;

  procedure TGPMatrix.SetNativeMatrix(nativeMatrix: GpMatrix);
  begin
    self.nativeMatrix := nativeMatrix;
  end;

  function TGPMatrix.SetStatus(status: TStatus): TStatus;
  begin
    if (status <> Ok) then lastResult := status;
    result := status;
  end;


(**************************************************************************\
*
*   GDI+ StringFormat class
*
\**************************************************************************)

  constructor TGPStringFormat.Create(formatFlags: Integer = 0; language: LANGID = LANG_NEUTRAL);
  begin
    nativeFormat := nil;
    lastError := GdipCreateStringFormat(formatFlags, language, nativeFormat);
  end;

  class function TGPStringFormat.GenericDefault: TGPStringFormat;
  begin
    if not assigned(GenericDefaultStringFormatBuffer) then
    begin
      GenericDefaultStringFormatBuffer := TGPStringFormat.Create;
      GenericDefaultStringFormatBuffer.lastError :=
        GdipStringFormatGetGenericDefault(GenericDefaultStringFormatBuffer.nativeFormat);
    end;
    result := GenericDefaultStringFormatBuffer;
  end;

  class function TGPStringFormat.GenericTypographic: TGPStringFormat;
  begin
    if not assigned(GenericTypographicStringFormatBuffer) then
    begin
      GenericTypographicStringFormatBuffer := TGPStringFormat.Create;
      GenericTypographicStringFormatBuffer.lastError :=
        GdipStringFormatGetGenericTypographic(GenericTypographicStringFormatBuffer.nativeFormat);
    end;
    result := GenericTypographicStringFormatBuffer;
  end;

  constructor TGPStringFormat.Create(format: TGPStringFormat);
  var gpstf: GPSTRINGFORMAT;
  begin
    nativeFormat := nil;
    if assigned(format) then gpstf := format.nativeFormat
                        else gpstf := nil;
    lastError := GdipCloneStringFormat(gpstf, nativeFormat);
  end;

  function TGPStringFormat.Clone: TGPStringFormat;
  var
    clonedStringFormat: GpStringFormat;
  begin
    clonedStringFormat := nil;
    lastError := GdipCloneStringFormat(nativeFormat, clonedStringFormat);
    if (lastError = Ok) then
      result := TGPStringFormat.Create(clonedStringFormat, lastError)
    else
      result := nil;
  end;

  destructor TGPStringFormat.Destroy;
  begin
    GdipDeleteStringFormat(nativeFormat);
  end;

  function TGPStringFormat.SetFormatFlags(flags: Integer): TStatus;
  begin
    result := SetStatus(GdipSetStringFormatFlags(nativeFormat, flags));
  end;

  function TGPStringFormat.GetFormatFlags: Integer;
  begin
    SetStatus(GdipGetStringFormatFlags(nativeFormat, result));
  end;

  function TGPStringFormat.SetAlignment(align: TStringAlignment): TStatus;
  begin
    result := SetStatus(GdipSetStringFormatAlign(nativeFormat, align));
  end;

  function TGPStringFormat.GetAlignment: TStringAlignment;
  begin
    SetStatus(GdipGetStringFormatAlign(nativeFormat, result));
  end;

  function TGPStringFormat.SetLineAlignment(align: TStringAlignment): TStatus;
  begin
    result := SetStatus(GdipSetStringFormatLineAlign(nativeFormat, align));
  end;

  function TGPStringFormat.GetLineAlignment: TStringAlignment;
  begin
    SetStatus(GdipGetStringFormatLineAlign(nativeFormat, result));
  end;

  function TGPStringFormat.SetHotkeyPrefix(hotkeyPrefix: THotkeyPrefix): TStatus;
  begin
    result := SetStatus(GdipSetStringFormatHotkeyPrefix(nativeFormat, Integer(hotkeyPrefix)));
  end;

  function TGPStringFormat.GetHotkeyPrefix: THotkeyPrefix;
  var HotkeyPrefix: Integer;
  begin
    SetStatus(GdipGetStringFormatHotkeyPrefix(nativeFormat, HotkeyPrefix));
    result := THotkeyPrefix(HotkeyPrefix);
  end;

  function TGPStringFormat.SetTabStops(firstTabOffset: Single; count: Integer; tabStops: PSingle): TStatus;
  begin
    result := SetStatus(GdipSetStringFormatTabStops(nativeFormat, firstTabOffset, count, tabStops));
  end;

  function TGPStringFormat.GetTabStopCount: Integer;
  begin
    SetStatus(GdipGetStringFormatTabStopCount(nativeFormat, result));
  end;

  function TGPStringFormat.GetTabStops(count: Integer; firstTabOffset, tabStops: PSingle): TStatus;
  begin
    result := SetStatus(GdipGetStringFormatTabStops(nativeFormat, count, firstTabOffset, tabStops));
  end;

  function TGPStringFormat.SetDigitSubstitution(language: LANGID; substitute: TStringDigitSubstitute): TStatus;
  begin
    result := SetStatus(GdipSetStringFormatDigitSubstitution(nativeFormat, language, substitute));
  end;

  function TGPStringFormat.GetDigitSubstitutionLanguage: LANGID;
  begin
    SetStatus(GdipGetStringFormatDigitSubstitution(nativeFormat, @result, nil));
  end;

  function TGPStringFormat.GetDigitSubstitutionMethod: TStringDigitSubstitute;
  begin
    SetStatus(GdipGetStringFormatDigitSubstitution(nativeFormat, nil, @result));
  end;

  function TGPStringFormat.SetTrimming(trimming: TStringTrimming): TStatus;
  begin
    result := SetStatus(GdipSetStringFormatTrimming(nativeFormat, trimming));
  end;

  function TGPStringFormat.GetTrimming: TStringTrimming;
  begin
    SetStatus(GdipGetStringFormatTrimming(nativeFormat, result));
  end;

  function TGPStringFormat.SetMeasurableCharacterRanges(rangeCount: Integer;
    ranges: PCharacterRange): TStatus;
  begin
    result := SetStatus(GdipSetStringFormatMeasurableCharacterRanges(nativeFormat,
      rangeCount, ranges));
  end;

  function TGPStringFormat.GetMeasurableCharacterRangeCount: Integer;
  begin
    SetStatus(GdipGetStringFormatMeasurableCharacterRangeCount(nativeFormat, result));
  end;

  function TGPStringFormat.GetLastStatus: TStatus;
  begin
    result := lastError;
    lastError := Ok;
  end;

  function TGPStringFormat.SetStatus(newStatus: GpStatus): TStatus;
  begin
    if (newStatus <> Ok) then lastError := newStatus;
    result := newStatus;
  end;

  // operator =
  procedure TGPStringFormat.Assign(source: TGPStringFormat);
  begin
    assert(assigned(source));
    GdipDeleteStringFormat(nativeFormat);
    lastError := GdipCloneStringFormat(source.nativeFormat, nativeFormat);
  end;

  constructor TGPStringFormat.Create(clonedStringFormat: GpStringFormat; status: TStatus);
  begin
    lastError := status;
    nativeFormat := clonedStringFormat;
  end;



  // ---------------------------------------------------------------------------
  //  TAdjustableArrowCap
  // ---------------------------------------------------------------------------

  constructor TGPAdjustableArrowCap.Create(height, width: Single; isFilled: Bool = TRUE);
  var cap: GpAdjustableArrowCap;
  begin
    cap := nil;
    lastResult := GdipCreateAdjustableArrowCap(height, width, isFilled, cap);
    SetNativeCap(cap);
  end;

  function TGPAdjustableArrowCap.SetHeight(height: Single): TStatus;
  begin
    result := SetStatus(GdipSetAdjustableArrowCapHeight(GpAdjustableArrowCap(nativeCap), height));
  end;

  function TGPAdjustableArrowCap.GetHeight: Single;
  begin
    SetStatus(GdipGetAdjustableArrowCapHeight(GpAdjustableArrowCap(nativeCap), result));
  end;

  function TGPAdjustableArrowCap.SetWidth(width: Single): TStatus;
  begin
    result := SetStatus(GdipSetAdjustableArrowCapWidth(GpAdjustableArrowCap(nativeCap), width));
  end;

  function TGPAdjustableArrowCap.GetWidth: Single;
  begin
    SetStatus(GdipGetAdjustableArrowCapWidth(GpAdjustableArrowCap(nativeCap), result));
  end;

  function TGPAdjustableArrowCap.SetMiddleInset(middleInset: Single): TStatus;
  begin
    result := SetStatus(GdipSetAdjustableArrowCapMiddleInset(GpAdjustableArrowCap(nativeCap), middleInset));
  end;

  function TGPAdjustableArrowCap.GetMiddleInset: Single;
  begin
    SetStatus(GdipGetAdjustableArrowCapMiddleInset(
      GpAdjustableArrowCap(nativeCap), result));
  end;

  function TGPAdjustableArrowCap.SetFillState(isFilled: Bool): TStatus;
  begin
    result := SetStatus(GdipSetAdjustableArrowCapFillState(
      GpAdjustableArrowCap(nativeCap), isFilled));
  end;

  function TGPAdjustableArrowCap.IsFilled: BOOL;
  begin
    SetStatus(GdipGetAdjustableArrowCapFillState(
      GpAdjustableArrowCap(nativeCap), result));
  end;

(**************************************************************************\
*
*   GDI+ Metafile class
*
\**************************************************************************)

    // Playback a metafile from a HMETAFILE
    // If deleteWmf is TRUE, then when the metafile is deleted,
    // the hWmf will also be deleted.  Otherwise, it won't be.

  constructor TGPMetafile.Create(hWmf: HMETAFILE;
    var wmfPlaceableFileHeader: TWmfPlaceableFileHeader; deleteWmf: BOOL = FALSE);
  var
    metafile: GpMetafile;
  begin
    metafile := nil;
    lastResult := GdipCreateMetafileFromWmf(hWmf, deleteWmf, @wmfPlaceableFileHeader, metafile);
    SetNativeImage(metafile);
  end;

    // Playback a metafile from a HENHMETAFILE
    // If deleteEmf is TRUE, then when the metafile is deleted,
    // the hEmf will also be deleted.  Otherwise, it won't be.

  constructor TGPMetafile.Create(hEmf: HENHMETAFILE; deleteEmf: BOOL = FALSE);
  var
    metafile: GpMetafile;
  begin
    metafile := nil;
    lastResult := GdipCreateMetafileFromEmf(hEmf, deleteEmf, metafile);
    SetNativeImage(metafile);
  end;

  constructor TGPMetafile.Create(filename: WideString);
  var
    metafile: GpMetafile;
  begin
    metafile := nil;
    lastResult := GdipCreateMetafileFromFile(PWideChar(filename), metafile);
    SetNativeImage(metafile);
  end;

    // Playback a WMF metafile from a file.

  constructor TGPMetafile.Create(filename: Widestring; var wmfPlaceableFileHeader: TWmfPlaceableFileHeader);
  var
    metafile: GpMetafile;
  begin
    metafile := nil;
    lastResult := GdipCreateMetafileFromWmfFile(PWideChar(filename), @wmfPlaceableFileHeader, metafile);
    SetNativeImage(metafile);
  end;

  constructor TGPMetafile.Create(stream: IStream);
  var
    metafile: GpMetafile;
  begin
    metafile := nil;
    lastResult := GdipCreateMetafileFromStream(stream, metafile);
    SetNativeImage(metafile);
  end;

    // Record a metafile to memory.

  constructor TGPMetafile.Create(referenceHdc: HDC; type_: TEmfType = EmfTypeEmfPlusDual;
    description: PWCHAR = nil);
  var
    metafile: GpMetafile;
  begin
    metafile := nil;
    lastResult := GdipRecordMetafile(referenceHdc, type_, nil, MetafileFrameUnitGdi,
       description, metafile);
    SetNativeImage(metafile);
  end;

    // Record a metafile to memory.

  constructor TGPMetafile.Create(referenceHdc: HDC; frameRect: TGPRectF;
     frameUnit: TMetafileFrameUnit = MetafileFrameUnitGdi;
     type_: TEmfType = EmfTypeEmfPlusDual; description: PWCHAR = nil);
  var metafile: GpMetafile;
  begin
    metafile := nil;
    lastResult := GdipRecordMetafile(referenceHdc, type_, @frameRect, frameUnit,
      description, metafile);
    SetNativeImage(metafile);
  end;

    // Record a metafile to memory.

  constructor TGPMetafile.Create(referenceHdc: HDC; frameRect: TGPRect;
    frameUnit: TMetafileFrameUnit = MetafileFrameUnitGdi;
    type_: TEmfType = EmfTypeEmfPlusDual; description: PWCHAR = nil);
  var
    metafile: GpMetafile;
  begin
    metafile := nil;
    lastResult := GdipRecordMetafileI(referenceHdc, type_, @frameRect, frameUnit,
      description, metafile);
    SetNativeImage(metafile);
  end;

  constructor TGPMetafile.Create(fileName: WideString; referenceHdc: HDC;
    type_: TEmfType = EmfTypeEmfPlusDual; description: PWCHAR = nil);
  var
    metafile: GpMetafile;
  begin
    metafile := nil;
    lastResult := GdipRecordMetafileFileName(PWideChar(fileName),
      referenceHdc, type_, nil, MetafileFrameUnitGdi, description, metafile);
    SetNativeImage(metafile);
  end;

  constructor TGPMetafile.Create(fileName: WideString; referenceHdc: HDC; frameRect: TGPRectF;
    frameUnit: TMetafileFrameUnit = MetafileFrameUnitGdi; type_: TEmfType = EmfTypeEmfPlusDual;
    description: PWCHAR = nil);
  var
    metafile: GpMetafile;
  begin
    metafile := nil;
    lastResult := GdipRecordMetafileFileName(PWideChar(fileName), referenceHdc,
      type_, @frameRect, frameUnit, description, metafile);
    SetNativeImage(metafile);
  end;

  constructor TGPMetafile.Create(fileName: WideString; referenceHdc: HDC; frameRect: TGPRect;
    frameUnit: TMetafileFrameUnit = MetafileFrameUnitGdi; type_: TEmfType = EmfTypeEmfPlusDual;
    description: PWCHAR = nil);
  var
    metafile: GpMetafile;
  begin
    metafile := nil;
    lastResult := GdipRecordMetafileFileNameI(PWideChar(fileName),
      referenceHdc, type_, @frameRect, frameUnit, description, metafile);
    SetNativeImage(metafile);
  end;

  constructor TGPMetafile.Create(stream: IStream; referenceHdc: HDC;
    type_: TEmfType = EmfTypeEmfPlusDual; description: PWCHAR = nil);
  var
    metafile: GpMetafile;
  begin
    metafile := nil;
    lastResult := GdipRecordMetafileStream(stream, referenceHdc, type_, nil,
      MetafileFrameUnitGdi, description, metafile);
    SetNativeImage(metafile);
  end;

  constructor TGPMetafile.Create(stream: IStream; referenceHdc: HDC; frameRect: TGPRectF;
    frameUnit: TMetafileFrameUnit = MetafileFrameUnitGdi; type_: TEmfType = EmfTypeEmfPlusDual;
    description: PWCHAR = nil);
  var
    metafile: GpMetafile;
  begin
    metafile := nil;
    lastResult := GdipRecordMetafileStream(stream, referenceHdc, type_,
      @frameRect, frameUnit, description, metafile);
    SetNativeImage(metafile);
  end;

  constructor TGPMetafile.Create(stream : IStream; referenceHdc : HDC; frameRect : TGPRect;
    frameUnit : TMetafileFrameUnit = MetafileFrameUnitGdi; type_ : TEmfType = EmfTypeEmfPlusDual;
    description : PWCHAR = nil);
  var
    metafile: GpMetafile;
  begin
    metafile := nil;
    lastResult := GdipRecordMetafileStreamI(stream, referenceHdc, type_,
      @frameRect, frameUnit, description, metafile);
    SetNativeImage(metafile);
  end;

  function TGPMetafile.GetMetafileHeader(hWmf: HMETAFILE;
    var wmfPlaceableFileHeader: TWmfPlaceableFileHeader; header: TMetafileHeader): TStatus;
  begin
    result := GdipGetMetafileHeaderFromWmf(hWmf, @wmfPlaceableFileHeader, @header.Type_);
  end;

  function TGPMetafile.GetMetafileHeader(hEmf: HENHMETAFILE; header: TMetafileHeader): TStatus;
  begin
    result := GdipGetMetafileHeaderFromEmf(hEmf, @header.Type_);
  end;

  function TGPMetafile.GetMetafileHeader(filename: WideString; header: TMetafileHeader): TStatus;
  begin
    result := GdipGetMetafileHeaderFromFile(PWideChar(filename), @header.Type_);
  end;

  function TGPMetafile.GetMetafileHeader(stream: IStream; header: TMetafileHeader): TStatus;
  begin
    result := GdipGetMetafileHeaderFromStream(stream, @header.Type_);
  end;

  function TGPMetafile.GetMetafileHeader(header: TMetafileHeader): TStatus;
  begin
    result := SetStatus(GdipGetMetafileHeaderFromMetafile(GpMetafile(nativeImage),
      @header.Type_));
  end;

  function TGPMetafile.GetHENHMETAFILE: HENHMETAFILE;
  begin
    SetStatus(GdipGetHemfFromMetafile(GpMetafile(nativeImage), result));
  end;

  function TGPMetafile.PlayRecord(recordType: TEmfPlusRecordType; flags, dataSize: UINT;
    data: PBYTE): TStatus;
  begin
    result := SetStatus(GdipPlayMetafileRecord(GpMetafile(nativeImage),
      recordType, flags, dataSize, data));
  end;

  function TGPMetafile.SetDownLevelRasterizationLimit(metafileRasterizationLimitDpi: UINT): TStatus;
  begin
    result := SetStatus(GdipSetMetafileDownLevelRasterizationLimit(
      GpMetafile(nativeImage), metafileRasterizationLimitDpi));
  end;

  function TGPMetafile.GetDownLevelRasterizationLimit: UINT;
  var metafileRasterizationLimitDpi: UINT;
  begin
    metafileRasterizationLimitDpi := 0;
    SetStatus(GdipGetMetafileDownLevelRasterizationLimit(
      GpMetafile(nativeImage), metafileRasterizationLimitDpi));
    result := metafileRasterizationLimitDpi;
  end;

  function TGPMetafile.EmfToWmfBits(hemf: HENHMETAFILE; cbData16: UINT; pData16: PBYTE;
    iMapMode: Integer = MM_ANISOTROPIC; eFlags: TEmfToWmfBitsFlags = EmfToWmfBitsFlagsDefault): UINT;
  begin
    result := GdipEmfToWmfBits(hemf, cbData16, pData16, iMapMode, Integer(eFlags));
  end;

  constructor TGPMetafile.Create;
  begin
    SetNativeImage(nil);
    lastResult := Ok;
  end;

//--------------------------------------------------------------------------
// Codec Management APIs
//--------------------------------------------------------------------------

  function GetImageDecodersSize(out numDecoders, size: UINT): TStatus;
  begin
    result := GdipGetImageDecodersSize(numDecoders, size);
  end;

  function GetImageDecoders(numDecoders, size: UINT;
     decoders: PImageCodecInfo): TStatus;
  begin
    result := GdipGetImageDecoders(numDecoders, size, decoders);
  end;


  function GetImageEncodersSize(out numEncoders, size: UINT): TStatus;
  begin
    result := GdipGetImageEncodersSize(numEncoders, size);
  end;

  function GetImageEncoders(numEncoders, size: UINT;
     encoders: PImageCodecInfo): TStatus;
  begin
    result := GdipGetImageEncoders(numEncoders, size, encoders);
  end;

  constructor TGPRegion.Create;
  var
    region: GpRegion;
  begin
    region := nil;
    lastResult := GdipCreateRegion(region);
    SetNativeRegion(region);
  end;

  constructor TGPRegion.Create(rect: TGPRectF);
  var
    region: GpRegion;
  begin
    region := nil;
    lastResult := GdipCreateRegionRect(@rect, region);
    SetNativeRegion(region);
  end;

  constructor TGPRegion.Create(rect: TGPRect);
  var
    region: GpRegion;
  begin
    region := nil;
    lastResult := GdipCreateRegionRectI(@rect, region);
    SetNativeRegion(region);
  end;

  constructor TGPRegion.Create(path: TGPGraphicsPath);
  var
    region: GpRegion;
  begin
    region := nil;
    lastResult := GdipCreateRegionPath(path.nativePath, region);
    SetNativeRegion(region);
  end;

  constructor TGPRegion.Create(regionData: PBYTE; size: Integer);
  var
    region: GpRegion;
  begin
    region := nil;
    lastResult := GdipCreateRegionRgnData(regionData, size, region);
    SetNativeRegion(region);
  end;

  {$IFNDEF KS_CBUILDER}
  constructor TGPRegion.Create(hRgn: HRGN);
  var
    region: GpRegion;
  begin
    region := nil;
    lastResult := GdipCreateRegionHrgn(hRgn, region);
    SetNativeRegion(region);
  end;
  {$ENDIF}

  function TGPRegion.FromHRGN(hRgn: HRGN): TGPRegion;
  var
    region: GpRegion;
  begin
    region := nil;
    if (GdipCreateRegionHrgn(hRgn, region) = Ok) then
    begin
        result := TGPRegion.Create(region);
        if (result = nil) then
            GdipDeleteRegion(region);
        exit;
    end
    else
      result := nil;
  end;

  destructor TGPRegion.Destroy;
  begin
    GdipDeleteRegion(nativeRegion);
  end;

  function TGPRegion.Clone: TGPRegion;
  var region: GpRegion;
  begin
    region := nil;
    SetStatus(GdipCloneRegion(nativeRegion, region));
    result := TGPRegion.Create(region);
  end;

  function TGPRegion.MakeInfinite: TStatus;
  begin
    result := SetStatus(GdipSetInfinite(nativeRegion));
  end;

  function TGPRegion.MakeEmpty: TStatus;
  begin
    result := SetStatus(GdipSetEmpty(nativeRegion));
  end;

  function TGPRegion.GetDataSize: UINT;
  var bufferSize: UINT;
  begin
    bufferSize := 0;
    SetStatus(GdipGetRegionDataSize(nativeRegion, bufferSize));
    result := bufferSize;
  end;

  function TGPRegion.GetData(buffer: PBYTE; bufferSize: UINT; sizeFilled: PUINT = nil): TStatus;
  begin
    result := SetStatus(GdipGetRegionData(nativeRegion, buffer, bufferSize, sizeFilled));
  end;

  function TGPRegion.Intersect(const rect: TGPRect): TStatus;
  begin
    result := SetStatus(GdipCombineRegionRectI(nativeRegion, @rect, CombineModeIntersect));
  end;

  function TGPRegion.Intersect(const rect: TGPRectF): TStatus;
  begin
    result := SetStatus(GdipCombineRegionRect(nativeRegion, @rect, CombineModeIntersect));
  end;

  function TGPRegion.Intersect(path: TGPGraphicsPath): TStatus;
  begin
    result := SetStatus(GdipCombineRegionPath(nativeRegion, path.nativePath,
      CombineModeIntersect));
  end;

  function TGPRegion.Intersect(region: TGPRegion): TStatus;
  begin
    result := SetStatus(GdipCombineRegionRegion(nativeRegion, region.nativeRegion,
      CombineModeIntersect));
  end;

  function TGPRegion.Union(const rect: TGPRect): TStatus;
  begin
    result := SetStatus(GdipCombineRegionRectI(nativeRegion, @rect, CombineModeUnion));
  end;

  function TGPRegion.Union(const rect: TGPRectF): TStatus;
  begin
    result := SetStatus(GdipCombineRegionRect(nativeRegion, @rect, CombineModeUnion));
  end;

  function TGPRegion.Union(path: TGPGraphicsPath): TStatus;
  begin
    result := SetStatus(GdipCombineRegionPath(nativeRegion, path.nativePath, CombineModeUnion));
  end;

  function TGPRegion.Union(region: TGPRegion): TStatus;
  begin
    result := SetStatus(GdipCombineRegionRegion(nativeRegion, region.nativeRegion,
      CombineModeUnion));
  end;

  function TGPRegion.Xor_(const rect: TGPRect): TStatus;
  begin
    result := SetStatus(GdipCombineRegionRectI(nativeRegion, @rect, CombineModeXor));
  end;

  function TGPRegion.Xor_(const rect: TGPRectF): TStatus;
  begin
    result := SetStatus(GdipCombineRegionRect(nativeRegion, @rect, CombineModeXor));
  end;

  function TGPRegion.Xor_(path: TGPGraphicsPath): TStatus;
  begin
    result := SetStatus(GdipCombineRegionPath(nativeRegion, path.nativePath, CombineModeXor));
  end;

  function TGPRegion.Xor_(region: TGPRegion): TStatus;
  begin
    result := SetStatus(GdipCombineRegionRegion(nativeRegion, region.nativeRegion,
      CombineModeXor));
  end;

  function TGPRegion.Exclude(const rect: TGPRect): TStatus;
  begin
     result := SetStatus(GdipCombineRegionRectI(nativeRegion, @rect, CombineModeExclude));
  end;

  function TGPRegion.Exclude(const rect: TGPRectF): TStatus;
  begin
    result := SetStatus(GdipCombineRegionRect(nativeRegion, @rect, CombineModeExclude));
  end;

  function TGPRegion.Exclude(path: TGPGraphicsPath): TStatus;
  begin
    result := SetStatus(GdipCombineRegionPath(nativeRegion, path.nativePath, CombineModeExclude));
  end;

  function TGPRegion.Exclude(region: TGPRegion): TStatus;
  begin
    result := SetStatus(GdipCombineRegionRegion(nativeRegion,
                                               region.nativeRegion,
                                                         CombineModeExclude));
  end;

  function TGPRegion.Complement(const rect: TGPRect): TStatus;
  begin
    result := SetStatus(GdipCombineRegionRectI(nativeRegion, @rect,
                                                        CombineModeComplement));
  end;

  function TGPRegion.Complement(const rect: TGPRectF): TStatus;
  begin
    result := SetStatus(GdipCombineRegionRect(nativeRegion, @rect,
                                                       CombineModeComplement));
  end;

  function TGPRegion.Complement(path: TGPGraphicsPath): TStatus;
  begin
    result := SetStatus(GdipCombineRegionPath(nativeRegion,
                                                path.nativePath,
                                                CombineModeComplement));
  end;

  function TGPRegion.Complement(region: TGPRegion): TStatus;
  begin
    result := SetStatus(GdipCombineRegionRegion(nativeRegion,
                                                  region.nativeRegion,
                                                         CombineModeComplement));
  end;

  function TGPRegion.Translate(dx, dy: Single): TStatus;
  begin
    result := SetStatus(GdipTranslateRegion(nativeRegion, dx, dy));
  end;

  function TGPRegion.Translate(dx, dy: Integer): TStatus;
  begin
    result := SetStatus(GdipTranslateRegionI(nativeRegion, dx, dy));
  end;

  function TGPRegion.Transform(matrix: TGPMatrix): TStatus;
  begin
    result := SetStatus(GdipTransformRegion(nativeRegion,
                                                     matrix.nativeMatrix));
  end;

  function TGPRegion.GetBounds(out rect: TGPRect; g: TGPGraphics): TStatus;
  begin
    result := SetStatus(GdipGetRegionBoundsI(nativeRegion,
                                                g.nativeGraphics,
                                                @rect));
  end;

  function TGPRegion.GetBounds(out rect: TGPRectF; g: TGPGraphics): TStatus;
  begin
    result := SetStatus(GdipGetRegionBounds(nativeRegion,
                                                g.nativeGraphics,
                                                @rect));
  end;

  function TGPRegion.GetHRGN(g: TGPGraphics): HRGN;
  begin
    SetStatus(GdipGetRegionHRgn(nativeRegion, g.nativeGraphics, result));
  end;

  function TGPRegion.IsEmpty(g: TGPGraphics): BOOL;
  var booln: BOOL;
  begin
    booln := FALSE;
    SetStatus(GdipIsEmptyRegion(nativeRegion, g.nativeGraphics, booln));
    result := booln;
  end;

  function TGPRegion.IsInfinite(g: TGPGraphics): BOOL ;
  var booln: BOOL;
  begin
    booln := FALSE;
    SetStatus(GdipIsInfiniteRegion(nativeRegion, g.nativeGraphics, booln));
    result := booln;
  end;

  function TGPRegion.IsVisible(x, y: Integer; g: TGPGraphics = nil): BOOL;
  var
    booln: BOOL;
    GPX: GpGraphics;
  begin
    booln := FALSE;
    if assigned(g) then gpx := g.nativeGraphics else gpx := nil;
    SetStatus(GdipIsVisibleRegionPointI(nativeRegion, X, Y, gpx, booln));
    result := booln;
  end;

  function TGPRegion.IsVisible(const point: TGPPoint; g: TGPGraphics = nil): BOOL;
  var
    booln: BOOL;
    GPX: GpGraphics;
  begin
    booln := FALSE;
    if assigned(g) then gpx := g.nativeGraphics else gpx := nil;
    SetStatus(GdipIsVisibleRegionPointI(nativeRegion, point.X, point.Y, gpx, booln));
    result := booln;
  end;

  function TGPRegion.IsVisible(x, y: Single; g: TGPGraphics = nil): BOOL;
  var
    booln: BOOL;
    GPX: GpGraphics;
  begin
    booln := FALSE;
    if assigned(g) then gpx := g.nativeGraphics else gpx := nil;
    SetStatus(GdipIsVisibleRegionPoint(nativeRegion, X, Y, gpx, booln));
    result := booln;
  end;

  function TGPRegion.IsVisible(const point: TGPPointF; g: TGPGraphics = nil): BOOL;
  var
    booln: BOOL;
    GPX: GpGraphics;
  begin
    booln := FALSE;
    if assigned(g) then gpx := g.nativeGraphics else gpx := nil;
    SetStatus(GdipIsVisibleRegionPoint(nativeRegion, point.X, point.Y, gpx, booln));
    result := booln;
  end;

  function TGPRegion.IsVisible(x, y, width, height: Integer; g: TGPGraphics): BOOL;
  var
    booln: BOOL;
    GPX: GpGraphics;
  begin
    booln := FALSE;
    if assigned(g) then gpx := g.nativeGraphics else gpx := nil;
    SetStatus(GdipIsVisibleRegionRectI(nativeRegion,
                                                  X,
                                                  Y,
                                                  Width,
                                                  Height,
                                                  gpx,
                                                  booln));
    result := booln;
  end;

  function TGPRegion.IsVisible(const rect: TGPRect; g: TGPGraphics = nil): BOOL;
  var
    booln: BOOL;
    GPX: GpGraphics;
  begin
    booln := FALSE;
    if assigned(g) then gpx := g.nativeGraphics else gpx := nil;
    SetStatus(GdipIsVisibleRegionRectI(nativeRegion,
                                                  rect.X,
                                                  rect.Y,
                                                  rect.Width,
                                                  rect.Height,
                                                  gpx,
                                                  booln));
    result := booln;
  end;

  function TGPRegion.IsVisible(x, y, width, height: Single; g: TGPGraphics = nil): BOOL;
  var
    booln: BOOL;
    GPX: GpGraphics;
  begin
    booln := FALSE;
    if assigned(g) then gpx := g.nativeGraphics else gpx := nil;
    SetStatus(GdipIsVisibleRegionRect(nativeRegion, X,
                                                    Y, Width,
                                                    Height,
                                                    gpx,
                                                    booln));
    result := booln;
  end;

  function TGPRegion.IsVisible(const rect: TGPRectF; g: TGPGraphics = nil): BOOL;
  var
    booln: BOOL;
    GPX: GpGraphics;
  begin
    booln := FALSE;
    if assigned(g) then gpx := g.nativeGraphics else gpx := nil;
    SetStatus(GdipIsVisibleRegionRect(nativeRegion, rect.X,
                                                    rect.Y, rect.Width,
                                                    rect.Height,
                                                    gpx,
                                                    booln));
    result := booln;
  end;

  function TGPRegion.Equals(region: TGPRegion; g: TGPGraphics): BOOL;
  var
    booln: BOOL;
  begin
    booln := FALSE;
    SetStatus(GdipIsEqualRegion(nativeRegion,
                                              region.nativeRegion,
                                              g.nativeGraphics,
                                              booln));
    result := booln;
  end;

  function TGPRegion.GetRegionScansCount(matrix: TGPMatrix): UINT;
  var Count: UINT;
  begin
    count := 0;
    SetStatus(GdipGetRegionScansCount(nativeRegion, count, matrix.nativeMatrix));
    result := count;
  end;

  function TGPRegion.GetRegionScans(matrix: TGPMatrix; rects: PGPRectF; out count: Integer): TStatus;
  begin
    result := SetStatus(GdipGetRegionScans(nativeRegion,
                                          rects,
                                          count,
                                          matrix.nativeMatrix));
  end;

  function TGPRegion.GetRegionScans(matrix: TGPMatrix; rects: PGPRect; out count: Integer): TStatus;
  begin
    result := SetStatus(GdipGetRegionScansI(nativeRegion,
                                          rects,
                                          count,
                                          matrix.nativeMatrix));
  end;

  function TGPRegion.GetLastStatus: TStatus;
  begin
    result := lastResult;
    lastResult := Ok;
  end;

  function TGPRegion.SetStatus(status: TStatus): TStatus;
  begin
    if (status <> Ok) then lastResult := status;
    result := status;
  end;

  constructor TGPRegion.Create(nativeRegion: GpRegion);
  begin
    SetNativeRegion(nativeRegion);
  end;

  procedure TGPRegion.SetNativeRegion(nativeRegion: GpRegion);
  begin
    self.nativeRegion := nativeRegion;
  end;

  constructor TGPCustomLineCap.Create(fillPath, strokePath: TGPGraphicsPath;
                  baseCap: TLineCap = LineCapFlat; baseInset: Single = 0);
  var nativeFillPath, nativeStrokePath: GpPath;
  begin
    nativeCap := nil;
    nativeFillPath := nil;
    nativeStrokePath := nil;

    if assigned(fillPath) then nativeFillPath := fillPath.nativePath;
    if assigned(strokePath) then nativeStrokePath := strokePath.nativePath;

    lastResult := GdipCreateCustomLineCap(nativeFillPath, nativeStrokePath,
                    baseCap, baseInset, nativeCap);
  end;

  destructor TGPCustomLineCap.Destroy;
  begin
    GdipDeleteCustomLineCap(nativeCap);
  end;

  function TGPCustomLineCap.Clone: TGPCustomLineCap;
  var newNativeLineCap: GpCustomLineCap;
  begin
    newNativeLineCap := nil;
    SetStatus(GdipCloneCustomLineCap(nativeCap, newNativeLineCap));
    if (lastResult = Ok) then
    begin
      result := TGPCustomLineCap.Create(newNativeLineCap, lastResult);
      if (result = nil) then
         SetStatus(GdipDeleteCustomLineCap(newNativeLineCap));
    end
    else
      result := nil;
  end;

  // This changes both the start and end cap.
  function TGPCustomLineCap.SetStrokeCap(strokeCap: TLineCap): TStatus;
  begin
    result := SetStrokeCaps(strokeCap, strokeCap);
  end;

  function TGPCustomLineCap.SetStrokeCaps(startCap, endCap: TLineCap): TStatus;
  begin
    result := SetStatus(GdipSetCustomLineCapStrokeCaps(nativeCap, startCap, endCap));
  end;

  function TGPCustomLineCap.GetStrokeCaps(out startCap, endCap: TLineCap): TStatus;
  begin
    result := SetStatus(GdipGetCustomLineCapStrokeCaps(nativeCap, startCap, endCap));
  end;

  function TGPCustomLineCap.SetStrokeJoin(lineJoin: TLineJoin): TStatus;
  begin
    result := SetStatus(GdipSetCustomLineCapStrokeJoin(nativeCap, lineJoin));
  end;

  function TGPCustomLineCap.GetStrokeJoin: TLineJoin;
  begin
    SetStatus(GdipGetCustomLineCapStrokeJoin(nativeCap, result));
  end;

  function TGPCustomLineCap.SetBaseCap(baseCap: TLineCap): TStatus;
  begin
    result := SetStatus(GdipSetCustomLineCapBaseCap(nativeCap, baseCap));
  end;

  function TGPCustomLineCap.GetBaseCap: TLineCap;
  begin
    SetStatus(GdipGetCustomLineCapBaseCap(nativeCap, result));
  end;

  function TGPCustomLineCap.SetBaseInset(inset: Single): TStatus;
  begin
    result := SetStatus(GdipSetCustomLineCapBaseInset(nativeCap, inset));
  end;

  function TGPCustomLineCap.GetBaseInset: Single;
  begin
    SetStatus(GdipGetCustomLineCapBaseInset(nativeCap, result));
  end;

  function TGPCustomLineCap.SetWidthScale(widthScale: Single): TStatus;
  begin
    result := SetStatus(GdipSetCustomLineCapWidthScale(nativeCap, widthScale));
  end;

  function TGPCustomLineCap.GetWidthScale: Single;
  begin
    SetStatus(GdipGetCustomLineCapWidthScale(nativeCap, result));
  end;

  function TGPCustomLineCap.GetLastStatus: TStatus;
  begin
    result := lastResult;
    lastResult := Ok;
  end;

  constructor TGPCustomLineCap.Create;
  begin
    nativeCap := nil;
    lastResult := Ok;
  end;

  constructor TGPCustomLineCap.Create(nativeCap: GpCustomLineCap; status: TStatus);
  begin
    lastResult := status;
    SetNativeCap(nativeCap);
  end;

  procedure TGPCustomLineCap.SetNativeCap(nativeCap: GpCustomLineCap);
  begin
    self.nativeCap := nativeCap;
  end;

  function TGPCustomLineCap.SetStatus(status: TStatus): TStatus;
  begin
    if (status <> Ok) then lastResult := status;
    result := status;
  end;

  constructor TGPCachedBitmap.Create(bitmap: TGPBitmap; graphics: TGPGraphics);
  begin
    nativeCachedBitmap := nil;
    lastResult := GdipCreateCachedBitmap(
        GpBitmap(bitmap.nativeImage),
        graphics.nativeGraphics,
        nativeCachedBitmap);
  end;

  destructor TGPCachedBitmap.Destroy;
  begin
    GdipDeleteCachedBitmap(nativeCachedBitmap);
  end;

  function TGPCachedBitmap.GetLastStatus: TStatus;
  begin
    result := lastResult;
    lastResult := Ok;
  end;

  //--------------------------------------------------------------------------
  // Pen class
  //--------------------------------------------------------------------------

  constructor TGPPen.Create(color: TGPColor; width: Single = 1.0);
  var unit_: TUnit;
  begin
    unit_ := UnitWorld;
    nativePen := nil;
    lastResult := GdipCreatePen1(color, width, unit_, nativePen);
  end;

  constructor TGPPen.Create(brush: TGPBrush; width: Single = 1.0);
  var unit_: TUnit;
  begin
    unit_ := UnitWorld;
    nativePen := nil;
    lastResult := GdipCreatePen2(brush.nativeBrush, width, unit_, nativePen);
  end;

  destructor TGPPen.Destroy;
  begin
    GdipDeletePen(nativePen);
  end;

  function TGPPen.Clone: TGPPen;
  var clonePen: GpPen;
  begin
    clonePen := nil;
    lastResult := GdipClonePen(nativePen, clonePen);
    result := TGPPen.Create(clonePen, lastResult);
  end;

  function TGPPen.SetWidth(width: Single): TStatus;
  begin
    result := SetStatus(GdipSetPenWidth(nativePen, width));
  end;

  function TGPPen.GetWidth: Single;
  begin
    SetStatus(GdipGetPenWidth(nativePen, result));
  end;
    
  // Set/get line caps: start, end, and dash
  // Line cap and join APIs by using LineCap and LineJoin enums.

  function TGPPen.SetLineCap(startCap, endCap: TLineCap; dashCap: TDashCap): TStatus;
  begin
    result := SetStatus(GdipSetPenLineCap197819(nativePen, startCap, endCap, dashCap));
  end;

  function TGPPen.SetStartCap(startCap: TLineCap): TStatus;
  begin
    result := SetStatus(GdipSetPenStartCap(nativePen, startCap));
  end;

  function TGPPen.SetEndCap(endCap: TLineCap): TStatus;
  begin
    result := SetStatus(GdipSetPenEndCap(nativePen, endCap));
  end;

  function TGPPen.SetDashCap(dashCap: TDashCap): TStatus;
  begin
    result := SetStatus(GdipSetPenDashCap197819(nativePen, dashCap));
  end;

  function TGPPen.GetStartCap: TLineCap;
  begin
    SetStatus(GdipGetPenStartCap(nativePen, result));
  end;

  function TGPPen.GetEndCap: TLineCap;
  begin
    SetStatus(GdipGetPenEndCap(nativePen, result));
  end;

  function TGPPen.GetDashCap: TDashCap;
  begin
    SetStatus(GdipGetPenDashCap197819(nativePen, result));
  end;

  function TGPPen.SetLineJoin(lineJoin: TLineJoin): TStatus;
  begin
    result := SetStatus(GdipSetPenLineJoin(nativePen, lineJoin));
  end;

  function TGPPen.GetLineJoin: TLineJoin;
  begin
    SetStatus(GdipGetPenLineJoin(nativePen, result));
  end;

  function TGPPen.SetCustomStartCap(customCap: TGPCustomLineCap): TStatus;
  var nativeCap: GpCustomLineCap;
  begin
    nativeCap := nil;
    if assigned(customCap) then nativeCap := customCap.nativeCap;
    result := SetStatus(GdipSetPenCustomStartCap(nativePen, nativeCap));
  end;

  function TGPPen.GetCustomStartCap(customCap: TGPCustomLineCap): TStatus;
  begin
    if(customCap = nil) then
      result := SetStatus(InvalidParameter)
    else
      result := SetStatus(GdipGetPenCustomStartCap(nativePen, customCap.nativeCap));
  end;

  function TGPPen.SetCustomEndCap(customCap: TGPCustomLineCap): TStatus;
  var nativeCap: GpCustomLineCap;
  begin
    nativeCap := nil;
    if assigned(customCap) then nativeCap := customCap.nativeCap;
    result := SetStatus(GdipSetPenCustomEndCap(nativePen, nativeCap));
  end;

  function TGPPen.GetCustomEndCap(customCap: TGPCustomLineCap): TStatus;
  begin
    if(customCap = nil) then
      result := SetStatus(InvalidParameter)
    else
      result := SetStatus(GdipGetPenCustomEndCap(nativePen, customCap.nativeCap));
  end;

  function TGPPen.SetMiterLimit(miterLimit: Single): TStatus;
  begin
    result := SetStatus(GdipSetPenMiterLimit(nativePen, miterLimit));
  end;

  function TGPPen.GetMiterLimit: Single;
  begin
    SetStatus(GdipGetPenMiterLimit(nativePen, result));
  end;

  function TGPPen.SetAlignment(penAlignment: TPenAlignment): TStatus;
  begin
    result := SetStatus(GdipSetPenMode(nativePen, penAlignment));
  end;

  function TGPPen.GetAlignment: TPenAlignment;
  begin
    SetStatus(GdipGetPenMode(nativePen, result));
  end;

  function TGPPen.SetTransform(matrix: TGPMatrix): TStatus;
  begin
    result := SetStatus(GdipSetPenTransform(nativePen, matrix.nativeMatrix));
  end;

  function TGPPen.GetTransform(matrix: TGPMatrix): TStatus;
  begin
    result := SetStatus(GdipGetPenTransform(nativePen, matrix.nativeMatrix));
  end;

  function TGPPen.ResetTransform: TStatus;
  begin
    result := SetStatus(GdipResetPenTransform(nativePen));
  end;

  function TGPPen.MultiplyTransform(matrix: TGPMatrix; order: TMatrixOrder = MatrixOrderPrepend): TStatus;
  begin
    result := SetStatus(GdipMultiplyPenTransform(nativePen, matrix.nativeMatrix, order));
  end;

  function TGPPen.TranslateTransform(dx, dy: Single; order: TMatrixOrder = MatrixOrderPrepend): TStatus;
  begin
    result := SetStatus(GdipTranslatePenTransform(nativePen, dx, dy, order));
  end;

  function TGPPen.ScaleTransform(sx, sy: Single; order: TMatrixOrder = MatrixOrderPrepend): TStatus;
  begin
    result := SetStatus(GdipScalePenTransform(nativePen, sx, sy, order));
  end;

  function TGPPen.RotateTransform(angle: Single; order: TMatrixOrder = MatrixOrderPrepend): TStatus;
  begin
    result := SetStatus(GdipRotatePenTransform(nativePen, angle, order));
  end;

  function TGPPen.GetPenType: TPenType;
  begin
    SetStatus(GdipGetPenFillType(nativePen, result));
  end;

  function TGPPen.SetColor(color: TGPColor): TStatus;
  begin
    result := SetStatus(GdipSetPenColor(nativePen, color));
  end;

  function TGPPen.SetBrush(brush: TGPBrush): TStatus;
  begin
    result := SetStatus(GdipSetPenBrushFill(nativePen, brush.nativeBrush));
  end;

  function TGPPen.GetColor(out Color: TGPColor): TStatus;
  var
    type_: TPenType;
    argb: DWORD;
  begin
    type_ := GetPenType;

    if (type_ <> PenTypeSolidColor) then
    begin
      result := WrongState;
      exit;
    end;

    SetStatus(GdipGetPenColor(nativePen, argb));
    if (lastResult = Ok) then color := argb;
    result := lastResult;
  end;

  function TGPPen.GetBrush: TGPBrush;
  var
    type_: TPenType;
    Brush: TGPBrush;
    nativeBrush: GpBrush;
  begin
    type_ := GetPenType;
    brush := nil;

    case type_ of
       PenTypeSolidColor     : brush := TGPSolidBrush.Create;
       PenTypeHatchFill      : brush := TGPHatchBrush.Create;
       PenTypeTextureFill    : brush := TGPTextureBrush.Create;
       PenTypePathGradient   : brush := TGPBrush.Create;
       PenTypeLinearGradient : brush := TGPLinearGradientBrush.Create;
     end;

     if (brush <> nil) then
     begin
       SetStatus(GdipGetPenBrushFill(nativePen, nativeBrush));
       brush.SetNativeBrush(nativeBrush);
     end;

     result := brush;
  end;

  function TGPPen.GetDashStyle: TDashStyle;
  begin
    SetStatus(GdipGetPenDashStyle(nativePen, result));
  end;

  function TGPPen.SetDashStyle(dashStyle: TDashStyle): TStatus;
  begin
    result := SetStatus(GdipSetPenDashStyle(nativePen, dashStyle));
  end;

  function TGPPen.GetDashOffset: Single;
  begin
    SetStatus(GdipGetPenDashOffset(nativePen, result));
  end;

  function TGPPen.SetDashOffset(dashOffset: Single): TStatus;
  begin
    result := SetStatus(GdipSetPenDashOffset(nativePen, dashOffset));
  end;
    
  function TGPPen.SetDashPattern(dashArray: PSingle; count: Integer): TStatus;
  begin
    result := SetStatus(GdipSetPenDashArray(nativePen, dashArray, count));
  end;

  function TGPPen.GetDashPatternCount: Integer;
  begin
    SetStatus(GdipGetPenDashCount(nativePen, result));
  end;

  function TGPPen.GetDashPattern(dashArray: PSingle; count: Integer): TStatus;
  begin
    result := SetStatus(GdipGetPenDashArray(nativePen, dashArray, count));
  end;

  function TGPPen.SetCompoundArray(compoundArray: PSingle; count: Integer): TStatus;
  begin
    result := SetStatus(GdipSetPenCompoundArray(nativePen, compoundArray, count));
  end;

  function TGPPen.GetCompoundArrayCount: Integer;
  begin
    SetStatus(GdipGetPenCompoundCount(nativePen, result));
  end;

  function TGPPen.GetCompoundArray(compoundArray: PSingle; count: Integer): TStatus;
  begin
    if (count <= 0) then
      result := SetStatus(InvalidParameter)
    else
      result := SetStatus(GdipGetPenCompoundArray(nativePen, compoundArray, count));
  end;

  function TGPPen.GetLastStatus: TStatus;
  begin
    result := lastResult;
    lastResult := Ok;
  end;

  constructor TGPPen.Create(nativePen: GpPen; status: TStatus);
  begin
    lastResult := status;
    SetNativePen(nativePen);
  end;

  procedure TGPPen.SetNativePen(nativePen: GpPen);
  begin
    self.nativePen := nativePen;
  end;

  function TGPPen.SetStatus(status: TStatus): TStatus;
  begin
    if (status <> Ok) then lastResult := status;
    result := status;
  end;

//--------------------------------------------------------------------------
// Abstract base class for various brush types
//--------------------------------------------------------------------------

  destructor TGPBrush.Destroy;
  begin
    GdipDeleteBrush(nativeBrush);
  end;

  function TGPBrush.Clone: TGPBrush;
  var
    brush: GpBrush;
    newBrush: TGPBrush;
  begin
    brush := nil;
    SetStatus(GdipCloneBrush(nativeBrush, brush));
    newBrush := TGPBrush.Create(brush, lastResult);
    if (newBrush = nil) then
      GdipDeleteBrush(brush);
    result := newBrush;
  end;

  function TGPBrush.GetType: TBrushType;
  var type_: TBrushType;
  begin
    type_ := TBrushType(-1);
    SetStatus(GdipGetBrushType(nativeBrush, type_));
    result := type_;
  end;

  function TGPBrush.GetLastStatus: TStatus;
  begin
    result := lastResult;
    lastResult := Ok;
  end;

  constructor TGPBrush.Create;
  begin
    SetStatus(NotImplemented);
  end;

  constructor TGPBrush.Create(nativeBrush: GpBrush; status: TStatus);
  begin
    lastResult := status;
    SetNativeBrush(nativeBrush);
  end;

  procedure TGPBrush.SetNativeBrush(nativeBrush: GpBrush);
  begin
    self.nativeBrush := nativeBrush;
  end;

  function TGPBrush.SetStatus(status: TStatus): TStatus;
  begin
    if (status <> Ok) then lastResult := status;
    result := status;
  end;

//--------------------------------------------------------------------------
// Solid Fill Brush Object
//--------------------------------------------------------------------------

  constructor TGPSolidBrush.Create(color: TGPColor);
  var
    brush: GpSolidFill;
  begin
    brush := nil;
    lastResult := GdipCreateSolidFill(color, brush);
    SetNativeBrush(brush);
  end;

  function TGPSolidBrush.GetColor(out color: TGPColor): TStatus;
  begin
    SetStatus(GdipGetSolidFillColor(GPSOLIDFILL(nativeBrush), color));
    result := lastResult;
  end;

  function TGPSolidBrush.SetColor(color: TGPColor): TStatus;
  begin
    result := SetStatus(GdipSetSolidFillColor(GpSolidFill(nativeBrush),
                color));
  end;

  constructor TGPSolidBrush.Create;
  begin
    // hide parent function
  end;

//--------------------------------------------------------------------------
// Texture Brush Fill Object
//--------------------------------------------------------------------------

  constructor TGPTextureBrush.Create(image: TGPImage; wrapMode: TWrapMode = WrapModeTile);
  var texture: GpTexture;
  begin
    //texture := nil;
    lastResult := GdipCreateTexture(image.nativeImage, wrapMode, texture);
    SetNativeBrush(texture);
  end;

  constructor TGPTextureBrush.Create(image: TGPImage; wrapMode: TWrapMode; dstRect: TGPRectF);
  var texture: GpTexture;
  begin
    texture := nil;
    lastResult := GdipCreateTexture2(image.nativeImage, wrapMode, dstRect.X,
                    dstRect.Y, dstRect.Width, dstRect.Height, texture);
    SetNativeBrush(texture);
  end;

  constructor TGPTextureBrush.Create(image: TGPImage; dstRect: TGPRectF; imageAttributes: TGPImageAttributes = nil);
  var texture: GpTexture;
      ImgAtt: GpImageAttributes;
  begin
    texture := nil;
    if assigned(imageAttributes) then ImgAtt := imageAttributes.nativeImageAttr
                                 else ImgAtt := nil;

    lastResult := GdipCreateTextureIA(image.nativeImage, ImgAtt, dstRect.X,
                    dstRect.Y, dstRect.Width, dstRect.Height, texture);
    SetNativeBrush(texture);
  end;

  constructor TGPTextureBrush.Create(image: TGPImage; dstRect: TGPRect; imageAttributes: TGPImageAttributes = nil);
  var texture: GpTexture;
      ImgAtt: GpImageAttributes;
  begin
    texture := nil;
    if assigned(imageAttributes) then ImgAtt := imageAttributes.nativeImageAttr
                                 else ImgAtt := nil;
    lastResult := GdipCreateTextureIAI(image.nativeImage, ImgAtt, dstRect.X,
                    dstRect.Y, dstRect.Width, dstRect.Height, texture);
     SetNativeBrush(texture);
  end;

  constructor TGPTextureBrush.Create(image: TGPImage; wrapMode: TWrapMode; dstRect: TGPRect);
  var texture: GpTexture;
  begin
    texture := nil;
    lastResult := GdipCreateTexture2I(image.nativeImage, wrapMode, dstRect.X,
                    dstRect.Y, dstRect.Width, dstRect.Height, texture);
    SetNativeBrush(texture);
  end;

  constructor TGPTextureBrush.Create(image: TGPImage; wrapMode: TWrapMode; dstX, dstY, dstWidth, dstHeight: Single);
  var texture: GpTexture;
  begin
    texture := nil;
    lastResult := GdipCreateTexture2(image.nativeImage, wrapMode, dstX, dstY,
                    dstWidth, dstHeight, texture);
    SetNativeBrush(texture);
  end;

  constructor TGPTextureBrush.Create(image: TGPImage; wrapMode: TWrapMode; dstX, dstY, dstWidth, dstHeight: Integer);
  var texture: GpTexture;
  begin
    texture := nil;
    lastResult := GdipCreateTexture2I(image.nativeImage, wrapMode, dstX, dstY,
                    dstWidth, dstHeight, texture);
    SetNativeBrush(texture);
  end;

  function TGPTextureBrush.SetTransform(matrix: TGPMatrix): TStatus;
  begin
    result := SetStatus(GdipSetTextureTransform(GpTexture(nativeBrush),
                matrix.nativeMatrix));
  end;

  function TGPTextureBrush.GetTransform(matrix: TGPMatrix): TStatus;
  begin
    result := SetStatus(GdipGetTextureTransform(GpTexture(nativeBrush),
                matrix.nativeMatrix));
  end;

  function TGPTextureBrush.ResetTransform: TStatus;
  begin
    result := SetStatus(GdipResetTextureTransform(GpTexture(nativeBrush)));
  end;

  function TGPTextureBrush.MultiplyTransform(matrix: TGPMatrix; order: TMatrixOrder = MatrixOrderPrepend): TStatus;
  begin
    result := SetStatus(GdipMultiplyTextureTransform(GpTexture(nativeBrush),
                matrix.nativeMatrix, order));
  end;

  function TGPTextureBrush.TranslateTransform(dx, dy: Single; order: MatrixOrder = MatrixOrderPrepend): TStatus;
  begin
    result := SetStatus(GdipTranslateTextureTransform(GpTexture(nativeBrush),
                dx, dy, order));
  end;

  function TGPTextureBrush.ScaleTransform(sx, sy: Single; order: TMatrixOrder = MatrixOrderPrepend): TStatus;
  begin
    result := SetStatus(GdipScaleTextureTransform(GpTexture(nativeBrush),
                 sx, sy, order));
  end;

  function TGPTextureBrush.RotateTransform(angle: Single; order: TMatrixOrder = MatrixOrderPrepend): TStatus;
  begin
    result := SetStatus(GdipRotateTextureTransform(GpTexture(nativeBrush),
                                                              angle, order));
  end;

  function TGPTextureBrush.SetWrapMode(wrapMode: TWrapMode): TStatus;
  begin
    result := SetStatus(GdipSetTextureWrapMode(GpTexture(nativeBrush), wrapMode));
  end;

  function TGPTextureBrush.GetWrapMode: TWrapMode;
  begin
    SetStatus(GdipGetTextureWrapMode(GpTexture(nativeBrush), result));
  end;

  function TGPTextureBrush.GetImage: TGPImage;
  var image: GpImage;
  begin
    SetStatus(GdipGetTextureImage(GpTexture(nativeBrush), image));
    result := TGPImage.Create(image, lastResult);
    if (result = nil) then
      GdipDisposeImage(image);
  end;

  constructor TGPTextureBrush.Create;
  begin
    // hide parent function
  end;

//--------------------------------------------------------------------------
// Linear Gradient Brush Object
//--------------------------------------------------------------------------

  constructor TGPLinearGradientBrush.Create(const point1, point2: TGPPointF; color1, color2: TGPColor);
  var brush: GpLineGradient;
  begin
    brush := nil;
    lastResult := GdipCreateLineBrush(@point1, @point2, color1, color2, WrapModeTile, brush);
    SetNativeBrush(brush);
  end;

  constructor TGPLinearGradientBrush.Create(const point1, point2: TGPPoint; color1, color2: TGPColor);
  var brush: GpLineGradient;
  begin
    brush := nil;
    lastResult := GdipCreateLineBrushI(@point1, @point2, color1,
                    color2, WrapModeTile, brush);
    SetNativeBrush(brush);
  end;

  constructor TGPLinearGradientBrush.Create(rect: TGPRectF; color1, color2: TGPColor; mode: TLinearGradientMode);
  var brush: GpLineGradient;
  begin
    brush := nil;
    lastResult := GdipCreateLineBrushFromRect(@rect, color1,
                    color2, mode, WrapModeTile, brush);
    SetNativeBrush(brush);
  end;

  constructor TGPLinearGradientBrush.Create(rect: TGPRect; color1, color2: TGPColor; mode: TLinearGradientMode);
  var brush: GpLineGradient;
  begin
    brush := nil;
    lastResult := GdipCreateLineBrushFromRectI(@rect, color1,
                    color2, mode, WrapModeTile, brush);
    SetNativeBrush(brush);
  end;

  constructor TGPLinearGradientBrush.Create(rect: TGPRectF; color1, color2: TGPColor; angle: Single; isAngleScalable: BOOL = FALSE);
  var brush: GpLineGradient;
  begin
    brush := nil;
    lastResult := GdipCreateLineBrushFromRectWithAngle(@rect, color1,
                    color2, angle, isAngleScalable, WrapModeTile, brush);
    SetNativeBrush(brush);
  end;

  constructor TGPLinearGradientBrush.Create(rect: TGPRect; color1, color2: TGPColor; angle: Single; isAngleScalable: BOOL = FALSE);
  var brush: GpLineGradient;
  begin
    brush := nil;
    lastResult := GdipCreateLineBrushFromRectWithAngleI(@rect, color1,
                    color2, angle, isAngleScalable, WrapModeTile, brush);
    SetNativeBrush(brush);
  end;

  function TGPLinearGradientBrush.SetLinearColors(color1, color2: TGPColor): TStatus;
  begin
    result := SetStatus(GdipSetLineColors(GpLineGradient(nativeBrush),
                color1, color2));
  end;

  function TGPLinearGradientBrush.GetLinearColors(out color1, color2: TGPColor): TStatus;
  var colors: array[0..1] of TGPColor;
  begin
    SetStatus(GdipGetLineColors(GpLineGradient(nativeBrush), @colors));
    if (lastResult = Ok) then
    begin
      color1 := colors[0];
      color2 := colors[1];
    end;
    result := lastResult;
  end;

  function TGPLinearGradientBrush.GetRectangle(out rect: TGPRectF): TStatus;
  begin
    result := SetStatus(GdipGetLineRect(GpLineGradient(nativeBrush), @rect));
  end;

  function TGPLinearGradientBrush.GetRectangle(out rect: TGPRect): TStatus;
  begin
    result := SetStatus(GdipGetLineRectI(GpLineGradient(nativeBrush), @rect));
  end;

  function TGPLinearGradientBrush.SetGammaCorrection(useGammaCorrection: BOOL): TStatus;
  begin
    result := SetStatus(GdipSetLineGammaCorrection(GpLineGradient(nativeBrush),
                useGammaCorrection));
  end;
    
  function TGPLinearGradientBrush.GetGammaCorrection: BOOL;
  var useGammaCorrection: BOOL;
  begin
    SetStatus(GdipGetLineGammaCorrection(GpLineGradient(nativeBrush),
                useGammaCorrection));
    result := useGammaCorrection;
  end;

  function TGPLinearGradientBrush.GetBlendCount: Integer;
  var count: Integer;
  begin
    count := 0;
    SetStatus(GdipGetLineBlendCount(GpLineGradient(nativeBrush), count));
    result := count;
  end;

  function TGPLinearGradientBrush.SetBlend(blendFactors, blendPositions: PSingle; count: Integer): TStatus;
  begin
    result := SetStatus(GdipSetLineBlend(GpLineGradient(nativeBrush),
                          blendFactors, blendPositions, count));
  end;

  function TGPLinearGradientBrush.GetBlend(blendFactors, blendPositions: PSingle; count: Integer): TStatus;
  begin
    if ((count <= 0) or (blendFactors = nil) or (blendPositions = nil)) then
      result := SetStatus(InvalidParameter)
    else
      result := SetStatus(GdipGetLineBlend(GpLineGradient(nativeBrush), blendFactors,
        blendPositions, count));
  end;

  function TGPLinearGradientBrush.GetInterpolationColorCount: Integer;
  var count: Integer;
  begin
    count := 0;
    SetStatus(GdipGetLinePresetBlendCount(GpLineGradient(nativeBrush), count));
    result := count;
  end;

  function TGPLinearGradientBrush.SetInterpolationColors(presetColors: PGPColor;
    blendPositions: PSingle; count: Integer): TStatus;
  begin
    if (count <= 0)  then
      result := SetStatus(InvalidParameter)
    else
      result := SetStatus(GdipSetLinePresetBlend(GpLineGradient(nativeBrush),
        PARGB(presetColors), blendPositions, count));
  end;

  function TGPLinearGradientBrush.GetInterpolationColors(presetColors: PGPColor; blendPositions: PSingle; count: Integer): TStatus;
  begin
    if (count <= 0) then
      result := SetStatus(InvalidParameter)
    else
      result := SetStatus(GdipGetLinePresetBlend(GpLineGradient(nativeBrush),
                          PARGB(presetColors), blendPositions, count));
  end;

  function TGPLinearGradientBrush.SetBlendBellShape(focus: Single; scale: Single = 1.0): TStatus;
  begin
    result := SetStatus(GdipSetLineSigmaBlend(GpLineGradient(nativeBrush), focus, scale));
  end;

  function TGPLinearGradientBrush.SetBlendTriangularShape(focus: Single; scale: Single = 1.0): TStatus;
  begin
    result := SetStatus(GdipSetLineLinearBlend(GpLineGradient(nativeBrush), focus, scale));
  end;

  function TGPLinearGradientBrush.SetTransform(matrix: TGPMatrix): TStatus;
  begin
    result := SetStatus(GdipSetLineTransform(GpLineGradient(nativeBrush),
                                                          matrix.nativeMatrix));
  end;

  function TGPLinearGradientBrush.GetTransform(matrix: TGPMatrix): TStatus;
  begin
    result := SetStatus(GdipGetLineTransform(GpLineGradient(nativeBrush),
                                                     matrix.nativeMatrix));
  end;

  function TGPLinearGradientBrush.ResetTransform: TStatus;
  begin
    result := SetStatus(GdipResetLineTransform(GpLineGradient(nativeBrush)));
  end;

  function TGPLinearGradientBrush.MultiplyTransform(matrix: TGPMatrix; order: TMatrixOrder = MatrixOrderPrepend): TStatus;
  begin
    result := SetStatus(GdipMultiplyLineTransform(GpLineGradient(nativeBrush),
                                                                matrix.nativeMatrix,
                                                                order));
  end;

  function TGPLinearGradientBrush.TranslateTransform(dx, dy: Single; order: TMatrixOrder = MatrixOrderPrepend): TStatus;
  begin
    result := SetStatus(GdipTranslateLineTransform(GpLineGradient(nativeBrush),
                                                               dx, dy, order));
  end;

  function TGPLinearGradientBrush.ScaleTransform(sx, sy: Single; order: TMatrixOrder = MatrixOrderPrepend): TStatus;
  begin
    result := SetStatus(GdipScaleLineTransform(GpLineGradient(nativeBrush),
                                                             sx, sy, order));
  end;

  function TGPLinearGradientBrush.RotateTransform(angle: Single; order: TMatrixOrder = MatrixOrderPrepend): TStatus;
  begin
    result := SetStatus(GdipRotateLineTransform(GpLineGradient(nativeBrush),
                                                              angle, order));
  end;

  function TGPLinearGradientBrush.SetWrapMode(wrapMode: TWrapMode): TStatus;
  begin
    result := SetStatus(GdipSetLineWrapMode(GpLineGradient(nativeBrush), wrapMode));
  end;

  function TGPLinearGradientBrush.GetWrapMode: TWrapMode;
  begin
     SetStatus(GdipGetLineWrapMode(GpLineGradient(nativeBrush), result));
  end;

  constructor TGPLinearGradientBrush.Create;
  begin
    // hide parent function
  end;

//--------------------------------------------------------------------------
// Hatch Brush Object
//--------------------------------------------------------------------------

  constructor TGPHatchBrush.Create(hatchStyle: THatchStyle; foreColor: TGPColor; backColor: TGPColor = aclBlack);
  var
    brush: GpHatch;
  begin
    brush := nil;
    lastResult := GdipCreateHatchBrush(Integer(hatchStyle), foreColor, backColor, brush);
    SetNativeBrush(brush);
  end;

  function TGPHatchBrush.GetHatchStyle: THatchStyle;
  begin
    SetStatus(GdipGetHatchStyle(GpHatch(nativeBrush), result));
  end;

  function TGPHatchBrush.GetForegroundColor(out color: TGPColor): TStatus;
  begin
    result := SetStatus(GdipGetHatchForegroundColor(GpHatch(nativeBrush), color));
  end;

  function TGPHatchBrush.GetBackgroundColor(out color: TGPColor): TStatus;
  begin
    result := SetStatus(GdipGetHatchBackgroundColor(GpHatch(nativeBrush), color));
  end;

  constructor TGPHatchBrush.Create;
  begin
  end;

  constructor TGPImage.Create(filename: WideString;
                  useEmbeddedColorManagement: BOOL = FALSE);
  begin
    nativeImage := nil;
    if(useEmbeddedColorManagement) then
    begin
        lastResult := GdipLoadImageFromFileICM(
            PWideChar(filename),
            nativeImage
        );
    end
    else
    begin
        lastResult := GdipLoadImageFromFile(
            PWideChar(filename),
            nativeImage
        );
    end;
  end;

  constructor TGPImage.Create(stream: IStream;
                  useEmbeddedColorManagement: BOOL  = FALSE);
  begin
    nativeImage := nil;
    if(useEmbeddedColorManagement) then
         lastResult := GdipLoadImageFromStreamICM(stream, nativeImage)
    else lastResult := GdipLoadImageFromStream(stream, nativeImage);
  end;

  function TGPImage.FromFile(filename: WideString;
               useEmbeddedColorManagement: BOOL = FALSE): TGPImage;
  begin
    result := TGPImage.Create(
        PWideChar(filename),
        useEmbeddedColorManagement
    );
  end;

  function TGPImage.FromStream(stream: IStream;
               useEmbeddedColorManagement: BOOL = FALSE): TGPImage;
  begin
    result := TGPImage.Create(
        stream,
        useEmbeddedColorManagement
    );
  end;

  destructor TGPImage.destroy;
  begin
    GdipDisposeImage(nativeImage);
  end;

  function TGPImage.Clone: TGPImage;
  var cloneimage: GpImage;
  begin
    cloneimage := nil;
    SetStatus(GdipCloneImage(nativeImage, cloneimage));
    result := TGPImage.Create(cloneimage, lastResult);
  end;

  function TGPImage.Save(filename: WideString; const clsidEncoder: TGUID;
               encoderParams: PEncoderParameters = nil): TStatus;
  begin
    result := SetStatus(GdipSaveImageToFile(nativeImage,
                                                     PWideChar(filename),
                                                     @clsidEncoder,
                                                     encoderParams));
  end;

  function TGPImage.Save(stream: IStream; const clsidEncoder: TGUID;
               encoderParams: PEncoderParameters  = nil): TStatus;
  begin
    result := SetStatus(GdipSaveImageToStream(nativeImage,
                                                       stream,
                                                       @clsidEncoder,
                                                       encoderParams));
  end;

  function TGPImage.SaveAdd(encoderParams: PEncoderParameters): TStatus;
  begin
    result := SetStatus(GdipSaveAdd(nativeImage,
                                             encoderParams));
  end;

  function TGPImage.SaveAdd(newImage: TGPImage;
               encoderParams: PEncoderParameters): TStatus;
  begin
    if (newImage = nil) then
    begin
      result := SetStatus(InvalidParameter);
      exit;
    end;
    result := SetStatus(GdipSaveAddImage(nativeImage,
                                                  newImage.nativeImage,
                                                  encoderParams));
  end;

  function TGPImage.GetType: TImageType;
  begin
    SetStatus(GdipGetImageType(nativeImage, result));
  end;

  function TGPImage.GetPhysicalDimension(out size: TGPSizeF): TStatus;
  var
    width, height: Single;
    status: TStatus;
  begin
    status := SetStatus(GdipGetImageDimension(nativeImage, width, height));
    size.Width  := width;
    size.Height := height;
    result := status;
  end;

  function TGPImage.GetBounds(out srcRect: TGPRectF; out srcUnit: TUnit): TStatus;
  begin
    result := SetStatus(GdipGetImageBounds(nativeImage, @srcRect, srcUnit));
  end;

  function TGPImage.GetWidth: UINT;
  var width: UINT;
  begin
    width := 0;
    SetStatus(GdipGetImageWidth(nativeImage, width));
    result := width;
  end;

  function TGPImage.GetHeight: UINT;
  var height: UINT;
  begin
    height := 0;
    SetStatus(GdipGetImageHeight(nativeImage, height));
    result := height;
  end;

  function TGPImage.GetHorizontalResolution: Single;
  var resolution: Single;
  begin
    resolution := 0.0;
    SetStatus(GdipGetImageHorizontalResolution(nativeImage, resolution));
    result := resolution;
  end;

  function TGPImage.GetVerticalResolution: Single;
  var resolution: Single;
  begin
    resolution := 0.0;
    SetStatus(GdipGetImageVerticalResolution(nativeImage, resolution));
    result := resolution;
  end;

  function TGPImage.GetFlags: UINT;
  var flags: UINT;
  begin
    flags := 0;
    SetStatus(GdipGetImageFlags(nativeImage, flags));
    result := flags;
  end;

  function TGPImage.GetRawFormat(out format: TGUID): TStatus;
  begin
    result := SetStatus(GdipGetImageRawFormat(nativeImage, @format));
  end;

  function TGPImage.GetPixelFormat: TPixelFormat;
  begin
    SetStatus(GdipGetImagePixelFormat(nativeImage, result));
  end;

  function TGPImage.GetPaletteSize: Integer;
  var size: Integer;
  begin
    size := 0;
    SetStatus(GdipGetImagePaletteSize(nativeImage, size));
    result := size;
  end;

  function TGPImage.GetPalette(palette: PColorPalette; size: Integer): TStatus;
  begin
    result := SetStatus(GdipGetImagePalette(nativeImage, palette, size));
  end;

  function TGPImage.SetPalette(palette: PColorPalette): TStatus;
  begin
    result := SetStatus(GdipSetImagePalette(nativeImage, palette));
  end;

  function TGPImage.GetThumbnailImage(thumbWidth, thumbHeight: UINT;
                callback: GetThumbnailImageAbort = nil;
                callbackData: pointer = nil): TGPImage;
  var
    thumbimage: GpImage;
    newImage: TGPImage;
  begin
    thumbimage := nil;
    SetStatus(GdipGetImageThumbnail(nativeImage,
                                                thumbWidth, thumbHeight,
                                                thumbimage,
                                                callback, callbackData));

    newImage := TGPImage.Create(thumbimage, lastResult);
    if (newImage = nil) then
        GdipDisposeImage(thumbimage);

    result := newImage;
  end;

  function TGPImage.GetFrameDimensionsCount: UINT;
  var count: UINT;
  begin
    count := 0;
    SetStatus(GdipImageGetFrameDimensionsCount(nativeImage, count));
    result := count;
  end;

  function TGPImage.GetFrameDimensionsList(dimensionIDs: PGUID; count: UINT): TStatus;
  begin
    result := SetStatus(GdipImageGetFrameDimensionsList(nativeImage, dimensionIDs, count));
  end;

  function TGPImage.GetFrameCount(const dimensionID: TGUID): UINT;
  var count: UINT;
  begin
    count := 0;
    SetStatus(GdipImageGetFrameCount(nativeImage, @dimensionID, count));
    result := count;
  end;

  function TGPImage.SelectActiveFrame(const dimensionID: TGUID; frameIndex: UINT): TStatus;
  begin
    result := SetStatus(GdipImageSelectActiveFrame(nativeImage,
                                                            @dimensionID,
                                                            frameIndex));
  end;

  function TGPImage.RotateFlip(rotateFlipType: TRotateFlipType): TStatus;
  begin
    result := SetStatus(GdipImageRotateFlip(nativeImage,
                                                     rotateFlipType));
  end;

  function TGPImage.GetPropertyCount: UINT;
  var numProperty: UINT;
  begin
    numProperty := 0;
    SetStatus(GdipGetPropertyCount(nativeImage, numProperty));
    result := numProperty;
  end;

  function TGPImage.GetPropertyIdList(numOfProperty: UINT; list: PPropID): TStatus;
  begin
    result := SetStatus(GdipGetPropertyIdList(nativeImage, numOfProperty, list));
  end;

  function TGPImage.GetPropertyItemSize(propId: PROPID): UINT;
  var size: UINT;
  begin
    size := 0;
    SetStatus(GdipGetPropertyItemSize(nativeImage, propId, size));
    result := size;
  end;

  function TGPImage.GetPropertyItem(propId: PROPID; propSize: UINT;
               buffer: PPropertyItem): TStatus;
  begin
    result := SetStatus(GdipGetPropertyItem(nativeImage,
                                                     propId, propSize, buffer));
  end;

  function TGPImage.GetPropertySize(out totalBufferSize, numProperties : UINT): TStatus;
  begin
    result := SetStatus(GdipGetPropertySize(nativeImage,
                                                     totalBufferSize,
                                                     numProperties));
  end;

  function TGPImage.GetAllPropertyItems(totalBufferSize, numProperties: UINT;
               allItems: PPROPERTYITEM): TStatus;
  begin
    result := SetStatus(GdipGetAllPropertyItems(nativeImage,
                                                         totalBufferSize,
                                                         numProperties,
                                                         allItems));
  end;

  function TGPImage.RemovePropertyItem(propId: TPROPID): TStatus;
  begin
    result := SetStatus(GdipRemovePropertyItem(nativeImage, propId));
  end;

  function TGPImage.SetPropertyItem(const item: TPropertyItem): TStatus;
  begin
    result := SetStatus(GdipSetPropertyItem(nativeImage, @item));
  end;

  function TGPImage.GetEncoderParameterListSize(const clsidEncoder: TGUID): UINT;
  var size: UINT;
  begin
    size := 0;
    SetStatus(GdipGetEncoderParameterListSize(nativeImage, @clsidEncoder, size));
    result := size;
  end;

  function TGPImage.GetEncoderParameterList(const clsidEncoder: TGUID; size: UINT;
               buffer: PEncoderParameters): TStatus;
  begin
    result := SetStatus(GdipGetEncoderParameterList(nativeImage,
                                                             @clsidEncoder,
                                                             size,
                                                             buffer));
  end;

  function TGPImage.GetLastStatus: TStatus;
  begin
    result := lastResult;
    lastResult := Ok;
  end;

  constructor TGPImage.Create(nativeImage: GpImage; status: TStatus);
  begin
    SetNativeImage(nativeImage);
    lastResult := status;
  end;

  procedure TGPImage.SetNativeImage(nativeImage: GpImage);
  begin
    self.nativeImage := nativeImage;
  end;

  function TGPImage.SetStatus(status: TStatus): TStatus;
  begin
    if (status <> Ok) then lastResult := status;
    result := status;
  end;

  // TGPBitmap

  constructor TGPBitmap.Create(filename: WideString; useEmbeddedColorManagement: BOOL = FALSE);
  var bitmap: GpBitmap;
  begin
    bitmap := nil;
    if(useEmbeddedColorManagement) then
      lastResult := GdipCreateBitmapFromFileICM(PWideChar(filename), bitmap)
    else
        lastResult := GdipCreateBitmapFromFile(PWideChar(filename), bitmap);
    SetNativeImage(bitmap);
  end;

  constructor TGPBitmap.Create(stream: IStream; useEmbeddedColorManagement: BOOL = FALSE);
  var bitmap: GpBitmap;
  begin
    bitmap := nil;
    if(useEmbeddedColorManagement) then
      lastResult := GdipCreateBitmapFromStreamICM(stream, bitmap)
    else
        lastResult := GdipCreateBitmapFromStream(stream, bitmap);
    SetNativeImage(bitmap);
  end;

  function TGPBitmap.FromFile(filename: WideString; useEmbeddedColorManagement: BOOL = FALSE): TGPBitmap;
  begin
    result := TGPBitmap.Create(
        PWideChar(filename),
        useEmbeddedColorManagement
    );
  end;

  function TGPBitmap.FromStream(stream: IStream; useEmbeddedColorManagement: BOOL = FALSE): TGPBitmap;
  begin
    result := TGPBitmap.Create(
        stream,
        useEmbeddedColorManagement
    );
  end;

  constructor TGPBitmap.Create(width, height, stride: Integer; format: TPixelFormat; scan0: PBYTE);
  var bitmap: GpBitmap;
  begin
    bitmap := nil;
    lastResult := GdipCreateBitmapFromScan0(width,
                                                       height,
                                                       stride,
                                                       format,
                                                       scan0,
                                                       bitmap);

    SetNativeImage(bitmap);
  end;

  constructor TGPBitmap.Create(width, height: Integer; format: TPixelFormat = PixelFormat32bppARGB);
  var bitmap: GpBitmap;
  begin
    bitmap := nil;
    lastResult := GdipCreateBitmapFromScan0(width,
                                                       height,
                                                       0,
                                                       format,
                                                       nil,
                                                       bitmap);

    SetNativeImage(bitmap);
  end;

  constructor TGPBitmap.Create(width, height: Integer; target: TGPGraphics);
  var bitmap: GpBitmap;
  begin
    bitmap := nil;
    lastResult := GdipCreateBitmapFromGraphics(width,
                                                          height,
                                                          target.nativeGraphics,
                                                          bitmap);

    SetNativeImage(bitmap);
  end;

  function TGPBitmap.Clone(rect: TGPRect; format: TPixelFormat): TGPBitmap;
  begin
    result := Clone(rect.X, rect.Y, rect.Width, rect.Height, format);
  end;

  function TGPBitmap.Clone(x, y, width, height: Integer; format: TPixelFormat): TGPBitmap;
  var
    bitmap: TGPBitmap;
    gpdstBitmap: GpBitmap;
  begin
    gpdstBitmap := nil;
    lastResult := GdipCloneBitmapAreaI(
                               x,
                               y,
                               width,
                               height,
                               format,
                               GpBitmap(nativeImage),
                               gpdstBitmap);

    if (lastResult = Ok) then
    begin
       bitmap := TGPBitmap.Create(gpdstBitmap);
       if (bitmap = nil) then
         GdipDisposeImage(gpdstBitmap);
       result := bitmap;
       exit;
    end
    else
       result := nil;
  end;

  function TGPBitmap.Clone(rect: TGPRectF; format: TPixelFormat): TGPBitmap;
  begin
    result := Clone(rect.X, rect.Y, rect.Width, rect.Height, format);
  end;

  function TGPBitmap.Clone(x, y, width, height: Single; format: TPixelFormat): TGPBitmap;
  var
    bitmap: TGPBitmap;
    gpdstBitmap: GpBitmap;
  begin
    gpdstBitmap := nil;
    SetStatus(GdipCloneBitmapArea(
                               x,
                               y,
                               width,
                               height,
                               format,
                               GpBitmap(nativeImage),
                               gpdstBitmap));

   if (lastResult = Ok) then
   begin
     bitmap := TGPBitmap.Create(gpdstBitmap);
     if (bitmap = nil) then
       GdipDisposeImage(gpdstBitmap);
       result := bitmap;
   end
   else
       result := nil;
  end;

  function TGPBitmap.LockBits(rect: TGPRect; flags: UINT; format: TPixelFormat;
               out lockedBitmapData: TBitmapData): TStatus;
  begin
    result := SetStatus(GdipBitmapLockBits(
                                    GpBitmap(nativeImage),
                                    @rect,
                                    flags,
                                    format,
                                    @lockedBitmapData));
  end;

  function TGPBitmap.UnlockBits(var lockedBitmapData: TBitmapData): TStatus;
  begin
    result := SetStatus(GdipBitmapUnlockBits(
                                    GpBitmap(nativeImage),
                                    @lockedBitmapData));
  end;

  function TGPBitmap.GetPixel(x, y: Integer; out color: TGPColor): TStatus;
  begin
    result := SetStatus(GdipBitmapGetPixel(GpBitmap(nativeImage), x, y, color));
  end;

  function TGPBitmap.SetPixel(x, y: Integer; color: TGPColor): TStatus;
  begin
    result := SetStatus(GdipBitmapSetPixel(
        GpBitmap(nativeImage),
        x, y,
        color));
  end;

  function TGPBitmap.SetResolution(xdpi, ydpi: Single): TStatus;
  begin
    result := SetStatus(GdipBitmapSetResolution(
        GpBitmap(nativeImage),
        xdpi, ydpi));
  end;

  constructor TGPBitmap.Create(var gdiBitmapInfo: TBITMAPINFO; gdiBitmapData: Pointer);
  var bitmap: GpBitmap;
  begin
    bitmap := nil;
    lastResult := GdipCreateBitmapFromGdiDib(@gdiBitmapInfo, gdiBitmapData, bitmap);
    SetNativeImage(bitmap);
  end;

  constructor TGPBitmap.Create(hbm: HBITMAP; hpal: HPALETTE);
  var bitmap: GpBitmap;
  begin
    bitmap := nil;
    lastResult := GdipCreateBitmapFromHBITMAP(hbm, hpal, bitmap);
    SetNativeImage(bitmap);
  end;

  {$IFNDEF KS_CBUILDER}
  constructor TGPBitmap.Create(hicon: HICON);
  var bitmap: GpBitmap;
  begin
    bitmap := nil;
    lastResult := GdipCreateBitmapFromHICON(hicon, bitmap);
    SetNativeImage(bitmap);
  end;

  function TGPBitmap.FromHICON(hicon: HICON): TGPBitmap;
  begin
    result := TGPBitmap.Create(hicon);
  end;
  {$ENDIF}

  constructor TGPBitmap.Create(hInstance: HMODULE; bitmapName: WideString);
  var bitmap: GpBitmap;
  begin
    bitmap := nil;
    lastResult := GdipCreateBitmapFromResource(hInstance, PWideChar(bitmapName), bitmap);
    SetNativeImage(bitmap);
  end;

  function TGPBitmap.FromBITMAPINFO(var gdiBitmapInfo: TBITMAPINFO; gdiBitmapData: Pointer): TGPBitmap;
  begin
    result := TGPBitmap.Create(gdiBitmapInfo, gdiBitmapData);
  end;

  function TGPBitmap.FromHBITMAP(hbm: HBITMAP; hpal: HPALETTE): TGPBitmap;
  begin
    result := TGPBitmap.Create(hbm, hpal);
  end;

  function TGPBitmap.FromResource(hInstance: HMODULE; bitmapName: WideString): TGPBitmap;
  begin
    result := TGPBitmap.Create(hInstance, PWideChar(bitmapName));
  end;

  function TGPBitmap.GetHBITMAP(colorBackground: TGPColor; out hbmreturn: HBITMAP): TStatus;
  begin
    result := SetStatus(GdipCreateHBITMAPFromBitmap(
                                        GpBitmap(nativeImage),
                                        hbmreturn,
                                        colorBackground));
  end;

  function TGPBitmap.GetHICON(out hicon: HICON): TStatus;
  begin
    result := SetStatus(GdipCreateHICONFromBitmap(
                                        GpBitmap(nativeImage),
                                        hicon));
  end;

  constructor TGPBitmap.Create(nativeBitmap: GpBitmap);
  begin
    lastResult := Ok;
    SetNativeImage(nativeBitmap);
  end;

(**************************************************************************\
*
*   GDI+ Graphics Object
*
\**************************************************************************)

  function TGPGraphics.FromHDC(hdc: HDC): TGPGraphics;
  begin
    result := TGPGraphics.Create(hdc, 0, 0);
  end;

  function TGPGraphics.FromHDC(hdc: HDC; hdevice: THANDLE): TGPGraphics;
  begin
    result := TGPGraphics.Create(hdc, hdevice);
  end;

  function TGPGraphics.FromHWND(hwnd: HWND; icm: BOOL = FALSE): TGPGraphics;
  begin
    result := TGPGraphics.Create(hwnd, icm);
  end;

  function TGPGraphics.FromImage(image: TGPImage): TGPGraphics;
  begin
    result := TGPGraphics.Create(image);
  end;

  constructor TGPGraphics.Create(hdc: HDC; NotUses1, NotUses2: integer);
  var graphics: GpGraphics;
  begin
    graphics:= nil;
    lastResult := GdipCreateFromHDC(hdc, graphics);
    SetNativeGraphics(graphics);
  end;

  constructor TGPGraphics.Create(hdc: HDC; hdevice: THANDLE);
  var graphics: GpGraphics;
  begin
    graphics:= nil;
    lastResult := GdipCreateFromHDC2(hdc, hdevice, graphics);
    SetNativeGraphics(graphics);
  end;

  constructor TGPGraphics.Create(hwnd: HWND; icm: BOOL{ = FALSE});
  var graphics: GpGraphics;
  begin
    graphics:= nil;
    if icm then lastResult := GdipCreateFromHWNDICM(hwnd, graphics)
           else lastResult := GdipCreateFromHWND(hwnd, graphics);
    SetNativeGraphics(graphics);
  end;

  constructor TGPGraphics.Create(image: TGPImage);
  var graphics: GpGraphics;
  begin
    graphics:= nil;
    if (image <> nil) then
      lastResult := GdipGetImageGraphicsContext(image.nativeImage, graphics);
    SetNativeGraphics(graphics);
  end;

  destructor TGPGraphics.destroy;
  begin
    GdipDeleteGraphics(nativeGraphics);
  end;

  procedure TGPGraphics.Flush(intention: TFlushIntention = FlushIntentionFlush);
  begin
    GdipFlush(nativeGraphics, intention);
  end;

    //------------------------------------------------------------------------
    // GDI Interop methods
    //------------------------------------------------------------------------

    // Locks the graphics until ReleaseDC is called

  function TGPGraphics.GetHDC: HDC;
  begin
    SetStatus(GdipGetDC(nativeGraphics, result));
  end;

  procedure TGPGraphics.ReleaseHDC(hdc: HDC);
  begin
    SetStatus(GdipReleaseDC(nativeGraphics, hdc));
  end;

    //------------------------------------------------------------------------
    // Rendering modes
    //------------------------------------------------------------------------

  function TGPGraphics.SetRenderingOrigin(x, y: Integer): TStatus;
  begin
    result := SetStatus(GdipSetRenderingOrigin(nativeGraphics, x, y));
  end;

  function TGPGraphics.GetRenderingOrigin(out x, y: Integer): TStatus;
  begin
    result := SetStatus(GdipGetRenderingOrigin(nativeGraphics, x, y));
  end;

  function TGPGraphics.SetCompositingMode(compositingMode: TCompositingMode): TStatus;
  begin
    result := SetStatus(GdipSetCompositingMode(nativeGraphics,
                                compositingMode));
  end;

  function TGPGraphics.GetCompositingMode: TCompositingMode;
  begin
    SetStatus(GdipGetCompositingMode(nativeGraphics, result));
  end;

  function TGPGraphics.SetCompositingQuality(compositingQuality: TCompositingQuality): TStatus;
  begin
    result := SetStatus(GdipSetCompositingQuality( nativeGraphics, compositingQuality));
  end;

  function TGPGraphics.GetCompositingQuality: TCompositingQuality;
  begin
    SetStatus(GdipGetCompositingQuality(nativeGraphics, result));
  end;

  function TGPGraphics.SetTextRenderingHint(newMode: TTextRenderingHint): TStatus;
  begin
    result := SetStatus(GdipSetTextRenderingHint(nativeGraphics, newMode));
  end;

  function TGPGraphics.GetTextRenderingHint: TTextRenderingHint;
  begin
    SetStatus(GdipGetTextRenderingHint(nativeGraphics, result));
  end;

  function TGPGraphics.SetTextContrast(contrast: UINT): TStatus;
  begin
    result := SetStatus(GdipSetTextContrast(nativeGraphics, contrast));
  end;

  function TGPGraphics.GetTextContrast: UINT;
  begin
    SetStatus(GdipGetTextContrast(nativeGraphics, result));
  end;

  function TGPGraphics.GetInterpolationMode: TInterpolationMode;
  var mode: TInterpolationMode;
  begin
    mode := InterpolationModeInvalid;
    SetStatus(GdipGetInterpolationMode(nativeGraphics, mode));
    result := mode;
  end;

  function TGPGraphics.SetInterpolationMode(interpolationMode: TInterpolationMode): TStatus;
  begin
    result := SetStatus(GdipSetInterpolationMode(nativeGraphics,
                               interpolationMode));
  end;

  function TGPGraphics.GetSmoothingMode: TSmoothingMode;
  var smoothingMode: TSmoothingMode;
  begin
    smoothingMode := SmoothingModeInvalid;
    SetStatus(GdipGetSmoothingMode(nativeGraphics,  smoothingMode));
    result := smoothingMode;
  end;

  function TGPGraphics.SetSmoothingMode(smoothingMode: TSmoothingMode): TStatus;
  begin
    result := SetStatus(GdipSetSmoothingMode(nativeGraphics, smoothingMode));
  end;

  function TGPGraphics.GetPixelOffsetMode: TPixelOffsetMode;
  var pixelOffsetMode: TPixelOffsetMode;
  begin
    pixelOffsetMode := PixelOffsetModeInvalid;
    SetStatus(GdipGetPixelOffsetMode(nativeGraphics, pixelOffsetMode));
    result := pixelOffsetMode;
  end;

  function TGPGraphics.SetPixelOffsetMode(pixelOffsetMode: TPixelOffsetMode): TStatus;
  begin
    result := SetStatus(GdipSetPixelOffsetMode(nativeGraphics, pixelOffsetMode));
  end;

    //------------------------------------------------------------------------
    // Manipulate current world transform
    //------------------------------------------------------------------------

  function TGPGraphics.SetTransform(matrix: TGPMatrix): TStatus;
  begin
    result := SetStatus(GdipSetWorldTransform(nativeGraphics, matrix.nativeMatrix));
  end;

  function TGPGraphics.ResetTransform: TStatus;
  begin
    result := SetStatus(GdipResetWorldTransform(nativeGraphics));
  end;

  function TGPGraphics.MultiplyTransform(matrix: TGPMatrix; order: TMatrixOrder = MatrixOrderPrepend): TStatus;
  begin
    result := SetStatus(GdipMultiplyWorldTransform(nativeGraphics,
                                matrix.nativeMatrix,
                                order));
  end;

  function TGPGraphics.TranslateTransform(dx, dy: Single; order: TMatrixOrder = MatrixOrderPrepend): TStatus;
  begin
    result := SetStatus(GdipTranslateWorldTransform(nativeGraphics,
                                   dx, dy, order));
  end;

  function TGPGraphics.ScaleTransform(sx, sy: Single; order: TMatrixOrder = MatrixOrderPrepend): TStatus;
  begin
    result := SetStatus(GdipScaleWorldTransform(nativeGraphics,
                                 sx, sy, order));
  end;

  function TGPGraphics.RotateTransform(angle: Single; order: TMatrixOrder = MatrixOrderPrepend): TStatus;
  begin
    result := SetStatus(GdipRotateWorldTransform(nativeGraphics,
                                  angle, order));
  end;

  function TGPGraphics.GetTransform(matrix: TGPMatrix): TStatus;
  begin
    result := SetStatus(GdipGetWorldTransform(nativeGraphics,
                               matrix.nativeMatrix));
  end;

  function TGPGraphics.SetPageUnit(unit_: TUnit): TStatus;
  begin
    result := SetStatus(GdipSetPageUnit(nativeGraphics,
                             unit_));
  end;

  function TGPGraphics.SetPageScale(scale: Single): TStatus;
  begin
    result := SetStatus(GdipSetPageScale(nativeGraphics,
                              scale));
  end;

  function TGPGraphics.GetPageUnit: TUnit;
  begin
    SetStatus(GdipGetPageUnit(nativeGraphics, result));
  end;

  function TGPGraphics.GetPageScale: Single;
  begin
    SetStatus(GdipGetPageScale(nativeGraphics, result));
  end;

  function TGPGraphics.GetDpiX: Single;
  begin
    SetStatus(GdipGetDpiX(nativeGraphics, result));
  end;

  function TGPGraphics.GetDpiY: Single;
  begin
    SetStatus(GdipGetDpiY(nativeGraphics, result));
  end;

  function TGPGraphics.TransformPoints(destSpace: TCoordinateSpace;
               srcSpace: TCoordinateSpace;
               pts: PGPPointF;
               count: Integer): TStatus;
  begin
    result := SetStatus(GdipTransformPoints(nativeGraphics,
                             destSpace,
                             srcSpace,
                             pts,
                             count));
  end;

  function TGPGraphics.TransformPoints(destSpace: TCoordinateSpace;
               srcSpace: TCoordinateSpace;
               pts: PGPPoint;
               count: Integer): TStatus;
  begin

    result := SetStatus(GdipTransformPointsI(nativeGraphics,
                              destSpace,
                              srcSpace,
                              pts,
                              count));
  end;

    //------------------------------------------------------------------------
    // GetNearestColor (for <= 8bpp surfaces).  Note: Alpha is ignored.
    //------------------------------------------------------------------------

  function TGPGraphics.GetNearestColor(var color: TGPColor): TStatus;
  begin
    result := SetStatus(GdipGetNearestColor(nativeGraphics, @color));
  end;

  function TGPGraphics.DrawLine(pen: TGPPen; x1, y1, x2, y2: Single): TStatus;
  begin
    result := SetStatus(GdipDrawLine(nativeGraphics,
                          pen.nativePen, x1, y1, x2,
                          y2));
  end;

  function TGPGraphics.DrawLine(pen: TGPPen; const pt1, pt2: TGPPointF): TStatus;
  begin
    result := DrawLine(pen, pt1.X, pt1.Y, pt2.X, pt2.Y);
  end;

  function TGPGraphics.DrawLines(pen: TGPPen; points: PGPPointF; count: Integer): TStatus;
  begin
    result := SetStatus(GdipDrawLines(nativeGraphics,
                           pen.nativePen,
                           points, count));
  end;

  function TGPGraphics.DrawLine(pen: TGPPen; x1, y1, x2, y2: Integer): TStatus;
  begin
    result := SetStatus(GdipDrawLineI(nativeGraphics,
                           pen.nativePen,
                           x1,
                           y1,
                           x2,
                           y2));
  end;

  function TGPGraphics.DrawLine(pen: TGPPen; const pt1, pt2: TGPPoint): TStatus;
  begin
    result := DrawLine(pen,
            pt1.X,
            pt1.Y,
            pt2.X,
            pt2.Y);
  end;

  function TGPGraphics.DrawLines(pen: TGPPen; points: PGPPoint; count: Integer): TStatus;
  begin
    result := SetStatus(GdipDrawLinesI(nativeGraphics,
                            pen.nativePen,
                            points,
                            count));
  end;

  function TGPGraphics.DrawArc(pen: TGPPen; x, y, width, height, startAngle, sweepAngle: Single): TStatus;
  begin
    result := SetStatus(GdipDrawArc(nativeGraphics,
                         pen.nativePen,
                         x,
                         y,
                         width,
                         height,
                         startAngle,
                         sweepAngle));
  end;

  function TGPGraphics.DrawArc(pen: TGPPen; const rect: TGPRectF; startAngle, sweepAngle: Single): TStatus;
  begin
    result := DrawArc(pen, rect.X, rect.Y, rect.Width, rect.Height,
               startAngle, sweepAngle);
  end;

  function TGPGraphics.DrawArc(pen: TGPPen; x, y, width, height: Integer; startAngle,
           sweepAngle: Single): TStatus; 
  begin
    result := SetStatus(GdipDrawArcI(nativeGraphics,
                          pen.nativePen,
                          x,
                          y,
                          width,
                          height,
                          startAngle,
                          sweepAngle));
  end;


  function TGPGraphics.DrawArc(pen: TGPPen; const rect: TGPRect; startAngle, sweepAngle: Single): TStatus;
  begin
    result := DrawArc(pen,
               rect.X,
               rect.Y,
               rect.Width,
               rect.Height,
               startAngle,
               sweepAngle);
  end;

  function TGPGraphics.DrawBezier(pen: TGPPen; x1, y1, x2, y2, x3, y3, x4, y4: Single): TStatus;
  begin
    result := SetStatus(GdipDrawBezier(nativeGraphics,
                            pen.nativePen, x1, y1,
                            x2, y2, x3, y3, x4, y4));
  end;

  function TGPGraphics.DrawBezier(pen: TGPPen; const pt1, pt2, pt3, pt4: TGPPointF): TStatus;
  begin
    result := DrawBezier(pen,
              pt1.X,
              pt1.Y,
              pt2.X,
              pt2.Y,
              pt3.X,
              pt3.Y,
              pt4.X,
              pt4.Y);
  end;

  function TGPGraphics.DrawBeziers(pen: TGPPen; points: PGPPointF; count: Integer): TStatus;
  begin
    result := SetStatus(GdipDrawBeziers(nativeGraphics,
                             pen.nativePen,
                             points,
                             count));
  end;

  function TGPGraphics.DrawBezier(pen: TGPPen; x1, y1, x2, y2, x3, y3, x4, y4: Integer): TStatus;
  begin
    result := SetStatus(GdipDrawBezierI(nativeGraphics,
                             pen.nativePen,
                             x1,
                             y1,
                             x2,
                             y2,
                             x3,
                             y3,
                             x4,
                             y4));
  end;

  function TGPGraphics.DrawBezier(pen: TGPPen; const pt1, pt2, pt3, pt4: TGPPoint): TStatus;
  begin
    result := DrawBezier(pen,
              pt1.X,
              pt1.Y,
              pt2.X,
              pt2.Y,
              pt3.X,
              pt3.Y,
              pt4.X,
              pt4.Y);
  end;

  function TGPGraphics.DrawBeziers(pen: TGPPen; points: PGPPoint; count: Integer): TStatus;
  begin
    result := SetStatus(GdipDrawBeziersI(nativeGraphics,
                              pen.nativePen,
                              points,
                              count));
  end;

  function TGPGraphics.DrawRectangle(pen: TGPPen; const rect: TGPRectF): TStatus;
  begin
    result := DrawRectangle(pen, rect.X, rect.Y, rect.Width, rect.Height);
  end;

  function TGPGraphics.DrawRectangle(pen: TGPPen; x, y, width, height: Single): TStatus;
  begin
    result := SetStatus(GdipDrawRectangle(nativeGraphics,
                               pen.nativePen, x, y,
                               width, height));
  end;

  function TGPGraphics.DrawRectangles(pen: TGPPen;  rects: PGPRectF; count: Integer): TStatus;
  begin
    result := SetStatus(GdipDrawRectangles(nativeGraphics,
                            pen.nativePen,
                            rects, count));
  end;

  function TGPGraphics.DrawRectangle(pen: TGPPen; const rect: TGPRect): TStatus;
  begin
    result := DrawRectangle(pen,
                 rect.X,
                 rect.Y,
                 rect.Width,
                 rect.Height);
  end;

  function TGPGraphics.DrawRectangle(pen: TGPPen; x, y, width, height: Integer): TStatus;
  begin
    result := SetStatus(GdipDrawRectangleI(nativeGraphics,
                            pen.nativePen,
                            x,
                            y,
                            width,
                            height));
  end;

  function TGPGraphics.DrawRectangles(pen: TGPPen; rects: PGPRect; count: Integer): TStatus;
  begin
    result := SetStatus(GdipDrawRectanglesI(nativeGraphics,
                             pen.nativePen,
                             rects,
                             count));
  end;

  function TGPGraphics.DrawEllipse(pen: TGPPen; const rect: TGPRectF): TStatus;
  begin
    result := DrawEllipse(pen, rect.X, rect.Y, rect.Width, rect.Height);
  end;

  function TGPGraphics.DrawEllipse(pen: TGPPen; x, y, width, height: Single): TStatus;
  begin
    result := SetStatus(GdipDrawEllipse(nativeGraphics,
                             pen.nativePen,
                             x,
                             y,
                             width,
                             height));
  end;

  function TGPGraphics.DrawEllipse(pen: TGPPen; const rect: TGPRect): TStatus;
  begin
    result := DrawEllipse(pen,
               rect.X,
               rect.Y,
               rect.Width,
               rect.Height);
  end;

  function TGPGraphics.DrawEllipse(pen: TGPPen; x, y, width, height: Integer): TStatus;
  begin
    result := SetStatus(GdipDrawEllipseI(nativeGraphics,
                              pen.nativePen,
                              x,
                              y,
                              width,
                              height));
  end;

  function TGPGraphics.DrawPie(pen: TGPPen; const rect: TGPRectF; startAngle, sweepAngle: Single): TStatus;
  begin
    result := DrawPie(pen,
               rect.X,
               rect.Y,
               rect.Width,
               rect.Height,
               startAngle,
               sweepAngle);
  end;

  function TGPGraphics.DrawPie(pen: TGPPen; x, y, width, height, startAngle, sweepAngle: Single): TStatus;
  begin
    result := SetStatus(GdipDrawPie(nativeGraphics,
                         pen.nativePen,
                         x,
                         y,
                         width,
                         height,
                         startAngle,
                         sweepAngle));
  end;

  function TGPGraphics.DrawPie(pen: TGPPen; const rect: TGPRect; startAngle, sweepAngle: Single): TStatus;
  begin
    result := DrawPie(pen,
               rect.X,
               rect.Y,
               rect.Width,
               rect.Height,
               startAngle,
               sweepAngle);
  end;

  function TGPGraphics.DrawPie(pen: TGPPen; x, y, width, height: Integer;
                 startAngle, sweepAngle: Single): TStatus;
  begin
    result := SetStatus(GdipDrawPieI(nativeGraphics,
                          pen.nativePen,
                          x,
                          y,
                          width,
                          height,
                          startAngle,
                          sweepAngle));
  end;

  function TGPGraphics.DrawPolygon(pen: TGPPen; points: PGPPointF; count: Integer): TStatus;
  begin
    result := SetStatus(GdipDrawPolygon(nativeGraphics,
                             pen.nativePen,
                             points,
                             count));
  end;

  function TGPGraphics.DrawPolygon(pen: TGPPen; points: PGPPoint; count: Integer): TStatus;
  begin
    result := SetStatus(GdipDrawPolygonI(nativeGraphics,
                              pen.nativePen,
                              points,
                              count));
  end;

  function TGPGraphics.DrawPath(pen: TGPPen; path: TGPGraphicsPath): TStatus;
  var
    nPen: GpPen;
    nPath: GpPath;
  begin
    if assigned(pen) then nPen := pen.nativePen else nPen  := nil;
    if assigned(path) then nPath := path.nativePath else nPath := nil;
    result := SetStatus(GdipDrawPath(nativeGraphics, nPen, nPath));
  end;

  function TGPGraphics.DrawCurve(pen: TGPPen; points: PGPPointF; count: Integer): TStatus;
  begin
    result := SetStatus(GdipDrawCurve(nativeGraphics,
                           pen.nativePen, points,
                           count));
  end;

  function TGPGraphics.DrawCurve(pen: TGPPen; points: PGPPointF; count: Integer; tension: Single): TStatus;
  begin
    result := SetStatus(GdipDrawCurve2(nativeGraphics,
                            pen.nativePen, points,
                            count, tension));
  end;

  function TGPGraphics.DrawCurve(pen: TGPPen; points: PGPPointF; count, offset,
           numberOfSegments: Integer; tension: Single = 0.5): TStatus;
  begin
    result := SetStatus(GdipDrawCurve3(nativeGraphics,
                            pen.nativePen, points,
                            count, offset,
                            numberOfSegments, tension));
  end;

  function TGPGraphics.DrawCurve(pen: TGPPen; points: PGPPoint; count: Integer): TStatus;
  begin
    result := SetStatus(GdipDrawCurveI(nativeGraphics,
                            pen.nativePen,
                            points,
                            count));
  end;

  function TGPGraphics.DrawCurve(pen: TGPPen; points: PGPPoint; count: Integer; tension: Single): TStatus;
  begin
    result := SetStatus(GdipDrawCurve2I(nativeGraphics,
                             pen.nativePen,
                             points,
                             count,
                             tension));
  end;

  function TGPGraphics.DrawCurve(pen: TGPPen; points: PGPPoint; count, offset,
           numberOfSegments: Integer; tension: Single = 0.5): TStatus; 
  begin
    result := SetStatus(GdipDrawCurve3I(nativeGraphics,
                             pen.nativePen,
                             points,
                             count,
                             offset,
                             numberOfSegments,
                             tension));
  end;

  function TGPGraphics.DrawClosedCurve(pen: TGPPen; points: PGPPointF; count: Integer): TStatus;
  begin
    result := SetStatus(GdipDrawClosedCurve(nativeGraphics,
                             pen.nativePen,
                             points, count));
  end;

  function TGPGraphics.DrawClosedCurve(pen: TGPPen; points: PGPPointF; count: Integer;
           tension: Single): TStatus;
  begin
    result := SetStatus(GdipDrawClosedCurve2(nativeGraphics,
                              pen.nativePen,
                              points, count,
                              tension));
  end;

  function TGPGraphics.DrawClosedCurve(pen: TGPPen; points: PGPPoint; count: Integer): TStatus;
  begin
    result := SetStatus(GdipDrawClosedCurveI(nativeGraphics,
                              pen.nativePen,
                              points,
                              count));
  end;

  function TGPGraphics.DrawClosedCurve(pen: TGPPen; points: PGPPoint;
           count: Integer; tension: Single): TStatus; 
  begin
    result := SetStatus(GdipDrawClosedCurve2I(nativeGraphics,
                               pen.nativePen,
                               points,
                               count,
                               tension));
  end;

  function TGPGraphics.Clear(color: TGPColor): TStatus;
  begin
    result := SetStatus(GdipGraphicsClear(
        nativeGraphics,
        color));
  end;

  function TGPGraphics.FillRectangle(brush: TGPBrush; const rect: TGPRectF): TStatus;
  begin
    result := FillRectangle(brush, rect.X, rect.Y, rect.Width, rect.Height);
  end;

  function TGPGraphics.FillRectangle(brush: TGPBrush; x, y, width, height: Single): TStatus;
  begin
    result := SetStatus(GdipFillRectangle(nativeGraphics,
                               brush.nativeBrush, x, y,
                               width, height));
  end;

  function TGPGraphics.FillRectangles(brush: TGPBrush; rects: PGPRectF; count: Integer): TStatus;
  begin
    result := SetStatus(GdipFillRectangles(nativeGraphics,
                            brush.nativeBrush,
                            rects, count));
  end;

  function TGPGraphics.FillRectangle(brush: TGPBrush; const rect: TGPRect): TStatus;
  begin
    result := FillRectangle(brush,
                 rect.X,
                 rect.Y,
                 rect.Width,
                 rect.Height);
  end;

  function TGPGraphics.FillRectangle(brush: TGPBrush; x, y, width, height: Integer): TStatus;
  begin
    result := SetStatus(GdipFillRectangleI(nativeGraphics,
                            brush.nativeBrush,
                            x,
                            y,
                            width,
                            height));
  end;

  function TGPGraphics.FillRectangles(brush: TGPBrush; rects: PGPRect; count: Integer): TStatus;
  begin
    result := SetStatus(GdipFillRectanglesI(nativeGraphics,
                             brush.nativeBrush,
                             rects,
                             count));
  end;

  function TGPGraphics.FillPolygon(brush: TGPBrush; points: PGPPointF; count: Integer): TStatus;
  begin
    result := FillPolygon(brush, points, count, FillModeAlternate);
  end;

  function TGPGraphics.FillPolygon(brush: TGPBrush; points: PGPPointF; count: Integer;
               fillMode: TFillMode): TStatus;
  begin
    result := SetStatus(GdipFillPolygon(nativeGraphics,
                             brush.nativeBrush,
                             points, count, fillMode));
  end;

  function TGPGraphics.FillPolygon(brush: TGPBrush; points: PGPPoint; count: Integer): TStatus;
  begin
    result := FillPolygon(brush, points, count, FillModeAlternate);
  end;

  function TGPGraphics.FillPolygon(brush: TGPBrush; points: PGPPoint; count: Integer;
               fillMode: TFillMode): TStatus;
  begin
    result := SetStatus(GdipFillPolygonI(nativeGraphics,
                              brush.nativeBrush,
                              points, count,
                              fillMode));
  end;

  function TGPGraphics.FillEllipse(brush: TGPBrush; const rect: TGPRectF): TStatus;
  begin
    result := FillEllipse(brush, rect.X, rect.Y, rect.Width, rect.Height);
  end;

  function TGPGraphics.FillEllipse(brush: TGPBrush; x, y, width, height: Single): TStatus;
  begin
    result := SetStatus(GdipFillEllipse(nativeGraphics,
                             brush.nativeBrush, x, y,
                             width, height));
  end;

  function TGPGraphics.FillEllipse(brush: TGPBrush; const rect: TGPRect): TStatus;
  begin
    result := FillEllipse(brush, rect.X, rect.Y, rect.Width, rect.Height);
  end;

  function TGPGraphics.FillEllipse(brush: TGPBrush; x, y, width, height: Integer): TStatus;
  begin
    result := SetStatus(GdipFillEllipseI(nativeGraphics,
                              brush.nativeBrush,
                              x,
                              y,
                              width,
                              height));
  end;

  function TGPGraphics.FillPie(brush: TGPBrush; const rect: TGPRectF; startAngle, sweepAngle: Single): TStatus;
  begin
    result := FillPie(brush, rect.X, rect.Y, rect.Width, rect.Height,
               startAngle, sweepAngle);
  end;

  function TGPGraphics.FillPie(brush: TGPBrush; x, y, width, height, startAngle, sweepAngle: Single): TStatus;
  begin
    result := SetStatus(GdipFillPie(nativeGraphics,
                         brush.nativeBrush, x, y,
                         width, height, startAngle,
                         sweepAngle));
  end;

  function TGPGraphics.FillPie(brush: TGPBrush; const rect: TGPRect; startAngle, sweepAngle: Single): TStatus;
  begin
    result := FillPie(brush, rect.X, rect.Y, rect.Width, rect.Height,
               startAngle, sweepAngle);
  end;

  function TGPGraphics.FillPie(brush: TGPBrush; x, y, width, height: Integer; startAngle,
           sweepAngle: Single): TStatus; 
  begin
    result := SetStatus(GdipFillPieI(nativeGraphics,
                          brush.nativeBrush,
                          x,
                          y,
                          width,
                          height,
                          startAngle,
                          sweepAngle));
  end;

  function TGPGraphics.FillPath(brush: TGPBrush; path: TGPGraphicsPath): TStatus;
  begin
    result := SetStatus(GdipFillPath(nativeGraphics,
                          brush.nativeBrush,
                          path.nativePath));
  end;

  function TGPGraphics.FillClosedCurve(brush: TGPBrush; points: PGPPointF; count: Integer): TStatus;
  begin
    result := SetStatus(GdipFillClosedCurve(nativeGraphics,
                             brush.nativeBrush,
                             points, count));

  end;

  function TGPGraphics.FillClosedCurve(brush: TGPBrush; points: PGPPointF; count: Integer;
               fillMode: TFillMode; tension: Single = 0.5): TStatus;
  begin
    result := SetStatus(GdipFillClosedCurve2(nativeGraphics,
                              brush.nativeBrush,
                              points, count,
                              tension, fillMode));
  end;

  function TGPGraphics.FillClosedCurve(brush: TGPBrush; points:  PGPPoint; count: Integer): TStatus;
  begin
    result := SetStatus(GdipFillClosedCurveI(nativeGraphics,
                              brush.nativeBrush,
                              points,
                              count));
  end;

  function TGPGraphics.FillClosedCurve(brush: TGPBrush; points: PGPPoint;
           count: Integer; fillMode: TFillMode; tension: Single = 0.5): TStatus;
  begin
    result := SetStatus(GdipFillClosedCurve2I(nativeGraphics,
                               brush.nativeBrush,
                               points, count,
                               tension, fillMode));
  end;

  function TGPGraphics.FillRegion(brush: TGPBrush; region: TGPRegion): TStatus;
  begin
    result := SetStatus(GdipFillRegion(nativeGraphics,
                            brush.nativeBrush,
                            region.nativeRegion));
  end;


  function TGPGraphics.DrawString( string_: WideString; length: Integer; font: TGPFont;
     const layoutRect: TGPRectF; stringFormat: TGPStringFormat; brush: TGPBrush): TStatus;
  var
    nFont: GpFont;
    nStringFormat: GpStringFormat;
    nBrush: GpBrush;
  begin
    if assigned(font) then nfont := font.nativeFont else nfont := nil;
    if assigned(stringFormat) then nstringFormat := stringFormat.nativeFormat else nstringFormat := nil;
    if assigned(brush) then nbrush := brush.nativeBrush else nbrush := nil;
    result := SetStatus(GdipDrawString(
        nativeGraphics,
        PWideChar(string_),
        length,
        nfont,
        @layoutRect,
        nstringFormat,
        nbrush));
  end;

  function TGPGraphics.DrawString(string_: WideString; length: Integer; font: TGPFont;
           const origin: TGPPointF; brush: TGPBrush): TStatus;
  var
    rect: TGPRectF;
    nfont: Gpfont;
    nBrush: GpBrush;
  begin
    rect.X := origin.X;
    rect.Y := origin.Y;
    rect.Width := 0.0;
    rect.Height := 0.0;
    if assigned(font) then nfont := font.nativeFont else nfont := nil;
    if assigned(Brush) then nBrush := Brush.nativeBrush else nBrush := nil;
    result := SetStatus(GdipDrawString(
        nativeGraphics,
        PWideChar(string_),
        length,
        nfont,
        @rect,
        nil,
        nbrush));
  end;

  function TGPGraphics.DrawString(string_: WideString; length: Integer; font: TGPFont;
      const origin: TGPPointF; stringFormat: TGPStringFormat; brush: TGPBrush): TStatus;
  var
    rect: TGPRectF;
    nFont: GpFont;
    nStringFormat: GpStringFormat;
    nBrush: GpBrush;
  begin
    rect.X := origin.X;
    rect.Y := origin.Y;
    rect.Width := 0.0;
    rect.Height := 0.0;
    if assigned(font) then nfont := font.nativeFont else nfont := nil;
    if assigned(stringFormat) then nstringFormat := stringFormat.nativeFormat else nstringFormat := nil;
    if assigned(brush) then nbrush := brush.nativeBrush else nbrush := nil;
    result := SetStatus(GdipDrawString(
        nativeGraphics,
        PWideChar(string_),
        length,
        nfont,
        @rect,
        nstringFormat,
        nbrush));
  end;


  function TGPGraphics.MeasureString(string_: WideString; length: Integer; font: TGPFont;
       const layoutRect: TGPRectF; stringFormat: TGPStringFormat; out boundingBox: TGPRectF;
       codepointsFitted: PInteger = nil; linesFilled: PInteger = nil): TStatus;
  var
    nFont: GpFont;
    nStringFormat: GpStringFormat;
  begin
    if assigned(font) then nfont := font.nativeFont else nfont := nil;
    if assigned(stringFormat) then nstringFormat := stringFormat.nativeFormat else nstringFormat := nil;
    result := SetStatus(GdipMeasureString(
        nativeGraphics,
        PWideChar(string_),
        length,
        nfont,
        @layoutRect,
        nstringFormat,
        @boundingBox,
        codepointsFitted,
        linesFilled
    ));
  end;


  function TGPGraphics.MeasureString(string_: WideString; length: Integer; font: TGPFont;
       const layoutRectSize: TGPSizeF; stringFormat: TGPStringFormat; out size: TGPSizeF;
       codepointsFitted: PInteger = nil; linesFilled: PInteger = nil): TStatus;
  var
    layoutRect, boundingBox: TGPRectF;
    status: TStatus;
    nFont: GpFont;
    nStringFormat: GpStringFormat;
  begin
    layoutRect.X := 0;
    layoutRect.Y := 0;
    layoutRect.Width := layoutRectSize.Width;
    layoutRect.Height := layoutRectSize.Height;

    if assigned(font) then nfont := font.nativeFont else nfont := nil;
    if assigned(stringFormat) then nstringFormat := stringFormat.nativeFormat else nstringFormat := nil;

    status := SetStatus(GdipMeasureString(
        nativeGraphics,
        PWideChar(string_),
        length,
        nfont,
        @layoutRect,
        nstringFormat,
        @boundingBox,
        codepointsFitted,
        linesFilled
    ));

    if (status = Ok) then
    begin
      size.Width  := boundingBox.Width;
      size.Height := boundingBox.Height;
    end;
    result := status;
  end;

    
  function TGPGraphics.MeasureString(string_: WideString ; length: Integer; font: TGPFont;
       const origin: TGPPointF; stringFormat: TGPStringFormat; out boundingBox: TGPRectF): TStatus;
  var
    rect: TGPRectF;
    nFont: GpFont;
    nstringFormat: GpstringFormat;
  begin
    rect.X := origin.X;
    rect.Y := origin.Y;
    rect.Width := 0.0;
    rect.Height := 0.0;

    if assigned(font) then nfont := font.nativeFont else nfont := nil;
    if assigned(stringFormat) then nstringFormat := stringFormat.nativeFormat else nstringFormat := nil;

    result := SetStatus(GdipMeasureString(
        nativeGraphics,
        PWideChar(string_),
        length,
        nfont,
        @rect,
        nstringFormat,
        @boundingBox,
        nil,
        nil
    ));
  end;

    
  function TGPGraphics.MeasureString(string_: WideString; length: Integer; font: TGPFont;
       const layoutRect: TGPRectF; out boundingBox: TGPRectF): TStatus;
  var
    nFont: GpFont;
  begin
    if assigned(font) then nfont := font.nativeFont else nfont := nil;
    result := SetStatus(GdipMeasureString(
        nativeGraphics,
        PWideChar(string_),
        length,
        nfont,
        @layoutRect,
        nil,
        @boundingBox,
        nil,
        nil
    ));
  end;

    
  function TGPGraphics.MeasureString(string_: WideString; length: Integer; font: TGPFont;
       const origin: TGPPointF; out boundingBox: TGPRectF): TStatus;
  var
    nFont: GpFont;
    rect: TGPRectF;
  begin
    if assigned(font) then nfont := font.nativeFont else nfont := nil;
    rect.X := origin.X;
    rect.Y := origin.Y;
    rect.Width := 0.0;
    rect.Height := 0.0;

    result := SetStatus(GdipMeasureString(
        nativeGraphics,
        PWideChar(string_),
        length,
        nfont,
        @rect,
        nil,
        @boundingBox,
        nil,
        nil
    ));
  end;



  function TGPGraphics.MeasureCharacterRanges(string_: WideString; length: Integer; font: TGPFont;
       const layoutRect: TGPRectF; stringFormat: TGPStringFormat; regionCount: Integer;
       const regions: array of TGPRegion): TStatus;
  var
    nativeRegions: Pointer;
    i: Integer;
    Status: TStatus;
    nFont: GpFont;
    nstringFormat: GpstringFormat;
  type
    TArrayGpRegion = array of GpRegion;
  begin
    if assigned(font) then nfont := font.nativeFont else nfont := nil;
    if assigned(stringFormat) then nstringFormat := stringFormat.nativeFormat else nstringFormat := nil;

    if (regionCount <= 0) then
    begin
      result := InvalidParameter;
      exit;
    end;

    getmem(nativeRegions, Sizeof(GpRegion)* regionCount);

    for i := 0 to regionCount - 1 do
      TArrayGpRegion(nativeRegions)[i] := regions[i].nativeRegion;

    status := SetStatus(GdipMeasureCharacterRanges(
        nativeGraphics,
        PWideChar(string_),
        length,
        nfont,
        @layoutRect,
        nstringFormat,
        regionCount,
        nativeRegions
    ));

    freemem(nativeRegions, Sizeof(GpRegion)* regionCount);
    result := status;
  end;

  function TGPGraphics.DrawDriverString(text: PUINT16; length: Integer; font: TGPFont
       ; brush: TGPBrush; positions: PGPPointF; flags: Integer
       ; matrix: TGPMatrix): TStatus;
  var
    nfont: Gpfont;
    nbrush: Gpbrush;
    nmatrix: Gpmatrix;
  begin
    if assigned(font) then nfont := font.nativeFont else nfont := nil;
    if assigned(brush) then nbrush := brush.nativeBrush else nbrush := nil;
    if assigned(matrix) then nmatrix := matrix.nativeMatrix else nmatrix := nil;

    result := SetStatus(GdipDrawDriverString(
        nativeGraphics,
        text,
        length,
        nfont,
        nbrush,
        positions,
        flags,
        nmatrix));
  end;

  function TGPGraphics.MeasureDriverString(text: PUINT16; length: Integer; font: TGPFont;
       positions: PGPPointF; flags: Integer; matrix: TGPMatrix;
       out boundingBox: TGPRectF): TStatus;
  var
    nfont: Gpfont;
    nmatrix: Gpmatrix;
  begin
    if assigned(font) then nfont := font.nativeFont else nfont := nil;
    if assigned(matrix) then nmatrix := matrix.nativeMatrix else nmatrix := nil;

    result := SetStatus(GdipMeasureDriverString(
        nativeGraphics,
        text,
        length,
        nfont,
        positions,
        flags,
        nmatrix,
        @boundingBox
    ));
  end;

    // Draw a cached bitmap on this graphics destination offset by
    // x, y. Note this will fail with WrongState if the CachedBitmap
    // native format differs from this Graphics.

  function TGPGraphics.DrawCachedBitmap(cb: TGPCachedBitmap;  x, y: Integer): TStatus;
  begin
    result := SetStatus(GdipDrawCachedBitmap(
        nativeGraphics,
        cb.nativeCachedBitmap,
        x, y
    ));
  end;

  function TGPGraphics.DrawImage(image: TGPImage; const point: TGPPointF): TStatus;
  begin
    result := DrawImage(image, point.X, point.Y);
  end;

  function TGPGraphics.DrawImage(image: TGPImage; x, y: Single): TStatus;
  var
   nImage: GpImage;
  begin
    if assigned(Image) then nImage := Image.nativeImage else nImage := nil;
    result := SetStatus(GdipDrawImage(nativeGraphics, nImage, x, y));
  end;

  function TGPGraphics.DrawImage(image: TGPImage; const rect: TGPRectF): TStatus;
  begin
    result := DrawImage(image, rect.X, rect.Y, rect.Width, rect.Height);
  end;

  function TGPGraphics.DrawImage(image: TGPImage; x, y, width, height: Single): TStatus;
  var
   nImage: GpImage;
  begin
    if assigned(Image) then nImage := Image.nativeImage else nImage := nil;
    result := SetStatus(GdipDrawImageRect(nativeGraphics,
                               nimage,
                               x,
                               y,
                               width,
                               height));
  end;

  function TGPGraphics.DrawImage(image: TGPImage; const point: TGPPoint): TStatus;
  begin
    result := DrawImage(image, point.X, point.Y);
  end;

  function TGPGraphics.DrawImage(image: TGPImage; x, y: Integer): TStatus;
  var
   nImage: GpImage;
  begin
    if assigned(Image) then nImage := Image.nativeImage else nImage := nil;
    result := SetStatus(GdipDrawImageI(nativeGraphics,
                            nimage,
                            x,
                            y));
  end;

  function TGPGraphics.DrawImage(image: TGPImage; const rect: TGPRect): TStatus;
  begin
    result := DrawImage(image,
             rect.X,
             rect.Y,
             rect.Width,
             rect.Height);
  end;

  function TGPGraphics.DrawImage(image: TGPImage; x, y, width, height: Integer): TStatus;
  var
   nImage: GpImage;
  begin
    if assigned(Image) then nImage := Image.nativeImage else nImage := nil;
    result := SetStatus(GdipDrawImageRectI(nativeGraphics,
                            nimage,
                            x,
                            y,
                            width,
                            height));
  end;


  function TGPGraphics.DrawImage(image: TGPImage; destPoints: PGPPointF; count: Integer): TStatus;
  var
   nImage: GpImage;
  begin
    if (((count <> 3) and (count <> 4)) or (destPoints = nil)) then
    begin
      result := SetStatus(InvalidParameter);
      exit;
    end;
    if assigned(Image) then nImage := Image.nativeImage else nImage := nil;
    result := SetStatus(GdipDrawImagePoints(nativeGraphics,
                             nimage,
                             destPoints, count));
  end;

  function TGPGraphics.DrawImage(image: TGPImage; destPoints: PGPPoint; count: Integer): TStatus;
  var
   nImage: GpImage;
  begin
    if (((count <> 3) and (count <> 4))or (destPoints = nil)) then
    begin
      result := SetStatus(InvalidParameter);
      exit;
    end;

    if assigned(Image) then nImage := Image.nativeImage else nImage := nil;
    result := SetStatus(GdipDrawImagePointsI(nativeGraphics,
                              nimage,
                              destPoints,
                              count));
  end;

  function TGPGraphics.DrawImage(image: TGPImage; x, y, srcx, srcy, srcwidth, srcheight: Single;
        srcUnit: TUnit): TStatus;
  var
    nImage: GpImage;
  begin
    if assigned(Image) then nImage := Image.nativeImage else nImage := nil;
    result := SetStatus(GdipDrawImagePointRect(nativeGraphics,
                                nimage,
                                x, y,
                                srcx, srcy,
                                srcwidth, srcheight, srcUnit));
  end;

  function TGPGraphics.DrawImage(image: TGPImage; const destRect: TGPRectF; srcx, srcy, srcwidth, srcheight: Single;
       srcUnit: TUnit; imageAttributes: TGPImageAttributes = nil; callback: DrawImageAbort = nil;
       callbackData: Pointer = nil): TStatus;
  var
    nImage: GpImage;
    nimageAttributes: GpimageAttributes;
  begin
    if assigned(Image) then nImage := Image.nativeImage else nImage := nil;
    if assigned(imageAttributes) then nimageAttributes := imageAttributes.nativeImageAttr else nimageAttributes := nil;
    result := SetStatus(GdipDrawImageRectRect(nativeGraphics,
                               nimage,
                               destRect.X,
                               destRect.Y,
                               destRect.Width,
                               destRect.Height,
                               srcx, srcy,
                               srcwidth, srcheight,
                               srcUnit,
                               nimageAttributes,
                               callback,
                               callbackData));
  end;

  function TGPGraphics.DrawImage(image: TGPImage; destPoints: PGPPointF; count: Integer;
       srcx, srcy, srcwidth, srcheight: Single; srcUnit: TUnit;
       imageAttributes: TGPImageAttributes = nil; callback: DrawImageAbort = nil;
       callbackData: Pointer = nil): TStatus;
  var
    nImage: GpImage;
    nimageAttributes: GpimageAttributes;
  begin
    if assigned(Image) then nImage := Image.nativeImage else nImage := nil;
    if assigned(imageAttributes) then nimageAttributes := imageAttributes.nativeImageAttr else nimageAttributes := nil;
    result := SetStatus(GdipDrawImagePointsRect(nativeGraphics,
                                 nimage,
                                 destPoints, count,
                                 srcx, srcy,
                                 srcwidth,
                                 srcheight,
                                 srcUnit,
                                 nimageAttributes,
                                 callback,
                                 callbackData));
  end;

  function TGPGraphics.DrawImage(image: TGPImage; x, y, srcx, srcy, srcwidth, srcheight: Integer;
       srcUnit: TUnit): TStatus;
  var
    nImage: GpImage;
  begin
    if assigned(Image) then nImage := Image.nativeImage else nImage := nil;
    result := SetStatus(GdipDrawImagePointRectI(nativeGraphics,
                                 nimage,
                                 x,
                                 y,
                                 srcx,
                                 srcy,
                                 srcwidth,
                                 srcheight,
                                 srcUnit));
  end;

  function TGPGraphics.DrawImage(image: TGPImage; const destRect: TGPRect; srcx, srcy, srcwidth,
       srcheight: Integer; srcUnit: TUnit; imageAttributes: TGPImageAttributes = nil;
       callback: DrawImageAbort = nil; callbackData: Pointer = nil): TStatus;
  var
    nImage: GpImage;
    nimageAttributes: GpimageAttributes;
  begin
    if assigned(Image) then nImage := Image.nativeImage else nImage := nil;
    if assigned(imageAttributes) then nimageAttributes := imageAttributes.nativeImageAttr else nimageAttributes := nil;
    result := SetStatus(GdipDrawImageRectRectI(nativeGraphics,
                                nimage,
                                destRect.X,
                                destRect.Y,
                                destRect.Width,
                                destRect.Height,
                                srcx,
                                srcy,
                                srcwidth,
                                srcheight,
                                srcUnit,
                                nimageAttributes,
                                callback,
                                callbackData));
  end;

  function TGPGraphics.DrawImage(image: TGPImage; destPoints: PGPPoint;
       count, srcx, srcy, srcwidth, srcheight: Integer; srcUnit: TUnit;
       imageAttributes: TGPImageAttributes = nil; callback: DrawImageAbort = nil;
       callbackData: Pointer = nil): TStatus;
  var
    nImage: GpImage;
    nimageAttributes: GpimageAttributes;
  begin
    if assigned(Image) then nImage := Image.nativeImage else nImage := nil;
    if assigned(imageAttributes) then nimageAttributes := imageAttributes.nativeImageAttr else nimageAttributes := nil;

    result := SetStatus(GdipDrawImagePointsRectI(nativeGraphics,
                                  nimage,
                                  destPoints,
                                  count,
                                  srcx,
                                  srcy,
                                  srcwidth,
                                  srcheight,
                                  srcUnit,
                                  nimageAttributes,
                                  callback,
                                  callbackData));
  end;

    
  function TGPGraphics.EnumerateMetafile(metafile: TGPMetafile; const destPoint: TGPPointF;
      callback: EnumerateMetafileProc; callbackData: Pointer = nil;
      imageAttributes: TGPImageAttributes = nil): TStatus;
  var
    nMetafile: GpMetafile;
    nimageAttributes: GpimageAttributes;
  begin
    if assigned(Metafile) then nMetafile := GpMetafile(Metafile.nativeImage) else nMetafile := nil;
    if assigned(imageAttributes) then nimageAttributes := imageAttributes.nativeImageAttr else nimageAttributes := nil;
    result := SetStatus(GdipEnumerateMetafileDestPoint(
            nativeGraphics,
            nmetafile,
            @destPoint,
            callback,
            callbackData,
            nimageAttributes));
  end;

    
  function TGPGraphics.EnumerateMetafile(metafile: TGPMetafile; const destPoint: TGPPoint;
       callback: EnumerateMetafileProc; callbackData: pointer = nil;
       imageAttributes: TGPImageAttributes = nil): TStatus;
  var
    nMetafile: GpMetafile;
    nimageAttributes: GpimageAttributes;
  begin
    if assigned(Metafile) then nMetafile := GpMetafile(Metafile.nativeImage) else nMetafile := nil;
    if assigned(imageAttributes) then nimageAttributes := imageAttributes.nativeImageAttr else nimageAttributes := nil;
    result := SetStatus(GdipEnumerateMetafileDestPointI(
            nativeGraphics,
            nmetafile,
            @destPoint,
            callback,
            callbackData,
            nimageAttributes));
  end;


  function TGPGraphics.EnumerateMetafile(metafile: TGPMetafile; const destRect: TGPRectF;
       callback: EnumerateMetafileProc; callbackData: Pointer = nil;
       imageAttributes: TGPImageAttributes = nil): TStatus;
  var
    nMetafile: GpMetafile;
    nimageAttributes: GpimageAttributes;
  begin
    if assigned(Metafile) then nMetafile := GpMetafile(Metafile.nativeImage) else nMetafile := nil;
    if assigned(imageAttributes) then nimageAttributes := imageAttributes.nativeImageAttr else nimageAttributes := nil;
    result := SetStatus(GdipEnumerateMetafileDestRect(
            nativeGraphics,
            nmetafile,
            @destRect,
            callback,
            callbackData,
            nimageAttributes));
  end;


  function TGPGraphics.EnumerateMetafile(metafile: TGPMetafile; const destRect: TGPRect;
       callback: EnumerateMetafileProc; callbackData: Pointer = nil;
       imageAttributes: TGPImageAttributes = nil): TStatus;
  var
    nMetafile: GpMetafile;
    nimageAttributes: GpimageAttributes;
  begin
    if assigned(Metafile) then nMetafile := GpMetafile(Metafile.nativeImage) else nMetafile := nil;
    if assigned(imageAttributes) then nimageAttributes := imageAttributes.nativeImageAttr else nimageAttributes := nil;
    result := SetStatus(GdipEnumerateMetafileDestRectI(
            nativeGraphics,
            nmetafile,
            @destRect,
            callback,
            callbackData,
            nimageAttributes));
  end;


  function TGPGraphics.EnumerateMetafile(metafile: TGPMetafile; destPoints: PGPPointF;
       count: Integer; callback: EnumerateMetafileProc; callbackData: Pointer = nil;
       imageAttributes: TGPImageAttributes = nil): TStatus;
  var
    nMetafile: GpMetafile;
    nimageAttributes: GpimageAttributes;
  begin
    if assigned(Metafile) then nMetafile := GpMetafile(Metafile.nativeImage) else nMetafile := nil;
    if assigned(imageAttributes) then nimageAttributes := imageAttributes.nativeImageAttr else nimageAttributes := nil;
    result := SetStatus(GdipEnumerateMetafileDestPoints(
            nativeGraphics,
            nmetafile,
            destPoints,
            count,
            callback,
            callbackData,
            nimageAttributes));
  end;

    
  function TGPGraphics.EnumerateMetafile(metafile: TGPMetafile; destPoints: PGPPoint;
       count: Integer; callback: EnumerateMetafileProc; callbackData: Pointer = nil;
       imageAttributes: TGPImageAttributes = nil): TStatus;
  var
    nMetafile: GpMetafile;
    nimageAttributes: GpimageAttributes;
  begin
    if assigned(Metafile) then nMetafile := GpMetafile(Metafile.nativeImage) else nMetafile := nil;
    if assigned(imageAttributes) then nimageAttributes := imageAttributes.nativeImageAttr else nimageAttributes := nil;
    result := SetStatus(GdipEnumerateMetafileDestPointsI(
            nativeGraphics,
            nmetafile,
            destPoints,
            count,
            callback,
            callbackData,
            nimageAttributes));
  end;


  function TGPGraphics.EnumerateMetafile(metafile: TGPMetafile; const destPoint: TGPPointF;
       const srcRect: TGPRectF; srcUnit: TUnit; callback: EnumerateMetafileProc;
       callbackData: pointer = nil; imageAttributes: TGPImageAttributes = nil): TStatus;
  var
    nMetafile: GpMetafile;
    nimageAttributes: GpimageAttributes;
  begin
    if assigned(Metafile) then nMetafile := GpMetafile(Metafile.nativeImage) else nMetafile := nil;
    if assigned(imageAttributes) then nimageAttributes := imageAttributes.nativeImageAttr else nimageAttributes := nil;
    result := SetStatus(GdipEnumerateMetafileSrcRectDestPoint(
            nativeGraphics,
            nmetafile,
            @destPoint,
            @srcRect,
            srcUnit,
            callback,
            callbackData,
            nimageAttributes));
  end;

    
  function TGPGraphics.EnumerateMetafile(metafile : TGPMetafile; const destPoint : TGPPoint;
       const srcRect : TGPRect; srcUnit : TUnit; callback : EnumerateMetafileProc;
       callbackData : Pointer = nil; imageAttributes : TGPImageAttributes = nil): TStatus;
  var
    nMetafile: GpMetafile;
    nimageAttributes: GpimageAttributes;
  begin
    if assigned(Metafile) then nMetafile := GpMetafile(Metafile.nativeImage) else nMetafile := nil;
    if assigned(imageAttributes) then nimageAttributes := imageAttributes.nativeImageAttr else nimageAttributes := nil;
    result := SetStatus(GdipEnumerateMetafileSrcRectDestPointI(
            nativeGraphics,
            nmetafile,
            @destPoint,
            @srcRect,
            srcUnit,
            callback,
            callbackData,
            nimageAttributes));
  end;


  function TGPGraphics.EnumerateMetafile(metafile: TGPMetafile; const destRect: TGPRectF;
       const srcRect: TGPRectF; srcUnit: TUnit; callback: EnumerateMetafileProc;
       callbackData: Pointer = nil; imageAttributes: TGPImageAttributes = nil): TStatus;
  var
    nMetafile: GpMetafile;
    nimageAttributes: GpimageAttributes;
  begin
    if assigned(Metafile) then nMetafile := GpMetafile(Metafile.nativeImage) else nMetafile := nil;
    if assigned(imageAttributes) then nimageAttributes := imageAttributes.nativeImageAttr else nimageAttributes := nil;
    result := SetStatus(GdipEnumerateMetafileSrcRectDestRect(
            nativeGraphics,
            nmetafile,
            @destRect,
            @srcRect,
            srcUnit,
            callback,
            callbackData,
            nimageAttributes));
  end;


  function TGPGraphics.EnumerateMetafile(metafile : TGPMetafile; const destRect, srcRect: TGPRect;
       srcUnit : TUnit; callback : EnumerateMetafileProc; callbackData : Pointer = nil;
       imageAttributes : TGPImageAttributes = nil): TStatus;
  var
    nMetafile: GpMetafile;
    nimageAttributes: GpimageAttributes;
  begin
    if assigned(Metafile) then nMetafile := GpMetafile(Metafile.nativeImage) else nMetafile := nil;
    if assigned(imageAttributes) then nimageAttributes := imageAttributes.nativeImageAttr else nimageAttributes := nil;
    result := SetStatus(GdipEnumerateMetafileSrcRectDestRectI(
            nativeGraphics,
            nmetafile,
            @destRect,
            @srcRect,
            srcUnit,
            callback,
            callbackData,
            nimageAttributes));
  end;

    
  function TGPGraphics.EnumerateMetafile( metafile: TGPMetafile; destPoints: PGPPointF;
    count: Integer; const srcRect: TGPRectF; srcUnit: TUnit; callback: EnumerateMetafileProc;
    callbackData: Pointer = nil; imageAttributes: TGPImageAttributes = nil): TStatus;
  var
    nMetafile: GpMetafile;
    nimageAttributes: GpimageAttributes;
  begin
    if assigned(Metafile) then nMetafile := GpMetafile(Metafile.nativeImage) else nMetafile := nil;
    if assigned(imageAttributes) then nimageAttributes := imageAttributes.nativeImageAttr else nimageAttributes := nil;
    result := SetStatus(GdipEnumerateMetafileSrcRectDestPoints(
            nativeGraphics,
            nmetafile,
            destPoints,
            count,
            @srcRect,
            srcUnit,
            callback,
            callbackData,
            nimageAttributes));
  end;


  function TGPGraphics.EnumerateMetafile(metafile: TGPMetafile; destPoints: PGPPoint;
    count: Integer; const srcRect: TGPRect; srcUnit: TUnit; callback: EnumerateMetafileProc;
    callbackData: Pointer = nil; imageAttributes: TGPImageAttributes = nil): TStatus;
  var
    nMetafile: GpMetafile;
    nimageAttributes: GpimageAttributes;
  begin
    if assigned(Metafile) then nMetafile := GpMetafile(Metafile.nativeImage) else nMetafile := nil;
    if assigned(imageAttributes) then nimageAttributes := imageAttributes.nativeImageAttr else nimageAttributes := nil;
    result := SetStatus(GdipEnumerateMetafileSrcRectDestPointsI(
            nativeGraphics,
            nmetafile,
            destPoints,
            count,
            @srcRect,
            srcUnit,
            callback,
            callbackData,
            nimageAttributes));
  end;
    
  function TGPGraphics.SetClip(g: TGPGraphics; combineMode: TCombineMode = CombineModeReplace): TStatus;
  begin
    result := SetStatus(GdipSetClipGraphics(nativeGraphics,
                             g.nativeGraphics,
                             combineMode));
  end;

  function TGPGraphics.SetClip(rect: TGPRectF; combineMode: TCombineMode = CombineModeReplace): TStatus;
  begin
    result := SetStatus(GdipSetClipRect(nativeGraphics,
                             rect.X, rect.Y,
                             rect.Width, rect.Height,
                             combineMode));
  end;

  function TGPGraphics.SetClip(rect: TGPRect; combineMode: TCombineMode = CombineModeReplace): TStatus;
  begin
    result := SetStatus(GdipSetClipRectI(nativeGraphics,
                              rect.X, rect.Y,
                              rect.Width, rect.Height,
                              combineMode));
  end;

  function TGPGraphics.SetClip(path: TGPGraphicsPath; combineMode: TCombineMode = CombineModeReplace): TStatus;
  begin
    result := SetStatus(GdipSetClipPath(nativeGraphics,
                             path.nativePath,
                             combineMode));
  end;

  function TGPGraphics.SetClip(region: TGPRegion; combineMode: TCombineMode = CombineModeReplace): TStatus;
  begin
    result := SetStatus(GdipSetClipRegion(nativeGraphics,
                               region.nativeRegion,
                               combineMode));
  end;

  function TGPGraphics.SetClip(hRgn: HRGN; combineMode: TCombineMode = CombineModeReplace): TStatus;
  begin
    result := SetStatus(GdipSetClipHrgn(nativeGraphics, hRgn,
                             combineMode));
  end;

  function TGPGraphics.IntersectClip(const rect: TGPRectF): TStatus;
  begin
    result := SetStatus(GdipSetClipRect(nativeGraphics,
                             rect.X, rect.Y,
                             rect.Width, rect.Height,
                             CombineModeIntersect));
  end;

  function TGPGraphics.IntersectClip(const rect: TGPRect): TStatus;
  begin
    result := SetStatus(GdipSetClipRectI(nativeGraphics,
                              rect.X, rect.Y,
                              rect.Width, rect.Height,
                              CombineModeIntersect));
  end;

  function TGPGraphics.IntersectClip(region: TGPRegion): TStatus;
  begin
    result := SetStatus(GdipSetClipRegion(nativeGraphics,
                               region.nativeRegion,
                               CombineModeIntersect));
  end;

  function TGPGraphics.ExcludeClip(const rect: TGPRectF): TStatus;
  begin
    result := SetStatus(GdipSetClipRect(nativeGraphics,
                             rect.X, rect.Y,
                             rect.Width, rect.Height,
                             CombineModeExclude));
  end;

  function TGPGraphics.ExcludeClip(const rect: TGPRect): TStatus;
  begin
    result := SetStatus(GdipSetClipRectI(nativeGraphics,
                              rect.X, rect.Y,
                              rect.Width, rect.Height,
                              CombineModeExclude));
  end;

  function TGPGraphics.ExcludeClip(region: TGPRegion): TStatus;
  begin
    result := SetStatus(GdipSetClipRegion(nativeGraphics,
                               region.nativeRegion,
                               CombineModeExclude));
  end;

  function TGPGraphics.ResetClip: TStatus;
  begin
    result := SetStatus(GdipResetClip(nativeGraphics));
  end;

  function TGPGraphics.TranslateClip(dx, dy: Single): TStatus;
  begin
    result := SetStatus(GdipTranslateClip(nativeGraphics, dx, dy));
  end;

  function TGPGraphics.TranslateClip(dx, dy: Integer): TStatus;
  begin
    result := SetStatus(GdipTranslateClipI(nativeGraphics,
                            dx, dy));
  end;

  function TGPGraphics.GetClip(region: TGPRegion): TStatus;
  begin
    result := SetStatus(GdipGetClip(nativeGraphics,
                         region.nativeRegion));
  end;

  function TGPGraphics.GetClipBounds(out rect: TGPRectF): TStatus;
  begin
    result := SetStatus(GdipGetClipBounds(nativeGraphics, @rect));
  end;

  function TGPGraphics.GetClipBounds(out rect: TGPRect): TStatus;
  begin
    result := SetStatus(GdipGetClipBoundsI(nativeGraphics, @rect));
  end;

  function TGPGraphics.IsClipEmpty: Bool;
  var booln: BOOL;
  begin
    booln := FALSE;
    SetStatus(GdipIsClipEmpty(nativeGraphics, @booln));
    result := booln;
  end;

  function TGPGraphics.GetVisibleClipBounds(out rect: TGPRectF): TStatus;
  begin
    result := SetStatus(GdipGetVisibleClipBounds(nativeGraphics, @rect));
  end;

  function TGPGraphics.GetVisibleClipBounds(out rect: TGPRect): TStatus;
  begin
    result := SetStatus(GdipGetVisibleClipBoundsI(nativeGraphics, @rect));
  end;

  function TGPGraphics.IsVisibleClipEmpty: BOOL;
  var booln: BOOL;
  begin
    booln := FALSE;
    SetStatus(GdipIsVisibleClipEmpty(nativeGraphics, booln));
    result := booln;
  end;

  function TGPGraphics.IsVisible(x, y: Integer): BOOL;
  var pt: TGPPoint;
  begin
    pt.X := x; pt.Y := y;
    result := IsVisible(pt);
  end;

  function TGPGraphics.IsVisible(const point: TGPPoint): BOOL;
  var booln: BOOL;
  begin
    booln := FALSE;
    SetStatus(GdipIsVisiblePointI(nativeGraphics,
                          point.X,
                          point.Y,
                          booln));
    result := booln;
  end;

  function TGPGraphics.IsVisible(x, y, width, height: Integer): BOOL;
  var booln: BOOL;
  begin
    booln := TRUE;
    SetStatus(GdipIsVisibleRectI(nativeGraphics,
                         X,
                         Y,
                         Width,
                         Height,
                         booln));
    result := booln;
  end;

  function TGPGraphics.IsVisible(const rect: TGPRect): BOOL;
  var booln: BOOL;
  begin
    booln := TRUE;
    SetStatus(GdipIsVisibleRectI(nativeGraphics,
                         rect.X,
                         rect.Y,
                         rect.Width,
                         rect.Height,
                         booln));
    result := booln;
  end;

  function TGPGraphics.IsVisible(x, y: Single): BOOL;
  var booln: BOOL;
  begin
    booln := FALSE;
    SetStatus(GdipIsVisiblePoint(nativeGraphics,
                         X,
                         Y,
                         booln));

    result := booln;
  end;

  function TGPGraphics.IsVisible(const point: TGPPointF): BOOL;
  var booln: BOOL;
  begin
    booln := FALSE;
    SetStatus(GdipIsVisiblePoint(nativeGraphics,
                         point.X,
                         point.Y,
                         booln));

    result := booln;
  end;

  function TGPGraphics.IsVisible(x, y, width, height: Single): BOOL;
  var booln: BOOL;
  begin
    booln := TRUE;
    SetStatus(GdipIsVisibleRect(nativeGraphics,
                        X,
                        Y,
                        Width,
                        Height,
                        booln));
    result := booln;
  end;

  function TGPGraphics.IsVisible(const rect: TGPRectF): BOOL;
  var booln: BOOL;
  begin
    booln := TRUE;
    SetStatus(GdipIsVisibleRect(nativeGraphics,
                        rect.X,
                        rect.Y,
                        rect.Width,
                        rect.Height,
                        booln));
    result := booln;
  end;

  function TGPGraphics.Save: GraphicsState;
  begin
    SetStatus(GdipSaveGraphics(nativeGraphics, result));
  end;

  function TGPGraphics.Restore(gstate: GraphicsState): TStatus;
  begin
    result := SetStatus(GdipRestoreGraphics(nativeGraphics,
                             gstate));
  end;

  function TGPGraphics.BeginContainer(const dstrect,srcrect: TGPRectF; unit_: TUnit): GraphicsContainer;
  begin
    SetStatus(GdipBeginContainer(nativeGraphics, @dstrect,
                         @srcrect, unit_, result));
  end;

  function TGPGraphics.BeginContainer(const dstrect, srcrect: TGPRect; unit_: TUnit): GraphicsContainer;
  begin
    SetStatus(GdipBeginContainerI(nativeGraphics, @dstrect,
                          @srcrect, unit_, result));
  end;

  function TGPGraphics.BeginContainer: GraphicsContainer;
  begin
    SetStatus(GdipBeginContainer2(nativeGraphics, result));
  end;

  function TGPGraphics.EndContainer(state: GraphicsContainer): TStatus;
  begin
    result := SetStatus(GdipEndContainer(nativeGraphics, state));
  end;

  function TGPGraphics.AddMetafileComment(data: PBYTE; sizeData: UINT): TStatus;
  begin
    result := SetStatus(GdipComment(nativeGraphics, sizeData, data));
  end;

  function TGPGraphics.GetHalftonePalette: HPALETTE;
  begin
    result := GdipCreateHalftonePalette;
  end;

  function TGPGraphics.GetLastStatus: TStatus;
  begin
    result := lastResult;
    lastResult := Ok;
  end;

  constructor TGPGraphics.Create(graphics: GpGraphics);
  begin
    lastResult := Ok;
    SetNativeGraphics(graphics);
  end;

  procedure TGPGraphics.SetNativeGraphics(graphics: GpGraphics);
  begin
    self.nativeGraphics := graphics;
  end;

  function TGPGraphics.SetStatus(status: TStatus): TStatus;
  begin
    if (status <> Ok) then lastResult := status;
    result := status;
  end;

  function TGPGraphics.GetNativeGraphics: GpGraphics;
  begin
    result := self.nativeGraphics;
  end;

  function TGPGraphics.GetNativePen(pen: TGPPen): GpPen;
  begin
    result := pen.nativePen;
  end;

  constructor TGPFontFamily.Create;
  begin
    nativeFamily := nil;
    lastResult   := Ok;
  end;

  constructor TGPFontFamily.Create(name: WideString; fontCollection: TGPFontCollection = nil);
  var nfontCollection: GpfontCollection;
  begin
    nativeFamily := nil;
    if assigned(fontCollection) then nfontCollection := fontCollection.nativeFontCollection else nfontCollection := nil;
    lastResult := GdipCreateFontFamilyFromName(PWideChar(name), nfontCollection, nativeFamily);
  end;

  destructor TGPFontFamily.Destroy;
  begin
    GdipDeleteFontFamily (nativeFamily);
  end;

  class function TGPFontFamily.GenericSansSerif: TGPFontFamily;
  var
    nFontFamily: GpFontFamily;
  begin
    if (GenericSansSerifFontFamily <> nil) then
    begin
      result := GenericSansSerifFontFamily;
      exit;
    end;
    GenericSansSerifFontFamily := TGPFontFamily.Create;
    GenericSansSerifFontFamily.lastResult := GdipGetGenericFontFamilySansSerif(nFontFamily);
    GenericSansSerifFontFamily.nativeFamily := nFontFamily;
    result := GenericSansSerifFontFamily;
  end;

  class function TGPFontFamily.GenericSerif: TGPFontFamily;
  var nFontFamily: GpFontFamily;
  begin
    if (GenericSerifFontFamily <> nil) then
    begin
      result := GenericSerifFontFamily;
      exit;
    end;

    GenericSerifFontFamily := TGPFontFamily.Create;// (GenericSerifFontFamilyBuffer);
    GenericSerifFontFamily.lastResult := GdipGetGenericFontFamilySerif(nFontFamily);
    GenericSerifFontFamily.nativeFamily := nFontFamily;
    result := GenericSerifFontFamily;
  end;

  class function TGPFontFamily.GenericMonospace: TGPFontFamily;
  var nFontFamily: GpFontFamily;
  begin
    if (GenericMonospaceFontFamily <> nil) then
    begin
      result := GenericMonospaceFontFamily;
      exit;
    end;
    GenericMonospaceFontFamily := TGPFontFamily.Create;// (GenericMonospaceFontFamilyBuffer);
    GenericMonospaceFontFamily.lastResult := GdipGetGenericFontFamilyMonospace(nFontFamily);
    GenericMonospaceFontFamily.nativeFamily := nFontFamily;
    result := GenericMonospaceFontFamily;
  end;

  function TGPFontFamily.GetFamilyName(out name: string; language: LANGID = 0): TStatus;
  var str: array[0..LF_FACESIZE - 1] of WideChar;
  begin
    result := SetStatus(GdipGetFamilyName(nativeFamily, @str, language));
    name := str;
  end;

  function TGPFontFamily.Clone: TGPFontFamily;
  var
    clonedFamily: GpFontFamily;
  begin
    clonedFamily := nil;
    SetStatus(GdipCloneFontFamily (nativeFamily, clonedFamily));
    result := TGPFontFamily.Create(clonedFamily, lastResult);
  end;

  function TGPFontFamily.IsAvailable: BOOL;
  begin
    result := (nativeFamily <> nil);
  end;

  function TGPFontFamily.IsStyleAvailable(style: Integer): BOOL;
  var
    StyleAvailable: BOOL;
    status: TStatus;
  begin
    status := SetStatus(GdipIsStyleAvailable(nativeFamily, style, StyleAvailable));
    if (status <> Ok) then StyleAvailable := FALSE;
    result := StyleAvailable;
  end;

  function TGPFontFamily.GetEmHeight(style: Integer): UINT16;
  begin
    SetStatus(GdipGetEmHeight(nativeFamily, style, result));
  end;

  function TGPFontFamily.GetCellAscent(style: Integer): UINT16;
  begin
    SetStatus(GdipGetCellAscent(nativeFamily, style, result));
  end;

  function TGPFontFamily.GetCellDescent(style: Integer): UINT16;
  begin
    SetStatus(GdipGetCellDescent(nativeFamily, style, result));
  end;

  function TGPFontFamily.GetLineSpacing(style: Integer): UINT16;
  begin
    SetStatus(GdipGetLineSpacing(nativeFamily, style, result));
  end;

  function TGPFontFamily.GetLastStatus: TStatus;
  begin
    result := lastResult;
    lastResult := Ok;
  end;

  function TGPFontFamily.SetStatus(status: TStatus): TStatus;
  begin
    if (status <> Ok) then lastResult := status;
    result := status;
  end;

  constructor TGPFontFamily.Create(nativeOrig: GpFontFamily; status: TStatus);
  begin
    lastResult  := status;
    nativeFamily := nativeOrig;
  end;

(**************************************************************************\
*
*   GDI+ Font class
*
\**************************************************************************)

  constructor TGPFont.Create(hdc: HDC);
  var font: GpFont;
  begin
    font := nil;
    lastResult := GdipCreateFontFromDC(hdc, font);
    SetNativeFont(font);
  end;

  constructor TGPFont.Create(hdc: HDC; logfont: PLogFontA);
  var font: GpFont;
  begin
    font := nil;
    if assigned(logfont) then
      lastResult := GdipCreateFontFromLogfontA(hdc, logfont, font)
    else
      lastResult := GdipCreateFontFromDC(hdc, font);
    SetNativeFont(font);
  end;

  constructor TGPFont.Create(hdc: HDC; logfont: PLogFontW);
  var font: GpFont;
  begin
    font := nil;
    if assigned(logfont) then
      lastResult := GdipCreateFontFromLogfontW(hdc, logfont, font)
    else
      lastResult := GdipCreateFontFromDC(hdc, font);
    SetNativeFont(font);
  end;

  constructor TGPFont.Create(hdc: HDC; hfont: HFONT);
  var
    font: GpFont;
    {$IFNDEF FPC}
    lf: LOGFONTA;
    {$ENDIF}
  begin
    font := nil;
    {$IFNDEF FPC}
    if BOOL(hfont) then
    begin
      if( BOOL(GetObjectA(hfont, sizeof(LOGFONTA), @lf))) then
            lastResult := GdipCreateFontFromLogfontA(hdc, @lf, font)
        else
            lastResult := GdipCreateFontFromDC(hdc, font);
    end
    else
      lastResult := GdipCreateFontFromDC(hdc, font);
    SetNativeFont(font);
    {$ENDIF}
  end;

  constructor TGPFont.Create(family: TGPFontFamily; emSize: Single;
      style: TFontStyle = FontStyleRegular; unit_: TUnit = UnitPoint);
  var
    font: GpFont;
    nFontFamily: GpFontFamily;
  begin
    font := nil;
    if assigned(Family) then nFontFamily := Family.nativeFamily else nFontFamily := nil;
    lastResult := GdipCreateFont(nFontFamily, emSize, Integer(style), Integer(unit_), font);
    SetNativeFont(font);
  end;

  constructor TGPFont.Create(familyName: WideString; emSize: Single;
      style: TFontStyle = FontStyleRegular; unit_: TUnit = UnitPoint;
      fontCollection: TGPFontCollection = nil);
  var
    family: TGPFontFamily;
    nativeFamily: GpFontFamily;
  begin
    nativeFont := nil;
    family := TGPFontFamily.Create(familyName, fontCollection);
    nativeFamily := family.nativeFamily;
    lastResult := family.GetLastStatus;
    if (lastResult <> Ok) then
    begin
      nativeFamily := TGPFontFamily.GenericSansSerif.nativeFamily;
      lastResult := TGPFontFamily.GenericSansSerif.lastResult;
      if (lastResult <> Ok) then
      begin
        family.Free;
        exit;
      end;
    end;

    lastResult := GdipCreateFont(nativeFamily,
                            emSize,
                            integer(style),
                            integer(unit_),
                            nativeFont);

    if (lastResult <> Ok) then
      begin
        nativeFamily := TGPFontFamily.GenericSansSerif.nativeFamily;
        lastResult := TGPFontFamily.GenericSansSerif.lastResult;
        if (lastResult <> Ok) then
        begin
          family.Free;
          exit;
        end;

        lastResult := GdipCreateFont(
            nativeFamily,
            emSize,
            Integer(style),
            Integer(unit_),
            nativeFont);
      end;
      family.Free;
  end;

  function TGPFont.GetLogFontA(g: TGPGraphics; out logfontA: TLogFontA): TStatus;
  var nGraphics: GpGraphics;
  begin
    if assigned(g) then nGraphics := g.nativeGraphics else nGraphics := nil;
    {$IFNDEF FPC}
    result := SetStatus(GdipGetLogFontA(nativeFont, nGraphics, logfontA));
    {$ENDIF}
  end;

  function TGPFont.GetLogFontW(g: TGPGraphics; out logfontW: TLogFontW): TStatus;
  var nGraphics: GpGraphics;
  begin
    if assigned(g) then nGraphics := g.nativeGraphics else nGraphics := nil;
    result := SetStatus(GdipGetLogFontW(nativeFont, nGraphics, logfontW));
  end;

  function TGPFont.Clone: TGPFont;
  var cloneFont: GpFont;
  begin
    cloneFont := nil;
    SetStatus(GdipCloneFont(nativeFont, cloneFont));
    result := TGPFont.Create(cloneFont, lastResult);
  end;

  destructor TGPFont.Destroy;
  begin
    GdipDeleteFont(nativeFont);
  end;

  function TGPFont.IsAvailable: BOOL;
  begin
    result := (nativeFont <> nil);
  end;

  function TGPFont.GetStyle: Integer;
  begin
    SetStatus(GdipGetFontStyle(nativeFont, result));
  end;

  function TGPFont.GetSize: Single;
  begin
    SetStatus(GdipGetFontSize(nativeFont, result));
  end;

  function TGPFont.GetUnit: TUnit;
  begin
    SetStatus(GdipGetFontUnit(nativeFont, result));
  end;

  function TGPFont.GetLastStatus: TStatus;
  begin
    result := lastResult;
  end;

  function TGPFont.GetHeight(graphics: TGPGraphics): Single;
  var ngraphics: Gpgraphics;
  begin
    if assigned(graphics) then ngraphics := graphics.nativeGraphics else ngraphics := nil;
    SetStatus(GdipGetFontHeight(nativeFont, ngraphics, result));
  end;

  function TGPFont.GetHeight(dpi: Single): Single;
  begin
    SetStatus(GdipGetFontHeightGivenDPI(nativeFont, dpi, result));
  end;

  function TGPFont.GetFamily(family: TGPFontFamily): TStatus;
  var
    status: TStatus;
    nFamily: GpFontFamily;
  begin
    if (family = nil) then
    begin
      result := SetStatus(InvalidParameter);
      exit;
    end;

    status := GdipGetFamily(nativeFont, nFamily);
    family.nativeFamily := nFamily;
    family.SetStatus(status);
    result := SetStatus(status);
  end;

  constructor TGPFont.Create(font: GpFont; status: TStatus);
  begin
    lastResult := status;
    SetNativeFont(font);
  end;

  procedure TGPFont.SetNativeFont(Font: GpFont);
  begin
    nativeFont := Font;
  end;

  function TGPFont.SetStatus(status: TStatus): TStatus;
  begin
    if (status <> Ok) then lastResult := status;
    result := status;
  end;

(**************************************************************************\
*
*   Font collections (Installed and Private)
*
\**************************************************************************)

  constructor TGPFontCollection.Create;
  begin
    nativeFontCollection := nil;
  end;

  destructor TGPFontCollection.Destroy;
  begin
    inherited Destroy;
  end;

  function TGPFontCollection.GetFamilyCount: Integer;
  var
    numFound: Integer;
  begin
    numFound := 0;
    lastResult := GdipGetFontCollectionFamilyCount(nativeFontCollection, numFound);
    result := numFound;
  end;

  function TGPFontCollection.GetFamilies(numSought: Integer; out gpfamilies: array of TGPFontFamily;
      out numFound: Integer): TStatus;
  var
    nativeFamilyList: Pointer;
    Status: TStatus;
    i: Integer;
  type
    ArrGpFontFamily = array of GpFontFamily;
  begin
    if ((numSought <= 0) or (length(gpfamilies) = 0)) then
    begin
      result := SetStatus(InvalidParameter);
      exit;
    end;
    numFound := 0;

    getMem(nativeFamilyList, numSought * SizeOf(GpFontFamily));
    if nativeFamilyList = nil then
    begin
      result := SetStatus(OutOfMemory);
      exit;
    end;

    status := SetStatus(GdipGetFontCollectionFamilyList(
        nativeFontCollection,
        numSought,
        nativeFamilyList,
        numFound
    ));
    
    if (status = Ok) then
      for i := 0 to numFound - 1 do
         GdipCloneFontFamily(ArrGpFontFamily(nativeFamilyList)[i], gpfamilies[i].nativeFamily);
    Freemem(nativeFamilyList, numSought * SizeOf(GpFontFamily));
    result := status;
  end;

  function TGPFontCollection.GetLastStatus: TStatus;
  begin
    result := lastResult;
  end;

  function TGPFontCollection.SetStatus(status: TStatus): TStatus;
  begin
    lastResult := status;
    result := lastResult;
  end;


  constructor TGPInstalledFontCollection.Create;
  begin
    nativeFontCollection := nil;
    lastResult := GdipNewInstalledFontCollection(nativeFontCollection);
  end;

  destructor TGPInstalledFontCollection.Destroy;
  begin
    inherited Destroy;
  end;

  constructor TGPPrivateFontCollection.Create;
  begin
    nativeFontCollection := nil;
    lastResult := GdipNewPrivateFontCollection(nativeFontCollection);
  end;

  destructor TGPPrivateFontCollection.destroy;
  begin
    GdipDeletePrivateFontCollection(nativeFontCollection);
    inherited Destroy;
  end;

  function TGPPrivateFontCollection.AddFontFile(filename: WideString): TStatus;
  begin
    result := SetStatus(GdipPrivateAddFontFile(nativeFontCollection, PWideChar(filename)));
  end;

  function TGPPrivateFontCollection.AddMemoryFont(memory: Pointer; length: Integer): TStatus;
  begin
    result := SetStatus(GdipPrivateAddMemoryFont(
        nativeFontCollection,
        memory,
        length));
  end;

  constructor TGPGraphicsPath.Create(fillMode: TFillMode = FillModeAlternate);
  begin
    nativePath := nil;
    lastResult := GdipCreatePath(fillMode, nativePath);
  end;

  constructor TGPGraphicsPath.Create(points: PGPPointF; types: PBYTE; count: Integer;
              fillMode: TFillMode = FillModeAlternate);
  begin
    nativePath := nil;
    lastResult := GdipCreatePath2(points, types, count, fillMode, nativePath);
  end;

  constructor TGPGraphicsPath.Create(points: PGPPoint; types: PBYTE; count: Integer;
              fillMode: TFillMode = FillModeAlternate);
  begin
      nativePath := nil;
      lastResult := GdipCreatePath2I(points, types, count, fillMode, nativePath);
  end;

  destructor TGPGraphicsPath.destroy;
  begin
    GdipDeletePath(nativePath);
  end;

  function TGPGraphicsPath.Clone: TGPGraphicsPath;
  var
    clonepath: GpPath;
  begin
    clonepath := nil;
    SetStatus(GdipClonePath(nativePath, clonepath));
    result := TGPGraphicsPath.Create(clonepath);
  end;

  function TGPGraphicsPath.Reset: TStatus;
  begin
    result := SetStatus(GdipResetPath(nativePath));
  end;

  function TGPGraphicsPath.GetFillMode: TFillMode;
  var FMode: TFillMode;
  begin
    FMode := FillModeAlternate;
    SetStatus(GdipGetPathFillMode(nativePath, result));
    result := FMode;
  end;

  function TGPGraphicsPath.SetFillMode(fillmode: TFillMode): TStatus;
  begin
    result := SetStatus(GdipSetPathFillMode(nativePath, fillmode));
  end;

  function TGPGraphicsPath.GetPathData(pathData: TPathData): TStatus;
  var
    count: Integer;
  begin
    count := GetPointCount;
    if ((count <= 0) or ((pathData.Count > 0) and (pathData.Count < Count))) then
    begin
      pathData.Count := 0;
      if assigned(pathData.Points) then
      begin
        FreeMem(pathData.Points);
        pathData.Points := nil;
      end;
      if assigned(pathData.Types) then
      begin
        freemem(pathData.Types);
        pathData.Types := nil;
      end;
      if (count <= 0) then
      begin
        result := lastResult;
        exit;
      end;
    end;

    if (pathData.Count = 0) then
    begin
      getmem(pathData.Points, SizeOf(TGPPointF) * count);
      if (pathData.Points = nil) then
      begin
        result := SetStatus(OutOfMemory);
        exit;
      end;
      Getmem(pathData.Types, count);
      if (pathData.Types = nil) then
      begin
        freemem(pathData.Points);
        pathData.Points := nil;
        result := SetStatus(OutOfMemory);
        exit;
      end;
      pathData.Count := count;
    end;

    result := SetStatus(GdipGetPathData(nativePath, @pathData.Count));
  end;

  function TGPGraphicsPath.StartFigure: TStatus;
  begin
    result := SetStatus(GdipStartPathFigure(nativePath));
  end;

  function TGPGraphicsPath.CloseFigure: TStatus;
  begin
    result := SetStatus(GdipClosePathFigure(nativePath));
  end;

  function TGPGraphicsPath.CloseAllFigures: TStatus;
  begin
    result := SetStatus(GdipClosePathFigures(nativePath));
  end;

  function TGPGraphicsPath.SetMarker: TStatus;
  begin
    result := SetStatus(GdipSetPathMarker(nativePath));
  end;

  function TGPGraphicsPath.ClearMarkers: TStatus;
  begin
    result := SetStatus(GdipClearPathMarkers(nativePath));
  end;

  function TGPGraphicsPath.Reverse: TStatus;
  begin
    result := SetStatus(GdipReversePath(nativePath));
  end;

  function TGPGraphicsPath.GetLastPoint(out lastPoint: TGPPointF): TStatus;
  begin
    result := SetStatus(GdipGetPathLastPoint(nativePath,
                                            @lastPoint));
  end;

  function TGPGraphicsPath.AddLine(const pt1, pt2: TGPPointF): TStatus;
  begin
    result := AddLine(pt1.X, pt1.Y, pt2.X, pt2.Y);
  end;

  function TGPGraphicsPath.AddLine(x1, y1, x2, y2: Single): TStatus;
  begin
    result := SetStatus(GdipAddPathLine(nativePath, x1, y1,
                                         x2, y2));
  end;

  function TGPGraphicsPath.AddLines(points: PGPPointF; count: Integer): TStatus;
  begin
    result := SetStatus(GdipAddPathLine2(nativePath, points, count));
  end;

  function TGPGraphicsPath.AddLine(const pt1, pt2: TGPPoint): TStatus;
  begin
    result := AddLine(pt1.X, pt1.Y, pt2.X, pt2.Y);
  end;

  function TGPGraphicsPath.AddLine(x1, y1, x2, y2: Integer): TStatus;
  begin
    result := SetStatus(GdipAddPathLineI(nativePath, x1, y1, x2, y2));
  end;

  function TGPGraphicsPath.AddLines(points: PGPPoint; count: Integer): TStatus;
  begin
    result := SetStatus(GdipAddPathLine2I(nativePath, points, count));
  end;

  function TGPGraphicsPath.AddArc(rect: TGPRectF; startAngle, sweepAngle: Single): TStatus;
  begin
    result := AddArc(rect.X, rect.Y, rect.Width, rect.Height,
                  startAngle, sweepAngle);
  end;

  function TGPGraphicsPath.AddArc(x, y, width, height, startAngle, sweepAngle: Single): TStatus;
  begin
    result := SetStatus(GdipAddPathArc(nativePath, x, y, width, height, startAngle, sweepAngle));
  end;

  function TGPGraphicsPath.AddArc(rect: TGPRect; startAngle, sweepAngle: Single): TStatus;
  begin
    result := AddArc(rect.X, rect.Y, rect.Width, rect.Height, startAngle, sweepAngle);
  end;

  function TGPGraphicsPath.AddArc(x, y, width, height: Integer; startAngle, sweepAngle: Single): TStatus;
  begin
    result := SetStatus(GdipAddPathArcI(nativePath, x, y, width, height, startAngle, sweepAngle));
  end;

  function TGPGraphicsPath.AddBezier(pt1, pt2, pt3, pt4: TGPPointF): TStatus;
  begin
    result := AddBezier(pt1.X, pt1.Y, pt2.X, pt2.Y, pt3.X, pt3.Y, pt4.X, pt4.Y);
  end;

  function TGPGraphicsPath.AddBezier(x1, y1, x2, y2, x3, y3, x4, y4: Single): TStatus;
  begin
    result := SetStatus(GdipAddPathBezier(nativePath, x1, y1, x2, y2, x3, y3, x4, y4));
  end;

  function TGPGraphicsPath.AddBeziers(points: PGPPointF; count: Integer): TStatus;
  begin
    result := SetStatus(GdipAddPathBeziers(nativePath, points, count));
  end;

  function TGPGraphicsPath.AddBezier(pt1, pt2, pt3, pt4: TGPPoint): TStatus;
  begin
    result := AddBezier(pt1.X, pt1.Y, pt2.X, pt2.Y, pt3.X, pt3.Y, pt4.X, pt4.Y);
  end;

  function TGPGraphicsPath.AddBezier(x1, y1, x2, y2, x3, y3, x4, y4: Integer): TStatus;
  begin
       result := SetStatus(GdipAddPathBezierI(nativePath, x1, y1, x2, y2, x3, y3, x4, y4));
  end;

  function TGPGraphicsPath.AddBeziers(points: PGPPoint; count: Integer): TStatus;
  begin
       result := SetStatus(GdipAddPathBeziersI(nativePath, points, count));
  end;

  function TGPGraphicsPath.AddCurve(points: PGPPointF; count: Integer): TStatus;
  begin
    result := SetStatus(GdipAddPathCurve(nativePath, points, count));
  end;

  function TGPGraphicsPath.AddCurve(points: PGPPointF; count: Integer;
    tension: Single): TStatus;
  begin
    result := SetStatus(GdipAddPathCurve2(nativePath, points, count, tension));
  end;

  function TGPGraphicsPath.AddCurve(points: PGPPointF; count, offset,
    numberOfSegments: Integer; tension: Single): TStatus;
  begin
    result := SetStatus(GdipAddPathCurve3(nativePath, points, count, offset,
                          numberOfSegments, tension));
  end;

  function TGPGraphicsPath.AddCurve(points: PGPPoint; count: Integer): TStatus;
  begin
    result := SetStatus(GdipAddPathCurveI(nativePath, points, count));
  end;

  function TGPGraphicsPath.AddCurve(points: PGPPoint; count: Integer; tension: Single): TStatus;
  begin
    result := SetStatus(GdipAddPathCurve2I(nativePath, points, count, tension));
  end;

  function TGPGraphicsPath.AddCurve(points: PGPPoint; count, offset,
    numberOfSegments: Integer; tension: Single): TStatus;
  begin
    result := SetStatus(GdipAddPathCurve3I(nativePath, points, count, offset,
     numberOfSegments, tension));
  end;

  function TGPGraphicsPath.AddClosedCurve(points: PGPPointF; count: Integer): TStatus;
  begin
    result := SetStatus(GdipAddPathClosedCurve(nativePath, points, count));
  end;

  function TGPGraphicsPath.AddClosedCurve(points: PGPPointF; count: Integer; tension: Single): TStatus;
  begin
    result := SetStatus(GdipAddPathClosedCurve2(nativePath, points, count, tension));
  end;

  function TGPGraphicsPath.AddClosedCurve(points: PGPPoint; count: Integer): TStatus;
  begin
    result := SetStatus(GdipAddPathClosedCurveI(nativePath, points, count));
  end;


  function TGPGraphicsPath.AddClosedCurve(points: PGPPoint; count: Integer; tension: Single): TStatus;
  begin
       result := SetStatus(GdipAddPathClosedCurve2I(nativePath, points, count, tension));
  end;

  function TGPGraphicsPath.AddRectangle(rect: TGPRectF): TStatus;
  begin
    result := SetStatus(GdipAddPathRectangle(nativePath,
                                            rect.X,
                                            rect.Y,
                                            rect.Width,
                                            rect.Height));
  end;

  function TGPGraphicsPath.AddRectangles(rects: PGPRectF; count: Integer): TStatus;
  begin
    result := SetStatus(GdipAddPathRectangles(nativePath,
                                             rects,
                                             count));
  end;

  function TGPGraphicsPath.AddRectangle(rect: TGPRect): TStatus;
  begin
    result := SetStatus(GdipAddPathRectangleI(nativePath,
                                            rect.X,
                                            rect.Y,
                                            rect.Width,
                                            rect.Height));
  end;

  function TGPGraphicsPath.AddRectangles(rects: PGPRect; count: Integer): TStatus;
  begin
    result := SetStatus(GdipAddPathRectanglesI(nativePath,
                                             rects,
                                             count));
  end;

  function TGPGraphicsPath.AddEllipse(rect: TGPRectF): TStatus;
  begin
    result := AddEllipse(rect.X, rect.Y, rect.Width, rect.Height);
  end;

  function TGPGraphicsPath.AddEllipse(x, y, width, height: Single): TStatus;
  begin
    result := SetStatus(GdipAddPathEllipse(nativePath,
                                          x,
                                          y,
                                          width,
                                          height));
  end;

  function TGPGraphicsPath.AddEllipse(rect: TGPRect): TStatus;
  begin
    result := AddEllipse(rect.X, rect.Y, rect.Width, rect.Height);
  end;

  function TGPGraphicsPath.AddEllipse(x, y, width, height: Integer): TStatus;
  begin
    result := SetStatus(GdipAddPathEllipseI(nativePath,
                                          x,
                                          y,
                                          width,
                                          height));
  end;

  function TGPGraphicsPath.AddPie(rect: TGPRectF; startAngle, sweepAngle: Single): TStatus;
  begin
    result := AddPie(rect.X, rect.Y, rect.Width, rect.Height, startAngle,
                  sweepAngle);
  end;

  function TGPGraphicsPath.AddPie(x, y, width, height, startAngle, sweepAngle: Single): TStatus;
  begin
    result := SetStatus(GdipAddPathPie(nativePath, x, y, width,
                                        height, startAngle,
                                        sweepAngle));
  end;

  function TGPGraphicsPath.AddPie(rect: TGPRect; startAngle, sweepAngle: Single): TStatus;
  begin
    result := AddPie(rect.X,
                  rect.Y,
                  rect.Width,
                  rect.Height,
                  startAngle,
                  sweepAngle);
  end;

  function TGPGraphicsPath.AddPie(x, y, width, height: Integer; startAngle, sweepAngle: Single): TStatus;
  begin
    result := SetStatus(GdipAddPathPieI(nativePath,
                                        x,
                                        y,
                                        width,
                                        height,
                                        startAngle,
                                        sweepAngle));
  end;

  function TGPGraphicsPath.AddPolygon(points: PGPPointF; count: Integer): TStatus;
  begin
    result := SetStatus(GdipAddPathPolygon(nativePath, points, count));
  end;

  function TGPGraphicsPath.AddPolygon(points: PGPPoint; count: Integer): TStatus;
  begin
       result := SetStatus(GdipAddPathPolygonI(nativePath, points,
                                          count));
  end;

  function TGPGraphicsPath.AddPath(addingPath: TGPGraphicsPath; connect: Bool): TStatus;
  var
    nativePath2: GpPath;
  begin
    nativePath2 := nil;
    if assigned(addingPath) then nativePath2 := addingPath.nativePath;
    result := SetStatus(GdipAddPathPath(nativePath, nativePath2, connect));
  end;

  function TGPGraphicsPath.AddString(
      string_: WideString; length: Integer;
      family : TGPFontFamily;
      style  : Integer;
      emSize : Single;  // World units
      origin : TGPPointF;
      format : TGPStringFormat): TStatus;
  var
    rect : TGPRectF;
    gpff : GPFONTFAMILY;
    gpsf : GPSTRINGFORMAT;
  begin
    rect.X := origin.X;
    rect.Y := origin.Y;
    rect.Width := 0.0;
    rect.Height := 0.0;

    gpff := nil;
    gpsf := nil;
    if assigned(family) then gpff := family.nativeFamily;
    if assigned(format) then gpsf := format.nativeFormat;
    result := SetStatus(GdipAddPathString(nativePath, PWideChar(string_), length, gpff,
          style, emSize, @rect, gpsf));
  end;

  function TGPGraphicsPath.AddString(
      string_: WideString;
      length : Integer;
      family : TGPFontFamily;
      style  : Integer;
      emSize : Single;  // World units
      layoutRect: TGPRectF;
      format : TGPStringFormat): TStatus;
  var
    gpff : GPFONTFAMILY;
    gpsf : GPSTRINGFORMAT;
  begin
    gpff := nil;
    gpsf := nil;
    if assigned(family) then gpff := family.nativeFamily;
    if assigned(format) then gpsf := format.nativeFormat;
    result := SetStatus(GdipAddPathString( nativePath, PWideChar(string_), length, gpff,
          style, emSize, @layoutRect, gpsf));
  end;

  function TGPGraphicsPath.AddString(
      string_: WideString;
      length : Integer;
      family : TGPFontFamily;
      style  : Integer;
      emSize : Single;  // World units
      origin : TGPPoint;
      format : TGPStringFormat): TStatus;
  var
    rect : TGPRect;
    gpff : GPFONTFAMILY;
    gpsf : GPSTRINGFORMAT;
  begin
    rect.X := origin.X;
    rect.Y := origin.Y;
    rect.Width := 0;
    rect.Height := 0;
    gpff := nil;
    gpsf := nil;
    if assigned(family) then gpff := family.nativeFamily;
    if assigned(format) then gpsf := format.nativeFormat;
    result := SetStatus(GdipAddPathStringI(nativePath, PWideChar(string_), length, gpff,
          style, emSize, @rect, gpsf));
  end;

  function TGPGraphicsPath.AddString(
      string_: WideString;
      length : Integer;
      family : TGPFontFamily;
      style  : Integer;
      emSize : Single;  // World units
      layoutRect: TGPRect;
      format : TGPStringFormat): TStatus;
  var
    gpff : GPFONTFAMILY;
    gpsf : GPSTRINGFORMAT;
  begin
    gpff := nil;
    gpsf := nil;
    if assigned(family) then gpff := family.nativeFamily;
    if assigned(format) then gpsf := format.nativeFormat;
    result := SetStatus(GdipAddPathStringI( nativePath, PWideChar(string_), length, gpff,
          style, emSize, @layoutRect, gpsf));
  end;

  function TGPGraphicsPath.Transform(matrix: TGPMatrix): TStatus;
  begin
      if assigned(matrix) then
        result := SetStatus(GdipTransformPath(nativePath, matrix.nativeMatrix))
      else
        result := Ok;
  end;

  function TGPGraphicsPath.GetBounds(out bounds: TGPRectF; matrix: TGPMatrix = nil; pen: TGPPen = nil): TStatus;
  var
    nativeMatrix: GpMatrix;
    nativePen: GpPen;
  begin
    nativeMatrix := nil;
    nativePen    := nil;
    if assigned(matrix) then nativeMatrix := matrix.nativeMatrix;
    if assigned(pen) then nativePen := pen.nativePen;

    result := SetStatus(GdipGetPathWorldBounds(nativePath, @bounds, nativeMatrix, nativePen));
  end;

  function TGPGraphicsPath.GetBounds(out bounds: TGPRect; matrix: TGPMatrix = nil; pen: TGPPen = nil): TStatus;
  var
    nativeMatrix: GpMatrix;
    nativePen: GpPen;
  begin
    nativeMatrix := nil;
    nativePen    := nil;
    if assigned(matrix) then nativeMatrix := matrix.nativeMatrix;
    if assigned(pen) then nativePen := pen.nativePen;

    result := SetStatus(GdipGetPathWorldBoundsI(nativePath, @bounds, nativeMatrix, nativePen));
  end;

  function TGPGraphicsPath.Flatten(matrix: TGPMatrix = nil; flatness: Single = FlatnessDefault): TStatus;
  var nativeMatrix: GpMatrix;
  begin
    nativeMatrix := nil;
    if assigned(matrix) then nativeMatrix := matrix.nativeMatrix;
    result := SetStatus(GdipFlattenPath(nativePath, nativeMatrix, flatness));
  end;

  function TGPGraphicsPath.Widen(pen: TGPPen; matrix: TGPMatrix = nil; flatness: Single = FlatnessDefault): TStatus;
  var nativeMatrix: GpMatrix;
  begin
    nativeMatrix := nil;
    if assigned(matrix) then nativeMatrix := matrix.nativeMatrix;
    result := SetStatus(GdipWidenPath(nativePath, pen.nativePen, nativeMatrix, flatness));
  end;

  function TGPGraphicsPath.Outline(matrix: TGPMatrix = nil; flatness: Single = FlatnessDefault): TStatus;
  var nativeMatrix: GpMatrix;
  begin
    nativeMatrix := nil;
    if assigned(matrix) then nativeMatrix := matrix.nativeMatrix;
    result := SetStatus(GdipWindingModeOutline(nativePath, nativeMatrix, flatness));
  end;

    // Once this is called, the resultant path is made of line segments and
    // the original path information is lost.  When matrix is nil, the
    // identity matrix is assumed.

  function TGPGraphicsPath.Warp(destPoints: PGPPointF; count: Integer; srcRect: TGPRectF;
            matrix: TGPMatrix = nil; warpMode: TWarpMode = WarpModePerspective;
            flatness: Single = FlatnessDefault): TStatus;
  var nativeMatrix: GpMatrix;
  begin
    nativeMatrix := nil;
    if assigned(matrix) then nativeMatrix := matrix.nativeMatrix;
    result := SetStatus(GdipWarpPath(nativePath, nativeMatrix, destPoints,
                count, srcRect.X, srcRect.Y, srcRect.Width, srcRect.Height,
                warpMode, flatness));
  end;

  function TGPGraphicsPath.GetPointCount: Integer;
  var count: Integer;
  begin
    count := 0;
    SetStatus(GdipGetPointCount(nativePath, count));
    result := count;
  end;

  function TGPGraphicsPath.GetPathTypes(types: PBYTE; count: Integer): TStatus;
  begin
    result := SetStatus(GdipGetPathTypes(nativePath, types, count));
  end;

  function TGPGraphicsPath.GetPathPoints(points: PGPPointF; count: Integer): TStatus;
  begin
    result := SetStatus(GdipGetPathPoints(nativePath, points, count));
  end;

  function TGPGraphicsPath.GetPathPoints(points: PGPPoint; count: Integer): TStatus;
  begin
    result := SetStatus(GdipGetPathPointsI(nativePath, points, count));
  end;

  function TGPGraphicsPath.GetLastStatus: TStatus;
  begin
    result := lastResult;
    lastResult := Ok;
  end;

  function TGPGraphicsPath.IsVisible(point: TGPPointF; g: TGPGraphics = nil): BOOL;
  begin
    result := IsVisible(point.X, point.Y, g);
  end;

  function TGPGraphicsPath.IsVisible(x, y: Single; g: TGPGraphics = nil): BOOL;
  var
    booln: BOOL;
    nativeGraphics: GpGraphics;
  begin
    booln := FALSE;
    nativeGraphics := nil;
    if assigned(g) then nativeGraphics := g.nativeGraphics;
    SetStatus(GdipIsVisiblePathPoint(nativePath, x, y, nativeGraphics, booln));
    result := booln;
  end;

  function TGPGraphicsPath.IsVisible(point: TGPPoint; g : TGPGraphics = nil): BOOL;
  begin
    result := IsVisible(point.X, point.Y, g);
  end;

  function TGPGraphicsPath.IsVisible(x, y: Integer; g: TGPGraphics = nil): BOOL;
  var
    booln: BOOL;
    nativeGraphics: GpGraphics;
  begin
    booln := FALSE;
    nativeGraphics := nil;
    if assigned(g) then nativeGraphics := g.nativeGraphics;
    SetStatus(GdipIsVisiblePathPointI(nativePath, x, y, nativeGraphics, booln));
    result := booln;
  end;

  function TGPGraphicsPath.IsOutlineVisible(point: TGPPointF; pen: TGPPen; g: TGPGraphics = nil): BOOL;
  begin
    result := IsOutlineVisible(point.X, point.Y, pen, g);
  end;

  function TGPGraphicsPath.IsOutlineVisible(x, y: Single; pen: TGPPen; g: TGPGraphics = nil): BOOL;
  var
    booln: BOOL;
    nativeGraphics: GpGraphics;
    nativePen: GpPen;
  begin
    booln := FALSE;
    nativeGraphics := nil;
    nativePen := nil;
    if assigned(g) then nativeGraphics := g.nativeGraphics;
    if assigned(pen) then nativePen := pen.nativePen;
    SetStatus(GdipIsOutlineVisiblePathPoint(nativePath, x, y, nativePen,
        nativeGraphics, booln));
    result := booln;
  end;

  function TGPGraphicsPath.IsOutlineVisible(point: TGPPoint; pen: TGPPen; g: TGPGraphics = nil): BOOL;
  begin
    result := IsOutlineVisible(point.X, point.Y, pen, g);
  end;

  function TGPGraphicsPath.IsOutlineVisible(x, y: Integer; pen: TGPPen; g: TGPGraphics = nil): BOOL;
  var
    booln: BOOL;
    nativeGraphics: GpGraphics;
    nativePen: GpPen;
  begin
    booln := FALSE;
    nativeGraphics := nil;
    nativePen := nil;
    if assigned(g) then nativeGraphics := g.nativeGraphics;
    if assigned(pen) then nativePen := pen.nativePen;
    SetStatus(GdipIsOutlineVisiblePathPointI(nativePath, x, y, nativePen,
       nativeGraphics, booln));
    result := booln;
  end;

  constructor TGPGraphicsPath.Create(path: TGPGraphicsPath);
  var clonepath: GpPath;
  begin
    clonepath := nil;
    SetStatus(GdipClonePath(path.nativePath, clonepath));
    SetNativePath(clonepath);
  end;

  constructor TGPGraphicsPath.Create(nativePath: GpPath);
  begin
    lastResult := Ok;
    SetNativePath(nativePath);
  end;

  procedure TGPGraphicsPath.SetNativePath(nativePath: GpPath);
  begin
    self.nativePath := nativePath;
  end;

  function TGPGraphicsPath.SetStatus(status: TStatus): TStatus;
  begin
    if (status <> Ok) then LastResult := status;
    result := status;
  end;

//--------------------------------------------------------------------------
// GraphisPathIterator class
//--------------------------------------------------------------------------

  constructor TGPGraphicsPathIterator.Create(path: TGPGraphicsPath);
  var
    nativePath: GpPath;
    iter: GpPathIterator;
  begin
    nativePath := nil;
    if assigned(path) then nativePath := path.nativePath;
    iter := nil;
    lastResult := GdipCreatePathIter(iter, nativePath);
    SetNativeIterator(iter);
  end;

  destructor TGPGraphicsPathIterator.Destroy;
  begin
    GdipDeletePathIter(nativeIterator);
  end;


  function TGPGraphicsPathIterator.NextSubpath(out startIndex, endIndex: Integer; out isClosed: bool): Integer;
  begin
    SetStatus(GdipPathIterNextSubpath(nativeIterator, result, startIndex, endIndex, isClosed));
  end;

  function TGPGraphicsPathIterator.NextSubpath(path: TGPGraphicsPath; out isClosed: BOOL): Integer;
  var
    nativePath: GpPath;
    resultCount: Integer;
  begin
    nativePath := nil;
    if assigned(path) then nativePath := path.nativePath;
    SetStatus(GdipPathIterNextSubpathPath(nativeIterator, resultCount,
      nativePath, isClosed));
    result := resultCount;
  end;

  function TGPGraphicsPathIterator.NextPathType(out pathType: TPathPointType; out startIndex, endIndex: Integer): Integer;
  var
    resultCount: Integer;
  begin
    SetStatus(GdipPathIterNextPathType(nativeIterator, resultCount, @pathType,
       startIndex, endIndex));
    result := resultCount;
  end;

  function TGPGraphicsPathIterator.NextMarker(out startIndex, endIndex: Integer): Integer;
  begin
    SetStatus(GdipPathIterNextMarker(nativeIterator, result, startIndex, endIndex));
  end;

  function TGPGraphicsPathIterator.NextMarker(path: TGPGraphicsPath): Integer;
  var nativePath: GpPath;
  begin
    nativePath := nil;
    if assigned(path) then nativePath := path.nativePath;
    SetStatus(GdipPathIterNextMarkerPath(nativeIterator, result, nativePath));
  end;

  function TGPGraphicsPathIterator.GetCount: Integer;
  begin
    SetStatus(GdipPathIterGetCount(nativeIterator, result));
  end;

  function TGPGraphicsPathIterator.GetSubpathCount: Integer;
  begin
    SetStatus(GdipPathIterGetSubpathCount(nativeIterator, result));
  end;

  function TGPGraphicsPathIterator.HasCurve: BOOL;
  begin
    SetStatus(GdipPathIterHasCurve(nativeIterator, result));
  end;

  procedure TGPGraphicsPathIterator.Rewind;
  begin
    SetStatus(GdipPathIterRewind(nativeIterator));
  end;

  function TGPGraphicsPathIterator.Enumerate(points: PGPPointF; types: PBYTE;
    count: Integer): Integer;
  begin
    SetStatus(GdipPathIterEnumerate(nativeIterator, result, points, types, count));
  end;

  function TGPGraphicsPathIterator.CopyData(points: PGPPointF; types: PBYTE;
    startIndex, endIndex: Integer): Integer;
  begin
    SetStatus(GdipPathIterCopyData(nativeIterator, result, points, types,
      startIndex, endIndex));
  end;

  function TGPGraphicsPathIterator.GetLastStatus: TStatus;
  begin
    result := lastResult;
    lastResult := Ok;
  end;

  procedure TGPGraphicsPathIterator.SetNativeIterator(nativeIterator: GpPathIterator);
  begin
    self.nativeIterator := nativeIterator;
  end;

  function TGPGraphicsPathIterator.SetStatus(status: TStatus): TStatus;
  begin
    if (status <> Ok) then lastResult := status;
    result := status;
  end;

//--------------------------------------------------------------------------
// Path Gradient Brush
//--------------------------------------------------------------------------

  constructor TGPPathGradientBrush.Create(points: PGPPointF; count: Integer; wrapMode: TWrapMode = WrapModeClamp);
  var brush: GpPathGradient;
  begin
    brush := nil;
    lastResult := GdipCreatePathGradient(points, count, wrapMode, brush);
    SetNativeBrush(brush);
  end;

  constructor TGPPathGradientBrush.Create(points: PGPPoint; count: Integer; wrapMode: TWrapMode = WrapModeClamp);
  var brush: GpPathGradient;
  begin
    brush := nil;
    lastResult := GdipCreatePathGradientI(points, count, wrapMode, brush);
    SetNativeBrush(brush);
  end;

  constructor TGPPathGradientBrush.Create(path: TGPGraphicsPath);
  var brush: GpPathGradient;
  begin
    brush := nil;
    lastResult := GdipCreatePathGradientFromPath(path.nativePath, brush);
    SetNativeBrush(brush);
  end;

  function TGPPathGradientBrush.GetCenterColor(out Color: TGPColor): TStatus;
  begin
    SetStatus(GdipGetPathGradientCenterColor(GpPathGradient(nativeBrush), Color));
    result := lastResult;
  end;

  function TGPPathGradientBrush.SetCenterColor(color: TGPColor): TStatus;
  begin
    SetStatus(GdipSetPathGradientCenterColor(GpPathGradient(nativeBrush),color));
    result := lastResult;
  end;

  function TGPPathGradientBrush.GetPointCount: Integer;
  begin
    SetStatus(GdipGetPathGradientPointCount(GpPathGradient(nativeBrush), result));
  end;

  function TGPPathGradientBrush.GetSurroundColorCount: Integer;
  begin
    SetStatus(GdipGetPathGradientSurroundColorCount(GpPathGradient(nativeBrush), result));
  end;

  function TGPPathGradientBrush.GetSurroundColors(colors: PARGB; var count: Integer): TStatus;
  var
    count1: Integer;
  begin
    if not assigned(colors) then
    begin
      result := SetStatus(InvalidParameter);
      exit;
    end;

    SetStatus(GdipGetPathGradientSurroundColorCount(GpPathGradient(nativeBrush), count1));

    if(lastResult <> Ok) then
    begin
      result := lastResult;
      exit;
    end;

    if((count < count1) or (count1 <= 0)) then
    begin
      result := SetStatus(InsufficientBuffer);
      exit;
    end;

    SetStatus(GdipGetPathGradientSurroundColorsWithCount(GpPathGradient(nativeBrush), colors, count1));
    if(lastResult = Ok) then
      count := count1;

    result := lastResult;
  end;

  function TGPPathGradientBrush.SetSurroundColors(colors: PARGB; var count: Integer): TStatus;
  var
    count1: Integer;
  type
    TDynArrDWORD = array of DWORD;
  begin
    if (colors = nil) then
    begin
      result := SetStatus(InvalidParameter);
      exit;
    end;

    count1 := GetPointCount;

    if((count > count1) or (count1 <= 0)) then
    begin
      result := SetStatus(InvalidParameter);
      exit;
    end;

    count1 := count;

    SetStatus(GdipSetPathGradientSurroundColorsWithCount(
                GpPathGradient(nativeBrush), colors, count1));

    if(lastResult = Ok) then count := count1;
    result := lastResult;
  end;

  function TGPPathGradientBrush.GetGraphicsPath(path: TGPGraphicsPath): TStatus;
  begin
    if(path = nil) then
    begin
      result := SetStatus(InvalidParameter);
      exit;
    end;
    result := SetStatus(GdipGetPathGradientPath(GpPathGradient(nativeBrush), path.nativePath));
  end;

  function TGPPathGradientBrush.SetGraphicsPath(path: TGPGraphicsPath): TStatus;
  begin
    if(path = nil) then
    begin
      result := SetStatus(InvalidParameter);
      exit;
    end;
    result := SetStatus(GdipSetPathGradientPath(GpPathGradient(nativeBrush), path.nativePath));
  end;

  function TGPPathGradientBrush.GetCenterPoint(out point: TGPPointF): TStatus;
  begin
    result := SetStatus(GdipGetPathGradientCenterPoint(GpPathGradient(nativeBrush), @point));
  end;

  function TGPPathGradientBrush.GetCenterPoint(out point: TGPPoint): TStatus;
  begin
    result := SetStatus(GdipGetPathGradientCenterPointI(GpPathGradient(nativeBrush), @point));
  end;

  function TGPPathGradientBrush.SetCenterPoint(point: TGPPointF): TStatus;
  begin
    result := SetStatus(GdipSetPathGradientCenterPoint(GpPathGradient(nativeBrush), @point));
  end;

  function TGPPathGradientBrush.SetCenterPoint(point: TGPPoint): TStatus;
  begin
    result := SetStatus(GdipSetPathGradientCenterPointI(GpPathGradient(nativeBrush), @point));
  end;

  function TGPPathGradientBrush.GetRectangle(out rect: TGPRectF): TStatus;
  begin
    result := SetStatus(GdipGetPathGradientRect(GpPathGradient(nativeBrush), @rect));
  end;

  function TGPPathGradientBrush.GetRectangle(out rect: TGPRect): TStatus;
  begin
    result := SetStatus(GdipGetPathGradientRectI(GpPathGradient(nativeBrush), @rect));
  end;

  function TGPPathGradientBrush.SetGammaCorrection(useGammaCorrection: BOOL): TStatus;
  begin
    result := SetStatus(GdipSetPathGradientGammaCorrection(GpPathGradient(nativeBrush),
      useGammaCorrection));
  end;

  function TGPPathGradientBrush.GetGammaCorrection: BOOL;
  begin
    SetStatus(GdipGetPathGradientGammaCorrection(GpPathGradient(nativeBrush), result));
  end;

  function TGPPathGradientBrush.GetBlendCount: Integer;
  var count: Integer;
  begin
    count := 0;
    SetStatus(GdipGetPathGradientBlendCount(GpPathGradient(nativeBrush), count));
    result := count;
  end;

  function TGPPathGradientBrush.GetBlend(blendFactors, blendPositions:PSingle; count: Integer): TStatus;
  begin
    result := SetStatus(GdipGetPathGradientBlend(
                      GpPathGradient(nativeBrush),
                      blendFactors, blendPositions, count));
  end;

  function TGPPathGradientBrush.SetBlend(blendFactors, blendPositions: PSingle; count: Integer): TStatus;
  begin
    result := SetStatus(GdipSetPathGradientBlend(
                      GpPathGradient(nativeBrush),
                      blendFactors, blendPositions, count));
  end;

  function TGPPathGradientBrush.GetInterpolationColorCount: Integer;
  var count: Integer;
  begin
    count := 0;
    SetStatus(GdipGetPathGradientPresetBlendCount(GpPathGradient(nativeBrush), count));
    result := count;
  end;

  function TGPPathGradientBrush.SetInterpolationColors(presetColors: PARGB;
    blendPositions: PSingle; count: Integer): TStatus;
  var
    status: TStatus;
  begin
    if ((count <= 0) or (presetColors = nil)) then
    begin
      result := SetStatus(InvalidParameter);
      exit;
    end;

    status := SetStatus(GdipSetPathGradientPresetBlend(GpPathGradient(nativeBrush),
                          presetColors, blendPositions, count));
    result := status;
  end;

  function TGPPathGradientBrush.GetInterpolationColors(presetColors: PARGB;
    blendPositions: PSingle; count: Integer): TStatus;
  var
    status: GpStatus;
    i: Integer;
    argbs: PARGB;
  begin
        if ((count <= 0) or (presetColors = nil)) then
        begin
          result := SetStatus(InvalidParameter);
          exit;
        end;
        getmem(argbs, count*SizeOf(ARGB));
        if (argbs = nil) then
        begin
          result := SetStatus(OutOfMemory);
          exit;
        end;

        status := SetStatus(GdipGetPathGradientPresetBlend(nativeBrush, argbs,
                     blendPositions, count));

        for i := 0 to count - 1 do
          TColorDynArray(presetColors)[i] := TColorDynArray(argbs)[i];

        freemem(argbs);

        result := status;
  end;

  function TGPPathGradientBrush.SetBlendBellShape(focus: Single; scale: Single = 1.0): TStatus;
  begin
    result := SetStatus(GdipSetPathGradientSigmaBlend(GpPathGradient(nativeBrush), focus, scale));
  end;

  function TGPPathGradientBrush.SetBlendTriangularShape(focus: Single; scale: Single = 1.0): TStatus;
  begin
    result := SetStatus(GdipSetPathGradientLinearBlend(GpPathGradient(nativeBrush), focus, scale));
  end;

  function TGPPathGradientBrush.GetTransform(matrix: TGPMatrix): TStatus;
  begin
    result := SetStatus(GdipGetPathGradientTransform(GpPathGradient(nativeBrush),
                      matrix.nativeMatrix));
  end;

  function TGPPathGradientBrush.SetTransform(matrix: TGPMatrix): TStatus;
  begin
    result := SetStatus(GdipSetPathGradientTransform(
                      GpPathGradient(nativeBrush),
                      matrix.nativeMatrix));
  end;

  function TGPPathGradientBrush.ResetTransform: TStatus;
  begin
    result := SetStatus(GdipResetPathGradientTransform(
                      GpPathGradient(nativeBrush)));
  end;

  function TGPPathGradientBrush.MultiplyTransform(matrix: TGPMatrix; order: TMatrixOrder = MatrixOrderPrepend): TStatus;
  begin
    result := SetStatus(GdipMultiplyPathGradientTransform(
                      GpPathGradient(nativeBrush),
                      matrix.nativeMatrix,
                      order));
  end;

  function TGPPathGradientBrush.TranslateTransform(dx, dy: Single; order: TMatrixOrder = MatrixOrderPrepend): TStatus;
  begin
    result := SetStatus(GdipTranslatePathGradientTransform(
                      GpPathGradient(nativeBrush),
                      dx, dy, order));
  end;

  function TGPPathGradientBrush.ScaleTransform(sx, sy: Single; order: TMatrixOrder = MatrixOrderPrepend): TStatus;
  begin
    result := SetStatus(GdipScalePathGradientTransform(
                      GpPathGradient(nativeBrush),
                      sx, sy, order));
  end;

  function TGPPathGradientBrush.RotateTransform(angle: Single; order: TMatrixOrder = MatrixOrderPrepend): TStatus;
  begin
    result := SetStatus(GdipRotatePathGradientTransform(
                      GpPathGradient(nativeBrush),
                      angle, order));
  end;

  function TGPPathGradientBrush.GetFocusScales(out xScale, yScale: Single): TStatus;
  begin
    result := SetStatus(GdipGetPathGradientFocusScales(
                      GpPathGradient(nativeBrush), xScale, yScale));
  end;

  function TGPPathGradientBrush.SetFocusScales(xScale, yScale: Single): TStatus;
  begin
    result := SetStatus(GdipSetPathGradientFocusScales(
                      GpPathGradient(nativeBrush), xScale, yScale));
  end;

  function TGPPathGradientBrush.GetWrapMode: TWrapMode;
  begin
    SetStatus(GdipGetPathGradientWrapMode(GpPathGradient(nativeBrush), result));
  end;

  function TGPPathGradientBrush.SetWrapMode(wrapMode: TWrapMode): TStatus;
  begin
    result := SetStatus(GdipSetPathGradientWrapMode(
                      GpPathGradient(nativeBrush), wrapMode));
  end;

  constructor TGPPathGradientBrush.Create;
  begin
  end;









function ValueTypeFromULONG(Type_: ULONG): String;
begin
  case Type_ of
    0 : result := 'Nothing';
    1 : result := 'PropertyTagTypeByte';
    2 : result := 'PropertyTagTypeASCII';
    3 : result := 'PropertyTagTypeShort';
    4 : result := 'PropertyTagTypeLong';
    5 : result := 'PropertyTagTypeRational';
    6 : result := 'Nothing';
    7 : result := 'PropertyTagTypeUndefined';
    8 : result := 'Nothing';
    9 : result := 'PropertyTagTypeSLONG';
    10: result := 'PropertyTagTypeSRational';
  else
    result := '<UnKnown>';
  end;
end;

function GetMetaDataIDString(id: ULONG): string;
begin
  case id of
    PropertyTagExifIFD                        : result := 'PropertyTagExifIFD';
    PropertyTagGpsIFD                         : result := 'PropertyTagGpsIFD';
    PropertyTagNewSubfileType                 : result := 'PropertyTagNewSubfileType';
    PropertyTagSubfileType                    : result := 'PropertyTagSubfileType';
    PropertyTagImageWidth                     : result := 'PropertyTagImageWidth';
    PropertyTagImageHeight                    : result := 'PropertyTagImageHeight';
    PropertyTagBitsPerSample                  : result := 'PropertyTagBitsPerSample';
    PropertyTagCompression                    : result := 'PropertyTagCompression';
    PropertyTagPhotometricInterp              : result := 'PropertyTagPhotometricInterp';
    PropertyTagThreshHolding                  : result := 'PropertyTagThreshHolding';
    PropertyTagCellWidth                      : result := 'PropertyTagCellWidth';
    PropertyTagCellHeight                     : result := 'PropertyTagCellHeight';
    PropertyTagFillOrder                      : result := 'PropertyTagFillOrder';
    PropertyTagDocumentName                   : result := 'PropertyTagDocumentName';
    PropertyTagImageDescription               : result := 'PropertyTagImageDescription';
    PropertyTagEquipMake                      : result := 'PropertyTagEquipMake';
    PropertyTagEquipModel                     : result := 'PropertyTagEquipModel';
    PropertyTagStripOffsets                   : result := 'PropertyTagStripOffsets';
    PropertyTagOrientation                    : result := 'PropertyTagOrientation';
    PropertyTagSamplesPerPixel                : result := 'PropertyTagSamplesPerPixel';
    PropertyTagRowsPerStrip                   : result := 'PropertyTagRowsPerStrip';
    PropertyTagStripBytesCount                : result := 'PropertyTagStripBytesCount';
    PropertyTagMinSampleValue                 : result := 'PropertyTagMinSampleValue';
    PropertyTagMaxSampleValue                 : result := 'PropertyTagMaxSampleValue';
    PropertyTagXResolution                    : result := 'PropertyTagXResolution';
    PropertyTagYResolution                    : result := 'PropertyTagYResolution';
    PropertyTagPlanarConfig                   : result := 'PropertyTagPlanarConfig';
    PropertyTagPageName                       : result := 'PropertyTagPageName';
    PropertyTagXPosition                      : result := 'PropertyTagXPosition';
    PropertyTagYPosition                      : result := 'PropertyTagYPosition';
    PropertyTagFreeOffset                     : result := 'PropertyTagFreeOffset';
    PropertyTagFreeByteCounts                 : result := 'PropertyTagFreeByteCounts';
    PropertyTagGrayResponseUnit               : result := 'PropertyTagGrayResponseUnit';
    PropertyTagGrayResponseCurve              : result := 'PropertyTagGrayResponseCurve';
    PropertyTagT4Option                       : result := 'PropertyTagT4Option';
    PropertyTagT6Option                       : result := 'PropertyTagT6Option';
    PropertyTagResolutionUnit                 : result := 'PropertyTagResolutionUnit';
    PropertyTagPageNumber                     : result := 'PropertyTagPageNumber';
    PropertyTagTransferFuncition              : result := 'PropertyTagTransferFuncition';
    PropertyTagSoftwareUsed                   : result := 'PropertyTagSoftwareUsed';
    PropertyTagDateTime                       : result := 'PropertyTagDateTime';
    PropertyTagArtist                         : result := 'PropertyTagArtist';
    PropertyTagHostComputer                   : result := 'PropertyTagHostComputer';
    PropertyTagPredictor                      : result := 'PropertyTagPredictor';
    PropertyTagWhitePoint                     : result := 'PropertyTagWhitePoint';
    PropertyTagPrimaryChromaticities          : result := 'PropertyTagPrimaryChromaticities';
    PropertyTagColorMap                       : result := 'PropertyTagColorMap';
    PropertyTagHalftoneHints                  : result := 'PropertyTagHalftoneHints';
    PropertyTagTileWidth                      : result := 'PropertyTagTileWidth';
    PropertyTagTileLength                     : result := 'PropertyTagTileLength';
    PropertyTagTileOffset                     : result := 'PropertyTagTileOffset';
    PropertyTagTileByteCounts                 : result := 'PropertyTagTileByteCounts';
    PropertyTagInkSet                         : result := 'PropertyTagInkSet';
    PropertyTagInkNames                       : result := 'PropertyTagInkNames';
    PropertyTagNumberOfInks                   : result := 'PropertyTagNumberOfInks';
    PropertyTagDotRange                       : result := 'PropertyTagDotRange';
    PropertyTagTargetPrinter                  : result := 'PropertyTagTargetPrinter';
    PropertyTagExtraSamples                   : result := 'PropertyTagExtraSamples';
    PropertyTagSampleFormat                   : result := 'PropertyTagSampleFormat';
    PropertyTagSMinSampleValue                : result := 'PropertyTagSMinSampleValue';
    PropertyTagSMaxSampleValue                : result := 'PropertyTagSMaxSampleValue';
    PropertyTagTransferRange                  : result := 'PropertyTagTransferRange';
    PropertyTagJPEGProc                       : result := 'PropertyTagJPEGProc';
    PropertyTagJPEGInterFormat                : result := 'PropertyTagJPEGInterFormat';
    PropertyTagJPEGInterLength                : result := 'PropertyTagJPEGInterLength';
    PropertyTagJPEGRestartInterval            : result := 'PropertyTagJPEGRestartInterval';
    PropertyTagJPEGLosslessPredictors         : result := 'PropertyTagJPEGLosslessPredictors';
    PropertyTagJPEGPointTransforms            : result := 'PropertyTagJPEGPointTransforms';
    PropertyTagJPEGQTables                    : result := 'PropertyTagJPEGQTables';
    PropertyTagJPEGDCTables                   : result := 'PropertyTagJPEGDCTables';
    PropertyTagJPEGACTables                   : result := 'PropertyTagJPEGACTables';
    PropertyTagYCbCrCoefficients              : result := 'PropertyTagYCbCrCoefficients';
    PropertyTagYCbCrSubsampling               : result := 'PropertyTagYCbCrSubsampling';
    PropertyTagYCbCrPositioning               : result := 'PropertyTagYCbCrPositioning';
    PropertyTagREFBlackWhite                  : result := 'PropertyTagREFBlackWhite';
    PropertyTagICCProfile                     : result := 'PropertyTagICCProfile';
    PropertyTagGamma                          : result := 'PropertyTagGamma';
    PropertyTagICCProfileDescriptor           : result := 'PropertyTagICCProfileDescriptor';
    PropertyTagSRGBRenderingIntent            : result := 'PropertyTagSRGBRenderingIntent';
    PropertyTagImageTitle                     : result := 'PropertyTagImageTitle';
    PropertyTagCopyright                      : result := 'PropertyTagCopyright';
    PropertyTagResolutionXUnit                : result := 'PropertyTagResolutionXUnit';
    PropertyTagResolutionYUnit                : result := 'PropertyTagResolutionYUnit';
    PropertyTagResolutionXLengthUnit          : result := 'PropertyTagResolutionXLengthUnit';
    PropertyTagResolutionYLengthUnit          : result := 'PropertyTagResolutionYLengthUnit';
    PropertyTagPrintFlags                     : result := 'PropertyTagPrintFlags';
    PropertyTagPrintFlagsVersion              : result := 'PropertyTagPrintFlagsVersion';
    PropertyTagPrintFlagsCrop                 : result := 'PropertyTagPrintFlagsCrop';
    PropertyTagPrintFlagsBleedWidth           : result := 'PropertyTagPrintFlagsBleedWidth';
    PropertyTagPrintFlagsBleedWidthScale      : result := 'PropertyTagPrintFlagsBleedWidthScale';
    PropertyTagHalftoneLPI                    : result := 'PropertyTagHalftoneLPI';
    PropertyTagHalftoneLPIUnit                : result := 'PropertyTagHalftoneLPIUnit';
    PropertyTagHalftoneDegree                 : result := 'PropertyTagHalftoneDegree';
    PropertyTagHalftoneShape                  : result := 'PropertyTagHalftoneShape';
    PropertyTagHalftoneMisc                   : result := 'PropertyTagHalftoneMisc';
    PropertyTagHalftoneScreen                 : result := 'PropertyTagHalftoneScreen';
    PropertyTagJPEGQuality                    : result := 'PropertyTagJPEGQuality';
    PropertyTagGridSize                       : result := 'PropertyTagGridSize';
    PropertyTagThumbnailFormat                : result := 'PropertyTagThumbnailFormat';
    PropertyTagThumbnailWidth                 : result := 'PropertyTagThumbnailWidth';
    PropertyTagThumbnailHeight                : result := 'PropertyTagThumbnailHeight';
    PropertyTagThumbnailColorDepth            : result := 'PropertyTagThumbnailColorDepth';
    PropertyTagThumbnailPlanes                : result := 'PropertyTagThumbnailPlanes';
    PropertyTagThumbnailRawBytes              : result := 'PropertyTagThumbnailRawBytes';
    PropertyTagThumbnailSize                  : result := 'PropertyTagThumbnailSize';
    PropertyTagThumbnailCompressedSize        : result := 'PropertyTagThumbnailCompressedSize';
    PropertyTagColorTransferFunction          : result := 'PropertyTagColorTransferFunction';
    PropertyTagThumbnailData                  : result := 'PropertyTagThumbnailData';
    PropertyTagThumbnailImageWidth            : result := 'PropertyTagThumbnailImageWidth';
    PropertyTagThumbnailImageHeight           : result := 'PropertyTagThumbnailImageHeight';
    PropertyTagThumbnailBitsPerSample         : result := 'PropertyTagThumbnailBitsPerSample';
    PropertyTagThumbnailCompression           : result := 'PropertyTagThumbnailCompression';
    PropertyTagThumbnailPhotometricInterp     : result := 'PropertyTagThumbnailPhotometricInterp';
    PropertyTagThumbnailImageDescription      : result := 'PropertyTagThumbnailImageDescription';
    PropertyTagThumbnailEquipMake             : result := 'PropertyTagThumbnailEquipMake';
    PropertyTagThumbnailEquipModel            : result := 'PropertyTagThumbnailEquipModel';
    PropertyTagThumbnailStripOffsets          : result := 'PropertyTagThumbnailStripOffsets';
    PropertyTagThumbnailOrientation           : result := 'PropertyTagThumbnailOrientation';
    PropertyTagThumbnailSamplesPerPixel       : result := 'PropertyTagThumbnailSamplesPerPixel';
    PropertyTagThumbnailRowsPerStrip          : result := 'PropertyTagThumbnailRowsPerStrip';
    PropertyTagThumbnailStripBytesCount       : result := 'PropertyTagThumbnailStripBytesCount';
    PropertyTagThumbnailResolutionX           : result := 'PropertyTagThumbnailResolutionX';
    PropertyTagThumbnailResolutionY           : result := 'PropertyTagThumbnailResolutionY';
    PropertyTagThumbnailPlanarConfig          : result := 'PropertyTagThumbnailPlanarConfig';
    PropertyTagThumbnailResolutionUnit        : result := 'PropertyTagThumbnailResolutionUnit';
    PropertyTagThumbnailTransferFunction      : result := 'PropertyTagThumbnailTransferFunction';
    PropertyTagThumbnailSoftwareUsed          : result := 'PropertyTagThumbnailSoftwareUsed';
    PropertyTagThumbnailDateTime              : result := 'PropertyTagThumbnailDateTime';
    PropertyTagThumbnailArtist                : result := 'PropertyTagThumbnailArtist';
    PropertyTagThumbnailWhitePoint            : result := 'PropertyTagThumbnailWhitePoint';
    PropertyTagThumbnailPrimaryChromaticities : result := 'PropertyTagThumbnailPrimaryChromaticities';
    PropertyTagThumbnailYCbCrCoefficients     : result := 'PropertyTagThumbnailYCbCrCoefficients';
    PropertyTagThumbnailYCbCrSubsampling      : result := 'PropertyTagThumbnailYCbCrSubsampling';
    PropertyTagThumbnailYCbCrPositioning      : result := 'PropertyTagThumbnailYCbCrPositioning';
    PropertyTagThumbnailRefBlackWhite         : result := 'PropertyTagThumbnailRefBlackWhite';
    PropertyTagThumbnailCopyRight             : result := 'PropertyTagThumbnailCopyRight';
    PropertyTagLuminanceTable                 : result := 'PropertyTagLuminanceTable';
    PropertyTagChrominanceTable               : result := 'PropertyTagChrominanceTable';
    PropertyTagFrameDelay                     : result := 'PropertyTagFrameDelay';
    PropertyTagLoopCount                      : result := 'PropertyTagLoopCount';
    PropertyTagPixelUnit                      : result := 'PropertyTagPixelUnit';
    PropertyTagPixelPerUnitX                  : result := 'PropertyTagPixelPerUnitX';
    PropertyTagPixelPerUnitY                  : result := 'PropertyTagPixelPerUnitY';
    PropertyTagPaletteHistogram               : result := 'PropertyTagPaletteHistogram';
    PropertyTagExifExposureTime               : result := 'PropertyTagExifExposureTime';
    PropertyTagExifFNumber                    : result := 'PropertyTagExifFNumber';
    PropertyTagExifExposureProg               : result := 'PropertyTagExifExposureProg';
    PropertyTagExifSpectralSense              : result := 'PropertyTagExifSpectralSense';
    PropertyTagExifISOSpeed                   : result := 'PropertyTagExifISOSpeed';
    PropertyTagExifOECF                       : result := 'PropertyTagExifOECF';
    PropertyTagExifVer                        : result := 'PropertyTagExifVer';
    PropertyTagExifDTOrig                     : result := 'PropertyTagExifDTOrig';
    PropertyTagExifDTDigitized                : result := 'PropertyTagExifDTDigitized';
    PropertyTagExifCompConfig                 : result := 'PropertyTagExifCompConfig';
    PropertyTagExifCompBPP                    : result := 'PropertyTagExifCompBPP';
    PropertyTagExifShutterSpeed               : result := 'PropertyTagExifShutterSpeed';
    PropertyTagExifAperture                   : result := 'PropertyTagExifAperture';
    PropertyTagExifBrightness                 : result := 'PropertyTagExifBrightness';
    PropertyTagExifExposureBias               : result := 'PropertyTagExifExposureBias';
    PropertyTagExifMaxAperture                : result := 'PropertyTagExifMaxAperture';
    PropertyTagExifSubjectDist                : result := 'PropertyTagExifSubjectDist';
    PropertyTagExifMeteringMode               : result := 'PropertyTagExifMeteringMode';
    PropertyTagExifLightSource                : result := 'PropertyTagExifLightSource';
    PropertyTagExifFlash                      : result := 'PropertyTagExifFlash';
    PropertyTagExifFocalLength                : result := 'PropertyTagExifFocalLength';
    PropertyTagExifMakerNote                  : result := 'PropertyTagExifMakerNote';
    PropertyTagExifUserComment                : result := 'PropertyTagExifUserComment';
    PropertyTagExifDTSubsec                   : result := 'PropertyTagExifDTSubsec';
    PropertyTagExifDTOrigSS                   : result := 'PropertyTagExifDTOrigSS';
    PropertyTagExifDTDigSS                    : result := 'PropertyTagExifDTDigSS';
    PropertyTagExifFPXVer                     : result := 'PropertyTagExifFPXVer';
    PropertyTagExifColorSpace                 : result := 'PropertyTagExifColorSpace';
    PropertyTagExifPixXDim                    : result := 'PropertyTagExifPixXDim';
    PropertyTagExifPixYDim                    : result := 'PropertyTagExifPixYDim';
    PropertyTagExifRelatedWav                 : result := 'PropertyTagExifRelatedWav';
    PropertyTagExifInterop                    : result := 'PropertyTagExifInterop';
    PropertyTagExifFlashEnergy                : result := 'PropertyTagExifFlashEnergy';
    PropertyTagExifSpatialFR                  : result := 'PropertyTagExifSpatialFR';
    PropertyTagExifFocalXRes                  : result := 'PropertyTagExifFocalXRes';
    PropertyTagExifFocalYRes                  : result := 'PropertyTagExifFocalYRes';
    PropertyTagExifFocalResUnit               : result := 'PropertyTagExifFocalResUnit';
    PropertyTagExifSubjectLoc                 : result := 'PropertyTagExifSubjectLoc';
    PropertyTagExifExposureIndex              : result := 'PropertyTagExifExposureIndex';
    PropertyTagExifSensingMethod              : result := 'PropertyTagExifSensingMethod';
    PropertyTagExifFileSource                 : result := 'PropertyTagExifFileSource';
    PropertyTagExifSceneType                  : result := 'PropertyTagExifSceneType';
    PropertyTagExifCfaPattern                 : result := 'PropertyTagExifCfaPattern';
    PropertyTagGpsVer                         : result := 'PropertyTagGpsVer';
    PropertyTagGpsLatitudeRef                 : result := 'PropertyTagGpsLatitudeRef';
    PropertyTagGpsLatitude                    : result := 'PropertyTagGpsLatitude';
    PropertyTagGpsLongitudeRef                : result := 'PropertyTagGpsLongitudeRef';
    PropertyTagGpsLongitude                   : result := 'PropertyTagGpsLongitude';
    PropertyTagGpsAltitudeRef                 : result := 'PropertyTagGpsAltitudeRef';
    PropertyTagGpsAltitude                    : result := 'PropertyTagGpsAltitude';
    PropertyTagGpsGpsTime                     : result := 'PropertyTagGpsGpsTime';
    PropertyTagGpsGpsSatellites               : result := 'PropertyTagGpsGpsSatellites';
    PropertyTagGpsGpsStatus                   : result := 'PropertyTagGpsGpsStatus';
    PropertyTagGpsGpsMeasureMode              : result := 'PropertyTagGpsGpsMeasureMode';
    PropertyTagGpsGpsDop                      : result := 'PropertyTagGpsGpsDop';
    PropertyTagGpsSpeedRef                    : result := 'PropertyTagGpsSpeedRef';
    PropertyTagGpsSpeed                       : result := 'PropertyTagGpsSpeed';
    PropertyTagGpsTrackRef                    : result := 'PropertyTagGpsTrackRef';
    PropertyTagGpsTrack                       : result := 'PropertyTagGpsTrack';
    PropertyTagGpsImgDirRef                   : result := 'PropertyTagGpsImgDirRef';
    PropertyTagGpsImgDir                      : result := 'PropertyTagGpsImgDir';
    PropertyTagGpsMapDatum                    : result := 'PropertyTagGpsMapDatum';
    PropertyTagGpsDestLatRef                  : result := 'PropertyTagGpsDestLatRef';
    PropertyTagGpsDestLat                     : result := 'PropertyTagGpsDestLat';
    PropertyTagGpsDestLongRef                 : result := 'PropertyTagGpsDestLongRef';
    PropertyTagGpsDestLong                    : result := 'PropertyTagGpsDestLong';
    PropertyTagGpsDestBearRef                 : result := 'PropertyTagGpsDestBearRef';
    PropertyTagGpsDestBear                    : result := 'PropertyTagGpsDestBear';
    PropertyTagGpsDestDistRef                 : result := 'PropertyTagGpsDestDistRef';
    PropertyTagGpsDestDist                    : result := 'PropertyTagGpsDestDist';
  else
    result := '<UnKnown>';
  end;
end;

function GetEncoderClsid(format: String; out pClsid: TGUID): integer;
var
  num, size, j: UINT;
  ImageCodecInfo: PImageCodecInfo;
Type
  ArrIMgInf = array of TImageCodecInfo;
begin
  num  := 0; // number of image encoders
  size := 0; // size of the image encoder array in bytes
  result := -1;

  GetImageEncodersSize(num, size);
  if (size = 0) then exit;

  GetMem(ImageCodecInfo, size);
  if(ImageCodecInfo = nil) then exit;

  GetImageEncoders(num, size, ImageCodecInfo);

  for j := 0 to num - 1 do
  begin
    if( ArrIMgInf(ImageCodecInfo)[j].MimeType = format) then
    begin
      pClsid := ArrIMgInf(ImageCodecInfo)[j].Clsid;
      result := j;  // Success
    end;
  end;
  FreeMem(ImageCodecInfo, size);
end;

function GetStatus(Stat: TStatus): string;
begin
  case Stat of
    Ok                        : result := 'Ok';
    GenericError              : result := 'GenericError';
    InvalidParameter          : result := 'InvalidParameter';
    OutOfMemory               : result := 'OutOfMemory';
    ObjectBusy                : result := 'ObjectBusy';
    InsufficientBuffer        : result := 'InsufficientBuffer';
    NotImplemented            : result := 'NotImplemented';
    Win32Error                : result := 'Win32Error';
    WrongState                : result := 'WrongState';
    Aborted                   : result := 'Aborted';
    FileNotFound              : result := 'FileNotFound';
    ValueOverflow             : result := 'ValueOverflow';
    AccessDenied              : result := 'AccessDenied';
    UnknownImageFormat        : result := 'UnknownImageFormat';
    FontFamilyNotFound        : result := 'FontFamilyNotFound';
    FontStyleNotFound         : result := 'FontStyleNotFound';
    NotTrueTypeFont           : result := 'NotTrueTypeFont';
    UnsupportedGdiplusVersion : result := 'UnsupportedGdiplusVersion';
    GdiplusNotInitialized     : result := 'GdiplusNotInitialized';
    PropertyNotFound          : result := 'PropertyNotFound';
    PropertyNotSupported      : result := 'PropertyNotSupported';
  else
    result := '<UnKnown>';
  end;
end;

function PixelFormatString(PixelFormat: TPixelFormat): string;
begin
  case PixelFormat of
    PixelFormatIndexed        : result := 'PixelFormatIndexed';
    PixelFormatGDI            : result := 'PixelFormatGDI';
    PixelFormatAlpha          : result := 'PixelFormatAlpha';
    PixelFormatPAlpha         : result := 'PixelFormatPAlpha';
    PixelFormatExtended       : result := 'PixelFormatExtended';
    PixelFormatCanonical      : result := 'PixelFormatCanonical';
    PixelFormatUndefined      : result := 'PixelFormatUndefined';
    PixelFormat1bppIndexed    : result := 'PixelFormat1bppIndexed';
    PixelFormat4bppIndexed    : result := 'PixelFormat4bppIndexed';
    PixelFormat8bppIndexed    : result := 'PixelFormat8bppIndexed';
    PixelFormat16bppGrayScale : result := 'PixelFormat16bppGrayScale';
    PixelFormat16bppRGB555    : result := 'PixelFormat16bppRGB555';
    PixelFormat16bppRGB565    : result := 'PixelFormat16bppRGB565';
    PixelFormat16bppARGB1555  : result := 'PixelFormat16bppARGB1555';
    PixelFormat24bppRGB       : result := 'PixelFormat24bppRGB';
    PixelFormat32bppRGB       : result := 'PixelFormat32bppRGB';
    PixelFormat32bppARGB      : result := 'PixelFormat32bppARGB';
    PixelFormat32bppPARGB     : result := 'PixelFormat32bppPARGB';
    PixelFormat48bppRGB       : result := 'PixelFormat48bppRGB';
    PixelFormat64bppARGB      : result := 'PixelFormat64bppARGB';
    PixelFormat64bppPARGB     : result := 'PixelFormat64bppPARGB';
    PixelFormatMax            : result := 'PixelFormatMax';
  else
    result := '<UnKnown>';
  end;
end;

function MakeLangID(PrimaryLanguage, SubLanguage: LANGID): Word;
begin
  result := (SubLanguage shl 10) or PrimaryLanguage;
end;


