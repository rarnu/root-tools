{
 /***************************************************************************
                                graphics.pp
                                -----------
                             Graphic Controls
                   Initial Revision : Mon Jul 26 0:02:58 1999

 ***************************************************************************/

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit Graphics;

{$mode objfpc}{$H+}
{$I lcl_defines.inc}

interface

{$ifdef Trace}
{$ASSERTIONS ON}
{$endif}

{$IF FPC_FULLVERSION>=20601}
{$DEFINE HasFPCanvas1}
{$ENDIF}

{$IF FPC_FULLVERSION>=20603}
{$DEFINE HasFPEndCap}
{$ENDIF}

{$IF FPC_FULLVERSION>=20603}
{$DEFINE HasFPJoinStyle}
{$ENDIF}


uses
  SysUtils, Math, Types, Classes, Contnrs, FPCAdds, LazUTF8Classes,
  FileUtil,
  FPImage, FPCanvas,
  FPWriteBMP,              // bmp support
  FPWritePNG, PNGComn,     // png support
  FPReadPNM, FPWritePNM,   // PNM (Portable aNyMap) support
  FPReadJpeg, FPWriteJpeg, // jpg support
  FPReadTiff, FPTiffCmn,   // tiff support
  FPReadGif,
  AvgLvlTree,
  IntfGraphics,
  LCLStrConsts, LCLType, LCLProc, LMessages, LResources, LCLResCache,
  GraphType, IcnsTypes, GraphMath, WSReferences;

type
  PColor = ^TColor;
  TColor = TGraphicsColor;

  TFontPitch = (fpDefault, fpVariable, fpFixed);
  TFontName = string;
  TFontDataName = string[LF_FACESIZE -1];
  TFontStyle = (fsBold, fsItalic, fsStrikeOut, fsUnderline);
  TFontStyles = set of TFontStyle;
  TFontStylesbase = set of TFontStyle;
  TFontCharSet = 0..255;
  TFontQuality = (fqDefault, fqDraft, fqProof, fqNonAntialiased, fqAntialiased,
    fqCleartype, fqCleartypeNatural);

  TFontData = record
    Handle: HFont;
    Height: Integer;
    Pitch: TFontPitch;
    Style: TFontStylesBase;
    CharSet: TFontCharSet;
    Quality: TFontQuality;
    Name: TFontDataName;
    Orientation: Integer;
  end;

const
  // New TFont instances are initialized with the values in this structure.
  // About font default values: The default font is chosen by the interfaces
  // depending on the context. For example, there can be a different default
  // font for a button and a groupbox.
  DefFontData: TFontData = (
    Handle: 0;
    Height: 0;
    Pitch: fpDefault;
    Style: [];
    Charset: DEFAULT_CHARSET;
    Quality: fqDefault;
    Name: 'default';
    Orientation: 0;
    );

type
  { Reflects text style when drawn in a rectangle }

  TTextLayout = (tlTop, tlCenter, tlBottom);
  TTextStyle = packed record
    Alignment : TAlignment;  // TextRect Only: horizontal alignment

    Layout    : TTextLayout; // TextRect Only: vertical alignment

    SingleLine: boolean;     // If WordBreak is false then process #13, #10 as
                             // standard chars and perform no Line breaking.

    Clipping  : boolean;     // TextRect Only: Clip Text to passed Rectangle

    ExpandTabs: boolean;     // Replace #9 by apropriate amount of spaces (default is usually 8)

    ShowPrefix: boolean;     // TextRect Only: Process first single '&' per
                             //    line as an underscore and draw '&&' as '&'

    Wordbreak : boolean;     // TextRect Only: If line of text is too long
                             //    too fit between left and right boundaries
                             //    try to break into multiple lines between
                             //    words
                             //    See also EndEllipsis.

    Opaque    : boolean;     // TextRect: Fills background with current Brush
                             // TextOut : Fills background with current
                             //            foreground color

    SystemFont: Boolean;     // Use the system font instead of Canvas Font
    
    RightToLeft: Boolean;    //For RightToLeft text reading (Text Direction)

    EndEllipsis: Boolean;    // TextRect Only: If line of text is too long
                             //    to fit between left and right boundaries
                             //    truncates the text and adds "..."
                             //    If Wordbreak is set as well, Workbreak will
                             //    dominate.
  end;

const
  psSolid = FPCanvas.psSolid;
  psDash = FPCanvas.psDash;
  psDot = FPCanvas.psDot;
  psDashDot = FPCanvas.psDashDot;
  psDashDotDot = FPCanvas.psDashDotDot;
  psClear = FPCanvas.psClear;
  psInsideframe = FPCanvas.psInsideframe;
  psPattern = FPCanvas.psPattern;

  pmBlack = FPCanvas.pmBlack;
  pmWhite = FPCanvas.pmWhite;
  pmNop = FPCanvas.pmNop;
  pmNot = FPCanvas.pmNot;
  pmCopy = FPCanvas.pmCopy;
  pmNotCopy = FPCanvas.pmNotCopy;
  pmMergePenNot = FPCanvas.pmMergePenNot;
  pmMaskPenNot = FPCanvas.pmMaskPenNot;
  pmMergeNotPen = FPCanvas.pmMergeNotPen;
  pmMaskNotPen = FPCanvas.pmMaskNotPen;
  pmMerge = FPCanvas.pmMerge;
  pmNotMerge = FPCanvas.pmNotMerge;
  pmMask = FPCanvas.pmMask;
  pmNotMask = FPCanvas.pmNotMask;
  pmXor = FPCanvas.pmXor;
  pmNotXor = FPCanvas.pmNotXor;

  bsSolid = FPCanvas.bsSolid;
  bsClear = FPCanvas.bsClear;
  bsHorizontal = FPCanvas.bsHorizontal;
  bsVertical = FPCanvas.bsVertical;
  bsFDiagonal = FPCanvas.bsFDiagonal;
  bsBDiagonal = FPCanvas.bsBDiagonal;
  bsCross = FPCanvas.bsCross;
  bsDiagCross = FPCanvas.bsDiagCross;

  {$IFDEF HasFPEndCap}
  pecRound = FPCanvas.pecRound;
  pecSquare = FPCanvas.pecSquare;
  pecFlat = FPCanvas.pecFlat;
  {$ENDIF}

  {$IFDEF HasFPJoinStyle}
  pjsRound = FPCanvas.pjsRound;
  pjsBevel = FPCanvas.pjsBevel;
  pjsMiter =FPCanvas.pjsMiter;
  {$ENDIF}

type
  TFillStyle = TGraphicsFillStyle;
  TFillMode = (fmAlternate, fmWinding);

  TCopymode = longint;

  TCanvasStates = (csHandleValid,
                   csFontValid, // true if Font properties correspond to
                                // selected Font Handle in DC
                   csPenvalid, csBrushValid, csRegionValid);
  TCanvasState = set of TCanvasStates;
  TCanvasOrientation = (csLefttoRight, coRighttoLeft);

  { TProgressEvent is a generic progress notification event which may be
        used by TGraphic classes with computationally intensive (slow)
        operations, such as loading, storing, or transforming image data.
    Event params:
      Stage - Indicates whether this call to the OnProgress event is to
        prepare for, process, or clean up after a graphic operation.  If
        OnProgress is called at all, the first call for a graphic operation
        will be with Stage = psStarting, to allow the OnProgress event handler
        to allocate whatever resources it needs to process subsequent progress
        notifications.  After Stage = psStarting, you are guaranteed that
        OnProgress will be called again with Stage = psEnding to allow you
        to free those resources, even if the graphic operation is aborted by
        an exception.  Zero or more calls to OnProgress with Stage = psRunning
        may occur between the psStarting and psEnding calls.
      PercentDone - The ratio of work done to work remaining, on a scale of
        0 to 100.  Values may repeat or even regress (get smaller) in
        successive calls.  PercentDone is usually only a guess, and the
        guess may be dramatically altered as new information is discovered
        in decoding the image.
      RedrawNow - Indicates whether the graphic can be/should be redrawn
        immediately.  Useful for showing successive approximations of
        an image as data is available instead of waiting for all the data
        to arrive before drawing anything.  Since there is no message loop
        activity during graphic operations, you should call Update to force
        a control to be redrawn immediately in the OnProgress event handler.
        Redrawing a graphic when RedrawNow = False could corrupt the image
        and/or cause exceptions.
      Rect - Area of image that has changed and needs to be redrawn.
      Msg - Optional text describing in one or two words what the graphic
        class is currently working on.  Ex:  "Loading" "Storing"
        "Reducing colors".  The Msg string can also be empty.
        Msg strings should be resourced for translation,  should not
        contain trailing periods, and should be used only for
        display purposes.  (do not: if Msg = 'Loading' then...)
  }
  TProgressStage = TFPImgProgressStage;
  TProgressEvent = TFPImgProgressEvent;

  { For Delphi compatibility }
  TPixelFormat = (
    pfDevice,
    pf1bit,
    pf4bit,
    pf8bit,
    pf15bit,
    pf16bit,
    pf24bit,
    pf32bit,
    pfCustom
    );

const
  PIXELFORMAT_BPP: array[TPixelFormat] of Byte = (
    0, 1, 4, 8, 15, 16, 24, 32, 0
  );


type
  TTransparentMode = (
    tmAuto,
    tmFixed
    );

const
  // The following colors match the predefined Delphi Colors

  // standard colors
  clBlack   = TColor($000000);
  clMaroon  = TColor($000080);
  clGreen   = TColor($008000);
  clOlive   = TColor($008080);
  clNavy    = TColor($800000);
  clPurple  = TColor($800080);
  clTeal    = TColor($808000);
  clGray    = TColor($808080);
  clSilver  = TColor($C0C0C0);
  clRed     = TColor($0000FF);
  clLime    = TColor($00FF00);
  clYellow  = TColor($00FFFF);
  clBlue    = TColor($FF0000);
  clFuchsia = TColor($FF00FF);
  clAqua    = TColor($FFFF00);
  clLtGray  = TColor($C0C0C0); // clSilver alias
  clDkGray  = TColor($808080); // clGray alias
  clWhite   = TColor($FFFFFF);
  StandardColorsCount = 16;

  // extended colors
  clMoneyGreen = TColor($C0DCC0);
  clSkyBlue    = TColor($F0CAA6);
  clCream      = TColor($F0FBFF);
  clMedGray    = TColor($A4A0A0);
  ExtendedColorCount = 4;

  // special colors
  clNone    = TColor($1FFFFFFF);
  clDefault = TColor($20000000);

  // system colors
  clScrollBar               = TColor(SYS_COLOR_BASE or COLOR_SCROLLBAR);
  clBackground              = TColor(SYS_COLOR_BASE or COLOR_BACKGROUND);
  clActiveCaption           = TColor(SYS_COLOR_BASE or COLOR_ACTIVECAPTION);
  clInactiveCaption         = TColor(SYS_COLOR_BASE or COLOR_INACTIVECAPTION);
  clMenu                    = TColor(SYS_COLOR_BASE or COLOR_MENU);
  clWindow                  = TColor(SYS_COLOR_BASE or COLOR_WINDOW);
  clWindowFrame             = TColor(SYS_COLOR_BASE or COLOR_WINDOWFRAME);
  clMenuText                = TColor(SYS_COLOR_BASE or COLOR_MENUTEXT);
  clWindowText              = TColor(SYS_COLOR_BASE or COLOR_WINDOWTEXT);
  clCaptionText             = TColor(SYS_COLOR_BASE or COLOR_CAPTIONTEXT);
  clActiveBorder            = TColor(SYS_COLOR_BASE or COLOR_ACTIVEBORDER);
  clInactiveBorder          = TColor(SYS_COLOR_BASE or COLOR_INACTIVEBORDER);
  clAppWorkspace            = TColor(SYS_COLOR_BASE or COLOR_APPWORKSPACE);
  clHighlight               = TColor(SYS_COLOR_BASE or COLOR_HIGHLIGHT);
  clHighlightText           = TColor(SYS_COLOR_BASE or COLOR_HIGHLIGHTTEXT);
  clBtnFace                 = TColor(SYS_COLOR_BASE or COLOR_BTNFACE);
  clBtnShadow               = TColor(SYS_COLOR_BASE or COLOR_BTNSHADOW);
  clGrayText                = TColor(SYS_COLOR_BASE or COLOR_GRAYTEXT);
  clBtnText                 = TColor(SYS_COLOR_BASE or COLOR_BTNTEXT);
  clInactiveCaptionText     = TColor(SYS_COLOR_BASE or COLOR_INACTIVECAPTIONTEXT);
  clBtnHighlight            = TColor(SYS_COLOR_BASE or COLOR_BTNHIGHLIGHT);
  cl3DDkShadow              = TColor(SYS_COLOR_BASE or COLOR_3DDKSHADOW);
  cl3DLight                 = TColor(SYS_COLOR_BASE or COLOR_3DLIGHT);
  clInfoText                = TColor(SYS_COLOR_BASE or COLOR_INFOTEXT);
  clInfoBk                  = TColor(SYS_COLOR_BASE or COLOR_INFOBK);

  clHotLight                = TColor(SYS_COLOR_BASE or COLOR_HOTLIGHT);
  clGradientActiveCaption   = TColor(SYS_COLOR_BASE or COLOR_GRADIENTACTIVECAPTION);
  clGradientInactiveCaption = TColor(SYS_COLOR_BASE or COLOR_GRADIENTINACTIVECAPTION);
  clMenuHighlight           = TColor(SYS_COLOR_BASE or COLOR_MENUHILIGHT);
  clMenuBar                 = TColor(SYS_COLOR_BASE or COLOR_MENUBAR);
  clForm                    = TColor(SYS_COLOR_BASE or COLOR_FORM);

  // synonyms: do not show them in color lists
  clColorDesktop            = TColor(SYS_COLOR_BASE or COLOR_DESKTOP);
  cl3DFace                  = TColor(SYS_COLOR_BASE or COLOR_3DFACE);
  cl3DShadow                = TColor(SYS_COLOR_BASE or COLOR_3DSHADOW);
  cl3DHiLight               = TColor(SYS_COLOR_BASE or COLOR_3DHIGHLIGHT);
  clBtnHiLight              = TColor(SYS_COLOR_BASE or COLOR_BTNHILIGHT);

  clFirstSpecialColor = clBtnHiLight;

  clMask = clWhite;
  clDontMask = clBlack;

  // !! deprecated colors !!
  {$warnings off}
  // CLX base, mapped, pseudo, rgb values
  clForeground = TColor(-1) deprecated;
  clButton = TColor(-2) deprecated;
  clLight = TColor(-3) deprecated;
  clMidlight = TColor(-4) deprecated;
  clDark = TColor(-5) deprecated;
  clMid = TColor(-6) deprecated;
  clText = TColor(-7) deprecated;
  clBrightText = TColor(-8) deprecated;
  clButtonText = TColor(-9) deprecated;
  clBase = TColor(-10) deprecated;
  clxBackground = TColor(-11) deprecated;
  clShadow = TColor(-12) deprecated;
  clxHighlight = TColor(-13) deprecated;
  clHighlightedText = TColor(-14) deprecated;

  // CLX mapped role offsets
  cloNormal = 32 deprecated;
  cloDisabled = 64 deprecated;
  cloActive = 96 deprecated;

  // CLX normal, mapped, pseudo, rgb values
  clNormalForeground = TColor(clForeground - cloNormal) deprecated;
  clNormalButton = TColor(clButton - cloNormal) deprecated;
  clNormalLight = TColor(clLight - cloNormal) deprecated;
  clNormalMidlight = TColor(clMidlight - cloNormal) deprecated;
  clNormalDark = TColor(clDark - cloNormal) deprecated;
  clNormalMid = TColor(clMid - cloNormal) deprecated;
  clNormalText = TColor(clText - cloNormal) deprecated;
  clNormalBrightText = TColor(clBrightText - cloNormal) deprecated;
  clNormalButtonText = TColor(clButtonText - cloNormal) deprecated;
  clNormalBase = TColor(clBase - cloNormal) deprecated;
  clNormalBackground = TColor(clxBackground - cloNormal) deprecated;
  clNormalShadow = TColor(clShadow - cloNormal) deprecated;
  clNormalHighlight = TColor(clxHighlight - cloNormal) deprecated;
  clNormalHighlightedText = TColor(clHighlightedText - cloNormal) deprecated;

  // CLX disabled, mapped, pseudo, rgb values
  clDisabledForeground = TColor(clForeground - cloDisabled) deprecated;
  clDisabledButton = TColor(clButton - cloDisabled) deprecated;
  clDisabledLight = TColor(clLight - cloDisabled) deprecated;
  clDisabledMidlight = TColor(clMidlight - cloDisabled) deprecated;
  clDisabledDark = TColor(clDark - cloDisabled) deprecated;
  clDisabledMid = TColor(clMid - cloDisabled) deprecated;
  clDisabledText = TColor(clText - cloDisabled) deprecated;
  clDisabledBrightText = TColor(clBrightText - cloDisabled) deprecated;
  clDisabledButtonText = TColor(clButtonText - cloDisabled) deprecated;
  clDisabledBase = TColor(clBase - cloDisabled) deprecated;
  clDisabledBackground = TColor(clxBackground - cloDisabled) deprecated;
  clDisabledShadow = TColor(clShadow - cloDisabled) deprecated;
  clDisabledHighlight = TColor(clxHighlight - cloDisabled) deprecated;
  clDisabledHighlightedText = TColor(clHighlightedText - cloDisabled) deprecated;

  // CLX active, mapped, pseudo, rgb values
  clActiveForeground = TColor(clForeground - cloActive) deprecated;
  clActiveButton = TColor(clButton - cloActive) deprecated;
  clActiveLight = TColor(clLight - cloActive) deprecated;
  clActiveMidlight = TColor(clMidlight - cloActive) deprecated;
  clActiveDark = TColor(clDark - cloActive) deprecated;
  clActiveMid = TColor(clMid - cloActive) deprecated;
  clActiveText = TColor(clText - cloActive) deprecated;
  clActiveBrightText = TColor(clBrightText - cloActive) deprecated;
  clActiveButtonText = TColor(clButtonText - cloActive) deprecated;
  clActiveBase = TColor(clBase - cloActive) deprecated;
  clActiveBackground = TColor(clxBackground - cloActive) deprecated;
  clActiveShadow = TColor(clShadow - cloActive) deprecated;
  clActiveHighlight = TColor(clxHighlight - cloActive) deprecated;
  clActiveHighlightedText = TColor(clHighlightedText - cloActive) deprecated;

type
  TMappedColor = clActiveHighlightedText..clNormalForeground;

  TColorGroup = (cgInactive, cgDisabled, cgActive);
  TColorRole = (crForeground, crButton, crLight, crMidlight, crDark, crMid,
    crText, crBrightText, crButtonText, crBase, crBackground, crShadow,
    crHighlight, crHighlightText, crNoRole);
  {$warnings on}

const
  cmBlackness = BLACKNESS;
  cmDstInvert = DSTINVERT;
  cmMergeCopy = MERGECOPY;
  cmMergePaint = MERGEPAINT;
  cmNotSrcCopy = NOTSRCCOPY;
  cmNotSrcErase = NOTSRCERASE;
  cmPatCopy = PATCOPY;
  cmPatInvert = PATINVERT;
  cmPatPaint = PATPAINT;
  cmSrcAnd = SRCAND;
  cmSrcCopy = SRCCOPY;
  cmSrcErase = SRCERASE;
  cmSrcInvert = SRCINVERT;
  cmSrcPaint = SRCPAINT;
  cmWhiteness = WHITENESS;


type
  TCanvas = class;
  
  // base class
  TRasterImage = class;
  TRasterImageClass = class of TRasterImage;
  TCustomBitmap = class;
  TCustomBitmapClass = class of TCustomBitmap;
  // standard LCL graphic formats
  TBitmap = class;                  // bmp
  TPixmap = class;                  // xpm
  TIcon = class;                    // ico
  TPortableNetworkGraphic = class;  // png
  TPortableAnyMapGraphic = class;   // pnm formats: pbm, pgm and ppm
  TJpegImage = class;               // jpg
  TGIFImage = class;                // gif (read only)

  { TGraphicsObject
    In Delphi VCL this is the ancestor of TFont, TPen and TBrush.
    Since FPC 2.0 the LCL uses TFPCanvasHelper as ancestor. }

  TGraphicsObject = class(TPersistent)
  private
    FOnChanging: TNotifyEvent;
    FOnChange: TNotifyEvent;
    procedure DoChange(var Msg); message LM_CHANGED;
  protected
    procedure Changing; virtual;
    procedure Changed; virtual;
    procedure Lock;
    procedure UnLock;
  public
    property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  { TFontHandleCacheDescriptor }

  TFontHandleCacheDescriptor = class(TResourceCacheDescriptor)
  public
    LogFont: TLogFont;
    LongFontName: string;
  end;

  { TFontHandleCache }

  TFontHandleCache = class(TResourceCache)
  protected
    procedure RemoveItem(Item: TResourceCacheItem); override;
  public
    constructor Create;
    function CompareDescriptors(Tree: TAvgLvlTree; Desc1, Desc2: Pointer): integer; override;
    function FindFont(TheFont: TLCLHandle): TResourceCacheItem;
    function FindFontDesc(const LogFont: TLogFont;
                          const LongFontName: string): TFontHandleCacheDescriptor;
    function Add(TheFont: TLCLHandle; const LogFont: TLogFont;
                 const LongFontName: string): TFontHandleCacheDescriptor;
  end;

  { TFont }

  TFont = class(TFPCustomFont)
  private
    FCanUTF8: boolean;
    FCanUTF8Valid: boolean;
    FIsMonoSpace: boolean;
    FIsMonoSpaceValid: boolean;
    FOrientation: Integer;
    FPitch: TFontPitch;
    FQuality: TFontQuality;
    FStyle: TFontStylesBase;
    FCharSet: TFontCharSet;
    FPixelsPerInch: Integer;
    FUpdateCount: integer;
    FChanged: boolean;
    FFontHandleCached: boolean;
    FColor: TColor;
    FHeight: integer; // FHeight = -(FSize * FPixelsPerInch) div 72
    FReference: TWSFontReference;
    procedure FreeReference;
    function GetCanUTF8: boolean;
    function GetHandle: HFONT;
    function GetData: TFontData;
    function GetIsMonoSpace: boolean;
    function GetReference: TWSFontReference;
    function IsHeightStored: boolean;
    function IsNameStored: boolean;
    procedure SetData(const FontData: TFontData);
    procedure SetHandle(const Value: HFONT);
    procedure ReferenceNeeded;
  protected
    function GetCharSet: TFontCharSet;
    function GetHeight: Integer;
    function GetName: string;
    function GetOrientation: Integer;
    function GetPitch: TFontPitch;
    function GetSize: Integer;
    function GetStyle: TFontStyles;
    procedure Changed; override;
    procedure DoAllocateResources; override;
    procedure DoCopyProps(From: TFPCanvasHelper); override;
    procedure DoDeAllocateResources; override;
    procedure SetCharSet(const AValue: TFontCharSet);
    procedure SetColor(const NewColor: TColor; const NewFPColor: TFPColor); virtual;
    procedure SetColor(Value: TColor);
    function GetColor: TColor;
    procedure SetFlags(Index: integer; AValue: boolean); override;
    procedure SetFPColor(const AValue: TFPColor); override;
    procedure SetHeight(Avalue: Integer);
    procedure SetName(AValue: string); override;
    procedure SetOrientation(AValue: Integer); override; // This was introduced in 2.5 quite late, and the Android pre-compiled compiler was before this, so I prefer to let it only for 2.6
    procedure SetPitch(Value: TFontPitch);
    procedure SetSize(AValue: integer); override;
    procedure SetStyle(Value: TFontStyles);
    procedure SetQuality(const AValue: TFontQuality);
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Assign(const ALogFont: TLogFont);
    procedure BeginUpdate;
    procedure EndUpdate;
    property FontData: TFontData read GetData write SetData;
    function HandleAllocated: boolean;
    property Handle: HFONT read GetHandle write SetHandle; deprecated 'use Reference.Handle instead';
    function IsDefault: boolean;
    function IsEqual(AFont: TFont): boolean; virtual;
    property IsMonoSpace: boolean read GetIsMonoSpace;
    procedure SetDefault;
    property CanUTF8: boolean read GetCanUTF8; deprecated;
    property PixelsPerInch: Integer read FPixelsPerInch write FPixelsPerInch;
    property Reference: TWSFontReference read GetReference;
  published
    property CharSet: TFontCharSet read GetCharSet write SetCharSet default DEFAULT_CHARSET;
    property Color: TColor read FColor write SetColor default {$ifdef UseCLDefault}clDefault{$else}clWindowText{$endif};
    property Height: Integer read GetHeight write SetHeight stored IsHeightStored;
    property Name: string read GetName write SetName stored IsNameStored;
    property Orientation: Integer read GetOrientation write SetOrientation default 0;
    property Pitch: TFontPitch read GetPitch write SetPitch default fpDefault;
    property Quality: TFontQuality read FQuality write SetQuality default fqDefault;
    property Size: Integer read GetSize write SetSize stored false;
    property Style: TFontStyles read GetStyle write SetStyle default [];
  end;

  { TPen }

  TPenStyle = TFPPenStyle;
  TPenMode = TFPPenMode;

  // pen end caps. valid only for geometric pens
  {$IFDEF HasFPEndCap}
  TPenEndCap = TFPPenEndCap;
  {$ELSE}
  TPenEndCap = (
    pecRound,
    pecSquare,
    pecFlat
  );
  {$ENDIF}

  // join style. valid only for geometric pens
  {$IFDEF HasFPJoinStyle}
  TPenJoinStyle = FPCanvas.TFPPenJoinStyle;
  {$ELSE}
  TPenJoinStyle = (
    pjsRound,
    pjsBevel,
    pjsMiter
  );
  {$ENDIF}

  TPenPattern = array of LongWord;

  { TPenHandleCacheDescriptor }

  TPenHandleCacheDescriptor = class(TResourceCacheDescriptor)
  public
    ExtPen: TExtLogPen;
    Pattern: TPenPattern;
  end;

  { TPenHandleCache }

  TPenHandleCache = class(TResourceCache)
  protected
    procedure RemoveItem(Item: TResourceCacheItem); override;
  public
    constructor Create;
    function CompareDescriptors(Tree: TAvgLvlTree; Desc1, Desc2: Pointer): integer; override;
    function FindPen(APen: TLCLHandle): TResourceCacheItem;
    function FindPenDesc(const AExtPen: TExtLogPen;
                         const APattern: TPenPattern): TPenHandleCacheDescriptor;
    function Add(APen: TLCLHandle; const AExtPen: TExtLogPen;
                 const APattern: TPenPattern): TPenHandleCacheDescriptor;
  end;

  TPen = class(TFPCustomPen)
  private
    FColor: TColor;
    {$IFNDEF HasFPEndCap}
    FEndCap: TPenEndCap;
    {$ENDIF}
    FCosmetic: Boolean;
    {$IFNDEF HasFPJoinStyle}
    FJoinStyle: TPenJoinStyle;
    {$ENDIF}
    FPattern: TPenPattern;
    FPenHandleCached: boolean;
    FReference: TWSPenReference;
    procedure FreeReference;
    function GetHandle: HPEN;
    function GetReference: TWSPenReference;
    procedure ReferenceNeeded;
    procedure SetCosmetic(const AValue: Boolean);
    procedure SetHandle(const Value: HPEN);
  protected
    procedure DoAllocateResources; override;
    procedure DoDeAllocateResources; override;
    procedure DoCopyProps(From: TFPCanvasHelper); override;
    procedure SetColor(const NewColor: TColor; const NewFPColor: TFPColor); virtual;
    procedure SetFPColor(const AValue: TFPColor); override;
    procedure SetColor(Value: TColor);
    procedure SetEndCap(AValue: TPenEndCap); {$IFDEF HasFPEndCap}override;{$ENDIF}
    procedure SetJoinStyle(AValue: TPenJoinStyle); {$IFDEF HasFPJoinStyle}override;{$ENDIF}
    procedure SetMode(Value: TPenMode); override;
    procedure SetStyle(Value: TPenStyle); override;
    procedure SetWidth(value: Integer); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property Handle: HPEN read GetHandle write SetHandle; deprecated;
    property Reference: TWSPenReference read GetReference;

    function GetPattern: TPenPattern;
    procedure SetPattern(APattern: TPenPattern); reintroduce;
  published
    property Color: TColor read FColor write SetColor default clBlack;
    property Cosmetic: Boolean read FCosmetic write SetCosmetic default True;
    {$IFDEF HasFPEndCap}
    property EndCap default pecRound;
    {$ELSE}
    property EndCap: TPenEndCap read FEndCap write SetEndCap default pecRound;
    {$ENDIF}
    {$IFDEF HasFPJoinStyle}
    property JoinStyle default pjsRound;
    {$ELSE}
    property JoinStyle: TPenJoinStyle read FJoinStyle write SetJoinStyle default pjsRound;
    {$ENDIF}
    property Mode default pmCopy;
    property Style default psSolid;
    property Width default 1;
  end;

  { TBrush }

  TBrushStyle = TFPBrushStyle;

  TBrushHandleCache = class(TBlockResourceCache)
  protected
    procedure RemoveItem(Item: TResourceCacheItem); override;
  public
    constructor Create;
  end;

  TBrush = class(TFPCustomBrush)
  private
    FBrushHandleCached: boolean;
    FColor: TColor;
    FBitmap: TCustomBitmap;
    FReference: TWSBrushReference;
    FInternalUpdateIndex: Integer;
    procedure FreeReference;
    function GetHandle: HBRUSH;
    function GetReference: TWSBrushReference;
    function GetColor: TColor;
    procedure ReferenceNeeded;
    procedure SetHandle(const Value: HBRUSH);
    procedure DoChange(var Msg); message LM_CHANGED;
  protected
    procedure DoAllocateResources; override;
    procedure DoDeAllocateResources; override;
    procedure DoCopyProps(From: TFPCanvasHelper); override;
    procedure SetColor(const NewColor: TColor; const NewFPColor: TFPColor); virtual;
    procedure SetFPColor(const AValue: TFPColor); override;
    procedure SetBitmap(Value: TCustomBitmap);
    procedure SetColor(Value: TColor);
    procedure SetStyle(Value: TBrushStyle); override;
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create; override;
    destructor Destroy; override;
    function EqualsBrush(ABrush: TBrush): boolean;
    property Bitmap: TCustomBitmap read FBitmap write SetBitmap;
    property Handle: HBRUSH read GetHandle write SetHandle; deprecated; // use instead Reference.Handle
    property Reference: TWSBrushReference read GetReference;
  published
    property Color: TColor read FColor write SetColor default clWhite;
    property Style default bsSolid;
  end;

  TRegionCombineMode = (rgnAnd, rgnCopy, rgnDiff, rgnOr, rgnXOR);

  TRegionOperationType = (rgnNewRect, rgnCombine);

  TRegionOperation = record
    ROType: TRegionOperationType;
    Source1, Source2, Dest: Integer; // Index to the list of sub-regions, -1 indicates the main region
    CombineMode: TRegionCombineMode; // Used only if ROType=rgnCombine
    Rect: TRect; // Used for ROType=rgnNewRect
  end;

  TRegionOperations = array of TRegionOperation;

  { TRegion }

  TRegion = class(TGraphicsObject)
  private
    FReference: TWSRegionReference;
    // Description of the region
    //RegionOperations: TRegionOperations;
    //SubRegions: array of HRGN;
    procedure AddOperation(AOp: TRegionOperation);
    procedure ClearSubRegions();
    procedure AddSubRegion(AHandle: HRGN);
    //
    procedure FreeReference;
    function GetReference: TWSRegionReference;
    function GetHandle: HRGN;
    procedure ReferenceNeeded;
    procedure SetHandle(const Value: HRGN);
  protected
    procedure SetClipRect(value: TRect);
    function GetClipRect: TRect;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    // Convenience routines to add elements to the region
    procedure AddRectangle(X1, Y1, X2, Y2: Integer);

    property ClipRect: TRect read GetClipRect write SetClipRect;
    property Handle: HRGN read GetHandle write SetHandle; deprecated;
    property Reference: TWSRegionReference read GetReference;
  end;


  { TGraphic }

  { The TGraphic class is an abstract base class for dealing with graphic images
    such as bitmaps, pixmaps, icons, and other image formats.
      LoadFromFile - Read the graphic from the file system.  The old contents of
        the graphic are lost.  If the file is not of the right format, an
        exception will be generated.
      SaveToFile - Writes the graphic to disk in the file provided.
      LoadFromStream - Like LoadFromFile except source is a stream (e.g.
        TBlobStream).
      SaveToStream - stream analogue of SaveToFile.
      LoadFromClipboardFormat - Replaces the current image with the data
        provided.  If the TGraphic does not support that format it will generate
        an exception.
      SaveToClipboardFormats - Converts the image to a clipboard format.  If the
        image does not support being translated into a clipboard format it
        will generate an exception.
      Height - The native, unstretched, height of the graphic.
      Palette - Color palette of image.  Zero if graphic doesn't need/use palettes.
      Transparent - Some parts of the image are not opaque. aka the background
        can be seen through.
      Width - The native, unstretched, width of the graphic.
      OnChange - Called whenever the graphic changes
      PaletteModified - Indicates in OnChange whether color palette has changed.
        Stays true until whoever's responsible for realizing this new palette
        (ex: TImage) sets it to False.
      OnProgress - Generic progress indicator event. Propagates out to TPicture
        and TImage OnProgress events.}

  TGraphic = class(TPersistent)
  private
    FModified: Boolean;
    FOnChange: TNotifyEvent;
    FOnProgress: TProgressEvent;
    FPaletteModified: Boolean;
  protected
    procedure Changed(Sender: TObject); virtual;
    function Equals(Graphic: TGraphic): Boolean; virtual; {$IF declared(vmtEquals)}overload;{$ENDIF}
    procedure DefineProperties(Filer: TFiler); override;
    procedure Draw(ACanvas: TCanvas; const Rect: TRect); virtual; abstract;
    function GetEmpty: Boolean; virtual; abstract;
    function GetHeight: Integer; virtual; abstract;
    function GetMimeType: string; virtual;
    function GetPalette: HPALETTE; virtual;
    function GetTransparent: Boolean; virtual; abstract;
    function GetWidth: Integer; virtual; abstract;
    procedure Progress(Sender: TObject; Stage: TProgressStage;
      PercentDone: Byte;  RedrawNow: Boolean; const R: TRect;
      const Msg: string; var DoContinue: boolean); virtual;
    procedure Progress(Sender: TObject; Stage: TProgressStage;
      PercentDone: Byte;  RedrawNow: Boolean; const R: TRect;
      const Msg: string); virtual;
    procedure ReadData(Stream: TStream); virtual; // used by Filer
    procedure SetHeight(Value: Integer); virtual; abstract;
    procedure SetPalette(Value: HPALETTE); virtual;
    procedure SetTransparent(Value: Boolean); virtual; abstract;
    procedure SetWidth(Value: Integer); virtual; abstract;
    procedure SetModified(Value: Boolean);
    procedure WriteData(Stream: TStream); virtual; // used by filer
  public
    procedure Assign(ASource: TPersistent); override;
    constructor Create; virtual;
    procedure Clear; virtual;
    {$IF declared(vmtEquals)}
    function Equals(Obj: TObject): Boolean; override; overload;
    {$ENDIF}
    function LazarusResourceTypeValid(const AResourceType: string): boolean; virtual;
    procedure LoadFromFile(const Filename: string); virtual;
    procedure LoadFromStream(Stream: TStream); virtual; abstract;
    procedure LoadFromMimeStream(AStream: TStream; const AMimeType: string); virtual;
    procedure LoadFromLazarusResource(const ResName: String); virtual;
    procedure LoadFromResourceName(Instance: THandle; const ResName: String); virtual;
    procedure LoadFromResourceID(Instance: THandle; ResID: PtrInt); virtual;
    procedure LoadFromClipboardFormat(FormatID: TClipboardFormat); virtual;
    procedure LoadFromClipboardFormatID(ClipboardType: TClipboardType;
      FormatID: TClipboardFormat); virtual;
    procedure SaveToFile(const Filename: string); virtual;
    procedure SaveToStream(Stream: TStream); virtual; abstract;
    procedure SaveToClipboardFormat(FormatID: TClipboardFormat); virtual;
    procedure SaveToClipboardFormatID(ClipboardType: TClipboardType;
      FormatID: TClipboardFormat); virtual;
    procedure GetSupportedSourceMimeTypes(List: TStrings); virtual;
    function GetResourceType: TResourceType; virtual;
    class function GetFileExtensions: string; virtual;
    class function IsStreamFormatSupported(Stream: TStream): Boolean; virtual;
  public
    property Empty: Boolean read GetEmpty;
    property Height: Integer read GetHeight write SetHeight;
    property Modified: Boolean read FModified write SetModified;
    property MimeType: string read GetMimeType;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnProgress: TProgressEvent read FOnProgress write FOnProgress;
    property Palette: HPALETTE read GetPalette write SetPalette;
    property PaletteModified: Boolean read FPaletteModified write FPaletteModified;
    property Transparent: Boolean read GetTransparent write SetTransparent;
    property Width: Integer read GetWidth write SetWidth;
  end;

  TGraphicClass = class of TGraphic;


  { TPicture }

  { TPicture is a TGraphic container.  It is used in place of a TGraphic if the
    graphic can be of any TGraphic class.  LoadFromFile and SaveToFile are
    polymorphic. For example, if the TPicture is holding an Icon, you can
    LoadFromFile a bitmap file, where if the class is TIcon you could only read
    .ICO files.

      LoadFromFile - Reads a picture from disk. The TGraphic class created
        determined by the file extension of the file. If the file extension is
        not recognized an exception is generated.
      SaveToFile - Writes the picture to disk.
      LoadFromClipboardFormat - ToDo: Reads the picture from the handle provided in
        the given clipboard format.  If the format is not supported, an
        exception is generated.
      SaveToClipboardFormats - ToDo: Allocates a global handle and writes the picture
        in its native clipboard format (CF_BITMAP for bitmaps, CF_METAFILE
        for metafiles, etc.).  Formats will contain the formats written.
        Returns the number of clipboard items written to the array pointed to
        by Formats and Datas or would be written if either Formats or Datas are
        nil.
      SupportsClipboardFormat - Returns true if the given clipboard format
        is supported by LoadFromClipboardFormat.
      Assign - Copys the contents of the given TPicture.  Used most often in
        the implementation of TPicture properties.
      RegisterFileFormat - Register a new TGraphic class for use in
        LoadFromFile.
      RegisterClipboardFormat - Registers a new TGraphic class for use in
        LoadFromClipboardFormat.
      UnRegisterGraphicClass - Removes all references to the specified TGraphic
        class and all its descendents from the file format and clipboard format
        internal lists.
      Height - The native, unstretched, height of the picture.
      Width - The native, unstretched, width of the picture.
      Graphic - The TGraphic object contained by the TPicture
      Bitmap - Returns a bitmap.  If the contents is not already a bitmap, the
        contents are thrown away and a blank bitmap is returned.
      Pixmap - Returns a pixmap.  If the contents is not already a pixmap, the
        contents are thrown away and a blank pixmap is returned.
      PNG - Returns a png.  If the contents is not already a png, the
        contents are thrown away and a blank png (TPortableNetworkGraphic) is
        returned.
      PNM - Returns a pnm.  If the contents is not already a pnm, the
        contents are thrown away and a blank pnm (TPortableAnyMapGraphic) is
        returned.
      Jpeg - Returns a jpeg. If the contents is not already a jpeg, the
        contents are thrown away and a blank jpeg (TJPegImage) is
        returned.
      }

  TPicture = class(TPersistent)
  private
    FGraphic: TGraphic;
    FOnChange: TNotifyEvent;
    //FNotify: IChangeNotifier;
    FOnProgress: TProgressEvent;
    procedure ForceType(GraphicType: TGraphicClass);
    function GetBitmap: TBitmap;
    function GetIcon: TIcon;
    function GetJpeg: TJpegImage;
    function GetPNG: TPortableNetworkGraphic;
    function GetPNM: TPortableAnyMapGraphic;
    function GetPixmap: TPixmap;
    function GetHeight: Integer;
    function GetWidth: Integer;
    procedure ReadData(Stream: TStream);
    procedure SetBitmap(Value: TBitmap);
    procedure SetIcon(Value: TIcon);
    procedure SetJpeg(Value: TJpegImage);
    procedure SetPNG(const AValue: TPortableNetworkGraphic);
    procedure SetPNM(const AValue: TPortableAnyMapGraphic);
    procedure SetPixmap(Value: TPixmap);
    procedure SetGraphic(Value: TGraphic);
    procedure WriteData(Stream: TStream);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure Changed(Sender: TObject); virtual;
    procedure DefineProperties(Filer: TFiler); override;
    procedure Progress(Sender: TObject; Stage: TProgressStage;
                       PercentDone: Byte; RedrawNow: Boolean; const R: TRect;
                       const Msg: string; var DoContinue: boolean); virtual;
    procedure LoadFromStreamWithClass(Stream: TStream; AClass: TGraphicClass);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear; virtual;
    // load methods
    procedure LoadFromClipboardFormat(FormatID: TClipboardFormat);
    procedure LoadFromClipboardFormatID(ClipboardType: TClipboardType; FormatID: TClipboardFormat);
    procedure LoadFromFile(const Filename: string);
    procedure LoadFromResourceName(Instance: THandle; const ResName: String);
    procedure LoadFromResourceName(Instance: THandle; const ResName: String; AClass: TGraphicClass);
    procedure LoadFromLazarusResource(const AName: string);
    procedure LoadFromStream(Stream: TStream);
    procedure LoadFromStreamWithFileExt(Stream: TStream; const FileExt: string);
    // save methods
    procedure SaveToClipboardFormat(FormatID: TClipboardFormat);
    procedure SaveToFile(const Filename: string; const FileExt: string = '');
    procedure SaveToStream(Stream: TStream);
    procedure SaveToStreamWithFileExt(Stream: TStream; const FileExt: string);

    class function SupportsClipboardFormat(FormatID: TClipboardFormat): Boolean;
    procedure Assign(Source: TPersistent); override;
    class procedure RegisterFileFormat(const AnExtension, ADescription: string;
      AGraphicClass: TGraphicClass);
    class procedure RegisterClipboardFormat(FormatID: TClipboardFormat;
      AGraphicClass: TGraphicClass);
    class procedure UnregisterGraphicClass(AClass: TGraphicClass);
    function FindGraphicClassWithFileExt(const Ext: string;
      ExceptionOnNotFound: boolean = true): TGraphicClass;
  public
    property Bitmap: TBitmap read GetBitmap write SetBitmap;
    property Icon: TIcon read GetIcon write SetIcon;
    property Jpeg: TJpegImage read GetJpeg write SetJpeg;
    property Pixmap: TPixmap read GetPixmap write SetPixmap;
    property PNG: TPortableNetworkGraphic read GetPNG write SetPNG;
    property PNM: TPortableAnyMapGraphic read GetPNM write SetPNM;
    property Graphic: TGraphic read FGraphic write SetGraphic;
    //property PictureAdapter: IChangeNotifier read FNotify write FNotify;
    property Height: Integer read GetHeight;
    property Width: Integer read GetWidth;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnProgress: TProgressEvent read FOnProgress write FOnProgress;
  end;


  EGraphicException = class(Exception);
  EInvalidGraphic = class(EGraphicException);
  EInvalidGraphicOperation = class(EGraphicException);

type
  TGradientDirection = (
    gdVertical,   // Fill vertical
    gdHorizontal  // Fill Horizontal
  );

  TAntialiasingMode = (
    amDontCare, // default antialiasing
    amOn,       // enabled
    amOff       // disabled
  );

  TLCLTextMetric = record
    Ascender: Integer;
    Descender: Integer;
    Height: Integer;
  end;

  TDefaultColorType = (
    dctBrush,
    dctFont
  );

  { TCanvas }

  TCanvas = class(TFPCustomCanvas)
  private
    FAntialiasingMode: TAntialiasingMode;
    FAutoRedraw: Boolean;
    FState: TCanvasState;
    FSavedFontHandle: HFont;
    FSavedPenHandle: HPen;
    FSavedBrushHandle: HBrush;
    FSavedRegionHandle: HRGN;
    FCopyMode: TCopyMode;
    FHandle: HDC;
    FOnChange: TNotifyEvent;
    FOnChanging: TNotifyEvent;
    FTextStyle: TTextStyle;
    FLock: TCriticalSection;// FLock is initialized on demand
    FRegion: TRegion;
    FLazPen: TPen;
    FLazFont: TFont;
    FLazBrush: TBrush;
    FSavedHandleStates: TFPList;
    procedure BrushChanged(ABrush: TObject);
    procedure FontChanged(AFont: TObject);
    procedure PenChanged(APen: TObject);
    procedure RegionChanged(ARegion: TObject);
    function GetHandle: HDC;
    procedure SetAntialiasingMode(const AValue: TAntialiasingMode);
    procedure SetAutoRedraw(Value: Boolean); virtual;
    procedure SetLazFont(Value: TFont);
    procedure SetLazPen(Value: TPen);
    procedure SetLazBrush(Value: TBrush);
    procedure SetRegion(Value: TRegion);
  protected
    function DoCreateDefaultFont: TFPCustomFont; override;
    function DoCreateDefaultPen: TFPCustomPen; override;
    function DoCreateDefaultBrush: TFPCustomBrush; override;
    procedure SetColor(x, y: integer; const Value: TFPColor); override;
    function  GetColor(x, y: integer): TFPColor; override;
    procedure SetHeight(AValue: integer); override;
    function  GetHeight: integer; override;
    procedure SetWidth(AValue: integer); override;
    function  GetWidth: integer; override;
    procedure SetPenPos(const AValue: TPoint); override;
    procedure DoLockCanvas; override;
    procedure DoUnlockCanvas; override;
    procedure DoTextOut(x, y: integer; Text: string); override;
    procedure DoGetTextSize(Text: string; var w,h:integer); override;
    function  DoGetTextHeight(Text: string): integer; override;
    function  DoGetTextWidth(Text: string): integer; override;
    procedure DoRectangle(const Bounds: TRect); override;
    procedure DoRectangleFill(const Bounds: TRect); override;
    procedure DoRectangleAndFill(const Bounds: TRect); override;
    procedure DoEllipse(const Bounds: TRect); override;
    procedure DoEllipseFill(const Bounds: TRect); override;
    procedure DoEllipseAndFill(const Bounds: TRect); override;
    procedure DoPolygon(const Points: array of TPoint); override;
    procedure DoPolygonFill(const Points: array of TPoint); override;
    procedure DoPolygonAndFill(const Points: array of TPoint); override;
    procedure DoPolyline(const Points: array of TPoint); override;
    procedure DoPolyBezier(Points: PPoint; NumPts: Integer;
                           Filled: boolean = False;
                           Continuous: boolean = False); override;
    procedure DoFloodFill(x, y: integer); override;
    procedure DoMoveTo(x, y: integer); override;
    procedure DoLineTo(x, y: integer); override;
    procedure DoLine(x1, y1, x2, y2: integer); override;
    procedure DoCopyRect(x, y: integer; SrcCanvas: TFPCustomCanvas;
                         const SourceRect: TRect); override;
    procedure DoDraw(x, y: integer; const Image: TFPCustomImage); override;
    procedure CheckHelper(AHelper: TFPCanvasHelper); override;
    function GetDefaultColor(const ADefaultColorType: TDefaultColorType): TColor; virtual;
  protected
    function GetClipRect: TRect; override;
    procedure SetClipRect(const ARect: TRect); override;
    function GetClipping: Boolean; override;
    procedure SetClipping(const AValue: boolean); override;
    function GetPixel(X,Y: Integer): TColor; virtual;
    procedure CreateBrush; virtual;
    procedure CreateFont; virtual;
    procedure CreateHandle; virtual;
    procedure CreatePen; virtual;
    procedure CreateRegion; virtual;
    procedure DeselectHandles; virtual;
    procedure PenChanging(APen: TObject); virtual;
    procedure FontChanging(AFont: TObject); virtual;
    procedure BrushChanging(ABrush: TObject); virtual;
    procedure RegionChanging(ARegion: TObject); virtual;
    procedure RealizeAutoRedraw; virtual;
    procedure RealizeAntialiasing; virtual;
    procedure RequiredState(ReqState: TCanvasState); virtual;
    procedure SetHandle(NewHandle: HDC); virtual;
    procedure SetInternalPenPos(const Value: TPoint); virtual;
    procedure SetPixel(X,Y: Integer; Value: TColor); virtual;
    procedure FreeHandle;virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Lock; virtual;
    function TryLock: Boolean;
    procedure Unlock; virtual;
    procedure Refresh; virtual;
    procedure Changing; virtual;
    procedure Changed; virtual;
    procedure SaveHandleState; virtual;
    procedure RestoreHandleState; virtual;

    // extra drawing methods (there are more in the ancestor TFPCustomCanvas)
    procedure Arc(ALeft, ATop, ARight, ABottom, Angle16Deg, Angle16DegLength: Integer); virtual; {$IFDEF HasFPCanvas1}reintroduce;{$ENDIF}
    procedure Arc(ALeft, ATop, ARight, ABottom, SX, SY, EX, EY: Integer); virtual; {$IFDEF HasFPCanvas1}reintroduce;{$ENDIF}
    procedure ArcTo(ALeft, ATop, ARight, ABottom, SX, SY, EX, EY: Integer); virtual; //As Arc(), but updates pen position
    procedure AngleArc(X, Y: Integer; Radius: Longword; StartAngle, SweepAngle: Single);
    procedure BrushCopy(ADestRect: TRect; ABitmap: TBitmap; ASourceRect: TRect;
                        ATransparentColor: TColor); virtual;
    procedure Chord(x1, y1, x2, y2,
                    Angle16Deg, Angle16DegLength: Integer); virtual; {$IFDEF HasFPCanvas1}reintroduce;{$ENDIF}
    procedure Chord(x1, y1, x2, y2, SX, SY, EX, EY: Integer); virtual; {$IFDEF HasFPCanvas1}reintroduce;{$ENDIF}
    procedure CopyRect(const Dest: TRect; SrcCanvas: TCanvas;
                       const Source: TRect); virtual;
    procedure Draw(X,Y: Integer; SrcGraphic: TGraphic); virtual;
    procedure DrawFocusRect(const ARect: TRect); virtual;
    procedure StretchDraw(const DestRect: TRect; SrcGraphic: TGraphic); virtual;
    procedure Ellipse(const ARect: TRect); {$IFDEF HasFPCanvas1}reintroduce;{$ENDIF}
    procedure Ellipse(x1, y1, x2, y2: Integer); virtual; {$IFDEF HasFPCanvas1}reintroduce;{$ENDIF}
    procedure FillRect(const ARect: TRect); virtual; {$IFDEF HasFPCanvas1}reintroduce;{$ENDIF}
    procedure FillRect(X1,Y1,X2,Y2: Integer); {$IFDEF HasFPCanvas1}reintroduce;{$ENDIF}
    procedure FloodFill(X, Y: Integer; FillColor: TColor;
                        FillStyle: TFillStyle); virtual; {$IFDEF HasFPCanvas1}reintroduce;{$ENDIF}
    procedure Frame3d(var ARect: TRect; const FrameWidth: integer;
                      const Style: TGraphicsBevelCut); virtual;
    procedure Frame3D(var ARect: TRect; TopColor, BottomColor: TColor;
                      const FrameWidth: integer); overload;
    procedure Frame(const ARect: TRect); virtual; // border using pen
    procedure Frame(X1,Y1,X2,Y2: Integer);     // border using pen
    procedure FrameRect(const ARect: TRect); virtual; // border using brush
    procedure FrameRect(X1,Y1,X2,Y2: Integer); // border using brush
    function  GetTextMetrics(out TM: TLCLTextMetric): boolean; virtual;
    procedure GradientFill(ARect: TRect; AStart, AStop: TColor; ADirection: TGradientDirection);
    procedure RadialPie(x1, y1, x2, y2,
                        StartAngle16Deg, Angle16DegLength: Integer); virtual; {$IFDEF HasFPCanvas1}reintroduce;{$ENDIF}
    procedure Pie(EllipseX1,EllipseY1,EllipseX2,EllipseY2,
                  StartX,StartY,EndX,EndY: Integer); virtual;
    procedure PolyBezier(Points: PPoint; NumPts: Integer;
                         Filled: boolean = False;
                         Continuous: boolean = False); virtual; {$IFDEF HasFPCanvas1}reintroduce;{$ENDIF}
    procedure PolyBezier(const Points: array of TPoint;
                         Filled: boolean = False;
                         Continuous: boolean = False); {$IFDEF HasFPCanvas1}reintroduce;{$ENDIF}
    procedure Polygon(const Points: array of TPoint;
                      Winding: Boolean;
                      StartIndex: Integer = 0;
                      NumPts: Integer = -1);
    procedure Polygon(Points: PPoint; NumPts: Integer;
                      Winding: boolean = False); virtual;
    procedure Polygon(const Points: array of TPoint); {$IFDEF HasFPCanvas1}reintroduce;{$ENDIF}
    procedure Polyline(const Points: array of TPoint;
                       StartIndex: Integer;
                       NumPts: Integer = -1);
    procedure Polyline(Points: PPoint; NumPts: Integer); virtual;
    procedure Polyline(const Points: array of TPoint); {$IFDEF HasFPCanvas1}reintroduce;{$ENDIF}
    procedure Rectangle(X1,Y1,X2,Y2: Integer); virtual; {$IFDEF HasFPCanvas1}reintroduce;{$ENDIF}
    procedure Rectangle(const ARect: TRect); {$IFDEF HasFPCanvas1}reintroduce;{$ENDIF}
    procedure RoundRect(X1, Y1, X2, Y2: Integer; RX,RY: Integer); virtual;
    procedure RoundRect(const Rect: TRect; RX,RY: Integer);
    procedure TextOut(X,Y: Integer; const Text: String); virtual; {$IFDEF HasFPCanvas1}reintroduce;{$ENDIF}
    procedure TextRect(const ARect: TRect; X, Y: integer; const Text: string);
    procedure TextRect(ARect: TRect; X, Y: integer; const Text: string;
                       const Style: TTextStyle); virtual;
    function TextExtent(const Text: string): TSize; virtual; {$IFDEF HasFPCanvas1}reintroduce;{$ENDIF}
    function TextHeight(const Text: string): Integer; virtual; {$IFDEF HasFPCanvas1}reintroduce;{$ENDIF}
    function TextWidth(const Text: string): Integer; virtual; {$IFDEF HasFPCanvas1}reintroduce;{$ENDIF}
    function TextFitInfo(const Text: string; MaxWidth: Integer): Integer;
    function HandleAllocated: boolean; virtual;
    function GetUpdatedHandle(ReqState: TCanvasState): HDC; virtual;
  public
    property Pixels[X, Y: Integer]: TColor read GetPixel write SetPixel;
    property Handle: HDC read GetHandle write SetHandle;
    property TextStyle: TTextStyle read FTextStyle write FTextStyle;
  published
    property AntialiasingMode: TAntialiasingMode read FAntialiasingMode write SetAntialiasingMode default amDontCare;
    property AutoRedraw: Boolean read FAutoRedraw write SetAutoRedraw;
    property Brush: TBrush read FLazBrush write SetLazBrush;
    property CopyMode: TCopyMode read FCopyMode write FCopyMode default cmSrcCopy;
    property Font: TFont read FLazFont write SetLazFont;
    property Height: integer read GetHeight;
    property Pen: TPen read FLazPen write SetLazPen;
    property Region: TRegion read FRegion write SetRegion;
    property Width: integer read GetWidth;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;
  end;


  { TSharedImage -  base class for reference counted images }

  TSharedImage = class
  private
    FRefCount: Integer;
  protected
    procedure Reference; // increase reference count
    procedure Release;   // decrease reference count
    procedure FreeHandle; virtual; abstract;
    property RefCount: Integer read FRefCount;
  public
    function HandleAllocated: boolean; virtual; abstract;
  end;


  { TCustomBitmapImage

    Descendent of TSharedImage for TCustomBitmap. If a TCustomBitmap is assigned to another
    TCustomBitmap, only the reference count will be increased and both will share the
    same TCustomBitmapImage }

  TBitmapHandleType = (bmDIB, bmDDB);

  { TSharedCustomBitmap }

  { TSharedCustomBitmap is base class used for sharing imagedata for derived
    classes of TCustomBitmap. Data can only be shared between classes of the
    same type. IE. TBitmap data can only be shared with (descendant of) TBitmap.
    Therefore each graphic "end" class should define its own share class.
  }

  TSharedRasterImage = class(TSharedImage)
  private
    FHandle: THandle; // generic type, can be HBITMAP or HICON or ....
    FBitmapCanvas: TCanvas; // current canvas selected into
    FSaveStream: TMemoryStream;
  protected
    procedure FreeHandle; override;
    function ReleaseHandle: THandle; virtual;
    function IsEmpty: boolean; virtual;
  public
    constructor Create; virtual;
    procedure CreateDefaultHandle(AWidth, AHeight: Integer; ABPP: Byte); virtual; abstract;
    destructor Destroy; override;
    function HandleAllocated: boolean; override;
    property BitmapCanvas: TCanvas read FBitmapCanvas write FBitmapCanvas;
    property SaveStream: TMemoryStream read FSaveStream write FSaveStream;
  end;

  TSharedRasterImageClass = class of TSharedRasterImage;

  { TRasterImage }

  TRasterImage = class(TGraphic)
  private
    FCanvas: TCanvas;
    FTransparentColor: TColor;
    FTransparentMode: TTransparentMode;
    FUpdateCount: Integer;
    FUpdateCanvasOnly: Boolean;
    FMasked: Boolean;

    procedure CanvasChanging(Sender: TObject);
    procedure CreateCanvas;
    procedure CreateMask(AColor: TColor = clDefault);
    procedure FreeCanvasContext;
    function  GetCanvas: TCanvas;
    function  GetRawImage: TRawImage;
    function  GetScanline(ARow: Integer): Pointer;
    function  GetTransparentColor: TColor;
    procedure SetTransparentColor(AValue: TColor);
  protected
    FSharedImage: TSharedRasterImage;
    function  CanShareImage(AClass: TSharedRasterImageClass): Boolean; virtual;
    procedure Changed(Sender: TObject); override;
    function  CreateDefaultBitmapHandle(const ADesc: TRawImageDescription): HBITMAP; virtual;
    procedure Draw(DestCanvas: TCanvas; const DestRect: TRect); override;
    function GetEmpty: Boolean; override;
    function GetHandle: THandle;
    function GetBitmapHandle: HBITMAP; virtual; abstract;
    function GetMasked: Boolean; virtual;
    function GetMaskHandle: HBITMAP; virtual; abstract;
    function GetMimeType: string; override;
    function GetPixelFormat: TPixelFormat; virtual; abstract;
    function GetRawImagePtr: PRawImage; virtual; abstract;
    function GetRawImageDescriptionPtr: PRawImageDescription; virtual; abstract;
    function GetTransparent: Boolean; override;
    class function GetSharedImageClass: TSharedRasterImageClass; virtual;
    function GetHeight: Integer; override;
    function GetWidth: Integer; override;
    procedure BitmapHandleNeeded; virtual;
    procedure HandleNeeded; virtual; abstract;
    procedure MaskHandleNeeded; virtual; abstract;
    procedure PaletteNeeded; virtual; abstract;
    function  InternalReleaseBitmapHandle: HBITMAP; virtual; abstract;
    function  InternalReleaseMaskHandle: HBITMAP; virtual; abstract;
    function  InternalReleasePalette: HPALETTE; virtual; abstract;
    procedure SetBitmapHandle(AValue: HBITMAP);
    procedure SetMasked(AValue: Boolean); virtual;
    procedure SetMaskHandle(AValue: HBITMAP);
    procedure SetTransparent(AValue: Boolean); override;
    procedure UnshareImage(CopyContent: boolean); virtual; abstract;
    function  UpdateHandles(ABitmap, AMask: HBITMAP): Boolean; virtual; abstract; // called when handles are created from rawimage (true when handle changed)
    procedure SaveStreamNeeded;
    procedure FreeSaveStream;
    procedure ReadData(Stream: TStream); override;
    procedure ReadStream(AStream: TMemoryStream; ASize: Longint); virtual; abstract; // loads imagedata into rawimage, this method shouldn't call changed().
    procedure SetSize(AWidth, AHeight: integer); virtual; abstract;
    procedure SetHandle(AValue: THandle); virtual;
    procedure SetHeight(AHeight: Integer); override;
    procedure SetWidth(AWidth: Integer); override;
    procedure SetTransparentMode(AValue: TTransparentMode);
    procedure SetPixelFormat(AValue: TPixelFormat); virtual; abstract;
    procedure WriteData(Stream: TStream); override;
    procedure WriteStream(AStream: TMemoryStream); virtual; abstract;
    function  RequestTransparentColor: TColor;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;
    procedure BeginUpdate(ACanvasOnly: Boolean = False);
    procedure EndUpdate(AStreamIsValid: Boolean = False);
    procedure FreeImage; virtual;
    function BitmapHandleAllocated: boolean; virtual; abstract;
    function MaskHandleAllocated: boolean; virtual; abstract;
    function PaletteAllocated: boolean; virtual; abstract;
    procedure LoadFromBitmapHandles(ABitmap, AMask: HBitmap; ARect: PRect = nil);
    procedure LoadFromDevice(DC: HDC); virtual;
    procedure LoadFromStream(AStream: TStream); overload; override;
    procedure LoadFromStream(AStream: TStream; ASize: Cardinal); overload; virtual;
    procedure LoadFromMimeStream(AStream: TStream; const AMimeType: string); override;
    procedure LoadFromRawImage(const AIMage: TRawImage; ADataOwner: Boolean);
    procedure LoadFromIntfImage(IntfImage: TLazIntfImage);
    procedure SaveToStream(AStream: TStream); override;
    procedure GetSupportedSourceMimeTypes(List: TStrings); override;
    procedure GetSize(out AWidth, AHeight: Integer);
    procedure Mask(ATransparentColor: TColor);
    procedure SetHandles(ABitmap, AMask: HBITMAP); virtual; abstract; // called when handles are set by user
    function ReleaseBitmapHandle: HBITMAP;
    function ReleaseMaskHandle: HBITMAP;
    function ReleasePalette: HPALETTE;
    function CreateIntfImage: TLazIntfImage;
  public
    property Canvas: TCanvas read GetCanvas;
    function HandleAllocated: boolean;
    property BitmapHandle: HBITMAP read GetBitmapHandle write SetBitmapHandle;
    property Masked: Boolean read GetMasked write SetMasked;
    property MaskHandle: HBITMAP read GetMaskHandle write SetMaskHandle;
    property PixelFormat: TPixelFormat read GetPixelFormat write SetPixelFormat default pfDevice;
    property RawImage: TRawImage read GetRawImage; // be carefull with this, modify only within a begin/endupdate
    property ScanLine[Row: Integer]: Pointer read GetScanLine; platform; // Use only when wrpped by a begin/endupdate
    property TransparentColor: TColor read GetTransparentColor
                                      write SetTransparentColor default clDefault;
    property TransparentMode: TTransparentMode read FTransparentMode
                                        write SetTransparentMode default tmAuto;
  end;
  
  TSharedCustomBitmap = class(TSharedRasterImage)
  private
    FHandleType: TBitmapHandleType;
    FImage: TRawImage;
    FHasMask: Boolean; // set if atleast one maskpixel is set
    FPalette: HPALETTE;
    function GetHeight: Integer;
    function GetWidth: Integer;
  protected
    procedure FreeHandle; override;
    procedure FreePalette;
    procedure FreeImage;
    function ReleasePalette: HPALETTE;
    function GetPixelFormat: TPixelFormat;
    function IsEmpty: boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function HandleAllocated: boolean; override;
    function ImageAllocated: boolean;
    property HandleType: TBitmapHandleType read FHandleType write FHandleType;
    property Height: Integer read GetHeight;
    property PixelFormat: TPixelFormat read GetPixelFormat;
    property Width: Integer read GetWidth;
  end;

  { TCustomBitmap
    is the data of an image. The image can be loaded from a file,
    stream or resource in .bmp (windows bitmap format) or .xpm (XPixMap format)
    The loading routine automatically recognizes the format, so it is also used
    to load the imagess from Delphi form streams (e.g. .dfm files).
    When the handle is created, it is up to the interface (gtk, win32, ...)
    to convert it automatically to the best internal format. That is why the
    Handle is interface dependent.
    To access the raw data, see TLazIntfImage in IntfGraphics.pas }

  TCustomBitmap = class(TRasterImage)
  private
    FPixelFormat: TPixelFormat;
    FPixelFormatNeedsUpdate: Boolean;
    FMaskHandle: HBITMAP; // mask is not part of the image, so not shared
    function GetHandleType: TBitmapHandleType;
    function GetMonochrome: Boolean;
    procedure SetBitmapHandle(const AValue: HBITMAP);
    procedure SetHandleType(AValue: TBitmapHandleType);
    procedure SetMonochrome(AValue: Boolean);
    procedure UpdatePixelFormat;
  protected
    procedure MaskHandleNeeded; override;
    procedure PaletteNeeded; override;
    function  CanShareImage(AClass: TSharedRasterImageClass): Boolean; override;
    procedure Changed(Sender: TObject); override;
    function CreateDefaultBitmapHandle(const ADesc: TRawImageDescription): HBITMAP; override;
    procedure FreeMaskHandle;
    function GetBitmapHandle: HBITMAP; override;
    function GetMaskHandle: HBITMAP; override;
    function GetPalette: HPALETTE; override;
    function GetPixelFormat: TPixelFormat; override;
    function GetRawImagePtr: PRawImage; override;
    function GetRawImageDescriptionPtr: PRawImageDescription; override;
    procedure HandleNeeded; override;
    function InternalReleaseBitmapHandle: HBITMAP; override;
    function InternalReleaseMaskHandle: HBITMAP; override;
    function InternalReleasePalette: HPALETTE; override;
    procedure RawimageNeeded(ADescOnly: Boolean);
    procedure SetHandle(AValue: THandle); override;
    procedure SetPixelFormat(AValue: TPixelFormat); override;
    procedure UnshareImage(CopyContent: boolean); override;
    function  UpdateHandles(ABitmap, AMask: HBITMAP): Boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;
    procedure FreeImage; override;
    function LazarusResourceTypeValid(const ResourceType: string): Boolean; override;
    function BitmapHandleAllocated: boolean; override;
    function MaskHandleAllocated: boolean; override;
    function PaletteAllocated: boolean; override;
    function ReleaseHandle: HBITMAP;

    procedure SetHandles(ABitmap, AMask: HBITMAP); override;
    procedure SetSize(AWidth, AHeight: integer); override;

    property Handle: HBITMAP read GetBitmapHandle write SetBitmapHandle; // for custombitmap handle = bitmaphandle
    property HandleType: TBitmapHandleType read GetHandleType write SetHandleType;
    property Monochrome: Boolean read GetMonochrome write SetMonochrome;
  end;
  
  { TFPImageBitmap }
  { Use this class to easily create a TCustomBitmap descendent for FPImage
    reader and writer }

  TFPImageBitmap = class(TCustomBitmap)
  private
  protected
    function GetMimeType: string; override;
    class function GetReaderClass: TFPCustomImageReaderClass; virtual; abstract;
    class function GetWriterClass: TFPCustomImageWriterClass; virtual; abstract;
    procedure InitializeReader(AImage: TLazIntfImage; AReader: TFPCustomImageReader); virtual;
    procedure InitializeWriter(AImage: TLazIntfImage; AWriter: TFPCustomImageWriter); virtual;
    procedure FinalizeReader(AReader: TFPCustomImageReader); virtual;
    procedure FinalizeWriter(AWriter: TFPCustomImageWriter); virtual;
    procedure ReadStream(AStream: TMemoryStream; ASize: Longint); override;
    procedure WriteStream(AStream: TMemoryStream); override;
  public
    class function GetFileExtensions: string; override;
    class function IsStreamFormatSupported(Stream: TStream): Boolean; override;
    class function IsFileExtensionSupported(const FileExtension: string): boolean;
    function LazarusResourceTypeValid(const ResourceType: string): boolean; override;
  end;
  
  TFPImageBitmapClass = class of TFPImageBitmap;


  { TSharedBitmap }
  
  TSharedBitmap = class(TSharedCustomBitmap)
  end;

  { TBitmap }

  TBitmap = class(TFPImageBitmap)
  protected
    procedure InitializeReader(AImage: TLazIntfImage; AReader: TFPCustomImageReader); override;
    class function GetReaderClass: TFPCustomImageReaderClass; override;
    class function GetWriterClass: TFPCustomImageWriterClass; override;
    class function GetSharedImageClass: TSharedRasterImageClass; override;
  public
    class function GetFileExtensions: string; override;
    function GetResourceType: TResourceType; override;
    procedure LoadFromStream(AStream: TStream; ASize: Cardinal); override;
  end;


  { TSharedPixmap }

  TSharedPixmap = class(TSharedCustomBitmap)
  end;

  { TPixmap }

  TPixmap = class(TFPImageBitmap)
  protected
    class function GetReaderClass: TFPCustomImageReaderClass; override;
    class function GetWriterClass: TFPCustomImageWriterClass; override;
    class function GetSharedImageClass: TSharedRasterImageClass; override;
  public
    function LazarusResourceTypeValid(const ResourceType: string): boolean; override;
    class function GetFileExtensions: string; override;
  end;

  { TSharedPortableNetworkGraphic }

  TSharedPortableNetworkGraphic = class(TSharedCustomBitmap)
  end;

  { TPortableNetworkGraphic }

  TPortableNetworkGraphic = class(TFPImageBitmap)
  protected
    class function GetReaderClass: TFPCustomImageReaderClass; override;
    class function GetWriterClass: TFPCustomImageWriterClass; override;
    procedure InitializeWriter(AImage: TLazIntfImage; AWriter: TFPCustomImageWriter); override;
    class function GetSharedImageClass: TSharedRasterImageClass; override;
  public
    class function IsStreamFormatSupported(Stream: TStream): Boolean; override;
    class function GetFileExtensions: string; override;
  end;


  { TSharedPortableAnyMapGraphic }

  TSharedPortableAnyMapGraphic = class(TSharedCustomBitmap)
  end;
  
  { TPortableAnyMapGraphic }

  TPortableAnyMapGraphic = class(TFPImageBitmap)
  protected
    class function GetReaderClass: TFPCustomImageReaderClass; override;
    class function GetWriterClass: TFPCustomImageWriterClass; override;
    class function GetSharedImageClass: TSharedRasterImageClass; override;
  public
    class function IsStreamFormatSupported(Stream: TStream): Boolean; override;
    class function GetFileExtensions: string; override;
  end;

  TIconImage = class;
  TIconImageClass = class of TIconImage;

  { TSharedIcon }

  TSharedIcon = class(TSharedRasterImage)
  private
    FImages: TFPList;
  protected
    procedure FreeHandle; override;
    procedure UpdateFromHandle(NewHandle: THandle); virtual;
    function IsEmpty: boolean; override;
    function GetImage(const AIndex: Integer): TIconImage;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Clear;
    procedure Delete(AIndex: Integer);
    function GetIndex(AFormat: TPixelFormat; AHeight, AWidth: Word): Integer;
    class function GetImagesClass: TIconImageClass; virtual;
    procedure Add(AIconImage: TIconImage);
    function Count: Integer;
    property Images[AIndex: Integer]: TIconImage read GetImage;
  end;

  { TIconImage }

  TIconImage = class
  private
    FHeight: Word;
    FPixelFormat: TPixelFormat;
    FWidth: Word;
    FImage: TRawImage;
    FHandle: HBITMAP;
    FMaskHandle: HBITMAP;
    FPalette: HPALETTE;
    function GetPalette: HPALETTE;
  protected
    procedure RawImageNeeded(ADescOnly: Boolean);
    procedure UpdateFromImage(const AImage: TRawImage);
  public
    constructor Create(AFormat: TPixelFormat; AHeight, AWidth: Word);
    constructor Create(const AImage: TRawImage);
    constructor Create(const AInfo: TIconInfo); virtual;
    destructor Destroy; override;

    function ReleaseHandle: HBITMAP;
    function ReleaseMaskHandle: HBITMAP;
    function ReleasePalette: HPALETTE;
    function UpdateHandles(ABitmap, AMask: HBITMAP): Boolean;

    property Height: Word read FHeight;
    property Width: Word read FWidth;
    property PixelFormat: TPixelFormat read FPixelFormat;
    property Handle: HBITMAP read FHandle;
    property MaskHandle: HBITMAP read FMaskHandle;
    property Palette: HPALETTE read GetPalette;
    property RawImage: TRawImage read FImage;
  end;


  { TIcon }
  {
    TIcon reads and writes .ICO file format.
    A .ico file typically contains several versions of the same image. When loading,
    the largest/most colourful image is loaded as the TCustomBitmap and so can be handled
    as any other bitmap. Any other versions of the images are available via the
    Bitmaps property
    Writing is not (yet) implemented.
  }
  

  { TCustomIcon }

  TCustomIcon = class(TRasterImage)
  private
    function GetCount: Integer;
    procedure SetCurrent(const AValue: Integer);
  protected
    FCurrent: Integer;
    FRequestedSize: TSize;
    procedure MaskHandleNeeded; override;
    procedure PaletteNeeded; override;
    function  CanShareImage(AClass: TSharedRasterImageClass): Boolean; override;
    procedure CheckRequestedSize;
    function GetIndex(AFormat: TPixelFormat; AHeight, AWidth: Word): Integer;
    function GetBitmapHandle: HBITMAP; override;
    class function GetDefaultSize: TSize; virtual;
    function GetMaskHandle: HBITMAP; override;
    function GetPalette: HPALETTE; override;
    function GetPixelFormat: TPixelFormat; override;
    function GetRawImagePtr: PRawImage; override;
    function GetRawImageDescriptionPtr: PRawImageDescription; override;
    function GetTransparent: Boolean; override;
    class function GetSharedImageClass: TSharedRasterImageClass; override;
    class function GetStreamSignature: Cardinal; virtual;
    class function GetTypeID: Word; virtual;
    procedure HandleNeeded; override;
    function InternalReleaseBitmapHandle: HBITMAP; override;
    function InternalReleaseMaskHandle: HBITMAP; override;
    function InternalReleasePalette: HPALETTE; override;
    procedure ReadData(Stream: TStream); override;
    procedure ReadStream(AStream: TMemoryStream; ASize: Longint); override;
    procedure SetMasked(AValue: Boolean); override;
    procedure SetPixelFormat(AValue: TPixelFormat); override;
    procedure SetTransparent(Value: Boolean); override;
    procedure UnshareImage(CopyContent: boolean); override;
    procedure UpdateCurrentView;
    procedure SetHandle(AValue: THandle); override;
    function UpdateHandle(AValue: HICON): Boolean; virtual;
    function  UpdateHandles(ABitmap, AMask: HBITMAP): Boolean; override;
    procedure WriteStream(AStream: TMemoryStream); override;
  public
    constructor Create; override;

    procedure Add(AFormat: TPixelFormat; AHeight, AWidth: Word);
    procedure Assign(Source: TPersistent); override;
    procedure AssignImage(ASource: TRasterImage); virtual;
    procedure Clear; override;
    procedure Delete(Aindex: Integer);
    procedure Remove(AFormat: TPixelFormat; AHeight, AWidth: Word);
    procedure GetDescription(Aindex: Integer; out AFormat: TPixelFormat; out AHeight, AWidth: Word);
    procedure SetSize(AWidth, AHeight: integer); override;
    class function GetFileExtensions: string; override;
    function LazarusResourceTypeValid(const ResourceType: string): boolean; override;
    procedure LoadFromResourceName(Instance: THandle; const ResName: String); override;
    procedure LoadFromResourceID(Instance: THandle; ResID: PtrInt); override;
    procedure LoadFromResourceHandle(Instance: THandle; ResHandle: TFPResourceHandle); virtual;
    function BitmapHandleAllocated: boolean; override;
    function MaskHandleAllocated: boolean; override;
    function PaletteAllocated: boolean; override;
    procedure SetHandles(ABitmap, AMask: HBITMAP); override;
    function GetBestIndexForSize(ASize: TSize): Integer;

    property Current: Integer read FCurrent write SetCurrent;
    property Count: Integer read GetCount;
  end;

  { TIcon }
  
  TIcon = class(TCustomIcon)
  private
    function GetIconHandle: HICON;
    procedure SetIconHandle(const AValue: HICON);
  protected
    class function GetStreamSignature: Cardinal; override;
    class function GetTypeID: Word; override;
    procedure HandleNeeded; override;
  public
    procedure LoadFromResourceHandle(Instance: THandle; ResHandle: TFPResourceHandle); override;
    function ReleaseHandle: HICON;
    function GetResourceType: TResourceType; override;
    property Handle: HICON read GetIconHandle write SetIconHandle;
  end;
  
  TIcnsRec = record
    IconType: TicnsIconType;
    RawImage: TRawImage;
  end;
  PIcnsRec = ^TIcnsRec;

  { TIcnsList }

  TIcnsList = class(TList)
  private
    function GetItem(Index: Integer): PIcnsRec;
    procedure SetItem(Index: Integer; const AValue: PIcnsRec);
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  public
    function Add(AIconType: TicnsIconType; ARawImage: TRawImage): Integer; reintroduce;
    property Items[Index: Integer]: PIcnsRec read GetItem write SetItem; default;
  end;

  TSharedIcnsIcon = class(TSharedIcon)
  end;

  { TIcnsIcon }

  TIcnsIcon = class(TCustomIcon)
  private
    FImageList: TIcnsList;
    FMaskList: TIcnsList;
    procedure IcnsAdd(AIconType: TicnsIconType; ARawImage: TRawImage);
    procedure IcnsProcess;
  protected
    class function GetSharedImageClass: TSharedRasterImageClass; override;
    procedure ReadData(Stream: TStream); override;
    procedure ReadStream(AStream: TMemoryStream; ASize: Longint); override;
    procedure WriteStream(AStream: TMemoryStream); override;
  public
    constructor Create; override;
    destructor Destroy; override;

    class function GetFileExtensions: string; override;
    function LazarusResourceTypeValid(const ResourceType: string): boolean; override;
  end;

  { TSharedCursorImage }

  TSharedCursorImage = class(TSharedIcon)
  protected
    procedure FreeHandle; override;
  public
    class function GetImagesClass: TIconImageClass; override;
  end;
  
  { TCursorImageImage }

  TCursorImageImage = class(TIconImage)
  private
    FHotSpot: TPoint;
  public
    constructor Create(const AInfo: TIconInfo); override;
    property HotSpot: TPoint read FHotSpot write FHotSpot;
  end;

  { TCursorImage }
  TCursorImage = class(TCustomIcon)
  private
    function GetHotSpot: TPoint;
    procedure SetHotSpot(const P: TPoint);
    function GetCursorHandle: HCURSOR;
    procedure SetCursorHandle(AValue: HCURSOR);
  protected
    procedure HandleNeeded; override;
    class function GetDefaultSize: TSize; override;
    class function GetStreamSignature: Cardinal; override;
    class function GetSharedImageClass: TSharedRasterImageClass; override;
    class function GetTypeID: Word; override;
  public
    class function GetFileExtensions: string; override;
    function GetResourceType: TResourceType; override;
    procedure LoadFromResourceHandle(Instance: THandle; ResHandle: TFPResourceHandle); override;
    function LazarusResourceTypeValid(const ResourceType: string): boolean; override;
    function ReleaseHandle: HCURSOR;
    procedure SetCenterHotSpot;
    property HotSpot: TPoint read GetHotSpot write SetHotSpot;
    property Handle: HCURSOR read GetCursorHandle write SetCursorHandle;
  end;
  

  { TSharedJpegImage }

  TSharedJpegImage = class(TSharedCustomBitmap)
  end;
  
  { TJpegImage }

  TJPEGQualityRange = TFPJPEGCompressionQuality;
  TJPEGPerformance = TJPEGReadPerformance;

  TJPEGImage = class(TFPImageBitmap)
  private
    FGrayScale: Boolean;
    FPerformance: TJPEGPerformance;
    FProgressiveEncoding: boolean;
    FQuality: TJPEGQualityRange;
  protected
    procedure InitializeReader(AImage: TLazIntfImage; AReader: TFPCustomImageReader); override;
    procedure InitializeWriter(AImage: TLazIntfImage; AWriter: TFPCustomImageWriter); override;
    procedure FinalizeReader(AReader: TFPCustomImageReader); override;
    class function GetReaderClass: TFPCustomImageReaderClass; override;
    class function GetWriterClass: TFPCustomImageWriterClass; override;
    class function GetSharedImageClass: TSharedRasterImageClass; override;
  public
    constructor Create; override;
    class function IsStreamFormatSupported(Stream: TStream): Boolean; override;
    class function GetFileExtensions: string; override;
  public
    property CompressionQuality: TJPEGQualityRange read FQuality write FQuality;
    property GrayScale: Boolean read FGrayScale;
    property ProgressiveEncoding: boolean read FProgressiveEncoding;
    property Performance: TJPEGPerformance read FPerformance write FPerformance;
  end;

  { TSharedTiffImage }

  TSharedTiffImage = class(TSharedCustomBitmap)
  end;

  { TTiffImage }

  TTiffUnit = (
    tuUnknown,
    tuNone,       // No absolute unit of measurement. Used for images that may have a non-square
                  // aspect ratio, but no meaningful absolute dimensions.
    tuInch,
    tuCentimeter
  );

  TTiffImage = class(TFPImageBitmap)
  private
    FArtist: string;
    FCopyright: string;
    FDateTime: TDateTime;
    FDocumentName: string;
    FHostComputer: string;
    FImageDescription: string;
    FMake: string; {ScannerManufacturer}
    FModel: string; {Scanner}
    FResolutionUnit: TTiffUnit;
    FSoftware: string;
    FXResolution: TTiffRational;
    FYResolution: TTiffRational;
  protected
    procedure InitializeReader(AImage: TLazIntfImage; AReader: TFPCustomImageReader); override;
    procedure InitializeWriter(AImage: TLazIntfImage; AWriter: TFPCustomImageWriter); override;
    procedure FinalizeReader(AReader: TFPCustomImageReader); override;
    class function GetReaderClass: TFPCustomImageReaderClass; override;
    class function GetWriterClass: TFPCustomImageWriterClass; override;
    class function GetSharedImageClass: TSharedRasterImageClass; override;
  public
    constructor Create; override;
    class function GetFileExtensions: string; override;
  public
    property Artist: string read FArtist write FArtist;
    property Copyright: string read FCopyright write FCopyright;
    property DateTime: TDateTime read FDateTime write FDateTime;
    property DocumentName: string read FDocumentName write FDocumentName;
    property HostComputer: string read FHostComputer write FHostComputer;
    property ImageDescription: string read FImageDescription write FImageDescription;
//    property ImageIsMask: Boolean;
//    property ImageIsPage: Boolean;
//    property ImageIsThumbNail: Boolean;
    property Make: string read FMake write FMake;
    property Model: string read FModel write FModel;
    property ResolutionUnit: TTiffUnit read FResolutionUnit write FResolutionUnit;
    property Software: string read FSoftware write FSoftware;
    property XResolution: TTiffRational read FXResolution write FXResolution;
    property YResolution: TTiffRational read FYResolution write FYResolution;
  end;

  { TSharedGIFImage }

  TSharedGIFImage = class(TSharedCustomBitmap)
  end;

  { TGIFImage }

  TGIFImage = class(TFPImageBitmap)
  private
    FTransparent: Boolean;
    FInterlaced: Boolean;
    FBitsPerPixel: byte;
  protected
    procedure InitializeReader(AImage: TLazIntfImage; AReader: TFPCustomImageReader); override;
    procedure FinalizeReader(AReader: TFPCustomImageReader); override;
    class function GetReaderClass: TFPCustomImageReaderClass; override;
    class function GetSharedImageClass: TSharedRasterImageClass; override;
  public
    constructor Create; override;
    class function IsStreamFormatSupported(Stream: TStream): Boolean; override;
    class function GetFileExtensions: string; override;
  public
    property Transparent: Boolean read FTransparent;
    property Interlaced: Boolean read FInterlaced;
    property BitsPerPixel: byte read FBitsPerPixel;
  end;

function GraphicFilter(GraphicClass: TGraphicClass): string;
function GraphicExtension(GraphicClass: TGraphicClass): string;
function GraphicFileMask(GraphicClass: TGraphicClass): string;
function GetGraphicClassForFileExtension(const FileExt: string): TGraphicClass;

type
  // Color / Identifier mapping
  TGetColorStringProc = procedure(const s: AnsiString) of object;

function IdentEntry(Entry: Longint; out MapEntry: TIdentMapEntry): boolean;
function ColorToIdent(Color: Longint; out Ident: String): Boolean;
function IdentToColor(const Ident: string; out Color: Longint): Boolean;
function ColorIndex(Color: Longint; out Index: Integer): Boolean;
function SysColorToSysColorIndex(Color: TColor): integer;
function ColorToRGB(Color: TColor): Longint;
function ColorToString(Color: TColor): AnsiString;
function StringToColor(const S: shortstring): TColor;
function StringToColorDef(const S: shortstring; const DefaultValue: TColor): TColor;
procedure GetColorValues(Proc: TGetColorStringProc);
function InvertColor(AColor: TColor): TColor;
function DecColor(AColor: TColor; AQuantity: Byte): TColor;
function IsSysColor(AColor: TColorRef): Boolean;

function Blue(rgb: TColorRef): BYTE; // does not work on system color
function Green(rgb: TColorRef): BYTE; // does not work on system color
function Red(rgb: TColorRef): BYTE; // does not work on system color
function RGBToColor(R, G, B: Byte): TColor;
procedure RedGreenBlue(rgb: TColorRef; out Red, Green, Blue: Byte); // does not work on system color
function FPColorToTColorRef(const FPColor: TFPColor): TColorRef;
function FPColorToTColor(const FPColor: TFPColor): TColor;
function TColorToFPColor(const c: TColorRef): TFPColor; overload;
function TColorToFPColor(const c: TColor): TFPColor; overload; // does not work on system color

// fonts
procedure GetCharsetValues(Proc: TGetStrProc);
function CharsetToIdent(Charset: Longint; out Ident: string): Boolean;
function IdentToCharset(const Ident: string; out Charset: Longint): Boolean;
function GetFontData(Font: HFont): TFontData;

function GetDefFontCharSet: TFontCharSet;
function IsFontNameXLogicalFontDesc(const LongFontName: string): boolean;
function XLFDNameToLogFont(const XLFDName: string): TLogFont;
function ExtractXLFDItem(const XLFDName: string; Index: integer): string;
function ExtractFamilyFromXLFDName(const XLFDName: string): string;
function ClearXLFDItem(const LongFontName: string; Index: integer): string;
function ClearXLFDHeight(const LongFontName: string): string;
function ClearXLFDPitch(const LongFontName: string): string;
function ClearXLFDStyle(const LongFontName: string): string;
function XLFDHeightIsSet(const LongFontName: string): boolean;
procedure FontNameToPangoFontDescStr(const LongFontName: string;
  out aFamily,aStyle:String; out aSize: Integer; out aSizeInPixels: Boolean);

// graphics
type
  TOnLoadGraphicFromClipboardFormat =
    procedure(Dest: TGraphic; ClipboardType: TClipboardType;
              FormatID: TClipboardFormat);
  TOnSaveGraphicToClipboardFormat =
    procedure(Src: TGraphic; ClipboardType: TClipboardType;
              FormatID: TClipboardFormat);
  TOnGetSystemFont = function: HFONT;

var
  OnLoadSaveClipBrdGraphicValid: boolean = false;
  OnLoadGraphicFromClipboardFormat: TOnLoadGraphicFromClipboardFormat=nil;
  OnSaveGraphicToClipboardFormat: TOnSaveGraphicToClipboardFormat=nil;
  OnGetSystemFont: TOnGetSystemFont = nil;

function TestStreamIsBMP(const AStream: TStream): boolean;
function TestStreamIsXPM(const AStream: TStream): boolean;
function TestStreamIsIcon(const AStream: TStream): boolean;
function TestStreamIsCursor(const AStream: TStream): boolean;

function XPMToPPChar(const XPM: string): PPChar;
function LazResourceXPMToPPChar(const ResourceName: string): PPChar;
function ReadXPMFromStream(Stream: TStream; Size: integer): PPChar;
function ReadXPMSize(XPM: PPChar; var Width, Height, ColorCount: integer): boolean;
function LoadCursorFromLazarusResource(ACursorName: String): HCursor;
function LoadBitmapFromLazarusResource(const ResourceName: String): TBitmap; deprecated;
function LoadBitmapFromLazarusResourceHandle(Handle: TLResource): TBitmap; deprecated;

// technically a bitmap is created and not loaded
function CreateGraphicFromResourceName(Instance: THandle; const ResName: String): TGraphic;
function CreateBitmapFromResourceName(Instance: THandle; const ResName: String): TCustomBitmap;
function CreateBitmapFromLazarusResource(const AName: String): TCustomBitmap;
function CreateBitmapFromLazarusResource(const AName: String; AMinimumClass: TCustomBitmapClass): TCustomBitmap;
function CreateBitmapFromLazarusResource(AHandle: TLResource): TCustomBitmap;
function CreateBitmapFromLazarusResource(AHandle: TLResource; AMinimumClass: TCustomBitmapClass): TCustomBitmap;

function CreateCompatibleBitmaps(const ARawImage: TRawImage; out ABitmap, AMask: HBitmap; ASkipMask: Boolean = False): Boolean;

function CreateBitmapFromFPImage(Img: TFPCustomImage): TBitmap;

var
  { Stores information about the current screen
    - initialized on Interface startup }
  ScreenInfo: TScreenInfo = (
    PixelsPerInchX: 72;
    PixelsPerInchY: 72;
    ColorDepth: 24;
    Initialized: False;
  );

  FontResourceCache: TFontHandleCache;
  PenResourceCache: TPenHandleCache;
  BrushResourceCache: TBrushHandleCache;

const
  FontCharsets: array[0..18] of TIdentMapEntry = (
    (Value: ANSI_CHARSET;        Name: 'ANSI_CHARSET'),
    (Value: DEFAULT_CHARSET;     Name: 'DEFAULT_CHARSET'),
    (Value: SYMBOL_CHARSET;      Name: 'SYMBOL_CHARSET'),
    (Value: MAC_CHARSET;         Name: 'MAC_CHARSET'),
    (Value: SHIFTJIS_CHARSET;    Name: 'SHIFTJIS_CHARSET'),
    (Value: HANGEUL_CHARSET;     Name: 'HANGEUL_CHARSET'),
    (Value: JOHAB_CHARSET;       Name: 'JOHAB_CHARSET'),
    (Value: GB2312_CHARSET;      Name: 'GB2312_CHARSET'),
    (Value: CHINESEBIG5_CHARSET; Name: 'CHINESEBIG5_CHARSET'),
    (Value: GREEK_CHARSET;       Name: 'GREEK_CHARSET'),
    (Value: TURKISH_CHARSET;     Name: 'TURKISH_CHARSET'),
    (Value: VIETNAMESE_CHARSET;  Name: 'VIETNAMESE_CHARSET'),
    (Value: HEBREW_CHARSET;      Name: 'HEBREW_CHARSET'),
    (Value: ARABIC_CHARSET;      Name: 'ARABIC_CHARSET'),
    (Value: BALTIC_CHARSET;      Name: 'BALTIC_CHARSET'),
    (Value: RUSSIAN_CHARSET;     Name: 'RUSSIAN_CHARSET'),
    (Value: THAI_CHARSET;        Name: 'THAI_CHARSET'),
    (Value: EASTEUROPE_CHARSET;  Name: 'EASTEUROPE_CHARSET'),
    (Value: OEM_CHARSET;         Name: 'OEM_CHARSET'));


(***************************************************************************
 ***************************************************************************)

function DbgS(const Style: TFontStyles): string; overload;

function ScaleX(const SizeX, FromDPI: Integer): Integer;
function ScaleY(const SizeY, FromDPI: Integer): Integer;

procedure Register;
procedure UpdateHandleObjects;

implementation

uses
  SyncObjs, LCLIntf, InterfaceBase;

var
  GraphicsUpdateCount: Integer = 0;
  UpdateLock: TCriticalSection;

procedure UpdateHandleObjects;
begin
  // renew all brushes, pens, fonts, ...
  UpdateLock.Enter;
  try
    if GraphicsUpdateCount=High(GraphicsUpdateCount) then
      GraphicsUpdateCount:=Low(GraphicsUpdateCount);
    inc(GraphicsUpdateCount);
    // at moment update only brushes, but later maybe we will need to update others
    // don't clear BrushResourceCache because TBrush instances have references to cache items
    // BrushResourceCache.Clear;
  finally
    UpdateLock.Leave;
  end;
end;

function DbgS(const Style: TFontStyles): string;

  procedure Add(const s: string);
  begin
    if Result<>'' then Result:=Result+',';
    Result:=Result+s;
  end;

begin
  Result:='';
  if fsBold in Style then Add('fsBold');
  if fsItalic in Style then Add('fsItalic');
  if fsStrikeOut in Style then Add('fsStrikeOut');
  if fsUnderline in Style then Add('fsUnderline');
  Result:='['+Result+']';
end;

function LoadCursorFromLazarusResource(ACursorName: String): HCursor;
var
  CursorImage: TCursorImage;
begin
  CursorImage := TCursorImage.Create;
  try
    CursorImage.LoadFromLazarusResource(ACursorName);
    Result := CursorImage.ReleaseHandle;
  finally
    CursorImage.Free;
  end;
end;

function LocalLoadBitmap(hInstance: THandle; lpBitmapName: PChar): HBitmap;
var
  Bmp: TBitmap;
begin
  Bmp := TBitmap.Create;
  try
    if PtrUInt(lpBitmapName) > High(Word)
    then Bmp.LoadFromResourceName(hInstance, lpBitmapName)
    else Bmp.LoadFromResourceID(hInstance, PtrInt(lpBitmapName));
    Result := Bmp.ReleaseHandle;
  finally
    Bmp.Free;
  end;
end;

function LocalLoadCursor(hInstance: THandle; lpCursorName: PChar): HCursor;
var
  Cur: TCursorImage;
begin
  Cur := TCursorImage.Create;
  try
    if PtrUInt(lpCursorName) > High(Word)
    then Cur.LoadFromResourceName(hInstance, lpCursorName)
    else Cur.LoadFromResourceID(hInstance, PtrInt(lpCursorName));
    Result := Cur.ReleaseHandle;
  finally
    Cur.Free;
  end;
end;

function LocalLoadIcon(hInstance: THandle; lpIconName: PChar): HIcon;
var
  Ico: TIcon;
begin
  Ico := TIcon.Create;
  try
    if PtrUInt(lpIconName) > High(Word)
    then Ico.LoadFromResourceName(hInstance, lpIconName)
    else Ico.LoadFromResourceID(hInstance, PtrInt(lpIconName));
    Result := Ico.ReleaseHandle;
  finally
    Ico.Free;
  end;
end;

function CreateBitmapFromLazarusResource(AStream: TLazarusResourceStream; AMinimumClass: TCustomBitmapClass): TCustomBitmap;
var
  GraphicClass: TGraphicClass;
begin
  Result := nil;
  if AStream = nil then Exit;

  GraphicClass := GetGraphicClassForFileExtension(AStream.Res.ValueType);
  if GraphicClass = nil then Exit;
  if not GraphicClass.InheritsFrom(AMinimumClass) then Exit;
  
  Result := TCustomBitmap(GraphicClass.Create);
  try
    Result.LoadFromStream(AStream);
  except
    Result.Free;
    Result := nil;
    raise;
  end;
end;

function CreateBitmapFromLazarusResource(const AName: String): TCustomBitmap;
begin
  Result := CreateBitmapFromLazarusResource(AName, TCustomBitmap);
end;

function CreateBitmapFromLazarusResource(const AName: String; AMinimumClass: TCustomBitmapClass): TCustomBitmap;
var
  Stream: TLazarusResourceStream;
begin
  Stream := TLazarusResourceStream.Create(AName, nil);
  try
    Result := CreateBitmapFromLazarusResource(Stream, AMinimumClass);
  finally
    Stream.Free;
  end;
end;

function CreateBitmapFromLazarusResource(AHandle: TLResource): TCustomBitmap;
begin
  Result := CreateBitmapFromLazarusResource(AHandle, TCustomBitmap);
end;

function CreateBitmapFromLazarusResource(AHandle: TLResource; AMinimumClass: TCustomBitmapClass): TCustomBitmap;
var
  Stream: TLazarusResourceStream;
begin
  Stream := TLazarusResourceStream.CreateFromHandle(AHandle);
  try
    Result := CreateBitmapFromLazarusResource(Stream, AMinimumClass);
  finally
    Stream.Free;
  end;
end;

function LoadBitmapFromLazarusResourceHandle(Handle: TLResource): TBitmap;
var
  CB: TCustomBitmap;
begin
  CB := CreateBitmapFromLazarusResource(Handle, TCustomBitmap);
  if CB is TBitmap
  then begin
    Result := TBitmap(CB);
    Exit;
  end;
  
  Result := TBitmap.Create;
  Result.Assign(CB);
  CB.Free;
end;

function LoadBitmapFromLazarusResource(const ResourceName: String): TBitmap;
var
  CB: TCustomBitmap;
begin
  CB := CreateBitmapFromLazarusResource(ResourceName, TCustomBitmap);

  if CB is TBitmap
  then begin
    Result := TBitmap(CB);
    Exit;
  end;

  Result := TBitmap.Create;
  Result.Assign(CB);
  CB.Free;
end;

//TODO: publish ?? (as RawImage_CreateCompatibleBitmaps)
function CreateCompatibleBitmaps(const ARawImage: TRawImage; out ABitmap, AMask: HBitmap; ASkipMask: Boolean = False): Boolean;
var
  Desc: TRawImageDescription absolute ARawimage.Description;

  ImagePtr: PRawImage;
  DevImage: TRawImage;
  DevDesc: TRawImageDescription;
  SrcImage, DstImage: TLazIntfImage;
  QueryFlags: TRawImageQueryFlags;
  W, H: Integer;
begin
  W := Desc.Width;
  if W < 1 then W := 1;
  H := Desc.Height;
  if H < 1 then H := 1;

  if Desc.Depth = 1
  then QueryFlags := [riqfMono]
  else QueryFlags := [riqfRGB];
  if Desc.AlphaPrec <> 0
  then Include(QueryFlags, riqfAlpha);
  if Desc.MaskBitsPerPixel <> 0
  then Include(QueryFlags, riqfMask);
  QueryDescription(DevDesc, QueryFlags, W, H);

  if DevDesc.IsEqual(Desc)
  then begin
    // image is compatible, so use it
    DstImage := nil;
    ImagePtr := @ARawImage;
  end
  else begin
    // create compatible copy
    SrcImage := TLazIntfImage.Create(ARawImage, False);
    DstImage := TLazIntfImage.Create(0,0,[]);
    // create mask for alphachannel when device has no alpha support
    if (DevDesc.AlphaPrec = 0) and (riqfAlpha in QueryFlags)
    then begin
      //add mask if not already queried
      if not (riqfMask in QueryFlags)
      then QueryDescription(DevDesc, [riqfMask, riqfUpdate]);
      DstImage.DataDescription := DevDesc;
      DstImage.CopyPixels(SrcImage, 0, 0, True, $8000);
    end
    else begin
      // update DevDesc because of unusual bitmaps. issue #12362
      // widgetset can provide same DevDesc, but also can change it
      // like gtk/gtk2 does since it expects XBM format for mono bitmaps.
      if DevDesc.Depth = 1 then
      begin
        QueryFlags := QueryFlags + [riqfUpdate];
        QueryDescription(DevDesc, QueryFlags);
      end;
      DstImage.DataDescription := DevDesc;
      DstImage.CopyPixels(SrcImage);
    end;
    SrcImage.Free;
    DstImage.GetRawImage(DevImage);
    ImagePtr := @DevImage;
  end;

  try
    Result := RawImage_CreateBitmaps(ImagePtr^, ABitmap, AMask, ASkipMask);
  finally
    DstImage.Free;
  end;
end;

function CreateBitmapFromFPImage(Img: TFPCustomImage): TBitmap;
var
  IntfImg: TLazIntfImage;
  ok: Boolean;
begin
  Result:=nil;
  IntfImg:=nil;
  ok:=false;
  try
    Result:=TBitmap.Create;
    IntfImg:=Result.CreateIntfImage;
    IntfImg.SetSize(Img.Width,Img.Height);
    IntfImg.CopyPixels(Img);
    Result.LoadFromIntfImage(IntfImg);
    ok:=true;
  finally
    if not ok then FreeAndNil(Result);
    IntfImg.Free;
  end;
end;

function ScaleX(const SizeX, FromDPI: Integer): Integer;
begin
  Result := MulDiv(SizeX, ScreenInfo.PixelsPerInchX, FromDPI);
end;

function ScaleY(const SizeY, FromDPI: Integer): Integer;
begin
  Result := MulDiv(SizeY, ScreenInfo.PixelsPerInchY, FromDPI);
end;

procedure Register;
begin
  RegisterClasses([TBitmap,TPixmap,TPortableNetworkGraphic,
                   TPortableAnyMapGraphic,TJpegImage,TGIFImage,TPicture,
                   TFont,TPen,TBrush,TRegion]);
end;

const
  GraphicsFinalized: boolean = false;

type
  TBitmapCanvas = class(TCanvas)
  private
    FImage: TRasterImage;
    FOldBitmap: HBITMAP;
    FOldPalette: HPALETTE;
    procedure FreeDC; // called by TCustomBitmap.FreeCanvasContext
  protected
    procedure CreateHandle; override;
  public
    constructor Create(AImage: TRasterImage);
    destructor Destroy; override;
  end;


{ Color mapping routines }

const
  FirstDeprecatedColorIndex = 53;
  LastDeprecatedColorIndex = 106;
  Colors: array[0..106] of TIdentMapEntry = (
    // standard colors
    (Value: clBlack; Name: 'clBlack'),
    (Value: clMaroon; Name: 'clMaroon'),
    (Value: clGreen; Name: 'clGreen'),
    (Value: clOlive; Name: 'clOlive'),
    (Value: clNavy; Name: 'clNavy'),
    (Value: clPurple; Name: 'clPurple'),
    (Value: clTeal; Name: 'clTeal'),
    (Value: clGray; Name: 'clGray'),
    (Value: clSilver; Name: 'clSilver'),
    (Value: clRed; Name: 'clRed'),
    (Value: clLime; Name: 'clLime'),
    (Value: clYellow; Name: 'clYellow'),
    (Value: clBlue; Name: 'clBlue'),
    (Value: clFuchsia; Name: 'clFuchsia'),
    (Value: clAqua; Name: 'clAqua'),
    (Value: clWhite; Name: 'clWhite'),

    // extended colors
    (Value: clMoneyGreen; Name: 'clMoneyGreen'),
    (Value: clSkyBlue; Name: 'clSkyBlue'),
    (Value: clCream; Name: 'clCream'),
    (Value: clMedGray; Name: 'clMedGray'),

    // special colors
    (Value: clNone; Name: 'clNone'),
    (Value: clDefault; Name: 'clDefault'),

    // system colors
    (Value: clScrollBar; Name: 'clScrollBar'),
    (Value: clBackground; Name: 'clBackground'),
    (Value: clActiveCaption; Name: 'clActiveCaption'),
    (Value: clInactiveCaption; Name: 'clInactiveCaption'),
    (Value: clMenu; Name: 'clMenu'),
    (Value: clMenuBar; Name: 'clMenuBar'),
    (Value: clMenuHighlight; Name: 'clMenuHighlight'),
    (Value: clMenuText; Name: 'clMenuText'),
    (Value: clWindow; Name: 'clWindow'),
    (Value: clWindowFrame; Name: 'clWindowFrame'),
    (Value: clWindowText; Name: 'clWindowText'),
    (Value: clCaptionText; Name: 'clCaptionText'),
    (Value: clActiveBorder; Name: 'clActiveBorder'),
    (Value: clInactiveBorder; Name: 'clInactiveBorder'),
    (Value: clAppWorkspace; Name: 'clAppWorkspace'),
    (Value: clHighlight; Name: 'clHighlight'),
    (Value: clHighlightText; Name: 'clHighlightText'),
    (Value: clBtnFace; Name: 'clBtnFace'),
    (Value: clBtnShadow; Name: 'clBtnShadow'),
    (Value: clGrayText; Name: 'clGrayText'),
    (Value: clBtnText; Name: 'clBtnText'),
    (Value: clInactiveCaptionText; Name: 'clInactiveCaptionText'),
    (Value: clBtnHighlight; Name: 'clBtnHighlight'),
    (Value: cl3DDkShadow; Name: 'cl3DDkShadow'),
    (Value: cl3DLight; Name: 'cl3DLight'),
    (Value: clInfoText; Name: 'clInfoText'),
    (Value: clInfoBk; Name: 'clInfoBk'),

    (Value: clHotLight; Name: 'clHotLight'),
    (Value: clGradientActiveCaption; Name: 'clGradientActiveCaption'),
    (Value: clGradientInactiveCaption; Name: 'clGradientInactiveCaption'),

    // one our special color
    (Value: clForm; Name: 'clForm'),

    {$warnings off}
    // CLX base, mapped, pseudo, rgb values
    (Value: clForeground; Name: 'clForeground'),
    (Value: clButton; Name: 'clButton'),
    (Value: clLight; Name: 'clLight'),
    (Value: clMidlight; Name: 'clMidlight'),
    (Value: clDark; Name: 'clDark'),
    (Value: clMid; Name: 'clMid'),
    (Value: clText; Name: 'clText'),
    (Value: clBrightText; Name: 'clBrightText'),
    (Value: clButtonText; Name: 'clButtonText'),
    (Value: clBase; Name: 'clBase'),
    //clBackground
    (Value: clShadow; Name: 'clShadow'),
    //clHighlight
    (Value: clHighlightedText; Name: 'clHighlightedText'),

    // CLX normal, mapped, pseudo, rgb values
    (Value: clNormalForeground; Name: 'clNormalForeground'),
    (Value: clNormalButton; Name: 'clNormalButton'),
    (Value: clNormalLight; Name: 'clNormalLight'),
    (Value: clNormalMidlight; Name: 'clNormalMidlight'),
    (Value: clNormalDark; Name: 'clNormalDark'),
    (Value: clNormalMid; Name: 'clNormalMid'),
    (Value: clNormalText; Name: 'clNormalText'),
    (Value: clNormalBrightText; Name: 'clNormalBrightText'),
    (Value: clNormalButtonText; Name: 'clNormalButtonText'),
    (Value: clNormalBase; Name: 'clNormalBase'),
    (Value: clNormalBackground; Name: 'clNormalBackground'),
    (Value: clNormalShadow; Name: 'clNormalShadow'),
    (Value: clNormalHighlight; Name: 'clNormalHighlight'),
    (Value: clNormalHighlightedText; Name: 'clNormalHighlightedText'),

    // CLX disabled, mapped, pseudo, rgb values
    (Value: clDisabledForeground; Name: 'clDisabledForeground'),
    (Value: clDisabledButton; Name: 'clDisabledButton'),
    (Value: clDisabledLight; Name: 'clDisabledLight'),
    (Value: clDisabledMidlight; Name: 'clDisabledMidlight'),
    (Value: clDisabledDark; Name: 'clDisabledDark'),
    (Value: clDisabledMid; Name: 'clDisabledMid'),
    (Value: clDisabledText; Name: 'clDisabledText'),
    (Value: clDisabledBrightText; Name: 'clDisabledBrightText'),
    (Value: clDisabledButtonText; Name: 'clDisabledButtonText'),
    (Value: clDisabledBase; Name: 'clDisabledBase'),
    (Value: clDisabledBackground; Name: 'clDisabledBackground'),
    (Value: clDisabledShadow; Name: 'clDisabledShadow'),
    (Value: clDisabledHighlight; Name: 'clDisabledHighlight'),
    (Value: clDisabledHighlightedText; Name: 'clDisabledHighlightedText'),

    // CLX active, mapped, pseudo, rgb values
    (Value: clActiveForeground; Name: 'clActiveForeground'),
    (Value: clActiveButton; Name: 'clActiveButton'),
    (Value: clActiveLight; Name: 'clActiveLight'),
    (Value: clActiveMidlight; Name: 'clActiveMidlight'),
    (Value: clActiveDark; Name: 'clActiveDark'),
    (Value: clActiveMid; Name: 'clActiveMid'),
    (Value: clActiveText; Name: 'clActiveText'),
    (Value: clActiveBrightText; Name: 'clActiveBrightText'),
    (Value: clActiveButtonText; Name: 'clActiveButtonText'),
    (Value: clActiveBase; Name: 'clActiveBase'),
    (Value: clActiveBackground; Name: 'clActiveBackground'),
    (Value: clActiveShadow; Name: 'clActiveShadow'),
    (Value: clActiveHighlight; Name: 'clActiveHighlight'),
    (Value: clActiveHighlightedText; Name: 'clActiveHighlightedText')
    {$warnings on}
    );

function IdentEntry(Entry: Longint; out MapEntry: TIdentMapEntry): boolean;
begin
  Result := False;
  if (Entry >= 0) and (Entry <= High(Colors)) then
  begin
    MapEntry := Colors[Entry];
    Result := True;
  end;
end;

function ColorToIdent(Color: Longint; out Ident: String): Boolean;
begin
  Result := IntToIdent(Color, Ident, Colors);
end;

function IdentToColor(const Ident: string; out Color: Longint): Boolean;
begin
  Result := IdentToInt(Ident, Color, Colors);
end;

function ColorIndex(Color: Longint; out Index: Integer): Boolean;
var
  i: integer;
begin
  for i := Low(Colors) to High(Colors) do
    if Colors[i].Value = Color then
    begin
      Result := True;
      Index := i;
      exit;
    end;
  Result := False;
end;

function SysColorToSysColorIndex(Color: TColor): integer;
begin
  if (Cardinal(Color) and Cardinal(SYS_COLOR_BASE)) <> 0 then begin
    case Color of
    {$warnings off}
    clHighlightedText..clForeground:   // Deprecated values!
      Result:=clForeground+COLOR_clForeground-Color;
    clNormalHighlightedText..clNormalForeground:
      Result:=clNormalForeground+COLOR_clNormalForeground-Color;
    clDisabledHighlightedText..clDisabledForeground:
      Result:=clDisabledForeground+COLOR_clDisabledForeground-Color;
    clActiveHighlightedText..clActiveForeground:
      Result:=clActiveForeground+COLOR_clActiveForeground-Color;
    {$warnings on}
    else
      Result:=Color and $FF;
    end;
  end else begin
    Result:=-1;
  end;
end;

function ColorToRGB(Color: TColor): Longint;
var
  i: integer;
begin
  i := SysColorToSysColorIndex(Color);
  if i <> -1 then
    Result := GetSysColor(i)
  else
    Result := Color;
  Result := Result and $FFFFFF;
end;

function ColorToString(Color: TColor): AnsiString;
begin
  Result := '';
  if not ColorToIdent(Color, Result) then
    Result:='$'+HexStr(Color,8);
end;

function StringToColor(const S: shortstring): TColor;
begin
  Result := clNone;
  if not IdentToColor(S, Longint(Result)) then
    Result := TColor(StrToInt(S));
end;

function StringToColorDef(const S: shortstring; const DefaultValue: TColor): TColor;
begin
  Result := DefaultValue;
  if not IdentToColor(S, Longint(Result)) then
    Result := TColor(StrToIntDef(S,DefaultValue));
end;

procedure GetColorValues(Proc: TGetColorStringProc);
var
  I: Integer;
begin
  for I := Low(Colors) to High(Colors) do
    if (I >= FirstDeprecatedColorIndex) and (I <= LastDeprecatedColorIndex) then
      Continue
    else
      Proc(Colors[I].Name);
end;

function InvertColor(AColor: TColor): TColor;
var
  R, G, B: Integer;
begin
  R := AColor and $ff;
  G := (AColor shr 8) and $ff;
  B := (AColor shr 16) and $ff;

  if Abs($80 - R) + Abs($80 - G) + Abs($80 - B) < $140 then
  begin
    if R<$80 then
      R:=Min($ff,R+$a0)
    else
      R:=Max(0,R-$a0);
    if G<$80 then
      G:=Min($ff,G+$a0)
    else
      G:=Max(0,G-$a0);
    if B<$80 then
      B:=Min($ff,B+$a0)
    else
      B:=Max(0,B-$a0);
  end
  else
  begin
    R := $ff - R;
    G := $ff - G;
    B := $ff - B;
  end;
  
  Result := ((B and $ff) shl 16) or ((G and $ff) shl 8) or (R and $ff);
end;

function Blue(rgb: TColorRef): BYTE;
begin
  Result := (rgb shr 16) and $000000ff;
end;

function Green(rgb: TColorRef): BYTE;
begin
  Result := (rgb shr 8) and $000000ff;
end;

function Red(rgb: TColorRef): BYTE;
begin
  Result := rgb and $000000ff;
end;

function RGBToColor(R, G, B: Byte): TColor;
begin
  Result := (B shl 16) or (G shl 8) or R;
end;

procedure RedGreenBlue(rgb: TColorRef; out Red, Green, Blue: Byte);
begin
  Red := rgb and $000000ff;
  Green := (rgb shr 8) and $000000ff;
  Blue := (rgb shr 16) and $000000ff;
end;

function FPColorToTColorRef(const FPColor: TFPColor): TColorRef;
begin
  Result:=((FPColor.Red shr 8) and $ff)
       or (FPColor.Green and $ff00)
       or ((FPColor.Blue shl 8) and $ff0000);
end;

function FPColorToTColor(const FPColor: TFPColor): TColor;
begin
  Result:=TColor(FPColorToTColorRef(FPColor));
end;

function TColorToFPColor(const c: TColorRef): TFPColor;
begin
  Result.Red:=(c and $ff);
  Result.Red:=Result.Red+(Result.Red shl 8);
  Result.Green:=(c and $ff00);
  Result.Green:=Result.Green+(Result.Green shr 8);
  Result.Blue:=(c and $ff0000) shr 8;
  Result.Blue:=Result.Blue+(Result.Blue shr 8);
  Result.Alpha:=FPImage.alphaOpaque;
end;

function TColorToFPColor(const c: TColor): TFPColor;
begin
  Result:=TColorToFPColor(TColorRef(c));
end;

// ------------------------------------------------------------------
// Decrease the component RGBs of a color of the quantity' passed
//
// Color    : Color to decrease
// Quantity : Decrease quantity
// ------------------------------------------------------------------
function DecColor(AColor: TColor; AQuantity: Byte) : TColor;
var
  R, G, B : Byte;
begin
  RedGreenBlue(ColorToRGB(AColor), R, G, B);
  R := Max(0, Integer(R) - AQuantity);
  G := Max(0, Integer(G) - AQuantity);
  B := Max(0, Integer(B) - AQuantity);
  Result := RGBToColor(R, G, B);
end;

function IsSysColor(AColor: TColorRef): Boolean;
begin
  Result := (AColor and SYS_COLOR_BASE) <> 0;
end;


{$I graphicsobject.inc}
{$I graphic.inc}
{$I picture.inc}
{$I sharedimage.inc}
{$I sharedrasterimage.inc}
{$I sharedcustombitmap.inc}
{$I rasterimage.inc}
{$I custombitmap.inc}
{$I bitmapcanvas.inc}
{$I pen.inc}
{$I brush.inc}
{$I region.inc}
{$I font.inc}
{$I canvas.inc}
{$I pixmap.inc}
{$I png.inc}
{$I pnm.inc}
{$I jpegimage.inc}
{$I icon.inc}
{$I icnsicon.inc}
{$I cursorimage.inc}
{$I fpimagebitmap.inc}
{$I bitmap.inc}
{$I tiffimage.inc}
{$I gifimage.inc}

function CreateGraphicFromResourceName(Instance: THandle; const ResName: String): TGraphic;
var
  ResHandle: TFPResourceHandle;
begin
  // test Icon
  ResHandle := FindResource(Instance, PChar(ResName), PChar(RT_GROUP_ICON));
  if ResHandle <> 0 then
  begin
    Result := TIcon.Create;
    TIcon(Result).LoadFromResourceHandle(Instance, ResHandle);
    Exit;
  end;
  // test Cursor
  ResHandle := FindResource(Instance, PChar(ResName), PChar(RT_GROUP_CURSOR));
  if ResHandle <> 0 then
  begin
    Result := TCursorImage.Create;
    TCursorImage(Result).LoadFromResourceHandle(Instance, ResHandle);
  end
  else
    Result := CreateBitmapFromResourceName(Instance, ResName)
end;

function CreateBitmapFromResourceName(Instance: THandle; const ResName: String): TCustomBitmap;
var
  ResHandle: TFPResourceHandle;
  Stream: TResourceStream;
  GraphicClass: TGraphicClass;
begin
  ResHandle := FindResource(Instance, PChar(ResName), PChar(RT_BITMAP));
  if ResHandle <> 0 then
  begin
    Result := TBitmap.Create;
    Result.LoadFromResourceName(Instance, ResName);
    Exit;
  end;
  ResHandle := FindResource(Instance, PChar(ResName), PChar(RT_RCDATA));
  if ResHandle <> 0 then
  begin
    Stream := TResourceStream.Create(Instance, ResName, RT_RCDATA);
    try
      GraphicClass := GetPicFileFormats.FindByStreamFormat(Stream);
      if Assigned(GraphicClass) and GraphicClass.InheritsFrom(TCustomBitmap) then
      begin
        Result := TCustomBitmap(GraphicClass.Create);
        Result.LoadFromStream(Stream);
      end
      else
        Result := nil;
    finally
      Stream.Free;
    end;
  end
  else
    Result := nil;
end;

function LocalGetSystemFont: HFont;
begin
  Result := GetStockObject(DEFAULT_GUI_FONT);
end;

procedure InterfaceInit;
begin
  //debugln('Graphics.InterfaceInit');
  FontResourceCache:=TFontHandleCache.Create;
  PenResourceCache:=TPenHandleCache.Create;
  BrushResourceCache:=TBrushHandleCache.Create;
end;

procedure InterfaceFinal;
begin
  //debugln('Graphics.InterfaceFinal');
  FreeAndNil(FontResourceCache);
  FreeAndNil(PenResourceCache);
  FreeAndNil(BrushResourceCache);
end;

{ TCursorImageImage }

constructor TCursorImageImage.Create(const AInfo: TIconInfo);
begin
  inherited Create(AInfo);
  FHotSpot.x := AInfo.xHotspot;
  FHotSpot.y := AInfo.yHotspot;
end;

initialization
  UpdateLock := TCriticalSection.Create;
  OnGetSystemFont := @LocalGetSystemFont;
  LoadBitmapFunction := @LocalLoadBitmap;
  LoadCursorFunction := @LocalLoadCursor;
  LoadIconFunction := @LocalLoadIcon;
  RegisterIntegerConsts(TypeInfo(TColor), TIdentToInt(@IdentToColor), TIntToIdent(@ColorToIdent));
  RegisterIntegerConsts(TypeInfo(TFontCharset), TIdentToInt(@IdentToCharset), TIntToIdent(@CharsetToIdent));
  RegisterInterfaceInitializationHandler(@InterfaceInit);
  RegisterInterfaceFinalizationHandler(@InterfaceFinal);

finalization
  GraphicsFinalized:=true;
  OnLoadSaveClipBrdGraphicValid:=false;
  FreeAndNil(PicClipboardFormats);
  FreeAndNil(PicFileFormats);
  UpdateLock.Free;

end.
