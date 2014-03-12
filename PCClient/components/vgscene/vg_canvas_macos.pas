unit vg_canvas_macos;

{$I vg_define.inc}
{$H+}

interface

uses
  {$IFDEF DARWIN}
  {$IFDEF NOVCL}
  CocoaAll, MacTypes,
  CFNumber, CFString, CFBase, CFUrl,
  CGImage, CGGradient, CGFunction, CGShading, CGColor, CGColorSpace, CGContext, CGBitmapContext,
  CGGeometry, CGImageSource, CGImageDestination, CGDataProvider, CGDataConsumer, CFDictionary,
  CGAffineTransforms,
  MacOSAll,
  {$ELSE}
  MacOSAll,
  {$ENDIF}
  {$ENDIF}
  Classes, SysUtils, vg_scene;

const
  merge2 = 0;

{$IFDEF DARWIN}

type

  { TvgFilterQuartz }

  TvgFilterQuartz = class(TvgFilter)
  private
  public
  published
    class function GetFileTypes: string; override;
    class function GetImageSize(const AFileName: string): TvgPoint; override;
    function LoadFromFile(const AFileName: string; const Rotate: single; var Bitmap: TvgBitmap): boolean; override;
    function LoadThumbnailFromFile(const AFileName: string; const AFitWidth, AFitHeight: single; const UseEmbedded: boolean;var Bitmap: TvgBitmap): boolean; override;
    function SaveToFile(const AFileName: string; var Bitmap: TvgBitmap; const Params: string = ''): boolean; override;
    function LoadFromStream(const AStream: TStream; var Bitmap: TvgBitmap): boolean; override;
    function SaveToStream(const AStream: TStream; var Bitmap: TvgBitmap; const Format: string;
      const Params: string = ''): boolean; override;
  end;

  { TvgCanvasQuartz }

  TvgCanvasQuartz = class(TvgCanvas)
  private
    Func: CGFunctionRef;
    BitmapRef: CGImageRef;
    Callback: CGFunctionCallbacks;
    Shading: CGShadingRef;
    ColorSpace: CGColorSpaceRef;
  protected
    procedure ApplyFill(ARect: TvgRect; const AOpacity: single);
    procedure DeApplyFill(ARect: TvgRect; const AOpacity: single);
    procedure ApplyStroke(ARect: TvgRect; const AOpacity: single);
    procedure FontChanged(Sender: TObject); override;
    procedure UpdateBitmap(ABitmap: TvgBitmap);
    procedure DoDestroyBitmap(Sender: TObject);
  public
    constructor Create(const AWidth, AHeight: integer); override;
    constructor CreateFromBitmap(const ABitmap: TvgBitmap); override;
    destructor Destroy; override;
    { buffer }
    procedure FreeBuffer; override;
    procedure ResizeBuffer(const AWidth, AHeight: integer); override;
    procedure FlushBuffer(const X, Y: integer; const DC: Cardinal); override;
    procedure FlushBufferRect(const X, Y: integer; const DC: Cardinal; const ARect: TvgRect); override;
    procedure Clear(const Color: cardinal); override;
    procedure ClearRect(const ARect: TvgRect; const AColor: TvgColor = 0); override;
    class function GetBitmapScanline(Bitmap: TvgBitmap; y: integer): PvgColorArray; override;
    { matrix }
    procedure SetMatrix(const M: TvgMatrix); override;
    procedure MultyMatrix(const M: TvgMatrix); override;
    { cliping }
    function SaveCanvas: cardinal; override;
    procedure RestoreCanvas(const AState: cardinal); override;
    procedure SetClipRects(const ARects: array of TvgRect); override;
    procedure IntersectClipRect(const ARect: TvgRect); override;
    procedure ExcludeClipRect(const ARect: TvgRect); override;
    procedure ResetClipRect; override;
    { drawing }
    procedure DrawLine(const APt1, APt2: TvgPoint; const AOpacity: single); override;
    procedure FillRect(const ARect: TvgRect; const xRadius, yRadius: single; const ACorners: TvgCorners; const AOpacity: single;
      const ACornerType: TvgCornerType = vgCornerRound); override;
    procedure DrawRect(const ARect: TvgRect; const xRadius, yRadius: single; const ACorners: TvgCorners; const AOpacity: single;
      const ACornerType: TvgCornerType = vgCornerRound); override;
    procedure FillEllipse(const ARect: TvgRect; const AOpacity: single); override;
    procedure DrawEllipse(const ARect: TvgRect; const AOpacity: single); override;
    function PtInPath(const APoint: TvgPoint; const ARect: TvgRect; const APath: TvgPathData): boolean; override;
    procedure FillPath(const APath: TvgPathData; const ARect: TvgRect; const AOpacity: single); override;
    procedure DrawPath(const APath: TvgPathData; const ARect: TvgRect; const AOpacity: single); override;
    procedure DrawBitmap(const ABitmap: TvgBitmap; const SrcRect, DstRect: TvgRect; const AOpacity: single; const HighSpeed: boolean = false); override;
    procedure FillText(const ARect, AClipRect: TvgRect; const AText: WideString; const WordWrap: boolean;
      const AOpacity: single; const ATextAlign: TvgTextAlign; const AVTextAlign: TvgTextAlign = vgTextAlignCenter); override;
    procedure MeasureText(var ARect: TvgRect; AClipRect: TvgRect; const AText: WideString; const WordWrap: boolean; const ATextAlign: TvgTextAlign;
      const AVTextAlign: TvgTextAlign = vgTextAlignCenter); override;
    function TextToPath(Path: TvgPathData; const ARect: TvgRect; const AText: WideString; const WordWrap: boolean; const ATextAlign: TvgTextAlign;
      const AVTextAlign: TvgTextAlign = vgTextAlignCenter): boolean; override;
  published
  end;

function CGRectFromRect(const R: TvgRect): CGRect;

{$ENDIF}

implementation {===============================================================}


{$IFDEF DARWIN}

{ TvgFilterQuartz }

class function TvgFilterQuartz.GetFileTypes: string;
begin
  Result := '*.jpg;*.jpeg;*.gif;*.tif;*.tiff;*.psd;*.jp2;*.png;*.bmp;*.tga;*.icns';
end;

class function TvgFilterQuartz.GetImageSize(const AFileName: string): TvgPoint;
var
  path: CFStringRef;
  url: CFURLRef;
  imgSourceRef: CGImageSourceRef;
  imgRef: CGImageRef;
  ctxRef: CGContextRef;
  bits: PvgColorArray;
begin
  Result := vgPoint(0, 0);
  path := CFStringCreateWithCString(nil, PChar(AFileName), kCFStringEncodingUTF8);
  url := CFURLCreateWithFileSystemPath(nil, path, kCFURLPOSIXPathStyle, false);
  CFRelease (path);
  imgSourceRef := CGImageSourceCreateWithURL(url, nil);
  CFRelease (url);
  if imgSourceRef <> nil then
  begin
    ImgRef := CGImageSourceCreateImageAtIndex(imgSourceRef, 0, nil);
    if ImgRef <> nil then
    begin
      Result := vgPoint(CGImageGetWidth(ImgRef), CGImageGetHeight(ImgRef));
      CGImageRelease(imgRef);
    end;
    CFRelease(imgSourceRef);
  end;
end;

function TvgFilterQuartz.LoadFromStream(const AStream: TStream; var Bitmap: TvgBitmap): boolean;
var
  provider: CGDataProviderRef;
  memStream: TMemoryStream;
  imgSourceRef: CGImageSourceRef;
  imgRef: CGImageRef;
  ctxRef: CGContextRef;
  bits: PvgColorArray;
  colorspace: CGColorSpaceRef;
begin
  memStream := TMemoryStream.Create;
  memStream.CopyFrom(AStream, AStream.Size);
  memStream.Position := 0;
  provider := CGDataProviderCreateWithData(nil, memStream.Memory, memStream.Size, nil);
  if provider <> nil then
  begin
    imgSourceRef := CGImageSourceCreateWithDataProvider(provider, nil);
    if imgSourceRef <> nil then
    begin
      ImgRef := CGImageSourceCreateImageAtIndex(imgSourceRef, 0, nil);
      if ImgRef <> nil then
      begin
        Bitmap.SetSize(CGImageGetWidth(ImgRef), CGImageGetHeight(ImgRef));
        colorspace := CGColorSpaceCreateDeviceRGB;
        ctxRef := CGBitmapContextCreate(Bitmap.StartLine, Bitmap.Width, Bitmap.Height, 8,
           Bitmap.Width * 4, CGColorSpaceCreateDeviceRGB, kCGImageAlphaPremultipliedLast);
        CGContextDrawImage(ctxRef, CGRectFromRect(vgRect(0, 0, Bitmap.Width, Bitmap.Height)), imgRef);
        CGContextRelease(ctxRef);
        CGColorSpaceRelease(colorspace);
        CGImageRelease(imgRef);
      end;
      CFRelease(imgSourceRef);
    end;
    CGDataProviderRelease(provider);
  end;
  memStream.Free;
end;

function streamPutBytesCallback(info: Pointer; buffer: {const} Pointer; count: size_t ): size_t; cdecl;
begin
  TStream(info).Write(buffer^, count);
end;

procedure streamReleaseInfoCallback( info: Pointer );
begin
//  TMemoryStream(info).Free;
end;

function TvgFilterQuartz.SaveToStream(const AStream: TStream; var Bitmap: TvgBitmap; const Format: string;
      const Params: string = ''): boolean;
var
  consumer: CGDataConsumerRef;
  imgDestRef: CGImageDestinationRef;
  imgRef: CGImageRef;
  ctxRef: CGContextRef;
  bits: PvgColorArray;
  encoderType: CFStringRef;
  callback: CGDataConsumerCallbacks;
  colorspace: CGColorSpaceRef;
  keys: array [0..10] of pointer;
  value: array [0..10] of pointer;
  floatVal: single;
  S, param, name, val: string;
  dict: CFDictionaryRef;
begin
  callback.putBytes := @streamPutBytesCallback;
  callback.releaseConsumer := @streamReleaseInfoCallback;
  consumer := CGDataConsumerCreate(AStream, callback);

  encoderType := kUTTypePng;
  if (LowerCase(Format) = 'jpg') or (LowerCase(Format) = 'jpeg') then
    encoderType := kUTTypeJpeg;
  if (LowerCase(Format) = 'jp2') then
    encoderType := kUTTypeJPEG2000;
  if (LowerCase(Format) = 'bmp') then
    encoderType := kUTTypeBmp;
  if (LowerCase(Format) = 'png') then
    encoderType := kUTTypePng;
  if (LowerCase(Format) = 'tif') or (LowerCase(Format) = 'tiff') then
    encoderType := kUTTypeTiff;
  if (LowerCase(Format) = 'gif') then
    encoderType := kUTTypeGif;

  imgDestRef := CGImageDestinationCreateWithDataConsumer(consumer, encoderType, 1, nil);
  if imgDestRef <> nil then
  begin
    colorSpace := CGColorSpaceCreateDeviceRGB;
    ctxRef := CGBitmapContextCreate(Bitmap.StartLine, Bitmap.Width, Bitmap.Height, 8,
       Bitmap.Width * 4, colorSpace, kCGImageAlphaPremultipliedLast);
    imgRef := CGBitmapContextCreateImage(ctxRef);
    if imgRef <> nil then
    begin
      dict := nil;
      { set params }
      if Params <> '' then
      begin
        S := Params;
        while S <> '' do
        begin
          param := vgGetToken(S, ' ');
          name := vgGetToken(param, '=');
          val := vgGetToken(param, '');
          if sysutils.CompareText(name, 'quality') = 0 then
          begin
            floatVal := strToFloat(val) / 100;
            keys[0] := kCGImageDestinationLossyCompressionQuality;
            value[0] := CFNumberCreate(nil, kCFNumberFloat32Type, @floatVal);
            dict := CFDictionaryCreate(nil, @keys[0], @value[0], 1, nil, nil);
            CGImageDestinationSetProperties(imgDestRef, dict);
          end;
        end;
      end;
      CGImageDestinationAddImage(imgDestRef, imgRef, dict);
      CGImageDestinationFinalize(imgDestRef);
      if dict <> nil then
        CFRelease(dict);
    end;
    CGImageRelease(imgRef);
    CGContextRelease(ctxRef);
    CGColorSpaceRelease(colorspace);

    CFRelease(imgDestRef);
  end;
end;

function TvgFilterQuartz.LoadFromFile(const AFileName: string; const Rotate: single;
  var Bitmap: TvgBitmap): boolean;
var
  path: CFStringRef;
  url: CFURLRef;
  imgSourceRef: CGImageSourceRef;
  imgRef: CGImageRef;
  ctxRef: CGContextRef;
  bits: PvgColorArray;
  M, M2: TvgMatrix;
  R: TvgRect;
  Pts: array [1..4] of TvgPoint;
begin
  Result := false;
  path := CFStringCreateWithCString(nil, PChar(AFileName), kCFStringEncodingUTF8);
  url := CFURLCreateWithFileSystemPath(nil, path, kCFURLPOSIXPathStyle, false);
  CFRelease (path);
  imgSourceRef := CGImageSourceCreateWithURL(url, nil);
  CFRelease (url);
  if imgSourceRef <> nil then
  begin
    ImgRef := CGImageSourceCreateImageAtIndex(imgSourceRef, 0, nil);
    if ImgRef <> nil then
    begin
      if Rotate <> 0.0 then
      begin
        M := IdentityMatrix;
        M.m31 := -(CGImageGetWidth(ImgRef) / 2);
        M.m32 := -(CGImageGetHeight(ImgRef) / 2);
        M := vgMatrixMultiply(M, vgCreateRotationMatrix(vgDegToRad(-Rotate)));
        { calc new size }
        Pts[1] := vgPointFromVector(vgVectorTransform(vgVector(0, 0), M));
        Pts[2] := vgPointFromVector(vgVectorTransform(vgVector(CGImageGetWidth(ImgRef), 0), M));
        Pts[3] := vgPointFromVector(vgVectorTransform(vgVector(CGImageGetWidth(ImgRef), CGImageGetHeight(ImgRef)), M));
        Pts[4] := vgPointFromVector(vgVectorTransform(vgVector(0, CGImageGetHeight(ImgRef)), M));
        R := vgNormalizeRect(Pts);
        { translate }
        M2 := IdentityMatrix;
        M2.m31 := vgRectWidth(R) / 2;
        M2.m32 := vgRectHeight(R) / 2;
        M := vgMatrixMultiply(M, M2);
        { rotate }
        Bitmap.SetSize(Trunc(vgRectWidth(R)), Trunc(vgRectHeight(R)));
          ctxRef := CGBitmapContextCreate(Bitmap.StartLine, Bitmap.Width, Bitmap.Height, 8,
             Bitmap.Width * 4, CGColorSpaceCreateDeviceRGB, kCGImageAlphaPremultipliedLast);
          with M do
            CGContextConcatCTM(CtxRef, CGAffineTransformMake(m11, m12, m21, m22, m31, m32));
          CGContextDrawImage(ctxRef, CGRectFromRect(vgRect(0, 0, CGImageGetWidth(ImgRef), CGImageGetHeight(ImgRef))), imgRef);
          CGContextRelease(ctxRef);
      end
      else
      begin
        Bitmap.SetSize(CGImageGetWidth(ImgRef), CGImageGetHeight(ImgRef));
          ctxRef := CGBitmapContextCreate(Bitmap.StartLine, Bitmap.Width, Bitmap.Height, 8,
             Bitmap.Width * 4, CGColorSpaceCreateDeviceRGB, kCGImageAlphaPremultipliedLast);
          CGContextDrawImage(ctxRef, CGRectFromRect(vgRect(0, 0, Bitmap.Width, Bitmap.Height)), imgRef);
          CGContextRelease(ctxRef);
      end;
      Result := true;
      CGImageRelease(imgRef);
    end;
    CFRelease(imgSourceRef);
  end;
end;

function TvgFilterQuartz.LoadThumbnailFromFile(const AFileName: string;
  const AFitWidth, AFitHeight: single; const UseEmbedded: boolean; var Bitmap: TvgBitmap): boolean;
var
  path: CFStringRef;
  url: CFURLRef;
  imgSourceRef: CGImageSourceRef;
  imgRef: CGImageRef;
  ctxRef: CGContextRef;
  bits: PvgColorArray;
  dict: CFDictionaryRef;
  keys: array [0..10] of pointer;
  value: array [0..10] of pointer;
  intValue: cardinal;
begin
  Result := false;
  path := CFStringCreateWithCString(nil, PChar(AFileName), kCFStringEncodingUTF8);
  url := CFURLCreateWithFileSystemPath(nil, path, kCFURLPOSIXPathStyle, false);
  CFRelease (path);
  imgSourceRef := CGImageSourceCreateWithURL(url, nil);
  CFRelease (url);
  if imgSourceRef <> nil then
  begin
    keys[0] := kCGImageSourceCreateThumbnailFromImageAlways;
    value[0] := kCFBooleanTrue;
    keys[1] := kCGImageSourceThumbnailMaxPixelSize;
    if AFitWidth > AFitHeight then
      intValue := trunc(AFitWidth)
    else
      intValue := trunc(AFitHeight);
    value[1] := CFNumberCreate(nil, kCFNumberSInt32Type, @intValue);
    dict := CFDictionaryCreate(nil, @keys[0], @value[0], 2, nil, nil);
    ImgRef := CGImageSourceCreateThumbnailAtIndex(imgSourceRef, 0, dict);
    CFRelease(dict);
    if ImgRef <> nil then
    begin
      Bitmap.SetSize(CGImageGetWidth(ImgRef), CGImageGetHeight(ImgRef));
        ctxRef := CGBitmapContextCreate(Bitmap.StartLine, Bitmap.Width, Bitmap.Height, 8,
           Bitmap.Width * 4, CGColorSpaceCreateDeviceRGB, kCGImageAlphaPremultipliedLast);
        CGContextDrawImage(ctxRef, CGRectFromRect(vgRect(0, 0, Bitmap.Width, Bitmap.Height)), imgRef);
        CGContextRelease(ctxRef);
      CGImageRelease(imgRef);
      Result := true;
    end;
    CFRelease(imgSourceRef);
  end;
end;

function TvgFilterQuartz.SaveToFile(const AFileName: string;
  var Bitmap: TvgBitmap; const Params: string = ''): boolean;
var
  path: CFStringRef;
  url: CFURLRef;
  imgDestRef: CGImageDestinationRef;
  imgRef: CGImageRef;
  ctxRef: CGContextRef;
  bits: PvgColorArray;
  encoderType: CFStringRef;
  keys: array [0..10] of pointer;
  value: array [0..10] of pointer;
  floatVal: single;
  S, param, name, val: string;
  dict: CFDictionaryRef;
begin
  path := CFStringCreateWithCString(nil, PChar(AFileName), kCFStringEncodingUTF8);
  url := CFURLCreateWithFileSystemPath(nil, path, kCFURLPOSIXPathStyle, false);
  CFRelease (path);
  encoderType := nil;
  if (LowerCase(ExtractFileExt(AFileName)) = '.jpg') or (LowerCase(ExtractFileExt(AFileName)) = '.jpeg') then
    encoderType := kUTTypeJpeg;
  if (LowerCase(ExtractFileExt(AFileName)) = '.jp2') then
    encoderType := kUTTypeJPEG2000;
  if (LowerCase(ExtractFileExt(AFileName)) = '.bmp') then
    encoderType := kUTTypeBmp;
  if (LowerCase(ExtractFileExt(AFileName)) = '.png') then
    encoderType := kUTTypePng;
  if (LowerCase(ExtractFileExt(AFileName)) = '.tif') or (LowerCase(ExtractFileExt(AFileName)) = '.tiff') then
    encoderType := kUTTypeTiff;
  if (LowerCase(ExtractFileExt(AFileName)) = '.gif') then
    encoderType := kUTTypeGif;
  if encoderType <> nil then
  begin
    imgDestRef := CGImageDestinationCreateWithURL(url, encoderType, 1, nil);

    CFRelease (url);
    if imgDestRef <> nil then
    begin
        ctxRef := CGBitmapContextCreate(Bitmap.StartLine, Bitmap.Width, Bitmap.Height, 8,
           Bitmap.Width * 4, CGColorSpaceCreateDeviceRGB, kCGImageAlphaPremultipliedLast);
        imgRef := CGBitmapContextCreateImage(ctxRef);
        if imgRef <> nil then
        begin
          dict := nil;
          { set params }
          if Params <> '' then
          begin
            S := Params;
            while S <> '' do
            begin
              param := vgGetToken(S, ' ');
              name := vgGetToken(param, '=');
              val := vgGetToken(param, '');
              if sysutils.CompareText(name, 'quality') = 0 then
              begin
                floatVal := strToFloat(val) / 100;
                keys[0] := kCGImageDestinationLossyCompressionQuality;
                value[0] := CFNumberCreate(nil, kCFNumberFloat32Type, @floatVal);
                dict := CFDictionaryCreate(nil, @keys[0], @value[0], 1, nil, nil);
                CGImageDestinationSetProperties(imgDestRef, dict);
              end;
            end;
          end;
          CGImageDestinationAddImage(imgDestRef, imgRef, dict);
          CGImageDestinationFinalize(imgDestRef);
          if dict <> nil then
            CFRelease(dict);
        end;
        CGImageRelease(imgRef);
        CGContextRelease(ctxRef);
      CFRelease(imgDestRef);
    end;
  end;
end;

{ TvgCanvasQuartz }

const
  inputRange: array [1..2] of single = (0, 1);

type

  TRGBFloat = packed record
    r, g, b, a: single;
  end;

function CGColor(const C: TvgColor; Opacity: single = 1): TRGBFloat;
var
  cc: TvgColor;
begin
  cc := vgOpacity(C, Opacity);
  Result.a := TvgColorRec(cc).a / $FF;
  Result.r := TvgColorRec(cc).r / $FF;
  Result.g := TvgColorRec(cc).g / $FF;
  Result.b := TvgColorRec(cc).b / $FF;
end;

function CGRectFromRect(const R: TvgRect): CGRect;
begin
  Result.origin.x := R.Left;
  Result.origin.Y := R.Top;
  Result.size.Width := R.Right - R.Left;
  Result.size.Height := R.Bottom - R.Top;
end;

procedure CGContextDrawTiledImage(CtxRef: CGContextRef; ARect: TvgRect; Img: CGImageRef; Bitmap: TvgBitmap);
var
  x, y: integer;
  CR: CGRect;
begin
  if Bitmap.Width * Bitmap.Height = 0 then Exit;
  for x := 0 to Trunc(vgRectWidth(ARect) / Bitmap.Width)  do
    for y := 0 to Trunc(vgRectHeight(ARect) / Bitmap.Height) do
    begin
      CR := CGRectFromRect(vgRect(ARect.Left + (Bitmap.Width * x), ARect.Top + (Bitmap.Height * y),
        ARect.Left + (Bitmap.Width * (x + 1)), ARect.Top + (Bitmap.Height * (y + 1))));
      CR.origin.y := -ARect.Top - (Bitmap.Height * (y + 1));
      CGContextDrawImage(CtxRef, CR, Img);
    end;
end;

var
  ColorArray: array [0..100] of cardinal;
  OffsetArray: array [0..100] of single;

const
  SavedCount = 2000;

constructor TvgCanvasQuartz.Create(const AWidth, AHeight: integer);
begin
  inherited ;
end;

constructor TvgCanvasQuartz.CreateFromBitmap(const ABitmap: TvgBitmap);
var
  B: PvgColorArray;
begin
  inherited;
  FBitmap := ABitmap;
  UpdateBitmap(FBitmap);
  FHandle := FBitmap.Handle;
end;

destructor TvgCanvasQuartz.Destroy;
begin
  inherited;
end;

procedure TvgCanvasQuartz.FreeBuffer;
begin
  if (FBitmap = nil) then
  begin
    if FBuffered then
    begin
      if FHandle <> 0 then
        CGContextRelease(CGContextRef(FHandle));
    end
    else
    begin
    end;
    if FBufferBits <> nil then
      System.FreeMem(FBufferBits);
  end;
end;

procedure TvgCanvasQuartz.ResizeBuffer(const AWidth, AHeight: integer);
begin
  if (AWidth = FWidth) and (AHeight = FHeight) then Exit;
  FreeBuffer;
  FWidth := AWidth;
  FHeight := AHeight;
  if FWidth <= 0 then FWidth := 1;
  if FHeight <= 0 then FHeight := 1;
  FResized := true;

  if FWidth * FHeight = 0 then Exit;
  if FBuffered then
  begin
    GetMem(FBufferBits, FWidth * FHeight * 4);
    FHandle := cardinal(CGBitmapContextCreate(FBufferBits, FWidth, FHeight, 8,
      FWidth * 4, CGColorSpaceCreateDeviceRGB, kCGImageAlphaPremultipliedLast));
    CGContextTranslateCTM(CGContextRef(Handle), 0, FHeight);
    CGContextScaleCTM(CGContextRef(Handle), 1, -1);
  end
  else
  begin
    GetMem(FBufferBits, 4);
  end;
end;

procedure TvgCanvasQuartz.FlushBuffer(const X, Y: integer; const DC: Cardinal);
var
  CGR: CGRect;
  ImgRef: CGImageRef;
begin
  if FBuffered then
  begin
    if (CGContextRef(Handle) <> nil) and (DC <> 0) then
    begin
      CGR.origin.x := X;
      CGR.origin.y := -Y + FHeight;
      CGR.size.width := FWidth;
      CGR.size.height := FHeight;
      ImgRef := CGBitmapContextCreateImage(CGContextRef(Handle));
      CGContextDrawImage(CGContextRef(DC), CGR, ImgRef);
      CFRelease(ImgRef);
    end;
  end;
end;

procedure TvgCanvasQuartz.FlushBufferRect(const X, Y: integer;
  const DC: Cardinal; const ARect: TvgRect);
var
  NewR: TvgRect;
  R, SubR: CGRect;
  SubImgRef, ImgRef: CGImageRef;
begin
  if FBuffered and (DC <> 0) and (CGContextRef(Handle) <> nil) then
  begin
    vgIntersectRect(NewR, ARect, vgRect(0, 0, FWidth, FHeight));
    R.origin.x := X + NewR.Left;
    R.origin.y := -(Y + NewR.Bottom);
    R.size.width := vgRectWidth(NewR);
    R.size.height := vgRectHeight(NewR);
    SubR.origin.x := NewR.Left;
    SubR.origin.y := NewR.Top;
    SubR.size.width := vgRectWidth(NewR);
    SubR.size.height := vgRectHeight(NewR);

    ImgRef := CGBitmapContextCreateImage(CGContextRef(Handle));
    SubImgRef := CGImageCreateWithImageInRect(ImgRef, SubR);
    if SubImgRef <> nil then
    begin
      CGContextSaveGState(CGContextRef(DC));
      CGContextScaleCTM(CGContextRef(DC), 1, -1);
      CGContextDrawImage(CGContextRef(DC), R, SubImgRef);
      CFRelease(SubImgRef);
      CGContextRestoreGState(CGContextRef(DC));
    end;
    CFRelease(ImgRef);
  end;
end;

procedure TvgCanvasQuartz.Clear(const Color: cardinal);
begin
  if not FBuffered then
  begin
    CGContextClearRect(CGContextRef(Handle), CGRectFromRect(vgRect(0, 0, FWidth, FHeight)));
  end
  else
    vgFillLongword(FBufferBits, FWidth * FHeight, Color);
end;

procedure TvgCanvasQuartz.ClearRect(const ARect: TvgRect; const AColor: TvgColor);
var
  R: TRect;
begin
  if FBufferBits = nil then Exit;
  R := Rect(Trunc(ARect.Left), Trunc(ARect.Top), Round(ARect.Right), Round(ARect.Bottom));
  if R.Left < 0 then R.Left := 0;
  if R.Top < 0 then R.Top := 0;
  if R.Top < 0 then R.Top := 0;
  if R.Right > FWidth then R.Right := FWidth;
  if R.Bottom > FHeight then R.Bottom := FHeight;
  if R.Bottom < R.Top then R.Bottom := R.Top;
  if R.Right < R.Left then R.Right := R.Left;
  if (R.Right < 0) or (R.Top < 0) or (R.Left > FWidth) or (R.Top > FHeight) then Exit;
  if not FBuffered then
  begin
    CGContextClearRect(CGContextRef(Handle), CGRectFromRect(ARect));
  end
  else
    vgFillLongwordRect(FBufferBits, FWidth, FHeight, R.Left, R.Top, R.Right, R.Bottom, AColor);
end;

class function TvgCanvasQuartz.GetBitmapScanline(Bitmap: TvgBitmap; y: integer): PvgColorArray;
begin
  if (y >= 0) and (y < Bitmap.Height) and (Bitmap.StartLine <> nil) then
    Result := @PvgColorArray(Bitmap.StartLine)[(y) * Bitmap.Width]
  else
    Result := nil;
end;

procedure TvgCanvasQuartz.SetMatrix(const M: TvgMatrix);
var
  IM: TvgMatrix;
  CurM: CGAffineTransform;
begin
  FMatrix := M;
  { restore CTM }
  CurM := CGContextGetCTM(CGContextRef(Handle));
  IM := IdentityMatrix;
  with IM do
  begin
    m11 := CurM.a;
    m12 := CurM.b;
    m21 := CurM.c;
    m22 := CurM.d;
    m31 := CurM.tx;
    m32 := CurM.ty;
  end;
  vgInvertMatrix(IM);

  if CGContextRef(Handle) = nil then Exit;
  with IM do
    CGContextConcatCTM(CGContextRef(Handle), CGAffineTransformMake(m11, m12, m21, m22, m31, m32));

  // Quartz inverse
  if FBitmap <> nil then
    CGContextTranslateCTM(CGContextRef(Handle), 0, FBitmap.Height)
  else
    CGContextTranslateCTM(CGContextRef(Handle), 0, FHeight);
  CGContextScaleCTM(CGContextRef(Handle), 1, -1);
  { Set new }
  with FMatrix do
    CGContextConcatCTM(CGContextRef(Handle), CGAffineTransformMake(m11, m12, m21, m22, m31, m32));
end;

procedure TvgCanvasQuartz.MultyMatrix(const M: TvgMatrix);
var
  IM: TvgMatrix;
  CurM: CGAffineTransform;
begin
  FMatrix := M;
  { restore CTM }
  CurM := CGContextGetCTM(CGContextRef(Handle));
  IM := IdentityMatrix;
  with IM do
  begin
    m11 := CurM.a;
    m12 := CurM.b;
    m21 := CurM.c;
    m22 := CurM.d;
    m31 := CurM.tx;
    m32 := CurM.ty;
  end;
  vgInvertMatrix(IM);

  if CGContextRef(Handle) = nil then Exit;
  with IM do
    CGContextConcatCTM(CGContextRef(Handle), CGAffineTransformMake(m11, m12, m21, m22, m31, m32));
  if not FBuffered and (FBitmap = nil) then
  begin
    CGContextTranslateCTM(CGContextRef(Handle), 0, FHeight);
    CGContextScaleCTM(CGContextRef(Handle), 1, -1);
  end;
  { Set new }
  with FMatrix do
    CGContextConcatCTM(CGContextRef(Handle), CGAffineTransformMake(m11, m12, m21, m22, m31, m32));
end;

function TvgCanvasQuartz.SaveCanvas: cardinal;
var
  i: integer;
begin
  Result := InvalideCanvasState;
  if CGContextRef(Handle) = nil then Exit;

  CGContextSaveGState(CGContextRef(Handle));

  // find exists
  if Length(FSaveData) > 0 then
    for i := 0 to High(FSaveData) do
      if FSaveData[i].Index = InvalideCanvasState then
      begin
        Result := i;
        Break;
      end;
  if Result = InvalideCanvasState then
  begin
    SetLength(FSaveData, Length(FSaveData) + 1);
    Result := High(FSaveData);
  end;
  FSaveData[Result].Index := Result;
  FSaveData[Result].Matrix := FMatrix;
  FSaveData[Result].Stroke := TvgBrush.Create(vgBrushSolid, InvalideCanvasState);
  FSaveData[Result].Stroke.Assign(Stroke);
  FSaveData[Result].Fill := TvgBrush.Create(vgBrushSolid, $FF000000);
  FSaveData[Result].Fill.Assign(Fill);
  FSaveData[Result].StrokeThickness := FStrokeThickness;
  FSaveData[Result].StrokeCap := StrokeCap;
  FSaveData[Result].StrokeJoin := StrokeJoin;
  FSaveData[Result].StrokeDash := StrokeDash;
  if StrokeDash <> vgDashSolid then
  begin
    SetLength(FSaveData[Result].Dash, Length(FDash));
    System.Move(FDash[0], FSaveData[Result].Dash[0], SizeOf(FDash[0]) * Length(FDash));
  end;
  FSaveData[Result].DashOffset := FDashOffset;
  FSaveData[Result].Font := TvgFont.Create;
  FSaveData[Result].Font.Assign(Font);
end;

procedure TvgCanvasQuartz.RestoreCanvas(const AState: cardinal);
begin
  if CGContextRef(Handle) = nil then Exit;

  if (AState >= 0) and (AState < Length(FSaveData)) then
  begin
    CGContextRestoreGState(CGContextRef(Handle));
    FSaveData[AState].Index := InvalideCanvasState;

    FMatrix := FSaveData[AState].Matrix;
    Stroke.Assign(FSaveData[AState].Stroke);
    FSaveData[AState].Stroke.Free;
    Fill.Assign(FSaveData[AState].Fill);
    FSaveData[AState].Fill.Free;
    SetMatrix(FMatrix);
    FStrokeThickness := FSaveData[AState].StrokeThickness;
    FStrokeCap := FSaveData[AState].StrokeCap;
    FStrokeJoin := FSaveData[AState].StrokeJoin;
    FStrokeDash := FSaveData[AState].StrokeDash;
    if StrokeDash <> vgDashSolid then
    begin
      SetLength(FDash, Length(FSaveData[AState].Dash));
      System.Move(FSaveData[AState].Dash[0], FDash[0], SizeOf(FDash[0]) * Length(FDash));
    end;
    FDashOffset := FSaveData[AState].DashOffset;
    Font.Assign(FSaveData[AState].Font);
    FSaveData[AState].Font.Free;
  end
end;

procedure TvgCanvasQuartz.SetClipRects(const ARects: array of TvgRect);
var
  i: integer;
  Rcts: array of CGRect;
begin
  if CGContextRef(Handle) = nil then Exit;
  if not FBuffered then Exit;

  SetLength(Rcts, Length(ARects));
  for i := 0 to High(ARects) do
  begin
    Rcts[i] := CGRectFromRect(ARects[i]);
  end;
  CGContextClipToRects(CGContextRef(Handle), @Rcts[0], Length(Rcts));
end;

procedure TvgCanvasQuartz.IntersectClipRect(const ARect: TvgRect);
begin
  if CGContextRef(Handle) = nil then Exit;
  CGContextClipToRect(CGContextRef(Handle), CGRectFromRect(ARect));
end;

procedure TvgCanvasQuartz.ExcludeClipRect(const ARect: TvgRect);
var
  R: TvgRect;
  RR: array [0..3] of CGRect;
begin
  if CGContextRef(Handle) = nil then Exit;
  R := ARect;
  RR[0] := CGRectFromRect(vgRect(0, 0, R.Left, FHeight));
  RR[1] := CGRectFromRect(vgRect(R.Right, 0, FWidth, FHeight));
  RR[2] := CGRectFromRect(vgRect(R.Left, 0, R.Right, R.Top));
  RR[3] := CGRectFromRect(vgRect(R.Left, R.Bottom, R.Right, FHeight));
  CGContextClipToRects(CGContextRef(Handle), @RR[0], 4);
end;

procedure TvgCanvasQuartz.ResetClipRect;
begin
  if not FBuffered then Exit;
  if CGContextRef(Handle) = nil then Exit;
  if CGContextRef(Handle) <> nil then
    CGContextRelease(CGContextRef(Handle));
  if FBitmap <> nil then
  begin
    UpdateBitmap(FBitmap);
    Handle := FBitmap.Handle;
  end
  else
  begin
    Handle := THandle(CGBitmapContextCreate(FBufferBits, FWidth, FHeight, 8,
      FWidth * 4, CGColorSpaceCreateDeviceRGB, kCGImageAlphaPremultipliedLast));
  end;
end;

var
  ShadeOpacity: single;

procedure myLinearShadingValues(info: Pointer; inp: {const} Float32Ptr; val: Float32Ptr); cdecl;
var
  c: TvgColor;
begin
  if info <> nil then
  begin
    c := vgOpacity(TvgGradient(info).InterpolateColor(inp^), ShadeOpacity);
    val^ := TvgColorRec(c).R / $FF;
    Inc(val);
    val^ := TvgColorRec(c).G / $FF;
    Inc(val);
    val^ := TvgColorRec(c).B / $FF;
    Inc(val);
    val^ := TvgColorRec(c).A / $FF;
    Inc(val);
  end;
end;

procedure TvgCanvasQuartz.ApplyFill(ARect: TvgRect; const AOpacity: single);
begin
  if CGContextRef(Handle) = nil then Exit;

  if (FFill.Style = vgBrushResource) and (FFill.Resource <> nil) and (FFill.Resource.Brush <> nil) then
    FFill.Assign(FFill.Resource.Brush);

  with FFill do
  begin
    case Style of
      vgBrushSolid:
        begin
          with CGColor(SolidColor, AOpacity) do
            CGContextSetRGBFillColor(CGContextRef(Handle), r, g, b, a);
        end;
      vgBrushGradient:
        begin
          Callback.version := 0;
          Callback.evaluate := @myLinearShadingValues;
          Callback.releaseInfo:= nil;
          ShadeOpacity := AOpacity;
          Func := CGFunctionCreate(FFill.Gradient, 1, @inputRange, 4, nil, Callback);
          ColorSpace := CGColorSpaceCreateDeviceRGB();
          Shading := CGShadingCreateAxial(ColorSpace,
            CGPoint(vgPoint(ARect.Left + Gradient.StartPosition.X * ARect.Right, ARect.Top + Gradient.StartPosition.Y * ARect.Bottom)),
            CGPoint(vgPoint(ARect.Left + Gradient.StopPosition.X * ARect.Right, ARect.Top + Gradient.StopPosition.Y * ARect.Bottom)),
            Func,
            1, 1);
        end;
      vgBrushResource:
        begin
        end;
      vgBrushVisual:
        begin
        end;
      vgBrushBitmap:
        begin
          if (Bitmap.Bitmap <> nil) and (Bitmap.Bitmap.Width > 0) and (Bitmap.Bitmap.Height > 0) then
          begin
            UpdateBitmap(Bitmap.Bitmap);
            if (Bitmap.Bitmap.Handle <> 0) then
            begin
              CGContextSetAlpha(CGContextRef(Handle), AOpacity);
              BitmapRef := CGBitmapContextCreateImage(CGContextRef(Bitmap.Bitmap.Handle));
            end;
          end;
        end;
    else
      CGContextSetRGBFillColor(CGContextRef(Handle), 0, 0, 0, 0);
    end;
  end;
end;

procedure TvgCanvasQuartz.DeApplyFill(ARect: TvgRect; const AOpacity: single);
begin
  if CGContextRef(Handle) = nil then Exit;

  with FFill do
  begin
    case Style of
      vgBrushSolid:
        begin
        end;
      vgBrushGradient:
        begin
          CGShadingRelease(Shading);
          CGColorSpaceRelease(ColorSpace);
          CGFunctionRelease(Func);
        end;
      vgBrushResource:
        begin
        end;
      vgBrushVisual:
        begin
        end;
      vgBrushBitmap:
        begin
          CGContextSetAlpha(CGContextRef(Handle), 1);
          CFRelease(BitmapRef);
        end;
    end;
  end;
  Shading := nil;
  BitmapRef := nil;
end;

procedure TvgCanvasQuartz.ApplyStroke(ARect: TvgRect; const AOpacity: single);
var
  i: integer;
  dash: array of single;
begin
  if CGContextRef(Handle) = nil then Exit;

  if (FStroke.Style = vgBrushResource) and (FStroke.Resource <> nil) and (FStroke.Resource.Brush <> nil) then
    FStroke.Assign(FStroke.Resource.Brush);

  with FStroke do
  begin
    case Style of
      vgBrushSolid:
        begin
          with CGColor(SolidColor, AOpacity) do
            CGContextSetRGBStrokeColor(CGContextRef(Handle), r, g, b, a);
        end;
      vgBrushGradient:
        begin
        end;
      vgBrushBitmap:
        begin
{          if (Bitmap.Bitmap <> nil) and (Bitmap.Bitmap.Width > 0) and (Bitmap.Bitmap.Height > 0) then
          begin
            UpdateBitmap(Bitmap.Bitmap);
            if (Bitmap.Bitmap.Handle <> 0) then
            begin
              if Bitmap.WrapMode <> vgWrapTileStretch then
                FGPPenBrush := TGPTextureBrush.Create(TGPBitmap(Bitmap.Bitmap.Handle), TWrapMode(Bitmap.WrapMode))
              else
              begin
                FGPPenBrush := TGPTextureBrush.Create(TGPBitmap(Bitmap.Bitmap.Handle), WrapModeClamp);
                TGPTextureBrush(FGPPenBrush).ScaleTransform(vgRectWidth(ARect) / Bitmap.Bitmap.Width, vgRectHeight(ARect) / Bitmap.Bitmap.Height);
              end;
            end
            else
              FGPPenBrush := TGPSolidBrush.Create($00000000);
          end
          else
            FGPPenBrush := TGPSolidBrush.Create($00000000);}
        end;
    else
      CGContextSetRGBStrokeColor(CGContextRef(Handle), 0, 0, 0, 0);
    end;
  end;
  case StrokeCap of
    vgCapFlat: CGContextSetLineCap(CGContextRef(Handle), kCGLineCapButt);
    vgCapRound: CGContextSetLineCap(CGContextRef(Handle), kCGLineCapRound);
  end;
  if Length(FDash) > 0 then
  begin
    SetLength(dash, Length(FDash));
    for i := 0 to High(FDash) do
    begin
      dash[i] := FDash[i] * StrokeThickness;
      if (StrokeCap = vgCapRound) then
      begin
        if odd(i) then
          dash[i] := (FDash[i] + 1) * StrokeThickness
        else
          dash[i] := (FDash[i] - 1) * StrokeThickness;
      end;
    end;
    CGContextSetLineDash(CGContextRef(Handle), FDashOffset, @dash[0], Length(FDash));
  end
  else
    CGContextSetLineDash(CGContextRef(Handle), 0, nil, 0);
  case StrokeJoin of
    vgJoinMiter: CGContextSetLineJoin(CGContextRef(Handle), kCGLineJoinMiter);
    vgJoinRound: CGContextSetLineJoin(CGContextRef(Handle), kCGLineJoinRound);
    vgJoinBevel: CGContextSetLineJoin(CGContextRef(Handle), kCGLineJoinBevel);
  end;
  CGContextSetLineWidth(CGContextRef(Handle), StrokeThickness);
end;

procedure TvgCanvasQuartz.FontChanged(Sender: TObject);
begin
end;

procedure TvgCanvasQuartz.DrawLine(const APt1, APt2: TvgPoint; const AOpacity: single);
begin
  if CGContextRef(Handle) = nil then Exit;
  if FStroke.Style <> vgBrushNone then
  begin
    ApplyStroke(vgRect(APt1.X, APt1.Y, APt2.X, APt2.Y), AOpacity);
    CGContextBeginPath(CGContextRef(Handle));
    CGContextMoveToPoint(CGContextRef(Handle), APt1.X, APt1.Y);
    CGContextAddLineToPoint(CGContextRef(Handle), APt2.X, APt2.Y);
    CGContextClosePath(CGContextRef(Handle));
    CGContextStrokePath(CGContextRef(Handle));
  end;
end;

procedure TvgCanvasQuartz.DrawRect(const ARect: TvgRect;
  const xRadius, yRadius: single; const ACorners: TvgCorners; const AOpacity: single;
  const ACornerType: TvgCornerType = vgCornerRound);
var
  x1, x2, y1, y2: single;
  R: TvgRect;
begin
  if CGContextRef(Handle) = nil then Exit;
  if FStroke.Style <> vgBrushNone then
  begin
    ApplyStroke(ARect, AOpacity);
    if (xRadius < Epsilon) and (yRadius < Epsilon) then
    begin
      CGContextStrokeRect(CGContextRef(Handle), CGRectFromRect(ARect));
    end
    else
    begin
      R := ARect;
      x1 := xRadius;
      if vgRectWidth(R) - (x1 * 2) < 0 then
        x1 := vgRectWidth(R) / 2;
      x2 := xRadius * CurveKappaInv;
      y1 := yRadius;
      if vgRectHeight(R) - (y1 * 2) < 0 then
        y1 := vgRectHeight(R) / 2;
      y2 := yRadius * CurveKappaInv;
      CGContextBeginPath(CGContextRef(Handle));
      CGContextMoveToPoint(CGContextRef(Handle), R.Left, R.Top + y1);
      if vgCornerTopLeft in ACorners then
      begin
        case ACornerType of
          // vgCornetRound - default
          vgCornerBevel: CGContextAddLineToPoint(CGContextRef(Handle), R.Left + x1, R.Top);
          vgCornerInnerRound: CGContextAddCurveToPoint(CGContextRef(Handle), R.Left + x2, R.Top + y1, R.Left + x1, R.Top + y2, R.Left + x1, R.Top);
          vgCornerInnerLine:
            begin
              CGContextAddLineToPoint(CGContextRef(Handle), R.Left + x2, R.Top + y1);
              CGContextAddLineToPoint(CGContextRef(Handle), R.Left + x1, R.Top + y2);
              CGContextAddLineToPoint(CGContextRef(Handle), R.Left + x1, R.Top);
            end;
        else
          CGContextAddCurveToPoint(CGContextRef(Handle), R.Left, R.Top + (y2), R.Left + x2, R.Top, R.Left + x1, R.Top)
        end;
      end
      else
      begin
        CGContextAddLineToPoint(CGContextRef(Handle), R.Left, R.Top);
        CGContextAddLineToPoint(CGContextRef(Handle), R.Left + x1, R.Top);
      end;
      CGContextAddLineToPoint(CGContextRef(Handle), R.Right - x1, R.Top);
      if vgCornerTopRight in ACorners then
      begin
        case ACornerType of
          // vgCornetRound - default
          vgCornerBevel: CGContextAddLineToPoint(CGContextRef(Handle), R.Right, R.Top + y1);
          vgCornerInnerRound: CGContextAddCurveToPoint(CGContextRef(Handle), R.Right - x1, R.Top + y2, R.Right - x2, R.Top + y1, R.Right, R.Top + y1);
          vgCornerInnerLine:
            begin
              CGContextAddLineToPoint(CGContextRef(Handle), R.Right - x1, R.Top + y2);
              CGContextAddLineToPoint(CGContextRef(Handle), R.Right - x2, R.Top + y1);
              CGContextAddLineToPoint(CGContextRef(Handle), R.Right, R.Top + y1);
            end;
        else
          CGContextAddCurveToPoint(CGContextRef(Handle), R.Right - x2, R.Top, R.Right, R.Top + (y2), R.Right, R.Top + y1)
        end;
      end
      else
      begin
        CGContextAddLineToPoint(CGContextRef(Handle), R.Right, R.Top);
        CGContextAddLineToPoint(CGContextRef(Handle), R.Right, R.Top + y1);
      end;
      CGContextAddLineToPoint(CGContextRef(Handle), R.Right, R.Bottom - y1);
      if vgCornerBottomRight in ACorners then
      begin
        case ACornerType of
          // vgCornetRound - default
          vgCornerBevel: CGContextAddLineToPoint(CGContextRef(Handle), R.Right - x1, R.Bottom);
          vgCornerInnerRound: CGContextAddCurveToPoint(CGContextRef(Handle), R.Right - x2, R.Bottom - y1, R.Right - x1, R.Bottom - y2, R.Right - x1, R.Bottom);
          vgCornerInnerLine:
            begin
              CGContextAddLineToPoint(CGContextRef(Handle), R.Right - x2, R.Bottom - y1);
              CGContextAddLineToPoint(CGContextRef(Handle), R.Right - x1, R.Bottom - y2);
              CGContextAddLineToPoint(CGContextRef(Handle), R.Right - x1, R.Bottom);
            end;
        else
          CGContextAddCurveToPoint(CGContextRef(Handle), R.Right, R.Bottom - (y2), R.Right - x2, R.Bottom, R.Right - x1, R.Bottom)
        end;
      end
      else
      begin
        CGContextAddLineToPoint(CGContextRef(Handle), R.Right, R.Bottom);
        CGContextAddLineToPoint(CGContextRef(Handle), R.Right - x1, R.Bottom);
      end;
      CGContextAddLineToPoint(CGContextRef(Handle), R.Left + x1, R.Bottom);
      if vgCornerBottomLeft in ACorners then
      begin
        case ACornerType of
          // vgCornetRound - default
          vgCornerBevel: CGContextAddLineToPoint(CGContextRef(Handle), R.Left, R.Bottom - y1);
          vgCornerInnerRound: CGContextAddCurveToPoint(CGContextRef(Handle), R.Left + x1, R.Bottom - y2, R.Left + x2, R.Bottom - y1, R.Left, R.Bottom - y1);
          vgCornerInnerLine:
            begin
              CGContextAddLineToPoint(CGContextRef(Handle), R.Left + x1, R.Bottom - y2);
              CGContextAddLineToPoint(CGContextRef(Handle), R.Left + x2, R.Bottom - y1);
              CGContextAddLineToPoint(CGContextRef(Handle), R.Left, R.Bottom - y1);
            end;
        else
          CGContextAddCurveToPoint(CGContextRef(Handle), R.Left + x2, R.Bottom, R.Left, R.Bottom - (y2), R.Left, R.Bottom - y1)
        end;
      end
      else
      begin
        CGContextAddLineToPoint(CGContextRef(Handle), R.Left, R.Bottom);
        CGContextAddLineToPoint(CGContextRef(Handle), R.Left, R.Bottom - y1);
      end;
      CGContextClosePath(CGContextRef(Handle));
      CGContextStrokePath(CGContextRef(Handle));
    end;
  end;
end;

procedure TvgCanvasQuartz.FillRect(const ARect: TvgRect; const xRadius, yRadius: single; const ACorners: TvgCorners; const AOpacity: single;
  const ACornerType: TvgCornerType = vgCornerRound);
var
  x1, x2, y1, y2: single;
  R: TvgRect;
begin
  if CGContextRef(Handle) = nil then Exit;
  if FFill.Style <> vgBrushNone then
  begin
    CGContextSaveGState(CGContextRef(Handle));
    ApplyFill(ARect, AOpacity);

    CGContextBeginPath(CGContextRef(Handle));
    if (xRadius < Epsilon) and (yRadius < Epsilon) then
    begin
      CGContextAddRect(CGContextRef(Handle), CGRectFromRect(ARect));
    end
    else
    begin
      R := ARect;
      x1 := xRadius;
      if vgRectWidth(R) - (x1 * 2) < 0 then
        x1 := vgRectWidth(R) / 2;
      x2 := xRadius * CurveKappaInv;
      y1 := yRadius;
      if vgRectHeight(R) - (y1 * 2) < 0 then
        y1 := vgRectHeight(R) / 2;
      y2 := yRadius * CurveKappaInv;
      CGContextMoveToPoint(CGContextRef(Handle), R.Left, R.Top + y1);
      if vgCornerTopLeft in ACorners then
      begin
        case ACornerType of
          // vgCornetRound - default
          vgCornerBevel: CGContextAddLineToPoint(CGContextRef(Handle), R.Left + x1, R.Top);
          vgCornerInnerRound: CGContextAddCurveToPoint(CGContextRef(Handle), R.Left + x2, R.Top + y1, R.Left + x1, R.Top + y2, R.Left + x1, R.Top);
          vgCornerInnerLine:
            begin
              CGContextAddLineToPoint(CGContextRef(Handle), R.Left + x2, R.Top + y1);
              CGContextAddLineToPoint(CGContextRef(Handle), R.Left + x1, R.Top + y2);
              CGContextAddLineToPoint(CGContextRef(Handle), R.Left + x1, R.Top);
            end;
        else
          CGContextAddCurveToPoint(CGContextRef(Handle), R.Left, R.Top + (y2), R.Left + x2, R.Top, R.Left + x1, R.Top)
        end;
      end
      else
      begin
        CGContextAddLineToPoint(CGContextRef(Handle), R.Left, R.Top);
        CGContextAddLineToPoint(CGContextRef(Handle), R.Left + x1, R.Top);
      end;
      CGContextAddLineToPoint(CGContextRef(Handle), R.Right - x1, R.Top);
      if vgCornerTopRight in ACorners then
      begin
        case ACornerType of
          // vgCornetRound - default
          vgCornerBevel: CGContextAddLineToPoint(CGContextRef(Handle), R.Right, R.Top + y1);
          vgCornerInnerRound: CGContextAddCurveToPoint(CGContextRef(Handle), R.Right - x1, R.Top + y2, R.Right - x2, R.Top + y1, R.Right, R.Top + y1);
          vgCornerInnerLine:
            begin
              CGContextAddLineToPoint(CGContextRef(Handle), R.Right - x1, R.Top + y2);
              CGContextAddLineToPoint(CGContextRef(Handle), R.Right - x2, R.Top + y1);
              CGContextAddLineToPoint(CGContextRef(Handle), R.Right, R.Top + y1);
            end;
        else
          CGContextAddCurveToPoint(CGContextRef(Handle), R.Right - x2, R.Top, R.Right, R.Top + (y2), R.Right, R.Top + y1)
        end;
      end
      else
      begin
        CGContextAddLineToPoint(CGContextRef(Handle), R.Right, R.Top);
        CGContextAddLineToPoint(CGContextRef(Handle), R.Right, R.Top + y1);
      end;
      CGContextAddLineToPoint(CGContextRef(Handle), R.Right, R.Bottom - y1);
      if vgCornerBottomRight in ACorners then
      begin
        case ACornerType of
          // vgCornetRound - default
          vgCornerBevel: CGContextAddLineToPoint(CGContextRef(Handle), R.Right - x1, R.Bottom);
          vgCornerInnerRound: CGContextAddCurveToPoint(CGContextRef(Handle), R.Right - x2, R.Bottom - y1, R.Right - x1, R.Bottom - y2, R.Right - x1, R.Bottom);
          vgCornerInnerLine:
            begin
              CGContextAddLineToPoint(CGContextRef(Handle), R.Right - x2, R.Bottom - y1);
              CGContextAddLineToPoint(CGContextRef(Handle), R.Right - x1, R.Bottom - y2);
              CGContextAddLineToPoint(CGContextRef(Handle), R.Right - x1, R.Bottom);
            end;
        else
          CGContextAddCurveToPoint(CGContextRef(Handle), R.Right, R.Bottom - (y2), R.Right - x2, R.Bottom, R.Right - x1, R.Bottom)
        end;
      end
      else
      begin
        CGContextAddLineToPoint(CGContextRef(Handle), R.Right, R.Bottom);
        CGContextAddLineToPoint(CGContextRef(Handle), R.Right - x1, R.Bottom);
      end;
      CGContextAddLineToPoint(CGContextRef(Handle), R.Left + x1, R.Bottom);
      if vgCornerBottomLeft in ACorners then
      begin
        case ACornerType of
          // vgCornetRound - default
          vgCornerBevel: CGContextAddLineToPoint(CGContextRef(Handle), R.Left, R.Bottom - y1);
          vgCornerInnerRound: CGContextAddCurveToPoint(CGContextRef(Handle), R.Left + x1, R.Bottom - y2, R.Left + x2, R.Bottom - y1, R.Left, R.Bottom - y1);
          vgCornerInnerLine:
            begin
              CGContextAddLineToPoint(CGContextRef(Handle), R.Left + x1, R.Bottom - y2);
              CGContextAddLineToPoint(CGContextRef(Handle), R.Left + x2, R.Bottom - y1);
              CGContextAddLineToPoint(CGContextRef(Handle), R.Left, R.Bottom - y1);
            end;
        else
          CGContextAddCurveToPoint(CGContextRef(Handle), R.Left + x2, R.Bottom, R.Left, R.Bottom - (y2), R.Left, R.Bottom - y1)
        end;
      end
      else
      begin
        CGContextAddLineToPoint(CGContextRef(Handle), R.Left, R.Bottom);
        CGContextAddLineToPoint(CGContextRef(Handle), R.Left, R.Bottom - y1);
      end;
    end;
    CGContextClosePath(CGContextRef(Handle));

    if (BitmapRef <> nil) and (FFill.Bitmap.Bitmap.Handle <> 0) then
    begin
      CGContextClip(CGContextRef(Handle));
      case FFill.Bitmap.WrapMode of
        vgWrapTile:
          begin
            CGContextScaleCTM(CGContextRef(Handle), 1, -1);
            CGContextDrawTiledImage(CGContextRef(Handle), ARect, BitmapRef, FFill.Bitmap.Bitmap);
          end;
        vgWrapTileOriginal:
          begin
            CGContextScaleCTM(CGContextRef(Handle), 1, -1);
            CGContextDrawImage(CGContextRef(Handle), CGRectFromRect(vgRect(ARect.Left, ARect.Top, ARect.Left + FFill.Bitmap.Bitmap.Width, -ARect.Top - FFill.Bitmap.Bitmap.Height)), BitmapRef);
          end;
        vgWrapTileStretch:
          begin
            CGContextScaleCTM(CGContextRef(Handle), 1, -1);
            CGContextDrawImage(CGContextRef(Handle), CGRectFromRect(vgRect(ARect.Left, ARect.Top, ARect.Right, -ARect.Bottom)), BitmapRef);
          end;
      end;
    end
    else
    if Shading <> nil then
    begin
      CGContextClip(CGContextRef(Handle));
      CGContextDrawShading(CGContextRef(Handle), Shading)
    end
    else
      CGContextFillPath(CGContextRef(Handle));

    DeApplyFill(ARect, AOpacity);
    CGContextRestoreGState(CGContextRef(Handle));
  end;
end;

procedure TvgCanvasQuartz.DrawEllipse(const ARect: TvgRect; const AOpacity: single);
begin
  if CGContextRef(Handle) = nil then Exit;
  if FStroke.Style <> vgBrushNone then
  begin
    ApplyStroke(ARect, AOpacity);
    CGContextStrokeEllipseInRect(CGContextRef(Handle), CGRectFromRect(ARect));
  end;
end;

procedure TvgCanvasQuartz.FillEllipse(const ARect: TvgRect; const AOpacity: single);
begin
  if CGContextRef(Handle) = nil then Exit;
  if FFill.Style <> vgBrushNone then
  begin
    CGContextSaveGState(CGContextRef(Handle));
    ApplyFill(ARect, AOpacity);

    CGContextBeginPath(CGContextRef(Handle));
    CGContextAddEllipseInRect(CGContextRef(Handle), CGRectFromRect(ARect));
    CGContextClosePath(CGContextRef(Handle));

    if (BitmapRef <> nil) and (FFill.Bitmap.Bitmap.Handle <> 0) then
    begin
      CGContextClip(CGContextRef(Handle));
      case FFill.Bitmap.WrapMode of
        vgWrapTile:
          begin
            CGContextScaleCTM(CGContextRef(Handle), 1, -1);
            CGContextDrawTiledImage(CGContextRef(Handle), ARect, BitmapRef, FFill.Bitmap.Bitmap);
          end;
        vgWrapTileOriginal:
          begin
            CGContextScaleCTM(CGContextRef(Handle), 1, -1);
            CGContextDrawImage(CGContextRef(Handle), CGRectFromRect(vgRect(ARect.Left, ARect.Top, ARect.Left + FFill.Bitmap.Bitmap.Width, -ARect.Top - FFill.Bitmap.Bitmap.Height)), BitmapRef);
          end;
        vgWrapTileStretch:
          begin
            CGContextScaleCTM(CGContextRef(Handle), 1, -1);
            CGContextDrawImage(CGContextRef(Handle), CGRectFromRect(vgRect(ARect.Left, ARect.Top, ARect.Right, -ARect.Bottom)), BitmapRef);
          end;
      end;
    end
    else
    if Shading <> nil then
    begin
      CGContextClip(CGContextRef(Handle));
      CGContextDrawShading(CGContextRef(Handle), Shading)
    end
    else
      CGContextFillPath(CGContextRef(Handle));

    DeApplyFill(ARect, AOpacity);
    CGContextRestoreGState(CGContextRef(Handle));
  end;
end;

{ Bitmaps }

procedure TvgCanvasQuartz.DrawBitmap(const ABitmap: TvgBitmap;
  const SrcRect, DstRect: TvgRect; const AOpacity: single; const HighSpeed: boolean = false);
var
  R, SubR: CGRect;
  ImgRef, SubImgRef: CGImageRef;
begin
  if CGContextRef(Handle) = nil then Exit;
  if ABitmap = nil then Exit;

  UpdateBitmap(ABitmap);
  if (ABitmap.Handle <> 0) then
  begin
    R := CGRectFromRect(DstRect);
    if (SrcRect.Left = 0) and (SrcRect.Top = 0) and (SrcRect.Right = ABitmap.Width) and (SrcRect.Bottom = ABitmap.Height) then
    begin
      ImgRef := CGBitmapContextCreateImage(CGContextRef(ABitmap.Handle));
      if ImgRef <> nil then
      begin
        CGContextSaveGState(CGContextRef(Handle));
        CGContextSetAlpha(CGContextRef(Handle), AOpacity);

        if HighSpeed then
          CGContextSetInterpolationQuality(CGContextRef(Handle), kCGInterpolationNone)
        else
          CGContextSetInterpolationQuality(CGContextRef(Handle), kCGInterpolationDefault);

        // flip
        R.origin.y := -DstRect.Bottom;
        CGContextScaleCTM(CGContextRef(Handle), 1, -1);
        //

        CGContextDrawImage(CGContextRef(Handle), R, ImgRef);

        CGContextRestoreGState(CGContextRef(Handle));

        CGImageRelease(ImgRef);
      end;
    end
    else
    begin
      SubR := CGRectFromRect(SrcRect);

      ImgRef := CGBitmapContextCreateImage(CGContextRef(ABitmap.Handle));
      if ImgRef <> nil then
      begin
        SubImgRef := CGImageCreateWithImageInRect(ImgRef, SubR);
        if SubImgRef <> nil then
        begin
          CGContextSaveGState(CGContextRef(Handle));
          CGContextSetAlpha(CGContextRef(Handle), AOpacity);
          if HighSpeed then
            CGContextSetInterpolationQuality(CGContextRef(Handle), kCGInterpolationNone)
          else
            CGContextSetInterpolationQuality(CGContextRef(Handle), kCGInterpolationDefault);

          // flip
          R.origin.y := -DstRect.Bottom;
          CGContextScaleCTM(CGContextRef(Handle), 1, -1);
          //
          CGContextDrawImage(CGContextRef(Handle), R, SubImgRef);
          CGImageRelease(SubImgRef);
          CGContextRestoreGState(CGContextRef(Handle));
        end;
        CGImageRelease(ImgRef);
      end;
    end;
  end;
end;

procedure TvgCanvasQuartz.UpdateBitmap(ABitmap: TvgBitmap);
var
  colorspace: CGColorSpaceRef;
begin
  { update bitmap to Quartz bitmap }
  if ABitmap = nil then Exit;
  { create - if need }
  if ABitmap.Handle = 0 then
  begin
    colorspace := CGColorSpaceCreateDeviceRGB;
    ABitmap.Handle := Cardinal(CGBitmapContextCreate(ABitmap.StartLine, ABitmap.Width, ABitmap.Height, 8,
      ABitmap.Width * 4, colorspace, kCGImageAlphaPremultipliedLast));
    CGColorSpaceRelease(colorspace);
  end;
  { clear flag }
  ABitmap.NeedUpdate := false;
  ABitmap.OnDestroyHandle := DoDestroyBitmap;
end;

procedure TvgCanvasQuartz.DoDestroyBitmap(Sender: TObject);
begin
  if (Sender <> nil) then
  begin
    if (TvgBitmap(Sender).Handle <> 0) then
      CGContextRelease(CGContextRef(TvgBitmap(Sender).Handle));
    TvgBitmap(Sender).Handle := 0;
  end;
end;
{ Path }

procedure TvgCanvasQuartz.DrawPath(const APath: TvgPathData; const ARect: TvgRect; const AOpacity: single);
var
  i: integer;
  B: TvgRect;
  W, H, NewW, NewH: single;
  CP1, CP2: TvgPoint;
begin
  if CGContextRef(Handle) = nil then Exit;
  if FStroke.Style = vgBrushNone then Exit;
  if APath.IsEmpty then Exit;
  ApplyStroke(ARect, AOpacity);
  B := APath.GetBounds;
  W := vgRectWidth(B);
  H := vgRectHeight(B);
  NewW := vgRectWidth(ARect);
  NewH := vgRectHeight(ARect);
  { draw }
  CGContextSaveGState(CGContextRef(Handle));
  CGContextBeginPath(CGContextRef(Handle));
  i := 0;
  while i < Length(APath.PathData) do
  begin
    case APath.PathData[i].Kind of
      vgPathPointMoveTo:
        begin
          CGContextMoveToPoint(CGContextRef(Handle), ARect.Left + (APath.PathData[i].Point.X - B.Left) / W * NewW,
            ARect.Top + (APath.PathData[i].Point.Y - B.Top) / H * NewH);
        end;
      vgPathPointLineTo:
        begin
          CGContextAddLineToPoint(CGContextRef(Handle), ARect.Left + (APath.PathData[i].Point.X - B.Left) / W * NewW,
            ARect.Top + (APath.PathData[i].Point.Y - B.Top) / H * NewH);
        end;
      vgPathPointCurveTo:
        begin
          CP1 := vgPoint(ARect.Left + (APath.PathData[i].Point.X - B.Left) / W * NewW,
            ARect.Top + (APath.PathData[i].Point.Y - B.Top) / H * NewH);
          Inc(i);
          CP2 := vgPoint(ARect.Left + (APath.PathData[i].Point.X - B.Left) / W * NewW,
            ARect.Top + (APath.PathData[i].Point.Y - B.Top) / H * NewH);
          Inc(i);
          CGContextAddCurveToPoint(CGContextRef(Handle), CP1.X,
            CP1.Y,
            CP2.X,
            CP2.Y,
            ARect.Left + (APath.PathData[i].Point.X - B.Left) / W * NewW,
            ARect.Top + (APath.PathData[i].Point.Y - B.Top) / H * NewH);
        end;
      vgPathPointClose:
        begin
          CGContextClosePath(CGContextRef(Handle));
        end;
    end;
    inc(i);
  end;
  CGContextStrokePath(CGContextRef(Handle));
  CGContextRestoreGState(CGContextRef(Handle));
end;

procedure TvgCanvasQuartz.FillPath(const APath: TvgPathData; const ARect: TvgRect; const AOpacity: single);
var
  i: integer;
  B: TvgRect;
  W, H, NewW, NewH: single;
  CP1, CP2: TvgPoint;
begin
  if CGContextRef(Handle) = nil then Exit;
  if FFill.Style = vgBrushNone then Exit;
  if APath.IsEmpty then Exit;

  B := APath.GetBounds;
  W := vgRectWidth(B);
  H := vgRectHeight(B);
  NewW := vgRectWidth(ARect);
  NewH := vgRectHeight(ARect);

  ApplyFill(ARect, AOpacity);
  CGContextSaveGState(CGContextRef(Handle));
  i := 0;
  while i < Length(APath.PathData) do
  begin
    case APath.PathData[i].Kind of
      vgPathPointMoveTo:
        begin
          CGContextMoveToPoint(CGContextRef(Handle), ARect.Left + (APath.PathData[i].Point.X - B.Left) / W * NewW,
            ARect.Top + (APath.PathData[i].Point.Y - B.Top) / H * NewH);
        end;
      vgPathPointLineTo:
        begin
          CGContextAddLineToPoint(CGContextRef(Handle), ARect.Left + (APath.PathData[i].Point.X - B.Left) / W * NewW,
            ARect.Top + (APath.PathData[i].Point.Y - B.Top) / H * NewH);
        end;
      vgPathPointCurveTo:
        begin
          CP1 := vgPoint(ARect.Left + (APath.PathData[i].Point.X - B.Left) / W * NewW,
            ARect.Top + (APath.PathData[i].Point.Y - B.Top) / H * NewH);
          Inc(i);
          CP2 := vgPoint(ARect.Left + (APath.PathData[i].Point.X - B.Left) / W * NewW,
            ARect.Top + (APath.PathData[i].Point.Y - B.Top) / H * NewH);
          Inc(i);
          CGContextAddCurveToPoint(CGContextRef(Handle), CP1.X,
            CP1.Y,
            CP2.X,
            CP2.Y,
            ARect.Left + (APath.PathData[i].Point.X - B.Left) / W * NewW,
            ARect.Top + (APath.PathData[i].Point.Y - B.Top) / H * NewH);
        end;
      vgPathPointClose:
        begin
          CGContextClosePath(CGContextRef(Handle));
        end;
    end;
    inc(i);
  end;

  if (BitmapRef <> nil) and (FFill.Bitmap.Bitmap.Handle <> 0) then
  begin
    CGContextClip(CGContextRef(Handle));
    case FFill.Bitmap.WrapMode of
        vgWrapTile:
          begin
            CGContextScaleCTM(CGContextRef(Handle), 1, -1);
            CGContextDrawTiledImage(CGContextRef(Handle), ARect, BitmapRef, FFill.Bitmap.Bitmap);
          end;
        vgWrapTileOriginal:
          begin
            CGContextScaleCTM(CGContextRef(Handle), 1, -1);
            CGContextDrawImage(CGContextRef(Handle), CGRectFromRect(vgRect(ARect.Left, ARect.Top, ARect.Left + FFill.Bitmap.Bitmap.Width, -ARect.Top - FFill.Bitmap.Bitmap.Height)), BitmapRef);
          end;
        vgWrapTileStretch:
          begin
            CGContextScaleCTM(CGContextRef(Handle), 1, -1);
            CGContextDrawImage(CGContextRef(Handle), CGRectFromRect(vgRect(ARect.Left, ARect.Top, ARect.Right, -ARect.Bottom)), BitmapRef);
          end;
    end;
  end
  else
  if Shading <> nil then
  begin
    CGContextClip(CGContextRef(Handle));
    CGContextDrawShading(CGContextRef(Handle), Shading)
  end
  else
    CGContextFillPath(CGContextRef(Handle));

  DeApplyFill(ARect, AOpacity);
  CGContextRestoreGState(CGContextRef(Handle));
end;

function TvgCanvasQuartz.PtInPath(const APoint: TvgPoint; const ARect: TvgRect; const APath: TvgPathData): boolean;
var
  i: integer;
  B: TvgRect;
  W, H, NewW, NewH: single;
  CP1, CP2: TvgPoint;
begin
  Result := false;
  if CGContextRef(Handle) = nil then Exit;
  if APath.IsEmpty then Exit;
  B := APath.GetBounds;
  W := vgRectWidth(B);
  H := vgRectHeight(B);
  NewW := vgRectWidth(ARect);
  NewH := vgRectHeight(ARect);
  { draw }
  CGContextSaveGState(CGContextRef(Handle));
  CGContextTranslateCTM(CGContextRef(Handle), ARect.Left, ARect.Top);
  CGContextBeginPath(CGContextRef(Handle));
  i := 0;
  while i < Length(APath.PathData) do
  begin
    case APath.PathData[i].Kind of
      vgPathPointMoveTo:
        begin
          CGContextMoveToPoint(CGContextRef(Handle), ARect.Left + (APath.PathData[i].Point.X - B.Left) / W * NewW,
            ARect.Top + (APath.PathData[i].Point.Y - B.Top) / H * NewH);
        end;
      vgPathPointLineTo:
        begin
          CGContextAddLineToPoint(CGContextRef(Handle), ARect.Left + (APath.PathData[i].Point.X - B.Left) / W * NewW,
            ARect.Top + (APath.PathData[i].Point.Y - B.Top) / H * NewH);
        end;
      vgPathPointCurveTo:
        begin
          CP1 := vgPoint(ARect.Left + (APath.PathData[i].Point.X - B.Left) / W * NewW,
            ARect.Top + (APath.PathData[i].Point.Y - B.Top) / H * NewH);
          Inc(i);
          CP2 := vgPoint(ARect.Left + (APath.PathData[i].Point.X - B.Left) / W * NewW,
            ARect.Top + (APath.PathData[i].Point.Y - B.Top) / H * NewH);
          Inc(i);
          CGContextAddCurveToPoint(CGContextRef(Handle), CP1.X,
            CP1.Y,
            CP2.X,
            CP2.Y,
            ARect.Left + (APath.PathData[i].Point.X - B.Left) / W * NewW,
            ARect.Top + (APath.PathData[i].Point.Y - B.Top) / H * NewH);
        end;
      vgPathPointClose:
        begin
          CGContextClosePath(CGContextRef(Handle));
        end;
    end;
    inc(i);
  end;
  CGContextRestoreGState(CGContextRef(Handle));
  Result := CGContextPathContainsPoint(CGContextRef(Handle), CGPoint(APoint), kCGPathFillStroke) > 0;
end;

procedure TvgCanvasQuartz.FillText(const ARect, AClipRect: TvgRect; const AText: WideString; const WordWrap: boolean;
  const AOpacity: single; const ATextAlign: TvgTextAlign; const AVTextAlign: TvgTextAlign = vgTextAlignCenter);
var
  TextR: TvgRect;
  Values: array [0..10] of ATSUAttributeValuePtr;
  Tags: array [0..10] of ATSUAttributeTag;
  Sizes: array [0..10] of ByteCount;
  myStyles: ATSUStyle;
  myTextLayout: ATSUTextLayout;
  myFont: ATSUFontID;
  atsuSize: Fixed;
  atsuItalic, atsuBold: boolean;
  lineTrunc: ATSULineTruncation;
  i, lineWidth: cardinal;
  textAlign: fract;
  CLeft, CRight, CTop, CBottom: Fixed;
  numSoftBreaks: integer;
  currentStart, currentEnd, layoutStart, layoutLength: UniCharArrayOffset;
  softBreaks: array [0..100] of UniCharArrayOffset;
  HorizontalOffset, VerticalOffset: single;
  S: String;
begin
  if CGContextRef(Handle) = nil then Exit;
  if FFill.Style = vgBrushNone then Exit;
  if Length(AText) = 0 then Exit;

{  TextR := ARect;
  MeasureText(TextR, ARect, AText, WordWrap, ATextAlign, AVTextAlign);
  Stroke.Style := vgBrushSolid;
  Stroke.SolidColor := $A0FF0000;
  DrawRect(TextR, 2, 2, AllCorners, 1);}

  ApplyFill(ARect, AOpacity);
  { Create ATSU }
  ATSUCreateStyle(myStyles);
  // font
  S := Font.Family;
  if ATSUFindFontFromName(@S[1], Length(S), kFontFamilyName, kFontNoPlatform, kFontNoScript, kFontNoLanguage, myFont) = noErr then
  begin
    Tags[0] := kATSUFontTag;
    Sizes[0] := sizeof(ATSUFontId);
    Values[0] := @myFont;
    ATSUSetAttributes(myStyles, 1, Tags, Sizes, Values);
  end;
  // style
  atsuSize := Round(Font.Size * $FFFF);
  atsuBold := Font.Style in [vgFontBold, vgFontBoldItalic];
  atsuItalic := Font.Style in [vgFontItalic, vgFontBoldItalic];

  Tags[0] := kATSUSizeTag;
  Sizes[0] := sizeof(Fixed);
  Values[0] := @atsuSize;
  Tags[1] := kATSUQDBoldFaceTag;
  Sizes[1] := sizeof(boolean);
  Values[1] := @atsuBold;
  Tags[2] := kATSUQDItalicTag;
  Sizes[2] := sizeof(boolean);
  Values[2] := @atsuItalic;
  ATSUSetAttributes(myStyles, 3, Tags, Sizes, Values);
  // layout
  myTextLayout := nil;
  ATSUCreateTextLayout(myTextLayout);
  // set context
  Tags[0] := kATSUCGContextTag;
  Sizes[0] := SizeOf(CGContextRef);
  Values[0] := @CGContextRef(Handle);
  // set align
  Tags[1] := kATSULineFlushFactorTag;
  Sizes[1] := SizeOf(textAlign);
  if WordWrap then
  begin
    case ATextAlign of
      vgTextAlignCenter:
        begin
          textAlign := kATSUCenterAlignment;
        end;
      vgTextAlignNear:
        begin
          textAlign := kATSUStartAlignment;
        end;
      vgTextAlignFar:
        begin
          textAlign := kATSUEndAlignment;
        end;
    end;
  end
  else
    textAlign := kATSUStartAlignment;
  Values[1] := @textAlign;
  // line width
  if WordWrap then
    lineWidth := Trunc(vgRectWidth(ARect) * $FFFF)
  else
    lineWidth := 5000 * $FFFF;
  if lineWidth <= 0 then lineWidth := 1;
  Tags[2] := kATSULineWidthTag;
  Sizes[2] := SizeOf(lineWidth);
  Values[2] := @lineWidth;
  // line trunk - set ...
  lineTrunc := kATSUTruncateNone;
  Tags[3] := kATSULineTruncationTag;
  Sizes[3] := SizeOf(lineTrunc);
  Values[3] := @lineTrunc;
  ATSUSetLayoutControls(myTextLayout, 4, Tags, Sizes, Values);
  // set text
  ATSUSetTextPointerLocation(myTextLayout,
          ConstUniCharArrayPtr(PWideChar(AText)),
          kATSUFromTextBeginning,
          kATSUToTextEnd,
          Length(AText));
  ATSUSetRunStyle(myTextLayout,
        myStyles,
        kATSUFromTextBeginning,
        kATSUToTextEnd);
  ATSUSetTransientFontMatching(myTextLayout, true);
  // set breacks
  ATSUBatchBreakLines(myTextLayout, kATSUFromTextBeginning, kATSUToTextEnd, lineWidth, @numSoftBreaks);
  // calc size
  CGContextSaveGState(CGContextRef(Handle));
  // Intersect
  CGContextClipToRect(CGContextRef(Handle), CGRectFromRect(vgRect(ARect.Left - 1, ARect.Top - 1, ARect.Right + 1, ARect.Bottom + 3)));
  // Scale
  CGContextScaleCTM(CGContextRef(Handle), 1, -1);
  // vertical aligning using numSoftBreaks
  case AVTextAlign of
    vgTextAlignCenter:
        VerticalOffset := (vgRectHeight(ARect) - ((numSoftBreaks + 0.5) * Font.Size * 1.33)) / 2;
    vgTextAlignNear:
        VerticalOffset := vgRectHeight(ARect) - ((numSoftBreaks + 0.75) * Font.Size * 1.33);
    vgTextAlignFar:
        VerticalOffset := 0.25 * Font.Size * 1.33;
  end;
  // final draw
  if numSoftBreaks = 0 then
  begin
    if not WordWrap then
    begin
      CLeft := 0;
      CTop := 0;
      Cright := 0;
      CBottom := 0;
      ATSUGetUnjustifiedBounds(myTextLayout, kATSUFromTextBeginning, kATSUToTextEnd, CLeft, CRight, CTop, CBottom);
      case ATextAlign of
        vgTextAlignCenter:
          HorizontalOffset := -(((CRight - CLeft) / $FFFF) - vgRectWidth(ARect)) / 2;
        vgTextAlignFar:
          HorizontalOffset := -(((CRight - CLeft) / $FFFF) - vgRectWidth(ARect));
      else
        HorizontalOffset := 0;
      end;
    end
    else
      HorizontalOffset := 0;
    ATSUDrawText(myTextLayout, kATSUFromTextBeginning, kATSUToTextEnd, (Trunc(ARect.Left + HorizontalOffset) * $FFFF), (Trunc(-ARect.Bottom + VerticalOffset) * $FFFF));
  end
  else
  begin
    ATSUGetSoftLineBreaks(myTextLayout, kATSUFromTextBeginning, kATSUToTextEnd, numSoftBreaks, @softBreaks[0], @numSoftBreaks);
    ATSUGetTextLocation(myTextLayout, nil, nil, @layoutStart, @layoutLength, nil);
    currentStart := layoutStart;
    for i := 0 to numSoftBreaks do
    begin
      if (numSoftBreaks > i) then
        currentEnd := softBreaks[i]
      else
        currentEnd := layoutStart + layoutLength;
      if not WordWrap then
      begin
        CLeft := 0;
        CTop := 0;
        Cright := 0;
        CBottom := 0;
        ATSUGetUnjustifiedBounds(myTextLayout, currentStart, currentEnd - currentStart, CLeft, CRight, CTop, CBottom);
        case ATextAlign of
          vgTextAlignCenter:
            HorizontalOffset := -(((CRight - CLeft) / $FFFF) - vgRectWidth(ARect)) / 2;
          vgTextAlignFar:
            HorizontalOffset := -(((CRight - CLeft) / $FFFF) - vgRectWidth(ARect));
        else
          HorizontalOffset := 0;
        end;
      end
      else
        HorizontalOffset := 0;
      ATSUDrawText(myTextLayout, currentStart, currentEnd - currentStart, Round((ARect.Left + HorizontalOffset) * $FFFF), Round((-ARect.Bottom + VerticalOffset + ((numSoftBreaks - i) * Font.Size * 1.33)) * $FFFF));
      currentStart := currentEnd;
    end;
  end;
  // restore
  CGContextRestoreGState(CGContextRef(Handle));
  // dispose
  ATSUDisposeTextLayout(myTextLayout);
  ATSUDisposeStyle(myStyles);
  DeApplyFill(ARect, AOpacity);
end;

procedure TvgCanvasQuartz.MeasureText(var ARect: TvgRect; AClipRect: TvgRect;
  const AText: WideString; const WordWrap: boolean;
  const ATextAlign: TvgTextAlign; const AVTextAlign: TvgTextAlign);
var
  curR, TextR: TvgRect;
  Values: array [0..10] of ATSUAttributeValuePtr;
  Tags: array [0..10] of ATSUAttributeTag;
  Sizes: array [0..10] of ByteCount;
  myStyles: ATSUStyle;
  myTextLayout: ATSUTextLayout;
  myFont: ATSUFontID;
  atsuSize: Fixed;
  atsuItalic, atsuBold: boolean;
  lineTrunc: ATSULineTruncation;
  i, lineWidth: cardinal;
  textAlign: fract;
  CLeft, CRight, CTop, CBottom: Fixed;
  numSoftBreaks: integer;
  currentStart, currentEnd, layoutStart, layoutLength: UniCharArrayOffset;
  softBreaks: array [0..100] of UniCharArrayOffset;
  HorizontalOffset, VerticalOffset: single;
  S: String;
begin
  if Length(AText) = 0 then
  begin
    ARect.Right := ARect.Left;
    Exit;
  end;
  { Create ATSU }
  ATSUCreateStyle(myStyles);
  // font
  S := Font.Family;
  if ATSUFindFontFromName(@S[1], Length(S), kFontFamilyName, kFontNoPlatform, kFontNoScript, kFontNoLanguage, myFont) = noErr then
  begin
    Tags[0] := kATSUFontTag;
    Sizes[0] := sizeof(ATSUFontId);
    Values[0] := @myFont;
    ATSUSetAttributes(myStyles, 1, Tags, Sizes, Values);
  end;
  // style
  atsuSize := Round(Font.Size * $FFFF);
  atsuBold := Font.Style in [vgFontBold, vgFontBoldItalic];
  atsuItalic := Font.Style in [vgFontItalic, vgFontBoldItalic];
  Tags[0] := kATSUSizeTag;
  Sizes[0] := sizeof(Fixed);
  Values[0] := @atsuSize;
  Tags[1] := kATSUQDBoldfaceTag;
  Sizes[1] := sizeof(boolean);
  Values[1] := @atsuBold;
  Tags[2] := kATSUQDItalicTag;
  Sizes[2] := sizeof(boolean);
  Values[2] := @atsuItalic;
  ATSUSetAttributes(myStyles, 3, Tags, Sizes, Values);
  // layout
  myTextLayout := nil;
  ATSUCreateTextLayout(myTextLayout);
  // set context
  Tags[0] := kATSUCGContextTag;
  Sizes[0] := SizeOf(CGContextRef);
  Values[0] := @CGContextRef(Handle);
  // set align
  Tags[1] := kATSULineFlushFactorTag;
  Sizes[1] := SizeOf(textAlign);
  if WordWrap then
  begin
    case ATextAlign of
      vgTextAlignCenter:
        begin
          textAlign := kATSUCenterAlignment;
        end;
      vgTextAlignNear:
        begin
          textAlign := kATSUStartAlignment;
        end;
      vgTextAlignFar:
        begin
          textAlign := kATSUEndAlignment;
        end;
    end;
  end
  else
    textAlign := kATSUStartAlignment;
  Values[1] := @textAlign;
  // line width
  if WordWrap then
    lineWidth := Trunc(vgRectWidth(ARect) * $FFFF)
  else
    lineWidth := 5000 * $FFFF;
  if lineWidth <= 0 then lineWidth := 1;
  Tags[2] := kATSULineWidthTag;
  Sizes[2] := SizeOf(lineWidth);
  Values[2] := @lineWidth;
  // line trunk - set ...
  lineTrunc := kATSUTruncateNone;
  Tags[3] := kATSULineTruncationTag;
  Sizes[3] := SizeOf(lineTrunc);
  Values[3] := @lineTrunc;
  ATSUSetLayoutControls(myTextLayout, 4, Tags, Sizes, Values);
  // set text
  ATSUSetTextPointerLocation(myTextLayout,
          ConstUniCharArrayPtr(PWideChar(AText)),
          kATSUFromTextBeginning,
          kATSUToTextEnd,
          Length(AText));
  ATSUSetRunStyle(myTextLayout,
        myStyles,
        kATSUFromTextBeginning,
        kATSUToTextEnd);
  ATSUSetTransientFontMatching(myTextLayout, true);
  // set breacks
  ATSUBatchBreakLines(myTextLayout, kATSUFromTextBeginning, kATSUToTextEnd, lineWidth, @numSoftBreaks);
  // vertical aligning using numSoftBreaks
  case AVTextAlign of
    vgTextAlignCenter:
        VerticalOffset := (vgRectHeight(ARect) - ((numSoftBreaks + 0.5) * Font.Size * 1.33)) / 2;
    vgTextAlignNear:
        VerticalOffset := vgRectHeight(ARect) - ((numSoftBreaks + 1) * Font.Size * 1.33);
    vgTextAlignFar:
        VerticalOffset := 0;
  end;
  // final draw
  if numSoftBreaks = 0 then
  begin
    ATSUGetUnjustifiedBounds(myTextLayout, kATSUFromTextBeginning, kATSUToTextEnd, CLeft, CRight, CTop, CBottom);
    curR := vgRect(CLeft / $FFFF, CBottom / $FFFF, CRight / $FFFF, CTop / $FFFF);
    curR.Bottom := curR.Top + (Font.Size * 1.33);
    vgRectCenter(curR, curR);
    textR := curR;
  end
  else
  begin
    ATSUGetSoftLineBreaks(myTextLayout, kATSUFromTextBeginning, kATSUToTextEnd, numSoftBreaks, @softBreaks[0], @numSoftBreaks);
    ATSUGetTextLocation(myTextLayout, nil, nil, @layoutStart, @layoutLength, nil);
    currentStart := layoutStart;
    for i := 0 to numSoftBreaks do
    begin
      if (numSoftBreaks > i) then
        currentEnd := softBreaks[i]
      else
        currentEnd := layoutStart + layoutLength;
      ATSUGetUnjustifiedBounds(myTextLayout, currentStart, currentEnd - currentStart, CLeft, CRight, CTop, CBottom);
      curR := vgRect(CLeft / $FFFF, CBottom / $FFFF, CRight / $FFFF, CTop / $FFFF);
      curR.Bottom := curR.Top + (Font.Size * 1.33);
//      vgRectCenter(curR, curR);
      vgOffsetRect(CurR, 0, i * Font.Size * 1.33);
      if i = 0 then
        TextR := curR
      else
        TextR := vgUnionRect(TextR, curR);
      currentStart := currentEnd;
    end;
  end;
  // align
  case ATextAlign of
    vgTextAlignCenter:
      begin
        vgOffsetRect(TextR, -TextR.Left, 0);
        vgOffsetRect(TextR, Trunc((vgRectWidth(ARect) - vgRectWidth(TextR)) / 2), 0);
        vgOffsetRect(TextR, ARect.Left, 0);
      end;
    vgTextAlignNear:
      begin
        vgOffsetRect(TextR, -TextR.Left, 0);
        vgOffsetRect(TextR, ARect.Left, 0);
      end;
    vgTextAlignFar:
      begin
        vgOffsetRect(TextR, -TextR.Left, 0);
        vgOffsetRect(TextR, Trunc((vgRectWidth(ARect) - vgRectWidth(TextR))), 0);
        vgOffsetRect(TextR, ARect.Left, 0);
      end;
  end;
  case AVTextAlign of
    vgTextAlignCenter:
      begin
        vgOffsetRect(TextR, 0, -TextR.Top);
        vgOffsetRect(TextR, 0, Trunc((vgRectHeight(ARect) - vgRectHeight(TextR)) / 2));
        vgOffsetRect(TextR, 0, ARect.Top);
      end;
    vgTextAlignNear:
      begin
        vgOffsetRect(TextR, 0, -TextR.Top);
        vgOffsetRect(TextR, 0, ARect.Top);
      end;
    vgTextAlignFar:
      begin
        vgOffsetRect(TextR, 0, -TextR.Top);
        vgOffsetRect(TextR, 0, Trunc((vgRectHeight(ARect) - vgRectHeight(TextR))));
        vgOffsetRect(TextR, 0, ARect.Top);
      end;
  end;
  // result
  ARect := TextR;
  // dispose
  ATSUDisposeTextLayout(myTextLayout);
  ATSUDisposeStyle(myStyles);
end;

type

  ATSLayoutRecord = packed record
    // The glyph ID reference.
    glyphID: ATSGlyphRef;
    // These flags describe the individual state of the glyph (see above).
    flags: ATSGlyphInfoFlags;
    // The byte offset of the original character that spawned this glyph.
    originalOffset: ByteCount;
    // This is the real position that the glyph sits.
    realPos: Fixed;
  end;

  ATSLayoutRecordArray = array of ATSLayoutRecord;
  ATSLayoutRecordArray2 = array [0..0] of ATSLayoutRecord;
  PATSLayoutRecordArray2 = ^ATSLayoutRecordArray2;
  TYOffsets = array [0..0] of Fixed;
  PYOffsets = ^TYOffsets;

  Text3DCallback = packed record
    Path: TvgPathData;
    SP: ^TvgPoint;
    GlyphLayouts: ATSLayoutRecord;
    Height: single;
    XOffset, YOffset: single;
  end;

function MyCubicMoveToProc( const (*var*) pt: Float32Point; callBackDataPtr: Pointer ): OSStatus; stdcall;
var
  G: Text3DCallback;
begin
  G := Text3DCallback(callBackDataPtr^);
  G.SP^ := vgPoint(G.XOffset + (G.GlyphLayouts.realPos / $FFFF) + pt.x, G.YOffset + pt.y);
  G.Path.MoveTo(vgPoint(G.XOffset + (G.GlyphLayouts.realPos / $FFFF) + pt.x, G.YOffset + pt.y));
  Result := noErr;
end;

function MyCubicLineToProc( const (*var*) pt: Float32Point; callBackDataPtr: Pointer ): OSStatus; stdcall;
var
  G: Text3DCallback;
begin
  G := Text3DCallback(callBackDataPtr^);
  G.Path.LineTo(vgPoint(G.XOffset + (G.GlyphLayouts.realPos / $FFFF) + pt.x, G.YOffset + pt.y));
  Result := noErr;
end;

function MyCubicCurveToProc( const (*var*) pt, pt2, pt3: Float32Point; callBackDataPtr: Pointer ): OSStatus; stdcall;
var
  G: Text3DCallback;
begin
  G := Text3DCallback(callBackDataPtr^);
  G.Path.CurveTo(vgPoint(G.XOffset + (G.GlyphLayouts.realPos / $FFFF) + pt.x, G.YOffset + pt.y),
    vgPoint(G.XOffset + (G.GlyphLayouts.realPos / $FFFF) + pt2.x, G.YOffset + pt2.y),
    vgPoint(G.XOffset + (G.GlyphLayouts.realPos / $FFFF) + pt3.x, G.YOffset + pt3.y));
  Result := noErr;
end;

function MyCubicClosePathProc(callBackDataPtr: Pointer): OSStatus; stdcall;
var
  G: Text3DCallback;
begin
  G := Text3DCallback(callBackDataPtr^);
  G.Path.ClosePath;
  Result := noErr;
end;

procedure GetGlyphIDsAndPositions(iLayout: ATSUTextLayout; iStart: UniCharArrayOffset; iLength: UniCharCount;
  var oGlyphRecordArray: ATSLayoutRecordArray; var oNumGlyphs: ItemCount);
var
  numDeltaYs: ItemCount;
  deltaYs: PYOffsets;
  MyGlyphRecordArray: PATSLayoutRecordArray2;
  i: integer;
begin
  // This block of code uses the new Direct Access APIs, which are only available on Mac OS X 10.2 and later systems
  // Get the arrays of glyph information
  ATSUDirectGetLayoutDataArrayPtrFromTextLayout(iLayout, iStart, kATSUDirectDataLayoutRecordATSLayoutRecordCurrent, @MyGlyphRecordArray, oNumGlyphs);
  ATSUDirectGetLayoutDataArrayPtrFromTextLayout(iLayout, iStart, kATSUDirectDataBaselineDeltaFixedArray, @deltaYs, numDeltaYs);
  // Build the array of MyGlyphRecords
  SetLength(oGlyphRecordArray, oNumGlyphs);
  for i := 0 to oNumGlyphs - 1 do
  begin
    // Fill in the glyphID
    oGlyphRecordArray[i] := MyGlyphRecordArray[i];
  end;
  // Free the arrays of glyph information
  if (deltaYs <> nil) then
    ATSUDirectReleaseLayoutDataArrayPtr(nil, kATSUDirectDataBaselineDeltaFixedArray, @deltaYs);
  ATSUDirectReleaseLayoutDataArrayPtr(nil, kATSUDirectDataLayoutRecordATSLayoutRecordCurrent, @MyGlyphRecordArray);
end;

function TvgCanvasQuartz.TextToPath(Path: TvgPathData; const ARect: TvgRect; const AText: WideString; const WordWrap: boolean; const ATextAlign: TvgTextAlign;
      const AVTextAlign: TvgTextAlign = vgTextAlignCenter): boolean;
var
  Values: array [0..10] of ATSUAttributeValuePtr;
  Tags: array [0..10] of ATSUAttributeTag;
  Sizes: array [0..10] of ByteCount;
  myStyles: ATSUStyle;
  myTextLayout: ATSUTextLayout;
  myFont: ATSUFontID;
  atsuSize: Fixed;
  atsuItalic, atsuBold: boolean;
  lineTrunc: ATSULineTruncation;
  i, j, lineWidth: cardinal;
  textAlign: fract;
  CLeft, CRight, CTop, CBottom: Fixed;
  numSoftBreaks: integer;
  currentStart, currentEnd, layoutStart, layoutLength: UniCharArrayOffset;
  softBreaks: array [0..100] of UniCharArrayOffset;
  S: String;
  Status: OSStatus;
  numGlyphs: cardinal;
  glyphArray: ATSLayoutRecordArray;
  moveToProc:  ATSCubicMoveToUPP                   ;
  lineToProc:  ATSCubicLineToUPP                   ;
  curveToProc:  ATSCubicCurveToUPP                  ;
  closePathProc:  ATSCubicClosePathUPP                ;
  Data: Text3DCallback;
  VerticalOffset, HorizontalOffset, lineStep: single;
  Size: TvgPoint;
  SP: TvgPoint;
begin
  Result := false;
  if Length(AText) = 0 then Exit;
  Path.Clear;

  { Create ATSU }
  ATSUCreateStyle(myStyles);
  // font
  S := Font.Family;
  if ATSUFindFontFromName(@S[1], Length(S), kFontFamilyName, kFontNoPlatform, kFontNoScript, kFontNoLanguage, myFont) = noErr then
  begin
    Tags[0] := kATSUFontTag;
    Sizes[0] := sizeof(ATSUFontId);
    Values[0] := @myFont;
    ATSUSetAttributes(myStyles, 1, Tags, Sizes, Values);
  end;
  // style
  atsuSize := Round(Font.Size * $FFFF);
  atsuBold := Font.Style in [vgFontBold, vgFontBoldItalic];
  atsuItalic := Font.Style in [vgFontItalic, vgFontBoldItalic];

  Tags[0] := kATSUSizeTag;
  Sizes[0] := sizeof(Fixed);
  Values[0] := @atsuSize;
  Tags[1] := kATSUQDBoldFaceTag;
  Sizes[1] := sizeof(boolean);
  Values[1] := @atsuBold;
  Tags[2] := kATSUQDItalicTag;
  Sizes[2] := sizeof(boolean);
  Values[2] := @atsuItalic;
  ATSUSetAttributes(myStyles, 3, Tags, Sizes, Values);
  // layout
  myTextLayout := nil;
  ATSUCreateTextLayout(myTextLayout);
  // set align
  Tags[1] := kATSULineFlushFactorTag;
  Sizes[1] := SizeOf(textAlign);
  if WordWrap then
  begin
    case ATextAlign of
      vgTextAlignCenter:
        begin
          textAlign := kATSUCenterAlignment;
        end;
      vgTextAlignNear:
        begin
          textAlign := kATSUStartAlignment;
        end;
      vgTextAlignFar:
        begin
          textAlign := kATSUEndAlignment;
        end;
    end;
  end
  else
    textAlign := kATSUStartAlignment;
  Values[1] := @textAlign;
  // line width
  if WordWrap then
    lineWidth := Trunc(vgRectWidth(ARect) * $FFFF)
  else
    lineWidth := 5000 * $FFFF;
  if lineWidth <= 0 then lineWidth := 1;
  Tags[2] := kATSULineWidthTag;
  Sizes[2] := SizeOf(lineWidth);
  Values[2] := @lineWidth;
  // line trunk - set ...
  lineTrunc := kATSUTruncateNone;
  Tags[3] := kATSULineTruncationTag;
  Sizes[3] := SizeOf(lineTrunc);
  Values[3] := @lineTrunc;
  ATSUSetLayoutControls(myTextLayout, 4, Tags, Sizes, Values);
  // set text
  ATSUSetTextPointerLocation(myTextLayout,
          ConstUniCharArrayPtr(PWideChar(AText)),
          kATSUFromTextBeginning,
          kATSUToTextEnd,
          Length(AText));
  ATSUSetRunStyle(myTextLayout,
        myStyles,
        kATSUFromTextBeginning,
        kATSUToTextEnd);
  ATSUSetTransientFontMatching(myTextLayout, true);
  // set breacks
  ATSUBatchBreakLines(myTextLayout, kATSUFromTextBeginning, kATSUToTextEnd, lineWidth, @numSoftBreaks);
  // procs
  moveToProc := NewATSCubicMoveToUPP(@MyCubicMoveToProc);
  lineToProc := NewATSCubicLineToUPP(@MyCubicLineToProc);
  curveToProc := NewATSCubicCurveToUPP(@MyCubicCurveToProc);
  closePathProc := NewATSCubicClosePathUPP(@MyCubicClosePathProc);
  // vertical aligning using numSoftBreaks
  case AVTextAlign of
    vgTextAlignCenter:
        VerticalOffset := (vgRectHeight(ARect) - (((numSoftBreaks - 0.5) * Font.Size * 1.33))) / 2;
    vgTextAlignNear:
        VerticalOffset := (0.75 * Font.Size * 1.33);
    vgTextAlignFar:
        VerticalOffset := vgRectHeight(ARect) - (Font.Size * 1.33 * (numSoftBreaks + 0.25));
  end;
  // final draw
  if numSoftBreaks = 0 then
  begin
    ATSUGetUnjustifiedBounds(myTextLayout, kATSUFromTextBeginning, kATSUToTextEnd, CLeft, CRight, CTop, CBottom);
    case ATextAlign of
      vgTextAlignCenter:
        HorizontalOffset := -(((CRight - CLeft) / $FFFF) - vgRectWidth(ARect)) / 2;
      vgTextAlignFar:
        HorizontalOffset := -(((CRight - CLeft) / $FFFF) - vgRectWidth(ARect));
    else
      HorizontalOffset := 0;
    end;

    GetGlyphIDsAndPositions(myTextLayout, kATSUFromTextBeginning, kATSUToTextEnd, GlyphArray, numGlyphs);
    for i := 0 to numGlyphs - 1 do
    begin
      SP := vgPoint(0, 0);
      Data.Path := Path;
      Data.SP := @SP;
      Data.GlyphLayouts := GlyphArray[i];
      Data.Height := (Font.Size * 1.33);
      Data.XOffset := ARect.Left + HorizontalOffset;
      Data.YOffset := VerticalOffset;
      ATSUGlyphGetCubicPaths(myStyles, GlyphArray[i].glyphID, moveToProc, lineToProc, curveToProc, closePathProc, @Data, Status);
    end;
  end
  else
  begin
    ATSUGetSoftLineBreaks(myTextLayout, kATSUFromTextBeginning, kATSUToTextEnd, numSoftBreaks, @softBreaks[0], @numSoftBreaks);
    ATSUGetTextLocation(myTextLayout, nil, nil, @layoutStart, @layoutLength, nil);
    currentStart := layoutStart;
    for i := 0 to numSoftBreaks do
    begin
      if (numSoftBreaks > i) then
        currentEnd := softBreaks[i]
      else
        currentEnd := layoutStart + layoutLength;

      ATSUGetUnjustifiedBounds(myTextLayout, currentStart, currentEnd - currentStart, CLeft, CRight, CTop, CBottom);
      case ATextAlign of
        vgTextAlignCenter:
          HorizontalOffset := -(((CRight - CLeft) / $FFFF) - vgRectWidth(ARect)) / 2;
        vgTextAlignFar:
          HorizontalOffset := -(((CRight - CLeft) / $FFFF) - vgRectWidth(ARect));
      else
        HorizontalOffset := 0;
      end;
      GetGlyphIDsAndPositions(myTextLayout, currentStart, currentEnd - currentStart, GlyphArray, numGlyphs);
      for j := 0 to numGlyphs - 1 do
      begin
        SP := vgPoint(0, 0);
        Data.Path := Path;
        Data.SP := @SP;
        Data.GlyphLayouts := GlyphArray[j];
        Data.Height := (Font.Size * 1.33);
        if i > 0 then
          Data.YOffset := i * Data.Height
        else
          Data.YOffset := 0;
        Data.YOffset := Data.YOffset + VerticalOffset;
        Data.XOffset := HorizontalOffset;
        ATSUGlyphGetCubicPaths(myStyles, GlyphArray[j].glyphID, moveToProc, lineToProc, curveToProc, closePathProc, @Data, Status);
      end;

      currentStart := currentEnd;
    end;
    ATSUGetUnjustifiedBounds(myTextLayout, kATSUFromTextBeginning, kATSUToTextEnd, CLeft, CRight, CTop, CBottom);
  end;
  // restore
  Result := true;
  // dispose
  DisposeATSCubicMoveToUPP(moveToProc);
  DisposeATSCubicLineToUPP(lineToProc);
  DisposeATSCubicCurveToUPP(curveToProc);
  DisposeATSCubicClosePathUPP(closePathProc);
  ATSUDisposeTextLayout(myTextLayout);
  ATSUDisposeStyle(myStyles);
end;

{$ENDIF}

initialization
{$IFDEF DARWIN}
  DefaultCanvasClass := TvgCanvasQuartz;
  DefaultFilterClass := TvgFilterQuartz;
{$ENDIF}
end.