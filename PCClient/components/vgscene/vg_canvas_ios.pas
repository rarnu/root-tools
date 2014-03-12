unit vg_canvas_ios;

{$I vg_define.inc}
{$modeswitch objectivec1}
{$H+}

interface

uses
  {$IFDEF NOVCL}
  {$IFDEF DARWIN}
  iPhoneAll, MacTypes,
  UTCoreTypes,
  CFNumber, CFString, CFBase, CFUrl,
  CGImage, CGGradient, CGFunction, CGShading, CGColor, CGColorSpace, CGContext, CGBitmapContext,
  CGGeometry, CGImageSource, CGImageDestination, CGDataProvider, CGDataConsumer, CFDictionary,
  CGAffineTransforms,
  {$ENDIF}
  {$ENDIF}
  Classes, SysUtils, vg_scene;

const
  merge5 = 0;

{$IFDEF NOVCL}
{$IFDEF DARWIN}

type

  { TvgFilterCocoa }

  TvgFilterCocoa = class(TvgFilter)
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

  { TvgCanvasCocoa }

  TvgCanvasCocoa = class(TvgCanvas)
  private
    Func: CGFunctionRef;
    BitmapRef: CGImageRef;
    Callback: CGFunctionCallbacks;
    Shading: CGShadingRef;
    Scale: single;
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
{$ENDIF}

implementation {===============================================================}

{$IFDEF NOVCL}
{$IFDEF DARWIN}

var
  MyColorSpace: CGColorSpaceRef;

function ColorSpace: CGColorSpaceRef;
begin
  if MyColorSpace = nil then
    MyColorSpace := CGColorSpaceCreateDeviceRGB;
  Result := MyColorSpace;
end;

{ TvgFilterCocoa }

class function TvgFilterCocoa.GetFileTypes: string;
begin
  Result := '*.jpg;*.jpeg;*.png;*.tif;*.tiff;*.gif;*.bmp;*.xbm;*.ico';
end;

class function TvgFilterCocoa.GetImageSize(const AFileName: string): TvgPoint;
var
  Img: UIImage;
  S: NSString;
begin
  Result := vgPoint(0, 0);
  S := NSSTR(PChar(AFileName));
  Img := UIImage.alloc.initWithContentsOfFile(NSStringPointer(S));
  if Img <> nil then
  begin
    Result := vgPoint(Img.Size.width, Img.Size.height);
    Img.release;
  end;
  S.release;
end;

function TvgFilterCocoa.LoadFromStream(const AStream: TStream; var Bitmap: TvgBitmap): boolean;
var
  Img: UIImage;
  memStream: TMemoryStream;
  data: NSData;
  ImgRef: CGImageRef;
  CtxRef: CGContextRef;
begin
  Result := false;
  memStream := TMemoryStream.Create;
  memStream.CopyFrom(AStream, AStream.Size);
  memStream.Position := 0;
  data := NSData.alloc.initWithBytesNoCopy_length_freeWhenDone(memStream.Memory, memStream.Size, false);
  if data.length > 0 then
  begin
    Img := UIImage.alloc.initWithData(NSDataPointer(data));
    if Img <> nil then
    begin
      ImgRef := Img.cGImage;
      if ImgRef <> nil then
      begin
        Bitmap.SetSize(CGImageGetWidth(ImgRef), CGImageGetHeight(ImgRef));
        CtxRef := CGBitmapContextCreate(Bitmap.StartLine, Bitmap.Width, Bitmap.Height, 8,
           Bitmap.Width * 4, ColorSpace, kCGImageAlphaPremultipliedLast);
        CGContextDrawImage(CtxRef, CGRectFromRect(vgRect(0, 0, Bitmap.Width, Bitmap.Height)), imgRef);
        CGContextRelease(CtxRef);
        Result := true;
        Img.release;
      end;
    end;
  end;
  data.release;
  memStream.Free;
end;

function TvgFilterCocoa.SaveToStream(const AStream: TStream; var Bitmap: TvgBitmap; const Format: string;
  const Params: string = ''): boolean;
var
  data: NSData;
  img: UIImage;
  imgRef: CGImageRef;
  CtxRef: CGContextRef;
begin
  Result := false;
  if (LowerCase(Format) = 'jpg') or (LowerCase(Format) = 'jpeg') then
  begin
    CtxRef := CGBitmapContextCreate(Bitmap.StartLine, Bitmap.Width, Bitmap.Height, 8,
       Bitmap.Width * 4, ColorSpace, kCGImageAlphaPremultipliedLast);
    imgRef := CGBitmapContextCreateImage(CtxRef);
    if imgRef <> nil then
    begin
      img := UIImage.imageWithCGImage(imgRef);
      if img <> nil then
      begin
        data := UIImageJPEGRepresentation(img, 1);
        AStream.Write(data.bytes^, data.length);
        data.release;
        Result := true;
        img.release;
      end;
      CGImageRelease(imgRef);
    end;
    CGContextRelease(CtxRef);
  end;
  if (LowerCase(Format) = 'png') then
  begin
    CtxRef := CGBitmapContextCreate(Bitmap.StartLine, Bitmap.Width, Bitmap.Height, 8,
       Bitmap.Width * 4, ColorSpace, kCGImageAlphaPremultipliedLast);
    imgRef := CGBitmapContextCreateImage(CtxRef);
    if imgRef <> nil then
    begin
      img := UIImage.imageWithCGImage(imgRef);
      if img <> nil then
      begin
        data := UIImagePNGRepresentation(img);
        AStream.Write(data.bytes^, data.length);
        data.release;
        Result := true;
        img.release;
      end;
      CGImageRelease(imgRef);
    end;
    CGContextRelease(CtxRef);
  end;
end;

function TvgFilterCocoa.LoadFromFile(const AFileName: string; const Rotate: single;
  var Bitmap: TvgBitmap): boolean;
var
  Img: UIImage;
  data: NSData;
  ImgRef: CGImageRef;
  CtxRef: CGContextRef;
begin
  Result := false;
  Img := UIImage.alloc.initWithContentsOfFile(NSStringPointer(NSSTR(PChar(AFileName))));
  if Img <> nil then
  begin
    ImgRef := Img.cGImage;
    if ImgRef <> nil then
    begin
      Bitmap.SetSize(CGImageGetWidth(ImgRef), CGImageGetHeight(ImgRef));
      CtxRef := CGBitmapContextCreate(Bitmap.StartLine, Bitmap.Width, Bitmap.Height, 8,
         Bitmap.Width * 4, ColorSpace, kCGImageAlphaPremultipliedLast);
      CGContextDrawImage(CtxRef, CGRectFromRect(vgRect(0, 0, Bitmap.Width, Bitmap.Height)), imgRef);
      CGContextRelease(CtxRef);
      Result := true;
    end;
    Img.release;
  end;
end;

function TvgFilterCocoa.LoadThumbnailFromFile(const AFileName: string;
  const AFitWidth, AFitHeight: single; const UseEmbedded: boolean; var Bitmap: TvgBitmap): boolean;
var
  Img: UIImage;
  data: NSData;
  ImgRef: CGImageRef;
  CtxRef: CGContextRef;
  R: TvgRect;
begin
  Result := false;
  Img := UIImage.alloc.initWithContentsOfFile(NSStringPointer(NSSTR(PChar(AFileName))));
  if Img <> nil then
  begin
    ImgRef := Img.cGImage;
    if ImgRef <> nil then
    begin
      R := vgRect(0, 0, CGImageGetWidth(ImgRef), CGImageGetHeight(ImgRef));
      vgFitRect(R, vgRect(0, 0, AFitWidth, AFitHeight));
      Bitmap.SetSize(round(vgRectwidth(R)), round(vgRectHeight(R)));
      CtxRef := CGBitmapContextCreate(Bitmap.StartLine, Bitmap.Width, Bitmap.Height, 8,
         Bitmap.Width * 4, ColorSpace, kCGImageAlphaPremultipliedLast);
      CGContextDrawImage(CtxRef, CGRectFromRect(vgRect(0, 0, Bitmap.Width, Bitmap.Height)), imgRef);
      CGContextRelease(CtxRef);
      Result := true;
    end;
    Img.release;
  end;
end;

function TvgFilterCocoa.SaveToFile(const AFileName: string;
  var Bitmap: TvgBitmap; const Params: string = ''): boolean;
begin

end;

{ TvgCanvasCocoa }

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

constructor TvgCanvasCocoa.Create(const AWidth, AHeight: integer);
begin
  inherited ;
  try
    Scale := UIScreen.mainScreen.scale;
  except
    Scale := 1;
  end;
end;

constructor TvgCanvasCocoa.CreateFromBitmap(const ABitmap: TvgBitmap);
begin
  inherited;
  FBitmap := ABitmap;
  UpdateBitmap(FBitmap);
  Handle := THandle(CGContextRef(FBitmap.Handle));
end;

destructor TvgCanvasCocoa.Destroy;
begin
  inherited;
end;

procedure TvgCanvasCocoa.FreeBuffer;
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

procedure TvgCanvasCocoa.ResizeBuffer(const AWidth, AHeight: integer);
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

procedure TvgCanvasCocoa.FlushBuffer(const X, Y: integer; const DC: Cardinal);
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

procedure TvgCanvasCocoa.FlushBufferRect(const X, Y: integer; const DC: Cardinal;
  const ARect: TvgRect);
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

procedure TvgCanvasCocoa.Clear(const Color: cardinal);
begin
  if not FBuffered then
  begin
    CGContextClearRect(CGContextRef(Handle), CGRectFromRect(vgRect(0, 0, FWidth, FHeight)));
  end
  else
    vgFillLongword(FBufferBits, FWidth * FHeight, Color);
end;

procedure TvgCanvasCocoa.ClearRect(const ARect: TvgRect; const AColor: TvgColor);
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

class function TvgCanvasCocoa.GetBitmapScanline(Bitmap: TvgBitmap; y: integer): PvgColorArray;
begin
  if (y >= 0) and (y < Bitmap.Height) and (Bitmap.StartLine <> nil) then
    Result := @PvgColorArray(Bitmap.StartLine)[(y) * Bitmap.Width]
  else
    Result := nil;
end;

procedure TvgCanvasCocoa.SetMatrix(const M: TvgMatrix);
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
    CGContextTranslateCTM(CGContextRef(Handle), 0, FHeight * Scale);
  CGContextScaleCTM(CGContextRef(Handle), Scale, -Scale);
  { Set new }
  with FMatrix do
    CGContextConcatCTM(CGContextRef(Handle), CGAffineTransformMake(m11, m12, m21, m22, m31, m32));
end;

procedure TvgCanvasCocoa.MultyMatrix(const M: TvgMatrix);
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

function TvgCanvasCocoa.SaveCanvas: cardinal;
var
  i: integer;
begin
  Result := $FFFFFFFF;
  if CGContextRef(Handle) = nil then Exit;

  CGContextSaveGState(CGContextRef(Handle));

  // find exists
  if Length(FSaveData) > 0 then
    for i := 0 to High(FSaveData) do
      if FSaveData[i].Index = $FFFFFFFF then
      begin
        Result := i;
        Break;
      end;
  if Result = $FFFFFFFF then
  begin
    SetLength(FSaveData, Length(FSaveData) + 1);
    Result := High(FSaveData);
  end;
  FSaveData[Result].Index := Result;
  FSaveData[Result].Matrix := FMatrix;
  FSaveData[Result].Stroke := TvgBrush.Create(vgBrushSolid, $FFFFFFFF);
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

procedure TvgCanvasCocoa.RestoreCanvas(const AState: cardinal);
begin
  if CGContextRef(Handle) = nil then Exit;

  if (AState >= 0) and (AState < Length(FSaveData)) then
  begin
    CGContextRestoreGState(CGContextRef(Handle));
    FSaveData[AState].Index := $FFFFFFFF;

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

procedure TvgCanvasCocoa.SetClipRects(const ARects: array of TvgRect);
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

procedure TvgCanvasCocoa.IntersectClipRect(const ARect: TvgRect);
begin
  if CGContextRef(Handle) = nil then Exit;
  CGContextClipToRect(CGContextRef(Handle), CGRectFromRect(ARect));
end;

procedure TvgCanvasCocoa.ExcludeClipRect(const ARect: TvgRect);
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

procedure TvgCanvasCocoa.ResetClipRect;
begin
  if not FBuffered then Exit;
  if CGContextRef(Handle) = nil then Exit;
  if CGContextRef(Handle) <> nil then
    CGContextRelease(CGContextRef(Handle));
  if FBitmap <> nil then
  begin
    UpdateBitmap(FBitmap);
    FHandle := FBitmap.Handle;
  end
  else
  begin
    FHandle := THandle(CGBitmapContextCreate(FBufferBits, FWidth, FHeight, 8,
      FWidth * 4, ColorSpace, kCGImageAlphaPremultipliedLast));
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

procedure TvgCanvasCocoa.ApplyFill(ARect: TvgRect; const AOpacity: single);
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
              BitmapRef := CGBitmapContextCreateImage(CGContextRef(Bitmap.Bitmap.Handle));
              CGContextSetAlpha(CGContextRef(Handle), AOpacity);
            end;
          end;
        end;
    else
      CGContextSetRGBFillColor(CGContextRef(Handle), 0, 0, 0, 0);
    end;
  end;
end;

procedure TvgCanvasCocoa.DeApplyFill(ARect: TvgRect; const AOpacity: single);
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

procedure TvgCanvasCocoa.ApplyStroke(ARect: TvgRect; const AOpacity: single);
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

procedure TvgCanvasCocoa.FontChanged(Sender: TObject);
begin
end;

procedure TvgCanvasCocoa.DrawLine(const APt1, APt2: TvgPoint; const AOpacity: single);
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

procedure TvgCanvasCocoa.DrawRect(const ARect: TvgRect;
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

procedure TvgCanvasCocoa.FillRect(const ARect: TvgRect; const xRadius, yRadius: single; const ACorners: TvgCorners; const AOpacity: single;
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

procedure TvgCanvasCocoa.DrawEllipse(const ARect: TvgRect; const AOpacity: single);
begin
  if CGContextRef(Handle) = nil then Exit;
  if FStroke.Style <> vgBrushNone then
  begin
    ApplyStroke(ARect, AOpacity);
    CGContextStrokeEllipseInRect(CGContextRef(Handle), CGRectFromRect(ARect));
  end;
end;

procedure TvgCanvasCocoa.FillEllipse(const ARect: TvgRect; const AOpacity: single);
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

procedure TvgCanvasCocoa.DrawBitmap(const ABitmap: TvgBitmap;
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

procedure TvgCanvasCocoa.UpdateBitmap(ABitmap: TvgBitmap);
begin
  { update bitmap to Quartz bitmap }
  if ABitmap = nil then Exit;
  { create - if need }
  if ABitmap.Handle = 0 then
  begin
    ABitmap.Handle := THandle(CGBitmapContextCreate(ABitmap.StartLine, ABitmap.Width, ABitmap.Height, 8,
      ABitmap.Width * 4, ColorSpace, kCGImageAlphaPremultipliedLast));
  end;
  { clear flag }
  ABitmap.NeedUpdate := false;
  ABitmap.OnDestroyHandle := DoDestroyBitmap;
end;

procedure TvgCanvasCocoa.DoDestroyBitmap(Sender: TObject);
begin
  if (Sender <> nil) then
  begin
    if (TvgBitmap(Sender).Handle <> 0) then
      CGContextRelease(CGContextRef(TvgBitmap(Sender).Handle));
    TvgBitmap(Sender).Handle := 0;
  end;
end;
{ Path }

procedure TvgCanvasCocoa.DrawPath(const APath: TvgPathData; const ARect: TvgRect; const AOpacity: single);
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

procedure TvgCanvasCocoa.FillPath(const APath: TvgPathData; const ARect: TvgRect; const AOpacity: single);
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

function TvgCanvasCocoa.PtInPath(const APoint: TvgPoint; const ARect: TvgRect; const APath: TvgPathData): boolean;
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

procedure TvgCanvasCocoa.FillText(const ARect, AClipRect: TvgRect; const AText: WideString; const WordWrap: boolean;
  const AOpacity: single; const ATextAlign: TvgTextAlign; const AVTextAlign: TvgTextAlign = vgTextAlignCenter);
var
  S: NSString;
  Size: CGSize;
  Offset: TvgPoint;
  UTAlign: cardinal;
  Mode: cardinal;
begin
  ApplyFill(ARect, AOpacity);
  S := NSString.alloc.initWithUTF8String(PChar(UTF8Encode(AText)));
  Mode := 0;
  if not WordWrap then
    Mode := UILineBreakModeMiddleTruncation;
  Size := S.sizeWithFont_constrainedToSize_lineBreakMode(UIFont.systemFontOfSize(FFont.Size), CGSize(NSMakeSize(ARect.right - Arect.left, ARect.bottom - ARect.top)), Mode);
  case AVTextAlign of
    vgTextAlignNear:
      Offset := vgPoint(0, 0);
    vgTextAlignCenter:
      Offset := vgPoint(0, (vgRectHeight(ARect) - Size.Height) / 2);
    vgTextAlignFar:
      Offset := vgPoint(0, (vgRectHeight(ARect) - Size.Height));
  end;
  case ATextAlign of
    vgTextAlignNear:
      UTAlign := 0;
    vgTextAlignCenter:
      UTAlign := 1;
    vgTextAlignFar:
      UTAlign := 2;
  end;
  S.drawInRect_withFont_lineBreakMode_alignment(NSMakeRect(ARect.left, Offset.Y + ARect.top, ARect.right - Arect.left, ARect.bottom - ARect.top), UIFont.systemFontOfSize(FFont.Size),
    UILineBreakModeMiddleTruncation, UTAlign);
  DeApplyFill(ARect, AOpacity);
  S.release;
end;

procedure TvgCanvasCocoa.MeasureText(var ARect: TvgRect; AClipRect: TvgRect;
  const AText: WideString; const WordWrap: boolean;
  const ATextAlign: TvgTextAlign; const AVTextAlign: TvgTextAlign);
var
  S: NSString;
  Size: CGSize;
  Mode: cardinal;
begin
  if Length(AText) = 0 then
  begin
    ARect.Right := ARect.Left;
    Exit;
  end;

  S := NSString.alloc.initWithUTF8String(PChar(UTF8Encode(AText)));
  Mode := 0;
  if not WordWrap then
    Mode := UILineBreakModeMiddleTruncation;
  Size := S.sizeWithFont_constrainedToSize_lineBreakMode(UIFont.systemFontOfSize(FFont.Size), CGSize(NSMakeSize(ARect.right - Arect.left, ARect.bottom - ARect.top)), Mode);

  ARect.right := ARect.left + Size.width;
  ARect.bottom := ARect.top + Size.height;
  S.release;
end;

function TvgCanvasCocoa.TextToPath(Path: TvgPathData; const ARect: TvgRect; const AText: WideString; const WordWrap: boolean; const ATextAlign: TvgTextAlign;
      const AVTextAlign: TvgTextAlign = vgTextAlignCenter): boolean;
begin
end;

{$ENDIF}
{$ENDIF}

initialization
{$IFDEF NOVCL}
{$IFDEF DARWIN}
  DefaultCanvasClass := TvgCanvasCocoa;
  DefaultFilterClass := TvgFilterCocoa;
{$ENDIF}
{$ENDIF}
finalization
  {$IFDEF DARWIN}
  if MyColorSpace <> nil then
    CGColorSpaceRelease(MyColorSpace);
  {$ENDIF}
end.
