unit vg_canvas_d2d;

{$ALIGN ON}
{$MINENUMSIZE 4}
{$I vg_define.inc}
{.$DEFINE FLATTEN}
{$DEFINE TEXTFLATTEN}

interface

{$IFDEF WINDOWS}
uses Windows, Messages, Classes, SysUtils, MultiMon, Math, vg_scene, ActiveX;
{$ENDIF}

const
  merged2d = 0;

{$IFDEF WINDOWS}
procedure SetD2DDefault;
{$ENDIF}

implementation {===============================================================}


{$IFDEF WINDOWS}
{$I vg_canvas_d2d_intf.inc}

var
  Res: HResult;

const
  TargetMode: TD2D1_RenderTargetType = D2D1_RENDER_TARGET_TYPE_default;

type

  TvgHackBitmap = class(TvgBitmap);

var
  Factory: ID2D1Factory;
  DWriteFactory: IDWriteFactory;
  ImagingFactory: IWICImagingFactory;

type

  TvgFilterWIC = class(TvgFilter)
  private
  public
  published
    class function GetFileTypes: string; override;
    class function GetImageSize(const AFileName: string): TvgPoint; override;
    function LoadFromFile(const AFileName: string; const Rotate: single; var Bitmap: TvgBitmap): boolean; override;
    function SaveToFile(const AFileName: string; var Bitmap: TvgBitmap; const Params: string = ''): boolean; override;
    function LoadThumbnailFromFile(const AFileName: string; const AFitWidth, AFitHeight: single; const UseEmbedded: boolean;
      var Bitmap: TvgBitmap): boolean; override;
    function LoadFromStream(const AStream: TStream; var Bitmap: TvgBitmap): boolean; override;
    function SaveToStream(const AStream: TStream; var Bitmap: TvgBitmap; const Format: string;
      const Params: string = ''): boolean; override;
  end;

  TvgCanvasD2D = class(TvgCanvas)
  private
    FBufferHandle: cardinal;
    FBitmapInfo: TBitmapInfo;
    RenderTarget: ID2D1RenderTarget;
    Brush: ID2D1Brush;
    StrokeBrush: ID2D1Brush;
    StrokeStyle: ID2D1StrokeStyle;
    Layer: ID2D1Layer;
    FSaved: Pointer;
    WICBitmap: IWICBitmap;
    FLastState: integer;
    procedure CreateResources;
    procedure DisposeResources;
  protected
    procedure ApplyFill(ARect: TvgRect; const AOpacity: single);
    procedure ApplyStroke(ARect: TvgRect; const AOpacity: single);
    procedure FontChanged(Sender: TObject); override;
    procedure IntFillPath(P: ID2D1Geometry; R: TvgRect; Opacity: single);
    procedure IntFillRect(R: TvgRect; Opacity: single);
    class function GetBitmapScanline(Bitmap: TvgBitmap; y: integer): PvgColorArray; override;
  public
    constructor Create(const AWidth, AHeight: integer); override;
    constructor CreateFromBitmap(const ABitmap: TvgBitmap); override;
    destructor Destroy; override;
    { begin and }
    function BeginScene: boolean; override;
    procedure EndScene; override;
    { buffer }
    procedure FreeBuffer; override;
    procedure ResizeBuffer(const AWidth, AHeight: integer); override;
    procedure FlushBuffer(const X, Y: integer; const DC: Cardinal); override;
    procedure FlushBufferRect(const X, Y: integer; const DC: Cardinal; const ARect: TvgRect); override;
    procedure Clear(const Color: TvgColor); override;
    procedure ClearRect(const ARect: TvgRect; const AColor: TvgColor = 0); override;
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
    procedure FillText(const ARect, AClipRect: TvgRect; const AText: WideString; const WordWrap: boolean;
      const AOpacity: single; const ATextAlign: TvgTextAlign; const AVTextAlign: TvgTextAlign = vgTextAlignCenter); override;
    procedure MeasureText(var ARect: TvgRect; AClipRect: TvgRect; const AText: WideString; const WordWrap: boolean; const ATextAlign: TvgTextAlign;
      const AVTextAlign: TvgTextAlign = vgTextAlignCenter); override;
    function TextToPath(Path: TvgPathData; const ARect: TvgRect; const AText: WideString; const WordWrap: boolean; const ATextAlign: TvgTextAlign;
      const AVTextAlign: TvgTextAlign = vgTextAlignCenter): boolean; override;
    function PtInPath(const APoint: TvgPoint; const ARect: TvgRect; const APath: TvgPathData): boolean; override;
    procedure FillPath(const APath: TvgPathData; const ARect: TvgRect; const AOpacity: single); override;
    procedure DrawPath(const APath: TvgPathData; const ARect: TvgRect; const AOpacity: single); override;
    procedure DrawBitmap(const ABitmap: TvgBitmap; const SrcRect, DstRect: TvgRect; const AOpacity: single; const HighSpeed: boolean = false); override;
    procedure DrawThumbnail(const ABitmap: TvgBitmap; const Width, Height: single); override;
  published
  end;

procedure SAFE_RELEASE(var i);
begin
  if IUnknown(i) <> nil then IUnknown(i):= nil;
end;

{ TvgFilterWIC }

class function TvgFilterWIC.GetFileTypes: string;
begin
  Result := '*.bmp;*.jpg;*.jpeg;*.png;*.tif;*.tiff;*.gif;*.ico;*.wmp'
end;

class function TvgFilterWIC.GetImageSize(const AFileName: string): TvgPoint;
var
  bmp: IWICBitmapSource;
  dec: IWICBitmapDecoder;
  frame: IWICBitmapFrameDecode;
  conv: IWICFormatConverter;
  W, H: cardinal;
  fn: WideString;
begin
  W := 0; H := 0;
  fn := AFileName;
  ImagingFactory.CreateDecoderFromFilename(PWideChar(fn), nil, $FFFFFFFF, WICDecodeMetadataCacheOnDemand, dec);
  if Assigned(dec) then
  begin
    dec.GetFrame(0, frame);
    if Assigned(frame) then
    begin
      frame.GetSize(W, H);
    end;
  end;
  Result := vgPoint(W, H);
end;

function TvgFilterWIC.LoadFromFile(const AFileName: string;
  const Rotate: single; var Bitmap: TvgBitmap): boolean;
var
  bmp: IWICBitmapSource;
  dec: IWICBitmapDecoder;
  frame: IWICBitmapFrameDecode;
  conv: IWICFormatConverter;
  W, H: cardinal;
  fn: WideString;
begin
  fn := AFileName;
  Result := false;
  ImagingFactory.CreateDecoderFromFilename(PWideChar(fn), nil, $FFFFFFFF, WICDecodeMetadataCacheOnDemand, dec);
  if Assigned(dec) then
  begin
    dec.GetFrame(0, frame);
    if Assigned(frame) then
    begin
      ImagingFactory.CreateFormatConverter(conv);
      Res := conv.Initialize(frame, GUID_WICPixelFormat32bppPBGRA, WICBitmapDitherTypeNone, nil, 0, 0);
      if Res = 0 then
      begin
        conv.GetSize(W, H);

        Bitmap.SetSize(W, H);
        Bitmap.Clear($FFFF0000);
        Res := conv.CopyPixels(nil, W * 4, W * H * 4, PByte(Bitmap.Startline));
        Result := true;
      end;
    end;
  end;
end;

function TvgFilterWIC.LoadFromStream(const AStream: TStream;
  var Bitmap: TvgBitmap): boolean;
var
  bmp: IWICBitmapSource;
  dec: IWICBitmapDecoder;
  mem: TMemoryStream;
  stream: IWICStream;
  frame: IWICBitmapFrameDecode;
  conv: IWICFormatConverter;
  W, H: cardinal;
begin
  Result := false;
  mem := TMemoryStream.Create;
  mem.CopyFrom(AStream, AStream.Size);
  ImagingFactory.CreateStream(stream);
  stream.InitializeFromMemory(mem.Memory, mem.Size);

  ImagingFactory.CreateDecoderFromStream(stream, nil, WICDecodeMetadataCacheOnDemand, dec);
  if Assigned(dec) then
  begin
    dec.GetFrame(0, frame);
    if Assigned(frame) then
    begin
      ImagingFactory.CreateFormatConverter(conv);
      Res := conv.Initialize(frame, GUID_WICPixelFormat32bppPBGRA, WICBitmapDitherTypeNone, nil, 0, 0);
      if Res = 0 then
      begin
        conv.GetSize(W, H);

        Bitmap.SetSize(W, H);
        Res := conv.CopyPixels(nil, W * 4, W * H * 4, PByte(Bitmap.Startline));
        Result := true;
      end;
    end;
  end;
  mem.Free;
end;

function TvgFilterWIC.LoadThumbnailFromFile(const AFileName: string;
  const AFitWidth, AFitHeight: single; const UseEmbedded: boolean;
  var Bitmap: TvgBitmap): boolean;
var
  bmp: IWICBitmapSource;
  dec: IWICBitmapDecoder;
  scale: single;
  scaler: IWICBitmapScaler;
  frame: IWICBitmapFrameDecode;
  conv: IWICFormatConverter;
  W, H: cardinal;
  R: TvgRect;
  fn: WideString;
begin
  fn := AFileName;
  Result := false;
  ImagingFactory.CreateDecoderFromFilename(PWideChar(fn), nil, $FFFFFFFF, WICDecodeMetadataCacheOnDemand, dec);
  if Assigned(dec) then
  begin
    dec.GetFrame(0, frame);
    if UseEmbedded then
      frame.GetThumbnail(bmp);
    if Assigned(bmp) then
    begin
      ImagingFactory.CreateFormatConverter(conv);
      Res := conv.Initialize(bmp, GUID_WICPixelFormat32bppPBGRA, WICBitmapDitherTypeNone, nil, 0, 0);
      if Res = 0 then
      begin
        conv.GetSize(W, H);

        Bitmap.SetSize(W, H);
        Bitmap.Clear($FFFF0000);
        Res := conv.CopyPixels(nil, W * 4, W * H * 4, PByte(Bitmap.Startline));
        Result := true;
      end;
    end
    else
    begin
      if Assigned(frame) then
      begin
        frame.GetSize(W, H);
        R := vgRect(0, 0, W, H);
        scale := vgFitRect(R, vgRect(0, 0, AFitWidth, AFitHeight));
        if scale = 0 then scale := 0.001;
        if scale < 1 then scale := 1;

        ImagingFactory.CreateBitmapScaler(scaler);
        Res := scaler.Initialize(frame, round(W / scale), round(H / scale), WICBitmapInterpolationModeLinear);

        ImagingFactory.CreateFormatConverter(conv);
        Res := conv.Initialize(scaler, GUID_WICPixelFormat32bppPBGRA, WICBitmapDitherTypeNone, nil, 0, 0);
        if Res = 0 then
        begin
          conv.GetSize(W, H);

          Bitmap.SetSize(W, H);
          Bitmap.Clear($FFFF0000);
          Res := conv.CopyPixels(nil, W * 4, W * H * 4, PByte(Bitmap.Startline));
          Result := true;
        end;
      end;
    end;
  end;
end;

function TvgFilterWIC.SaveToFile(const AFileName: string;
  var Bitmap: TvgBitmap; const Params: string): boolean;
var
  bmp: IWICBitmap;
  enc: IWICBitmapEncoder;
  encoderType: TGuid;
  stream: IWICStream;
  frame: IWICBitmapFrameEncode;
  conv: IWICFormatConverter;
  props: IPropertyBag2;
  W, H: cardinal;
  fn: WideString;
  pformat: WICPixelFormatGUID;
  buf: PByte;
  pname: TPropBag2;
  pvalue: Variant;
  S, param, name, value: ansistring;
  i: integer;
begin
  fn := AFileName;
  Result := false;
  encoderType := GUID_ContainerFormatPng;
  if (LowerCase(ExtractFileExt(AFileName)) = '.jpg') or (LowerCase(ExtractFileExt(AFileName)) = '.jpeg') then
    encoderType := GUID_ContainerFormatJpeg;
  if (LowerCase(ExtractFileExt(AFileName)) = '.bmp') then
    encoderType := GUID_ContainerFormatBmp;
  if (LowerCase(ExtractFileExt(AFileName)) = '.png') then
    encoderType := GUID_ContainerFormatPng;
  if (LowerCase(ExtractFileExt(AFileName)) = '.tif') or (LowerCase(ExtractFileExt(AFileName)) = '.tiff') then
    encoderType := GUID_ContainerFormatTiff;
  if (LowerCase(ExtractFileExt(AFileName)) = '.gif') then
    encoderType := GUID_ContainerFormatGif;
  if (LowerCase(ExtractFileExt(AFileName)) = '.wmp') then
    encoderType := GUID_ContainerFormatWmp;
  ImagingFactory.CreateEncoder(encoderType, nil, enc);
  if Assigned(enc) then
  begin
    ImagingFactory.CreateStream(stream);
    stream.InitializeFromFilename(PWideChar(fn), $FFFFFFFF);
    enc.Initialize(stream, WICBitmapEncoderNoCache);
    enc.CreateNewFrame(frame, props);
    if Assigned(frame) then
    begin
      { set params }
{      if Params <> '' then
      begin
        S := Params;
        i := 0;
        while S <> '' do
        begin
          param := vgGetToken(S, ' ');
          name := vgGetToken(param, '=');
          value := vgGetToken(param, '');
          if CompareText(name, 'quality') = 0 then
          begin
            Fillchar(pname, SizeOf(pname), 0);
            pname.dwType := 1;//PROPBAG2_TYPE_DATA;
            pname.vt := VT_VARIANT;
            pname.pstrName := PWideChar(WideString('ImageQuality'));
            pvalue := vgStrToFloat(value) / 100;
            Res := props.Write(1, @pname, @value);
          end;
          Inc(i);
        end;
      end;}
      Res := frame.Initialize(props);
      { }
      Res := frame.SetSize(Bitmap.Width, Bitmap.Height);
      pformat := GUID_WICPixelFormat32bppPBGRA;
      Res := frame.SetPixelFormat(pformat);
      if IsEqualGuid(pformat, GUID_WICPixelFormat32bppPBGRA) then
      begin
        Res := frame.WritePixels(Bitmap.Height, Bitmap.Width * 4, Bitmap.Width * Bitmap.Height * 4, PByte(Bitmap.Startline));
        Res := frame.Commit;
        Res := enc.Commit;
      end
      else
      begin
        ImagingFactory.CreateBitmapFromMemory(Bitmap.Width, Bitmap.Height, GUID_WICPixelFormat32bppPBGRA,
          Bitmap.Width * 4, Bitmap.Width * Bitmap.Height * 4, PByte(Bitmap.Startline), bmp);
        ImagingFactory.CreateFormatConverter(conv);
        Res := conv.Initialize(bmp, pformat, WICBitmapDitherTypeNone, nil, 0, 0);
        Res := frame.WriteSource(bmp, nil);
        Res := frame.Commit;
        Res := enc.Commit;
      end;
    end;
  end;
end;

function TvgFilterWIC.SaveToStream(const AStream: TStream;
  var Bitmap: TvgBitmap; const Format, Params: string): boolean;
var
  bmp: IWICBitmap;
  enc: IWICBitmapEncoder;
  encoderType: TGuid;
  stream: IWICStream;
  frame: IWICBitmapFrameEncode;
  conv: IWICFormatConverter;
  props: IPropertyBag2;
  W, H: cardinal;
  fn: WideString;
  pformat: WICPixelFormatGUID;
  buf: PByte;
  adapter: TStreamAdapter;
  str: Pointer;
  strSize: integer;
  stg: TStatStg;
begin
  Result := false;
  encoderType := GUID_ContainerFormatPng;
  if LowerCase(Format) = 'jpeg' then encoderType := GUID_ContainerFormatJpeg;
  if LowerCase(Format) = 'jpg' then encoderType := GUID_ContainerFormatJpeg;
  if LowerCase(Format) = 'png' then encoderType := GUID_ContainerFormatPng;
  if LowerCase(Format) = 'bmp' then encoderType := GUID_ContainerFormatBmp;
  if LowerCase(Format) = 'tif' then encoderType := GUID_ContainerFormatTiff;
  if LowerCase(Format) = 'tiff' then encoderType := GUID_ContainerFormatTiff;
  if LowerCase(Format) = 'gif' then encoderType := GUID_ContainerFormatGif;
  if LowerCase(Format) = 'wmp' then encoderType := GUID_ContainerFormatWmp;
  ImagingFactory.CreateEncoder(encoderType, nil, enc);
  if Assigned(enc) then
  begin
    ImagingFactory.CreateStream(stream);
    adapter := TStreamAdapter.Create(AStream);
    IUnknown(adapter)._AddRef;
    stream.InitializeFromIStream(adapter);
    enc.Initialize(stream, WICBitmapEncoderNoCache);
    enc.CreateNewFrame(frame, props);
    if Assigned(frame) then
    begin
      frame.Initialize(props);
      Res := frame.SetSize(Bitmap.Width, Bitmap.Height);
      pformat := GUID_WICPixelFormat32bppPBGRA;
      Res := frame.SetPixelFormat(pformat);
      if IsEqualGuid(pformat, GUID_WICPixelFormat32bppPBGRA) then
      begin
        Res := frame.WritePixels(Bitmap.Height, Bitmap.Width * 4, Bitmap.Width * Bitmap.Height * 4, PByte(Bitmap.Startline));
        Res := frame.Commit;
        Res := enc.Commit;
      end
      else
      begin
        ImagingFactory.CreateBitmapFromMemory(Bitmap.Width, Bitmap.Height, GUID_WICPixelFormat32bppPBGRA,
          Bitmap.Width * 4, Bitmap.Width * Bitmap.Height * 4, PByte(Bitmap.Startline), bmp);
        ImagingFactory.CreateFormatConverter(conv);
        Res := conv.Initialize(bmp, pformat, WICBitmapDitherTypeNone, nil, 0, 0);
        Res := frame.WriteSource(bmp, nil);
        Res := frame.Commit;
        Res := enc.Commit;
      end;
    end;
    IUnknown(adapter)._Release;
  end;
end;

{ Canvas =====}

const
  D2DERR_RECREATE_TARGET              = HResult($8899000C);

procedure SetD2DDefault;
begin
  if D2DLibrary <> 0 then
  begin
    DefaultCanvasClass := TvgCanvasD2D;
    DefaultFilterClass := TvgFilterWIC;
  end;
end;

function D2Rect(const R: TvgRect): TD2D1_RectF;
begin
  Result := TD2D1_RectF(R);
end;

function D2Color(const AColor: TvgColor; const Opacity: single): TD2D1_ColorF;
begin
  with TvgColorRec(AColor) do
    Result := D2D1ColorF(R / $FF, G / $FF, B / $FF, (A / $FF) * Opacity);
end;

function D2Point(X, Y: single): TD2D1_Point2F;
begin
  Result.X := X;
  Result.Y := Y;
end;

function D2Size(W, H: cardinal): TD2D_SizeU;
begin
  Result.Width := W;
  Result.Height := H;
end;

function D2Matrix(M: TvgMatrix): TD2D1_Matrix3X2F;
begin
  Result._11 := M.m11;
  Result._12 := M.m12;
  Result._21 := M.m21;
  Result._22 := M.m22;
  Result._31 := M.m31;
  Result._32 := M.m32;
end;

function D2Bezier(x1, y1, x2, y2, x3, y3: single): TD2D1_BezierSegment;
begin
  Result.Point1.X := x1;
  Result.Point1.Y := y1;
  Result.Point2.X := x2;
  Result.Point2.Y := y2;
  Result.Point3.X := x3;
  Result.Point3.Y := y3;
end;

function D2Ellipse(R: TvgRect): TD2D1_Ellipse;
begin
  Result.Point.x := (R.Right + R.left) / 2;
  Result.Point.y := (R.bottom + R.top) / 2;
  Result.RadiusX := (R.Right - R.left) / 2;
  Result.RadiusY := (R.bottom - R.top) / 2;
end;

function D2FontStyle(Style: TvgFontStyle): TDWrite_FontStyle;
begin
  case Style of
    vgFontRegular: Result := DWRITE_FONT_STYLE_NORMAL;
    vgFontBold: Result := DWRITE_FONT_STYLE_NORMAL;
    vgFontItalic: Result := DWRITE_FONT_STYLE_OBLIQUE;
    vgFontBoldItalic: Result := DWRITE_FONT_STYLE_OBLIQUE;
    vgFontUnderline: Result := DWRITE_FONT_STYLE_NORMAL;
    vgFontStrikeout: Result := DWRITE_FONT_STYLE_NORMAL;
  end;
end;

function D2FontWeight(Style: TvgFontStyle): TDWrite_FontWeight;
begin
  case Style of
    vgFontRegular: Result := DWRITE_FONT_WEIGHT_NORMAL;
    vgFontBold: Result := DWRITE_FONT_WEIGHT_BOLD;
    vgFontItalic: Result := DWRITE_FONT_WEIGHT_NORMAL;
    vgFontBoldItalic: Result := DWRITE_FONT_WEIGHT_BOLD;
    vgFontUnderline: Result := DWRITE_FONT_WEIGHT_NORMAL;
    vgFontStrikeout: Result := DWRITE_FONT_WEIGHT_NORMAL;
  end;
end;

function BitmapProp(DXGI: TDXGI_Format; AlphaMode: TD2D1_AlphaMode): TD2D1_BitmapProperties;
begin
  Result.PixelFormat.Format := DXGI;
  Result.PixelFormat.AlphaMode := AlphaMode;
  Result.DpiX := 0;
  Result.DpiY := 0;
end;

{ TvgCanvasD2D }

const
  SavedCount = 200;

type

  TSaveData = record
    GdiIndex: integer;
    Matrix: TvgMatrix;
    AbsoluteMatrix, InvertMatrix: TvgMatrix;
    StrokeThickness: single;
    Stroke: TvgBrush;
    Fill: TvgBrush;
  end;

  PSaveDataArray = ^TSaveDataArray;
  TSaveDataArray = array [0..100] of TSaveData;

var
  D2DLoadcount: integer = 0;

procedure AddD2DRef;
begin
  if Factory = nil then
  begin
    D2D1CreateFactory(D2D1_FACTORY_TYPE_MULTI_THREADED, ID2D1Factory, nil, Factory);
    DWriteCreateFactory(DWRITE_FACTORY_TYPE_SHARED, IDWriteFactory, IUnknown(DWriteFactory));
    CoCreateInstance(CLSID_WICImagingFactory, nil, CLSCTX_INPROC_SERVER or CLSCTX_LOCAL_SERVER, IUnknown, ImagingFactory);
  end;
  D2DLoadcount := D2DLoadcount + 1;
end;

procedure DecD2DRef;
begin
  D2DLoadcount := D2DLoadcount - 1;
  if D2DLoadcount = 0 then
  begin
    SAFE_RELEASE(ImagingFactory);
    SAFE_RELEASE(DWriteFactory);
    SAFE_RELEASE(Factory);
  end;
end;

constructor TvgCanvasD2D.Create(const AWidth, AHeight: integer);
begin
  AddD2DRef;
  inherited ;
  GetMem(FSaved, SizeOf(TSaveData) * SavedCount);
end;

constructor TvgCanvasD2D.CreateFromBitmap(const ABitmap: TvgBitmap);
begin
  AddD2DRef;
  inherited;
  FBitmap := ABitmap;
//  UpdateBitmap(FBitmap);
  GetMem(FSaved, SizeOf(TSaveData) * SavedCount);
  if Assigned(ImagingFactory) then
  begin
    ImagingFactory.CreateBitmapFromMemory(FBitmap.Width, FBitmap.Height, GUID_WICPixelFormat32bppPBGRA,
      FBitmap.Width * 4, FBitmap.Width * FBitmap.Height * 4, PByte(FBitmap.Startline), WICBitmap);
    CreateResources;
  end;
end;

destructor TvgCanvasD2D.Destroy;
begin
  FreeMem(FSaved, SizeOf(TSaveData) * 100);
  SAFE_RELEASE(Brush);
  SAFE_RELEASE(StrokeBrush);
  SAFE_RELEASE(StrokeStyle);
  inherited;
  DecD2DRef;
end;

procedure TvgCanvasD2D.FreeBuffer;
begin
  DisposeResources;
  if FBuffered then
  begin
    if FBufferHandle = 0 then Exit;
    if FHandle <> 0 then DeleteDC(FHandle);
    FHandle := 0;
    if FBufferHandle <> 0 then DeleteObject(FBufferHandle);
    FBufferHandle := 0;
  end;
end;

procedure TvgCanvasD2D.CreateResources;
var
  SizeInPixels: D2D1_SIZE_U;
begin
  if RenderTarget = nil then
  begin
    SizeInPixels.width := FWidth;
    SizeInPixels.height := FHeight;
    if FBitmap <> nil then
    begin
      Factory.CreateWicBitmapRenderTarget(WICBitmap, D2D1RenderTargetProperties(TargetMode, D2D1PixelFormat(DXGI_FORMAT_B8G8R8A8_UNORM, D2D1_ALPHA_MODE_PREMULTIPLIED)),
        RenderTarget);
    end
    else
    begin
      if FBuffered then
      begin
        if FHandle = 0 then
        begin
          if Assigned(ImagingFactory) then
          begin
            ImagingFactory.CreateBitmapFromMemory(FBitmap.Width, FBitmap.Height, GUID_WICPixelFormat32bppPBGRA,
              FBitmap.Width * 4, FBitmap.Width * FBitmap.Height * 4, PByte(FBitmap.Startline), WICBitmap);
            Factory.CreateWicBitmapRenderTarget(WICBitmap, D2D1RenderTargetProperties(TargetMode, D2D1PixelFormat(DXGI_FORMAT_B8G8R8A8_UNORM, D2D1_ALPHA_MODE_PREMULTIPLIED)),
              RenderTarget);
          end;
        end
        else
        begin
          Factory.CreateDCRenderTarget(D2D1RenderTargetProperties(TargetMode, D2D1PixelFormat(DXGI_FORMAT_B8G8R8A8_UNORM, D2D1_ALPHA_MODE_PREMULTIPLIED)),
            ID2D1DCRenderTarget(RenderTarget));
          ID2D1DCRenderTarget(RenderTarget).BindDC(FHandle, Rect(0, 0, FWidth, FHeight));
        end;
      end
      else
        Factory.CreateHWndRenderTarget(D2D1RenderTargetProperties(TargetMode, D2D1PixelFormat(DXGI_FORMAT_B8G8R8A8_UNORM, D2D1_ALPHA_MODE_PREMULTIPLIED)),
          D2D1HwndRenderTargetProperties(FParent, SizeInPixels),
          ID2D1HwndRenderTarget(RenderTarget));
    end;
    RenderTarget.SetDpi(96, 96);
    RenderTarget.SetTextAntialiasMode(D2D1_TEXT_ANTIALIAS_MODE_DEFAULT);
  end
end;

procedure TvgCanvasD2D.DisposeResources;
begin
  SAFE_RELEASE(StrokeBrush);
  SAFE_RELEASE(Brush);
  SAFE_RELEASE(Layer);
  SAFE_RELEASE(RenderTarget);
  SAFE_RELEASE(WICBitmap);
end;

procedure TvgCanvasD2D.Clear(const Color: TvgColor);
begin
  RenderTarget.Clear(D2Color(Color, 1));
end;

procedure TvgCanvasD2D.ClearRect(const ARect: TvgRect;
  const AColor: TvgColor);
begin
end;

procedure TvgCanvasD2D.ResizeBuffer(const AWidth, AHeight: integer);
var
  Size:D2D1_SIZE_U;
begin
  if Assigned(FScene) and (FScene.GetTransparency) and not FBuffered then
  begin
    FBuffered := true;
  end;
  if (AWidth = FWidth) and (AHeight = FHeight) then Exit;
  if (FParent <> 0) and not (FBuffered) and (RenderTarget <> nil) then
  begin
    FWidth := AWidth;
    FHeight := AHeight;
    if FWidth <= 0 then FWidth := 1;
    if FHeight <= 0 then FHeight := 1;
    Size.Width := AWidth;
    Size.Height := AHeight;
    ID2D1HwndRenderTarget(RenderTarget).Resize(Size);
    Exit;
  end;
  FreeBuffer;
  FWidth := AWidth;
  FHeight := AHeight;
  if FWidth <= 0 then FWidth := 1;
  if FHeight <= 0 then FHeight := 1;
  FResized := true;
  if FBuffered then
  begin
    { Initialization }
    with FBitmapInfo.bmiHeader do
    begin
      biSize := SizeOf(TBitmapInfoHeader);
      biPlanes := 1;
      biBitCount := 32;
      biCompression := BI_RGB;
      biWidth := AWidth;
      if biWidth <= 0 then biWidth := 1;
      biHeight := -AHeight;
      if biHeight >= 0 then biHeight := -1;
    end;
    { Create new DIB }
    FBufferHandle := CreateDIBSection(0, FBitmapInfo, DIB_RGB_COLORS, Pointer(FBufferBits), 0, 0);
    if FBufferBits = nil then
      raise Exception.Create('Can''t allocate the DIB handle');

    FHandle := CreateCompatibleDC(0);
    if FHandle = 0 then
    begin
      DeleteObject(FBufferHandle);
      FHandle := 0;
      FBufferBits := nil;
      raise Exception.Create('Can''t create compatible DC');
    end;

    if SelectObject(FHandle, FBufferHandle) = 0 then
    begin
      DeleteDC(FHandle);
      DeleteObject(FBufferHandle);
      FHandle := 0;
      FBufferHandle := 0;
      FBufferBits := nil;
      raise Exception.Create('Can''t select an object into DC');
    end;
  end;
end;

procedure TvgCanvasD2D.FlushBuffer(const X, Y: integer;
  const DC: Cardinal);
begin
  if FBufferHandle = 0 then Exit;
  if DC = 0 then Exit;
  Windows.BitBlt(DC, X, Y, FWidth, FHeight, FHandle, 0, 0, SRCCOPY);
end;

procedure TvgCanvasD2D.FlushBufferRect(const X, Y: integer;
  const DC: Cardinal; const ARect: TvgRect);
var
  R: TRect;
begin
  if FBufferHandle = 0 then Exit;
  if DC = 0 then Exit;
  R := Rect(trunc(ARect.left), trunc(ARect.top), trunc(ARect.right) + 1, trunc(ARect.bottom) + 1);
  with R do
  begin
    Windows.BitBlt(DC, X + R.left, Y + R.top, R.right - R.left, R.bottom - R.top, FHandle, R.left, R.top, SRCCOPY);
  end;
end;

function TvgCanvasD2D.BeginScene: boolean;
begin
  CreateResources;
  Result := inherited BeginScene and (RenderTarget <> nil);
  if Result then
  begin
    FLastState := -1;
    if (FParent <> 0) and not (FBuffered) then
      Result := (LongWord(ID2D1HwndRenderTarget(RenderTarget).CheckWindowState) and LongWord(D2D1_WINDOW_STATE_OCCLUDED) = 0);
    RenderTarget.BeginDraw;
  end;
end;

procedure TvgCanvasD2D.EndScene;
var
  Res: HResult;
begin
  inherited ;
  if RenderTarget <> nil then
  begin
    if Assigned(Layer) then
    begin
      RenderTarget.PopLayer;
      SAFE_RELEASE(Layer);
    end;
    Res := RenderTarget.EndDraw;
    if Res = D2DERR_RECREATE_TARGET then
    begin
      DisposeResources;
    end
    else
      if Res <> 0 then
      begin
        //Res := 10;
      end;
    if WICBitmap <> nil then
    begin
      Res := WICBitmap.CopyPixels(nil, Width * 4, Width * Height * 4, PByte(FBitmap.Startline));
    end;
  end;
end;

class function TvgCanvasD2D.GetBitmapScanline(Bitmap: TvgBitmap; y: integer): PvgColorArray;
begin
  if (y >= 0) and (y < Bitmap.Height) and (Bitmap.StartLine <> nil) then
    Result := @PvgColorArray(Bitmap.StartLine)[(y) * Bitmap.Width]
  else
    Result := nil;
end;

procedure TvgCanvasD2D.SetMatrix(const M: TvgMatrix);
begin
  FMatrix := M;
  RenderTarget.SetTransform(D2Matrix(FMatrix));
end;

procedure TvgCanvasD2D.MultyMatrix(const M: TvgMatrix);
var
  T: TvgMatrix;
begin
  inherited;
{  RenderTarget.GetTransform(D2D_MATRIX_3X2_F(T));
  M := vgMatrixMultipy(T, M);
  RenderTarget.SetTransform(D2Matrix(T));}
end;

function TvgCanvasD2D.SaveCanvas: cardinal;
var
  i: integer;
  B: ID2D1DrawingStateBlock;
begin
  Result := InvalideCanvasState;
  if RenderTarget = nil then Exit;

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
  Factory.CreateDrawingStateBlock(nil, nil, B);
  B._AddRef;
  RenderTarget.SaveDrawingState(B);

  FLastState := Result;
  FSaveData[Result].Index := Cardinal(B);
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
    Move(FDash[0], FSaveData[Result].Dash[0], SizeOf(FDash[0]) * Length(FDash));
  end;
  FSaveData[Result].DashOffset := FDashOffset;
  FSaveData[Result].Font := TvgFont.Create;
  FSaveData[Result].Font.Assign(Font);
  FSaveData[Result].Data := nil;
end;

procedure TvgCanvasD2D.RestoreCanvas(const AState: cardinal);
var
  B: ID2D1DrawingStateBlock;
begin
  if RenderTarget = nil then Exit;

  if (AState >= 0) and (AState < Length(FSaveData)) then
  begin
    B := ID2D1DrawingStateBlock(FSaveData[AState].Index);
    RenderTarget.RestoreDrawingState(B);
    B._Release;
    SAFE_RELEASE(B);

    if FSaveData[AState].Data <> nil then
    begin
      RenderTarget.PopLayer;
      SAFE_RELEASE(ID2D1Layer(FSaveData[AState].Data));
      FSaveData[AState].Data := nil;
    end;

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
      Move(FSaveData[AState].Dash[0], FDash[0], SizeOf(FDash[0]) * Length(FDash));
    end;
    FDashOffset := FSaveData[AState].DashOffset;
    Font.Assign(FSaveData[AState].Font);
    FSaveData[AState].Font.Free;
  end
end;

procedure TvgCanvasD2D.SetClipRects(const ARects: array of TvgRect);
var
  i: integer;
  Geoms: array of ID2D1Geometry;
  FClipGeom: ID2D1GeometryGroup;
  LayerPar: TD2D1_LayerParameters;
  R: TvgRect;
begin
  if Length(ARects) > 0 then
  begin
    RenderTarget.SetTransform(D2Matrix(IdentityMatrix));
    for i := 0 to High(ARects) do
    begin
      R := ARects[i];
      RenderTarget.PushAxisAlignedClip(D2Rect(R), D2D1_ANTIALIAS_MODE_ALIASED);
      RenderTarget.Clear(D2Color(0, 0));
      RenderTarget.PopAxisAlignedClip;
    end;
    SAFE_RELEASE(FClipGeom);
    SetLength(Geoms, Length(ARects));
    for i := 0 to High(ARects) do
    begin
      Factory.CreateRectangleGeometry(D2Rect(ARects[i]), ID2D1RectangleGeometry(Geoms[i]));
    end;
    Factory.CreateGeometryGroup(D2D1_FILL_MODE_WINDING, @Geoms[0], Length(Geoms), FClipGeom);
    for i := 0 to High(ARects) do
    begin
      SAFE_RELEASE(Geoms[i]);
    end;

    RenderTarget.CreateLayer(nil, Layer);
    LayerPar.ContentBounds := D2Rect(vgRect(0, 0, FWidth, FHeight));
    LayerPar.GeometricMask := FClipGeom;
    LayerPar.MaskAntialiasMode := D2D1_ANTIALIAS_MODE_ALIASED;
    LayerPar.MaskTransform := D2Matrix(IdentityMatrix);
    LayerPar.Opacity := 1;
    LayerPar.OpacityBrush := nil;
    LayerPar.LayerOptions := D2D1_LAYER_OPTIONS_NONE;
    RenderTarget.PushLayer(LayerPar, Layer);
    SAFE_RELEASE(FClipGeom);
  end;
end;

procedure TvgCanvasD2D.IntersectClipRect(const ARect: TvgRect);
var
  Geom: ID2D1Geometry;
  LayerPar: TD2D1_LayerParameters;
begin
  if FLastState < 0 then Exit;
  Factory.CreateRectangleGeometry(D2Rect(ARect), ID2D1RectangleGeometry(Geom));
  if Assigned(Geom) then
  begin
    RenderTarget.CreateLayer(nil, ID2D1Layer(FSaveData[FLastState].Data));
    LayerPar.ContentBounds := D2Rect(vgRect(0, 0, 100000, 400000));
    LayerPar.GeometricMask := Geom;
    LayerPar.MaskAntialiasMode := D2D1_ANTIALIAS_MODE_ALIASED;
    LayerPar.MaskTransform := D2Matrix(IdentityMatrix);
    LayerPar.Opacity := 1;
    LayerPar.OpacityBrush := nil;
    LayerPar.LayerOptions := D2D1_LAYER_OPTIONS_NONE;
    RenderTarget.PushLayer(LayerPar, ID2D1Layer(FSaveData[FLastState].Data));
    SAFE_RELEASE(Geom);
  end;
end;

procedure TvgCanvasD2D.ExcludeClipRect(const ARect: TvgRect);
var
  i: integer;
  Geoms: array [0..3] of ID2D1Geometry;
  R: TvgRect;
  RR: array [0..3] of TvgRect;
  GeomGroup: ID2D1GeometryGroup;
  LayerPar: TD2D1_LayerParameters;
begin
  if FLastState < 0 then Exit;
  R := ARect;
  RR[0] := vgRect(0, 0, R.Left, FHeight);
  RR[1] := vgRect(R.Right, 0, FWidth, FHeight);
  RR[2] := vgRect(R.Left, 0, R.Right, R.Top);
  RR[3] := vgRect(R.Left, R.Bottom, R.Right, FHeight);
  for i := 0 to High(RR) do
  begin
    Factory.CreateRectangleGeometry(D2Rect(RR[i]), ID2D1RectangleGeometry(Geoms[i]));
  end;
  Factory.CreateGeometryGroup(D2D1_FILL_MODE_WINDING, @Geoms[0], Length(Geoms), GeomGroup);
  for i := 0 to High(RR) do
  begin
    SAFE_RELEASE(Geoms[i]);
  end;
  if Assigned(GeomGroup) then
  begin
    RenderTarget.CreateLayer(nil, ID2D1Layer(FSaveData[FLastState].Data));
    LayerPar.ContentBounds := D2Rect(vgRect(0, 0, FWidth, FHeight));
    LayerPar.GeometricMask := GeomGroup;
    LayerPar.MaskAntialiasMode := D2D1_ANTIALIAS_MODE_ALIASED;
    LayerPar.MaskTransform := D2Matrix(IdentityMatrix);
    LayerPar.Opacity := 1;
    LayerPar.OpacityBrush := nil;
    LayerPar.LayerOptions := D2D1_LAYER_OPTIONS_NONE;
    RenderTarget.PushLayer(LayerPar, ID2D1Layer(FSaveData[FLastState].Data));
    SAFE_RELEASE(GeomGroup);
  end;
end;

procedure TvgCanvasD2D.ResetClipRect;
begin
end;

procedure TvgCanvasD2D.ApplyFill(ARect: TvgRect; const AOpacity: single);
var
  C: TvgColor;
  i: integer;
  count: integer;
  B: TvgBitmap;
  M: TvgMatrix;
  grad: array [0..100] of TD2D1_GradientStop;
  gradcol: ID2D1GradientStopCollection;
  gradbrushprop: TD2D1_LinearGradientBrushProperties;
  rgradbrushprop: TD2D1_RadialGradientBrushProperties;
  bitmapbrushprop: TD2D1_BitmapBrushProperties;
  brushprop: TD2D1_BrushProperties;
  d2dbmp: ID2D1Bitmap;
begin
  SAFE_RELEASE(Brush);
  if (FFill.Style = vgBrushResource) and (FFill.Resource <> nil) and (FFill.Resource.Brush <> nil) then
    FFill.Assign(FFill.Resource.Brush);

  with FFill do
  begin
    case Style of
      vgBrushSolid:
        begin
          RenderTarget.CreateSolidColorBrush(D2Color(FFill.SolidColor, AOpacity), nil, ID2D1SolidColorBrush(Brush));
        end;
      vgBrushGradient:
        begin
          if Gradient.Points.Count > 1 then
          begin
            count := 0;
            if Gradient.Points[0].Offset > 0 then
            begin
              grad[count].Color := D2Color(vgOpacity(Gradient.Points[0].IntColor, AOpacity), 1);
              grad[count].Position := 0;
              count := count + 1;
            end;
            for i := 0 to Gradient.Points.Count - 1 do
            begin
              grad[i + count].Color := D2Color(vgOpacity(Gradient.Points[i].IntColor, AOpacity), 1);
              grad[i + count].Position := Gradient.Points[i].Offset;
            end;
            if Gradient.Points[Gradient.Points.Count - 1].Offset < 1 then
            begin
              count := count + 1;
              grad[Gradient.Points.Count + count - 1].Color := D2Color(vgOpacity(Gradient.Points[Gradient.Points.Count - 1].IntColor, AOpacity), 1);
              grad[Gradient.Points.Count + count - 1].Position := 1;
            end;

            if Gradient.Style = vgLinearGradient then
            begin
              { Linear }
              RenderTarget.CreateGradientStopCollection(@grad[0], Gradient.Points.Count + count, D2D1_GAMMA_2_2, D2D1_EXTEND_MODE_CLAMP, gradcol);
              gradbrushprop.StartPoint := D2Point(ARect.Left + Gradient.StartPosition.X * ARect.Right, ARect.Top + Gradient.StartPosition.Y * ARect.Bottom);
              gradbrushprop.EndPoint := D2Point(ARect.Left + Gradient.StopPosition.X * ARect.Right, ARect.Top + Gradient.StopPosition.Y * ARect.Bottom);
              RenderTarget.CreateLinearGradientBrush(
                gradbrushprop,
                nil,
                gradcol,
                ID2D1LinearGradientBrush(Brush)
                );
              gradcol := nil;
            end
            else
            begin
              { Radial }
              for i := 0 to Gradient.Points.Count + count - 1 do
                grad[i].Position := 1 - grad[i].Position;
              RenderTarget.CreateGradientStopCollection(@grad[0], Gradient.Points.Count + count, D2D1_GAMMA_2_2, D2D1_EXTEND_MODE_CLAMP, gradcol);
              rgradbrushprop.GradientOriginOffset := TD2D1_Point2F(vgPoint(0, 0));
              rgradbrushprop.Center := TD2D1_Point2F(vgPoint(Gradient.RadialTransform.RotateCenter.X * vgRectWidth(ARect),
                Gradient.RadialTransform.RotateCenter.Y * vgRectHeight(ARect)));
              rgradbrushprop.RadiusX := vgRectWidth(ARect) / 2;
              rgradbrushprop.RadiusY := vgRectHeight(ARect) / 2;
              RenderTarget.CreateRadialGradientBrush(
                rgradbrushprop,
                nil,
                gradcol,
                ID2D1RadialGradientBrush(Brush)
                );
              gradcol := nil;
            end;
          end
          else
            RenderTarget.CreateSolidColorBrush(D2Color(0, 0), nil, ID2D1SolidColorBrush(Brush));
        end;
      vgBrushResource:
        begin
        end;
      vgBrushVisual:
        begin
        end;
      vgBrushBitmap:
        begin
          B := Bitmap.Bitmap;
          if (B <> nil) and (B.ResourceBitmap <> nil) then
            B := B.ResourceBitmap;
          if (B <> nil) and (B.Width > 0) and (B.Height > 0) then
          begin
            bitmapbrushprop.InterpolationMode := D2D1_BITMAP_INTERPOLATION_MODE_LINEAR;
            brushprop.Opacity := AOpacity;
            M := IdentityMatrix;
            case Bitmap.WrapMode of
              vgWrapTile:
                begin
                  bitmapbrushprop.ExtendModeX := D2D1_EXTEND_MODE_WRAP;
                  bitmapbrushprop.ExtendModeY := D2D1_EXTEND_MODE_WRAP;
                end;
              vgWrapTileOriginal:
                begin
                  bitmapbrushprop.ExtendModeX := D2D1_EXTEND_MODE_CLAMP;
                  bitmapbrushprop.ExtendModeY := D2D1_EXTEND_MODE_CLAMP;
                end;
              vgWrapTileStretch:
                begin
                  bitmapbrushprop.ExtendModeX := D2D1_EXTEND_MODE_WRAP;
                  bitmapbrushprop.ExtendModeY := D2D1_EXTEND_MODE_WRAP;
                  M.m11 := (vgRectWidth(ARect) + (StrokeThickness / 2)) / B.Width;
                  M.m22 := (vgRectHeight(ARect) + (StrokeThickness / 2)) / B.Height;
                end;
            end;
            brushprop.Transform := D2Matrix(M);
            Res := RenderTarget.CreateBitmap(D2Size(B.Width, B.Height), B.StartLine,
              B.Width * 4, BitmapProp(DXGI_FORMAT_B8G8R8A8_UNORM, D2D1_ALPHA_MODE_PREMULTIPLIED), d2dbmp);
            RenderTarget.CreateBitmapBrush(
              d2dbmp,
              @bitmapbrushprop,
              @brushprop,
              ID2D1BitmapBrush(Brush));
          end
          else
            RenderTarget.CreateSolidColorBrush(D2Color(0, 0), nil, ID2D1SolidColorBrush(Brush));
        end;
    else
      RenderTarget.CreateSolidColorBrush(D2Color(0, 0), nil, ID2D1SolidColorBrush(Brush));
    end;
  end;
end;

procedure TvgCanvasD2D.ApplyStroke(ARect: TvgRect; const AOpacity: single);
var
  i: integer;
  StyleProp: TD2D1_StrokeStyleProperties;
begin
  SAFE_RELEASE(StrokeBrush);
  SAFE_RELEASE(StrokeStyle);
  if (FStroke.Style = vgBrushResource) and (FStroke.Resource <> nil) and (FStroke.Resource.Brush <> nil) then
    FStroke.Assign(FStroke.Resource.Brush);

  with FStroke do
  begin
    case Style of
      vgBrushSolid, vgBrushGradient, vgBrushBitmap:
        begin
          RenderTarget.CreateSolidColorBrush(D2Color(FStroke.SolidColor, AOpacity), nil, ID2D1SolidColorBrush(StrokeBrush));
        end;
    else
      RenderTarget.CreateSolidColorBrush(D2Color(0, 0), nil, ID2D1SolidColorBrush(StrokeBrush));
    end;
  end;

  case StrokeCap of
    vgCapFlat: StyleProp.DashCap := D2D1_CAP_STYLE_SQUARE;
    vgCapRound: StyleProp.DashCap := D2D1_CAP_STYLE_ROUND;
  end;
  StyleProp.StartCap := StyleProp.DashCap;
  StyleProp.EndCap := StyleProp.DashCap;
  case StrokeJoin of
    vgJoinMiter: StyleProp.LineJoin := D2D1_LINE_JOIN_MITER;
    vgJoinRound: StyleProp.LineJoin := D2D1_LINE_JOIN_ROUND;
    vgJoinBevel: StyleProp.LineJoin := D2D1_LINE_JOIN_BEVEL;
  end;
  StyleProp.MiterLimit := 10;
  StyleProp.DashOffset := FDashOffset;
  StyleProp.DashStyle := TD2D1_DashStyle(StrokeDash);

  if StrokeDash = vgDashCustom then
    Factory.CreateStrokeStyle(StyleProp, @FDash[0], Length(FDash), StrokeStyle)
  else
    Factory.CreateStrokeStyle(StyleProp, nil, 0, StrokeStyle)
end;

procedure TvgCanvasD2D.FontChanged(Sender: TObject);
begin
end;

procedure TvgCanvasD2D.DrawLine(const APt1, APt2: TvgPoint; const AOpacity: single);
begin
  if FStroke.Style <> vgBrushNone then
  begin
    ApplyStroke(vgRect(APt1.X, APt1.Y, APt2.X, APt2.Y), AOpacity);
    RenderTarget.DrawLine(D2Point(APt1.X, APt1.Y), D2Point(APt2.X, APt2.Y), StrokeBrush, StrokeThickness, StrokeStyle);
  end;
end;

procedure TvgCanvasD2D.IntFillRect(R: TvgRect; Opacity: single);
begin
  if FFill.Style <> vgBrushNone then
  begin
    if FFill.Style = vgBrushVisual then
    begin
    end
    else
    begin
      ApplyFill(R, Opacity);
      RenderTarget.FillRectangle(D2Rect(R), Brush);
    end;
  end;
end;

procedure TvgCanvasD2D.IntFillPath(P: ID2D1Geometry; R: TvgRect;
  Opacity: single);
begin
  if FFill.Style <> vgBrushNone then
  begin
    if FFill.Style = vgBrushVisual then
    begin
    end
    else
    begin
      ApplyFill(R, Opacity);
      RenderTarget.FillGeometry(P, Brush);
    end;
  end;
end;

procedure TvgCanvasD2D.DrawRect(const ARect: TvgRect; const xRadius, yRadius: single; const ACorners: TvgCorners;
  const AOpacity: single; const ACornerType: TvgCornerType = vgCornerRound);
var
  Geometry: ID2D1PathGeometry;
  Path: ID2D1GeometrySink;
  x1, x2, y1, y2: single;
  R: TvgRect;
begin
  if FStroke.Style <> vgBrushNone then
  begin
    R := ARect;
    ApplyStroke(R, AOpacity);
    if (xRadius < Epsilon) and (yRadius < Epsilon) then
    begin
      RenderTarget.DrawRectangle(D2Rect(ARect), StrokeBrush, StrokeThickness, StrokeStyle);
    end
    else
    begin
      x1 := xRadius;
      if vgRectWidth(R) - (x1 * 2) < 0 then
        x1 := (xRadius * (vgRectWidth(R) / (x1 * 2)));
      x2 := x1 * CurveKappaInv;
      y1 := yRadius;
      if vgRectHeight(R) - (y1 * 2) < 0 then
        y1 := (yRadius * (vgRectHeight(R) / (y1 * 2)));
      y2 := y1 * CurveKappaInv;
      Factory.CreatePathGeometry(Geometry);
      Geometry.Open(Path);
      Path.BeginFigure(D2Point(R.Left, R.Top + y1), D2D1_FIGURE_BEGIN_FILLED);
      if vgCornerTopLeft in ACorners then
      begin
        case ACornerType of
          // vgCornetRound - default
          vgCornerBevel: Path.AddLine(D2Point(R.Left + x1, R.Top));
          vgCornerInnerRound: Path.AddBezier(D2Bezier(R.Left + x2, R.Top + y1, R.Left + x1, R.Top + y2, R.Left + x1, R.Top));
          vgCornerInnerLine:
            begin
              Path.AddLine(D2Point(R.Left + x2, R.Top + y1));
              Path.AddLine(D2Point(R.Left + x1, R.Top + y2));
              Path.AddLine(D2Point(R.Left + x1, R.Top));
            end;
        else
          Path.AddBezier(D2Bezier(R.Left, R.Top + (y2), R.Left + x2, R.Top, R.Left + x1, R.Top))
        end;
      end
      else
      begin
        Path.AddLine(D2Point(R.Left, R.Top));
        Path.AddLine(D2Point(R.Left + x1, R.Top));
      end;
      Path.AddLine(D2Point(R.Right - x1, R.Top));
      if vgCornerTopRight in ACorners then
      begin
        case ACornerType of
          // vgCornetRound - default
          vgCornerBevel: Path.AddLine(D2Point(R.Right, R.Top + y1));
          vgCornerInnerRound: Path.AddBezier(D2Bezier(R.Right - x1, R.Top + y2, R.Right - x2, R.Top + y1, R.Right, R.Top + y1));
          vgCornerInnerLine:
            begin
              Path.AddLine(D2Point(R.Right - x1, R.Top + y2));
              Path.AddLine(D2Point(R.Right - x2, R.Top + y1));
              Path.AddLine(D2Point(R.Right, R.Top + y1));
            end;
        else
          Path.AddBezier(D2Bezier(R.Right - x2, R.Top, R.Right, R.Top + (y2), R.Right, R.Top + y1))
        end;
      end
      else
      begin
        Path.AddLine(D2Point(R.Right, R.Top));
        Path.AddLine(D2Point(R.Right, R.Top + y1));
      end;
      Path.AddLine(D2Point(R.Right, R.Bottom - y1));
      if vgCornerBottomRight in ACorners then
      begin
        case ACornerType of
          // vgCornetRound - default
          vgCornerBevel: Path.AddLine(D2Point(R.Right - x1, R.Bottom));
          vgCornerInnerRound: Path.AddBezier(D2Bezier(R.Right - x2, R.Bottom - y1, R.Right - x1, R.Bottom - y2, R.Right - x1, R.Bottom));
          vgCornerInnerLine:
            begin
              Path.AddLine(D2Point(R.Right - x2, R.Bottom - y1));
              Path.AddLine(D2Point(R.Right - x1, R.Bottom - y2));
              Path.AddLine(D2Point(R.Right - x1, R.Bottom));
            end;
        else
          Path.AddBezier(D2Bezier(R.Right, R.Bottom - (y2), R.Right - x2, R.Bottom, R.Right - x1, R.Bottom))
        end;
      end
      else
      begin
        Path.AddLine(D2Point(R.Right, R.Bottom));
        Path.AddLine(D2Point(R.Right - x1, R.Bottom));
      end;
      Path.AddLine(D2Point(R.Left + x1, R.Bottom));
      if vgCornerBottomLeft in ACorners then
      begin
        case ACornerType of
          // vgCornetRound - default
          vgCornerBevel: Path.AddLine(D2Point(R.Left, R.Bottom - y1));
          vgCornerInnerRound: Path.AddBezier(D2Bezier(R.Left + x1, R.Bottom - y2, R.Left + x2, R.Bottom - y1, R.Left, R.Bottom - y1));
          vgCornerInnerLine:
            begin
              Path.AddLine(D2Point(R.Left + x1, R.Bottom - y2));
              Path.AddLine(D2Point(R.Left + x2, R.Bottom - y1));
              Path.AddLine(D2Point(R.Left, R.Bottom - y1));
            end;
        else
          Path.AddBezier(D2Bezier(R.Left + x2, R.Bottom, R.Left, R.Bottom - (y2), R.Left, R.Bottom - y1))
        end;
      end
      else
      begin
        Path.AddLine(D2Point(R.Left, R.Bottom));
        Path.AddLine(D2Point(R.Left, R.Bottom - y1));
      end;
      Path.EndFigure(D2D1_FIGURE_END_CLOSED);
      Path.Close;
      SAFE_RELEASE(Path);
      RenderTarget.DrawGeometry(Geometry, StrokeBrush, StrokeThickness, StrokeStyle);
      SAFE_RELEASE(Geometry);
    end;
  end;
end;

procedure TvgCanvasD2D.FillRect(const ARect: TvgRect; const xRadius, yRadius: single; const ACorners: TvgCorners; const AOpacity: single;
  const ACornerType: TvgCornerType = vgCornerRound);
var
  Geometry: ID2D1PathGeometry;
  Path: ID2D1GeometrySink;
  x1, x2, y1, y2: single;
  R: TvgRect;
begin
  if FFill.Style <> vgBrushNone then
  begin
    if ((xRadius = 0) and (yRadius = 0)) or (ACorners = []) then
    begin
      IntFillRect(ARect, AOpacity);
    end
    else
    begin
      R := ARect;
      x1 := xRadius;
      if vgRectWidth(R) - (x1 * 2) < 0 then
        x1 := (xRadius * (vgRectWidth(R) / (x1 * 2)));
      x2 := x1 * CurveKappaInv;
      y1 := yRadius;
      if vgRectHeight(R) - (y1 * 2) < 0 then
        y1 := (yRadius * (vgRectHeight(R) / (y1 * 2)));
      y2 := y1 * CurveKappaInv;
      Factory.CreatePathGeometry(Geometry);
      Geometry.Open(Path);
      Path.BeginFigure(D2Point(R.Left, R.Top + y1), D2D1_FIGURE_BEGIN_FILLED);
      if vgCornerTopLeft in ACorners then
      begin
        case ACornerType of
          // vgCornetRound - default
          vgCornerBevel: Path.AddLine(D2Point(R.Left + x1, R.Top));
          vgCornerInnerRound: Path.AddBezier(D2Bezier(R.Left + x2, R.Top + y1, R.Left + x1, R.Top + y2, R.Left + x1, R.Top));
          vgCornerInnerLine:
            begin
              Path.AddLine(D2Point(R.Left + x2, R.Top + y1));
              Path.AddLine(D2Point(R.Left + x1, R.Top + y2));
              Path.AddLine(D2Point(R.Left + x1, R.Top));
            end;
        else
          Path.AddBezier(D2Bezier(R.Left, R.Top + (y2), R.Left + x2, R.Top, R.Left + x1, R.Top))
        end;
      end
      else
      begin
        Path.AddLine(D2Point(R.Left, R.Top));
        Path.AddLine(D2Point(R.Left + x1, R.Top));
      end;
      Path.AddLine(D2Point(R.Right - x1, R.Top));
      if vgCornerTopRight in ACorners then
      begin
        case ACornerType of
          // vgCornetRound - default
          vgCornerBevel: Path.AddLine(D2Point(R.Right, R.Top + y1));
          vgCornerInnerRound: Path.AddBezier(D2Bezier(R.Right - x1, R.Top + y2, R.Right - x2, R.Top + y1, R.Right, R.Top + y1));
          vgCornerInnerLine:
            begin
              Path.AddLine(D2Point(R.Right - x1, R.Top + y2));
              Path.AddLine(D2Point(R.Right - x2, R.Top + y1));
              Path.AddLine(D2Point(R.Right, R.Top + y1));
            end;
        else
          Path.AddBezier(D2Bezier(R.Right - x2, R.Top, R.Right, R.Top + (y2), R.Right, R.Top + y1))
        end;
      end
      else
      begin
        Path.AddLine(D2Point(R.Right, R.Top));
        Path.AddLine(D2Point(R.Right, R.Top + y1));
      end;
      Path.AddLine(D2Point(R.Right, R.Bottom - y1));
      if vgCornerBottomRight in ACorners then
      begin
        case ACornerType of
          // vgCornetRound - default
          vgCornerBevel: Path.AddLine(D2Point(R.Right - x1, R.Bottom));
          vgCornerInnerRound: Path.AddBezier(D2Bezier(R.Right - x2, R.Bottom - y1, R.Right - x1, R.Bottom - y2, R.Right - x1, R.Bottom));
          vgCornerInnerLine:
            begin
              Path.AddLine(D2Point(R.Right - x2, R.Bottom - y1));
              Path.AddLine(D2Point(R.Right - x1, R.Bottom - y2));
              Path.AddLine(D2Point(R.Right - x1, R.Bottom));
            end;
        else
          Path.AddBezier(D2Bezier(R.Right, R.Bottom - (y2), R.Right - x2, R.Bottom, R.Right - x1, R.Bottom))
        end;
      end
      else
      begin
        Path.AddLine(D2Point(R.Right, R.Bottom));
        Path.AddLine(D2Point(R.Right - x1, R.Bottom));
      end;
      Path.AddLine(D2Point(R.Left + x1, R.Bottom));
      if vgCornerBottomLeft in ACorners then
      begin
        case ACornerType of
          // vgCornetRound - default
          vgCornerBevel: Path.AddLine(D2Point(R.Left, R.Bottom - y1));
          vgCornerInnerRound: Path.AddBezier(D2Bezier(R.Left + x1, R.Bottom - y2, R.Left + x2, R.Bottom - y1, R.Left, R.Bottom - y1));
          vgCornerInnerLine:
            begin
              Path.AddLine(D2Point(R.Left + x1, R.Bottom - y2));
              Path.AddLine(D2Point(R.Left + x2, R.Bottom - y1));
              Path.AddLine(D2Point(R.Left, R.Bottom - y1));
            end;
        else
          Path.AddBezier(D2Bezier(R.Left + x2, R.Bottom, R.Left, R.Bottom - (y2), R.Left, R.Bottom - y1))
        end;
      end
      else
      begin
        Path.AddLine(D2Point(R.Left, R.Bottom));
        Path.AddLine(D2Point(R.Left, R.Bottom - y1));
      end;
      Path.EndFigure(D2D1_FIGURE_END_CLOSED);
      Path.Close;
      SAFE_RELEASE(Path);
      IntFillPath(Geometry, ARect, AOpacity);
      SAFE_RELEASE(Geometry);
    end;
  end;
end;

procedure TvgCanvasD2D.DrawEllipse(const ARect: TvgRect; const AOpacity: single);
var
  R: TvgRect;
begin
  if FStroke.Style <> vgBrushNone then
  begin
    ApplyStroke(ARect, AOpacity);
    RenderTarget.DrawEllipse(D2Ellipse(ARect), StrokeBrush, StrokeThickness, StrokeStyle);
  end;
end;

procedure TvgCanvasD2D.FillEllipse(const ARect: TvgRect; const AOpacity: single);
var
  Geometry: ID2D1EllipseGeometry;
begin
  if FFill.Style <> vgBrushNone then
  begin
    if FFill.Style <> vgBrushVisual then
    begin
      ApplyFill(ARect, AOpacity);
      RenderTarget.FillEllipse(D2Ellipse(ARect), Brush);
    end
    else
    begin
      Factory.CreateEllipseGeometry(D2Ellipse(ARect), Geometry);
      IntFillPath(Geometry, ARect, AOpacity);
      SAFE_RELEASE(Geometry);
    end;
  end;
end;

procedure TvgCanvasD2D.FillText(const ARect, AClipRect: TvgRect; const AText: WideString; const WordWrap: boolean;
  const AOpacity: single; const ATextAlign: TvgTextAlign; const AVTextAlign: TvgTextAlign = vgTextAlignCenter);
var
  TextRange: TDWrite_TextRange;
  TextLayout: IDWriteTextLayout;
  TextFormat: IDWriteTextFormat;
  R: TvgRect;
  S: TvgFontStyle;
  WS: WideString;
begin
  if (FFill.Style <> vgBrushNone) and (AText <> '') then
  begin
    WS := FFont.Family;
    DWriteFactory.CreateTextFormat(PWideChar(WS), nil, D2FontWeight(FFont.Style),
      D2FontStyle(FFont.Style), DWRITE_FONT_STRETCH_NORMAL, FFont.Size, 'en-us', TextFormat);
    DWriteFactory.CreateTextLayout(PWideChar(AText), Length(AText), TextFormat, vgRectWidth(ARect), vgRectHeight(ARect),
      TextLayout);

    TextRange.startPosition := 0;
    TextRange.length := Length(AText);

    if not WordWrap then
      TextLayout.SetWordWrapping(DWRITE_WORD_WRAPPING_NO_WRAP)
    else
      TextLayout.SetWordWrapping(DWRITE_WORD_WRAPPING_WRAP);

    if FFont.Style = vgFontStrikeout then
      TextLayout.SetStrikethrough(True, TextRange);

    if FFont.Style = vgFontUnderline then
      TextLayout.SetUnderline(True, TextRange);

    // formating
    case AVTextAlign of
      vgTextAlignCenter:
        begin
          TextLayout.SetParagraphAlignment(DWRITE_PARAGRAPH_ALIGNMENT_CENTER);
        end;
      vgTextAlignNear:
        begin
          TextLayout.SetParagraphAlignment(DWRITE_PARAGRAPH_ALIGNMENT_NEAR);
        end;
      vgTextAlignFar:
        begin
          TextLayout.SetParagraphAlignment(DWRITE_PARAGRAPH_ALIGNMENT_FAR);
        end;
    end;
    case ATextAlign of
      vgTextAlignCenter:
        begin
          TextLayout.SetTextAlignment(DWRITE_TEXT_ALIGNMENT_CENTER);
        end;
      vgTextAlignNear:
        begin
          TextLayout.SetTextAlignment(DWRITE_TEXT_ALIGNMENT_LEADING);
        end;
      vgTextAlignFar:
        begin
          TextLayout.SetTextAlignment(DWRITE_TEXT_ALIGNMENT_TRAILING);
        end;
    end;
    // calc correct rect
    ApplyFill(ARect, AOpacity);
    // draw
    RenderTarget.DrawTextLayout(D2Point(ARect.Left, ARect.Top), TextLayout, Brush, D2D1_DRAW_TEXT_OPTIONS_CLIP);
    SAFE_RELEASE(TextFormat);
    SAFE_RELEASE(TextLayout);
  end;
end;

procedure TvgCanvasD2D.MeasureText(var ARect: TvgRect;
  AClipRect: TvgRect; const AText: WideString; const WordWrap: boolean;
  const ATextAlign, AVTextAlign: TvgTextAlign);
var
  TextRange: TDWrite_TextRange;
  TextLayout: IDWriteTextLayout;
  TextMetrics: TDWrite_TextMetrics;
  TextFormat: IDWriteTextFormat;
  R: TvgRect;
  S: TvgFontStyle;
  WS: WideString;
begin
  if not WordWrap then
    ARect.Right := ARect.Left;
  if Length(AText) = 0 then Exit;

  WS := FFont.Family;
  DWriteFactory.CreateTextFormat(PWideChar(WS), nil, D2FontWeight(FFont.Style),
    D2FontStyle(FFont.Style), DWRITE_FONT_STRETCH_NORMAL, FFont.Size, 'en-us', TextFormat);
  DWriteFactory.CreateTextLayout(PWideChar(AText), Length(AText), TextFormat, vgRectWidth(ARect), vgRectHeight(ARect),
    TextLayout);

  TextRange.startPosition := 0;
  TextRange.length := Length(AText);

  if not WordWrap then
    TextLayout.SetWordWrapping(DWRITE_WORD_WRAPPING_NO_WRAP)
  else
    TextLayout.SetWordWrapping(DWRITE_WORD_WRAPPING_WRAP);

  if FFont.Style = vgFontStrikeout then
    TextLayout.SetStrikethrough(True, TextRange);

  if FFont.Style = vgFontUnderline then
    TextLayout.SetUnderline(True, TextRange);

  // formating
  case AVTextAlign of
    vgTextAlignCenter:
      begin
        TextLayout.SetParagraphAlignment(DWRITE_PARAGRAPH_ALIGNMENT_CENTER);
      end;
    vgTextAlignNear:
      begin
        TextLayout.SetParagraphAlignment(DWRITE_PARAGRAPH_ALIGNMENT_NEAR);
      end;
    vgTextAlignFar:
      begin
        TextLayout.SetParagraphAlignment(DWRITE_PARAGRAPH_ALIGNMENT_FAR);
      end;
  end;
  case ATextAlign of
    vgTextAlignCenter:
      begin
        TextLayout.SetTextAlignment(DWRITE_TEXT_ALIGNMENT_CENTER);
      end;
    vgTextAlignNear:
      begin
        TextLayout.SetTextAlignment(DWRITE_TEXT_ALIGNMENT_LEADING);
      end;
    vgTextAlignFar:
      begin
        TextLayout.SetTextAlignment(DWRITE_TEXT_ALIGNMENT_TRAILING);
      end;
  end;
  // measure
  TextLayout.GetMetrics(TextMetrics);
  ARect := vgRect(ARect.Left + TextMetrics.Left, ARect.Top + TextMetrics.Top, ARect.Left + TextMetrics.Left + TextMetrics.widthIncludingTrailingWhitespace,
    ARect.Top + TextMetrics.Top + TextMetrics.height);

  SAFE_RELEASE(TextFormat);
  SAFE_RELEASE(TextLayout);
end;

{ Bitmaps }

procedure TvgCanvasD2D.DrawThumbnail(const ABitmap: TvgBitmap;
  const Width, Height: single);
var
  scale: single;
begin
//  UpdateBitmap(ABitmap);
  if ABitmap.Handle = 0 then Exit;
  scale := Width / ABitmap.Width;
  if FBitmap <> nil then
  begin
//    Fgraphics.ScaleTransform(scale, scale);
//    RenderTarget.DrawBitmap(ID2D1Bitmap(ABitmap.Handle), @DR, AOpacity, IntMode, @SR);
  end;
end;

procedure TvgCanvasD2D.DrawBitmap(const ABitmap: TvgBitmap;
  const SrcRect, DstRect: TvgRect; const AOpacity: single; const HighSpeed: boolean = false);
var
  SR, DR: TD2D1_RectF;
  IntMode: TD2D1_BitmapInterpolationMode;
  B: ID2D1Bitmap;
begin
  if HighSpeed then
    IntMode := D2D1_BITMAP_INTERPOLATION_MODE_NEAREST_NEIGHBOR
  else
    IntMode := D2D1_BITMAP_INTERPOLATION_MODE_LINEAR;

  SR := D2Rect(SrcRect);
  DR := D2Rect(DstRect);

  Res := RenderTarget.CreateBitmap(D2Size(ABitmap.Width, ABitmap.Height), ABitmap.StartLine,
    ABitmap.Width * 4, BitmapProp(DXGI_FORMAT_B8G8R8A8_UNORM, D2D1_ALPHA_MODE_PREMULTIPLIED), B);
  RenderTarget.DrawBitmap(B, @DR, AOpacity, IntMode, @SR);
end;

{ Path }

procedure TvgCanvasD2D.DrawPath(const APath: TvgPathData; const ARect: TvgRect; const AOpacity: single);
var
  i: integer;
  B: TvgRect;
  W, H, NewW, NewH: single;
  CP1, CP2: TvgPoint;
  Geometry: ID2D1PathGeometry;
  Path: ID2D1GeometrySink;
  Closed: boolean;
begin
  if FStroke.Style = vgBrushNone then Exit;
  if APath.IsEmpty then Exit;

  ApplyStroke(ARect, AOpacity);

  B := APath.GetBounds;
  W := vgRectWidth(B);
  H := vgRectHeight(B);
  NewW := vgRectWidth(ARect);
  NewH := vgRectHeight(ARect);

  Factory.CreatePathGeometry(Geometry);
  Geometry.Open(Path);
  i := 0;
  Closed := false;
  while i < Length(APath.PathData) do
  begin
    case APath.PathData[i].Kind of
      vgPathPointMoveTo:
        begin
          if (i > 0) and (APath.PathData[i - 1].Kind <> vgPathPointClose) then
            Path.EndFigure(D2D1_FIGURE_END_OPEN);
          Path.BeginFigure(D2Point(ARect.Left + (APath.PathData[i].Point.X - B.Left) / W * NewW,
            ARect.Top + (APath.PathData[i].Point.Y - B.Top) / H * NewH), D2D1_FIGURE_BEGIN_FILLED);
        end;
      vgPathPointLineTo:
        begin
          Path.AddLine(D2Point(ARect.Left + (APath.PathData[i].Point.X - B.Left) / W * NewW,
            ARect.Top + (APath.PathData[i].Point.Y - B.Top) / H * NewH));
        end;
      vgPathPointCurveTo:
        begin
          CP1 := vgPoint(ARect.Left + (APath.PathData[i].Point.X - B.Left) / W * NewW,
            ARect.Top + (APath.PathData[i].Point.Y - B.Top) / H * NewH);
          Inc(i);
          CP2 := vgPoint(ARect.Left + (APath.PathData[i].Point.X - B.Left) / W * NewW,
            ARect.Top + (APath.PathData[i].Point.Y - B.Top) / H * NewH);
          Inc(i);
          Path.AddBezier(D2Bezier(CP1.X,
            CP1.Y,
            CP2.X,
            CP2.Y,
            ARect.Left + (APath.PathData[i].Point.X - B.Left) / W * NewW,
            ARect.Top + (APath.PathData[i].Point.Y - B.Top) / H * NewH));
        end;
      vgPathPointClose:
        begin
          Path.EndFigure(D2D1_FIGURE_END_CLOSED);
          Closed := true;
        end;
    end;
    inc(i);
  end;

  if not Closed then
    Path.EndFigure(D2D1_FIGURE_END_OPEN);

  Res := Path.Close;
  SAFE_RELEASE(Path);
  RenderTarget.DrawGeometry(Geometry, StrokeBrush, StrokeThickness, StrokeStyle);
  SAFE_RELEASE(Geometry);
end;

procedure TvgCanvasD2D.FillPath(const APath: TvgPathData; const ARect: TvgRect; const AOpacity: single);
var
  i: integer;
  B: TvgRect;
  W, H, NewW, NewH: single;
  CP1, CP2: TvgPoint;
  Geometry: ID2D1PathGeometry;
  Path: ID2D1GeometrySink;
  Closed: boolean;
begin
//exit;
  if FFill.Style = vgBrushNone then Exit;
  if APath.IsEmpty then Exit;

  B := APath.GetBounds;
  W := vgRectWidth(B);
  H := vgRectHeight(B);
  NewW := vgRectWidth(ARect);
  NewH := vgRectHeight(ARect);

  Factory.CreatePathGeometry(Geometry);
  Geometry.Open(Path);
  i := 0;
  Closed := false;
  while i < Length(APath.PathData) do
  begin
    case APath.PathData[i].Kind of
      vgPathPointMoveTo:
        begin
          if (i > 0) and (APath.PathData[i - 1].Kind <> vgPathPointClose) then
            Path.EndFigure(D2D1_FIGURE_END_OPEN);
          Path.BeginFigure(D2Point(ARect.Left + (APath.PathData[i].Point.X - B.Left) / W * NewW,
            ARect.Top + (APath.PathData[i].Point.Y - B.Top) / H * NewH), D2D1_FIGURE_BEGIN_FILLED);
        end;
      vgPathPointLineTo:
        begin
          Path.AddLine(D2Point(ARect.Left + (APath.PathData[i].Point.X - B.Left) / W * NewW,
            ARect.Top + (APath.PathData[i].Point.Y - B.Top) / H * NewH));
        end;
      vgPathPointCurveTo:
        begin
          CP1 := vgPoint(ARect.Left + (APath.PathData[i].Point.X - B.Left) / W * NewW,
            ARect.Top + (APath.PathData[i].Point.Y - B.Top) / H * NewH);
          Inc(i);
          CP2 := vgPoint(ARect.Left + (APath.PathData[i].Point.X - B.Left) / W * NewW,
            ARect.Top + (APath.PathData[i].Point.Y - B.Top) / H * NewH);
          Inc(i);
          Path.AddBezier(D2Bezier(CP1.X,
            CP1.Y,
            CP2.X,
            CP2.Y,
            ARect.Left + (APath.PathData[i].Point.X - B.Left) / W * NewW,
            ARect.Top + (APath.PathData[i].Point.Y - B.Top) / H * NewH));
        end;
      vgPathPointClose:
        begin
          Path.EndFigure(D2D1_FIGURE_END_CLOSED);
          Closed := true;
        end;
    end;
    inc(i);
  end;
  if not Closed then
    Path.EndFigure(D2D1_FIGURE_END_OPEN);

  Path.Close;
  SAFE_RELEASE(Path);
  IntFillPath(Geometry, ARect, AOpacity);
  SAFE_RELEASE(Geometry);
end;

function TvgCanvasD2D.PtInPath(const APoint: TvgPoint; const ARect: TvgRect; const APath: TvgPathData): boolean;
var
  i: integer;
  B: TvgRect;
  W, H, NewW, NewH: single;
  CP1, CP2: TvgPoint;
  Geometry: ID2D1PathGeometry;
  Path: ID2D1GeometrySink;
  Cont: LongBool;
  Closed: boolean;
begin
  Result := false;
  if not vgPtInRect(APoint, ARect) then
    Result := false
  else
  begin
    if APath.IsEmpty then Exit;
    B := APath.GetBounds;
    W := vgRectWidth(B);
    H := vgRectHeight(B);
    NewW := vgRectWidth(ARect);
    NewH := vgRectHeight(ARect);

    Factory.CreatePathGeometry(Geometry);
    Geometry.Open(Path);
    i := 0;
    Closed := false;
    while i < Length(APath.PathData) do
    begin
      case APath.PathData[i].Kind of
        vgPathPointMoveTo:
          begin
            Path.BeginFigure(D2Point(ARect.Left + (APath.PathData[i].Point.X - B.Left) / W * NewW,
              ARect.Top + (APath.PathData[i].Point.Y - B.Top) / H * NewH), D2D1_FIGURE_BEGIN_FILLED);
          end;
        vgPathPointLineTo:
          begin
            Path.AddLine(D2Point(ARect.Left + (APath.PathData[i].Point.X - B.Left) / W * NewW,
              ARect.Top + (APath.PathData[i].Point.Y - B.Top) / H * NewH));
          end;
        vgPathPointCurveTo:
          begin
            CP1 := vgPoint(ARect.Left + (APath.PathData[i].Point.X - B.Left) / W * NewW,
              ARect.Top + (APath.PathData[i].Point.Y - B.Top) / H * NewH);
            Inc(i);
            CP2 := vgPoint(ARect.Left + (APath.PathData[i].Point.X - B.Left) / W * NewW,
              ARect.Top + (APath.PathData[i].Point.Y - B.Top) / H * NewH);
            Inc(i);
            Path.AddBezier(D2Bezier(CP1.X,
              CP1.Y,
              CP2.X,
              CP2.Y,
              ARect.Left + (APath.PathData[i].Point.X - B.Left) / W * NewW,
              ARect.Top + (APath.PathData[i].Point.Y - B.Top) / H * NewH));
          end;
        vgPathPointClose:
          begin
            Path.EndFigure(D2D1_FIGURE_END_CLOSED);
            Closed := true;
          end;
      end;
      inc(i);
    end;
    if not Closed then
      Path.EndFigure(D2D1_FIGURE_END_OPEN);

    Path.Close;
    SAFE_RELEASE(Path);
    Geometry.FillContainsPoint(D2Point(APoint.X, APoint.Y), nil, 1, Cont);
    SAFE_RELEASE(Geometry);

    Result := Cont;
  end;
end;

{ TvgTextRendering }

type
  TvgSink = class(TInterfacedPersistent, ID2D1SimplifiedGeometrySink)
  private
    Path: TvgPathData;
    procedure SetFillMode(FillMode:TD2D1_FillMode); stdcall;
    procedure SetSegmentFlags(VertexFlags:TD2D1_PathSegment); stdcall;
    procedure BeginFigure(StartPoint:TD2D1_Point2F;FigureBegin:TD2D1_FigureBegin); stdcall;
    procedure AddLines(Points:PTD2D1_Point2F; (* __in_ecount(pointsCount) *)PointsCount:LongWord); stdcall;
    procedure AddBeziers(Beziers:PTD2D1_BezierSegment; (* __in_ecount(beziersCount) *)BeziersCount:LongWord); stdcall;
    procedure EndFigure(FigureEnd:TD2D1_FigureEnd); stdcall;
    function Close:HResult; stdcall;
  public
  end;

{ TvgSink }

procedure TvgSink.BeginFigure(StartPoint: TD2D1_Point2F;
  FigureBegin: TD2D1_FigureBegin);
begin
  Path.MoveTo(TvgPoint(StartPoint));
end;

procedure TvgSink.AddBeziers(Beziers: PTD2D1_BezierSegment;
  BeziersCount: LongWord);
var
  i: integer;
begin
  for i := 0 to BeziersCount - 1 do
  begin
    Path.CurveTo(TvgPoint(Beziers.Point1), TvgPoint(Beziers.Point2), TvgPoint(Beziers.Point3));
    Inc(Beziers);
  end;
end;

procedure TvgSink.AddLines(Points: PTD2D1_Point2F; PointsCount: LongWord);
var
  i: integer;
begin
  for i := 0 to PointsCount - 1 do
  begin
    Path.LineTo(TvgPoint(Points^));
    Inc(Points);
  end;
end;

procedure TvgSink.EndFigure(FigureEnd: TD2D1_FigureEnd);
begin
  Path.ClosePath;
end;

function TvgSink.Close: HResult;
begin
  Result := S_OK;
end;

procedure TvgSink.SetFillMode(FillMode: TD2D1_FillMode);
begin
end;

procedure TvgSink.SetSegmentFlags(VertexFlags: TD2D1_PathSegment);
begin
end;

type

  TvgTextRendering = class(TInterfacedPersistent, IDWriteTextRenderer)
  private
    Canvas: TvgCanvasD2D;
    { TextRenderer }
    function IsPixelSnappingDisabled
    (
      ClientDrawingContext:Pointer; (* __maybenull *)
      out IsDisabled:LongBool (* __out *)
    ):HResult; stdcall;

    function GetCurrentTransform
    (
      ClientDrawingContext:Pointer; (* __maybenull *)
      out Transform:TDWrite_Matrix (* __out *)
    ):HResult; stdcall;

    function GetPixelsPerDip
    (
      ClientDrawingContext:Pointer; (* __maybenull *)
      out PixelsPerDip:Single (* __out *)
    ):HResult; stdcall;
    function DrawGlyphRun
    (
      ClientDrawingContext:Pointer; (* __maybenull *)
      BaselineOriginX:Single;
      BaselineOriginY:Single;
      MeasuringMode:TDWrite_MeasuringMode;
      const GlyphRun:TDWrite_GlyphRun; (* __in *)
      const GlyphRunDescription:TDWrite_GlyphRunDescription; (* __in *)
      ClientDrawingEffect:IUnknown (* __maybenull *)
    ):HResult; stdcall;

    function DrawUnderline
    (
      ClientDrawingContext:Pointer; (* __maybenull *)
      BaselineOriginX:Single;
      BaselineOriginY:Single;
      const Underline:TDWrite_Underline; (* __in *)
      ClientDrawingEffect:IUnknown (* __maybenull *)
    ):HResult; stdcall;

    function DrawStrikethrough
    (
      ClientDrawingContext:Pointer; (* __maybenull *)
      BaselineOriginX:Single;
      BaselineOriginY:Single;
      const Strikethrough:TDWrite_Strikethrough; (* __in *)
      ClientDrawingEffect:IUnknown (* __maybenull *)
    ):HResult; stdcall;

    function DrawInlineObject
    (
      ClientDrawingContext:Pointer; (* __maybenull *)
      OriginX:Single;
      OriginY:Single;
      InlineObject:IDWriteInlineObject;
      IsSideways:LongBool;
      IsRightToLeft:LongBool;
      ClientDrawingEffect:IUnknown (* __maybenull *)
    ):HResult; stdcall;
  public
  end;



function TvgTextRendering.DrawGlyphRun(ClientDrawingContext: Pointer;
  BaselineOriginX, BaselineOriginY: Single;
  MeasuringMode: TDWrite_MeasuringMode; const GlyphRun: TDWrite_GlyphRun;
  const GlyphRunDescription: TDWrite_GlyphRunDescription;
  ClientDrawingEffect: IInterface): HResult;
var
  pPathGeometry: ID2D1PathGeometry;
  pSink: ID2D1GeometrySink;
  pSimSink: TvgSink;
begin
  // Create the path geometry.
  Res := Factory.CreatePathGeometry(pPathGeometry);
  // Write to the path geometry using the geometry sink.
  if (SUCCEEDED(Res)) then
    Res := pPathGeometry.Open(pSink);
  // Get the glyph run outline geometries back from DirectWrite and place them within the geometry sink.
  if (SUCCEEDED(Res)) then
  begin
    Res := glyphRun.fontFace.GetGlyphRunOutline(
        glyphRun.fontEmSize,
        glyphRun.glyphIndices,
        glyphRun.glyphAdvances,
        glyphRun.glyphOffsets,
        glyphRun.glyphCount,
        glyphRun.isSideways,
        false {glyphRun.bidiLevel},
        pSink
        );
  end;
  // Close the geometry sink
  if (SUCCEEDED(Res)) then
    Res := pSink.Close();
  // Simplify
  pSimSink := TvgSink.Create;
  pSimSink.Path := TvgPathData(ClientDrawingContext);
  Res := pPathGeometry.Simplify(D2D1_GEOMETRY_SIMPLIFICATION_OPTION_CUBICS_AND_LINES, nil, 0, pSimSink);
  pSimSink.Close;
  pSimSink.Path.Offset(BaselineOriginX, BaselineOriginY);
  pSimSink.Free;
  // Free
  SAFE_RELEASE(pSink);
  SAFE_RELEASE(pPathGeometry);
end;

function TvgTextRendering.DrawInlineObject(ClientDrawingContext: Pointer;
  OriginX, OriginY: Single; InlineObject: IDWriteInlineObject; IsSideways,
  IsRightToLeft: LongBool; ClientDrawingEffect: IInterface): HResult;
begin
  Result := S_OK;
end;

function TvgTextRendering.DrawStrikethrough(ClientDrawingContext: Pointer;
  BaselineOriginX, BaselineOriginY: Single;
  const Strikethrough: TDWrite_Strikethrough;
  ClientDrawingEffect: IInterface): HResult;
begin
  Result := S_OK;
end;

function TvgTextRendering.DrawUnderline(ClientDrawingContext: Pointer;
  BaselineOriginX, BaselineOriginY: Single;
  const Underline: TDWrite_Underline;
  ClientDrawingEffect: IInterface): HResult;
begin
  Result := S_OK;
end;

function TvgTextRendering.GetCurrentTransform(
  ClientDrawingContext: Pointer; out Transform: TDWrite_Matrix): HResult;
begin
  Transform := TDWrite_Matrix(D2Matrix(IdentityMatrix));
  Result := S_OK;
end;

function TvgTextRendering.GetPixelsPerDip(ClientDrawingContext: Pointer;
  out PixelsPerDip: Single): HResult;
begin
  PixelsPerDip := 1;
  Result := S_OK;
end;

function TvgTextRendering.IsPixelSnappingDisabled(
  ClientDrawingContext: Pointer; out IsDisabled: LongBool): HResult;
begin
  IsDisabled := true;
  Result := S_OK;
end;

function TvgCanvasD2D.TextToPath(Path: TvgPathData; const ARect: TvgRect;
  const AText: WideString; const WordWrap: boolean; const ATextAlign,
  AVTextAlign: TvgTextAlign): boolean;
var
  TextRange: TDWrite_TextRange;
  TextLayout: IDWriteTextLayout;
  TextFormat: IDWriteTextFormat;
  R: TvgRect;
  S: TvgFontStyle;
  WS: WideString;
  MyRenderer: TvgTextRendering;
begin
  if (AText <> '') then
  begin
    Path.Clear;

    WS := FFont.Family;
    DWriteFactory.CreateTextFormat(PWideChar(WS), nil, D2FontWeight(FFont.Style),
      D2FontStyle(FFont.Style), DWRITE_FONT_STRETCH_NORMAL, FFont.Size, 'en-us', TextFormat);
    DWriteFactory.CreateTextLayout(PWideChar(AText), Length(AText), TextFormat, vgRectWidth(ARect), vgRectHeight(ARect),
      TextLayout);

    TextRange.startPosition := 0;
    TextRange.length := Length(AText);

    if not WordWrap then
      TextLayout.SetWordWrapping(DWRITE_WORD_WRAPPING_NO_WRAP)
    else
      TextLayout.SetWordWrapping(DWRITE_WORD_WRAPPING_WRAP);

    if FFont.Style = vgFontStrikeout then
      TextLayout.SetStrikethrough(True, TextRange);

    if FFont.Style = vgFontUnderline then
      TextLayout.SetUnderline(True, TextRange);

    // formating
    case AVTextAlign of
      vgTextAlignCenter:
        begin
          TextLayout.SetParagraphAlignment(DWRITE_PARAGRAPH_ALIGNMENT_CENTER);
        end;
      vgTextAlignNear:
        begin
          TextLayout.SetParagraphAlignment(DWRITE_PARAGRAPH_ALIGNMENT_NEAR);
        end;
      vgTextAlignFar:
        begin
          TextLayout.SetParagraphAlignment(DWRITE_PARAGRAPH_ALIGNMENT_FAR);
        end;
    end;
    case ATextAlign of
      vgTextAlignCenter:
        begin
          TextLayout.SetTextAlignment(DWRITE_TEXT_ALIGNMENT_CENTER);
        end;
      vgTextAlignNear:
        begin
          TextLayout.SetTextAlignment(DWRITE_TEXT_ALIGNMENT_LEADING);
        end;
      vgTextAlignFar:
        begin
          TextLayout.SetTextAlignment(DWRITE_TEXT_ALIGNMENT_TRAILING);
        end;
    end;
    // render
    MyRenderer := TvgTextRendering.Create;
    MyRenderer.Canvas := Self;
    TextLayout.Draw(Path, MyRenderer, 0, 0);
    MyRenderer.Free;
    // free
    SAFE_RELEASE(TextFormat);
    SAFE_RELEASE(TextLayout);
  end;
end;

{$ENDIF}

initialization
{$IFDEF WINDOWS}
  LoadD2D;
{$ENDIF}
finalization
{$IFDEF WINDOWS}
  FreeD2D;
{$ENDIF}
end.
