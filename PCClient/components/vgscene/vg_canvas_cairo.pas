unit vg_canvas_cairo;

{$ALIGN ON}
{$MINENUMSIZE 4}
{$I vg_define.inc}
{.$DEFINE FLATTEN}
{$DEFINE TEXTFLATTEN}

interface

uses
  {$IFDEF LINUX}
  cairo, cairoXlib, xlib, x,xutil, gtk2def, gtk2proc, gtk2, gdk2, gdk2x, gdk2pixbuf, glib2,
  pango,
  {$ENDIF}
  {$IFDEF WIN32}
  Cairo,
  {$ENDIF}
  {$IFDEF FPC}
  LCLProc,
  {$ENDIF}
  Graphics, Classes, SysUtils, Math, vg_scene;

const
  merge3 = 0;

{$IFDEF LINUX}

{$ifdef win32}
  {$define pangowin}
  pangocairolib = 'libpangocairo-1.0-0.dll';
  {$IFDEF FPC}
    {$ifndef NO_SMART_LINK}
      {$smartlink on}
    {$endif}
  {$ENDIF}
{$else}
  {$ifdef UseCustomLibs}
  pangocairolib = '';
  {$else}
  pangocairolib = 'libpangocairo-1.0.so.0';
  gdkcairolib = 'libgdkcairo-1.0.so.0';
  {$endif}
{$endif}

{$IFDEF LINUX}
type
  PGInputStream = Pointer;
function g_memory_input_stream_new_from_data(const Data: Pointer; size: gssize; Notify: Pointer): PGInputStream; cdecl; external 'libgio-2.0.so';

procedure gdk_cairo_set_source_pixbuf(cr: Pcairo_t; const Pb: PGdkPixbuf; const x, y: double); cdecl; external LIB_CAIRO;

function gdk_pixbuf_get_file_info(filename:Pchar; var width, height: gint): integer; cdecl; external gdkpixbuflib;
function gdk_pixbuf_new_from_stream(stream: PGInputStream; Cancelable: pointer; error:PPGError):PGdkPixbuf; cdecl; external gdkpixbuflib;

function gdk_pixbuf_save_to_buffer(pb: PGdkPixbuf; var bug: PByte; var Len: gsize; const typ: PChar; Error: Pointer): gboolean; cdecl; external gdkpixbuflib;
function gdk_pixbuf_save_to_bufferv(pb: PGdkPixbuf; var bug: PByte; var Len: gsize; const typ: PChar; option_keys:PPchar; option_values:PPchar; Error: Pointer): gboolean; cdecl; external gdkpixbuflib;

function gdk_screen_get_rgba_colormap(const screen: PGdkScreen): PGdkColormap; cdecl; external gdklib;
procedure gdk_window_set_composited(window: PGdkWindow; Composited: gboolean); cdecl; external gdklib;

procedure cairo_path_extents(cr: Pcairo_t; var x1, y1, x2, y2: double); cdecl; external LIB_CAIRO;

function gdk_cairo_create(drawable: PGdkDrawable): Pcairo_t; cdecl; external LIB_CAIRO;
function pango_cairo_create_context(cr: Pcairo_t): PPangoContext; cdecl; external pangocairolib;
function pango_cairo_create_layout(cr: Pcairo_t): PPangoLayout; cdecl; external pangocairolib;
procedure pango_cairo_update_layout(cr: Pcairo_t; layout: PPangoLayout); cdecl; external pangocairolib;
procedure pango_cairo_show_layout(cr: Pcairo_t; layout: PPangoLayout); cdecl; external pangocairolib;
procedure pango_cairo_layout_path(cr: Pcairo_t; layout: PPangoLayout); cdecl; external pangocairolib;
procedure pango_layout_set_height(layout:PPangoLayout; height:longint); cdecl; external pangolib;
{$ENDIF}

{$ENDIF}

implementation {===============================================================}


{$IFDEF LINUX}

type

  { TvgFilterGtk }

  TvgFilterGtk = class(TvgFilter)
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

  { TvgCanvasCairo }

  TvgCanvasCairo = class(TvgCanvas)
  private
    grad: Pcairo_pattern_t;
    sr: Pcairo_surface_t;
    memcr: Pcairo_t;
    memsr: Pcairo_surface_t;
    memlayout: PPangoLayout;
  protected
    procedure ApplyFill(ARect: TvgRect; const AOpacity: single);
    procedure DeapplyFill;
    procedure ApplyStroke(ARect: TvgRect; const AOpacity: single);
    procedure FontChanged(Sender: TObject); override;
    procedure UpdateBitmap(ABitmap: TvgBitmap);
    procedure DoDestroyBitmap(Sender: TObject);
    class function GetBitmapScanline(Bitmap: TvgBitmap; y: integer): PvgColorArray; override;
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
    function PtInPath(const APoint: TvgPoint; const ARect: TvgRect; const APath: TvgPathData): boolean; override;
    procedure FillPath(const APath: TvgPathData; const ARect: TvgRect; const AOpacity: single); override;
    procedure DrawPath(const APath: TvgPathData; const ARect: TvgRect; const AOpacity: single); override;
    procedure DrawBitmap(const ABitmap: TvgBitmap; const SrcRect, DstRect: TvgRect; const AOpacity: single; const HighSpeed: boolean = false); override;
    procedure DrawThumbnail(const ABitmap: TvgBitmap; const Width, Height: single); override;
    function TextToPath(Path: TvgPathData; const ARect: TvgRect; const AText: WideString; const WordWrap: boolean; const ATextAlign: TvgTextAlign;
      const AVTextAlign: TvgTextAlign = vgTextAlignCenter): boolean; override;
  published
  end;

procedure SwapRB(const Buf: Pointer; const Size: integer);
var
  i: integer;
  b: byte;
begin
  for i := 0 to Size - 1 do
  begin
    b := PvgColorRecArray(Buf)[i].R;
    PvgColorRecArray(Buf)[i].R := PvgColorRecArray(Buf)[i].B;
    PvgColorRecArray(Buf)[i].B := b;
  end;
end;

{ TvgFilterGtk }

class function TvgFilterGtk.GetFileTypes: string;
begin
  Result := '*.jpg;*.jpeg;*.gif;*.tif;*.tiff;*.png;*.bmp;*.JPG;*.JPEG;*.GIF;*.TIF;*.TIFF;*.PNG;*.BMP';
end;

class function TvgFilterGtk.GetImageSize(const AFileName: string): TvgPoint;
var
  W, H: gint;
begin
  Result := vgPoint(0, 0);
  if gdk_pixbuf_get_file_info(PChar(AFileName), W, H) <> 0 then
    Result := vgPoint(W, H);
end;

function TvgFilterGtk.LoadFromStream(const AStream: TStream; var Bitmap: TvgBitmap): boolean;
var
  GS: PGInputStream;
  res, pb: PGdkPixbuf;
  MS: TMemoryStream;
begin
  Result := false;
  if AStream.Size = 0 then Exit;

  MS := TMemoryStream.Create;
  MS.CopyFrom(AStream, AStream.Size);

  GS := g_memory_input_stream_new_from_data(MS.Memory, MS.Size, nil);
  if GS <> nil then
  begin
    pb := gdk_pixbuf_new_from_stream(GS, nil, nil);
    if pb <> nil then
    begin
      Bitmap.SetSize(gdk_pixbuf_get_width(pb), gdk_pixbuf_get_Height(pb));
      res := gdk_pixbuf_new_from_data(Pguchar(Bitmap.StartLine), GDK_COLORSPACE_RGB, true,
        8, gdk_pixbuf_get_width(pb), gdk_pixbuf_get_Height(pb), gdk_pixbuf_get_width(pb) * 4, nil, nil);
      if res <> nil then
      begin
        gdk_pixbuf_copy_area(pb, 0, 0, gdk_pixbuf_get_width(pb), gdk_pixbuf_get_Height(pb),
          res, 0, 0);
        g_object_unref(res);
        { swap }
        SwapRB(Bitmap.Startline, Bitmap.Width * Bitmap.Height);
        Result := true;
      end;
      g_object_unref(pb);
    end;
    g_object_unref(GS);
  end;
  MS.Free;
end;

function TvgFilterGtk.SaveToStream(const AStream: TStream; var Bitmap: TvgBitmap; const Format: string;
      const Params: string = ''): boolean;
var
  pb: PGdkPixbuf;
  buf: PByte;
  size: gsize;
  k, o: array [0..10] of PChar;
  param, name, value, S, encoderType, opts, keys: string;
  i: integer;
begin
  Result := false;
  encoderType := 'png';
  if (LowerCase(Format) = 'jpg') or (LowerCase(Format) = 'jpeg') then
    encoderType := 'jpeg';
  if (LowerCase(Format) = 'bmp') then
    encoderType := 'bmp';
  if (LowerCase(Format) = 'png') then
    encoderType := 'png';
  if (LowerCase(Format) = 'tif') or (LowerCase(Format) = 'tiff') then
    encoderType := 'tif';
  if (LowerCase(Format) = 'gif') then
    encoderType := 'gif';

  pb := gdk_pixbuf_new_from_data(Pguchar(Bitmap.StartLine), GDK_COLORSPACE_RGB, true,
    8, Bitmap.Width, Bitmap.Height, Bitmap.Width * 4, nil, nil);

  if pb <> nil then
  begin
    opts := '';
    keys := '';
    i := 0;
    if Params <> '' then
    begin
      S := Params;
      while S <> '' do
      begin
        param := vgGetToken(S, ' ');
        name := vgGetToken(param, '=');
        value := vgGetToken(param, '');
        if CompareText(name, 'quality') = 0 then
        begin
          o[i] := PChar('quality');
          k[i] := PChar(value);
          inc(i);
        end;
      end;
      o[i] := nil;
      k[i] := nil;
      SwapRB(Bitmap.Startline, Bitmap.Width * Bitmap.Height);
      if gdk_pixbuf_save_to_bufferv(pb, buf, size, PChar(encoderType), @o[0], @k[0], nil) then
      begin
        AStream.Write(buf^, size);
        g_free(buf);
        Result := true;
      end;
      SwapRB(Bitmap.Startline, Bitmap.Width * Bitmap.Height);
    end
    else
    begin
      SwapRB(Bitmap.Startline, Bitmap.Width * Bitmap.Height);
      if gdk_pixbuf_save_to_buffer(pb, buf, size, PChar(encoderType), nil) then
      begin
        AStream.Write(buf^, size);
        g_free(buf);
        Result := true;
      end;
      SwapRB(Bitmap.Startline, Bitmap.Width * Bitmap.Height);
    end;
    g_object_unref(pb);
  end;
end;

function TvgFilterGtk.LoadFromFile(const AFileName: string; const Rotate: single;
  var Bitmap: TvgBitmap): boolean;
var
  res, pb: PGdkPixbuf;
begin
  pb := gdk_pixbuf_new_from_file(PChar(AFileName), nil);
  if pb <> nil then
  begin
    Bitmap.SetSize(gdk_pixbuf_get_width(pb), gdk_pixbuf_get_Height(pb));
    res := gdk_pixbuf_new_from_data(Pguchar(Bitmap.StartLine), GDK_COLORSPACE_RGB, true,
      8, gdk_pixbuf_get_width(pb), gdk_pixbuf_get_Height(pb), gdk_pixbuf_get_width(pb) * 4, nil, nil);
    if res <> nil then
    begin
      gdk_pixbuf_copy_area(pb, 0, 0, gdk_pixbuf_get_width(pb), gdk_pixbuf_get_Height(pb),
        res, 0, 0);
      g_object_unref(res);
      SwapRB(Bitmap.Startline, Bitmap.Width * Bitmap.Height);
      Result := true;
    end;
    g_object_unref(pb);
  end
  else
    Result := false;
end;

function TvgFilterGtk.LoadThumbnailFromFile(const AFileName: string;
  const AFitWidth, AFitHeight: single; const UseEmbedded: boolean; var Bitmap: TvgBitmap): boolean;
var
  res, pb: PGdkPixbuf;
begin
  pb := gdk_pixbuf_new_from_file_at_scale(PChar(AFileName), round(AFitWidth), round(AFitHeight), true, nil);
  if pb <> nil then
  begin
    Bitmap.SetSize(gdk_pixbuf_get_width(pb), gdk_pixbuf_get_Height(pb));
    res := gdk_pixbuf_new_from_data(Pguchar(Bitmap.StartLine), GDK_COLORSPACE_RGB, true,
      8, gdk_pixbuf_get_width(pb), gdk_pixbuf_get_Height(pb), gdk_pixbuf_get_width(pb) * 4, nil, nil);
    if res <> nil then
    begin
      gdk_pixbuf_copy_area(pb, 0, 0, gdk_pixbuf_get_width(pb), gdk_pixbuf_get_Height(pb),
        res, 0, 0);
      g_object_unref(res);
      SwapRB(Bitmap.Startline, Bitmap.Width * Bitmap.Height);
      Result := true;
    end;
    g_object_unref(pb);
  end
  else
    Result := false;
end;

function TvgFilterGtk.SaveToFile(const AFileName: string;
  var Bitmap: TvgBitmap; const Params: string = ''): boolean;
var
  pb: PGdkPixbuf;
  param, name, value, S, encoderType, opts, keys: string;
  i: integer;
  k, o: array [0..10] of PChar;
begin
  Result := false;
  encoderType := 'png';
  if (LowerCase(ExtractFileExt(AFileName)) = '.jpg') or (LowerCase(ExtractFileExt(AFileName)) = '.jpeg') then
    encoderType := 'jpeg';
  if (LowerCase(ExtractFileExt(AFileName)) = '.bmp') then
    encoderType := 'bmp';
  if (LowerCase(ExtractFileExt(AFileName)) = '.png') then
    encoderType := 'png';
  if (LowerCase(ExtractFileExt(AFileName)) = '.tif') or (LowerCase(ExtractFileExt(AFileName)) = '.tiff') then
    encoderType := 'tiff';
  if (LowerCase(ExtractFileExt(AFileName)) = '.gif') then
    encoderType := 'gif';
  if encoderType = '' then Exit;

  pb := gdk_pixbuf_new_from_data(Pguchar(Bitmap.StartLine), GDK_COLORSPACE_RGB, true,
    8, Bitmap.Width, Bitmap.Height, Bitmap.Width * 4, nil, nil);
  if pb <> nil then
  begin
    opts := '';
    keys := '';
    i := 0;
    if Params <> '' then
    begin
      S := Params;
      while S <> '' do
      begin
        param := vgGetToken(S, ' ');
        name := vgGetToken(param, '=');
        value := vgGetToken(param, '');
        if CompareText(name, 'quality') = 0 then
        begin
          o[i] := PChar('quality');
          k[i] := PChar(value);
          inc(i);
        end;
      end;
      o[i] := nil;
      k[i] := nil;
      SwapRB(Bitmap.Startline, Bitmap.Width * Bitmap.Height);
      Result := gdk_pixbuf_savev(pb, PChar(AFileName), PChar(encoderType), @o[0], @k[0], nil);
      SwapRB(Bitmap.Startline, Bitmap.Width * Bitmap.Height);
    end
    else
    begin
      SwapRB(Bitmap.Startline, Bitmap.Width * Bitmap.Height);
      Result := gdk_pixbuf_save(pb, PChar(AFileName), PChar(encoderType), nil);
      SwapRB(Bitmap.Startline, Bitmap.Width * Bitmap.Height);
    end;
    gdk_pixbuf_unref(pb);
  end;
end;

{ TvgCanvasCairo }

var
  CS: string;

var
  ColorArray: array [0..100] of cardinal;
  OffsetArray: array [0..100] of single;

constructor TvgCanvasCairo.Create(const AWidth, AHeight: integer);
begin
  inherited ;
  memsr := cairo_image_surface_create(CAIRO_FORMAT_ARGB32, 1, 1);
  memcr := cairo_create(memsr);
  memlayout := pango_cairo_create_layout(memcr);
end;

constructor TvgCanvasCairo.CreateFromBitmap(const ABitmap: TvgBitmap);
begin
  inherited;
  memsr := cairo_image_surface_create(CAIRO_FORMAT_ARGB32, 1, 1);
  memcr := cairo_create(memsr);
  memlayout := pango_cairo_create_layout(memcr);

  FBitmap := ABitmap;
  UpdateBitmap(FBitmap);
  if FBitmap.Handle <> 0 then
    FHandle := Integer(cairo_create(Pcairo_surface_t(FBitmap.Handle)))
end;

destructor TvgCanvasCairo.Destroy;
begin
  g_object_unref(memlayout);
  cairo_destroy(memcr);
  cairo_surface_destroy(memsr);
  inherited;
end;

procedure TvgCanvasCairo.FreeBuffer;
begin
  if (FBitmap <> nil) and (Handle <> 0) then
  begin
    cairo_destroy(Pcairo_t(Handle));
  end;
  if FBufferBits <> nil then
    System.FreeMem(FBufferBits);
  if FBuffered then
  begin
    cairo_destroy(Pcairo_t(Handle));
    cairo_surface_destroy(sr);
  end;
  Handle := 0;
end;

procedure TvgCanvasCairo.ResizeBuffer(const AWidth, AHeight: integer);
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
    sr := cairo_image_surface_create_for_data(FBufferBits, CAIRO_FORMAT_ARGB32,
      FWidth, FHeight, FWidth * 4);
    Handle := Integer(cairo_create(sr));
  end
  else
  begin
    GetMem(FBufferBits, 4);
  end;
end;

procedure TvgCanvasCairo.FlushBuffer(const X, Y: integer; const DC: Cardinal);
begin
end;

procedure TvgCanvasCairo.FlushBufferRect(const X, Y: integer;
  const DC: Cardinal; const ARect: TvgRect);
begin
end;

procedure TvgCanvasCairo.Clear(const Color: cardinal);
begin
  if FBufferBits = nil then Exit;
  if not FBuffered then
  begin
    with TvgColorRec(Color) do
      cairo_set_source_rgba(Pcairo_t(Handle), R / $FF, G / $FF, B / $FF, A / $FF);
    cairo_paint(Pcairo_t(Handle));
  end
  else
    vgFillLongword(FBufferBits, FWidth * FHeight, Color);
end;

procedure TvgCanvasCairo.ClearRect(const ARect: TvgRect; const AColor: TvgColor);
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
    with TvgColorRec(AColor) do
      cairo_set_source_rgba(Pcairo_t(Handle), R / $FF, G / $FF, B / $FF, A / $FF);
    cairo_rectangle(Pcairo_t(Handle), R.Left, R.Top, R.right - R.left, R.bottom - R.top);
    cairo_fill(Pcairo_t(Handle));
  end
  else
    vgFillLongwordRect(FBufferBits, FWidth, FHeight, R.Left, R.Top, R.Right, R.Bottom, AColor);
end;

class function TvgCanvasCairo.GetBitmapScanline(Bitmap: TvgBitmap; y: integer): PvgColorArray;
begin
  if (y >= 0) and (y < Bitmap.Height) and (Bitmap.StartLine <> nil) then
    Result := @PvgColorArray(Bitmap.StartLine)[(y) * Bitmap.Width]
  else
    Result := nil;
end;

procedure TvgCanvasCairo.SetMatrix(const M: TvgMatrix);
var
  cm: cairo_matrix_t;
begin
  if Handle = 0 then Exit;

  FMatrix := M;
  with FMatrix do
  begin
    cm.xx := m11;
    cm.yx := m12;
    cm.xy := m21;
    cm.yy := m22;
    cm.x0 := m31;
    cm.y0 := m32;
  end;
  cairo_set_matrix(Pcairo_t(Handle), @cm);
end;

procedure TvgCanvasCairo.MultyMatrix(const M: TvgMatrix);
var
  cm: cairo_matrix_t;
begin
  if Handle = 0 then Exit;
  inherited;
  with M do
  begin
    cm.xx := m11;
    cm.yx := m12;
    cm.xy := m21;
    cm.yy := m22;
    cm.x0 := m31;
    cm.y0 := m32;
  end;
  cairo_transform(Pcairo_t(Handle), @cm);
end;

function TvgCanvasCairo.SaveCanvas: cardinal;
var
  i: integer;
begin
  Result := InvalideCanvasState;
  if Handle = 0 then Exit;

  cairo_save(Pcairo_t(Handle));

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
    Move(FDash[0], FSaveData[Result].Dash[0], SizeOf(FDash[0]) * Length(FDash));
  end;
  FSaveData[Result].DashOffset := FDashOffset;
  FSaveData[Result].Font := TvgFont.Create;
  FSaveData[Result].Font.Assign(Font);
end;

procedure TvgCanvasCairo.RestoreCanvas(const AState: cardinal);
begin
  if Handle = 0 then Exit;

  if (AState >= 0) and (AState < Length(FSaveData)) then
  begin
    cairo_restore(Pcairo_t(Handle));
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

procedure TvgCanvasCairo.SetClipRects(const ARects: array of TvgRect);
var
  i: integer;
begin
  if Handle = 0 then Exit;

  cairo_reset_clip(Pcairo_t(Handle));
  for i := 0 to High(ARects) do
  begin
    cairo_rectangle(Pcairo_t(Handle), ARects[i].Left, ARects[i].Top, vgRectWidth(ARects[i]), vgRectHeight(ARects[i]));
  end;
  cairo_clip(Pcairo_t(Handle));
end;

procedure TvgCanvasCairo.IntersectClipRect(const ARect: TvgRect);
begin
  if Handle = 0 then Exit;

  cairo_rectangle(Pcairo_t(Handle), ARect.Left, ARect.Top, vgRectWidth(ARect), vgRectHeight(ARect));
  cairo_clip(Pcairo_t(Handle));
end;

procedure TvgCanvasCairo.ExcludeClipRect(const ARect: TvgRect);
var
  R: TvgRect;
  i: integer;
  RR: array [0..3] of TvgRect;
begin
  if Handle = 0 then Exit;

  R := ARect;
  RR[0] := vgRect(0, 0, R.Left, FHeight);
  RR[1] := vgRect(R.Right, 0, FWidth, FHeight);
  RR[2] := vgRect(R.Left, 0, R.Right, R.Top);
  RR[3] := vgRect(R.Left, R.Bottom, R.Right, FHeight);
  for i := 0 to High(RR) do
  begin
    cairo_rectangle(Pcairo_t(Handle), RR[i].Left, RR[i].Top, vgRectWidth(RR[i]), vgRectHeight(RR[i]));
  end;
  cairo_clip(Pcairo_t(Handle));
end;

procedure TvgCanvasCairo.ResetClipRect;
begin
  if Handle = 0 then Exit;

  cairo_reset_clip(Pcairo_t(Handle));
end;

procedure TvgCanvasCairo.ApplyFill(ARect: TvgRect; const AOpacity: single);
var
  C: TvgColor;
  i: integer;
  count: integer;
  cm: cairo_matrix_t;
begin
  if Handle = 0 then Exit;

  if (FFill.Style = vgBrushResource) and (FFill.Resource <> nil) and (FFill.Resource.Brush <> nil) then
    FFill.Assign(FFill.Resource.Brush);

  with FFill do
  begin
    case Style of
      vgBrushSolid:
        begin
          with TvgColorRec(vgOpacity(SolidColor, AOpacity)) do
            cairo_set_source_rgba(Pcairo_t(Handle), R / $FF, G / $FF, B / $FF, A / $FF);
        end;
      vgBrushGradient:
        begin
          if Gradient.Points.Count > 1 then
          begin
            count := 0;

            if Gradient.Points[0].Offset > 0 then
            begin
              ColorArray[count] := vgOpacity(Gradient.Points[0].IntColor, AOpacity);
              OffsetArray[count] := 0;
              count := count + 1;
            end;
            for i := 0 to Gradient.Points.Count - 1 do
            begin
              ColorArray[i + count] := vgOpacity(Gradient.Points[i].IntColor, AOpacity);
              OffsetArray[i + count] := Gradient.Points[i].Offset;
            end;
            if Gradient.Points[Gradient.Points.Count - 1].Offset < 1 then
            begin
              count := count + 1;
              ColorArray[Gradient.Points.Count + count - 1] := vgOpacity(Gradient.Points[Gradient.Points.Count - 1].IntColor, AOpacity);
              OffsetArray[Gradient.Points.Count + count - 1] := 1;
            end;

            if Gradient.Style = vgLinearGradient then
            begin
              { Linear }
              grad := cairo_pattern_create_linear(ARect.Left + Gradient.StartPosition.X * ARect.Right, ARect.Top + Gradient.StartPosition.Y * ARect.Bottom,
                ARect.Left + Gradient.StopPosition.X * ARect.Right, ARect.Top + Gradient.StopPosition.Y * ARect.Bottom);
              for i := 0 to Gradient.Points.Count + count - 1 do
                with TvgColorRec(ColorArray[i]) do
                  cairo_pattern_add_color_stop_rgba(grad, OffsetArray[i], R / $FF, G / $FF, B / $FF, A / $FF);
//              TGPLinearGradientBrush(FGPBrush).SetWrapMode(WrapModeTileFlipX);
              cairo_set_source(Pcairo_t(Handle), grad);
            end
            else
            begin
              { Radial }
              grad := cairo_pattern_create_radial((ARect.Left + ARect.Right) / 2, (ARect.Top + ARect.Bottom) / 2, 0,
                (ARect.Left + ARect.Right) / 2, (ARect.Top + ARect.Bottom) / 2, vgMaxFloat(vgRectWidth(ARect), vgRectWidth(ARect)));
              for i := 0 to Gradient.Points.Count + count - 1 do
                with TvgColorRec(ColorArray[i]) do
                  cairo_pattern_add_color_stop_rgba(grad, OffsetArray[i], R / $FF, G / $FF, B / $FF, A / $FF);
//              TGPLinearGradientBrush(FGPBrush).SetWrapMode(WrapModeTileFlipX);
              cairo_set_source(Pcairo_t(Handle), grad);
            end;
          end
          else
            cairo_set_source_rgba(Pcairo_t(Handle), 0, 0, 0, 0);
        end;
      vgBrushBitmap:
        begin
          if (Bitmap.Bitmap <> nil) and (Bitmap.Bitmap.Width > 0) and (Bitmap.Bitmap.Height > 0) then
          begin
            UpdateBitmap(Bitmap.Bitmap);
            if (Bitmap.Bitmap.Handle <> 0) then
            begin
              grad := cairo_pattern_create_for_surface(Pcairo_surface_t(Bitmap.Bitmap.Handle));
              if Bitmap.WrapMode = vgWrapTileStretch then
              begin
                cairo_matrix_init_scale(@cm, Bitmap.Bitmap.Width / (vgRectWidth(ARect) + (StrokeThickness / 2)),
                  Bitmap.Bitmap.Height / (vgRectHeight(ARect) + (StrokeThickness / 2)));
                cairo_pattern_set_matrix(grad, @cm);
              end;
              if Bitmap.WrapMode = vgWrapTile then
                cairo_pattern_set_extend (grad, CAIRO_EXTEND_REPEAT);
              cairo_set_source(Pcairo_t(Handle), grad);
            end
            else
              cairo_set_source_rgba(Pcairo_t(Handle), 0, 0, 0, 0);
          end
          else
            cairo_set_source_rgba(Pcairo_t(Handle), 0, 0, 0, 0);
        end;
    else
      cairo_set_source_rgba(Pcairo_t(Handle), 0, 0, 0, 0);
    end;
  end;
end;

procedure TvgCanvasCairo.DeapplyFill;
begin
  if Handle = 0 then Exit;

  with FFill do
  begin
    case Style of
      vgBrushSolid:
        begin
        end;
      vgBrushGradient:
        begin
          cairo_pattern_destroy(grad);
        end;
      vgBrushResource:
        begin
        end;
      vgBrushVisual:
        begin
        end;
      vgBrushBitmap:
        begin
          cairo_pattern_destroy(grad);
        end;
    end;
  end;
end;

procedure TvgCanvasCairo.ApplyStroke(ARect: TvgRect; const AOpacity: single);
var
  i: integer;
  dash: array of double;
begin
  if Handle = 0 then Exit;

  if (FStroke.Style = vgBrushResource) and (FStroke.Resource <> nil) and (FStroke.Resource.Brush <> nil) then
    FStroke.Assign(FStroke.Resource.Brush);

  with FStroke do
  begin
    case Style of
      vgBrushSolid:
        begin
          with TvgColorRec(vgOpacity(SolidColor, AOpacity)) do
            cairo_set_source_rgba(Pcairo_t(Handle), R / $FF, G / $FF, B / $FF, A / $FF);
        end;
(*      vgBrushGradient:
        begin
          if Gradient.Points.Count > 1 then
          begin
            for i := 0 to Gradient.Points.Count - 1 do
            begin
              ColorArray[i] := vgOpacity(Gradient.Points[i].IntColor, AOpacity);
              OffsetArray[i] := Gradient.Points[i].Offset;
            end;
            FGPPenBrush := TGPLinearGradientBrush.Create(MakePoint(ARect.Left + Gradient.StartPosition.X * ARect.Right, ARect.Top + Gradient.StartPosition.Y * ARect.Bottom),
              MakePoint(ARect.Left + Gradient.StopPosition.X * ARect.Right, ARect.Top + Gradient.StopPosition.Y * ARect.Bottom), SolidColor, SolidColor);
            TGPLinearGradientBrush(FGPPenBrush).SetInterpolationColors(PGPColor(@ColorArray), PSingle(@OffsetArray), Gradient.Points.Count);
            if Abs(FStroke.Gradient.StartPosition.X - FStroke.Gradient.StopPosition.X) < Abs(FStroke.Gradient.StartPosition.Y - FStroke.Gradient.StopPosition.Y) then
              TGPLinearGradientBrush(FGPPenBrush).ScaleTransform(vgRectHeight(ARect), vgRectWidth(ARect))
            else
              TGPLinearGradientBrush(FGPPenBrush).ScaleTransform(vgRectWidth(ARect), vgRectHeight(ARect));
          end
          else
            FGPPenBrush := TGPSolidBrush.Create(vgOpacity(SolidColor, AOpacity));
        end;
      vgBrushVisual:
        begin
          FGPPenBrush := TGPSolidBrush.Create($00000000);
        end;
      vgBrushBitmap:
        begin
          if (Bitmap.Bitmap <> nil) and (Bitmap.Bitmap.Width > 0) and (Bitmap.Bitmap.Height > 0) then
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
            FGPPenBrush := TGPSolidBrush.Create($00000000);
        end; *)
    else
      cairo_set_source_rgba(Pcairo_t(Handle), 0, 0, 0, 0);
    end;
  end;

  case StrokeCap of
    vgCapFlat: cairo_set_line_cap(Pcairo_t(Handle), CAIRO_LINE_CAP_BUTT);
    vgCapRound: cairo_set_line_cap(Pcairo_t(Handle), CAIRO_LINE_CAP_ROUND);
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
    cairo_set_dash(Pcairo_t(Handle), @dash[0], Length(FDash), FDashOffset);
  end
  else
    cairo_set_dash(Pcairo_t(Handle), nil, 0, 0);
  case StrokeJoin of
    vgJoinMiter: cairo_set_line_join(Pcairo_t(Handle), CAIRO_LINE_JOIN_MITER);
    vgJoinRound: cairo_set_line_join(Pcairo_t(Handle), CAIRO_LINE_JOIN_ROUND);
    vgJoinBevel: cairo_set_line_join(Pcairo_t(Handle), CAIRO_LINE_JOIN_BEVEL);
  end;
  cairo_set_line_width(Pcairo_t(Handle), StrokeThickness);
end;

procedure TvgCanvasCairo.FontChanged(Sender: TObject);
begin
end;

procedure TvgCanvasCairo.DrawLine(const APt1, APt2: TvgPoint; const AOpacity: single);
begin
  if Handle = 0 then Exit;

  if FStroke.Style <> vgBrushNone then
  begin
    ApplyStroke(vgRect(APt1.X, APt1.Y, APt2.X, APt2.Y), AOpacity);
    cairo_move_to(Pcairo_t(Handle), APt1.X, APt1.Y);
    cairo_line_to(Pcairo_t(Handle), APt2.X, APt2.Y);
    cairo_stroke(Pcairo_t(Handle));
  end;
end;

procedure TvgCanvasCairo.DrawRect(const ARect: TvgRect; const xRadius, yRadius: single; const ACorners: TvgCorners;
  const AOpacity: single; const ACornerType: TvgCornerType = vgCornerRound);
var
  x1, x2, y1, y2: single;
  R: TvgRect;
begin
  if Handle = 0 then Exit;

  if FStroke.Style <> vgBrushNone then
  begin
    R := ARect;
    ApplyStroke(ARect, AOpacity);
    if (xRadius < Epsilon) and (yRadius < Epsilon) then
    begin
      cairo_rectangle(Pcairo_t(Handle), R.Left, R.Top, vgRectWidth(R), vgRectHeight(R));
      cairo_stroke(Pcairo_t(Handle));
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
      cairo_new_path(Pcairo_t(Handle));
      cairo_move_to(Pcairo_t(Handle), R.Left, R.Top + y1);
      if vgCornerTopLeft in ACorners then
      begin
        case ACornerType of
          // vgCornetRound - default
          vgCornerBevel: cairo_line_to(Pcairo_t(Handle), R.Left + x1, R.Top);
          vgCornerInnerRound: cairo_curve_to(Pcairo_t(Handle), R.Left + x2, R.Top + y1, R.Left + x1, R.Top + y2, R.Left + x1, R.Top);
          vgCornerInnerLine:
            begin
              cairo_line_to(Pcairo_t(Handle), R.Left + x2, R.Top + y1);
              cairo_line_to(Pcairo_t(Handle), R.Left + x1, R.Top + y2);
              cairo_line_to(Pcairo_t(Handle), R.Left + x1, R.Top);
            end;
        else
          cairo_curve_to(Pcairo_t(Handle), R.Left, R.Top + (y2), R.Left + x2, R.Top, R.Left + x1, R.Top)
        end;
      end
      else
      begin
        cairo_line_to(Pcairo_t(Handle), R.Left, R.Top);
        cairo_line_to(Pcairo_t(Handle), R.Left + x1, R.Top);
      end;
      cairo_line_to(Pcairo_t(Handle), R.Right - x1, R.Top);
      if vgCornerTopRight in ACorners then
      begin
        case ACornerType of
          // vgCornetRound - default
          vgCornerBevel: cairo_line_to(Pcairo_t(Handle), R.Right, R.Top + y1);
          vgCornerInnerRound: cairo_curve_to(Pcairo_t(Handle), R.Right - x1, R.Top + y2, R.Right - x2, R.Top + y1, R.Right, R.Top + y1);
          vgCornerInnerLine:
            begin
              cairo_line_to(Pcairo_t(Handle), R.Right - x1, R.Top + y2);
              cairo_line_to(Pcairo_t(Handle), R.Right - x2, R.Top + y1);
              cairo_line_to(Pcairo_t(Handle), R.Right, R.Top + y1);
            end;
        else
          cairo_curve_to(Pcairo_t(Handle), R.Right - x2, R.Top, R.Right, R.Top + (y2), R.Right, R.Top + y1)
        end;
      end
      else
      begin
        cairo_line_to(Pcairo_t(Handle), R.Right, R.Top);
        cairo_line_to(Pcairo_t(Handle), R.Right, R.Top + y1);
      end;
      cairo_line_to(Pcairo_t(Handle), R.Right, R.Bottom - y1);
      if vgCornerBottomRight in ACorners then
      begin
        case ACornerType of
          // vgCornetRound - default
          vgCornerBevel: cairo_line_to(Pcairo_t(Handle), R.Right - x1, R.Bottom);
          vgCornerInnerRound: cairo_curve_to(Pcairo_t(Handle), R.Right - x2, R.Bottom - y1, R.Right - x1, R.Bottom - y2, R.Right - x1, R.Bottom);
          vgCornerInnerLine:
            begin
              cairo_line_to(Pcairo_t(Handle), R.Right - x2, R.Bottom - y1);
              cairo_line_to(Pcairo_t(Handle), R.Right - x1, R.Bottom - y2);
              cairo_line_to(Pcairo_t(Handle), R.Right - x1, R.Bottom);
            end;
        else
          cairo_curve_to(Pcairo_t(Handle), R.Right, R.Bottom - (y2), R.Right - x2, R.Bottom, R.Right - x1, R.Bottom)
        end;
      end
      else
      begin
        cairo_line_to(Pcairo_t(Handle), R.Right, R.Bottom);
        cairo_line_to(Pcairo_t(Handle), R.Right - x1, R.Bottom);
      end;
      cairo_line_to(Pcairo_t(Handle), R.Left + x1, R.Bottom);
      if vgCornerBottomLeft in ACorners then
      begin
        case ACornerType of
          // vgCornetRound - default
          vgCornerBevel: cairo_line_to(Pcairo_t(Handle), R.Left, R.Bottom - y1);
          vgCornerInnerRound: cairo_curve_to(Pcairo_t(Handle), R.Left + x1, R.Bottom - y2, R.Left + x2, R.Bottom - y1, R.Left, R.Bottom - y1);
          vgCornerInnerLine:
            begin
              cairo_line_to(Pcairo_t(Handle), R.Left + x1, R.Bottom - y2);
              cairo_line_to(Pcairo_t(Handle), R.Left + x2, R.Bottom - y1);
              cairo_line_to(Pcairo_t(Handle), R.Left, R.Bottom - y1);
            end;
        else
          cairo_curve_to(Pcairo_t(Handle), R.Left + x2, R.Bottom, R.Left, R.Bottom - (y2), R.Left, R.Bottom - y1)
        end;
      end
      else
      begin
        cairo_line_to(Pcairo_t(Handle), R.Left, R.Bottom);
        cairo_line_to(Pcairo_t(Handle), R.Left, R.Bottom - y1);
      end;
    end;
    cairo_close_path(Pcairo_t(Handle));
    cairo_stroke(Pcairo_t(Handle));
  end;
end;

procedure TvgCanvasCairo.FillRect(const ARect: TvgRect; const xRadius, yRadius: single; const ACorners: TvgCorners; const AOpacity: single;
  const ACornerType: TvgCornerType = vgCornerRound);
var
  x1, x2, y1, y2: single;
  R: TvgRect;
begin
  if Handle = 0 then Exit;

  if FFill.Style <> vgBrushNone then
  begin
    R := ARect;
    ApplyFill(R, AOpacity);
    if ((xRadius = 0) and (yRadius = 0)) or (ACorners = []) then
    begin
      cairo_rectangle(Pcairo_t(Handle), R.Left, R.Top, vgRectWidth(R), vgRectHeight(R));
      cairo_fill(Pcairo_t(Handle));
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
      cairo_new_path(Pcairo_t(Handle));
      cairo_move_to(Pcairo_t(Handle), R.Left, R.Top + y1);
      if vgCornerTopLeft in ACorners then
      begin
        case ACornerType of
          // vgCornetRound - default
          vgCornerBevel: cairo_line_to(Pcairo_t(Handle), R.Left + x1, R.Top);
          vgCornerInnerRound: cairo_curve_to(Pcairo_t(Handle), R.Left + x2, R.Top + y1, R.Left + x1, R.Top + y2, R.Left + x1, R.Top);
          vgCornerInnerLine:
            begin
              cairo_line_to(Pcairo_t(Handle), R.Left + x2, R.Top + y1);
              cairo_line_to(Pcairo_t(Handle), R.Left + x1, R.Top + y2);
              cairo_line_to(Pcairo_t(Handle), R.Left + x1, R.Top);
            end;
        else
          cairo_curve_to(Pcairo_t(Handle), R.Left, R.Top + (y2), R.Left + x2, R.Top, R.Left + x1, R.Top)
        end;
      end
      else
      begin
        cairo_line_to(Pcairo_t(Handle), R.Left, R.Top);
        cairo_line_to(Pcairo_t(Handle), R.Left + x1, R.Top);
      end;
      cairo_line_to(Pcairo_t(Handle), R.Right - x1, R.Top);
      if vgCornerTopRight in ACorners then
      begin
        case ACornerType of
          // vgCornetRound - default
          vgCornerBevel: cairo_line_to(Pcairo_t(Handle), R.Right, R.Top + y1);
          vgCornerInnerRound: cairo_curve_to(Pcairo_t(Handle), R.Right - x1, R.Top + y2, R.Right - x2, R.Top + y1, R.Right, R.Top + y1);
          vgCornerInnerLine:
            begin
              cairo_line_to(Pcairo_t(Handle), R.Right - x1, R.Top + y2);
              cairo_line_to(Pcairo_t(Handle), R.Right - x2, R.Top + y1);
              cairo_line_to(Pcairo_t(Handle), R.Right, R.Top + y1);
            end;
        else
          cairo_curve_to(Pcairo_t(Handle), R.Right - x2, R.Top, R.Right, R.Top + (y2), R.Right, R.Top + y1)
        end;
      end
      else
      begin
        cairo_line_to(Pcairo_t(Handle), R.Right, R.Top);
        cairo_line_to(Pcairo_t(Handle), R.Right, R.Top + y1);
      end;
      cairo_line_to(Pcairo_t(Handle), R.Right, R.Bottom - y1);
      if vgCornerBottomRight in ACorners then
      begin
        case ACornerType of
          // vgCornetRound - default
          vgCornerBevel: cairo_line_to(Pcairo_t(Handle), R.Right - x1, R.Bottom);
          vgCornerInnerRound: cairo_curve_to(Pcairo_t(Handle), R.Right - x2, R.Bottom - y1, R.Right - x1, R.Bottom - y2, R.Right - x1, R.Bottom);
          vgCornerInnerLine:
            begin
              cairo_line_to(Pcairo_t(Handle), R.Right - x2, R.Bottom - y1);
              cairo_line_to(Pcairo_t(Handle), R.Right - x1, R.Bottom - y2);
              cairo_line_to(Pcairo_t(Handle), R.Right - x1, R.Bottom);
            end;
        else
          cairo_curve_to(Pcairo_t(Handle), R.Right, R.Bottom - (y2), R.Right - x2, R.Bottom, R.Right - x1, R.Bottom)
        end;
      end
      else
      begin
        cairo_line_to(Pcairo_t(Handle), R.Right, R.Bottom);
        cairo_line_to(Pcairo_t(Handle), R.Right - x1, R.Bottom);
      end;
      cairo_line_to(Pcairo_t(Handle), R.Left + x1, R.Bottom);
      if vgCornerBottomLeft in ACorners then
      begin
        case ACornerType of
          // vgCornetRound - default
          vgCornerBevel: cairo_line_to(Pcairo_t(Handle), R.Left, R.Bottom - y1);
          vgCornerInnerRound: cairo_curve_to(Pcairo_t(Handle), R.Left + x1, R.Bottom - y2, R.Left + x2, R.Bottom - y1, R.Left, R.Bottom - y1);
          vgCornerInnerLine:
            begin
              cairo_line_to(Pcairo_t(Handle), R.Left + x1, R.Bottom - y2);
              cairo_line_to(Pcairo_t(Handle), R.Left + x2, R.Bottom - y1);
              cairo_line_to(Pcairo_t(Handle), R.Left, R.Bottom - y1);
            end;
        else
          cairo_curve_to(Pcairo_t(Handle), R.Left + x2, R.Bottom, R.Left, R.Bottom - (y2), R.Left, R.Bottom - y1)
        end;
      end
      else
      begin
        cairo_line_to(Pcairo_t(Handle), R.Left, R.Bottom);
        cairo_line_to(Pcairo_t(Handle), R.Left, R.Bottom - y1);
      end;
    end;
    cairo_close_path(Pcairo_t(Handle));
    cairo_fill(Pcairo_t(Handle));
    DeapplyFill;
  end;
end;

procedure TvgCanvasCairo.DrawEllipse(const ARect: TvgRect; const AOpacity: single);
begin
  if Handle = 0 then Exit;

  if FStroke.Style <> vgBrushNone then
  begin
    ApplyStroke(ARect, AOpacity);
    cairo_save(Pcairo_t(Handle));
    cairo_translate(Pcairo_t(Handle), (ARect.Left + ARect.Right) / 2, (ARect.Top + ARect.Bottom) / 2);
    cairo_scale(Pcairo_t(Handle), vgRectWidth(ARect) / 2, vgRectHeight(ARect) / 2);
    cairo_arc(Pcairo_t(Handle), 0, 0, 1, 0, Pi * 2);
    cairo_restore(Pcairo_t(Handle));
    cairo_stroke(Pcairo_t(Handle));
  end;
end;

procedure TvgCanvasCairo.FillEllipse(const ARect: TvgRect; const AOpacity: single);
begin
  if Handle = 0 then Exit;

  if FFill.Style <> vgBrushNone then
  begin
    ApplyFill(ARect, AOpacity);
    cairo_save(Pcairo_t(Handle));
    cairo_translate(Pcairo_t(Handle), (ARect.Left + ARect.Right) / 2, (ARect.Top + ARect.Bottom) / 2);
    cairo_scale(Pcairo_t(Handle), vgRectWidth(ARect) / 2, vgRectHeight(ARect) / 2);
    cairo_arc(Pcairo_t(Handle), 0, 0, 1, 0, Pi * 2);
    cairo_restore(Pcairo_t(Handle));
    cairo_fill(Pcairo_t(Handle));
    DeapplyFill;
  end;
end;

procedure TvgCanvasCairo.FillText(const ARect, AClipRect: TvgRect; const AText: WideString; const WordWrap: boolean;
  const AOpacity: single; const ATextAlign: TvgTextAlign; const AVTextAlign: TvgTextAlign = vgTextAlignCenter);
var
  text: string;
  pdesc: PPangoFontDescription;
  playout: PPangoLayout;
  FullString: String;
  lineWidth: longint;
  pr, pr2: TPangoRectangle;
begin
  if Handle = 0 then Exit;

  ApplyFill(ARect, AOpacity);
  cairo_save(Pcairo_t(Handle));

  cairo_rectangle(Pcairo_t(Handle), ARect.Left, ARect.Top, vgRectWidth(ARect), vgRectHeight(ARect));
  cairo_clip(Pcairo_t(Handle));

  cairo_translate(Pcairo_t(Handle), ARect.left, ARect.Top);

  playout := pango_cairo_create_layout(Pcairo_t(Handle));
  if playout <> nil then
  begin
    FullString := FFont.Family;
    pdesc := pango_font_description_from_string(Pchar(FullString + ' ' + IntToStr(Round(FFont.Size * 0.75))));
    if pdesc = nil then
      pdesc := pango_font_description_from_string(Pchar('Sans ' + IntToStr(Round(FFont.Size * 0.75))));
    if pdesc <> nil then
    begin
      if FFont.Style in [vgFontItalic, vgFontBoldItalic] then
        pango_font_description_set_style(pdesc, PANGO_STYLE_ITALIC);
      if FFont.Style in [vgFontBold, vgFontBoldItalic] then
        pango_font_description_set_weight(pdesc, PANGO_WEIGHT_BOLD);
      pango_layout_set_font_description(playout, pdesc);
      pango_font_description_free(pdesc);
    end;
    pango_layout_set_height(playout, Round(vgRectHeight(ARect) * PANGO_SCALE));

    if WordWrap then
      pango_layout_set_width(playout, Round(vgRectWidth(ARect) * PANGO_SCALE))
    else
      pango_layout_set_width(playout, 5000 * PANGO_SCALE);
    // formating
    case ATextAlign of
        vgTextAlignCenter:
          begin
            pango_layout_set_alignment(playout, PANGO_ALIGN_CENTER);
            if not WordWrap then
              cairo_translate(Pcairo_t(Handle), -2500 + (vgRectWidth(ARect) / 2), 0);
          end;
        vgTextAlignNear:
          begin
            pango_layout_set_alignment(playout, PANGO_ALIGN_LEFT);
          end;
        vgTextAlignFar:
          begin
            pango_layout_set_alignment(playout, PANGO_ALIGN_RIGHT);
            if not WordWrap then
              cairo_translate(Pcairo_t(Handle), -5000 + vgRectWidth(ARect), 0);
          end;
    end;
    text := UTF8Encode(AText);
    pango_layout_set_text(playout, PAnsiChar(text), -1);

    pango_layout_get_pixel_extents(playout, @pr, @pr2);
    case AVTextAlign of
        vgTextAlignCenter:
          begin
            cairo_translate(Pcairo_t(Handle), 0, (vgRectHeight(ARect) - pr2.height) / 2);
          end;
        vgTextAlignNear:
          begin
          end;
        vgTextAlignFar:
          begin
            cairo_translate(Pcairo_t(Handle), 0, (vgRectHeight(ARect) - pr2.height));
          end;
    end;

    pango_cairo_show_layout(Pcairo_t(Handle), playout);

    g_object_unref(playout);
  end;
  cairo_restore(Pcairo_t(Handle));
  DeapplyFill;
end;

procedure TvgCanvasCairo.MeasureText(var ARect: TvgRect;
  AClipRect: TvgRect; const AText: WideString; const WordWrap: boolean;
  const ATextAlign, AVTextAlign: TvgTextAlign);
var
  text: string;
  pdesc: PPangoFontDescription;
  FullString: String;
  lineWidth: longint;
  pr, pr2: TPangoRectangle;
begin
  if not WordWrap then
    ARect.Right := ARect.Left;
  if Length(AText) = 0 then Exit;

  FullString := FFont.Family;
  pdesc := pango_font_description_from_string(Pchar(FullString + ' ' + IntToStr(Round(FFont.Size * 0.75))));
  if pdesc = nil then
    pdesc := pango_font_description_from_string(Pchar('Sans ' + IntToStr(Round(FFont.Size * 0.75))));
  if pdesc <> nil then
  begin
    if FFont.Style in [vgFontItalic, vgFontBoldItalic] then
      pango_font_description_set_style(pdesc, PANGO_STYLE_ITALIC);
    if FFont.Style in [vgFontBold, vgFontBoldItalic] then
      pango_font_description_set_weight(pdesc, PANGO_WEIGHT_BOLD);
    pango_layout_set_font_description(memlayout, pdesc);
    pango_font_description_free(pdesc);
  end;

  pango_layout_set_height(memlayout, Round(vgRectHeight(ARect) * PANGO_SCALE));

  if WordWrap then
    pango_layout_set_width(memlayout, Round(vgRectWidth(ARect) * PANGO_SCALE))
  else
    pango_layout_set_width(memlayout, 5000 * PANGO_SCALE);
  // formating
  case ATextAlign of
      vgTextAlignCenter:
        begin
          pango_layout_set_alignment(memlayout, PANGO_ALIGN_CENTER);
        end;
      vgTextAlignNear:
        begin
          pango_layout_set_alignment(memlayout, PANGO_ALIGN_LEFT);
        end;
      vgTextAlignFar:
        begin
          pango_layout_set_alignment(memlayout, PANGO_ALIGN_RIGHT);
        end;
  end;

  text := UTF8Encode(AText);
  pango_layout_set_text(memlayout, PChar(text), -1);

  pango_layout_get_pixel_extents(memlayout, @pr, @pr2);

  ARect.Right := ARect.Left + pr2.width;
  ARect.Bottom := ARect.Top + pr2.height;
end;

function TvgCanvasCairo.TextToPath(Path: TvgPathData; const ARect: TvgRect; const AText: WideString; const WordWrap: boolean; const ATextAlign: TvgTextAlign;
      const AVTextAlign: TvgTextAlign = vgTextAlignCenter): boolean;
var
  text: string;
  pdesc: PPangoFontDescription;
  FullString: String;
  lineWidth: longint;
  pr, pr2: TPangoRectangle;
  i: integer;
  B: TvgRect;
  W, H, NewW, NewH, VOffset, HOffset: single;
  cpath: Pcairo_path_t;
  pathdata: Pcairo_path_data_t;
  pointtype: cairo_path_data_type_t;
  CP1, CP2: TvgPoint;
begin
  if AText <> '' then
  begin
    Path.Clear;

    FullString := FFont.Family;
    pdesc := pango_font_description_from_string(Pchar(FullString + ' ' + IntToStr(Round(FFont.Size * 0.75))));
    if pdesc = nil then
      pdesc := pango_font_description_from_string(Pchar('Sans ' + IntToStr(Round(FFont.Size * 0.75))));
    if pdesc <> nil then
    begin
      if FFont.Style in [vgFontItalic, vgFontBoldItalic] then
        pango_font_description_set_style(pdesc, PANGO_STYLE_ITALIC);
      if FFont.Style in [vgFontBold, vgFontBoldItalic] then
        pango_font_description_set_weight(pdesc, PANGO_WEIGHT_BOLD);
      pango_layout_set_font_description(memlayout, pdesc);
      pango_font_description_free(pdesc);
    end;

    pango_layout_set_height(memlayout, Round(vgRectHeight(ARect) * PANGO_SCALE));

    if WordWrap then
      pango_layout_set_width(memlayout, Round(vgRectWidth(ARect) * PANGO_SCALE))
    else
      pango_layout_set_width(memlayout, 5000 * PANGO_SCALE);
    // formating
    HOffset := 0;
    case ATextAlign of
        vgTextAlignCenter:
          begin
            pango_layout_set_alignment(memlayout, PANGO_ALIGN_CENTER);
            if not WordWrap then
              HOffset := -2500 + (vgRectWidth(ARect) / 2);
          end;
        vgTextAlignNear:
          begin
            pango_layout_set_alignment(memlayout, PANGO_ALIGN_LEFT);
          end;
        vgTextAlignFar:
          begin
            pango_layout_set_alignment(memlayout, PANGO_ALIGN_RIGHT);
            if not WordWrap then
              HOffset := -5000 + vgRectWidth(ARect);
          end;
    end;

    text := UTF8Encode(AText);
    pango_layout_set_text(memlayout, PAnsiChar(text), -1);

    pango_layout_get_pixel_extents(memlayout, @pr, @pr2);
    case AVTextAlign of
        vgTextAlignCenter:
          begin
            VOffset := (vgRectHeight(ARect) - pr2.height) / 2;
          end;
        vgTextAlignNear:
          begin
            VOffset := 0;
          end;
        vgTextAlignFar:
          begin
            VOffset := vgRectHeight(ARect) - pr2.height;
          end;
    end;
    // create path
    cairo_new_path(memcr);
    // draw text
    pango_cairo_layout_path(memcr, memlayout);
    // parse path
    cpath := cairo_copy_path(memcr);
    if cpath <> nil then
    begin
      pathdata := cpath^.data;
      i := 0;
      while i < cpath^.num_data do
      begin
        // point type
        pointtype := pathdata.header._type;
        Inc(PathData);
        Inc(i);
        if pointtype = CAIRO_PATH_CLOSE_PATH then
        begin
          Path.ClosePath;
          Continue;
        end;
        // point
        if pointtype = CAIRO_PATH_MOVE_TO then
        begin
          Path.MoveTo(vgPoint(pathdata.point.x + HOffset, pathdata.point.y + VOffset));
        end;
        if pointtype = CAIRO_PATH_LINE_TO then
        begin
          Path.LineTo(vgPoint(pathdata.point.x + HOffset, pathdata.point.y + VOffset));
        end;
        if pointtype = CAIRO_PATH_CURVE_TO then
        begin
          CP1 := vgPoint(pathdata.point.x + HOffset, pathdata.point.y + VOffset);
          Inc(PathData);
          Inc(i);
          CP2 := vgPoint(pathdata.point.x + HOffset, pathdata.point.y + VOffset);
          Inc(PathData);
          Inc(i);
          Path.CurveTo(CP1, CP2, vgPoint(pathdata.point.x + HOffset, pathdata.point.y + VOffset));
        end;
        Inc(PathData);
        Inc(i);
      end;
    end;
    Result := true;
    cairo_path_destroy(cpath);
  end
  else
    Result := false;
end;


{ Bitmaps }

procedure TvgCanvasCairo.DrawThumbnail(const ABitmap: TvgBitmap;
  const Width, Height: single);
{var
  scale: single;
  graphics: TGPGraphics;}
begin
{  UpdateBitmap(ABitmap);
  if ABitmap.Handle = 0 then Exit;
  scale := Width / ABitmap.Width;
  if FBitmap <> nil then
  begin
    Fgraphics.ScaleTransform(scale, scale);
    Fgraphics.DrawImage(TGPBitmap(ABitmap.Handle), 0, 0, ABitmap.Width, ABitmap.Height);
  end;}
end;

procedure TvgCanvasCairo.DrawBitmap(const ABitmap: TvgBitmap;
  const SrcRect, DstRect: TvgRect; const AOpacity: single; const HighSpeed: boolean = false);
var
  pat: Pcairo_pattern_t;
  pb, subpb: PGdkPixbuf;
  cm: cairo_matrix_t;
  sc: single;
begin
  UpdateBitmap(ABitmap);
  if Handle = 0 then Exit;
  if ABitmap.Handle = 0 then Exit;

  if (SrcRect.Left <> 0) or (SrcRect.top <> 0) or (SrcRect.right <> ABitmap.Width) or (SrcRect.bottom <> ABitmap.Height) then
  begin
    pb := gdk_pixbuf_new_from_data(Pguchar(ABitmap.StartLine), GDK_COLORSPACE_RGB, true,
      8, ABitmap.Width, ABitmap.Height, ABitmap.Width * 4, nil, nil);
    if pb <> nil then
    begin
      subpb := gdk_pixbuf_new_subpixbuf(pb, Trunc(SrcRect.Left), Trunc(SrcRect.top), trunc(vgRectWidth(SrcRect)), trunc(vgRectHeight(SrcRect)));
      if subpb <> nil then
      begin
        cairo_save(Pcairo_t(Handle));

        gdk_cairo_set_source_pixbuf(Pcairo_t(Handle), subpb, 0, 0);

        pat := cairo_get_source(Pcairo_t(Handle));
        cairo_matrix_init_translate(@cm, -DstRect.Left * (vgRectWidth(SrcRect) / vgRectWidth(DstRect)), -DstRect.Top * (vgRectHeight(SrcRect) / vgRectHeight(DstRect)));
        cairo_matrix_scale(@cm, vgRectWidth(SrcRect) / vgRectWidth(DstRect), vgRectHeight(SrcRect) / vgRectHeight(DstRect));
        cairo_pattern_set_matrix(pat, @cm);
        if HighSpeed then
          cairo_pattern_set_filter(pat, CAIRO_FILTER_FAST)
        else
          cairo_pattern_set_filter(pat, CAIRO_FILTER_BEST);
        cairo_pattern_set_extend(pat, CAIRO_EXTEND_NONE);

        cairo_set_source(Pcairo_t(Handle), pat);
        cairo_rectangle(Pcairo_t(Handle), DstRect.Left, DstRect.Top, vgRectWidth(DstRect), vgRectHeight(DstRect));
        cairo_clip(Pcairo_t(Handle));
        cairo_paint_with_alpha(Pcairo_t(Handle), AOpacity);
        cairo_restore(Pcairo_t(Handle));

        g_object_unref(subpb);
      end;
      g_object_unref(pb);
    end;
    Exit;
  end;
  cairo_save(Pcairo_t(Handle));
  pat := cairo_pattern_create_for_surface(Pcairo_surface_t(ABitmap.Handle));
  cairo_matrix_init_translate(@cm, -DstRect.Left * (vgRectWidth(SrcRect) / vgRectWidth(DstRect)), -DstRect.Top * (vgRectHeight(SrcRect) / vgRectHeight(DstRect)));
  cairo_matrix_scale(@cm, vgRectWidth(SrcRect) / vgRectWidth(DstRect), vgRectHeight(SrcRect) / vgRectHeight(DstRect));
  cairo_pattern_set_matrix(pat, @cm);
  if HighSpeed then
    cairo_pattern_set_filter(pat, CAIRO_FILTER_FAST)
  else
    cairo_pattern_set_filter(pat, CAIRO_FILTER_BEST);
  cairo_pattern_set_extend(pat, CAIRO_EXTEND_NONE);

  cairo_set_source(Pcairo_t(Handle), pat);
  cairo_rectangle(Pcairo_t(Handle), DstRect.Left, DstRect.Top, vgRectWidth(DstRect), vgRectHeight(DstRect));
  cairo_clip(Pcairo_t(Handle));
  cairo_paint_with_alpha(Pcairo_t(Handle), AOpacity);
  cairo_pattern_destroy(pat);
  cairo_restore(Pcairo_t(Handle));
end;

procedure TvgCanvasCairo.UpdateBitmap(ABitmap: TvgBitmap);
begin
  // update bitmap to GDI+ bitmap
  if ABitmap = nil then Exit;
  // create - if need
  if ABitmap.Handle = 0 then
  begin
    ABitmap.Handle := Cardinal(cairo_image_surface_create_for_data(PByte(ABitmap.StartLine), CAIRO_FORMAT_ARGB32,
      ABitmap.Width, ABitmap.Height, ABitmap.Width * 4));
  end;
  // clear flag
  ABitmap.NeedUpdate := false;
  ABitmap.OnDestroyHandle := DoDestroyBitmap;
end;

procedure TvgCanvasCairo.DoDestroyBitmap(Sender: TObject);
begin
  if (Sender <> nil) then
  begin
    if (TvgBitmap(Sender).Handle <> 0) then
      cairo_surface_destroy(Pcairo_surface_t(TvgBitmap(Sender).Handle));
    TvgBitmap(Sender).Handle := 0;
  end;
end;

{ Path }

procedure TvgCanvasCairo.DrawPath(const APath: TvgPathData; const ARect: TvgRect; const AOpacity: single);
var
  i: integer;
  B: TvgRect;
  W, H, NewW, NewH: single;
  CP1, CP2: TvgPoint;
begin
  if Handle = 0 then Exit;
  if FStroke.Style = vgBrushNone then Exit;
  ApplyStroke(ARect, AOpacity);
  B := APath.GetBounds;
  W := vgRectWidth(B);
  H := vgRectHeight(B);
  NewW := vgRectWidth(ARect);
  NewH := vgRectHeight(ARect);
  { draw }
  cairo_save(Pcairo_t(Handle));
  cairo_new_path(Pcairo_t(Handle));
  i := 0;
  while i < Length(APath.PathData) do
  begin
    case APath.PathData[i].Kind of
      vgPathPointMoveTo:
        begin
          cairo_move_to(Pcairo_t(Handle), ARect.Left + (APath.PathData[i].Point.X - B.Left) / W * NewW,
            ARect.Top + (APath.PathData[i].Point.Y - B.Top) / H * NewH);
        end;
      vgPathPointLineTo:
        begin
          cairo_line_to(Pcairo_t(Handle), ARect.Left + (APath.PathData[i].Point.X - B.Left) / W * NewW,
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
          cairo_curve_to(Pcairo_t(Handle), CP1.X, CP1.Y, CP2.X, CP2.Y,
            ARect.Left + (APath.PathData[i].Point.X - B.Left) / W * NewW,
            ARect.Top + (APath.PathData[i].Point.Y - B.Top) / H * NewH);
        end;
      vgPathPointClose:
        begin
          cairo_close_path(Pcairo_t(Handle));
        end;
    end;
    inc(i);
  end;
  cairo_stroke(Pcairo_t(Handle));
  cairo_restore(Pcairo_t(Handle));
end;

procedure TvgCanvasCairo.FillPath(const APath: TvgPathData; const ARect: TvgRect; const AOpacity: single);
var
  i: integer;
  B: TvgRect;
  W, H, NewW, NewH: single;
  CP1, CP2: TvgPoint;
begin
  if Handle = 0 then Exit;
  if FFill.Style = vgBrushNone then Exit;
  ApplyFill(ARect, AOpacity);
  B := APath.GetBounds;
  W := vgRectWidth(B);
  H := vgRectHeight(B);
  NewW := vgRectWidth(ARect);
  NewH := vgRectHeight(ARect);
  cairo_save(Pcairo_t(Handle));
  cairo_new_path(Pcairo_t(Handle));
  i := 0;
  while i < Length(APath.PathData) do
  begin
    case APath.PathData[i].Kind of
      vgPathPointMoveTo:
        begin
          cairo_move_to(Pcairo_t(Handle), ARect.Left + (APath.PathData[i].Point.X - B.Left) / W * NewW,
            ARect.Top + (APath.PathData[i].Point.Y - B.Top) / H * NewH);
        end;
      vgPathPointLineTo:
        begin
          cairo_line_to(Pcairo_t(Handle), ARect.Left + (APath.PathData[i].Point.X - B.Left) / W * NewW,
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
          cairo_curve_to(Pcairo_t(Handle), CP1.X, CP1.Y, CP2.X, CP2.Y,
            ARect.Left + (APath.PathData[i].Point.X - B.Left) / W * NewW,
            ARect.Top + (APath.PathData[i].Point.Y - B.Top) / H * NewH);
        end;
      vgPathPointClose:
        begin
          cairo_close_path(Pcairo_t(Handle));
        end;
    end;
    inc(i);
  end;
  cairo_fill(Pcairo_t(Handle));
  DeApplyFill;
  cairo_restore(Pcairo_t(Handle));
end;

function TvgCanvasCairo.PtInPath(const APoint: TvgPoint; const ARect: TvgRect; const APath: TvgPathData): boolean;
var
  i: integer;
  B: TvgRect;
  W, H, NewW, NewH: single;
  CP1, CP2: TvgPoint;
begin
  Result := false;
  if Handle = 0 then Exit;
  B := APath.GetBounds;
  W := vgRectWidth(B);
  H := vgRectHeight(B);
  NewW := vgRectWidth(ARect);
  NewH := vgRectHeight(ARect);
  cairo_save(Pcairo_t(Handle));
  cairo_new_path(Pcairo_t(Handle));
  i := 0;
  while i < Length(APath.PathData) do
  begin
    case APath.PathData[i].Kind of
      vgPathPointMoveTo:
        begin
          cairo_move_to(Pcairo_t(Handle), ARect.Left + (APath.PathData[i].Point.X - B.Left) / W * NewW,
            ARect.Top + (APath.PathData[i].Point.Y - B.Top) / H * NewH);
        end;
      vgPathPointLineTo:
        begin
          cairo_line_to(Pcairo_t(Handle), ARect.Left + (APath.PathData[i].Point.X - B.Left) / W * NewW,
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
          cairo_curve_to(Pcairo_t(Handle), CP1.X, CP1.Y, CP2.X, CP2.Y,
            ARect.Left + (APath.PathData[i].Point.X - B.Left) / W * NewW,
            ARect.Top + (APath.PathData[i].Point.Y - B.Top) / H * NewH);
        end;
      vgPathPointClose:
        begin
          cairo_close_path(Pcairo_t(Handle));
        end;
    end;
    inc(i);
  end;
  cairo_restore(Pcairo_t(Handle));
  Result := cairo_in_fill(Pcairo_t(Handle), APoint.X, APoint.Y) > 0;
end;

{$ENDIF}

initialization
{$IFDEF LINUX}
  DefaultCanvasClass := TvgCanvasCairo;
  DefaultFilterClass := TvgFilterGtk;
{$ENDIF}
finalization
end.


