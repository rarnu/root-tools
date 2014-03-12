unit vg_canvas_gdip;

{$I vg_define.inc}
{$ALIGN ON}
{$MINENUMSIZE 4}

interface

{$IFDEF WINDOWS}
uses
  Windows, Messages, Classes, SysUtils, vg_scene, ActiveX;
{$ENDIF}

const
  mergegdip = 0;

{$IFDEF WINDOWS}
procedure InitGDIP;
procedure FreeGDIP;
{$ENDIF}

implementation {===============================================================}

{$IFDEF WINDOWS}
{$I vg_gdip.inc}

type

  TvgFilterGdiPlus = class(TvgFilter)
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

  TvgCanvasGdiPlus = class(TvgCanvas)
  private
    FBufferHandle: cardinal;
    FBitmapInfo: TBitmapInfo;
    FGraphics: TGPGraphics;
    FGPPen: TGPPen;
    FGPPenBrush: TGPBrush;
    FGPBrush: TGPBrush;
    FGPFamily: TGPFontFamily;
    FFontCollection: TGPPrivateFontCollection;
    FFontScale: single;
  protected
    procedure UpdateBitmap(ABitmap: TvgBitmap); 
    procedure DestroyBitmap(Sender: TObject);
    procedure ApplyFill(ARect: TvgRect; const AOpacity: single);
    procedure ApplyStroke(ARect: TvgRect; const AOpacity: single);
    procedure FontChanged(Sender: TObject); override;
    procedure IntFillPath(P: TGPGraphicsPath; R: TvgRect; Opacity: single);
    procedure IntFillRect(R: TvgRect; Opacity: single);
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
    function LoadFontFromStream(AStream: TStream): boolean; override;
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

procedure InitGDIP;
begin
  if GdipLibrary <> 0 then Exit;
  LoadGdiplus;
  // Initialize StartupInput structure
  StartupInput.DebugEventCallback := nil;
  StartupInput.SuppressBackgroundThread := false;
  StartupInput.SuppressExternalCodecs   := false;
  StartupInput.GdiplusVersion := 1;
  // Initialize GDI+
  GdiplusStartup(gdiplusToken, @StartupInput, nil);
end;

procedure FreeGDIP;
begin
  if GdipLibrary = 0 then Exit;

  if assigned(GenericSansSerifFontFamily) then GenericSansSerifFontFamily.Free;
  if assigned(GenericSerifFontFamily) then GenericSerifFontFamily.Free;
  if assigned(GenericMonospaceFontFamily) then GenericMonospaceFontFamily.Free;

  if assigned(GenericTypographicStringFormatBuffer) then GenericTypographicStringFormatBuffer.free;
  if assigned(GenericDefaultStringFormatBuffer) then GenericDefaultStringFormatBuffer.Free;
  // Close GDI +
  GdiplusShutdown(gdiplusToken);
  FreeGdiplus;
end;

function GPRectFromRect(const R: TvgRect): TGPRectF;
begin
  Result.X := R.Left;
  Result.Y := R.Top;
  Result.Width := R.Right - R.Left;
  Result.Height := R.Bottom - R.Top;
end;

function GPRectFromRectTruncated(const R: TvgRect): TGPRectF;
begin
  Result.X := Trunc(R.Left);
  Result.Y := Trunc(R.Top);
  Result.Width := Trunc(R.Right) - Trunc(R.Left);
  Result.Height := Trunc(R.Bottom) - Trunc(R.Top);
end;

type

  TMyStreamAdapter = class(TInterfacedObject, IStream)
  public
    FStream: TStream;
    FOwnership: TStreamOwnership;
  public
    constructor Create(Stream: TStream; Ownership: TStreamOwnership = soReference);
    destructor Destroy; override;
    {$IFDEF FPC}
    function Read(pv : Pointer;cb : DWORD;pcbRead : PDWORD) : HRESULT; virtual; stdcall;
    function Write(pv : Pointer;cb : DWORD;pcbWritten : PDWORD): HRESULT; virtual; stdcall;
    {$ELSE}
    function Read(pv: Pointer; cb: Longint; pcbRead: PLongint): HResult; virtual; stdcall;
    function Write(pv: Pointer; cb: Longint; pcbWritten: PLongint): HResult; virtual; stdcall;
    {$ENDIF}
    function Seek(dlibMove: Largeint; dwOrigin: Longint; out libNewPosition: Largeint): HResult; virtual; stdcall;
    function SetSize(libNewSize: Largeint): HResult; virtual; stdcall;
    function CopyTo(stm: IStream; cb: Largeint; out cbRead: Largeint; out cbWritten: Largeint): HResult; virtual; stdcall;
    function Commit(grfCommitFlags: Longint): HResult; virtual; stdcall;
    function Revert: HResult; virtual; stdcall;
    function LockRegion(libOffset: Largeint; cb: Largeint; dwLockType: Longint): HResult; virtual; stdcall;
    function UnlockRegion(libOffset: Largeint; cb: Largeint; dwLockType: Longint): HResult; virtual; stdcall;
    function Stat(out statstg: TStatStg; grfStatFlag: Longint): HResult; virtual; stdcall;
    function Clone(out stm: IStream): HResult; virtual; stdcall;
    property Stream: TStream read FStream;
    property StreamOwnership: TStreamOwnership read FOwnership write FOwnership;
  end;

{ TMyStreamAdapter }

constructor TMyStreamAdapter.Create(Stream: TStream;
  Ownership: TStreamOwnership);
begin
  inherited Create;
  FStream := Stream;
  FOwnership := Ownership;
end;

destructor TMyStreamAdapter.Destroy;
begin
  if FOwnership = soOwned then
  begin
    FStream.Free;
    FStream := nil;
  end;
  inherited Destroy;
end;

{$IFDEF FPC}
function TMyStreamAdapter.Read(pv : Pointer;cb : DWORD;pcbRead : PDWORD): HResult;
{$ELSE}
function TMyStreamAdapter.Read(pv: Pointer; cb: longint; pcbRead: PLongint): HResult;
{$ENDIF}
var
  NumRead: DWORD;
begin
  try
    if pv = Nil then
    begin
      Result := STG_E_INVALIDPOINTER;
      Exit;
    end;
    NumRead := FStream.Read(pv^, cb);
    if pcbRead <> Nil then pcbRead^ := NumRead;
    Result := S_OK;
  except
    Result := S_FALSE;
  end;
end;

function TMyStreamAdapter.LockRegion(libOffset: Largeint; cb: Largeint;
  dwLockType: Longint): HResult;
begin
  Result := STG_E_INVALIDFUNCTION;
end;

function TMyStreamAdapter.UnlockRegion(libOffset: Largeint; cb: Largeint;
  dwLockType: Longint): HResult;
begin
  Result := STG_E_INVALIDFUNCTION;
end;

function TMyStreamAdapter.Commit(grfCommitFlags: Longint): HResult;
begin
  Result := S_OK;
end;

{$IFDEF FPC}
function TMyStreamAdapter.Write(pv : Pointer;cb : DWORD;pcbWritten : PDWORD): HResult;
{$ELSE}
function TMyStreamAdapter.Write(pv: Pointer; cb: Longint; pcbWritten: PLongint): HResult;
{$ENDIF}
var
  NumWritten: DWORD;
begin
  try
    if pv = Nil then
    begin
      Result := STG_E_INVALIDPOINTER;
      Exit;
    end;
    NumWritten := FStream.Write(pv^, cb);
    if pcbWritten <> Nil then pcbWritten^ := NumWritten;
    Result := S_OK;
  except
    Result := STG_E_CANTSAVE;
  end;
end;

function TMyStreamAdapter.Seek(dlibMove: Largeint; dwOrigin: longint;
  out libNewPosition: Largeint): HResult;
var
  NewPos: LargeInt;
begin
  try
    if (dwOrigin < STREAM_SEEK_SET) or (dwOrigin > STREAM_SEEK_END) then
    begin
      Result := STG_E_INVALIDFUNCTION;
      Exit;
    end;
    NewPos := FStream.Seek(dlibMove, dwOrigin);
    if @libNewPosition <> nil then libNewPosition := NewPos;
    Result := S_OK;
  except
    Result := STG_E_INVALIDPOINTER;
  end;
end;

function TMyStreamAdapter.SetSize(libNewSize: Largeint): HResult;
begin
  try
    FStream.Size := libNewSize;
    if libNewSize <> FStream.Size then
      Result := E_FAIL
    else
      Result := S_OK;
  except
    Result := E_UNEXPECTED;
  end;
end;

function TMyStreamAdapter.CopyTo(stm: IStream; cb: Largeint; out cbRead: Largeint;
  out cbWritten: Largeint): HResult;
const
  MaxBufSize = 1024 * 1024;  // 1mb
var
  Buffer: Pointer;
  BufSize, N, I, R: Integer;
  BytesRead, BytesWritten, W: LargeInt;
begin
  Result := S_OK;
  BytesRead := 0;
  BytesWritten := 0;
  try
    if cb > MaxBufSize then
      BufSize := MaxBufSize
    else
      BufSize := Integer(cb);
    GetMem(Buffer, BufSize);
    try
      while cb > 0 do
      begin
        if cb > MaxInt then
          I := MaxInt
        else
          I := cb;
        while I > 0 do
        begin
          if I > BufSize then N := BufSize else N := I;
          R := FStream.Read(Buffer^, N);
          if R = 0 then Exit; // The end of the stream was hit.
          Inc(BytesRead, R);
          W := 0;
          Result := stm.Write(Buffer, R, @W);
          Inc(BytesWritten, W);
          if (Result = S_OK) and (Integer(W) <> R) then Result := E_FAIL;
          if Result <> S_OK then Exit;
          Dec(I, R);
          Dec(cb, R);
        end;
      end;
    finally
      FreeMem(Buffer);
      if (@cbWritten <> nil) then cbWritten := BytesWritten;
      if (@cbRead <> nil) then cbRead := BytesRead;
    end;
  except
    Result := E_UNEXPECTED;
  end;
end;

function TMyStreamAdapter.Revert: HResult;
begin
  Result := STG_E_REVERTED;
end;

function DateTimeToFileTime(FileTime: TDateTime): TFileTime;
var
  LocalFileTime, Ft: TFileTime;
  SystemTime: TSystemTime;
begin
  Result.dwLowDateTime  := 0;
  Result.dwHighDateTime := 0;
  DateTimeToSystemTime(FileTime, SystemTime);
  SystemTimeToFileTime(SystemTime, LocalFileTime);
  LocalFileTimeToFileTime(LocalFileTime, Ft);
  Result := Ft;
end;

function TMyStreamAdapter.Stat(out statstg: TStatStg; grfStatFlag: Longint): HResult;
begin
  Result := S_OK;
  try
    if (@statstg <> nil) then
    begin
      FillChar(statstg, SizeOf(statstg), 0);
      with statstg do
      begin
        dwType := STGTY_STREAM;
        cbSize := Stream.Size;
        mTime := DateTimeToFileTime(now);
        cTime := DateTimeToFileTime(now);
        aTime := DateTimeToFileTime(now);
        grfLocksSupported := LOCK_WRITE;
      end;
    end;
  except
    Result := E_UNEXPECTED;
  end;
end;

function TMyStreamAdapter.Clone(out stm: IStream): HResult;
begin
  Result := E_NOTIMPL;
end;

{ TvgFilterGdiPlus }

class function TvgFilterGdiPlus.GetFileTypes: string;
begin
  Result := '*.bmp;*.jpg;*.jpeg;*.png;*.tif;*.tiff;*.gif;*.ico'
end;

class function TvgFilterGdiPlus.GetImageSize(const AFileName: string): TvgPoint;
var
  img: TGPImage;
  S: TStream;
  adapter: TMyStreamAdapter;
begin
  S := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
  adapter := TMyStreamAdapter.Create(S);
  try
    img := TGPImage.Create(adapter, true);
    Result := vgPoint(img.GetWidth, img.GetHeight);
    img.Free;
  finally
    S.Free;
  end;
end;

function TvgFilterGdiPlus.LoadFromFile(const AFileName: string;
  const Rotate: single; var Bitmap: TvgBitmap): boolean;
var
  img: TGPImage;
  bmp: TGPBitmap;
  graphics: TGPGraphics;
  M, M2: TvgMatrix;
  Pts: array [1..4] of TvgPoint;
  GM: TGPMatrix;
  R: TvgRect;
  BD: TBitmapData;
  S: TStream;
  adapter: TMyStreamAdapter;
begin
  Result := false;
  S := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
  adapter := TMyStreamAdapter.Create(S);
  try
    img := TGPImage.Create(adapter, true);
    if img.GetWidth * img.GetHeight <> 0 then
    begin
      if (frac(Rotate) = 0) and (Trunc(rotate) mod 90 = 0) then
      begin
        // create bitmap and copy to TvgBitmap
        if (Trunc(rotate) mod 360 = 90) or (Trunc(rotate) mod 360 = 270) then
          Bitmap.SetSize(img.GetHeight, img.GetWidth)
        else
          Bitmap.SetSize(img.GetWidth, img.GetHeight);

        bmp := TGPBitmap.Create(adapter, true);
        if Trunc(rotate) mod 360 = 90 then
          bmp.RotateFlip(Rotate90FlipNone);
        if Trunc(rotate) mod 360 = 180 then
          bmp.RotateFlip(Rotate180FlipNone);
        if Trunc(rotate) mod 360 = 270 then
          bmp.RotateFlip(Rotate270FlipNone);
        if bmp.LockBits(MakeRect(0, 0, Bitmap.Width, Bitmap.Height), ImageLockModeRead, PixelFormat32bppPARGB, BD) = OK then
        begin
          vgMoveLongword(BD.Scan0, Bitmap.StartLine, Bitmap.Width * Bitmap.Height);
          bmp.UnlockBits(BD)
        end;
        bmp.Free;
      end
      else
      begin
        M := IdentityMatrix;
        M.m31 := -(img.GetWidth / 2);
        M.m32 := -(img.GetHeight / 2);
        M := vgMatrixMultiply(M, vgCreateRotationMatrix(vgDegToRad(Rotate)));
        { calc new size }
        Pts[1] := vgPointFromVector(vgVectorTransform(vgVector(0, 0), M));
        Pts[2] := vgPointFromVector(vgVectorTransform(vgVector(img.GetWidth, 0), M));
        Pts[3] := vgPointFromVector(vgVectorTransform(vgVector(img.GetWidth, img.GetHeight), M));
        Pts[4] := vgPointFromVector(vgVectorTransform(vgVector(0, img.GetHeight), M));
        R := vgNormalizeRect(Pts);
        { translate }
        M2 := IdentityMatrix;
        M2.m31 := vgRectWidth(R) / 2;
        M2.m32 := vgRectHeight(R) / 2;
        M := vgMatrixMultiply(M, M2);
        { rotate }
        Bitmap.SetSize(Trunc(vgRectWidth(R)), Trunc(vgRectHeight(R)));

        bmp := TGPBitmap.Create(Bitmap.Width, Bitmap.Height, Bitmap.Width * 4, PixelFormat32bppPARGB, PBYTE(Bitmap.StartLine));
        graphics := TGPGraphics.Create(bmp);
        with M do
          GM := TGPMatrix.Create(m11, m12, m21, m22, m31, m32);
        graphics.SetTransform(GM);
        GM.Free;
        graphics.DrawImage(img, 0, 0, img.GetWidth, img.GetHeight);
        graphics.Free;
        bmp.Free;
      end;
      Result := true;
    end;
    img.Free;
  finally
    S.Free;
  end;
end;

function TvgFilterGdiPlus.LoadThumbnailFromFile(const AFileName: string;
  const AFitWidth, AFitHeight: single; const UseEmbedded: boolean; var Bitmap: TvgBitmap): boolean;
var
  R: TvgRect;
  thumb, img: TGPImage;
  bmp: TGPBitmap;
  graphics: TGPGraphics;
  bits: PvgColorArray;
  scale: single;
  S: TStream;
  adapter: TMyStreamAdapter;
begin
  Result := false;
  S := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
  adapter := TMyStreamAdapter.Create(S);
  try
    img := TGPImage.Create(adapter, true);
    if img.GetWidth * img.GetHeight <> 0 then
    begin
      { calc thumb size }
      R := vgRect(0, 0, img.GetWidth, img.GetHeight);
      scale := vgFitRect(R, vgRect(0, 0, AFitWidth, AFitHeight));
      if scale = 0 then scale := 0.001;
      if scale < 1 then scale := 1;
      { create thumb }
      if UseEmbedded then
      begin
        thumb := img.GetThumbnailImage(trunc(img.GetWidth / scale), trunc(img.GetHeight / scale));
        if thumb <> nil then
        begin
          { create bitmap and copy to TvgBitmap }
          Bitmap.SetSize(thumb.GetWidth, thumb.GetHeight);
          bmp := TGPBitmap.Create(Bitmap.Width, Bitmap.Height, Bitmap.Width * 4, PixelFormat32bppPARGB, PBYTE(bitmap.Scanline[0]));
          graphics := TGPGraphics.Create(bmp);
          graphics.DrawImage(thumb, 0, 0, Bitmap.Width, Bitmap.Height);
          graphics.Free;
          bmp.Free;
          thumb.Free;
        end;
      end
      else
      begin
        { create from original }
        Bitmap.SetSize(trunc(vgRectWidth(R)), trunc(vgRectHeight(R)));
        bmp := TGPBitmap.Create(Bitmap.Width, Bitmap.Height, Bitmap.Width * 4, PixelFormat32bppPARGB, PBYTE(bitmap.Scanline[0]));
        bmp.SetResolution(img.GetHorizontalResolution / scale, img.GetVerticalResolution / scale);
        graphics := TGPGraphics.Create(bmp);
        graphics.SetInterpolationMode(QualityModeHigh);
        graphics.DrawImage(img, 0, 0);
        graphics.Free;
        bmp.Free;
      end;
      Result := true;
    end;
    img.Free;
  finally
    S.Free;
  end;
end;

function TvgFilterGdiPlus.LoadFromStream(const AStream: TStream; var Bitmap: TvgBitmap): boolean;
var
  img: TGPImage;
  bmp: TGPBitmap;
  graphics: TGPGraphics;
  adapter: TMyStreamAdapter;
begin
  Result := false;
  adapter := TMyStreamAdapter.Create(AStream);
  img := TGPImage.Create(adapter, true);
  if img.GetWidth * img.GetHeight <> 0 then
  begin
    { create bitmap and copy to TvgBitmap }
    Bitmap.SetSize(img.GetWidth, img.GetHeight);
    bmp := TGPBitmap.Create(Bitmap.Width, Bitmap.Height, Bitmap.Width * 4, PixelFormat32bppPARGB, PBYTE(bitmap.Scanline[0]));
    graphics := TGPGraphics.Create(bmp);
    graphics.DrawImage(img, 0, 0, Bitmap.Width, Bitmap.Height);
    graphics.Free;
    bmp.Free;
    Result := true;
  end;
  img.Free;
end;

function TvgFilterGdiPlus.SaveToStream(const AStream: TStream; var Bitmap: TvgBitmap; const Format: string;
  const Params: string = ''): boolean;
var
  bmp: TGPBitmap;
  adapter: TMyStreamAdapter;
  encoderClsid: TGUID;
  encoderParameters: TEncoderParameters;
  IntValue: integer;
  S, param, name, value: ansistring;
  encoder: ansistring;
  i: integer;
begin
  encoder := 'image/png';
  if LowerCase(Format) = 'jpeg' then encoder := 'image/jpeg';
  if LowerCase(Format) = 'jpg' then encoder := 'image/jpeg';
  if LowerCase(Format) = 'png' then encoder := 'image/png';
  if LowerCase(Format) = 'bmp' then encoder := 'image/bmp';
  if LowerCase(Format) = 'tif' then encoder := 'image/tiff';
  if LowerCase(Format) = 'tiff' then encoder := 'image/tiff';
  if LowerCase(Format) = 'gif' then encoder := 'image/gif';
  if GetEncoderClsid(encoder, encoderClsid) >= 0 then
  begin
    adapter := TMyStreamAdapter.Create(AStream);
    bmp := TGPBitmap.Create(Bitmap.Width, Bitmap.Height, Bitmap.Width * 4, PixelFormat32bppPARGB, PBYTE(bitmap.Scanline[0]));
    { set params }
    if Params <> '' then
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
          encoderParameters.Parameter[i].Guid := EncoderQuality;
          encoderParameters.Parameter[i].NumberOfValues := 1;
          encoderParameters.Parameter[i].Type_ := EncoderParameterValueTypeLong;
          IntValue := strToInt(value);
          encoderParameters.Parameter[i].Value := @IntValue;
        end;
        Inc(i);
      end;
      encoderParameters.Count := i;
      { save }
      bmp.Save(adapter, encoderClsid, @encoderParameters);
    end
    else
      bmp.Save(adapter, encoderClsid);
    bmp.Free;
  end;
end;

function TvgFilterGdiPlus.SaveToFile(const AFileName: string;
  var Bitmap: TvgBitmap; const Params: string = ''): boolean;
var
  S, param, name, value: ansistring;
  bmp: TGPBitmap;
  adapter: TMyStreamAdapter;
  encoderClsid: TGUID;
  encoderType: ansistring;
  i, IntValue: integer;
  encoderParameters: TEncoderParameters;
begin
  encoderType := '';
  if (LowerCase(ExtractFileExt(AFileName)) = '.jpg') or (LowerCase(ExtractFileExt(AFileName)) = '.jpeg') then
    encoderType := 'image/jpeg';
  if (LowerCase(ExtractFileExt(AFileName)) = '.bmp') then
    encoderType := 'image/bmp';
  if (LowerCase(ExtractFileExt(AFileName)) = '.png') then
    encoderType := 'image/png';
  if (LowerCase(ExtractFileExt(AFileName)) = '.tif') or (LowerCase(ExtractFileExt(AFileName)) = '.tiff') then
    encoderType := 'image/tiff';
  if (LowerCase(ExtractFileExt(AFileName)) = '.gif') then
    encoderType := 'image/gif';
  if GetEncoderClsid(encoderType, encoderClsid) >= 0 then
  begin
    bmp := TGPBitmap.Create(Bitmap.Width, Bitmap.Height, Bitmap.Width * 4, PixelFormat32bppPARGB, PBYTE(bitmap.Scanline[0]));
    { set params }
    if Params <> '' then
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
          encoderParameters.Parameter[i].Guid := EncoderQuality;
          encoderParameters.Parameter[i].NumberOfValues := 1;
          encoderParameters.Parameter[i].Type_ := EncoderParameterValueTypeLong;
          IntValue := strToInt(value);
          encoderParameters.Parameter[i].Value := @IntValue;
        end;
        Inc(i);
      end;
      encoderParameters.Count := i;
      { save }
      bmp.Save(AFileName, encoderClsid, @encoderParameters);
    end
    else
      bmp.Save(AFileName, encoderClsid);
    bmp.Free;
  end;
end;

{ TvgCanvasGdiPlus }

const
  imgColorMatrix : TColorMatrix =
    ((1, 0.0, 0.0, 0.0, 0.0),
     (0.0, 1.0, 0.0, 0.0, 0.0),
     (0.0, 0.0, 1.0, 0.0, 0.0),
     (0.0, 0.0, 0.0, 0.1, 0.0),
     (0.0,0.0, 0.0, 0.0, 1.0));
var
  ColorArray: array [0..100] of TGPColor;
  OffsetArray: array [0..100] of single;

function vgStyleToGPStyle(S: TvgFontStyle): integer;
begin
  case S of
    vgFontRegular: Result := FontStyleRegular;
    vgFontBold: Result := FontStyleBold;
    vgFontItalic: Result := FontStyleItalic;
    vgFontBoldItalic: Result := FontStyleBoldItalic;
    vgFontUnderline: Result := FontStyleUnderline;
    vgFontStrikeout: Result := FontStyleStrikeout;
  end;
end;

constructor TvgCanvasGdiPlus.Create(const AWidth, AHeight: integer);
begin
  FBuffered := true;
  inherited ;
  FGPPen := TGPPen.Create($FF000000);
  FGPPenBrush := TGPSolidBrush.Create($FF000000);
  FGPBrush := TGPSolidBrush.Create(InvalideCanvasState);
  FGPFamily := TGPFontFamily.Create('Tahoma');
  FFontScale := 1;
end;

constructor TvgCanvasGdiPlus.CreateFromBitmap(const ABitmap: TvgBitmap);
begin
  inherited;
  FBitmap := ABitmap;
  UpdateBitmap(FBitmap);
  FGraphics := TGPGraphics.Create(TGPBitmap(FBitmap.Handle));
  FGraphics.SetSmoothingMode(SmoothingModeHighQuality);
  FGraphics.SetPixelOffsetMode(PixelOffsetModeHalf);
  FGraphics.SetTextContrast(2);
  FGPPen := TGPPen.Create($FF000000);
  FGPPenBrush := TGPSolidBrush.Create($FF000000);
  FGPBrush := TGPSolidBrush.Create(InvalideCanvasState);
  FGPFamily := TGPFontFamily.Create('Tahoma');
  FFontScale := 96 / FGraphics.GetDpiX;
end;

destructor TvgCanvasGdiPlus.Destroy;
begin
  FreeAndNil(FFontCollection);
  FreeAndNil(FGraphics);
  FreeAndNil(FGPFamily);
  FreeAndNil(FGPBrush);
  FreeAndNil(FGPPenBrush);
  FreeAndNil(FGPPen);
  inherited;
end;

procedure TvgCanvasGdiPlus.FreeBuffer;
begin
  FreeAndNil(FGraphics);
  if FBuffered then
  begin
    if FBufferHandle = 0 then Exit;
    if FHandle <> 0 then DeleteDC(FHandle);
    FHandle := 0;
    if FBufferHandle <> 0 then DeleteObject(FBufferHandle);
    FBufferHandle := 0;
  end;
end;

procedure TvgCanvasGdiPlus.ResizeBuffer(const AWidth, AHeight: integer);
begin
  if (AWidth = FWidth) and (AHeight = FHeight) then Exit;
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
  FGraphics := TGPGraphics.Create(FHandle, 0, 0);
  FGraphics.SetSmoothingMode(QualityModeHigh);
  FGraphics.SetInterpolationMode(InterpolationModeHighQuality);
  FGraphics.SetPixelOffsetMode(PixelOffsetModeHalf);
  FGraphics.SetCompositingMode(CompositingModeSourceOver);
  FGraphics.SetTextContrast(2);
  FFontScale := 96 / FGraphics.GetDpiX;
end;

procedure TvgCanvasGdiPlus.FlushBuffer(const X, Y: integer; const DC: Cardinal);
begin
  if FBufferHandle = 0 then Exit;
  if DC = 0 then Exit;
  Windows.BitBlt(DC, X, Y, FWidth, FHeight, FHandle, 0, 0, SRCCOPY);
end;

procedure TvgCanvasGdiPlus.FlushBufferRect(const X, Y: integer; const DC: Cardinal;
  const ARect: TvgRect);
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

procedure TvgCanvasGdiPlus.Clear(const Color: cardinal);
begin
  if not (FBuffered or (FBitmap <> nil)) then Exit;
  FGraphics.Clear(Color)
end;

procedure TvgCanvasGdiPlus.ClearRect(const ARect: TvgRect; const AColor: TvgColor);
var
  R: TRect;
begin
  if not (FBuffered or (FBitmap <> nil)) then Exit;
  R := Rect(Trunc(ARect.Left), Trunc(ARect.Top), Round(ARect.Right), Round(ARect.Bottom));
  if R.Left < 0 then R.Left := 0;
  if R.Top < 0 then R.Top := 0;
  if R.Top < 0 then R.Top := 0;
  if R.Right > FWidth then R.Right := FWidth;
  if R.Bottom > FHeight then R.Bottom := FHeight;
  if R.Bottom < R.Top then R.Bottom := R.Top;
  if R.Right < R.Left then R.Right := R.Left;
  if (R.Right < 0) or (R.Top < 0) or (R.Left > FWidth) or (R.Top > FHeight) then Exit;
  vgFillLongwordRect(FBufferBits, FWidth, FHeight, R.Left, R.Top, R.Right, R.Bottom, AColor);
end;

class function TvgCanvasGdiPlus.GetBitmapScanline(Bitmap: TvgBitmap; y: integer): PvgColorArray;
begin
  if (y >= 0) and (y < Bitmap.Height) and (Bitmap.StartLine <> nil) then
    Result := @PvgColorArray(Bitmap.StartLine)[(y) * Bitmap.Width]
  else
    Result := nil;
end;

procedure TvgCanvasGdiPlus.SetMatrix(const M: TvgMatrix);
var
  GM: TGPMatrix;
begin
  FMatrix := M;
  with FMatrix do
  begin
    GM := TGPMatrix.Create(m11, m12, m21, m22, m31, m32);
    GM.Shear(m13, m23);
  end;
  FGraphics.SetTransform(GM);
  GM.Free;
end;

procedure TvgCanvasGdiPlus.MultyMatrix(const M: TvgMatrix);
var
  GM: TGPMatrix;
begin
  inherited;
  with M do
  begin
    GM := TGPMatrix.Create(m11, m12, m21, m22, m31, m32);
    GM.Shear(m13, m23);
  end;
  FGraphics.MultiplyTransform(GM);
  GM.Free;
end;

function TvgCanvasGdiPlus.SaveCanvas: cardinal;
var
  i: integer;
begin
  Result := InvalideCanvasState;
  if FGraphics = nil then Exit;

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
  FSaveData[Result].Index := FGraphics.Save;
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

procedure TvgCanvasGdiPlus.RestoreCanvas(const AState: cardinal);
begin
  if FGraphics = nil then Exit;

  if (AState >= 0) and (AState < Length(FSaveData)) then
  begin
    FGraphics.Restore(FSaveData[AState].Index);

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

procedure TvgCanvasGdiPlus.SetClipRects(const ARects: array of TvgRect);
var
  i: integer;
  R: TGPRegion;
  GM: TGPMatrix;
begin
  R := TGPRegion.Create;
  R.MakeEmpty;
  for i := 0 to High(ARects) do
  begin
    R.Union(GPRectFromRect(ARects[i]));
  end;
  FGraphics.SetClip(R);
  R.Free;
end;

procedure TvgCanvasGdiPlus.IntersectClipRect(const ARect: TvgRect);
begin
  FGraphics.IntersectClip(MakeRect(ARect.Left, ARect.Top, ARect.Right - ARect.Left, ARect.Bottom - ARect.Top));
end;

procedure TvgCanvasGdiPlus.ExcludeClipRect(const ARect: TvgRect);
begin
  FGraphics.ExcludeClip(GPRectFromRect(ARect));
end;

procedure TvgCanvasGdiPlus.ResetClipRect;
begin
  FGraphics.ResetClip;
end;

procedure TvgCanvasGdiPlus.ApplyFill(ARect: TvgRect; const AOpacity: single);
var
  C: TvgColor;
  i: integer;
  count: integer;
  B: TvgBitmap;
  P: TGPGraphicsPath;
  M: TGPMatrix;
  CM: TColorMatrix;
  ImageAttributes: TGPImageAttributes;
begin
  if FGPBrush <> nil then FreeAndNil(FGPBrush);
  if (FFill.Style = vgBrushResource) and (FFill.Resource <> nil) and (FFill.Resource.Brush <> nil) then
    FFill.Assign(FFill.Resource.Brush);

  with FFill do
  begin
    case Style of
      vgBrushSolid:
        begin
          FGPBrush := TGPSolidBrush.Create(vgOpacity(SolidColor, AOpacity));
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
              FGPBrush := TGPLinearGradientBrush.Create(MakePoint(ARect.Left + Gradient.StartPosition.X * ARect.Right, ARect.Top + Gradient.StartPosition.Y * ARect.Bottom),
                MakePoint(ARect.Left + Gradient.StopPosition.X * ARect.Right, ARect.Top + Gradient.StopPosition.Y * ARect.Bottom), SolidColor, SolidColor);
              TGPLinearGradientBrush(FGPBrush).SetWrapMode(WrapModeTileFlipX);
              TGPLinearGradientBrush(FGPBrush).SetInterpolationColors(PGPColor(@ColorArray), PSingle(@OffsetArray), Gradient.Points.Count + count);
            end
            else
            begin
              { Radial }
              P := TGPGraphicsPath.Create;
              P.AddEllipse(GPRectFromRect(ARect));
              FGPBrush := TGPPathGradientBrush.Create(P);
              P.Free;
              with Gradient.RadialTransform do
                M := TGPMatrix.Create(Matrix.m11, Matrix.m12, Matrix.m21, Matrix.m22, Matrix.m31, Matrix.m32);
              TGPPathGradientBrush(FGPBrush).SetTransform(M);
              M.Free;
              TGPPathGradientBrush(FGPBrush).SetWrapMode(WrapModeClamp);
              TGPPathGradientBrush(FGPBrush).SetInterpolationColors(PARGB(@ColorArray), PSingle(@OffsetArray), Gradient.Points.Count + count);
            end;
          end
          else
            FGPBrush := TGPSolidBrush.Create(vgOpacity(SolidColor, AOpacity));
        end;
      vgBrushResource:
        begin
          FGPBrush := TGPSolidBrush.Create($00000000);
        end;
      vgBrushVisual:
        begin
          FGPBrush := TGPSolidBrush.Create($00000000);
        end;
      vgBrushBitmap:
        begin
          B := Bitmap.Bitmap;
          if (B <> nil) and (B.ResourceBitmap <> nil) then
            B := B.ResourceBitmap;
          if (B <> nil) and (B.Width > 0) and (B.Height > 0) then
          begin
            UpdateBitmap(B);
            if (B.Handle <> 0) then
            begin
              CM := imgColorMatrix;
              CM[3][3] := AOpacity;
              if Aopacity <> 1 then
              begin
                ImageAttributes := TGPImageAttributes.Create;
                ImageAttributes.SetColorMatrix(CM, ColorMatrixFlagsDefault, ColorAdjustTypeBitmap);
              end
              else
                ImageAttributes := nil;
              if Bitmap.WrapMode <> vgWrapTileStretch then
              begin
                FGPBrush := TGPTextureBrush.Create(TGPBitmap(B.Handle),
                  GPRectFromRect(vgRect(0, 0, B.Width, B.Height)), ImageAttributes);
                TGPTextureBrush(FGPBrush).SetWrapMode(TWrapMode(Bitmap.WrapMode));
              end
              else
              begin
                FGPBrush := TGPTextureBrush.Create(TGPBitmap(B.Handle),
                  GPRectFromRect(vgRect(0, 0, B.Width, B.Height)), ImageAttributes);
                TGPTextureBrush(FGPBrush).SetWrapMode(WrapModeClamp);
                TGPTextureBrush(FGPBrush).ScaleTransform((vgRectWidth(ARect) + (StrokeThickness / 2)) / B.Width,
                  (vgRectHeight(ARect) + (StrokeThickness / 2)) / B.Height);
              end;
              if Aopacity <> 1 then
                ImageAttributes.Free;
            end
            else
              FGPBrush := TGPSolidBrush.Create($00000000);
          end
          else
            FGPBrush := TGPSolidBrush.Create($00000000);
        end;
    else
      FGPBrush := TGPSolidBrush.Create($00000000);
    end;
  end;
end;

procedure TvgCanvasGdiPlus.ApplyStroke(ARect: TvgRect; const AOpacity: single);
var
  i: integer;
begin
  if FGPPen <> nil then FreeAndNil(FGPPen);
  if FGPPenBrush <> nil then FreeAndNil(FGPPenBrush);
  if (FStroke.Style = vgBrushResource) and (FStroke.Resource <> nil) and (FStroke.Resource.Brush <> nil) then
    FStroke.Assign(FStroke.Resource.Brush);

  with FStroke do
  begin
    case Style of
      vgBrushSolid:
        begin
          FGPPenBrush := TGPSolidBrush.Create(vgOpacity(SolidColor, AOpacity));
        end;
      vgBrushGradient:
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
        end;
    else
      FGPPenBrush := TGPSolidBrush.Create($00000000);
    end;
  end;

  FGPPen := TGPPen.Create(FGPPenBrush);
  case StrokeCap of
    vgCapFlat: FGPPen.SetLineCap(LineCapFlat, LineCapFlat, LineCapFlat);
    vgCapRound: FGPPen.SetLineCap(LineCapRound, LineCapRound, LineCapRound);
  end;
  if Length(FDash) > 0 then
  begin
    FGPPen.SetDashOffset(FDashOffset);
    FGPPen.SetDashPattern(@FDash[0], Length(FDash));
  end
  else
    FGPPen.SetDashStyle(DashStyleSolid);
  case StrokeJoin of
    vgJoinMiter: FGPPen.SetLineJoin(LineJoinMiter);
    vgJoinRound: FGPPen.SetLineJoin(LineJoinRound);
    vgJoinBevel: FGPPen.SetLineJoin(LineJoinBevel);
  end;
  FGPPen.SetWidth(StrokeThickness);
end;

procedure TvgCanvasGdiPlus.FontChanged(Sender: TObject);
begin
  FreeAndNil(FGPFamily);
  FGPFamily := TGPFontFamily.Create(FFont.Family, FFontCollection);
  if not FGPFamily.IsAvailable then
  begin
    FGPFamily.Free;
    FGPFamily := TGPFontFamily.Create(FFont.Family);
    if not FGPFamily.IsAvailable then
    begin
      FGPFamily.Free;
      FGPFamily := TGPFontFamily.GenericSansSerif.Clone;
    end
  end;
  if FGraphics <> nil then
  begin
    if FFont.ClearType then
      FGraphics.SetTextRenderingHint(TextRenderingHintClearTypeGridFit)
    else
      FGraphics.SetTextRenderingHint(TextRenderingHintAntiAlias);
  end;
end;

procedure TvgCanvasGdiPlus.DrawLine(const APt1, APt2: TvgPoint; const AOpacity: single);
begin
  if FStroke.Style <> vgBrushNone then
  begin
    ApplyStroke(vgRect(APt1.X, APt1.Y, APt2.X, APt2.Y), AOpacity);
    FGraphics.DrawLine(FGPPen, APt1.X, APt1.Y, APt2.X, APt2.Y);
  end;
end;

procedure TvgCanvasGdiPlus.IntFillRect(R: TvgRect;
  Opacity: single);
var
  Save: cardinal;
  GPR: TGPRectF;
  Visual: TvgVisualObject;
  M: TvgMatrix;
  SaveOpacity: single;
  SaveStroke: TvgBrush;
  SaveStrokeThickness: single;
begin
  if FFill.Style <> vgBrushNone then
  begin
    if FFill.Style = vgBrushVisual then
    begin
      Save := SaveCanvas;
      FGraphics.IntersectClip(GPRectFromRect(R));
      if FFill.Visual.VisualObject <> nil then
      begin
        Visual := FFill.Visual.VisualObject;
        SaveOpacity := Visual.Opacity;
        SaveStroke := TvgBrush.Create(vgBrushSolid, $FF000000);
        SaveStroke.Assign(Stroke);
        SaveStrokeThickness := StrokeThickness;
        Visual.Opacity := Opacity;

        Visual.PaintTo(Self, R);

        Visual.Opacity := SaveOpacity;
        Stroke.Assign(SaveStroke);
        SaveStroke.Free;
        StrokeThickness := SaveStrokeThickness;
      end;
      RestoreCanvas(Save);
    end
    else
    begin
      ApplyFill(R, Opacity);
      FGraphics.FillRectangle(FGPBrush, GPRectFromRect(R));
    end;
  end;
end;

procedure TvgCanvasGdiPlus.IntFillPath(P: TGPGraphicsPath; R: TvgRect;
  Opacity: single);
var
  Save: cardinal;
  Rg: TGPRegion;
  Visual: TvgVisualObject;
  M: TvgMatrix;
  SaveOpacity: single;
  SaveStroke: TvgBrush;
  SaveStrokeThickness: single;
begin
  if FFill.Style <> vgBrushNone then
  begin
    if FFill.Style = vgBrushVisual then
    begin
      Save := SaveCanvas;
      Rg := TGPRegion.Create(P);
      FGraphics.IntersectClip(Rg);
      Rg.Free;
      if FFill.Visual.VisualObject <> nil then
      begin
        Visual := FFill.Visual.VisualObject;
        SaveOpacity := Visual.Opacity;
        SaveStroke := TvgBrush.Create(vgBrushSolid, $FF000000);
        SaveStroke.Assign(Stroke);
        SaveStrokeThickness := StrokeThickness;
        Visual.Opacity := Opacity;

        Visual.PaintTo(Self, R);

        Visual.Opacity := SaveOpacity;
        Stroke.Assign(SaveStroke);
        SaveStroke.Free;
        StrokeThickness := SaveStrokeThickness;
      end;
      RestoreCanvas(Save);
    end
    else
    begin
      ApplyFill(R, Opacity);
      FGraphics.FillPath(FGPBrush, P);
    end;
  end;
end;

procedure TvgCanvasGdiPlus.DrawRect(const ARect: TvgRect; const xRadius, yRadius: single; const ACorners: TvgCorners;
  const AOpacity: single; const ACornerType: TvgCornerType = vgCornerRound);
var
  Path: TGPGraphicsPath;
  x1, x2, y1, y2: single;
  R: TvgRect;
begin
  if FStroke.Style <> vgBrushNone then
  begin
    R := ARect;
    ApplyStroke(R, AOpacity);
    if (xRadius < Epsilon) and (yRadius < Epsilon) then
    begin
      FGraphics.DrawRectangle(FGPPen, GPRectFromRect(R));
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
      Path := TGPGraphicsPath.Create;
      if vgCornerTopLeft in ACorners then
      begin
        case ACornerType of
          // vgCornetRound - default
          vgCornerBevel: Path.AddLine(R.Left, R.Top + y1, R.Left + x1, R.Top);
          vgCornerInnerRound: Path.AddBezier(R.Left, R.Top + y1, R.Left + x2, R.Top + y1, R.Left + x1, R.Top + y2, R.Left + x1, R.Top);
          vgCornerInnerLine:
            begin
              Path.AddLine(R.Left, R.Top + y1, R.Left + x2, R.Top + y1);
              Path.AddLine(R.Left + x2, R.Top + y1, R.Left + x1, R.Top + y2);
              Path.AddLine(R.Left + x1, R.Top + y2, R.Left + x1, R.Top);
            end;
        else
          Path.AddBezier(R.Left, R.Top + y1, R.Left, R.Top + (y2), R.Left + x2, R.Top, R.Left + x1, R.Top)
        end;
      end
      else
      begin
        Path.AddLine(R.Left, R.Top + y1, R.Left, R.Top);
        Path.AddLine(R.Left, R.Top, R.Left + x1, R.Top);
      end;
      if vgCornerTopRight in ACorners then
      begin
        case ACornerType of
          // vgCornetRound - default
          vgCornerBevel: Path.AddLine(R.Right - x1, R.Top, R.Right, R.Top + y1);
          vgCornerInnerRound: Path.AddBezier(R.Right - x1, R.Top, R.Right - x1, R.Top + y2, R.Right - x2, R.Top + y1, R.Right, R.Top + y1);
          vgCornerInnerLine:
            begin
              Path.AddLine(R.Right - x1, R.Top, R.Right - x1, R.Top + y2);
              Path.AddLine(R.Right - x1, R.Top + y2, R.Right - x2, R.Top + y1);
              Path.AddLine(R.Right - x2, R.Top + y1, R.Right, R.Top + y1);
            end;
        else
          Path.AddBezier(R.Right - x1, R.Top, R.Right - x2, R.Top, R.Right, R.Top + (y2), R.Right, R.Top + y1)
        end;
      end
      else
      begin
        Path.AddLine(R.Right - x1, R.Top, R.Right, R.Top);
        Path.AddLine(R.Right, R.Top, R.Right, R.Top + y1);
      end;
      if vgCornerBottomRight in ACorners then
      begin
        case ACornerType of
          // vgCornetRound - default
          vgCornerBevel: Path.AddLine(R.Right, R.Bottom - y1, R.Right - x1, R.Bottom);
          vgCornerInnerRound: Path.AddBezier(R.Right, R.Bottom - y1, R.Right - x2, R.Bottom - y1, R.Right - x1, R.Bottom - y2, R.Right - x1, R.Bottom);
          vgCornerInnerLine:
            begin
              Path.AddLine(R.Right, R.Bottom - y1, R.Right - x2, R.Bottom - y1);
              Path.AddLine(R.Right - x2, R.Bottom - y1, R.Right - x1, R.Bottom - y2);
              Path.AddLine(R.Right - x1, R.Bottom - y2, R.Right - x1, R.Bottom);
            end;
        else
          Path.AddBezier(R.Right, R.Bottom - y1, R.Right, R.Bottom - (y2), R.Right - x2, R.Bottom, R.Right - x1, R.Bottom)
        end;
      end
      else
      begin
        Path.AddLine(R.Right, R.Bottom - y1, R.Right, R.Bottom);
        Path.AddLine(R.Right, R.Bottom, R.Right - x1, R.Bottom);
      end;
      if vgCornerBottomLeft in ACorners then
      begin
        case ACornerType of
          // vgCornetRound - default
          vgCornerBevel: Path.AddLine(R.Left + x1, R.Bottom, R.Left, R.Bottom - y1);
          vgCornerInnerRound: Path.AddBezier(R.Left + x1, R.Bottom, R.Left + x1, R.Bottom - y2, R.Left + x2, R.Bottom - y1, R.Left, R.Bottom - y1);
          vgCornerInnerLine:
            begin
              Path.AddLine(R.Left + x1, R.Bottom, R.Left + x1, R.Bottom - y2);
              Path.AddLine(R.Left + x1, R.Bottom - y2, R.Left + x2, R.Bottom - y1);
              Path.AddLine(R.Left + x2, R.Bottom - y1, R.Left, R.Bottom - y1);
            end;
        else
          Path.AddBezier(R.Left + x1, R.Bottom, R.Left + x2, R.Bottom, R.Left, R.Bottom - (y2), R.Left, R.Bottom - y1)
        end;
      end
      else
      begin
        Path.AddLine(R.Left + x1, R.Bottom, R.Left, R.Bottom);
        Path.AddLine(R.Left, R.Bottom, R.Left, R.Bottom - y1);
      end;
      {$IFDEF FLATTEN}
      Path.Flatten();
      {$ENDIF}
      Path.CloseFigure;
      FGraphics.DrawPath(FGPPen, Path);
      Path.Free;
    end;
  end;
end;

procedure TvgCanvasGdiPlus.FillRect(const ARect: TvgRect; const xRadius, yRadius: single; const ACorners: TvgCorners; const AOpacity: single;
  const ACornerType: TvgCornerType = vgCornerRound);
var
  Path: TGPGraphicsPath;
  x1, x2, y1, y2: single;
  R: TvgRect;
begin
  if FFill.Style <> vgBrushNone then
  begin
    R := ARect;
    if ((xRadius = 0) and (yRadius = 0)) or (ACorners = []) then
    begin
      IntFillRect(R, AOpacity);
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
      Path := TGPGraphicsPath.Create;
      if vgCornerTopLeft in ACorners then
      begin
        case ACornerType of
          // vgCornetRound - default
          vgCornerBevel: Path.AddLine(R.Left, R.Top + y1, R.Left + x1, R.Top);
          vgCornerInnerRound: Path.AddBezier(R.Left, R.Top + y1, R.Left + x2, R.Top + y1, R.Left + x1, R.Top + y2, R.Left + x1, R.Top);
          vgCornerInnerLine:
            begin
              Path.AddLine(R.Left, R.Top + y1, R.Left + x2, R.Top + y1);
              Path.AddLine(R.Left + x2, R.Top + y1, R.Left + x1, R.Top + y2);
              Path.AddLine(R.Left + x1, R.Top + y2, R.Left + x1, R.Top);
            end;
        else
          Path.AddBezier(R.Left, R.Top + y1, R.Left, R.Top + (y2), R.Left + x2, R.Top, R.Left + x1, R.Top)
        end;
      end
      else
      begin
        Path.AddLine(R.Left, R.Top + y1, R.Left, R.Top);
        Path.AddLine(R.Left, R.Top, R.Left + x1, R.Top);
      end;
      if vgCornerTopRight in ACorners then
      begin
        case ACornerType of
          // vgCornetRound - default
          vgCornerBevel: Path.AddLine(R.Right - x1, R.Top, R.Right, R.Top + y1);
          vgCornerInnerRound: Path.AddBezier(R.Right - x1, R.Top, R.Right - x1, R.Top + y2, R.Right - x2, R.Top + y1, R.Right, R.Top + y1);
          vgCornerInnerLine:
            begin
              Path.AddLine(R.Right - x1, R.Top, R.Right - x1, R.Top + y2);
              Path.AddLine(R.Right - x1, R.Top + y2, R.Right - x2, R.Top + y1);
              Path.AddLine(R.Right - x2, R.Top + y1, R.Right, R.Top + y1);
            end;
        else
          Path.AddBezier(R.Right - x1, R.Top, R.Right - x2, R.Top, R.Right, R.Top + (y2), R.Right, R.Top + y1)
        end;
      end
      else
      begin
        Path.AddLine(R.Right - x1, R.Top, R.Right, R.Top);
        Path.AddLine(R.Right, R.Top, R.Right, R.Top + y1);
      end;
      if vgCornerBottomRight in ACorners then
      begin
        case ACornerType of
          // vgCornetRound - default
          vgCornerBevel: Path.AddLine(R.Right, R.Bottom - y1, R.Right - x1, R.Bottom);
          vgCornerInnerRound: Path.AddBezier(R.Right, R.Bottom - y1, R.Right - x2, R.Bottom - y1, R.Right - x1, R.Bottom - y2, R.Right - x1, R.Bottom);
          vgCornerInnerLine:
            begin
              Path.AddLine(R.Right, R.Bottom - y1, R.Right - x2, R.Bottom - y1);
              Path.AddLine(R.Right - x2, R.Bottom - y1, R.Right - x1, R.Bottom - y2);
              Path.AddLine(R.Right - x1, R.Bottom - y2, R.Right - x1, R.Bottom);
            end;
        else
          Path.AddBezier(R.Right, R.Bottom - y1, R.Right, R.Bottom - (y2), R.Right - x2, R.Bottom, R.Right - x1, R.Bottom)
        end;
      end
      else
      begin
        Path.AddLine(R.Right, R.Bottom - y1, R.Right, R.Bottom);
        Path.AddLine(R.Right, R.Bottom, R.Right - x1, R.Bottom);
      end;
      if vgCornerBottomLeft in ACorners then
      begin
        case ACornerType of
          // vgCornetRound - default
          vgCornerBevel: Path.AddLine(R.Left + x1, R.Bottom, R.Left, R.Bottom - y1);
          vgCornerInnerRound: Path.AddBezier(R.Left + x1, R.Bottom, R.Left + x1, R.Bottom - y2, R.Left + x2, R.Bottom - y1, R.Left, R.Bottom - y1);
          vgCornerInnerLine:
            begin
              Path.AddLine(R.Left + x1, R.Bottom, R.Left + x1, R.Bottom - y2);
              Path.AddLine(R.Left + x1, R.Bottom - y2, R.Left + x2, R.Bottom - y1);
              Path.AddLine(R.Left + x2, R.Bottom - y1, R.Left, R.Bottom - y1);
            end;
        else
          Path.AddBezier(R.Left + x1, R.Bottom, R.Left + x2, R.Bottom, R.Left, R.Bottom - (y2), R.Left, R.Bottom - y1)
        end;
      end
      else
      begin
        Path.AddLine(R.Left + x1, R.Bottom, R.Left, R.Bottom);
        Path.AddLine(R.Left, R.Bottom, R.Left, R.Bottom - y1);
      end;
      {$IFDEF FLATTEN}
      Path.Flatten();
      {$ENDIF}
      Path.CloseFigure;
      IntFillPath(Path, ARect, AOpacity);
      Path.Free;
    end;
  end;
end;

procedure TvgCanvasGdiPlus.DrawEllipse(const ARect: TvgRect; const AOpacity: single);
var
  R: TvgRect;
  P: TGPGraphicsPath;
begin
  if FStroke.Style <> vgBrushNone then
  begin
    ApplyStroke(ARect, AOpacity);
    R := vgRect(ARect.left, ARect.Top, ARect.Right, ARect.Bottom);
    FGraphics.DrawEllipse(FGPPen, GPRectFromRect(R));
  end;
end;

procedure TvgCanvasGdiPlus.FillEllipse(const ARect: TvgRect; const AOpacity: single);
var
  R: TvgRect;
  P: TGPGraphicsPath;
begin
  if FFill.Style <> vgBrushNone then
  begin
    P := TGPGraphicsPath.Create();
    R := vgRect(ARect.left, ARect.Top, ARect.Right - ARect.Left, ARect.Bottom - ARect.Top);
    P.AddEllipse(R.Left, R.Top, R.Right, R.Bottom);
    IntFillPath(P, R, AOpacity);
    P.Free;
  end;
end;

function TvgCanvasGdiPlus.LoadFontFromStream(AStream: TStream): boolean;
var
  Stream: TMemoryStream;
begin
  if FFontCollection = nil then
    FFontCollection := TGPPrivateFontCollection.Create;
  Stream := TMemoryStream.Create;
  Stream.CopyFrom(AStream, AStream.Size);
  FFontCollection.AddMemoryFont(Stream.Memory, Stream.Size);
  Stream.Free;
end;

procedure TvgCanvasGdiPlus.FillText(const ARect, AClipRect: TvgRect; const AText: WideString; const WordWrap: boolean;
  const AOpacity: single; const ATextAlign: TvgTextAlign; const AVTextAlign: TvgTextAlign = vgTextAlignCenter);
var
  StringFormat: TGPStringFormat;
  Font: TGPFont;
  R: TvgRect;
  S: TvgFontStyle;
begin
  if (FFill.Style <> vgBrushNone) and (AText <> '') then                    
  begin
    StringFormat := TGPStringFormat.Create(StringFormatFlagsNoClip);
    if not WordWrap then
      StringFormat.SetFormatFlags(StringFormat.GetFormatFlags or StringFormatFlagsNoWrap);
    if not FGPFamily.IsStyleAvailable(vgStyleToGPStyle(FFont.Style)) then
    begin
      Font := nil;
      for S := vgFontRegular to vgFontStrikeout do
      begin
        if FGPFamily.IsStyleAvailable(vgStyleToGPStyle(S)) then
        begin
          Font := TGPFont.Create(FGPFamily, (FFont.Size * 0.75 * FFontScale), vgStyleToGPStyle(S));
          Break;
        end;
      end;
      if Font = nil then
        Font := TGPFont.Create(FGPFamily, (FFont.Size * 0.75 * FFontScale), vgStyleToGPStyle(FFont.Style));
    end
    else
      Font := TGPFont.Create(FGPFamily, (FFont.Size * 0.75 * FFontScale), vgStyleToGPStyle(FFont.Style), UnitPoint);
    // formating
    case ATextAlign of
      vgTextAlignCenter:
        begin
          stringFormat.SetAlignment(StringAlignmentCenter);
        end;
      vgTextAlignNear:
        begin
          stringFormat.SetAlignment(StringAlignmentNear);
        end;
      vgTextAlignFar:
        begin
          stringFormat.SetAlignment(StringAlignmentFar);
        end;
    end;
    case AVTextAlign of
      vgTextAlignCenter:
        begin
          stringFormat.SetLineAlignment(StringAlignmentCenter);
        end;
      vgTextAlignNear:
        begin
          stringFormat.SetLineAlignment(StringAlignmentNear);
        end;
      vgTextAlignFar:
        begin
          stringFormat.SetLineAlignment(StringAlignmentFar);
        end;
    end;
    // calc correct rect
    if AOpacity = 1 then
      ApplyFill(ARect, AOpacity * 0.99)
    else
      ApplyFill(ARect, AOpacity);
    if (frac(FMatrix.m11) <> 0) or (frac(FMatrix.m22) <> 0) then
    begin
      FGraphics.SetTextRenderingHint(TextRenderingHintAntiAlias);
      FGraphics.DrawString(AText, -1, Font, GPRectFromRect(ARect), StringFormat, FGPBrush);
      if FFont.ClearType then
        FGraphics.SetTextRenderingHint(TextRenderingHintClearTypeGridFit)
      else
        FGraphics.SetTextRenderingHint(TextRenderingHintAntiAlias);
    end
    else
    begin
      FGraphics.DrawString(AText, -1, Font, GPRectFromRect(ARect), StringFormat, FGPBrush);
    end;
      
    Font.Free;
    StringFormat.Free;
  end;
end;

procedure TvgCanvasGdiPlus.MeasureText(var ARect: TvgRect;
  AClipRect: TvgRect; const AText: WideString; const WordWrap: boolean;
  const ATextAlign, AVTextAlign: TvgTextAlign);
var
  StringFormat: TGPStringFormat;
  Font: TGPFont;
  GR: TGPRectF;
  charRanges: array[0..2] of TCharacterRange;
  pCharRangeRegions: array of TGPRegion;
  i, count: Integer;
  S: TvgFontStyle;
  B: TGPBitmap;
  G: TGPGraphics;
begin
  if not WordWrap then
    ARect.Right := ARect.Left;
  if Length(AText) = 0 then Exit;

  if FGraphics = nil then
  begin
    B := TGPBitmap.Create(1, 1, PixelFormat32bppARGB);
    G := TGPGraphics.Create(B);
    G.SetSmoothingMode(QualityModeHigh);
    G.SetInterpolationMode(InterpolationModeHighQuality);
    G.SetPixelOffsetMode(PixelOffsetModeHalf);
    if FFont.ClearType then
      G.SetTextRenderingHint(TextRenderingHintClearTypeGridFit)
    else
      G.SetTextRenderingHint(TextRenderingHintAntiAlias);
    FGraphics := G;
  end
  else
  begin
    G := nil;
  end;

  if (frac(FMatrix.m11) <> 0) or (frac(FMatrix.m22) <> 0) then
    FGraphics.SetTextRenderingHint(TextRenderingHintAntiAlias);

  StringFormat := TGPStringFormat.Create(StringFormatFlagsMeasureTrailingSpaces or StringFormatFlagsNoClip);
  if not WordWrap then
    StringFormat.SetFormatFlags(StringFormat.GetFormatFlags or StringFormatFlagsNoWrap);
  { measure rect height}
  if not FGPFamily.IsStyleAvailable(vgStyleToGPStyle(FFont.Style)) then
  begin
    Font := nil;
    for S := vgFontRegular to vgFontStrikeout do
    begin
      if FGPFamily.IsStyleAvailable(vgStyleToGPStyle(S)) then
      begin
        Font := TGPFont.Create(FGPFamily, (FFont.Size * 0.75 * FFontScale), vgStyleToGPStyle(S));
        Break;
      end;
    end;
    if Font = nil then
      Font := TGPFont.Create(FGPFamily, (FFont.Size * 0.75 * FFontScale), vgStyleToGPStyle(FFont.Style));
  end
  else
    Font := TGPFont.Create(FGPFamily, (FFont.Size * 0.75 * FFontScale), vgStyleToGPStyle(FFont.Style));
  // formating
  case ATextAlign of
    vgTextAlignCenter:
      begin
        stringFormat.SetAlignment(StringAlignmentCenter);
      end;
    vgTextAlignNear:
      begin
        stringFormat.SetAlignment(StringAlignmentNear);
      end;
    vgTextAlignFar:
      begin
        stringFormat.SetAlignment(StringAlignmentFar);
      end;
  end;
  case AVTextAlign of
    vgTextAlignCenter:
      begin
        stringFormat.SetLineAlignment(StringAlignmentCenter);
      end;
    vgTextAlignNear:
      begin
        stringFormat.SetLineAlignment(StringAlignmentNear);
      end;
    vgTextAlignFar:
      begin
        stringFormat.SetLineAlignment(StringAlignmentFar);
      end;
  end;
  // set char range
  charRanges[0] := MakeCharacterRange(0, Length(AText));
  StringFormat.SetMeasurableCharacterRanges(1, @charRanges);
  count := StringFormat.GetMeasurableCharacterRangeCount;
  SetLength(pCharRangeRegions, count);
  if count > 0 then
    for i := 0 to count-1 do
      pCharRangeRegions[i] := TGPRegion.Create;
  // measure
  FGraphics.MeasureCharacterRanges(AText, -1, Font, GPRectFromRect(ARect), StringFormat, count, pCharRangeRegions);
  for i := 0 to count - 1 do
  begin
    pCharRangeRegions[i].GetBounds(GR, FGraphics);
    if i = 0 then
      ARect := vgRect(GR.X, GR.Y, GR.X + GR.Width, GR.Y + GR.Height)
    else
      ARect := vgUnionRect(ARect, vgRect(GR.X, GR.Y, GR.X + GR.Width, GR.Y + GR.Height));
  end;

  if count > 0 then
    for i := 0 to count-1 do
      pCharRangeRegions[i].Free;
  SetLength(pCharRangeRegions, 0);
  // free
  Font.Free;
  StringFormat.Free;

  if G <> nil then
  begin
    G.Free;
    B.Free;
    FGraphics := nil;
  end
  else
  begin
    if FFont.ClearType then
      FGraphics.SetTextRenderingHint(TextRenderingHintClearTypeGridFit)
    else
      FGraphics.SetTextRenderingHint(TextRenderingHintAntiAlias);
  end;
end;

function TvgCanvasGdiPlus.TextToPath(Path: TvgPathData; const ARect: TvgRect; const AText: WideString; const WordWrap: boolean; const ATextAlign: TvgTextAlign;
   const AVTextAlign: TvgTextAlign = vgTextAlignCenter): boolean;
var
  StringFormat: TGPStringFormat;
  GPPath: TGPGraphicsPath;
  Bmp: TGPBitmap;
  FGraphics: TGPGraphics;
  Bits: Cardinal;
  i: integer;
  SP, CP1, CP2: TvgPoint;
  Data: TPathData;
  SavePoints: PGPPointF;
begin
  Result := false;
  if (AText <> '') then
  begin
    if not FGPFamily.IsStyleAvailable(vgStyleToGPStyle(FFont.Style)) then Exit;

    Path.Clear;
    Bmp := TGPBitmap.Create(1, 1, 1 * 4, PixelFormat32bppARGB, @Bits);
    FGraphics := TGPGraphics.Create(Bmp);

    StringFormat := TGPStringFormat.Create(0);
    if not WordWrap then
      StringFormat.SetFormatFlags(StringFormat.GetFormatFlags or StringFormatFlagsNoWrap or StringFormatFlagsMeasureTrailingSpaces);
    GPPath := TGPGraphicsPath.Create;
    // formating
    case ATextAlign of
      vgTextAlignCenter:
        begin
          stringFormat.SetAlignment(StringAlignmentCenter);
        end;
      vgTextAlignNear:
        begin
          stringFormat.SetAlignment(StringAlignmentNear);
        end;
      vgTextAlignFar:
        begin
          stringFormat.SetAlignment(StringAlignmentFar);
        end;
    end;
    case AVTextAlign of
      vgTextAlignCenter:
        begin
          stringFormat.SetLineAlignment(StringAlignmentCenter);
        end;
      vgTextAlignNear:
        begin
          stringFormat.SetLineAlignment(StringAlignmentNear);
        end;
      vgTextAlignFar:
        begin
          stringFormat.SetLineAlignment(StringAlignmentFar);
        end;
    end;
    // path
    GPPath.AddString(AText, -1, FGPFamily, vgStyleToGPStyle(FFont.Style), FFont.Size, GPRectFromRect(ARect), StringFormat);
    // expand path
    Data := TPathData.Create;
    GPPath.GetPathData(Data);
    SavePoints := Data.Points;
    // calc size
    i := 0;
    while i < Data.Count do
    begin
      if PByteArray(Data.Types)[i] = PathPointTypeStart then
      begin
        SP := vgPoint(Data.Points^.X, Data.Points^.Y);
        Path.MoveTo(vgPoint(Data.Points^.X, Data.Points^.Y));
      end;
      if PByteArray(Data.Types)[i] and PathPointTypeBezier = PathPointTypeBezier then
      begin
        CP1 := vgPoint(Data.Points^.X, Data.Points^.Y);
        Inc(i);
        Inc(Data.Points);
        CP2 := vgPoint(Data.Points^.X, Data.Points^.Y);
        Inc(i);
        Inc(Data.Points);
        Path.CurveTo(CP1, CP2, vgPoint(Data.Points^.X, Data.Points^.Y));
      end;
      if PByteArray(Data.Types)[i] and PathPointTypeLine = PathPointTypeLine then
      begin
        Path.LineTo(vgPoint(Data.Points^.X, Data.Points^.Y));
      end;
      if PByteArray(Data.Types)[i] and PathPointTypeCloseSubpath = PathPointTypeCloseSubpath then
      begin
        Path.ClosePath;
      end;
      Inc(i);
      Inc(Data.Points);
    end;
    Result := true;
    //
    Data.Points := SavePoints;
    Data.Free;
    StringFormat.Free;
    GPPath.Free;

    // free
    FGraphics.Free;
    Bmp.Free;
  end
  else
    Result := false;
end;

{ Bitmaps }

procedure TvgCanvasGdiPlus.DrawThumbnail(const ABitmap: TvgBitmap;
  const Width, Height: single);
var
  scale: single;
  graphics: TGPGraphics;
begin
  UpdateBitmap(ABitmap);
  if ABitmap.Handle = 0 then Exit;
  scale := Width / ABitmap.Width;
  if FBitmap <> nil then
  begin
    Fgraphics.ScaleTransform(scale, scale);
    Fgraphics.DrawImage(TGPBitmap(ABitmap.Handle), 0, 0, ABitmap.Width, ABitmap.Height);
  end;
end;

procedure TvgCanvasGdiPlus.DrawBitmap(const ABitmap: TvgBitmap;
  const SrcRect, DstRect: TvgRect; const AOpacity: single; const HighSpeed: boolean = false);
var
  CM: TColorMatrix;
  ImageAttributes: TGPImageAttributes;
  Pts: array [1..4] of TvgPoint;
begin
  if HighSpeed then
    FGraphics.SetInterpolationMode(InterpolationModeNearestNeighbor)
  else
    FGraphics.SetInterpolationMode(QualityModeHigh);
  if (AOpacity < 1) then
  begin
    if (AOpacity = 0) then Exit;
    CM := ImgColorMatrix;
    CM[3][3] := AOpacity;
    UpdateBitmap(ABitmap);
    if ABitmap.Handle = 0 then Exit;
    ImageAttributes := TGPImageAttributes.Create;
    ImageAttributes.SetColorMatrix(
      CM,
      ColorMatrixFlagsDefault,
      ColorAdjustTypeBitmap);
    FGraphics.DrawImage(TGPBitmap(ABitmap.Handle), MakeRect(DstRect.Left, DstRect.Top, DstRect.Right - DstRect.Left, DstRect.Bottom - DstRect.Top),
      SrcRect.Left, SrcRect.Top, SrcRect.Right - SrcRect.Left, SrcRect.Bottom - SrcRect.Top,
      UnitPixel, ImageAttributes);
    ImageAttributes.Free;
  end
  else
  begin
    UpdateBitmap(ABitmap);
    if ABitmap.Handle = 0 then Exit;
    FGraphics.DrawImage(TGPBitmap(ABitmap.Handle), MakeRect(DstRect.Left, DstRect.Top, DstRect.Right - DstRect.Left, DstRect.Bottom - DstRect.Top),
      SrcRect.Left, SrcRect.Top, SrcRect.Right - SrcRect.Left, SrcRect.Bottom - SrcRect.Top,
      UnitPixel);
  end;
end;

procedure TvgCanvasGdiPlus.UpdateBitmap(ABitmap: TvgBitmap);
var
  BD: TBitmapData;
  j: integer;
begin
  { update bitmap to GDI+ bitmap }
  if ABitmap = nil then Exit;
  if ABitmap.IsEmpty then Exit;
  { create - if need }
  if ABitmap.Handle = 0 then
  begin
    ABitmap.Handle := Cardinal(TGPBitmap.Create(ABitmap.Width, ABitmap.Height,
      ABitmap.Width * 4, PixelFormat32bppPARGB, PBYTE(ABitmap.StartLine)));
    ABitmap.NeedUpdate := true;
    ABitmap.OnDestroyHandle := DestroyBitmap;
  end;
  { resize if need }
  if (ABitmap.Width <> TGPBitmap(ABitmap.Handle).GetWidth) or
     (ABitmap.Height <> TGPBitmap(ABitmap.Handle).GetHeight) then
  begin
    TGPBitmap(ABitmap.Handle).Destroy;
    ABitmap.Handle := Cardinal(TGPBitmap.Create(ABitmap.Width, ABitmap.Height,
      ABitmap.Width * 4, PixelFormat32bppPARGB, PBYTE(ABitmap.StartLine)));
    ABitmap.NeedUpdate := true;
  end;
  { clear flag }
  ABitmap.NeedUpdate := false;
end;

procedure TvgCanvasGdiPlus.DestroyBitmap(Sender: TObject);
begin
  if (Sender <> nil) then
  begin
    if (TvgBitmap(Sender).Handle <> 0) then
      TGPBitmap(TvgBitmap(Sender).Handle).Free;
    TvgBitmap(Sender).Handle := 0;
  end;
end;

{ Path }

procedure TvgCanvasGdiPlus.DrawPath(const APath: TvgPathData; const ARect: TvgRect; const AOpacity: single);
var
  B: TvgRect;
  i: integer;
  W, H, NewW, NewH: single;
  CP1, CP2, CP, SP: TvgPoint;
begin
  if not APath.IsEmpty then
  begin
    B := APath.GetBounds;
    W := vgRectWidth(B);
    H := vgRectHeight(B);
    if W * H = 0 then Exit;
    ApplyStroke(APath.GetBounds, AOpacity);
    NewW := vgRectWidth(ARect);
    NewH := vgRectHeight(ARect);
    i := 0;
    while i < Length(APath.PathData) do
    begin
      case APath.PathData[i].Kind of
        vgPathPointMoveTo:
          begin
            CP.X := ARect.Left + (APath.PathData[i].Point.X - B.Left) / W * NewW;
            CP.Y := ARect.Top + (APath.PathData[i].Point.Y - B.Top) / H * NewH;
            SP := CP;
          end;
        vgPathPointLineTo:
          begin
            FGraphics.DrawLine(FGPPen, CP.X, CP.Y,
              ARect.Left + (APath.PathData[i].Point.X - B.Left) / W * NewW,
              ARect.Top + (APath.PathData[i].Point.Y - B.Top) / H * NewH);
            CP.X := ARect.Left + (APath.PathData[i].Point.X - B.Left) / W * NewW;
            CP.Y := ARect.Top + (APath.PathData[i].Point.Y - B.Top) / H * NewH;
          end;
        vgPathPointCurveTo:
          begin
            CP1 := vgPoint(ARect.Left + (APath.PathData[i].Point.X - B.Left) / W * NewW, ARect.Top + (APath.PathData[i].Point.Y - B.Top) / H * NewH);
            Inc(i);
            CP2 := vgPoint(ARect.Left + (APath.PathData[i].Point.X - B.Left) / W * NewW, ARect.Top + (APath.PathData[i].Point.Y - B.Top) / H * NewH);
            Inc(i);
            FGraphics.DrawBezier(FGPPen, CP.X, CP.Y,
              CP1.X,
              CP1.Y,
              CP2.X,
              CP2.Y,
              ARect.Left + (APath.PathData[i].Point.X - B.Left) / W * NewW,
              ARect.Top + (APath.PathData[i].Point.Y - B.Top) / H * NewH
            );
            CP.X := ARect.Left + (APath.PathData[i].Point.X - B.Left) / W * NewW;
            CP.Y := ARect.Top + (APath.PathData[i].Point.Y - B.Top) / H * NewH;
          end;
        vgPathPointClose:
          begin
            FGraphics.DrawLine(FGPPen, CP.X, CP.Y, SP.X, SP.Y);
          end;
      end;
      Inc(i);
    end;
  end;
end;

procedure TvgCanvasGdiPlus.FillPath(const APath: TvgPathData; const ARect: TvgRect; const AOpacity: single);
var
  B: TvgRect;
  i: integer;
  W, H, NewW, NewH: single;
  CP, CP1, CP2: TvgPoint;
  P: TGPGraphicsPath;
begin
  if not APath.IsEmpty then
  begin
    B := APath.GetBounds;
    W := vgRectWidth(B);
    H := vgRectHeight(B);
    if W * H = 0 then Exit;
    NewW := vgRectWidth(ARect);
    NewH := vgRectHeight(ARect);
    P := TGPGraphicsPath.Create;
    i := 0;
    while i < Length(APath.PathData) do
    begin
      case APath.PathData[i].Kind of
        vgPathPointMoveTo:
          begin
            CP.X := ARect.Left + (APath.PathData[i].Point.X - B.Left) / W * NewW;
            CP.Y := ARect.Top + (APath.PathData[i].Point.Y - B.Top) / H * NewH;
            P.StartFigure;
          end;
        vgPathPointLineTo:
          begin
            P.AddLine(CP.X, CP.Y,
              ARect.Left + (APath.PathData[i].Point.X - B.Left) / W * NewW,
              ARect.Top + (APath.PathData[i].Point.Y - B.Top) / H * NewH);
            CP.X := ARect.Left + (APath.PathData[i].Point.X - B.Left) / W * NewW;
            CP.Y := ARect.Top + (APath.PathData[i].Point.Y - B.Top) / H * NewH;
          end;
        vgPathPointCurveTo:
          begin
            CP1 := vgPoint(ARect.Left + (APath.PathData[i].Point.X - B.Left) / W * NewW, ARect.Top + (APath.PathData[i].Point.Y - B.Top) / H * NewH);
            Inc(i);
            CP2 := vgPoint(ARect.Left + (APath.PathData[i].Point.X - B.Left) / W * NewW, ARect.Top + (APath.PathData[i].Point.Y - B.Top) / H * NewH);
            Inc(i);
            P.AddBezier(CP.X, CP.Y,
              CP1.X,
              CP1.Y,
              CP2.X,
              CP2.Y,
              ARect.Left + (APath.PathData[i].Point.X - B.Left) / W * NewW,
              ARect.Top + (APath.PathData[i].Point.Y - B.Top) / H * NewH
            );
            CP.X := ARect.Left + (APath.PathData[i].Point.X - B.Left) / W * NewW;
            CP.Y := ARect.Top + (APath.PathData[i].Point.Y - B.Top) / H * NewH;
          end;
        vgPathPointClose:
          begin
            P.CloseFigure;
          end;
      end;
      Inc(i);
    end;

    IntFillPath(P, ARect, AOpacity);
    P.Free;
  end;
end;

function TvgCanvasGdiPlus.PtInPath(const APoint: TvgPoint; const ARect: TvgRect; const APath: TvgPathData): boolean;
var
  B: TvgRect;
  i: integer;
  W, H, NewW, NewH: single;
  CP, CP1, CP2: TvgPoint;
  P: TGPGraphicsPath;
begin
  Result := false;
  if not vgPtInRect(APoint, ARect) then
    Result := false
  else
  begin
    B := APath.GetBounds;
    W := vgRectWidth(B);
    H := vgRectHeight(B);
    if W * H = 0 then Exit;
    NewW := vgRectWidth(ARect);
    NewH := vgRectHeight(ARect);
    P := TGPGraphicsPath.Create;
    i := 0;
    while i < Length(APath.PathData) do
    begin
      case APath.PathData[i].Kind of
        vgPathPointMoveTo:
          begin
            CP.X := ARect.Left + (APath.PathData[i].Point.X - B.Left) / W * NewW;
            CP.Y := ARect.Top + (APath.PathData[i].Point.Y - B.Top) / H * NewH;
            P.StartFigure;
          end;
        vgPathPointLineTo:
          begin
            P.AddLine(CP.X, CP.Y,
              ARect.Left + (APath.PathData[i].Point.X - B.Left) / W * NewW,
              ARect.Top + (APath.PathData[i].Point.Y - B.Top) / H * NewH);
            CP.X := ARect.Left + (APath.PathData[i].Point.X - B.Left) / W * NewW;
            CP.Y := ARect.Top + (APath.PathData[i].Point.Y - B.Top) / H * NewH;
          end;
        vgPathPointCurveTo:
          begin
            CP1 := vgPoint(ARect.Left + (APath.PathData[i].Point.X - B.Left) / W * NewW, ARect.Top + (APath.PathData[i].Point.Y - B.Top) / H * NewH);
            Inc(i);
            CP2 := vgPoint(ARect.Left + (APath.PathData[i].Point.X - B.Left) / W * NewW, ARect.Top + (APath.PathData[i].Point.Y - B.Top) / H * NewH);
            Inc(i);
            P.AddBezier(CP.X, CP.Y,
              CP1.X,
              CP1.Y,
              CP2.X,
              CP2.Y,
              ARect.Left + (APath.PathData[i].Point.X - B.Left) / W * NewW,
              ARect.Top + (APath.PathData[i].Point.Y - B.Top) / H * NewH
            );
            CP.X := ARect.Left + (APath.PathData[i].Point.X - B.Left) / W * NewW;
            CP.Y := ARect.Top + (APath.PathData[i].Point.Y - B.Top) / H * NewH;
          end;
        vgPathPointClose:
          begin
            P.CloseFigure;
          end;
      end;
      Inc(i);
    end;

    Result := P.IsVisible(APoint.X, APoint.Y);

    P.Free;
  end;
end;
{$ENDIF}

initialization
  {$IFDEF WINDOWS}
  DefaultFilterClass := TvgFilterGdiPlus;
  if DefaultCanvasClass = nil then
    DefaultCanvasClass := TvgCanvasGdiPlus;
  DefaultPrinterCanvasClass := TvgCanvasGdiPlus;
  {$IFNDEF vg_no_init_gdip}
  InitGDIP;
  {$ENDIF}
  {$ENDIF}
finalization
  {$IFDEF WINDOWS}
  {$IFNDEF vg_no_free_gdip}
  FreeGDIP;
  {$ENDIF}
  {$ENDIF}
end.
