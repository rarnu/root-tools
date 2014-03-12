unit vg_effects;

{$I vg_define.inc}

interface

uses
  Classes, SysUtils, vg_scene;

type

  TvgShadowEffect = class(TvgEffect)
  private
    FDistance: single;
    FSoftness: single;
    FShadowColor: TvgColor;
    FOpacity: single;
    FDirection: single;
    procedure SetDistance(const Value: single);
    procedure SetSoftness(const Value: single);
    procedure SetShadowColor(const Value: string);
    procedure SetOpacity(const Value: single);
    function GetShadowColor: string;
    procedure SetDirection(const Value: single);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetRect(const ARect: TvgRect): TvgRect; override;
    function GetOffset: TvgPoint; override;
    procedure ProcessEffect(Canvas: TvgCanvas; const Visual: TvgBitmap; const Data: single); override;
  published
    property Distance: single read FDistance write SetDistance;
    property Direction: single read FDirection write SetDirection;
    property Softness: single read FSoftness write SetSoftness;
    property Opacity: single read FOpacity write SetOpacity;
    property ShadowColor: string read GetShadowColor write SetShadowColor;
  end;

  TvgBlurEffect = class(TvgEffect)
  private
    FSoftness: single;
    procedure SetSoftness(const Value: single);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetRect(const ARect: TvgRect): TvgRect; override;
    function GetOffset: TvgPoint; override;
    procedure ProcessEffect(Canvas: TvgCanvas; const Visual: TvgBitmap; const Data: single); override;
  published
    property Softness: single read FSoftness write SetSoftness;
  end;

  TvgGlowEffect = class(TvgEffect)
  private
    FSoftness: single;
    FGlowColor: TvgColor;
    FOpacity: single;
    procedure SetSoftness(const Value: single);
    procedure SetOpacity(const Value: single);
    function GetGlowColor: string;
    procedure SetGlowColor(const Value: string);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetRect(const ARect: TvgRect): TvgRect; override;
    function GetOffset: TvgPoint; override;
    procedure ProcessEffect(Canvas: TvgCanvas; const Visual: TvgBitmap; const Data: single); override;
  published
    property Softness: single read FSoftness write SetSoftness;
    property Opacity: single read FOpacity write SetOpacity;
    property GlowColor: string read GetGlowColor write SetGlowColor;
  end;

  TvgInnerGlowEffect = class(TvgGlowEffect)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetRect(const ARect: TvgRect): TvgRect; override;
    function GetOffset: TvgPoint; override;
    procedure ProcessEffect(Canvas: TvgCanvas; const Visual: TvgBitmap; const Data: single); override;
  published
  end;

  TvgBevelEffect = class(TvgEffect)
  private
    FDirection: single;
    FSize: integer;
    procedure SetDirection(const Value: single);
    procedure SetSize(const Value: integer);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetRect(const ARect: TvgRect): TvgRect; override;
    function GetOffset: TvgPoint; override;
    procedure ProcessEffect(Canvas: TvgCanvas; const Visual: TvgBitmap; const Data: single); override;
  published
    property Direction: single read FDirection write SetDirection;
    property Size: integer read FSize write SetSize;
  end;

  TvgReflectionEffect = class(TvgEffect)
  private
    FOffset: integer;
    FOpacity: single;
    FLength: single;
    procedure SetOpacity(const Value: single);
    procedure SetOffset(const Value: integer);
    procedure SetLength(const Value: single);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetRect(const ARect: TvgRect): TvgRect; override;
    function GetOffset: TvgPoint; override;
    procedure ProcessEffect(Canvas: TvgCanvas; const Visual: TvgBitmap; const Data: single); override;
  published
    property Opacity: single read FOpacity write SetOpacity;
    property Offset: integer read FOffset write SetOffset;
    property Length: single read FLength write SetLength;
  end;

procedure Blur(const Canvas: TvgCanvas; const Bitmap: TvgBitmap; const Radius: integer; UseAlpha: boolean = true);

implementation {===============================================================}

uses math;

procedure Blur(const Canvas: TvgCanvas; const Bitmap: TvgBitmap; const Radius: integer; UseAlpha: boolean = true);
var
  pix: PvgColorArray;
  w, h, wm, hm, wh, vdiv: integer;
  rsum,gsum,bsum,asum,x,y,i,yp,yi,yw: integer;
  P: cardinal;
  divsum: integer;
  stackpointer, stackstart: integer;
  sir: PvgColorRec;
  rbs, r1, routsum, goutsum, boutsum, aoutsum, rinsum, ginsum, binsum, ainsum: integer;
  dv: PIntArray;
  vmin: PIntArray;
  r, g, b, a: PIntArray;
  stack: PvgColorArray;
begin
  if (radius<1) then Exit;

  pix := Bitmap.Scanline[0];

    w := Bitmap.width;
    h := Bitmap.height;
    wm := w - 1;
    hm := h - 1;
    wh := w * h;
    vdiv := radius + radius + 1;

    GetMem(r, wh * SizeOf(Integer));
    GetMem(g, wh * SizeOf(Integer));
    GetMem(b, wh * SizeOf(Integer));
    GetMem(a, wh * SizeOf(Integer));
    GetMem(vmin, max(w, h) * SizeOf(Integer));
    divsum := (vdiv + 1) shr 1;
    divsum := divsum * divsum;
    GetMem(dv, 256 * divsum * SizeOf(Integer));
    for i := 0 to 256 * divsum - 1 do
      dv[i] := (i div divsum);

    yw := 0;
    yi := 0;

    GetMem(stack, vdiv * SizeOf(TvgColor));

    r1 := radius + 1;

    for y := 0 to h - 1 do
    begin
      rinsum := 0;
      ginsum := 0;
      binsum := 0;
      ainsum := 0;
      routsum := 0;
      goutsum := 0;
      boutsum := 0;
      aoutsum := 0;
      rsum := 0;
      gsum := 0;
      bsum :=0;
      asum :=0;
      for i := -radius to radius do
      begin
        p := pix[yi+min(wm,max(i,0))];
        sir := @stack[i + radius];
        sir.Color := p;
        rbs := r1-abs(i);
        rsum := rsum + (sir.r*rbs);
        gsum := gsum + (sir.g*rbs);
        bsum := bsum + (sir.b*rbs);
        if UseAlpha then
        asum := asum + (sir.a*rbs);
        if (i > 0) then
        begin
          rinsum := rinsum + sir.r;
          ginsum := ginsum + sir.g;
          binsum := binsum + sir.b;
          if UseAlpha then
          ainsum := ainsum + sir.a;
        end else
        begin
          routsum := routsum + sir.r;
          goutsum := goutsum + sir.g;
          boutsum := boutsum + sir.b;
          if UseAlpha then
          aoutsum := aoutsum + sir.a;
        end
      end;
      stackpointer := radius;

      for x := 0 to w - 1 do
      begin
        r[yi] := dv[rsum];
        g[yi] := dv[gsum];
        b[yi] := dv[bsum];
        if UseAlpha then
        a[yi] := dv[asum];

        rsum := rsum - routsum;
        gsum := gsum - goutsum;
        bsum := bsum - boutsum;
        if UseAlpha then
        asum := asum - aoutsum;

        stackstart := stackpointer-radius+vdiv;
        sir := @stack[stackstart mod vdiv];

        routsum := routsum - sir.r;
        goutsum := goutsum - sir.g;
        boutsum := boutsum - sir.b;
        if UseAlpha then
        aoutsum := aoutsum - sir.a;

        if (y=0)then
        begin
          vmin[x] := min(x+radius+1,wm);
        end;
        p := pix[yw+vmin[x]];
        sir.color := p;

        rinsum := rinsum + sir.r;
        ginsum := ginsum + sir.g;
        binsum := binsum + sir.b;
        if UseAlpha then
        ainsum := ainsum + sir.a;

        rsum := rsum + rinsum;
        gsum := gsum + ginsum;
        bsum := bsum + binsum;
        if UseAlpha then
        asum := asum + ainsum;

        stackpointer :=(stackpointer+1) mod vdiv;
        sir := @stack[(stackpointer) mod vdiv];

        routsum := routsum + sir.r;
        goutsum := goutsum + sir.g;
        boutsum := boutsum + sir.b;
        if UseAlpha then
        aoutsum := aoutsum + sir.a;

        rinsum := rinsum - sir.r;
        ginsum := ginsum - sir.g;
        binsum := binsum - sir.b;
        if UseAlpha then
        ainsum := ainsum - sir.a;

        yi := yi + 1;
      end;
      yw := yw + w;
    end;

    for x := 0 to w - 1 do
    begin
      rinsum := 0;
      ginsum := 0;
      binsum := 0;
      ainsum := 0;
      routsum := 0;
      goutsum := 0;
      boutsum := 0;
      aoutsum := 0;
      rsum := 0;
      gsum := 0;
      bsum :=0;
      asum :=0;
      yp := -radius * w;
      for i := -radius to radius do
      begin
        yi := max(0,yp) + x;

        sir := @stack[i+radius];

        sir.r := r[yi];
        sir.g := g[yi];
        sir.b := b[yi];
        if UseAlpha then
        sir.a := a[yi];

        rbs := r1 - abs(i);

        rsum := rsum + (r[yi]*rbs);
        gsum := gsum + (g[yi]*rbs);
        bsum := bsum + (b[yi]*rbs);
        if UseAlpha then
        asum := asum + (a[yi]*rbs);

        if (i > 0)then
        begin
          rinsum := rinsum + sir.r;
          ginsum := ginsum + sir.g;
          binsum := binsum + sir.b;
          if UseAlpha then
          ainsum := ainsum + sir.a;
        end else
        begin
          routsum := routsum + sir.r;
          goutsum := goutsum + sir.g;
          boutsum := boutsum + sir.b;
          if UseAlpha then
          aoutsum := aoutsum + sir.a;
        end;

        if (i < hm) then
        begin
          yp := yp + w;
        end
      end;
      yi := x;
      stackpointer := radius;
      for y := 0 to h - 1 do
      begin
        pix[yi] := (dv[asum] shl 24) or (dv[rsum] shl 16) or (dv[gsum] shl 8) or dv[bsum];

        rsum := rsum - routsum;
        gsum := gsum - goutsum;
        bsum := bsum - boutsum;
        if UseAlpha then
        asum := asum - aoutsum;

        stackstart := stackpointer-radius+vdiv;
        sir := @stack[stackstart mod vdiv];

        routsum := routsum - sir.r;
        goutsum := goutsum - sir.g;
        boutsum := boutsum - sir.b;
        if UseAlpha then
        aoutsum := aoutsum - sir.a;

        if (x = 0) then
        begin
          vmin[y] := min(y+r1,hm)*w;
        end;
        p := x + vmin[y];

        sir.r := r[p];
        sir.g := g[p];
        sir.b := b[p];
        if UseAlpha then
        sir.a := a[p];

        rinsum := rinsum + sir.r;
        ginsum := ginsum + sir.g;
        binsum := binsum + sir.b;
        if UseAlpha then
        ainsum := ainsum + sir.a;

        rsum := rsum + rinsum;
        gsum := gsum + ginsum;
        bsum := bsum + binsum;
        if UseAlpha then
        asum := asum + ainsum;

        stackpointer := (stackpointer + 1) mod vdiv;
        sir := @stack[stackpointer];

        routsum := routsum + sir.r;
        goutsum := goutsum + sir.g;
        boutsum := boutsum + sir.b;
        if UseAlpha then
        aoutsum := aoutsum + sir.a;

        rinsum := rinsum - sir.r;
        ginsum := ginsum - sir.g;
        binsum := binsum - sir.b;
        if UseAlpha then
        ainsum := ainsum - sir.a;

        yi := yi + w;
      end;
    end;
    FreeMem(stack, vdiv * SizeOf(TvgColor));
    FreeMem(dv, 256 * divsum * SizeOf(Integer));
    FreeMem(vmin, max(w, h) * SizeOf(Integer));
    FreeMem(a, wh * SizeOf(Integer));
    FreeMem(r, wh * SizeOf(Integer));
    FreeMem(g, wh * SizeOf(Integer));
    FreeMem(b, wh * SizeOf(Integer));
end;

{ TvgShadowEffect ===================================================================}

constructor TvgShadowEffect.Create(AOwner: TComponent);
begin
  inherited;
//  DisablePaint := true;
  FShadowColor := $FF000000;
  FDirection := 45;
  FOpacity := 0.6;
  FSoftness := 0.3;
  FDistance := 3;
end;

destructor TvgShadowEffect.Destroy;
begin
  inherited;
end;

function TvgShadowEffect.GetOffset: TvgPoint;
var
  S, C: single;
begin
  vgSinCos(vgDegToRad(FDirection), S, C);
  Result := vgPoint(Trunc(FSoftness * 20), Trunc(FSoftness * 20));
end;

function TvgShadowEffect.GetRect(const ARect: TvgRect): TvgRect;
var
  S, C: single;
begin
  Result := ARect;
  vgInflateRect(Result, Trunc(FSoftness * 20), Trunc(FSoftness * 20));
  vgSinCos(vgDegToRad(FDirection), S, C);
  vgOffsetRect(Result, FDistance * C, FDistance * S);
end;

function TvgShadowEffect.GetShadowColor: string;
begin
  Result := vgColorToStr(FShadowColor);
end;

procedure TvgShadowEffect.ProcessEffect(Canvas: TvgCanvas; const Visual: TvgBitmap; const Data: single);
var
  Bits: PvgColorRecArray;
  Mask: PByteArray;
  Sn, Cs: single;
  Off: TvgPoint;
begin
  { create mask }
  Mask := Visual.CreateMask;
  { fill color }
  Visual.FillColor(vgOpacity(vgCorrectColor(FShadowColor), FOpacity));
  { stack blur version }
  Blur(Canvas, Visual, Trunc(5 + FSoftness * 10));
  { apply mask }
  vgSinCos(vgDegToRad(FDirection), Sn, Cs);
  Off := vgPoint(FDistance * Cs * Data, FDistance * Sn * Data);
  Visual.ApplyMask(Mask, -round(Off.X), -round(Off.Y));
  { free mask }
  FreeMem(Mask, Visual.Width * Visual.Height);
end;

procedure TvgShadowEffect.SetDirection(const Value: single);
begin
  if FDirection <> Value then
  begin
    FDirection := Value;
    UpdateParentEffects;
  end;
end;

procedure TvgShadowEffect.SetDistance(const Value: single);
begin
  if FDistance <> Value then
  begin
    FDistance := Value;
    UpdateParentEffects;
  end;
end;

procedure TvgShadowEffect.SetOpacity(const Value: single);
begin
  if FOpacity <> Value then
  begin
    FOpacity := Value;
    if FOpacity < 0 then FOpacity := 0;
    if FOpacity > 1 then FOpacity := 1;
    UpdateParentEffects;
  end;
end;

procedure TvgShadowEffect.SetShadowColor(const Value: string);
begin
  if FShadowColor <> vgStrToColor(Value) then
  begin
    FShadowColor := vgStrToColor(Value);
    UpdateParentEffects;
  end;
end;

procedure TvgShadowEffect.SetSoftness(const Value: single);
begin
  if FSoftness <> Value then
  begin
    FSoftness := Value;
    if FSoftness < 0 then FSoftness := 0;
    if FSoftness > 3 then FSoftness := 3;
    UpdateParentEffects;
  end;
end;

{ TvgBlurEffect ===================================================================}

constructor TvgBlurEffect.Create(AOwner: TComponent);
begin
  inherited;
  DisablePaint := true;
  FSoftness := 0.3;
end;

destructor TvgBlurEffect.Destroy;
begin
  inherited;
end;

function TvgBlurEffect.GetOffset: TvgPoint;
begin
  Result := vgPoint(Trunc(FSoftness * 20), Trunc(FSoftness * 20));
end;

function TvgBlurEffect.GetRect(const ARect: TvgRect): TvgRect;
begin
  Result := ARect;
  vgInflateRect(Result, Trunc(FSoftness * 20), Trunc(FSoftness * 20));
end;

procedure TvgBlurEffect.ProcessEffect(Canvas: TvgCanvas; const Visual: TvgBitmap; const Data: single);
begin
  { stack blur version }
  if FSoftness > 0 then
    Blur(Canvas, Visual, Trunc(FSoftness * 15));
end;

procedure TvgBlurEffect.SetSoftness(const Value: single);
begin
  if FSoftness <> Value then
  begin
    FSoftness := Value;
    if FSoftness < 0 then FSoftness := 0;
    if FSoftness > 9 then FSoftness := 9;
    UpdateParentEffects;
  end;
end;

{ TvgGlowEffect ===================================================================}

constructor TvgGlowEffect.Create(AOwner: TComponent);
begin
  inherited;
//  DisablePaint := true;
  FOpacity := 0.9;
  FGlowColor := $FFFFD700;
  FSoftness := 0.4;
end;

destructor TvgGlowEffect.Destroy;
begin
  inherited;
end;

function TvgGlowEffect.GetGlowColor: string;
begin
  Result := vgColorToStr(FGlowColor);
end;

function TvgGlowEffect.GetOffset: TvgPoint;
begin
  Result := vgPoint(4 + Trunc(FSoftness * 23), 4 + Trunc(FSoftness * 23));
end;

function TvgGlowEffect.GetRect(const ARect: TvgRect): TvgRect;
begin
  Result := ARect;
  vgInflateRect(Result, 4 + Trunc(FSoftness * 23), 4 + Trunc(FSoftness * 23));
end;

procedure TvgGlowEffect.ProcessEffect(Canvas: TvgCanvas; const Visual: TvgBitmap; const Data: single);
var
  Bits: PvgColorRecArray;
  Mask: PByteArray;
begin
  { create mask }
  Mask := Visual.CreateMask;
  { fill color }
  Visual.FillColor(vgOpacity(vgCorrectColor(FGlowColor), FOpacity));
  { stack blur version }
  Blur(Canvas, Visual, Trunc(5 + FSoftness * 10));
  { apply mask }
  Visual.ApplyMask(Mask);
  { free mask }
  FreeMem(Mask, Visual.Width * Visual.Height);
end;

procedure TvgGlowEffect.SetGlowColor(const Value: string);
begin
  if FGlowColor <> vgStrToColor(Value) then
  begin
    FGlowColor := vgStrToColor(Value);
    UpdateParentEffects;
  end;
end;

procedure TvgGlowEffect.SetOpacity(const Value: single);
begin
  if FOpacity <> Value then
  begin
    FOpacity := Value;
    if FOpacity < 0 then FOpacity := 0;
    if FOpacity > 1 then FOpacity := 1;
    UpdateParentEffects;
  end;
end;

procedure TvgGlowEffect.SetSoftness(const Value: single);
begin
  if FSoftness <> Value then
  begin
    FSoftness := Value;
    if FSoftness < 0 then FSoftness := 0;
    if FSoftness > 9 then FSoftness := 9;
    UpdateParentEffects;
  end;
end;

{ TvgInnerGlowEffect ===================================================================}

constructor TvgInnerGlowEffect.Create(AOwner: TComponent);
begin
  inherited;
//  DisablePaint := true;
  AfterPaint := true;
end;

destructor TvgInnerGlowEffect.Destroy;
begin
  inherited;
end;

function TvgInnerGlowEffect.GetOffset: TvgPoint;
begin
  Result := vgPoint(Trunc(FSoftness * 20), Trunc(FSoftness * 20));
end;

function TvgInnerGlowEffect.GetRect(const ARect: TvgRect): TvgRect;
begin
  Result := ARect;
  vgInflateRect(Result, Trunc(FSoftness * 20), Trunc(FSoftness * 20));
end;

procedure TvgInnerGlowEffect.ProcessEffect(Canvas: TvgCanvas; const Visual: TvgBitmap; const Data: single);
var
  Bits: PvgColorRecArray;
  Mask: PByteArray;
begin
  { invert }
  Visual.InvertAlpha;
  { create mask }
  Mask := Visual.CreateMask;
  { fill color }
  Visual.FillColor(vgOpacity(vgCorrectColor(FGlowColor), FOpacity));
  { stack blur version }
  Blur(Canvas, Visual, Trunc(5 + FSoftness * 10));
  { apply mask }
  Visual.ApplyMask(Mask);
  { free mask }
  FreeMem(Mask, Visual.Width * Visual.Height);
end;

{ TvgBevelEffect ==============================================================}

constructor TvgBevelEffect.Create(AOwner: TComponent);
begin
  inherited;
  DisablePaint := true;
  FDirection := 45;
  FSize := 10;
end;

destructor TvgBevelEffect.Destroy;
begin
  inherited;
end;

function TvgBevelEffect.GetOffset: TvgPoint;
begin
  Result := vgPoint(5, 5);
end;

function TvgBevelEffect.GetRect(const ARect: TvgRect): TvgRect;
begin
  Result := ARect;
  vgInflateRect(Result, 5, 5);
end;

function VectorAngleCosine(const V1, V2: TvgVector): Single;
var
  dot, len1, len2: single;
begin
  len1 := sqrt((v1.x*v1.x) + (v1.y*v1.y) + (v1.w*v1.w));
  len2 := sqrt((v2.x*v2.x) + (v2.y*v2.y) + (v2.w*v2.w));
  dot := (V1.V[0] * V2.V[0] + V1.V[1] * V2.V[1] + V1.V[2] * V2.V[2]);
  Result := len1 * len2;
  if Abs(Result) > 1e-40 then
    Result := dot / Result
  else
    Result := 1;
end;

procedure TvgBevelEffect.ProcessEffect(Canvas: TvgCanvas; const Visual: TvgBitmap; const Data: single);
var
  color: cardinal;
  DestBits, Bits: PvgColorRecArray;
  i, j: integer;
  a, h0, h1, h2, h3: single;
  alpha: byte;
  light, n, v, b: TvgVector;
  LightMap: TvgBitmap;
begin
  if FSize = 0 then Exit;

  DestBits := PvgColorRecArray(Visual.Scanline[0]);
  { create lightmap }
  LightMap := TvgBitmap.Create(Visual.Width, Visual.Height);
  Bits := PvgColorRecArray(LightMap.Scanline[0]);
  { copy bitmap }
  System.Move(DestBits^, Bits^, Visual.Width * Visual.Height * 4);
  { blur - make HeightMap }
  Blur(Canvas, LightMap, FSize);
  { calculate lighting }
  a := DegToRad(FDirection);
  light.x := cos(a);
  light.y := 0;
  light.w := sin(a);
  { make normalmap from hightmap }
  for j := Visual.Height - 2 downto 0 do
  begin
    for i := Visual.Width - 2 downto 0 do
    begin
      // only calc not transparent pixels
      {$ifdef FPC_BIG_ENDIAN}
      alpha := DestBits[(i) + ((j) * Visual.Width)].Color and $FF;
      {$else}
      alpha := DestBits[(i) + ((j) * Visual.Width)].A;
      {$endif}
      if alpha > 0 then
      begin
        {$ifdef FPC_BIG_ENDIAN}
        h0 := (Bits[i + ((j + 1) * Visual.Width)].Color and $FF) / $FF; //.height(x  ,z+1);
        h1 := (Bits[(i + 1) + ((j + 1) * Visual.Width)].Color and $FF) / $FF; //height(x+1,z+1);
        h2 := (Bits[(i + 1) + (j * Visual.Width)].Color and $FF) / $FF; //height(x+1,  z);
        h3 := (Bits[(i + 1) + ((j + 1) * Visual.Width)].Color and $FF) / $FF; //height(x  ,  z);
        {$else}
        h0 := (Bits[i + ((j + 1) * Visual.Width)].Color and $FF000000 shr 24) / $FF; //.height(x  ,z+1);
        h1 := (Bits[(i + 1) + ((j + 1) * Visual.Width)].Color and $FF000000 shr 24) / $FF; //height(x+1,z+1);
        h2 := (Bits[(i + 1) + (j * Visual.Width)].Color and $FF000000 shr 24) / $FF; //height(x+1,  z);
        h3 := (Bits[(i + 1) + ((j + 1) * Visual.Width)].Color and $FF000000 shr 24) / $FF; //height(x  ,  z);
        {$endif}

        v.x := 1.0;
        v.y := h2 - h3;
        v.w := 0;

        b.x := 0;
        b.y := h0 - h3;
        b.w := 1; // vector length

        // calc normal
        n.x := b.y*v.w - v.y*b.w;
        n.y := v.x*b.w - b.x*v.w;
        n.w := b.x*v.y - v.x*b.y;

        // normalize
        n.x := n.x / b.w;
        n.y := n.y / b.w;
        n.w := n.w / b.w;

        // calc light
        a := VectorAngleCosine(light, n) * FSize;

        // set value
        {$ifdef FPC_BIG_ENDIAN}
        color := DestBits[(i) + ((j) * Visual.Width)].Color;
        ReverseBytes(@color, 4);
        DestBits[(i) + ((j) * Visual.Width)].Color := vgPremultyAlpha(vgOpacity(vgChangeHSL(color, 0, 0, a * 0.4), alpha / $FF));
        ReverseBytes(@DestBits[(i) + ((j) * Visual.Width)], 4);
        {$else}
        DestBits[(i) + ((j) * Visual.Width)].Color := vgPremultyAlpha(vgOpacity(vgChangeHSL(DestBits[(i) + ((j) * Visual.Width)].Color, 0, 0, a * 0.4), alpha / $FF));
        {$endif}
      end;
    end;
  end;
  LightMap.Free;
end;

procedure TvgBevelEffect.SetDirection(const Value: single);
begin
  if FDirection <> Value then
  begin
    FDirection := Value;
    UpdateParentEffects;
  end;
end;

procedure TvgBevelEffect.SetSize(const Value: integer);
begin
  if FSize <> Value then
  begin
    FSize := Value;
    UpdateParentEffects;
  end;
end;

{ TvgReflectionEffect }

constructor TvgReflectionEffect.Create(AOwner: TComponent);
begin
  inherited;
//  DisablePaint := true;
  FOffset := 0;
  FLength := 0.5;
  FOpacity := 0.5;
end;

destructor TvgReflectionEffect.Destroy;
begin
  inherited;
end;

function TvgReflectionEffect.GetOffset: TvgPoint;
begin
  Result := vgPoint(2, 2);
end;

function TvgReflectionEffect.GetRect(const ARect: TvgRect): TvgRect;
begin
  Result := ARect;
  vgInflateRect(Result, 2, 2);
  vgOffsetRect(Result, 0, vgRectHeight(ARect) + FOffset);
end;

procedure TvgReflectionEffect.ProcessEffect(Canvas: TvgCanvas; const Visual: TvgBitmap; const Data: single);
var
  Bits: PvgColorRecArray;
  Line: array [0..2048] of TvgColor;
  c, a, i, j: integer;
  alpha: byte;
  color: cardinal;
begin
  Bits := PvgColorRecArray(Visual.Scanline[0]);
  c := Visual.Height - 1;
  a := round(c * FLength);
  { without Offset }
  Visual.FlipHorizontal;
  { set alpha }
  for j := 0 to c do
    if j > (c - a) then
      for i := 0 to Visual.Width - 1 do
      begin
        // only calc not transparent pixels
        {$ifdef FPC_BIG_ENDIAN}
        alpha := Bits[i + (Visual.Height - 1 - j) * Visual.Width].Color and $FF;
        {$else}
        alpha := Bits[i + (Visual.Height - 1 - j) * Visual.Width].A;
        {$endif}
        if alpha > 0 then
        begin
          {$ifdef FPC_BIG_ENDIAN}
          color := Bits[i + (Visual.Height - 1 - j) * Visual.Width].Color;
          ReverseBytes(@color, 4);
          TvgColorRec(color).A := Trunc(((j - (c - a)) / a) * FOpacity * alpha);
          Bits[i + (Visual.Height - 1 - j) * Visual.Width].Color := vgPremultyAlpha(color);
          ReverseBytes(@Bits[i + (Visual.Height - 1 - j) * Visual.Width], 4);
          {$else}
          Bits[i + (Visual.Height - 1 - j) * Visual.Width].A := Trunc(((j - (c - a)) / a) * FOpacity * alpha);
          Bits[i + (Visual.Height - 1 - j) * Visual.Width].Color := vgPremultyAlpha(Bits[i + (Visual.Height - 1 - j) * Visual.Width].Color);
          {$endif}
        end;
      end
    else
      vgFillLongword(@Bits[(Visual.Height - 1 - j) * Visual.Width], Visual.Width, $0); 
end;

procedure TvgReflectionEffect.SetLength(const Value: single);
begin
  if FLength <> Value then
  begin
    FLength := Value;
    if FLength < 0.1 then
      FLength := 0.1;
    if FLength > 1 then
      FLength := 1;
    UpdateParentEffects;
  end;
end;

procedure TvgReflectionEffect.SetOffset(const Value: integer);
begin
  if FOffset <> Value then
  begin
    FOffset := Value;
    UpdateParentEffects;
  end;
end;

procedure TvgReflectionEffect.SetOpacity(const Value: single);
begin
  if FOpacity <> Value then
  begin
    FOpacity := Value;
    UpdateParentEffects;
  end;
end;

initialization
  RegisterVGObjects('Effects', [TvgShadowEffect, TvgBlurEffect, TvgGlowEffect, TvgInnerGlowEffect,
    TvgBevelEffect, TvgReflectionEffect]);
end.
