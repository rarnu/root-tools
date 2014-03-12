unit vg_ani;

{$I vg_define.inc}

interface

uses Classes, SysUtils, vg_scene;

type

  TvgColorAnimation = class(TvgAnimation)
  private
    FStartColor: TvgColor;
    FStopColor: TvgColor;
    FPath, FPropertyName: AnsiString;
    FInstance: TObject;
    FStartFromCurrent: boolean;
    function GetStartColor: string;
    function GetStopColor: string;
    procedure SetStartColor(const Value: string);
    procedure SetStopColor(const Value: string);
  protected
    procedure ProcessAnimation; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Start; override;
  published
    property StartValue: string read GetStartColor write SetStartColor;
    property StartFromCurrent: boolean read FStartFromCurrent write FStartFromCurrent default false;
    property StopValue: string read GetStopColor write SetStopColor;
    property PropertyName: AnsiString read FPropertyName write FPropertyName;
  end;

  TvgGradientAnimation = class(TvgAnimation)
  private
    FStartGradient: TvgGradient;
    FStopGradient: TvgGradient;
    FPath, FPropertyName: AnsiString;
    FInstance: TObject;
    FStartFromCurrent: boolean;
    procedure SetStartGradient(const Value: TvgGradient);
    procedure SetStopGradient(const Value: TvgGradient);
  protected
    procedure ProcessAnimation; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Start; override;
  published
    property StartValue: TvgGradient read FStartGradient write SetStartGradient;
    property StartFromCurrent: boolean read FStartFromCurrent write FStartFromCurrent default false;
    property StopValue: TvgGradient read FStopGradient write SetStopGradient;
    property PropertyName: AnsiString read FPropertyName write FPropertyName;
  end;

  TvgFloatAnimation = class(TvgAnimation)
  private
    FStartFloat: single;
    FStopFloat: single;
    FPath, FPropertyName: AnsiString;
    FInstance: TObject;
    FStartFromCurrent: boolean;
  protected
    procedure ProcessAnimation; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Start; override;
    procedure Stop; override;
  published
    property StartValue: single read FStartFloat write FStartFloat stored true;
    property StartFromCurrent: boolean read FStartFromCurrent write FStartFromCurrent default false;
    property StopValue: single read FStopFloat write FStopFloat stored true;
    property PropertyName: AnsiString read FPropertyName write FPropertyName;
  end;

  TvgRectAnimation = class(TvgAnimation)
  private
    FStartRect: TvgBounds;
    FCurrent: TvgBounds;
    FStopRect: TvgBounds;
    FPath, FPropertyName: AnsiString;
    FInstance: TObject;
    FStartFromCurrent: boolean;
  protected
    procedure ProcessAnimation; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Start; override;
  published
    property StartValue: TvgBounds read FStartRect write FStartRect;
    property StartFromCurrent: boolean read FStartFromCurrent write FStartFromCurrent default false;
    property StopValue: TvgBounds read FStopRect write FStopRect;
    property PropertyName: AnsiString read FPropertyName write FPropertyName;
  end;

  TvgBitmapAnimation = class(TvgAnimation)
  private
    FPropertyName: AnsiString;
    FInstance: TObject;
    FStartBitmap: TvgBitmap;
    FStopBitmap: TvgBitmap;
    FCurrent: TvgBitmap;
  protected
    procedure ProcessAnimation; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property StartValue: TvgBitmap read FStartBitmap write FStartBitmap;
    property StopValue: TvgBitmap read FStopBitmap write FStopBitmap;
    property PropertyName: AnsiString read FPropertyName write FPropertyName;
  end;

  TvgBitmapListAnimation = class(TvgAnimation)
  private
    FPropertyName: AnsiString;
    FInstance: TObject;
    FCurrent: TvgBitmap;
    FAnimationCount: Integer;
    FAnimationBitmap: TvgBitmap;
    FLastAnimationStep: Integer;
  protected
    procedure ProcessAnimation; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property PropertyName: AnsiString read FPropertyName write FPropertyName;
    property AnimationBitmap: TvgBitmap read FAnimationBitmap write FAnimationBitmap;
    property AnimationCount: Integer read FAnimationCount write FAnimationCount;
  end;

{ Key Animations }

  TvgKey = class(TCollectionItem)
  private
    FKey: single;
    procedure SetKey(const Value: single);
  public
  published
    property Key: single read FKey write SetKey;
  end;

  TvgKeys = class(TCollection)
  private
  public
    function FindKeys(const Time: single; var Key1, Key2: TvgKey): boolean;
  published
  end;

  TvgColorKey = class(TvgKey)
  private
    FValue: string;
  public
  published
    property Value: string read FValue write FValue;
  end;

  TvgColorKeyAnimation = class(TvgAnimation)
  private
    FPropertyName: AnsiString;
    FInstance: TObject;
    FKeys: TvgKeys;
    FPath: AnsiString;
    FStartFromCurrent: boolean;
  protected
    procedure ProcessAnimation; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Start; override;
  published
    property PropertyName: AnsiString read FPropertyName write FPropertyName;
    property Keys: TvgKeys read FKeys write FKeys;
    property StartFromCurrent: boolean read FStartFromCurrent write FStartFromCurrent;
  end;

  TvgFloatKey = class(TvgKey)
  private
    FValue: single;
  public
  published
    property Value: single read FValue write FValue;
  end;

  TvgFloatKeyAnimation = class(TvgAnimation)
  private
    FPropertyName: AnsiString;
    FInstance: TObject;
    FKeys: TvgKeys;
    FPath: AnsiString;
    FStartFromCurrent: boolean;
  protected
    procedure ProcessAnimation; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Start; override;
  published
    property PropertyName: AnsiString read FPropertyName write FPropertyName;
    property Keys: TvgKeys read FKeys write FKeys;
    property StartFromCurrent: boolean read FStartFromCurrent write FStartFromCurrent;
  end;

{ Path Animation }

  TvgPathAnimation = class(TvgAnimation)
  private
    FPath: TvgPathData;
    FPolygon: TvgPolygon;
    FObj: TvgVisualObject;
    FStart: TvgPoint;
    FRotate: boolean;
    FSpline: TvgSpline;
    procedure SetPath(const Value: TvgPathData);
  protected
    procedure ProcessAnimation; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Start; override;
  published
    property Path: TvgPathData read FPath write SetPath;
    property Rotate: boolean read FRotate write FRotate default false;
  end;

{ Switch Animations }

  TvgPathSwitcher = class(TvgAnimation)
  private
    FPath, FPropertyName: AnsiString;
    FInstance: TObject;
    FPathTrue: string;
    FPathFalse: string;
    procedure SetPathFalse(const Value: string);
    procedure SetPathTrue(const Value: string);
  protected
    procedure ProcessAnimation; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Start; override;
  published
    property PathTrue: string read FPathTrue write SetPathTrue;
    property PathFalse: string read FPathFalse write SetPathFalse;
    property PropertyName: AnsiString read FPropertyName write FPropertyName;
  end;

implementation {===============================================================}

uses vg_objects, math, typinfo;

{ TvgAnimation ===================================================================}

constructor TvgColorAnimation.Create(AOwner: TComponent);
begin
  inherited;
  Duration := 0.2;
  FStartColor := $FFFFFFFF;
  FStartColor := $FFFFFFFF;
end;

destructor TvgColorAnimation.Destroy;
begin
  inherited;
end;

procedure TvgColorAnimation.Start;
var
  Persistent: string;
begin
  if (Parent <> nil) and (FPropertyName <> '') then
  begin
    if FInstance = nil then
    begin
      FInstance := Parent;
      FPath := FPropertyName;
      while Pos('.', FPath) > 0 do
      begin
        Persistent := vgGetToken(FPath, '.');
        if GetPropInfo(FInstance.ClassInfo, Persistent, [tkClass]) <> nil then
          FInstance := GetObjectProp(FInstance, Persistent);
      end;
    end;
  end;
  if (FInstance <> nil) and StartFromCurrent then
  begin
    { is string prop }
    if GetPropInfo(FInstance.ClassInfo, FPath, [tkString, tkLString, tkWString{$IFDEF FPC},tkAString{$ENDIF}{$IFDEF KS_COMPILER11_UP}, tkUString{$ENDIF}]) <> nil then
      StartValue := GetStrProp(FInstance, FPath);
    { is int prop }
    if GetPropInfo(FInstance.ClassInfo, FPath, [tkInteger]) <> nil then
      StartValue := vgColorToStr(GetOrdProp(FInstance, FPath));
  end;
  inherited;
end;

procedure TvgColorAnimation.ProcessAnimation;
begin
  if FInstance <> nil then
  begin
    { is string prop }
    if GetPropInfo(FInstance.ClassInfo, FPath, [tkString, tkLString, tkWString{$IFDEF FPC},tkAString{$ENDIF}{$IFDEF KS_COMPILER11_UP}, tkUString{$ENDIF}]) <> nil then
      SetStrProp(FInstance, FPath, vgColorToStr(vgInterpolateColor(FStartColor, FStopColor, NormalizedTime)));
    { is int prop }
    if GetPropInfo(FInstance.ClassInfo, FPath, [tkInteger]) <> nil then
      SetOrdProp(FInstance, FPath, vgInterpolateColor(FStartColor, FStopColor, NormalizedTime));
  end;
end;

function TvgColorAnimation.GetStartColor: string;
begin
  Result := vgColorToStr(FStartColor);
end;

function TvgColorAnimation.GetStopColor: string;
begin
  Result := vgColorToStr(FStopColor);
end;

procedure TvgColorAnimation.SetStartColor(const Value: string);
begin
  FStartColor := vgStrToColor(Value);
end;

procedure TvgColorAnimation.SetStopColor(const Value: string);
begin
  FStopColor := vgStrToColor(Value);
end;

{ TvgAnimation ===================================================================}

constructor TvgGradientAnimation.Create(AOwner: TComponent);
begin
  inherited;
  Duration := 0.2;
  FStartGradient := TvgGradient.Create;
  FStopGradient := TvgGradient.Create;
end;

destructor TvgGradientAnimation.Destroy;
begin
  FStartGradient.Free;
  FStopGradient.Free;
  inherited;
end;

procedure TvgGradientAnimation.Start;
var
  Persistent: string;
begin
  if (Parent <> nil) and (FPropertyName <> '') then
  begin
    if FInstance = nil then
    begin
      FInstance := Parent;
      FPath := FPropertyName;
      while Pos('.', FPath) > 0 do
      begin
        Persistent := vgGetToken(FPath, '.');
        if GetPropInfo(FInstance.ClassInfo, Persistent, [tkClass]) <> nil then
          FInstance := GetObjectProp(FInstance, Persistent);
      end;
    end;
  end;
  if (FInstance <> nil) and StartFromCurrent then
  begin
    if GetPropInfo(FInstance.ClassInfo, FPath, [tkClass]) <> nil then
    begin
      StartValue := TvgGradient(GetObjectProp(FInstance, FPath, TvgGradient));
    end;
  end;
  inherited;
end;

procedure TvgGradientAnimation.ProcessAnimation;
var
  i: integer;
  G: TvgGradient;
begin
  if FInstance <> nil then
  begin
    if GetPropInfo(FInstance.ClassInfo, FPath, [tkClass]) <> nil then
    begin
      with TvgGradient(GetObjectProp(FInstance, FPath, TvgGradient)) do
      begin
        for i := 0 to Points.Count - 1 do
        begin
          if (i < FStopGradient.Points.Count) or (i < FStartGradient.Points.Count) then
            Points[i].Color := vgColorToStr(vgInterpolateColor(vgStrToColor(FStartGradient.Points[i].Color),
              vgStrToColor(FStopGradient.Points[i].Color), NormalizedTime));
        end;
        Change;
      end;
    end;
  end;
end;

procedure TvgGradientAnimation.SetStartGradient(const Value: TvgGradient);
begin
  FStartGradient.Assign(Value);
end;

procedure TvgGradientAnimation.SetStopGradient(const Value: TvgGradient);
begin
  FStopGradient.Assign(Value);
end;

{ TvgAnimation ===================================================================}

constructor TvgFloatAnimation.Create(AOwner: TComponent);
begin
  inherited;
  Duration := 0.2;
  FStartFloat := 0;
  FStopFloat := 0;
end;

destructor TvgFloatAnimation.Destroy;
begin
  inherited;
end;

procedure TvgFloatAnimation.Start;
var
  Persistent: string;
begin
  if (Parent <> nil) and (FPropertyName <> '') then
  begin
    if FInstance = nil then
    begin
      FInstance := Parent;
      FPath := FPropertyName;
      while Pos('.', FPath) > 0 do
      begin
        Persistent := vgGetToken(FPath, '.');
        if GetPropInfo(FInstance.ClassInfo, Persistent, [tkClass]) <> nil then
          FInstance := GetObjectProp(FInstance, Persistent);
      end;
    end;
  end;
  if (FInstance <> nil) and StartFromCurrent then
  begin
    { is float prop }
    if GetPropInfo(FInstance.ClassInfo, FPath, [tkFloat]) <> nil then
    begin
      StartValue := GetFloatProp(FInstance, FPath);
    end;
  end;
  inherited;
end;

procedure TvgFloatAnimation.Stop;
begin
  inherited;
  FInstance := nil;
end;

procedure TvgFloatAnimation.ProcessAnimation;
begin
  if FInstance <> nil then
  begin
    { is float prop }
    if GetPropInfo(FInstance.ClassInfo, FPath, [tkFloat]) <> nil then
    begin
      SetFloatProp(FInstance, FPath, vgInterpolateSingle(FStartFloat, FStopFloat, NormalizedTime));
    end;
  end;
end;

{ TvgAnimation ===================================================================}

constructor TvgRectAnimation.Create(AOwner: TComponent);
begin
  inherited;
  Duration := 0.2;
  FStartRect := TvgBounds.Create(vgRect(0, 0, 0, 0));
  FStopRect := TvgBounds.Create(vgRect(0, 0, 0, 0));
  FCurrent := TvgBounds.Create(vgRect(0, 0, 0, 0));
end;

destructor TvgRectAnimation.Destroy;
begin
  FCurrent.Free;
  FStartRect.Free;
  FStopRect.Free;
  inherited;
end;

procedure TvgRectAnimation.Start;
var
  Persistent: string;
  Value: TObject;
begin
  if (Parent <> nil) and (FPropertyName <> '') then
  begin
    if FInstance = nil then
    begin
      FInstance := Parent;
      FPath := FPropertyName;
      while Pos('.', FPath) > 0 do
      begin
        Persistent := vgGetToken(FPath, '.');
        if GetPropInfo(FInstance.ClassInfo, Persistent, [tkClass]) <> nil then
          FInstance := GetObjectProp(FInstance, Persistent);
      end;
    end;
  end;
  if (FInstance <> nil) and StartFromCurrent then
  begin
    { is Rect prop }
    if GetPropInfo(FInstance.ClassInfo, FPropertyName, [tkClass]) <> nil then
    begin
      Value := GetObjectProp(FInstance, FPropertyName);
      if (Value <> nil) and (Value is TPersistent) then
        FStartRect.Assign(TPersistent(Value));
    end;
  end;
  inherited;
end;

procedure TvgRectAnimation.ProcessAnimation;
var
  Value: TObject;
begin
  if FInstance <> nil then
  begin
    { calc value }
    FCurrent.Left := vgInterpolateSingle(FStartRect.Left, FStopRect.Left, NormalizedTime);
    FCurrent.Top := vgInterpolateSingle(FStartRect.Top, FStopRect.Top, NormalizedTime);
    FCurrent.Right := vgInterpolateSingle(FStartRect.Right, FStopRect.Right, NormalizedTime);
    FCurrent.Bottom := vgInterpolateSingle(FStartRect.Bottom, FStopRect.Bottom, NormalizedTime);

    { is Rect prop }
    if GetPropInfo(FInstance.ClassInfo, FPath, [tkClass]) <> nil then
    begin
      Value := GetObjectProp(FInstance, FPath);
      if (Value <> nil) and (Value is TPersistent) then
        TPersistent(Value).Assign(FCurrent);
    end;
  end;
end;

{ TvgAnimation ===================================================================}

constructor TvgBitmapAnimation.Create(AOwner: TComponent);
begin
  inherited;
  Duration := 0.2;
  FStartBitmap := TvgBitmap.Create(1, 1);
  FStopBitmap := TvgBitmap.Create(1, 1);
  FCurrent := TvgBitmap.Create(1, 1);
end;

destructor TvgBitmapAnimation.Destroy;
begin
  FCurrent.Free;
  FStartBitmap.Free;
  FStopBitmap.Free;
  inherited;
end;

procedure TvgBitmapAnimation.ProcessAnimation;
var
  Persistent, Path: string;
  Value: TObject;
begin
  if (Parent <> nil) and (FPropertyName <> '')  then
  begin
    if FInstance = nil then
    begin
      FInstance := Parent;
      Path := FPropertyName;
      while Pos('.', FPropertyName) > 0 do
      begin
        Persistent := vgGetToken(FPropertyName, '.');
        if GetPropInfo(FInstance.ClassInfo, Persistent, [tkClass]) <> nil then
          FInstance := GetObjectProp(FInstance, Persistent);
      end;
    end;

    if FInstance <> nil then
    begin
      { is Bitmap prop }
      if GetPropInfo(FInstance.ClassInfo, FPropertyName, [tkClass]) <> nil then
      begin
        { calc new value }
        Value := GetObjectProp(FInstance, FPropertyName);
        if (Value <> nil) and (Value is TPersistent) then
        begin
          if Inverse then
          begin
            { assign to start }
            FCurrent.Assign(FStopBitmap);
            { draw final with alpha }
            if FCurrent.Canvas.BeginScene then
            begin
              FCurrent.Canvas.Clear(0);
              FCurrent.Canvas.DrawBitmap(FStartBitmap, vgRect(0, 0, FCurrent.Width, FCurrent.Height),
                vgRect(0, 0, FStartBitmap.Width, FStartBitmap.Height), 1 - NormalizedTime);
              FCurrent.Canvas.EndScene;
            end;
          end
          else
          begin
            { assign to start }
            FCurrent.Assign(FStartBitmap);
            { draw final with alpha }
            if FCurrent.Canvas.BeginScene then
            begin
              FCurrent.Canvas.Clear(0);
              FCurrent.Canvas.DrawBitmap(FStopBitmap, vgRect(0, 0, FCurrent.Width, FCurrent.Height),
                vgRect(0, 0, FStopBitmap.Width, FStopBitmap.Height), NormalizedTime);
              FCurrent.Canvas.EndScene;
            end;
          end;
          { assign }
          TPersistent(Value).Assign(FCurrent);
        end;
      end;
    end;
  end;
end;

{ TvgBitmapListAnimation }

constructor TvgBitmapListAnimation.Create(AOwner: TComponent);
begin
  inherited;
  Duration := 0.2;
  FCurrent := TvgBitmap.Create(0, 0);
  FAnimationBitmap := TvgBitmap.Create(0, 0);
  FAnimationCount := 1;
  FLastAnimationStep := 0;
end;

destructor TvgBitmapListAnimation.Destroy;
begin
  FCurrent.Free;
  FAnimationBitmap.Free;
  inherited;
end;

procedure TvgBitmapListAnimation.ProcessAnimation;
var
  Persistent, Path: string;
  Value: TObject;
  LeftPos, CurrentIndex: Integer;
  NowValue: Single;
begin
  if (Parent <> nil) and (FPropertyName <> '')  then
  begin
    if FInstance = nil then
    begin
      FInstance := Parent;
      Path := FPropertyName;
      while Pos('.', FPropertyName) > 0 do
      begin
        Persistent := vgGetToken(FPropertyName, '.');
        if GetPropInfo(FInstance.ClassInfo, Persistent, [tkClass]) <> nil then
          FInstance := GetObjectProp(FInstance, Persistent);
      end;
    end;

    if FInstance <> nil then
    begin
      { is Bitmap prop }
      if GetPropInfo(FInstance.ClassInfo, FPropertyName, [tkClass]) <> nil then
      begin
        { calc new value }
        Value := GetObjectProp(FInstance, FPropertyName);
        if (Value <> nil) and (Value is TvgBitmap) then
        begin
          NowValue := vgInterpolateSingle(0, FAnimationCount - 1, NormalizedTime);

          FCurrent.SetSize(TvgBitmap(Value).Height, TvgBitmap(Value).Width);

          CurrentIndex := Trunc(NowValue);

          LeftPos := CurrentIndex * (FAnimationBitmap.Width div FAnimationCount) + CurrentIndex;

          if FCurrent.Canvas.BeginScene then
          begin
            FCurrent.Canvas.Clear(0);
            FCurrent.Canvas.DrawBitmap(FAnimationBitmap,
              vgRect(LeftPos, 0, LeftPos + FCurrent.Width, FCurrent.Height),
              vgRect(0, 0, FAnimationBitmap.Width div FAnimationCount, FAnimationBitmap.Height), 1);
            FCurrent.Canvas.EndScene;
          end;

          TPersistent(Value).Assign(FCurrent);
        end;
      end;
    end;
  end;
end;

{ Key Animation ===============================================================}

{ TvgKey }

procedure TvgKey.SetKey(const Value: single);
begin
  FKey := Value;
  if FKey < 0 then FKey := 0;
  if FKey > 1 then FKey := 1;
end;

{ TvgKeys }

function TvgKeys.FindKeys(const Time: single; var Key1, Key2: TvgKey): boolean;
var
  i: integer;
begin
  Result := false;
  if Count < 2 then Exit;
  for i := 0 to Count - 2 do
    if ((Time >= TvgKey(Items[i]).Key) and (Time <= TvgKey(Items[i + 1]).Key)) then
    begin
      Result := true;
      Key1 := TvgKey(Items[i]);
      Key2 := TvgKey(Items[i + 1]);
      Exit;
    end;
end;

{ TvgColorKeyAnimation ========================================================}

constructor TvgColorKeyAnimation.Create(AOwner: TComponent);
begin
  inherited;
  FKeys := TvgKeys.Create(TvgColorKey);
end;

destructor TvgColorKeyAnimation.Destroy;
begin
  FKeys.Free;
  inherited;
end;

procedure TvgColorKeyAnimation.Start;
var
  Persistent: string;
begin
  if (Parent <> nil) and (FPropertyName <> '') then
  begin
    if FInstance = nil then
    begin
      FInstance := Parent;
      FPath := FPropertyName;
      while Pos('.', FPath) > 0 do
      begin
        Persistent := vgGetToken(FPath, '.');
        if GetPropInfo(FInstance.ClassInfo, Persistent, [tkClass]) <> nil then
          FInstance := GetObjectProp(FInstance, Persistent);
      end;
    end;
  end;
  if (FInstance <> nil) and StartFromCurrent then
  begin
    if Keys.Count > 0 then
    begin
      { is string prop }
      if GetPropInfo(FInstance.ClassInfo, FPath, [tkString, tkLString, tkWString{$IFDEF FPC},tkAString{$ENDIF}{$IFDEF KS_COMPILER11_UP}, tkUString{$ENDIF}]) <> nil then
        TvgColorKey(Keys.Items[0]).Value := GetStrProp(FInstance, FPath);
      { is int prop }
      if GetPropInfo(FInstance.ClassInfo, FPath, [tkInteger]) <> nil then
        TvgColorKey(Keys.Items[0]).Value := vgColorToStr(GetOrdProp(FInstance, FPath));
    end;
  end;
  inherited;
end;

procedure TvgColorKeyAnimation.ProcessAnimation;
var
  Key1, Key2: TvgKey;
begin
  if FInstance <> nil then
  begin
    if FKeys.FindKeys(NormalizedTime, Key1, Key2) then
    begin
      if (TvgFloatKey(Key2).Key - TvgFloatKey(Key1).Key) = 0 then Exit;
      { is string prop }
      if GetPropInfo(FInstance.ClassInfo, FPath, [tkString, tkLString, tkWString{$IFDEF FPC},tkAString{$ENDIF}{$IFDEF KS_COMPILER11_UP}, tkUString{$ENDIF}]) <> nil then
        SetStrProp(FInstance, FPath, vgColorToStr(vgInterpolateColor(vgStrToColor(TvgColorKey(Key1).Value), vgStrToColor(TvgColorKey(Key2).Value), (NormalizedTime - TvgFloatKey(Key1).Key) / (TvgFloatKey(Key2).Key - TvgFloatKey(Key1).Key))));
      { is int prop }
      if GetPropInfo(FInstance.ClassInfo, FPath, [tkInteger]) <> nil then
        SetOrdProp(FInstance, FPath, vgInterpolateColor(vgStrToColor(TvgColorKey(Key1).Value), vgStrToColor(TvgColorKey(Key2).Value), (NormalizedTime - TvgFloatKey(Key1).Key) / (TvgFloatKey(Key2).Key - TvgFloatKey(Key1).Key)));
    end;
  end;
end;

{ TvgFloatKeyAnimation ========================================================}

constructor TvgFloatKeyAnimation.Create(AOwner: TComponent);
begin
  inherited;
  FKeys := TvgKeys.Create(TvgFloatKey);
end;

destructor TvgFloatKeyAnimation.Destroy;
begin
  FKeys.Free;
  inherited;
end;

procedure TvgFloatKeyAnimation.Start;
var
  Persistent: string;
begin
  if (Parent <> nil) and (FPropertyName <> '') then
  begin
    if FInstance = nil then
    begin
      FInstance := Parent;
      FPath := FPropertyName;
      while Pos('.', FPath) > 0 do
      begin
        Persistent := vgGetToken(FPath, '.');
        if GetPropInfo(FInstance.ClassInfo, Persistent, [tkClass]) <> nil then
          FInstance := GetObjectProp(FInstance, Persistent);
      end;
    end;
  end;
  if (FInstance <> nil) and StartFromCurrent then
  begin
    if Keys.Count > 0 then
    begin
      { is string prop }
      if GetPropInfo(FInstance.ClassInfo, FPath, [tkFloat]) <> nil then
        TvgFloatKey(Keys.Items[0]).Value := GetFloatProp(FInstance, FPath);
    end;
  end;
  inherited;
end;

procedure TvgFloatKeyAnimation.ProcessAnimation;
var
  Key1, Key2: TvgKey;
begin
  if FInstance <> nil then
  begin
    if FKeys.FindKeys(NormalizedTime, Key1, Key2) then
    begin
      if (TvgFloatKey(Key2).Key - TvgFloatKey(Key1).Key) = 0 then Exit;
      if GetPropInfo(FInstance.ClassInfo, FPath, [tkFloat]) <> nil then
        SetFloatProp(FInstance, FPath, vgInterpolateSingle(TvgFloatKey(Key1).Value, TvgFloatKey(Key2).Value, (NormalizedTime - TvgFloatKey(Key1).Key) / (TvgFloatKey(Key2).Key - TvgFloatKey(Key1).Key)));
    end;
  end;
end;

{ TvgPathAnimation }

constructor TvgPathAnimation.Create(AOwner: TComponent);
begin
  inherited;
  FPath := TvgPathData.Create;
end;

destructor TvgPathAnimation.Destroy;
begin
  if FSpline <> nil then FreeAndNil(FSpline);
  FPath.Free;
  inherited;
end;

procedure TvgPathAnimation.ProcessAnimation;
var
  OldP, P1: TvgPoint;
begin
  if (Length(FPolygon) > 0) and (FObj <> nil) then
  begin
    OldP := FObj.Position.Point;
    FSpline.SplineXY(NormalizedTime * Length(FPolygon), P1.X, P1.Y);
    FObj.Position.X := FStart.X + P1.X;
    FObj.Position.Y := FStart.Y + P1.Y;
    if FRotate and (NormalizedTime <> 0) and (NormalizedTime <> 1) and ((OldP.X <> FObj.Position.X) and (OldP.Y <> FObj.Position.Y)) then
    begin
      if Inverse then
      begin
        if vgVectorCrossProductZ(vgVector(FObj.Position.X - OldP.X, FObj.Position.Y - OldP.Y), vgVector(0, 1)) < 0 then
          FObj.RotateAngle := 180 + vgRadToDeg(ArcCos(vgVectorAngleCosine(vgVector(FObj.Position.X - OldP.X, FObj.Position.Y - OldP.Y), vgVector(0, 1))))
        else
          FObj.RotateAngle := 180 - vgRadToDeg(ArcCos(vgVectorAngleCosine(vgVector(FObj.Position.X - OldP.X, FObj.Position.Y - OldP.Y), vgVector(0, 1))))
      end
      else
      begin
        if vgVectorCrossProductZ(vgVector(FObj.Position.X - OldP.X, FObj.Position.Y - OldP.Y), vgVector(0, 1)) < 0 then
          FObj.RotateAngle := vgRadToDeg(ArcCos(vgVectorAngleCosine(vgVector(FObj.Position.X - OldP.X, FObj.Position.Y - OldP.Y), vgVector(0, 1))))
        else
          FObj.RotateAngle := -vgRadToDeg(ArcCos(vgVectorAngleCosine(vgVector(FObj.Position.X - OldP.X, FObj.Position.Y - OldP.Y), vgVector(0, 1))))
      end;
    end;
  end;
end;

procedure TvgPathAnimation.SetPath(const Value: TvgPathData);
begin
  FPath.Assign(Value);
end;

procedure TvgPathAnimation.Start;
var
  i: integer;
begin
  inherited;
  if FSpline <> nil then FreeAndNil(FSpline);
  SetLength(FPolygon, 0);
  if Assigned(Scene) and (Scene.GetCanvas <> nil) then
  begin
    FPath.FlattenToPolygon(FPolygon);
    if Length(FPolygon) > 1 then
      for i := 1 to High(FPolygon) do
        if (FPolygon[i].X = ClosePolygon.X) and (FPolygon[i].Y = ClosePolygon.Y) then
          FPolygon[i] := FPolygon[i - 1];
    FSpline := TvgSpline.Create(FPolygon);
  end;
  if (Parent <> nil) and (Parent.IsVisual) then
    FObj := Parent.Visual
  else
    FObj := nil;
  if FObj <> nil then
    FStart := FObj.Position.Point;
end;

{ TvgPathSwitcher =============================================================}

constructor TvgPathSwitcher.Create(AOwner: TComponent);
begin
  inherited;
  Duration := 0.001;
end;

destructor TvgPathSwitcher.Destroy;
begin
  inherited;
end;

procedure TvgPathSwitcher.Start;
begin
  inherited;
end;

procedure TvgPathSwitcher.ProcessAnimation;
begin
  inherited;
end;

procedure TvgPathSwitcher.SetPathFalse(const Value: string);
begin
  FPathFalse := Value;
end;

procedure TvgPathSwitcher.SetPathTrue(const Value: string);
begin
  FPathTrue := Value;
end;

initialization
  RegisterVGObjects('Animations', [TvgColorAnimation, TvgGradientAnimation, TvgFloatAnimation, TvgRectAnimation, TvgBitmapAnimation, TvgBitmapListAnimation,
    TvgColorKeyAnimation, TvgFloatKeyAnimation, TvgPathAnimation, TvgPathSwitcher]);
end.

