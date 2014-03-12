unit vg_actions;

{$I vg_define.inc}

interface

uses
  {$IFNDEF NOVCL}
  ImgList, Graphics, ActnList, Forms,
  {$ENDIF}
  Classes, SysUtils, vg_scene;

type

  TvgImageList = class(TCustomImageList)
  private
    FImages: TList;
    function GetCount: integer;
    function GetBitmap(Index: integer): TvgBitmap;
    procedure UpdateList;
  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadData(Stream: TStream); {$IFDEF KS_COMPILER7_UP}override;{$ENDIF}
    procedure WriteData(Stream: TStream); {$IFDEF KS_COMPILER7_UP}override;{$ENDIF}
    procedure ReadImage(Stream: TStream);
    procedure WriteImage(Stream: TStream);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Add(Bitmap: TvgBitmap): integer;
    procedure Clear;
    property Images[Index: integer]: TvgBitmap read GetBitmap;
    property Count: integer read GetCount;
  published
    property Height default 32;
    property Width default 32;
  end;

implementation {===============================================================}

{ TvgImageList }

constructor TvgImageList.Create(AOwner: TComponent);
begin
  inherited;
  FImages := TList.Create;
  Width := 32;
  Height := 32;
end;

destructor TvgImageList.Destroy;
begin
  Clear;
  FImages.Free;
  inherited;
end;

procedure TvgImageList.Clear;
var
  i: integer;
begin
  inherited Clear;

  for i := 0 to FImages.Count - 1 do
    TvgBitmap(FImages[i]).Free;
  FImages.Clear;
end;

function TvgImageList.Add(Bitmap: TvgBitmap): integer;
var
  B: TvgBitmap;
  R: TvgRect;
begin
  B := TvgBitmap.Create(Width, Height);
  R := vgRect(0, 0, Bitmap.Width, Bitmap.Height);
  vgFitRect(R, vgRect(0, 0, Width, Height));
  B.Canvas.DrawBitmap(Bitmap, vgRect(0, 0, Bitmap.Width, Bitmap.Height), R, 1);

  Result := FImages.Add(B);
  UpdateList;
end;

procedure TvgImageList.ReadImage(Stream: TStream);
var
  i: integer;
  S, C: cardinal;
  M: TMemoryStream;
  B: TvgBitmap;
begin
  Stream.Read(C, 4);
  for i := 0 to C - 1 do
  begin
    M := TMemoryStream.Create;

    Stream.Read(S, 4);
    M.SetSize(S);
    Stream.Read(M.Memory^, S);

    B := TvgBitmap.CreateFromStream(M);
    FImages.Add(B);

    M.Free;
  end;
  UpdateList;
end;

procedure TvgImageList.WriteImage(Stream: TStream);
var
  i: integer;
  S: cardinal;
  M: TMemoryStream;
begin
  S := FImages.Count;
  Stream.Write(S, 4);
  for i := 0 to FImages.Count - 1 do
  begin
    M := TMemoryStream.Create;
    TvgBitmap(FImages[i]).SaveToStream(M);

    S := M.Size;
    Stream.Write(S, 4);
    Stream.Write(M.Memory^, M.Size);

    M.Free;
  end;
end;

procedure TvgImageList.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineBinaryProperty('Images', ReadImage, WriteImage, FImages.Count > 0);
end;

function TvgImageList.GetCount: integer;
begin
  Result := FImages.Count;
end;

function TvgImageList.GetBitmap(Index: integer): TvgBitmap;
begin
  if (Index >= 0) and (Index < Count) then
    Result := TvgBitmap(FImages[Index])
  else
    Result := nil;
end;

procedure TvgImageList.UpdateList;
var
  i: integer;
  B: TBitmap;
begin
  if not (csDesigning in ComponentState) then Exit;

  inherited Clear;

  for i := 0 to FImages.Count - 1 do
  begin
    if (Images[i].Width <> Width) or (Images[i].Height <> Height) then Continue;

    B := TBitmap.Create;
    B.Assign(Images[i]);
    inherited AddMasked(B, 0);
    B.Free;
  end;
end;

procedure TvgImageList.ReadData(Stream: TStream);
begin
end;

procedure TvgImageList.WriteData(Stream: TStream);
begin
end;

end.

