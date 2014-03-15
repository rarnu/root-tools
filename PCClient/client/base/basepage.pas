unit basepage;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, baseform, Graphics, vg_scene, vg_controls, vg_objects,
  vg_ani, intf_notify, baseconfig, intf_paint, res_mapping, forms, dialogs;

type

  { TPageBase }

  TPageBase = class(TvgHudPanel, INotifyable)
  private
    FInitPosOnce: Boolean;
    FOnPaintOnce: TOnPaintOnce;
    FAni: TvgAniIndicator;
    FNoDevice: TvgText;
  protected
    FBase: TFormBase;
    FConfig: TConfigBase;
    procedure InitPage; virtual; abstract;
    procedure pagePaint(Sender: TObject; const ACanvas: TvgCanvas; const ARect: TvgRect);
    procedure pagePaintOnce(Sender: TObject; RealWidth: integer; RealHeight: integer); virtual;
  public
    constructor Create(AOwner: TComponent; ABase: TFormBase); virtual; reintroduce;
    destructor Destroy; override;
    procedure ThreadNotify(NotifyId: integer; AMap: TStringList); virtual; abstract;
  public
    property Config: TConfigBase read FConfig;
    property OnPaintOnce: TOnPaintOnce read FOnPaintOnce write FOnPaintOnce;
    procedure StartLoadingAni;
    procedure StopLoadingAni;
    procedure ShowNoDevice;
    procedure HideNoDevice;
  end;

implementation

{ TPageBase }

procedure TPageBase.pagePaint(Sender: TObject; const ACanvas: TvgCanvas;
  const ARect: TvgRect);
begin
  if not FInitPosOnce then
  begin
    FInitPosOnce := True;
    if Assigned(FOnPaintOnce) then
    begin
      FOnPaintOnce(Sender, trunc(Width), trunc(Height));
    end;
  end;
end;

procedure TPageBase.pagePaintOnce(Sender: TObject; RealWidth: integer;
  RealHeight: integer);
begin
  {$IFNDEF WINDOWS}
  FAni.Position.X := trunc((RealWidth - FAni.Width) / 2);
  FAni.Position.Y := trunc((RealHeight - FAni.Height) / 2);
  FNoDevice.Position.X := trunc((RealWidth - FNoDevice.Width) / 2);
  FNoDevice.Position.Y := trunc((RealHeight - FNoDevice.Height) / 2);
  {$ENDIF}

end;

constructor TPageBase.Create(AOwner: TComponent; ABase: TFormBase);
begin
  FInitPosOnce := False;
  inherited Create(AOwner);
  FBase := ABase;
  FConfig := TConfigBase.Create;
  OnPaint := @pagePaint;
  OnPaintOnce := @pagePaintOnce;

  FAni := TvgAniIndicator.Create(Self);
  FAni.Parent := Self;
  FAni.Width := 64;
  FAni.Height := 64;
  {$IFNDEF WINDOWS}
  FAni.Align := vaNone;
  {$ELSE}
  FAni.Align := vaCenter;
  {$ENDIF}
  FAni.Visible := False;

  FNoDevice := TvgText.Create(Self);
  FNoDevice.Parent := Self;
  {$IFNDEF WINDOWS}
  FNoDevice.Align := vaNone;
  {$ELSE}
  FNoDevice.Align := vaCenter;
  {$ENDIF}
  FNoDevice.Width := 200;
  FNoDevice.Height := 48;
  FNoDevice.Fill.SolidColor := vgColorFromVCL(clWhite);
  FNoDevice.Font.Size := 16;
  FNoDevice.HorzTextAlign := vgTextAlignCenter;
  FNoDevice.VertTextAlign := vgTextAlignCenter;
  FNoDevice.Text := Config.GetString(RES_NO_DEVICE);

  InitPage;
  FAni.BringToFront;
  FNoDevice.BringToFront;
end;

destructor TPageBase.Destroy;
begin
  FConfig.Free;
  inherited Destroy;
end;

procedure TPageBase.StartLoadingAni;
begin
  FAni.Enabled := True;
  FAni.Visible := True;
end;

procedure TPageBase.StopLoadingAni;
begin
  FAni.Enabled := False;
  FAni.Visible := False;
end;

procedure TPageBase.ShowNoDevice;
begin
  FNoDevice.Visible := True;
end;

procedure TPageBase.HideNoDevice;
begin
  FNoDevice.Visible := False;
end;

end.
