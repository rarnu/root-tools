unit frm_skin;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, baseform,
  res_mapping, vg_scene, vg_controls, vg_objects;

type

  { TFormSkin }

  TFormSkin = class(TFormBase)
  private
    FPanelAlpha: TvgHudContainer;
    FPanelRed: TvgHudContainer;
    FPanelGreen: TvgHudContainer;
    FPanelBlue: TvgHudContainer;

    FTextAlpha: TvgText;
    FTextRed: TvgText;
    FTextGreen: TvgText;
    FTextBlue: TvgText;
    FTrackRed: TvgHudTrackBar;
    FTrackGreen: TvgHudTrackBar;
    FTrackBlue: TvgHudTrackBar;
    FTrackAlpha: TvgHudTrackBar;

    FPanelButton: TvgHudContainer;
    FBtnReset: TvgHudButton;
    FBtnOk: TvgHudButton;
  private
    procedure InitTrackBar;
  protected
    procedure InitForm; override;
    function SetWindowTitle: string; override;
    procedure trackChanged(Sender: TObject);
    procedure resetClicked(Sender: TObject);
  public
    procedure ThreadNotify(NotifyId: integer; AMap: TStringList); override;
  end;

// var
//   FormSkin: TFormSkin;

implementation

{$R *.lfm}

{ TFormSkin }

procedure TFormSkin.InitTrackBar;
var
  clr: string;
  sAlpha: string;
  sRed: string;
  sGreen: string;
  sBlue: string;
  iAlpha: integer;
  iRed: integer;
  iGreen: integer;
  iBlue: integer;
begin
  clr := vgColorToStr(Window.Fill.SolidColor);
  sAlpha := '$' + Copy(clr, 2, 2);
  sRed := '$' + Copy(clr, 4, 2);
  sGreen := '$' + Copy(clr, 6, 2);
  sBlue := '$' + Copy(clr, 8, 2);
  iAlpha := StrToInt(sAlpha);
  iRed := StrToInt(sRed);
  iGreen := StrToInt(sGreen);
  iBlue := StrToInt(sBlue);
  FTrackAlpha.Value := iAlpha;
  FTrackRed.Value := iRed;
  FTrackGreen.Value := iGreen;
  FTrackBlue.Value := iBlue;

end;

procedure TFormSkin.InitForm;
begin
  Width := 300;
  Height := {$IFDEF WINDOWS}190{$ELSE}160{$ENDIF};

  Window.Fill.SolidColor := vgStrToColor(Config.SkinColor);

  // alpha
  FPanelAlpha := TvgHudContainer.Create(Panel);
  FPanelAlpha.Parent := Panel;
  FPanelAlpha.Align := vaTop;
  FPanelAlpha.Height := 24;
  FPanelAlpha.Position.Y := 0;

  FTextAlpha := TvgText.Create(FPanelAlpha);
  FTextAlpha.Align := vaLeft;
  FTextAlpha.Text := 'Alpha';
  FTextAlpha.Width := 60;
  FTextAlpha.Parent := FPanelAlpha;
  FTextAlpha.Fill.SolidColor := vgColorFromVCL(clWhite);

  FTrackAlpha := TvgHudTrackBar.Create(FPanelAlpha);
  FTrackAlpha.Parent := FPanelAlpha;
  FTrackAlpha.Align := vaClient;
  FTrackAlpha.Tag := 0;
  FTrackAlpha.Max := 255;

  // red
  FPanelRed := TvgHudContainer.Create(Panel);
  FPanelRed.Parent := Panel;
  FPanelRed.Align := vaTop;
  FPanelRed.Height := 24;
  FPanelRed.Position.Y := 25;

  FTextRed := TvgText.Create(FPanelRed);
  FTextRed.Align := vaLeft;
  FTextRed.Text := 'Red';
  FTextRed.Width := 60;
  FTextRed.Parent := FPanelRed;
  FTextRed.Fill.SolidColor := vgColorFromVCL(clWhite);

  FTrackRed := TvgHudTrackBar.Create(FPanelRed);
  FTrackRed.Parent := FPanelRed;
  FTrackRed.Align := vaClient;
  FTrackRed.Tag := 1;
  FTrackRed.Max := 255;

  // green
  FPanelGreen := TvgHudContainer.Create(Panel);
  FPanelGreen.Parent := Panel;
  FPanelGreen.Align := vaTop;
  FPanelGreen.Height := 24;
  FPanelGreen.Position.Y := 49;

  FTextGreen := TvgText.Create(FPanelGreen);
  FTextGreen.Align := vaLeft;
  FTextGreen.Text := 'Green';
  FTextGreen.Width := 60;
  FTextGreen.Parent := FPanelGreen;
  FTextGreen.Fill.SolidColor := vgColorFromVCL(clWhite);

  FTrackGreen := TvgHudTrackBar.Create(FPanelGreen);
  FTrackGreen.Parent := FPanelGreen;
  FTrackGreen.Align := vaClient;
  FTrackGreen.Tag := 2;
  FTrackGreen.Max := 255;

  // blue
  FPanelBlue := TvgHudContainer.Create(Panel);
  FPanelBlue.Parent := Panel;
  FPanelBlue.Align := vaTop;
  FPanelBlue.Height := 24;
  FPanelBlue.Position.Y := 73;

  FTextBlue := TvgText.Create(FPanelBlue);
  FTextBlue.Align := vaLeft;
  FTextBlue.Text := 'Blue';
  FTextBlue.Width := 60;
  FTextBlue.Parent := FPanelBlue;
  FTextBlue.Fill.SolidColor := vgColorFromVCL(clWhite);

  FTrackBlue := TvgHudTrackBar.Create(FPanelBlue);
  FTrackBlue.Parent := FPanelBlue;
  FTrackBlue.Align := vaClient;
  FTrackBlue.Tag := 3;
  FTrackBlue.Max := 255;

  // button
  FPanelButton := TvgHudContainer.Create(Panel);
  FPanelButton.Parent := Panel;
  FPanelButton.Align := vaBottom;
  FPanelButton.Height := 24;
  FPanelButton.Padding.Bottom := 4;

  FBtnOk := TvgHudButton.Create(FPanelButton);
  FBtnOk.Parent := FPanelButton;
  FBtnOk.Align := vaRight;
  FBtnOk.Width := 60;
  FBtnOk.Padding.Right := 70;
  FBtnOk.Text := Config.GetString(RES_OK);
  FBtnOk.Default := True;
  FBtnOk.ModalResult := mrOk;

  FBtnReset := TvgHudButton.Create(FPanelButton);
  FBtnReset.Parent := FPanelButton;
  FBtnReset.Align := vaLeft;
  FBtnReset.Width := 60;
  FBtnReset.Padding.Left := 70;
  FBtnReset.Text := Config.GetString(RES_RESET);
  FBtnReset.OnClick := @resetClicked;

  FTrackAlpha.OnChange := @trackChanged;
  FTrackRed.OnChange := @trackChanged;
  FTrackGreen.OnChange := @trackChanged;
  FTrackBlue.OnChange := @trackChanged;

  InitTrackBar;
end;

function TFormSkin.SetWindowTitle: string;
begin
  Result := Config.GetString(RES_SKIN);
end;

procedure TFormSkin.trackChanged(Sender: TObject);
var
  iAlpha: integer;
  iRed: integer;
  iGreen: integer;
  iBlue: integer;
  clr: TvgColor;
begin
  iAlpha := trunc(FTrackAlpha.Value);
  iRed := trunc(FTrackRed.Value);
  iGreen := trunc(FTrackGreen.Value);
  iBlue := trunc(FTrackBlue.Value);
  clr := vgColor(iRed, iGreen, iBlue, iAlpha);
  Window.Fill.SolidColor := clr;

end;

procedure TFormSkin.resetClicked(Sender: TObject);
var
  iAlpha: integer;
  iRed: integer;
  iGreen: integer;
  iBlue: integer;
begin
  iAlpha := StrToInt('$EA');
  iRed := StrToInt('$2F');
  iGreen := iRed;
  iBlue := iRed;
  FTrackAlpha.Value := iAlpha;
  FTrackRed.Value := iRed;
  FTrackGreen.Value := iGreen;
  FTrackBlue.Value := iBlue;

end;

procedure TFormSkin.ThreadNotify(NotifyId: integer; AMap: TStringList);
begin

end;

end.

