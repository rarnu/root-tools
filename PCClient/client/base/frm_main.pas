unit frm_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, baseform,
  vg_scene, vg_controls, vg_objects, res_mapping, vg_listbox, vg_ani,
  page_main, item_main, page_blank, unt_env, th_device_id, th_usb, frm_skin,
  frm_blank;

type

  { TFormMain }

  TFormMain = class(TFormBase)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FCurrentDevice: string;
  private
    FThreadDeviceId: TDeviceIdThread;
    FThreadUsb: TUsbThread;
  private
    FBtnSkin: TvgHudButton;
    FHead: TvgHudPanel;
    FUserLabel: TvgHudLabel;
    FListView: TvgHudListBox;
    FSubRoot: TvgHudContainer;
    FContainer: TvgHudContainer;
    FTailContainer: TvgHudPanel;
    FBtnInstall: TvgHudButton;
    FPages: array[0..9] of TvgHudPanel;
  private
    procedure mainListSelected(Sender: TObject);
    procedure skinClicked(Sender: TObject);
  protected
    function SetWindowTitle: string; override;
    procedure InitForm; override;
    procedure InitPages;
    procedure InitMainListItem;
  public
    procedure ThreadNotify(NotifyId: integer; AMap: TStringList); override;
  end;

var
  FormMain: TFormMain;

implementation

{$R *.lfm}

{ TFormMain }

procedure TFormMain.FormCreate(Sender: TObject);
begin
  inherited;
  InitMainListItem;
  FListView.ItemIndex := 0;
  mainListSelected(FListView);
  InitEnv(Config.SuPassword);
  FThreadDeviceId := TDeviceIdThread.Create(0, Self, True);
  FThreadUsb := TUsbThread.Create(1, Self);
  FThreadDeviceId.Start;

  Window.Fill.SolidColor := vgStrToColor(Config.SkinColor);

end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  FThreadUsb.Stop;
  Config.Save;
  inherited;
end;

procedure TFormMain.mainListSelected(Sender: TObject);
var
  i: integer;
  ATag: integer;
begin
  ATag := FListView.Selected.Tag;
  for i := 0 to Length(FPages) - 1 do
  begin
    FPages[i].Visible := ATag = i;
  end;
end;

procedure TFormMain.skinClicked(Sender: TObject);
begin
  try
    with TFormSkin.Create(nil) do
    begin
      if ShowModal = mrOk then
      begin
        Self.Config.SkinColor:= vgColorToStr(Window.Fill.SolidColor);
        Self.Window.Fill.SolidColor:= Window.Fill.SolidColor;
        Self.Config.Save;
      end;
      Free;
    end;

  except
  end;
end;

function TFormMain.SetWindowTitle: string;
begin
  Result := Config.GetString(RES_TITLE);
end;

procedure TFormMain.InitForm;
begin

  FBtnSkin := TvgHudButton.Create(Window);
  FBtnSkin.Parent := Window;
  FBtnSkin.Text := Config.GetString(RES_SKIN);
  FBtnSkin.Width := 60;
  FBtnSkin.Height := 16;
  FBtnSkin.Align := vaNone;
  FBtnSkin.Position.Y := 16;
  FBtnSkin.Position.X := Width - FBtnSkin.Width - 16;
  FBtnSkin.OnClick := @skinClicked;

  FHead := TvgHudPanel.Create(Panel);
  FHead.Parent := Panel;
  FHead.Align := vaMostTop;
  FHead.Height := 24;

  FUserLabel := TvgHudLabel.Create(FHead);
  FUserLabel.Parent := FHead;
  FUserLabel.Height := 24;
  FUserLabel.Width := 300;
  FUserLabel.Align := vaLeft;
  FUserLabel.VertTextAlign := vgTextAlignCenter;
  FUserLabel.TextAlign := vgTextAlignNear;
  FUserLabel.Padding.Left := 8;

  FListView := TvgHudListBox.Create(Panel);
  FListView.Parent := Panel;
  FListView.Width := 200;
  FListView.Align := vaLeft;
  FSubRoot := TvgHudContainer.Create(Panel);
  FSubRoot.Parent := Panel;
  FSubRoot.Align := vaClient;

  FTailContainer := TvgHudPanel.Create(FSubRoot);
  FTailContainer.Parent := FSubRoot;
  FTailContainer.Align := vaBottom;
  FTailContainer.Height := 64;

  FContainer := TvgHudContainer.Create(FSubRoot);
  FContainer.Parent := FSubRoot;
  FContainer.Align := vaClient;

  FBtnInstall := TvgHudButton.Create(FTailContainer);
  FBtnInstall.Parent := FTailContainer;
  FBtnInstall.Align := vaRight;
  FBtnInstall.Padding.Right := 8;
  FBtnInstall.Padding.Top := 8;
  FBtnInstall.Padding.Bottom := 8;
  FBtnInstall.Width := 120;
  FBtnInstall.Text := Config.GetString(RES_INSTALL_ROOTTOOLS);

  FListView.OnClick := @mainListSelected;
  InitPages;
end;

procedure TFormMain.InitPages;
var
  i: integer;
begin
  FPages[0] := TPageMain.Create(FContainer, Self);
  FPages[1] := TPageBlank.Create(FContainer, Self);
  FPages[2] := TPageBlank.Create(FContainer, Self);
  FPages[3] := TPageBlank.Create(FContainer, Self);
  FPages[4] := TPageBlank.Create(FContainer, Self);
  FPages[5] := TPageBlank.Create(FContainer, Self);
  FPages[6] := TPageBlank.Create(FContainer, Self);
  FPages[7] := TPageBlank.Create(FContainer, Self);
  FPages[8] := TPageBlank.Create(FContainer, Self);
  FPages[9] := TPageBlank.Create(FContainer, Self);

  for i := 0 to Length(FPages) - 1 do
  begin
    FPages[i].Align := vaClient;
    FPages[i].Parent := FContainer;

  end;
end;

procedure TFormMain.InitMainListItem;
var
  i: integer;
  item: TMainItem;
begin
  // TMainItem
  for i := 0 to Length(RES_MAIN_ITEM) - 1 do
  begin
    item := TMainItem.Create(FListView);
    item.Title := Config.GetString(RES_MAIN_ITEM[i]);
    item.Tag := i;
    FListView.AddObject(item);
  end;
end;

procedure TFormMain.ThreadNotify(NotifyId: integer; AMap: TStringList);
begin
  case NotifyId of
    0:
    begin
      FCurrentDevice := AMap.Values['deviceid'];
      FThreadUsb.Start;
    end;
    1:
    begin
      FCurrentDevice := AMap.Values['deviceid'];
    end;
  end;
  if FCurrentDevice = '' then
  begin
    FUserLabel.Text := Config.GetString(RES_NO_DEVICE);
  end
  else
  begin
    FUserLabel.Text := Config.GetString(RES_MY_DEVICE, [FCurrentDevice]);
  end;
end;

end.
