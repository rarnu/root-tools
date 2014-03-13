unit page_device;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, basepage, vg_scene, vg_controls, vg_objects,
  vg_ani, baseform, th_android, Graphics, res_mapping, vg_listbox,
  item_device_build_prop, strutils;

type

  { TPageMain }

  { TPageDevice }

  TPageDevice = class(TPageBase)
  private
    FDeviceId: string;
    FScreenPath: string;
    FBuildPropPath: string;
    FLeftPanel: TvgHudContainer;
    FRightPanel: TvgHudContainer;
    FDeviceScreen: TvgImage;
    FMyDevice: TvgText;
    FBtnRefresh: TvgHudButton;
    FBuildPropList: TvgHudListBox;
  private
    procedure GetScreenshot;
    procedure GetBuildProp;
    procedure LoadScreenshot(AFileName: string);
    procedure LoadBuildProp(AFileName: string);
  protected
    procedure InitPage; override;
    procedure refreshClicked(Sender: TObject);
    procedure DoRefresh;
    procedure DoDeviceOffline;
  public
    procedure ThreadNotify(NotifyId: integer; AMap: TStringList); override;
  end;

implementation

{ TPageMain }

procedure TPageDevice.GetScreenshot;
begin
  with TAndroidThread.Create(1, Self) do
  begin
    SetDeviceId(FDeviceId);
    SetCommand(0, []);
    Start;

  end;
end;

procedure TPageDevice.GetBuildProp;
begin
  with TAndroidThread.Create(2, Self) do
  begin
    SetDeviceId(FDeviceId);
    SetCommand(1, []);
    Start;
  end;
end;

procedure TPageDevice.LoadScreenshot(AFileName: string);
var
  png: TPortableNetworkGraphic;
begin
  WriteLn('LoadScreenshot: ' + AFileName);
  if AFileName = '' then
  begin
    if FDeviceId = '' then
    begin
      FDeviceScreen.Bitmap.Clear;
      Exit;

    end;
  end;
  FDeviceScreen.Bitmap.LoadFromFile(AFileName);
  StopLoadingAni;
end;

procedure TPageDevice.LoadBuildProp(AFileName: string);
var
  item: TBuildPropItem;
  SL: TStringList;
  i: integer;
  AName: string;
  AValue: string;
begin
  // TODO: load buildprop
  FBuildPropList.Clear;

  if AFileName = '' then
  begin
    Exit;
  end;
  SL := TStringList.Create;
  SL.LoadFromFile(AFileName);
  for i := 0 to SL.Count - 1 do
  begin
    if (Trim(SL[i]) = '') or (AnsiStartsStr('#', SL[i])) or
      (not AnsiContainsStr(SL[i], '=')) then
    begin
      Continue;
    end;
    item := TBuildPropItem.Create(FBuildPropList);
    item.PropName := SL.Names[i];
    item.PropValue := SL.ValueFromIndex[i];
    FBuildPropList.AddObject(item);
  end;
end;

procedure TPageDevice.InitPage;
begin

  FLeftPanel := TvgHudContainer.Create(Self);
  FLeftPanel.Parent := Self;
  FLeftPanel.Width := 200;
  FLeftPanel.Align := vaLeft;
  FLeftPanel.Padding.Left := 8;
  FLeftPanel.Padding.Right := 8;
  FLeftPanel.Padding.Top := 8;
  FLeftPanel.Padding.Bottom := 8;

  FRightPanel := TvgHudContainer.Create(Self);
  FRightPanel.Parent := Self;
  FRightPanel.Align := vaClient;
  FRightPanel.Padding.Left := 8;
  FRightPanel.Padding.Right := 8;
  FRightPanel.Padding.Top := 8;
  FRightPanel.Padding.Bottom := 8;

  FDeviceScreen := TvgImage.Create(FLeftPanel);
  FDeviceScreen.Parent := FLeftPanel;
  FDeviceScreen.Width := 200;
  FDeviceScreen.Height := 320;
  FDeviceScreen.WrapMode := vgImageStretch;

  FMyDevice := TvgText.Create(FLeftPanel);
  FMyDevice.Parent := FLeftPanel;
  FMyDevice.Align := vaBottom;
  FMyDevice.Fill.SolidColor := vgColorFromVCL(clWhite);
  FMyDevice.HorzTextAlign := vgTextAlignCenter;
  FMyDevice.VertTextAlign := vgTextAlignCenter;
  FMyDevice.Text := Config.GetString(RES_MAIN_ITEM[0]);
  FMyDevice.Font.Size := 16;
  FMyDevice.AutoSize := True;
  FMyDevice.Visible := False;

  FBtnRefresh := TvgHudButton.Create(FLeftPanel);
  FBtnRefresh.Parent := FLeftPanel;
  FBtnRefresh.Align := vaMostBottom;
  FBtnRefresh.Visible := False;
  FBtnRefresh.Text := Config.GetString(RES_REFRESH_DEVICE);
  FBtnRefresh.Padding.Left := 32;
  FBtnRefresh.Padding.Right := 32;
  FBtnRefresh.OnClick := @refreshClicked;

  FBuildPropList := TvgHudListBox.Create(FRightPanel);
  FBuildPropList.Parent := FRightPanel;
  FBuildPropList.Align := vaClient;
  FBuildPropList.Visible := False;

end;

procedure TPageDevice.refreshClicked(Sender: TObject);
begin
  DoRefresh;
end;

procedure TPageDevice.DoRefresh;
begin
  StartLoadingAni;
  HideNoDevice;
  FMyDevice.Visible := True;
  FBtnRefresh.Visible := True;
  FBuildPropList.Visible := True;
  GetScreenshot;
  GetBuildProp;
end;

procedure TPageDevice.DoDeviceOffline;
begin
  ShowNoDevice;
  LoadScreenshot('');
  FMyDevice.Visible := False;
  FBtnRefresh.Visible := False;
  FBuildPropList.Visible := False;
end;

procedure TPageDevice.ThreadNotify(NotifyId: integer; AMap: TStringList);
var
  NewDeviceId: string;
begin
  case NotifyId of
    0:
    begin
      // from container
      NewDeviceId := AMap.Values['deviceid'];
      if NewDeviceId = FDeviceId then
      begin
        Exit;
      end;
      FDeviceId := NewDeviceId;
      if (FDeviceId <> '') then
      begin
        DoRefresh;
      end
      else
      begin
        DoDeviceOffline;
      end;
    end;
    1:
    begin
      FScreenPath := AMap.Values['path'];
      LoadScreenshot(FScreenPath);
    end;
    2:
    begin
      FBuildPropPath := AMap.Values['path'];
      LoadBuildProp(FBuildPropPath);
    end;
  end;
end;

end.
