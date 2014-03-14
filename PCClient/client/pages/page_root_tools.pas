unit page_root_tools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, basepage, th_android, vg_scene, vg_controls,
  vg_objects, Graphics, baseform, th_network, unt_json, vg_listbox,
  res_mapping, item_update_log, unt_download, platform_mapping;

type

  { TRootToolsVersion }

  TRootToolsVersion = class
  private
    FVersionCode: string;
    FVersionDesc: string;
    FVersionFile: string;
    FVersionName: string;
    FVersionSize: string;
  public
    function ToString: string;
  public
    property VersionCode: string read FVersionCode write FVersionCode;
    property VersionName: string read FVersionName write FVersionName;
    property VersionFile: string read FVersionFile write FVersionFile;
    property VersionSize: string read FVersionSize write FVersionSize;
    property VersionDesc: string read FVersionDesc write FVersionDesc;
  end;

  { TPageRootTools }

  TPageRootTools = class(TPageBase)
  private
    FDeviceId: string;
    FRootToolsVersion: string;
    FRootToolsName: string;
    FLastVersion: TRootToolsVersion;
    FAppVersion: TvgText;
    FLeftPanel: TvgHudContainer;
    FRightPanel: TvgHudContainer;
    FVersionDesc: TvgText;
    FUpdateLogList: TvgHudListBox;
    FBtnUpdate: TvgHudButton;
    FSyncPanel: TvgHudContainer;
    FSyncFile: TvgText;
    FDownloadProgress: TvgProgressBar;
    FDownloadPercent: TvgText;
    FSyncServer: TDownloader;
    FInstalling: TvgAniIndicator;
    FBtnRefresh: TvgHudButton;
  protected
    procedure InitPage; override;
    procedure GetRootToolsVersionCode;
    procedure GetLastVersionFromServer;
    procedure LoadLastVersion(AJsonString: string);
    procedure SyncFile;
    procedure InstallOrUpdate;

    procedure downloadProgress(Sender: TObject; APercent: integer);
    procedure downloadError(Sender: TObject; AMsg: string);
    procedure downloadComplete(Sender: TObject);

    procedure updateClick(Sender: TObject);
    procedure refreshClicked(Sender: TObject);

    procedure pagePaintOnce(Sender: TObject; RealWidth: integer; RealHeight: integer);
      override;
  public
    constructor Create(AOwner: TComponent; ABase: TFormBase); override;
    destructor Destroy; override;
    procedure ThreadNotify(NotifyId: integer; AMap: TStringList); override;

  end;

implementation

{ TRootToolsVersion }

function TRootToolsVersion.ToString: string;
begin
  Result := Format('code:%s, name:%s, file:%s, size:%s, desc:%s',
    [FVersionCode, FVersionName, FVersionFile, FVersionSize, FVersionDesc]);
end;

{ TPageRootTools }

procedure TPageRootTools.InitPage;
begin

  FLeftPanel := TvgHudContainer.Create(Self);
  FLeftPanel.Parent := Self;
  FLeftPanel.Align := vaLeft;
  FLeftPanel.Width := 200;
  FLeftPanel.Padding.Left := 8;
  FLeftPanel.Padding.Right := 8;
  FLeftPanel.Padding.Top := 8;
  FLeftPanel.Padding.Bottom := 8;
  FLeftPanel.Visible := False;

  FRightPanel := TvgHudContainer.Create(Self);
  FRightPanel.Parent := Self;
  FRightPanel.Align := vaClient;
  FRightPanel.Padding.Left := 8;
  FRightPanel.Padding.Right := 8;
  FRightPanel.Padding.Top := 8;
  FRightPanel.Padding.Bottom := 8;
  FRightPanel.Visible := False;

  FAppVersion := TvgText.Create(FLeftPanel);
  FAppVersion.Parent := FLeftPanel;
  FAppVersion.Align := vaMostTop;
  FAppVersion.Height := 48;
  FAppVersion.Padding.Top := 4;
  FAppVersion.Padding.Left := 8;
  FAppVersion.Padding.Right := 8;
  FAppVersion.Font.Size := 16;
  FAppVersion.HorzTextAlign := vgTextAlignNear;
  FAppVersion.VertTextAlign := vgTextAlignCenter;
  FAppVersion.Fill.SolidColor := vgColorFromVCL(clWhite);

  FVersionDesc := TvgText.Create(FRightPanel);
  FVersionDesc.Parent := FRightPanel;
  FVersionDesc.Align := vaTop;
  FVersionDesc.Height := 48;
  FVersionDesc.Padding.Top := 4;
  FVersionDesc.Padding.Left := 8;
  FVersionDesc.Padding.Right := 8;
  FVersionDesc.Font.Size := 16;
  FVersionDesc.HorzTextAlign := vgTextAlignNear;
  FVersionDesc.VertTextAlign := vgTextAlignCenter;
  FVersionDesc.Fill.SolidColor := vgColorFromVCL(clYellow);

  FUpdateLogList := TvgHudListBox.Create(FRightPanel);
  FUpdateLogList.Parent := FRightPanel;
  FUpdateLogList.Align := vaClient;

  FBtnUpdate := TvgHudButton.Create(FLeftPanel);
  FBtnUpdate.Parent := FLeftPanel;
  FBtnUpdate.Height := 40;
  FBtnUpdate.Align := vaTop;
  FBtnUpdate.Padding.Left := 16;
  FBtnUpdate.Padding.Right := 16;
  FBtnUpdate.Padding.Top := 16;
  FBtnUpdate.Text := Config.GetString(RES_UPDATE);
  FBtnUpdate.Visible := False;
  FBtnUpdate.OnClick := @updateClick;

  FInstalling := TvgAniIndicator.Create(FBtnUpdate);
  FInstalling.Parent := FBtnUpdate;
  FInstalling.Height := 32;
  FInstalling.Width := 32;
  FInstalling.Align := vaCenter;
  FInstalling.Enabled := False;
  FInstalling.Visible := False;

  FSyncPanel := TvgHudContainer.Create(FLeftPanel);
  FSyncPanel.Parent := FLeftPanel;
  FSyncPanel.Height := 48;
  FSyncPanel.Align := vaCenter;

  FDownloadProgress := TvgProgressBar.Create(FSyncPanel);
  FDownloadProgress.Parent := FSyncPanel;
  FDownloadProgress.Height := 24;
  FDownloadProgress.Align := vaMostBottom;
  FDownloadProgress.Padding.Bottom := 8;
  FDownloadProgress.Padding.Left := 4;
  FDownloadProgress.Padding.Right := 4;
  FDownloadProgress.Visible := False;
  FDownloadProgress.Max := 100;
  FDownloadProgress.Min := 0;

  FDownloadPercent := TvgText.Create(FDownloadProgress);
  FDownloadPercent.Parent := FDownloadProgress;
  FDownloadPercent.Align := vaClient;
  FDownloadPercent.HorzTextAlign := vgTextAlignCenter;
  FDownloadPercent.VertTextAlign := vgTextAlignCenter;
  FDownloadPercent.Fill.SolidColor := vgColorFromVCL(clWhite);

  FSyncFile := TvgText.Create(FSyncPanel);
  FSyncFile.Parent := FSyncPanel;
  FSyncFile.Align := vaBottom;
  FSyncFile.Height := 24;
  FSyncFile.HorzTextAlign := vgTextAlignCenter;
  FSyncFile.VertTextAlign := vgTextAlignCenter;
  FSyncFile.Text := Config.GetString(RES_SYNC_FILE);
  FSyncFile.Fill.SolidColor := vgColorFromVCL(clWhite);
  FSyncFile.Visible := False;

  FBtnRefresh := TvgHudButton.Create(FLeftPanel);
  FBtnRefresh.Parent := FLeftPanel;
  FBtnRefresh.Align := vaMostBottom;
  FBtnRefresh.Text := Config.GetString(RES_REFRESH_DEVICE);
  FBtnRefresh.Padding.Left := 32;
  FBtnRefresh.Padding.Right := 32;
  FBtnRefresh.Padding.Bottom := 16;
  FBtnRefresh.OnClick := @refreshClicked;

end;

procedure TPageRootTools.GetRootToolsVersionCode;
begin
  with TAndroidThread.Create(1, Self) do
  begin
    SetDeviceId(FDeviceId);
    SetCommand(2, []);
    Start;
  end;
end;

procedure TPageRootTools.GetLastVersionFromServer;
const
  REQUEST = 'http://rarnu.7thgen.info/root_tools/check_update.php?version=0';
begin
  with TNetworkthread.Create(2, Self) do
  begin
    Start(REQUEST);
  end;
end;

procedure TPageRootTools.LoadLastVersion(AJsonString: string);
var
  Json: TlkJSONobject;
  item: TUpdateLogItem;
  SL: TStringList;
  s: string;
  iServerVersion: integer;
  iLocalVersion: integer;
begin
  FLastVersion := TRootToolsVersion.Create;
  Json := TlkJSONobject(TlkJSON.ParseText(AJsonString));
  FLastVersion.VersionCode := Json.getString('version_code');
  FLastVersion.VersionName := Json.getString('version_name');
  FLastVersion.VersionFile := Json.getString('file');
  FLastVersion.VersionSize := Json.getString('size');
  FLastVersion.VersionDesc := Json.getString('desc');
  WriteLn(FLastVersion.ToString);
  FVersionDesc.Text := Config.GetString(RES_LAST_VERSION,
    [FLastVersion.VersionName, FLastVersion.VersionCode]);
  SL := TStringList.Create;
  SL.Text := StringReplace(FLastVersion.VersionDesc, '<br>', #13#10,
    [rfIgnoreCase, rfReplaceAll]);
  FUpdateLogList.Clear;
  for s in SL do
  begin
    item := TUpdateLogItem.Create(FUpdateLogList);
    item.Title := s;
    FUpdateLogList.AddObject(item);
  end;
  iServerVersion := StrToIntDef(FLastVersion.VersionCode, 0);
  iLocalVersion := StrToIntDef(FRootToolsVersion, 0);
  FBtnUpdate.Visible := (iServerVersion > iLocalVersion);
  SyncFile;
end;

procedure TPageRootTools.SyncFile;
const
  DOWNLOAD_BASE = 'http://rarnu.7thgen.info/root_tools/download/';
var
  AFileName: string;
begin
  AFileName := ExtractFilePath(ParamStr(0)) + 'tmp' + SPL + FLastVersion.VersionFile;
  if FileExists(AFileName) then
  begin
    Exit;
  end;
  FSyncFile.Visible := True;
  FDownloadProgress.Visible := True;
  FSyncServer.StartDownload(DOWNLOAD_BASE + FLastVersion.VersionFile,
    AFileName);
end;

procedure TPageRootTools.InstallOrUpdate;
var
  AFileName: string;
begin
  AFileName := ExtractFilePath(ParamStr(0)) + 'tmp' + SPL + FLastVersion.VersionFile;
  if not FileExists(AFileName) then
  begin
    SyncFile;
    Exit;
  end;
  with TAndroidThread.Create(3, Self) do
  begin
    SetDeviceId(FDeviceId);
    SetCommand(3, [AFileName]);
    Start;
  end;
end;

procedure TPageRootTools.downloadProgress(Sender: TObject; APercent: integer);
begin
  FDownloadProgress.Value := APercent;
  FDownloadPercent.Text := Format('%d %%', [APercent]);
end;

procedure TPageRootTools.downloadError(Sender: TObject; AMsg: string);
begin
  WriteLn(AMsg);
end;

procedure TPageRootTools.downloadComplete(Sender: TObject);
begin
  FSyncFile.Visible := False;
  FDownloadProgress.Visible := False;
  FDownloadPercent.Text := '';
  FDownloadProgress.Value := 0;
end;

procedure TPageRootTools.updateClick(Sender: TObject);
begin
  FBtnUpdate.Enabled := False;
  FInstalling.Visible := True;
  FInstalling.Enabled := True;
  InstallOrUpdate;
end;

procedure TPageRootTools.refreshClicked(Sender: TObject);
begin
  GetRootToolsVersionCode;
end;

procedure TPageRootTools.pagePaintOnce(Sender: TObject; RealWidth: integer;
  RealHeight: integer);
begin
  inherited pagePaintOnce(Sender, RealWidth, RealHeight);
  FSyncPanel.Width := FLeftPanel.Width;
end;

constructor TPageRootTools.Create(AOwner: TComponent; ABase: TFormBase);
begin
  inherited Create(AOwner, ABase);
  FSyncServer := TDownloader.Create;
  FSyncServer.OnDownloadProgress := @downloadProgress;
  FSyncServer.OnDownloadError := @downloadError;
  FSyncServer.OnDownloadComplete := @downloadComplete;
end;

destructor TPageRootTools.Destroy;
begin
  inherited Destroy;
end;

procedure TPageRootTools.ThreadNotify(NotifyId: integer; AMap: TStringList);
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
      if FDeviceId <> '' then
      begin
        StartLoadingAni;
        HideNoDevice;
        FLeftPanel.Visible := True;
        FRightPanel.Visible := True;
        GetRootToolsVersionCode;
      end
      else
      begin
        FLeftPanel.Visible := False;
        FRightPanel.Visible := False;
        ShowNoDevice;
      end;
    end;
    1:
    begin
      FRootToolsVersion := AMap.Values['version'];
      FRootToolsName := AMap.Values['name'];
      if FRootToolsVersion = '' then
      begin
        FAppVersion.Text := Config.GetString(RES_NOT_INSTALLED);
      end
      else
      begin
        FAppVersion.Text := Config.GetString(RES_CURRENT_VERSION,
          [FRootToolsName, FRootToolsVersion]);
      end;
      GetLastVersionFromServer;
    end;
    2:
    begin
      LoadLastVersion(AMap.Values['update']);
      StopLoadingAni;
    end;
    3:
    begin
      FBtnUpdate.Enabled := True;
      FInstalling.Enabled := False;
      FInstalling.Visible := False;
      WriteLn(AMap.Values['install']);
      GetRootToolsVersionCode;
    end;
  end;
end;

end.
