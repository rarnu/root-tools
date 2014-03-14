unit page_root_tools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, basepage, th_android, vg_scene, vg_controls,
  vg_objects, Graphics, baseform, th_network, unt_json, vg_listbox,
  res_mapping, item_update_log;

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
  protected
    procedure InitPage; override;
    procedure GetRootToolsVersionCode;
    procedure GetLastVersionFromServer;
    procedure LoadLastVersion(AJsonString: string);
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
  FAppVersion.Align := vaTop;
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
  for s in SL do
  begin
    item := TUpdateLogItem.Create(FUpdateLogList);
    item.Title := s;
    FUpdateLogList.AddObject(item);
  end;
end;

constructor TPageRootTools.Create(AOwner: TComponent; ABase: TFormBase);
begin
  inherited Create(AOwner, ABase);
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
      FAppVersion.Text := Config.GetString(RES_CURRENT_VERSION,
        [FRootToolsName, FRootToolsVersion]);
      GetLastVersionFromServer;
    end;
    2:
    begin
      LoadLastVersion(AMap.Values['update']);
      StopLoadingAni;
    end;
  end;
end;

end.
