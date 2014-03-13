unit page_root_tools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, basepage, th_android, vg_scene, vg_controls, vg_objects;

type

  { TPageRootTools }

  TPageRootTools = class(TPageBase)
  private
    FDeviceId: string;
    FAppVersion: TvgText;
  protected
    procedure InitPage; override;
    procedure GetRootToolsVersionCode;
  public
    procedure ThreadNotify(NotifyId: integer; AMap: TStringList); override;
  end;

implementation

{ TPageRootTools }

procedure TPageRootTools.InitPage;
begin
  FAppVersion := TvgText.Create(Self);
  FAppVersion.Parent := Self;
  FAppVersion.Align := vaTop;
  FAppVersion.Padding.Top := 4;
  FAppVersion.Padding.Left := 8;
  FAppVersion.Padding.Right := 8;
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
        GetRootToolsVersionCode;
      end
      else
      begin
        ShowNoDevice;
      end;
    end;
    1:
    begin
      WriteLn('Notify Version: ' + AMap.Values['version']);
      FAppVersion.Text := 'Current RootTools'' Version: ' + AMap.Values['version'];
      StopLoadingAni;
    end;
  end;
end;

end.

