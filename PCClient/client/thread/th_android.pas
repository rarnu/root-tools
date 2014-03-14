unit th_android;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, basethread, unt_android, strutils;

type

  { TAndroidThread }

  TAndroidThread = class(TThreadBase)
  private
    // 0: screenshot
    // 1: build.prop
    // 2: root tools version code
    // 3: install or update
    FDeviceId: string;
    FCmdType: integer;
    FCmdParam: array of string;
    // screenshot
    // build.prop
    FPath: string;
    // root tools version code
    FInstalled: Boolean;
    FVersionCode: string;
    FVersionName: string;
    // install or update
    FInstallMsg: string;
  protected
    procedure Execute; override;
    function MakeNotifyMap: TStringList; override;
  public
    procedure SetDeviceId(ADevice: string);
    procedure SetCommand(AType: integer; AParams: array of string);
  end;

implementation

{ TAndroidThread }

procedure TAndroidThread.Execute;
begin
  case FCmdType of
    0:
    begin
      FPath := GetScreenshot(FDeviceId);
    end;
    1:
    begin
      FPath := GetBuildProp(FDeviceId);
    end;
    2:
    begin
      FVersionCode := '';
      FVersionName := '';
      FInstalled := IsRootToolsInstalled(FDeviceId);
      if FInstalled then
      begin
        GetRootToolsVersion(FDeviceId, FVersionCode, FVersionName);
      end;
    end;
    3:
    begin
      FInstallMsg := InstallOrUpdateRootTools(FDeviceId, FCmdParam[0]);
    end;
  end;
end;

function TAndroidThread.MakeNotifyMap: TStringList;
begin
  Result := TStringList.Create;
  case FCmdType of
    0, 1:
    begin
      Result.Add('path=' + FPath);
    end;
    2:
    begin
      Result.Add('version=' + FVersionCode);
      Result.Add('name=' + FVersionName);
    end;
    3:
    begin
      Result.Add('install=' + FInstallMsg);
    end;
  end;
end;

procedure TAndroidThread.SetDeviceId(ADevice: string);
begin
  FDeviceId := ADevice;
end;

procedure TAndroidThread.SetCommand(AType: integer; AParams: array of string);
var
  i: integer;
begin
  FCmdType := AType;
  SetLength(FCmdParam, Length(AParams));
  for i := 0 to Length(AParams) - 1 do
  begin
    FCmdParam[i] := AParams[i];
  end;
end;

end.

