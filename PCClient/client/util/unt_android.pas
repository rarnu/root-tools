unit unt_android;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, unt_command, platform_mapping, strutils;

function GetScreenshot(ADeviceId: string): string;
function GetBuildProp(ADeviceId: string): string;
function IsRootToolsInstalled(ADeviceId: string): boolean;
procedure GetRootToolsVersion(ADeviceId: string; out AVersionCode: string;
  out AVersionName: string);
function InstallOrUpdateRootTools(ADeviceId: string; AFileName: string): string;

implementation

function GetScreenshot(ADeviceId: string): string;
var
  Path: string;
  AFilePath: string;
begin
  Result := '';
  Path := ExtractFilePath(ParamStr(0)) + 'tmp' + SPL;
  ExecuteCommandP(Format('adb -s %s shell screencap -p /sdcard/screenshot.png',
    [ADeviceId]), Path);
  ExecuteCommandP(Format('adb -s %s pull /sdcard/screenshot.png .', [ADeviceId]), Path);
  AFilePath := Path + 'screenshot.png';
  if FileExists(AFilePath) then
  begin
    Result := AFilePath;
  end;
end;

function GetBuildProp(ADeviceId: string): string;
var
  Path: string;
  AFilePath: string;
begin
  Result := '';
  Path := ExtractFilePath(ParamStr(0)) + 'tmp' + SPL;
  ExecuteCommandP(Format('adb -s %s pull /system/build.prop .', [ADeviceId]), Path);
  AFilePath := Path + 'build.prop';
  if FileExists(AFilePath) then
  begin
    Result := AFilePath;
  end;
end;

function IsRootToolsInstalled(ADeviceId: string): boolean;
var
  SL: TStringList;
  Path: string;
begin
  Path := ExtractFilePath(ParamStr(0));
  SL := ExecuteCommandF(Format('adb -s %s shell dumpsys package com.rarnu.tools.root',
    [ADeviceId]), Path);
  Result := AnsiContainsStr(SL.Text, 'versionCode');
end;

procedure GetRootToolsVersion(ADeviceId: string; out AVersionCode: string;
  out AVersionName: string);
var
  SL: TStringList;
  Path: string;
  s: string;
  sVersion: string;
  sName: string;
begin
  sVersion := '';
  sName := '';
  Path := ExtractFilePath(ParamStr(0));
  SL := ExecuteCommandF(Format('adb -s %s shell dumpsys package com.rarnu.tools.root',
    [ADeviceId]), Path);
  for s in SL do
  begin
    if (AnsiContainsStr(s, 'versionCode')) and (sVersion = '') then
    begin
      sVersion := s;
    end;
    if (AnsiContainsStr(s, 'versionName')) and (sName = '') then
    begin
      sName := s;
    end;
  end;
  if sVersion <> '' then
  begin
    sVersion := Trim(sVersion);
    if Pos(' ', sVersion) > 0 then
    begin
      sVersion := LeftStr(sVersion, Pos(' ', sVersion));
    end;
    WriteLn(sVersion);
    sVersion := StringReplace(sVersion, 'versionCode=', '',
      [rfIgnoreCase, rfReplaceAll]);
    sVersion := Trim(sVersion);
    AVersionCode := sVersion;
  end;
  if sName <> '' then
  begin
    sName := Trim(sName);
    if Pos(' ', sName) > 0 then
    begin
      sName := LeftStr(sName, Pos(' ', sName));
    end;
    WriteLn(sName);
    sName := StringReplace(sName, 'versionName=', '', [rfIgnoreCase, rfReplaceAll]);
    sName := Trim(sName);
    AVersionName := sName;
  end;
end;

function InstallOrUpdateRootTools(ADeviceId: string; AFileName: string): string;
var
  SL: TStringList;
  Path: string;
begin
  Path := ExtractFilePath(ParamStr(0));
  SL := ExecuteCommandF(Format('adb -s %s install -r %s', [ADeviceId, AFileName]), Path);
  Result := SL.Text;
end;

end.
