unit unt_android;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, unt_command, platform_mapping, strutils;

function GetScreenshot(ADeviceId: string): string;
function GetBuildProp(ADeviceId: string): string;
function IsRootToolsInstalled(ADeviceId: string): boolean;
function GetRootToolsVersion(ADeviceId: string): integer;

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
begin
  // TODO: root tools installed?
  Result := True;
end;

function GetRootToolsVersion(ADeviceId: string): integer;
var
  SL: TStringList;
  Path: string;
  s: string;
  sVersion: string;
begin
  Result := 0;
  sVersion := '';
  Path := ExtractFilePath(ParamStr(0));
  SL := ExecuteCommandF(Format('adb -s %s shell dumpsys package com.rarnu.tools.root',
    [ADeviceId]), Path);
  for s in SL do
  begin
    if AnsiContainsStr(s, 'versionCode') then
    begin
      sVersion := s;
      Break;
    end;
  end;
  if sVersion <> '' then
  begin
    sVersion := Trim(sVersion);
    sVersion := LeftStr(sVersion, Pos(' ', sVersion));
    WriteLn(sVersion);
    sVersion := StringReplace(sVersion, 'versionCode=', '', [rfIgnoreCase, rfReplaceAll]);
    sVersion := Trim(sVersion);
    Result := StrToIntDef(sVersion, 0);
  end;
end;

end.
