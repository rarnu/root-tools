unit unt_env;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, unt_command, strutils;

function InitEnv(APassword: string): boolean;
function GetFirstDeviceId(AKill: boolean): string;

implementation

procedure MakeExpectScript(APassword: string);
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  SL.Add('#!/bin/sh');
  SL.Add('expect -c "');
  SL.Add('spawn sudo adb devices');
  SL.Add('expect {');
  SL.Add('  \"password\" { set timeout 500; send \"' + APassword + '\r\"; }');
  SL.Add('}');
  SL.Add('expect eof');
  SL.Add('"');
  SL.SaveToFile(ExtractFilePath(ParamStr(0)) + 'expect.sh');
  SL.Free;
end;

function InitEnv(APassword: string): boolean;
var
  SL: TStringList;
  Path: string;
  AdbPath: string;
  ExpectPath: string;
  ExpectScript: string;
begin
  Result := False;
  Path := ExtractFilePath(ParamStr(0));
  // check adb installed
  SL := ExecuteCommandF('which adb', Path);
  if SL.Count = 0 then
  begin
    Exit;
  end;
  AdbPath := SL[0];
  SL := ExecuteCommandF('which expect', Path);
  if SL.Count = 0 then
  begin
    Exit;
  end;
  ExpectPath := SL[0];
  MakeExpectScript(APassword);

  Result := True;
end;

function GetFirstDeviceId(AKill: boolean): string;
var
  SL: TStringList;
  Path: string;
  s: string;
begin
  Result := '';
  Path := ExtractFilePath(ParamStr(0));
  if AKill then
  begin
    ExecuteCommandP('adb kill-server', Path);
    ExecuteCommandP('/bin/sh expect.sh', Path);
  end;
  SL := ExecuteCommandF('adb devices', Path);
  for s in SL do
  begin
    if AnsiStartsStr('*', s) then
    begin
      Continue;
    end;
    if AnsiStartsStr('List of devices attached', s) then
    begin
      Continue;
    end;
    Result := s;
    Break;
  end;
  if Result <> '' then
  begin
    Result := StringReplace(Result, 'device', '', [rfIgnoreCase, rfReplaceAll]);
    Result := Trim(Result);
  end;
end;

end.

