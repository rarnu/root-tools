unit unt_ljava;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, unt_cmd, strutils, jni2, jni_utils;

// JNI real method
function FreezeApplication(packageName: string; isFreezed: boolean): boolean;
function FreeComponents(packageName: string; Components: TStringArray; isFreezed: boolean): boolean;
function FreezeComponent(packageName: string; componentName: string; isFreezed: boolean): boolean;
function WriteFile(filePath: string; _text: string; perm: integer): boolean;

// function setPremission(env: PJNIEnv; ctx: jobject; filePath: jstring; text: jstring; perm: jint):Boolean;

implementation

function FreezeApplication(packageName: string; isFreezed: boolean): boolean;
var
  cmd: string;
  outstr: string;
begin
  cmd := Format('pm %s %s', [IfThen(isFreezed, 'disable', 'enable'), packageName]);
  Result := internalRun([cmd], outstr);
end;

function FreezeComponent(packageName: string; componentName: string; isFreezed: boolean): boolean;
var
  cmd: string;
  outstr: string;
begin
  cmd := Format('pm %s %s/%s', [IfThen(isFreezed, 'disable', 'enable'), packageName, componentName]);
  Result := internalRun([cmd], outstr);
end;

function WriteFile(filePath: string; _text: string; perm: integer): boolean;
const
  CACHE = '/sdcard/.tmp/';
var
  tmpPath: string;
  tmpPathEx: string;
  outstr: string;
  modStr: string;
begin
  Result := False;
  if not DirectoryExists(CACHE) then begin
    ForceDirectories(CACHE);
  end;
  tmpPath := CACHE + ExtractFileName(filePath);
  tmpPathEx := filePath + '.tmp';
  with TStringList.Create do begin
    Text := _text;
    SaveToFile(tmpPath);
    Free;
  end;
  modStr := IntToStr(perm);
  while modStr.Length < 3 do begin
    modStr := '0' + modStr;
  end;
  if FileExists(tmpPath) then begin
    Result := internalRun([Format('cp %s %s', [tmpPath, tmpPathEx]),     // cp /sdcard/build.prop /system/build.prop.tmp
      Format('chmod %s %s', [modStr, tmpPathEx]),   // chmod 755 /system/build.prop.tmp
      Format('cp %s %s', [tmpPathEx, filePath])     // cp /system/build.prop.tmp /system/build.prop
      ], outstr);
  end;
end;

function FreeComponents(packageName: string; Components: TStringArray; isFreezed: boolean): boolean;
var
  cmd: string = '';
  outstr: string;
  i: integer;
begin
  for i := 0 to Length(Components) - 1 do begin
    cmd += Format('pm %s %s/%s;', [IfThen(isFreezed, 'disable', 'enable'), packageName, Components[i]]);
  end;
  Result := internalRun([cmd], outstr);
end;

function setPremission(env: PJNIEnv; ctx: jobject; filePath: jstring; Text: jstring; perm: jint): boolean;
var
  cmd: string = '';
  permStr: string;
  outstr: string;
begin
  permStr := IntToStr(perm);
  while Length(permStr) < 3 do begin
    permStr := '0' + permStr;
  end;

  cmd := Format('chmod -R %s %s;', [permStr, filePath]);
  Result := internalRun([cmd], outstr);
end;


end.



