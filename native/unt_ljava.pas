unit unt_ljava;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, unt_cmd, strutils, jni2, jni_utils;

// JNI real method
function Mount(): Boolean;
procedure MakePreferenceReadable(sdk: integer; packageName: string);
function FreezeApplication(packageName: string; isFreezed: boolean): boolean;
function FreeComponents(packageName: string; Components: TStringArray; isFreezed: boolean): boolean;
function FreezeComponent(packageName: string; componentName: string; isFreezed: boolean): boolean;
function WriteFile(filePath: string; _text: string; perm: integer): boolean;
function CatFile(src: string; dest: string; perm: integer): Boolean;

implementation

function Mount: Boolean;
const
  cmd = 'mount -o remount,rw /system';
var
  outstr: string;
begin
  Result := internalRun([cmd], outstr);
  outstr:= outstr.ToLower;
  if (outstr.Contains('denied')) or (outstr.Contains('null environment')) or (outstr.Contains('not allowed')) then begin
    Result := False;
  end;
end;

procedure MakePreferenceReadable(sdk: integer; packageName: string);
var
  cmd: string;
  outstr: string;
begin
  if (sdk >= 24) then begin
    cmd := Format('chmod -R 777 /data/data/%s/shared_prefs', [packageName]);
    internalRun([cmd], outstr);
  end;
end;

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
      Format('cp %s %s', [tmpPathEx, filePath]),     // cp /system/build.prop.tmp /system/build.prop
      Format('chmod %s %s', [modStr, filePath])
      ], outstr);
  end;
end;

function CatFile(src: string; dest: string; perm: integer): Boolean;
var
  modstr: string;
  outstr: string;
begin
  modstr:= IntToStr(perm);
  while modstr.Length < 3 do begin
    modstr := '0' + modstr;
  end;
  Result := internalRun([
    Format('cat %s > %s', [src, dest]),
    Format('chmod %s %s', [modstr, dest])
  ], outstr);
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

end.



