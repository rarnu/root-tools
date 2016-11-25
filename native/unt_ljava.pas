unit unt_ljava;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, unt_cmd, strutils, android;

// JNI real method
function Mount(): Boolean;
function IsSystemRW(): Boolean;
procedure MakePreferenceReadable(sdk: integer; packageName: string);
function FreezeApplication(packageName: string; isFreezed: boolean): boolean;
function FreeComponents(packageName: string; Components: TStringArray; isFreezed: boolean): boolean;
function FreezeComponent(packageName: string; componentName: string; isFreezed: boolean): boolean;
function WriteFile(filePath: string; _text: string; perm: integer): boolean;
function CatFile(src: string; dest: string; perm: integer): Boolean;
procedure ForceDeleteFile(path: string);
procedure ForceDropCache();
procedure KillProcess();
function DeleteSystemApp(pkgName: string): Boolean;
function IsAppRequiredBySystem(pkgName: string): Boolean;

implementation

function Mount: Boolean;
const
  cmd = 'mount -o remount,rw /system';
var
  outstr: string;
begin
  Result := True;
  internalRun([cmd], outstr);
  LOGE(PChar(outstr));
  outstr:= outstr.ToLower;
  if (outstr.Contains('denied')) or (outstr.Contains('null environment')) or (outstr.Contains('not allowed')) then begin
    Result := False;
  end;
end;

function IsSystemRW: Boolean;
const
  cmd = 'mount';
var
  outstr: string;
  sl: TStringList;
  i: Integer;
begin
  // is system rw
  Result := False;
  internalRun([cmd], outstr);
  sl := TStringList.Create;
  sl.Text:= outstr;
  for i := 0 to sl.Count - 1 do begin
    if (sl[i].Contains(' /system')) and (sl[i].Contains('ext4')) then begin
      if (sl[i].Contains('rw')) then begin
        Result := True;
        Break;
      end;
    end;
  end;
  sl.Free;
end;

procedure MakePreferenceReadable(sdk: integer; packageName: string);
var
  cmd: string;
  outstr: string;
begin
  if (sdk >= 24) then begin
    cmd := Format('chmod -R 777 /data/data/%s/shared_prefs', [packageName]);
    internalRun([cmd], outstr);
    LOGE(PChar(outstr));
  end;
end;

function FreezeApplication(packageName: string; isFreezed: boolean): boolean;
var
  cmd: string;
  outstr: string;
begin
  cmd := Format('pm %s %s', [IfThen(isFreezed, 'disable', 'enable'), packageName]);
  Result := internalRun([cmd], outstr);
  LOGE(PChar(outstr));
end;

function FreezeComponent(packageName: string; componentName: string; isFreezed: boolean): boolean;
var
  cmd: string;
  outstr: string;
begin
  cmd := Format('pm %s %s/%s', [IfThen(isFreezed, 'disable', 'enable'), packageName, componentName]);
  Result := internalRun([cmd], outstr);
  LOGE(PChar(outstr));
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
    LOGE(PChar(outstr));
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
  LOGE(PChar(outstr));
end;

procedure ForceDeleteFile(path: string);
var
  cmd: string;
  outstr: string;
begin
  cmd := Format('rm -f -r %s', [path]);
  internalRun([cmd], outstr);
  LOGE(PChar(outstr));
end;

procedure ForceDropCache;
const
  // cmdSync = 'sync';
  cmdDrop = 'echo 3 > /proc/sys/vm/drop_caches';
  cmdDropRestore = 'echo 0 > /proc/sys/vm/drop_caches';
var
  outstr: string;
begin
  internalRun([cmdDrop, cmdDropRestore], outstr);
  LOGE(PChar(outstr));
end;

function GetProcessId(str: string): string;
var
  p: Integer;
  s: string;
begin
  // do not kill self
  if (str.Contains('com.rarnu.tools.neo')) or (str.EndsWith(' su')) then begin
    Result := '';
    Exit;
  end;
  s := str.Trim;
  p := Pos(' ', s);
  s := s.Substring(p).Trim;
  p := Pos(' ', s);
  Result := LeftStr(s, p - 1).Trim;
end;

function GetPackageName(str: string): string;
var
  p: Integer;
begin
  p := str.LastIndexOf(' ');
  Result := str.Substring(p + 1).Trim;
  LOGE(PChar(Format('GetPackageName: %s', [Result])));
end;

procedure KillProcess;
const
  CMD_PS = 'ps';
  CMD_KILL = 'kill %s';
var
  outstr: string;
  b: Boolean;
  slPs: TStringList;
  slPid: TStringList;
  pidstr: string;
  i: Integer;
  pkgName: string;
begin
  b := internalRun([CMD_PS], outstr);
  if b then begin
    slPs := TStringList.Create;
    slPid := TStringList.Create;
    slPs.Text:= outstr;
    for i := 0 to slPs.Count - 1 do begin
      if (slPs[i].StartsWith('u0')) then begin
        pkgName:= GetPackageName(slPs[i]);
        if (not pkgName.Contains('core')) and (not pkgName.StartsWith('android.')) and (not pkgName.Contains('secure')) then begin
          pidstr:= GetProcessId(slPs[i]);
          if (pidstr <> '') then begin
            slPid.Add(pidstr);
          end;
        end;
      end;
    end;
    // clean
    for i := 0 to slPid.Count - 1 do begin
      internalRun([Format(CMD_KILL, [slPid[i]])], outstr);
      LOGE(PChar(outstr));
    end;
    slPid.Free;
    slPs.Free;
  end;
end;

function GetSlashCount(path: string): Integer;
var
  i: Integer;
  c: Integer = 0;
begin
  for i := 1 to Length(path) do begin
    if (path[i] = '/') then begin
      c += 1;
    end;
  end;
  Result := c;
end;

function DeleteSystemApp(pkgName: string): Boolean;
var
  ret: Boolean;
  outstr: string;
  apkPath: string;
  parentDir: string;
  sc: Integer;
begin
  Result := False;
  // pm path com.android.email
  // package:/system/app/Email/Email.apk
  ret := internalRun(['pm path ' + pkgName], outstr);
  if (not ret) or (outstr.Trim = '') then begin
    Exit;
  end;
  outstr:= StringReplace(outstr, 'package:', '', [rfIgnoreCase, rfReplaceAll]).Trim;
  apkPath:= outstr;
  // outstr = /system/app/Email/Email.apk
  // /system/app/aaa.apk
  sc := GetSlashCount(apkPath);
  if sc = 4 then begin
    parentDir:= ExtractFileDir(apkPath);
    ret := internalRun(['rm -r ' + parentDir], outstr);
  end else if sc = 3 then begin
    ret := internalRun(['rm ' + apkPath], outstr);
  end;
  LOGE(PChar(outstr));
  Result := ret;
end;

function IsAppRequiredBySystem(pkgName: string): Boolean;
var
  ret: Boolean;
  outstr: string;
  apkPath: string;
begin
  Result := False;
  ret := internalRun(['pm path ' + pkgName], outstr);
  LOGE(PChar(outstr));
  if (not ret) or (outstr.Trim = '') then begin
    Exit;
  end;
  outstr:= StringReplace(outstr, 'package:', '', [rfIgnoreCase, rfReplaceAll]).Trim;
  apkPath:= outstr;
  if (apkPath.Contains('/data/app')) or (apkPath.Contains('/data/priv-app')) then begin
    Exit;
  end;

  // is app required by system?
  if (pkgName = 'android') or (pkgName.StartsWith('android.')) or (pkgName.StartsWith('com.android.')) or (pkgName.StartsWith('com.') and pkgName.Contains('.android.')) then begin
    Result := True;
  end;
  if (pkgName.EndsWith('.core') or pkgName.EndsWith('.securitycore')) then begin
    Result := True;
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
  LOGE(PChar(outstr));
end;

end.



