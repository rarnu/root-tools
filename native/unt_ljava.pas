unit unt_ljava;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, unt_cmd, strutils;

// JNI real method
function Mount(): Boolean;
procedure MakePreferenceReadable(sdk: integer; packageName: string);
function FreezeApplication(packageName: string; isFreezed: boolean): boolean;
function FreeComponents(packageName: string; Components: TStringArray; isFreezed: boolean): boolean;
function FreezeComponent(packageName: string; componentName: string; isFreezed: boolean): boolean;
function WriteFile(filePath: string; _text: string; perm: integer): boolean;
function CatFile(src: string; dest: string; perm: integer): Boolean;
procedure ForceDeleteFile(path: string);
procedure ForceDropCache();
procedure KillProcess();

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

procedure ForceDeleteFile(path: string);
var
  cmd: string;
  outstr: string;
begin
  cmd := Format('rm -f -r %s', [path]);
  internalRun([cmd], outstr);
end;

procedure ForceDropCache;
const
  cmdSync = 'sync';
  cmdDrop = 'echo 3 > /proc/sys/vm/drop_caches';
  cmdDropRestore = 'echo 0 > /proc/sys/vm/drop_caches';
var
  outstr: string;
begin
  internalRun([cmdSync, cmdDrop, cmdDropRestore], outstr);
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
  Result := LeftStr(s, p - 1);
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
begin
  b := internalRun([CMD_PS], outstr);
  if b then begin
    slPs := TStringList.Create;
    slPid := TStringList.Create;
    slPs.Text:= outstr;
    for i := 0 to slPs.Count - 1 do begin
      if (slPs[i].StartsWith('u')) then begin
        pidstr:= GetProcessId(slPs[i]);
        if (pidstr <> '') then begin
          slPid.Add(pidstr);
        end;
      end;
    end;
    // clean
    for i := 0 to slPid.Count - 1 do begin
      internalRun([Format(CMD_KILL, [slPid[i]])], outstr);
    end;
    slPid.Free;
    slPs.Free;
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

end.



