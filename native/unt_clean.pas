unit unt_clean;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, process, unt_cmd, jni2, jni_utils;

const
  STATUS_PROGRESS = 0;
  STATUS_COMPLETE = 1;
  STATUS_ERROR = 2;

procedure CleanData(env: PJNIEnv; ctx: jobject);

implementation

var
  _DU_CMD: string = '';

type

  { TCacheSize }

  TCacheSize = class
  private
    FSize: LongInt;
    FSizeReadable: String;

  public
    constructor Create(sr: String; s: LongInt);
  public
    property SizeReadable: String read FSizeReadable write FSizeReadable;
    property Size: LongInt read FSize write FSize;
  end;

procedure CleanCallback(env: PJNIEnv; ctx: jobject; status: Integer; data: string);
var
  cls: jclass;
  obj: jobject;
  clsInit: jmethodID;
  clsCallback: jmethodID;
  dataStr: jstring;
begin
  cls := env^^.FindClass(env, 'com/rarnu/tools/neo/api/NativeAPI');
  clsInit:= env^^.GetMethodID(env, cls, '<init>', '()V');
  clsCallback:= env^^.GetMethodID(env, cls, 'cleanCallback', '(Landroid/content/Context;ILjava/lang/String;)V');
  obj := env^^.NewObjectA(env, cls, clsInit, nil);
  dataStr:= stringToJString(env, data);
  env^^.CallVoidMethodA(env, obj, clsCallback, argsToJValues(env, [ctx, status, dataStr]));
  env^^.DeleteLocalRef(env, dataStr);
  env^^.DeleteLocalRef(env, obj);
  env^^.DeleteLocalRef(env, cls);
end;

function GetDuCmd(): string;
const
  BUSYBOX_PATH: array[0..1] of String = ('/system/bin/busybox', '/system/xbin/busybox');
  DU_PATH: array[0..1] of string = ('/system/bin/du', '/system/xbin/du');
var
  duExists: Boolean = false;
  busyboxExists: Boolean = false;
  s: string;
begin
  Result := '';
  for s in DU_PATH do begin
    if FileExists(s) then begin
      duExists:= True;
      Break;
    end;
  end;
  if duExists then begin
    Result := 'du';
    Exit;
  end;
  for s in BUSYBOX_PATH do begin
    if FileExists(s) then begin
      busyboxExists:= True;
      Break;
    end;
  end;
  if busyboxExists then begin
    Result := 'busybox du';
  end;
end;

function GetCacheSize(path: string): TCacheSize;
const
  CMD_SIZE_FMT = '%s -s -k "%s"';
var
  cmd: string;
  b: Boolean;
  outstr: string;
  sizeStr: string = '0';
  size: LongInt = 0;
begin
  cmd := Format(CMD_SIZE_FMT, [_DU_CMD, path]);
  b := internalRun([cmd], outstr);
  if b then begin
    try
      sizeStr:= outstr.Substring(0, outstr.IndexOf(#9)).Trim;
      size := StrToInt(sizeStr);
    except
    end;
  end;
  Result := TCacheSize.Create(sizeStr + 'K', size);
end;


function DeleteCache(path: String): Boolean;
const
  CMD_DELETE_FMT = 'rm -r "%s"';
var
  b: Boolean;
  outstr: string;
  cmd: string;
begin
  cmd := Format(CMD_DELETE_FMT, [path]);
  b := internalRun([cmd], outstr);
  Result := b;
end;

function DeleteAnrLog(): Boolean;
const
  CMD_DELETE_ANR = 'rm -r /data/anr/*';
var
  b: Boolean;
  outstr: string;
begin
  b := internalRun([CMD_DELETE_ANR], outstr);
  Result := b;
end;

function IsCachedAppInstalled(oriList: TStringList; app: String): Boolean;
var
  newAppPath: string;
  idx: Integer;
begin
  Result := False;
  if (app.StartsWith('system')) or (app.StartsWith('data@dalvik-cache')) then begin
    Result := True;
    Exit;
  end;
  newAppPath:= app.Replace('data@app@', '');
  newAppPath:= newAppPath.Substring(0, newAppPath.IndexOf('@'));
  idx := oriList.IndexOf(newAppPath);
  Result := idx <> -1;
end;

function IsProfileInstalled(oriList: TStringList; app: String): Boolean;
var
  s: string;
begin
  Result := False;
  for s in oriList do begin
    if s.Contains(app) then begin
      Result := True;
      Break;
    end;
  end;
end;

function DeleteRemainArtCache(env: PJNIEnv; ctx: jobject): LongInt;
const
  CMD_LS_APP = 'ls /data/app';
  CMD_LS_PKG = 'pm list packages';
  CMD_LS_ARM = 'ls /data/dalvik-cache/arm';
  CMD_LS_ARM64 = 'ls /data/dalvik-cache/arm64';
  CMD_LS_PROFILE = 'ls /data/dalvik-cache/profiles';
var
  outstr: string;
  listInstalled: TStringList;
  listAllInstalled: TStringList;
  listArm: TStringList;
  listArm64: TStringList;
  listProfile: TStringList;
  totalSize: LongInt = 0;
  tmpPath: string;
  size: TCacheSize;
  s: string;
begin
  internalRun([CMD_LS_APP], outstr);
  listInstalled := TStringList.Create;
  listInstalled.Text:= outstr;
  internalRun([CMD_LS_PKG], outstr);
  listAllInstalled := TStringList.Create;
  listAllInstalled.Text:= outstr;
  internalRun([CMD_LS_ARM], outstr);
  listArm := TStringList.Create;
  listArm.Text:= outstr;
  internalRun([CMD_LS_ARM64], outstr);
  listArm64 := TStringList.Create;
  listArm64.Text:= outstr;
  internalRun([CMD_LS_PROFILE], outstr);
  listProfile := TStringList.Create;
  listProfile.Text:= outstr;

  for s in listArm do begin
    if s.Trim <> '' then begin
      CleanCallback(env, ctx, STATUS_PROGRESS, Format('Scan %s', [s]));
      if (not IsCachedAppInstalled(listInstalled, s)) then begin
        tmpPath:= '/data/dalvik-cache/arm/' + s;
        size := GetCacheSize(tmpPath);
        if (DeleteCache(tmpPath)) then begin
          CleanCallback(env, ctx, STATUS_PROGRESS, Format('Clean %s(%s)', [s, size.SizeReadable]));
          totalSize += size.Size;
        end;
        size.Free;
      end;
    end;
  end;

  for s in listArm64 do begin
    if s.Trim <> '' then begin
      CleanCallback(env, ctx, STATUS_PROGRESS, Format('Scan %s', [s]));
      if (not IsCachedAppInstalled(listInstalled, s)) then begin
        tmpPath:= '/data/dalvik-cache/arm64/' + s;
        size := GetCacheSize(tmpPath);
        if (DeleteCache(tmpPath)) then begin
          CleanCallback(env, ctx, STATUS_PROGRESS, Format('Clean %s(%s)', [s, size.SizeReadable]));
          totalSize += size.Size;
        end;
        size.Free;
      end;
    end;
  end;

  for s in listProfile do begin
    if s.Trim <> '' then begin
      CleanCallback(env, ctx, STATUS_PROGRESS, Format('Scan %s', [s]));
      if (not IsProfileInstalled(listAllInstalled, s)) then begin
        tmpPath:= '/data/dalvik-cache/profiles/' + s;
        size := GetCacheSize(tmpPath);
        if (DeleteCache(tmpPath)) then begin
          CleanCallback(env, ctx, STATUS_PROGRESS, Format('Clean %s(%s)', [s, size.SizeReadable]));
          totalSize += size.Size;
        end;
        size.Free;
      end;
    end;
  end;

  listProfile.Free;
  listArm64.Free;
  listArm.Free;
  listAllInstalled.Free;
  listInstalled.Free;

  Result := totalSize;
end;

function CleanCache(env: PJNIEnv; ctx: jobject): LongInt;
const
  CMD_FIND_CACHE = 'find /data/data/ -type dir -name "cache"';
var
  b: Boolean;
  outstr: string;
  items: TStringList;
  s: string;
  cs: TCacheSize;
  totalSize: LongInt = 0;
begin
  b := internalRun([CMD_FIND_CACHE], outstr);
  if not b then begin
    CleanCallback(env, ctx, STATUS_ERROR, 'Can not clean Cache');
    Exit;
  end;
  items := TStringList.Create;
  items.Text:= outstr;
  for s in items do begin
    CleanCallback(env, ctx, STATUS_PROGRESS, Format('Scan %s', [s]));
    cs := GetCacheSize(s);
    if cs.Size > 16 then begin
      if (DeleteCache(s)) then begin
        CleanCallback(env, ctx, STATUS_PROGRESS, Format('Clean %s(%s)', [s, cs.SizeReadable]));
        totalSize += cs.size;
      end;
    end;
    cs.Free;
  end;
  items.Free;
  Result := totalSize;
end;

function CleanANR(env: PJNIEnv; ctx: jobject): LongInt;
var
  anrSize: TCacheSize;
begin
  Result := 0;
  anrSize := GetCacheSize('/data/anr/');
  if (DeleteAnrLog()) then begin
    CleanCallback(env, ctx, STATUS_PROGRESS, Format('Clean ANR (%s)', [anrSize.SizeReadable]));
    Result := anrSize.size;
  end;
  anrSize.Free;
end;

function getReadableFileSize(size: LongInt): String;
const
  UNITS: array[0..4] of String = ('K', 'M', 'G', 'T', 'P');
  MOOD = 1024.0;
var
  nSize: Double;
  i: Integer = 0;
begin
  nSize:= Double(size);
  while (nSize >= MOOD) do begin
    nSize /= MOOD;
    i += 1;
  end;
  Result := FormatFloat('#.##', nSize) + UNITS[i];
end;

function CleanART(env: PJNIEnv; ctx: jobject): LongInt;
begin
  Result := deleteRemainArtCache(env, ctx);
end;

procedure CleanData(env: PJNIEnv; ctx: jobject);
var
  totalSize: LongInt = 0;
begin
  if _DU_CMD = '' then begin
    _DU_CMD := GetDuCmd();
  end;
  totalSize += CleanCache(env, ctx);
  totalSize += CleanANR(env, ctx);
  totalSize += CleanART(env, ctx);
  CleanCallback(env, ctx, STATUS_COMPLETE, Format('Total Cleaned: %s', [getReadableFileSize(totalSize)]));
end;

{ TCacheSize }

constructor TCacheSize.Create(sr: String; s: LongInt);
begin
  FSizeReadable:= sr;
  FSize:= s;
end;

end.

