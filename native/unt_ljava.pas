unit unt_ljava;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, unt_cmd, strutils, jni2, jni_utils;

// JNI real method
function FreezeApplication(packageName: String; isFreezed: Boolean): Boolean;
function FreeComponents(packageName: String; components: TStringArray; isFreezed: Boolean): Boolean;
function FreezeComponent(packageName: String; componentName: String; isFreezed: Boolean): Boolean;
function setPremission(env: PJNIEnv; ctx: jobject; filePath: jstring; text: jstring; perm: jint):Boolean;

implementation

function FreezeApplication(packageName: String; isFreezed: Boolean): Boolean;
var
  cmd: string;
  outstr: string;
begin
  cmd := Format('pm %s %s', [IfThen(isFreezed, 'disable', 'enable'), packageName]);
  Result := internalRun([cmd], outstr);
end;

function FreezeComponent(packageName: String; componentName: String; isFreezed: Boolean): Boolean;
var
  cmd: string;
  outstr: string;
begin
  cmd := Format('pm %s %s/%s', [IfThen(isFreezed, 'disable', 'enable'), packageName, componentName]);
  Result := internalRun([cmd], outstr);
end;

function FreeComponents(packageName: string; components: TStringArray;
  isFreezed: Boolean): Boolean;
var
  cmd: string = '';
  outstr: string;
  i: Integer;
begin
  for i := 0 to Length(components) - 1 do begin
    cmd += Format('pm %s %s/%s;', [IfThen(isFreezed, 'disable', 'enable'), packageName, components[i]]);
  end;
  Result := internalRun([cmd], outstr);
end;

function setPremission(env: PJNIEnv; ctx: jobject; filePath: jstring; text: jstring; perm: jint): Boolean;
var
  cmd: string = '';
  permStr: String;
  outstr: string;
begin
  permStr := IntToStr(perm);
  while Length(permStr)<3 do begin
    permStr :='0' + permStr;
    end;

   cmd := Format('chmod -R %s %s;', [ permStr, filePath]);
   Result := internalRun([cmd], outstr);
end;


end.

