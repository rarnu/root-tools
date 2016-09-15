library cmd;

{$mode objfpc}{$H+}

uses
  cthreads, Classes, sysutils, jni2, jni_utils, math, unt_ljava, unt_clean;

// JNI exchange method
function Java_com_rarnu_tools_neo_api_NativeAPI_mount(env: PJNIEnv; obj: jobject): jboolean; stdcall;
var
  ret: Boolean;
begin
  ret := Mount();
  Result := ifthen(ret, JNI_TRUE, JNI_FALSE);
end;

procedure Java_com_rarnu_tools_neo_api_NativeAPI_makePreferenceReadable(env: PJNIEnv; obj: jobject; sdk: jint; packageName: jstring); stdcall;
begin
  MakePreferenceReadable(sdk, jstringToString(env, packageName));
end;

function Java_com_rarnu_tools_neo_api_NativeAPI_freezeApplication(env: PJNIEnv; obj: jobject; packageName: jstring; isFreezed: jboolean): jboolean; stdcall;
var
  ret: Boolean;
begin
  ret := FreezeApplication(jstringToString(env, packageName), isFreezed = JNI_TRUE);
  Result := ifthen(ret, JNI_TRUE, JNI_FALSE);
end;

function Java_com_rarnu_tools_neo_api_NativeAPI_freezeComponent(env: PJNIEnv; obj: jobject; packageName: jstring; componentName: jstring; isFreezed: jboolean): jboolean; stdcall;
var
  ret: Boolean;
begin
  ret := FreezeComponent(jstringToString(env, packageName), jstringToString(env, componentName), isFreezed = JNI_TRUE);
  Result := ifthen(ret, JNI_TRUE, JNI_FALSE);
end;

function Java_com_rarnu_tools_neo_api_NativeAPI_freezeComponents(env: PJNIEnv; obj: jobject; packageName: jstring; components: jarray; isFreezed: jboolean): jboolean; stdcall;
var
  strArr: TStringArray;
  ret: Boolean;
begin
  strArr := jstringArrayToStringArray(env, components);
  ret := FreeComponents(jstringToString(env, packageName), strArr, isFreezed = JNI_TRUE);
  Result := ifthen(ret, JNI_TRUE, JNI_FALSE);
end;

procedure Java_com_rarnu_tools_neo_api_NativeAPI_systemClean(env: PJNIEnv; obj: jobject; ctx: jobject); stdcall;
begin
  CleanData(env, ctx);
end;

function Java_com_rarnu_tools_neo_api_NativeAPI_writeFile(env: PJNIEnv; obj: jobject; ctx: jobject; filePath: jstring; text: jstring; perm: jint): jboolean; stdcall;
var
  ret: Boolean;
begin
  ret := WriteFile(jstringToString(env, filePath), jstringToString(env, text), perm);
  Result := ifthen(ret, JNI_TRUE, JNI_FALSE);
end;

function Java_com_rarnu_tools_neo_api_NativeAPI_catFile(env: PJNIEnv; obj: jobject; src: jstring; dest: jstring; perm: jint): jboolean; stdcall;
var
  ret: Boolean;
begin
  ret := CatFile(jstringToString(env, src), jstringToString(env, dest), perm);
  Result := ifthen(ret, JNI_TRUE, JNI_FALSE);
end;

exports
  Java_com_rarnu_tools_neo_api_NativeAPI_mount,
  Java_com_rarnu_tools_neo_api_NativeAPI_makePreferenceReadable,
  Java_com_rarnu_tools_neo_api_NativeAPI_freezeApplication,
  Java_com_rarnu_tools_neo_api_NativeAPI_freezeComponent,
  Java_com_rarnu_tools_neo_api_NativeAPI_freezeComponents,
  Java_com_rarnu_tools_neo_api_NativeAPI_systemClean,
  Java_com_rarnu_tools_neo_api_NativeAPI_writeFile,
  Java_com_rarnu_tools_neo_api_NativeAPI_catFile;

begin

end.

