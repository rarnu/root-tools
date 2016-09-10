library cmd;

{$mode objfpc}{$H+}

uses
  cthreads, Classes, sysutils, jni2, jni_utils, math, unt_ljava;

// JNI exchange method
function Java_com_rarnu_tools_neo_api_NativeAPI_freezeApplication(env: PJNIEnv; obj: jobject; packageName: jstring; isFreezed: jboolean): jboolean; stdcall;
var
  ret: Boolean;
begin
  ret := FreezeApplication(jstringToString(env, packageName), isFreezed = JNI_TRUE);
  Result := ifthen(ret, JNI_TRUE, JNI_FALSE);
end;

function Java_com_rarnu_tools_neo_api_NativeAPI_freezeComponent(env: PJNIEnv; obj: jobject; packageName: jstring; componentName: jstring; isFreezed: jboolean): jboolean; stdcall;
begin
  // TODO:
end;

function Java_com_rarnu_tools_neo_api_NativeAPI_freezeComponents(env: PJNIEnv; obj: jobject; packageName: jstring; components: jarray; isFreezed: jboolean): jboolean; stdcall;
var
  strArr: TStringArray;
  ret: Boolean;
begin
  // TODO:
  strArr := jstringArrayToStringArray(env, components);
  ret := FreeComponents(jstringToString(env, packageName), strArr, isFreezed = JNI_TRUE);
  Result := ifthen(ret, JNI_TRUE, JNI_FALSE);
end;

exports
  Java_com_rarnu_tools_neo_api_NativeAPI_freezeApplication,
  Java_com_rarnu_tools_neo_api_NativeAPI_freezeComponent,
  Java_com_rarnu_tools_neo_api_NativeAPI_freezeComponents;

begin

end.

