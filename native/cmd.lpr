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

exports
  Java_com_rarnu_tools_neo_api_NativeAPI_freezeApplication;

begin

end.

