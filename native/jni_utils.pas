unit jni_utils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, jni2;

function jstringToString(env: PJNIEnv; jstr: jstring): string;
function stringToJString(env: PJNIEnv; str: string): jstring;
function argsToJValues(env:PJNIEnv; const Args: array of const): PJValue;
function jstringArrayToStringArray(env: PJNIEnv; arr: jarray): TStringArray;

implementation

function argsToJValues(env: PJNIEnv; const Args: array of const): PJValue;
var
  I: Integer;
  FConvertedArgs: jvalueArray;
begin
  SetLength(FConvertedArgs, Length(Args));
  for I := 0 to High(Args) do
    case Args[I].VType of
      vtInteger:
        FConvertedArgs[I].i := Args[I].VInteger;
      vtBoolean:
        FConvertedArgs[I].z := jboolean(Args[I].VBoolean);
      vtWideChar:
        FConvertedArgs[I].c := jchar(Args[I].VWideChar);
      vtInt64:
        FConvertedArgs[I].j := Args[I].VInt64^;
      vtPointer, vtObject:
        FConvertedArgs[I].l := JObject(Args[I].VObject);
      vtAnsiString:
        FConvertedArgs[I].l := stringToJString(env, string(Args[I].VAnsiString));
      vtExtended:
        FConvertedArgs[I].d := Args[I].VExtended^;
    else
      raise Exception.Create('Unsupported variant argument');
    end;
  Result := PJValue(FConvertedArgs);
end;

function jstringArrayToStringArray(env: PJNIEnv; arr: jarray): TStringArray;
var
  len: Integer;
  i: Integer;
  jstr: jstring;
begin
  len := env^^.GetArrayLength(env, arr);
  SetLength(Result, len);
  for i := 0 to len - 1 do begin
    jstr:= env^^.GetObjectArrayElement(env, arr, i);
    Result[i] := jstringToString(env, jstr);
  end;
end;

function jstringToString(env: PJNIEnv; jstr: jstring): string;
var
  cb: jboolean;
  s: pchar;
begin
  cb := JNI_FALSE;
  s := env^^.GetStringUTFChars(env, jstr, @cb);
  Result := string(s);
  env^^.ReleaseStringUTFChars(env, jstr, s);
end;

function stringToJString(env: PJNIEnv; str: string): jstring;
begin
  Result := env^^.NewStringUTF(env, pchar(str));
end;

end.

