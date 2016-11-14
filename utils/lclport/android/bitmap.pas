{*
 * Copyright (C) 2009 The Android Open Source Project
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *}
unit bitmap;

{$mode delphi}
{$packrecords c}

interface

uses
  ctypes, jni2;

{#include <stdint.h>
#include <jni.h>}

{$linklib jnigraphics}

const
  libname='libjnigraphics.so';

  ANDROID_BITMAP_RESUT_SUCCESS            = 0;
  ANDROID_BITMAP_RESULT_BAD_PARAMETER     =-1;
  ANDROID_BITMAP_RESULT_JNI_EXCEPTION     =-2;
  ANDROID_BITMAP_RESULT_ALLOCATION_FAILED =-3;

type
  AndroidBitmapFormat = (
    ANDROID_BITMAP_FORMAT_NONE      = 0,
    ANDROID_BITMAP_FORMAT_RGBA_8888 = 1,
    ANDROID_BITMAP_FORMAT_RGB_565   = 4,
    ANDROID_BITMAP_FORMAT_RGBA_4444 = 7,
    ANDROID_BITMAP_FORMAT_A_8       = 8
  );

  AndroidBitmapInfo = record
    width: Cardinal;//uint32_t;
    height: Cardinal;//uint32_t
    stride: Cardinal;//uint32_t
    format: Integer;//int32_t
    flags: Cardinal;//uint32_t      // 0 for now
  end;
  PAndroidBitmapInfo = ^AndroidBitmapInfo;

{**
 * Given a java bitmap object, fill out the AndroidBitmap struct for it.
 * If the call fails, the info parameter will be ignored
 *}
function AndroidBitmap_getInfo(env: PJNIEnv;
  jbitmap: jobject; info: PAndroidBitmapInfo): cint;
   cdecl; external libname name 'AndroidBitmap_getInfo';

{**
 * Given a java bitmap object, attempt to lock the pixel address.
 * Locking will ensure that the memory for the pixels will not move
 * until the unlockPixels call, and ensure that, if the pixels had been
 * previously purged, they will have been restored.
 *
 * If this call succeeds, it must be balanced by a call to
 * AndroidBitmap_unlockPixels, after which time the address of the pixels should
 * no longer be used.
 *
 * If this succeeds, *addrPtr will be set to the pixel address. If the call
 * fails, addrPtr will be ignored.
 *}
function AndroidBitmap_lockPixels(env: PJNIEnv; jbitmap: jobject;
  addrPtr: PPointer {void**}): cint; cdecl; external libname name 'AndroidBitmap_lockPixels';

{**
 * Call this to balanace a successful call to AndroidBitmap_lockPixels
 *}
function AndroidBitmap_unlockPixels(env: PJNIEnv;
  jbitmap: jobject): cint; cdecl; external libname name 'AndroidBitmap_unlockPixels';

implementation

end.

