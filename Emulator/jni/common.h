#ifndef _COMMON_H
#define _COMMON_H 1

#include <stddef.h>
#include "jni.h"

int registerNativeMethods(JNIEnv* env, const char* className,
    JNINativeMethod* gMethods, int numMethods);

#endif
