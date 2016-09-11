#!/bin/sh
DoExitAsm ()
{ echo "An error occurred while assembling $1"; exit 1; }
DoExitLink ()
{ echo "An error occurred while linking $1"; exit 1; }
echo Assembling cmd
/usr/local/codetyphon/fpc/fpc64/bin/x86_64-linux/arm-android-as -march=armv5t -mfpu=softvfp -o /home/conquer/Documents/code/root-tools/native/lib/arm-android/cmd.o  /home/conquer/Documents/code/root-tools/native/lib/arm-android/cmd.s
if [ $? != 0 ]; then DoExitAsm cmd; fi
rm /home/conquer/Documents/code/root-tools/native/lib/arm-android/cmd.s
echo Linking libcmd.so
OFS=$IFS
IFS="
"
/usr/local/codetyphon/fpc/fpc64/bin/x86_64-linux/arm-android-ld -z max-page-size=0x1000 -z common-page-size=0x1000 -z noexecstack -z now --gc-sections -L. -T link.res -o libcmd.so -shared -soname libcmd.so
if [ $? != 0 ]; then DoExitLink libcmd.so; fi
IFS=$OFS
