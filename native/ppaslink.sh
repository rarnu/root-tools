#!/bin/sh
DoExitAsm ()
{ echo "An error occurred while assembling $1"; exit 1; }
DoExitLink ()
{ echo "An error occurred while linking $1"; exit 1; }
OFS=$IFS
IFS="
"
/usr/bin/ld /usr/lib/dylib1.o     -dynamic -dylib -multiply_defined suppress -L. -o /Users/rarnu/Code/Github/root-tools/native/libcmd.dylib `cat link.res` -exported_symbols_list linksyms.fpc
if [ $? != 0 ]; then DoExitLink ; fi
IFS=$OFS
