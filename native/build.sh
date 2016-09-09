#!/bin/sh

# environment
ROOT_PATH=/usr/local/codetyphon
TYPHON_PATH=${ROOT_PATH}/typhon
TYPHON_BIN_LIB=${ROOT_PATH}/binLibraries
FPC=/usr/local/codetyphon/fpc/fpc64/bin/x86_64-linux/fpc

if [ ! -d "lib" ]; then
	mkdir lib
fi

if [ ! -d "out" ]; then
	mkdir out
fi

# build for android

__compile() {
	CPU=$1
	LIB=$2
	PROJ=$3
	rm -fr lib/${LIB}-android/*
	mkdir lib/${LIB}-android/
	if [ ! -d "out/${LIB}" ]; then 
		mkdir out/${LIB}
	fi
	${FPC} -B -Tandroid -P${CPU} \
	-MObjFPC -Scghi -Cg -O1 -g -gl -l -vewnhibq \
	-Filib/${LIB}-android \
	-Fl${TYPHON_BIN_LIB}/android-5.0-api21-${LIB} \
	-Fu. -FUlib/${LIB}-android \
	-oout/${LIB}/lib${PROJ}.so \
	${PROJ}.lpr
}

__compile "arm" "arm" "cmd"
__compile "i386" "i386" "cmd"
__compile "mipsel" "mips" "cmd"

