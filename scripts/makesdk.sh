#!/bin/sh

DEX=$1
AJAR=$2
OUT=$3

d2j-dex2jar.sh $DEX -o framework-dex.jar
mv framework-dex.jar framework-dex.zip
unzip -d framework-dex framework-dex.zip
cp $AJAR android-sdk.zip
unzip -d android-sdk android-sdk.zip 
cp -r -f framework-dex/* android-sdk/
cd android-sdk
zip -r android-new.zip *
cd ..
mv android-sdk/android-new.zip android-new.zip
mv android-new.zip android-new.jar
rm android-sdk.zip
rm framework-dex.zip
rm -fr android-sdk/
rm -fr framework-dex/
mv android-new.jar $OUT
echo "done"



