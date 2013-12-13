#!/bin/sh

rm RootTools.apk RootTools_signed.apk
cp ../out/production/RootTools/RootTools.apk ./
java -jar signapk.jar rarnu.x509.pem rarnu.pk8 RootTools.apk RootTools_signed.apk
adb install -r RootTools_signed.apk

