#!/bin/sh
_APK=$1
echo "signing..."
java -jar signapk.jar platform.x509.pem platform.pk8 $_APK $_APK"_signed.apk"
echo "rm origin apk"
rm $_APK
echo "rename"
mv $_APK"_signed.apk" $_APK
echo "done"




