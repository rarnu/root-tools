ant release
cp ./bin/ActivityCalendar-release-unsigned.apk ./key/
cd ./key/
jarsigner -verbose -sigalg SHA1withRSA -digestalg SHA1 -keystore EventCalender.keystore ActivityCalendar-release-unsigned.apk ec
zipalign -v 4 ActivityCalendar-release-unsigned.apk ActivityCalendar-release.apk
echo "done"

