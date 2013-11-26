RootTools
========

the tool for rooted android devices

# how to compile

1) get the JelyBean jar with hidden APIs
```
   $ wget http://rarnu.7thgen.info/downloads/jellybean17.jar
```
2) backup your android.jar in $(ANDROID_SDK)/platforms/android-17/
```
   $ mv $(ANDROID_SDK)/platforms/android-17/android.jar ./backup/
```
3) replace android.jar said above with the downloaded file
```
   $ mv jellybean17.jar android.jar
   $ cp android.jar $(ANDROID_SDK)/platforms/android-17/
```
4) update following projects
```
   $ android update project -n CommandLib -p . -t android-17
   $ android update project -n CommonUtils -p . -t android-17
   $ android update project -n CommonDevLib -p . -t android-17
   $ android update project -n Emulator -p . -t android-17
   $ android update project -n PackageParser4 -p . -t android-17
   $ android update project -n RootTools -p . -t android-17
```
5) build emulator so
```
    $ cd Emulator
    $ ndk-build clean && ndk-build
```
6) build and install
```
   $ cd $(PROJECT)/RootTools
   $ ant debug
```
# signature
```
   $ cd $(PROJECT)/sign
   $ java -jar signapk.jar rarnu.x509.pem rarnu.pk8 RootTools-debug.apk RootTools_signed.apk
```
# install and start
```
   $ adb install -r RootTools_signed.apk
   $ adb shell am start -n com.rarnu.tools.root/.SplashActivity
```
