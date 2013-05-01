RootTools
========

how to compile:

1) get the froyo jar with full source
```
   $ wget http://rarnu.7thgen.info/downloads/froyo8.jar
```
2) backup your android.jar in $(ANDROID_SDK)/platforms/android-8/
```
   $ mv $(ANDROID_SDK)/platforms/android-8/android.jar ./backup/
```
3) replace android.jar said above with the downloaded file
```
   $ mv froyo8.jar android.jar
   $ cp android.jar $(ANDROID_SDK)/platforms/android-8/
```
4) import and fix CommonDevLib, PackageParser4 and RootTools
```
   $ android update project -n CommandLib -p . -t android-8
   $ android update project -n CommonDevLib -p . -t android-15
   $ android update project -n PackageParser4 -p . -t android-8
   $ android update project -n RootTools -p . -t android-15
```
5) build and install
```
   $ cd $(ROOT_TOOLS_SRC)
   $ ant debug
   $ adb install $(ROOT_TOOLS_SRC)/bin/RootTools-debug.apk
```
6) signature(necessary for publish)
```
   java -jar signapk.jar rarnu.x509.pem rarnu.pk8 <InputApkName> <OutoutApkName>
```
