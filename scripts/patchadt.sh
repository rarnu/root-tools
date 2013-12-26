#!/bin/sh

ADT=$1
OUT=$2
cp $ADT adt.zip
unzip -o adt.zip com/android/ide/eclipse/adt/internal/project/AndroidClasspathContainerInitializer.class
./adt_changer -n com/android/ide/eclipse/adt/internal/project/AndroidClasspathContainerInitializer.class
zip -r adt.zip com/
mv adt.zip com.android.ide.eclipse.adt_patched.jar
mv com.android.ide.eclipse.adt_patched.jar $OUT
echo "done"

