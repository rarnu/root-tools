import os
import sys
import shutil
from zipfile import ZipFile

HOME_PATH = sys.path[0:][0]
WORK_PATH = "adt"
ZIP_NAME = "adt.zip"
OUTPUT_NAME = "com.android.ide.eclipse.adt_patched.jar"
CLASS_FILE_NAME = "com/android/ide/eclipse/adt/internal/project/AndroidClasspathContainerInitializer.class"
KEY_WORD = "com/android/internal/**"
KEY_WORD_MODIFIED = "com/android/internax/**"


def do_unzip(filename):
    zip = ZipFile(filename)
    zip.extractall()
    zip.close()
    os.remove(filename)


def do_zip(filename):
    zip = ZipFile(filename, "w")
    for dirpath, dirname, filename in os.walk("./"):
        for fn in filename:
            if fn != ZIP_NAME:
                zip.write(os.path.join(dirpath, fn))
    zip.close()


def is_match(b, idx, str, strm):
    ret = 0
    bstr = ""
    for i in range(idx, idx + len(str)):
        bstr = bstr + b[i]
    if bstr == str:
        ret = 1
    elif bstr == strm:
        ret = 2
    return ret


def seek_match(b):
    ret = -1
    for i in range(0, len(b) - len(KEY_WORD)):
        match_ret = is_match(b, i, KEY_WORD, KEY_WORD_MODIFIED)
        if match_ret == 1:
            ret = i
            break
        elif match_ret == 2:
            ret = -2
            break
    return ret


def do_patch_class():
    ret = -1
    f = open(CLASS_FILE_NAME, "r+b")
    b = f.read()
    idx = seek_match(b)
    if idx == -2:
        print("File is already patched.")
    else:
        if idx != -1:
            idx += 19
            f.seek(idx, 0)
            f.write("x")
            print("File is patched.")
            ret = 0
        else:
            print("File error.")
    f.close()
    return ret


def patch(adt, output):
    os.makedirs(WORK_PATH)
    os.chdir(WORK_PATH)
    shutil.copyfile(adt, ZIP_NAME)
    do_unzip(ZIP_NAME)
    ret = do_patch_class()
    if ret == 0:
        do_zip(ZIP_NAME)
        shutil.move(ZIP_NAME, os.path.join(output, OUTPUT_NAME))
    os.chdir(HOME_PATH)
    shutil.rmtree(WORK_PATH)


if __name__ == "__main__":
    if len(sys.argv) != 3:
        print("usage: patchadt.py <ADT JAR PATH> <OUTPUT PATH>")
        exit()
    adt = sys.argv[1:][0]
    output = sys.argv[2:][0]
    patch(adt, output)