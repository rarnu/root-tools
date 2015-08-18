package com.rarnu.utils;

import android.content.Context;

import java.io.File;
import java.io.IOException;

public class FileCache {

    private File cacheDir;

    public FileCache(Context context) {
        if (MiscUtils.isSDCardExists()) {
            cacheDir = new File(android.os.Environment.getExternalStorageDirectory(), ".cache");
        } else {
            cacheDir = context.getCacheDir();
        }
        if (!cacheDir.exists()) {
            cacheDir.mkdirs();
        }
    }

    public File getFile(String url) {
        String filename = String.valueOf(url.hashCode());
        File f = new File(cacheDir, filename);
        return f;
    }

    public void putFile(String filePath) {
        String filename = filePath.substring(filePath.lastIndexOf("/") + 1);
        File fDest = new File(cacheDir, filename);
        if (fDest.exists()) {
            fDest.delete();
        }
        try {
            FileUtils.copyFile(filePath, fDest.getAbsolutePath(), null);
        } catch (IOException e) {

        }
    }

    public File getFileByName(String fileName) {
        File f = new File(cacheDir, fileName);
        File ret = null;
        if (f.exists()) {
            ret = f;
        }
        return ret;
    }

    public void clear() {
        File[] files = cacheDir.listFiles();
        if (files == null) {
            return;
        }
        for (File f : files) {
            f.delete();
        }
    }

}