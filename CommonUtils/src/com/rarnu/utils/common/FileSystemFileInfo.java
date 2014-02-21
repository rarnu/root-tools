package com.rarnu.utils.common;

import java.io.File;
import java.io.Serializable;
import java.net.FileNameMap;
import java.net.URLConnection;

public class FileSystemFileInfo implements Serializable {
    public boolean isDirectory;
    public String name;
    public String fullPath;
    public String mimeType = "";
    public int icon;
    public String ext = "";

    public FileSystemFileInfo(String name, String fullPath) {
        this.name = name;
        this.fullPath = fullPath;
        this.isDirectory = new File(fullPath).isDirectory();
        this.mimeType = getMimeType();
        if (fullPath.contains(".")) {
            ext = fullPath.substring(fullPath.lastIndexOf(".")).toLowerCase();
        }
    }

    private String getMimeType() {
        String type = "";
        try {
            FileNameMap fileNameMap = URLConnection.getFileNameMap();
            type = fileNameMap.getContentTypeFor("file://" + fullPath);
        } catch (Exception e) {

        }
        return type;
    }
}
