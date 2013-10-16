package com.rarnu.tools.root.common;

import java.net.FileNameMap;
import java.net.URLConnection;

public class FileSystemFileInfo {
    public boolean isDirectory;
    public String name;
    public String fullPath;
    public String mimeType;
    public int icon;

    public FileSystemFileInfo(boolean isDirectory, String name, String fullPath) {
        this.name = name;
        this.fullPath = fullPath;
        this.isDirectory = isDirectory;
        this.mimeType = getMimeType();
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
