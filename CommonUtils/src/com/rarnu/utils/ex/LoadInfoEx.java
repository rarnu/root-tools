package com.rarnu.utils.ex;

public class LoadInfoEx {

    public int fileSize;
    public int complete;
    public String urlstring;

    public LoadInfoEx() {

    }

    public LoadInfoEx(int fileSize, int complete, String urlstring) {

        this.fileSize = fileSize;
        this.complete = complete;
        this.urlstring = urlstring;
    }

    @Override
    public String toString() {
        return "LoadInfo [fileSize=" + fileSize + ", complete=" + complete + ", urlstring=" + urlstring + "]";
    }

}
