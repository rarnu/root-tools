package com.rarnu.tools.root.common;

public class GoogleInfo {

    public String fileName = "";

    public String path = "";

    // -1: to be checked
    // 0: normal
    // 1: not found
    // 2: file broken
    // 3: sign broken
    public int status = 0;

    // 0: apk
    // 1: jar
    // 2: lib
    // 3: xml
    public int type = 0;

}
