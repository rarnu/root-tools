package com.rarnu.tools.root.api;

/**
 * 4.0.3    -   15
 * 4.1.2    -   16
 * 4.2.2    -   17
 * 4.3.0    -   18
 * 4.4.2    -   19
 */
public class MobileGoogleApi {

    public static final String BASE_URL = "http://rarnu.7thgen.info/root_tools/google/";

    public static String getDownloadUrl(int sdkver) {
        return BASE_URL + String.format("google_%d.zip", sdkver);
    }
}
